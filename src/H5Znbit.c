/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#define H5Z_PACKAGE		/*suppress error about including H5Zpkg	  */

/* Pablo information */
/* (Put before include files to avoid problems with inline functions) */
#define PABLO_MASK      H5Z_nbit_mask

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"         /* File access                          */
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Ppublic.h"		/* Property lists			*/
#include "H5Tpublic.h"		/* Datatype functions			*/
#include "H5Zpkg.h"		/* Data filters				*/

#ifdef H5_HAVE_FILTER_NBIT

/* Local function prototypes */
static herr_t H5Z_can_apply_nbit(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static herr_t H5Z_set_local_nbit(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static size_t H5Z_filter_nbit(unsigned flags, size_t cd_nelmts, const unsigned cd_values[], 
                              size_t nbytes, size_t *buf_size, void **buf);

typedef struct {
   int precision;
   int offset;
   int sizeof_datatype;
   int order;
} para;

void H5Z_nbit_next_char(int *j, int *buf_len);
void H5Z_nbit_byte_order(int *big_endian, int *little_endian);
void H5Z_nbit_decompress_one_byte(void *data, int i, int k, int begin_i, int end_i, char *buffer,
                         int *j, int *buf_len, para p);
void H5Z_nbit_compress_one_byte(void *data, int i, int k, int begin_i, int end_i, char *buffer, 
                       int *j, int *buf_len, para p);
void H5Z_nbit_decompress(void *data, int data_size, char *buffer, int buffer_size, para p);
void H5Z_nbit_compress(void *data, int data_size, char *buffer, int buffer_size, para p);

/* This message derives from H5Z */
H5Z_class_t H5Z_NBIT[1] = {{
    H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
    H5Z_FILTER_NBIT,		/* Filter id number		*/
    1,              /* Assume encoder present: check before registering */
    1,                  /* decoder_present flag (set to true) */
    "nbit",			    /* Filter name for debugging	*/
    H5Z_can_apply_nbit,		/* The "can apply" callback     */
    H5Z_set_local_nbit,         /* The "set local" callback     */
    H5Z_filter_nbit,		/* The actual filter function	*/
}};

/* Local macros */
#define H5Z_NBIT_USER_NPARMS     0    /* Number of parameters that users can set */
#define H5Z_NBIT_TOTAL_NPARMS    5    /* Total number of parameters for filter */
#define H5Z_NBIT_PARM_PRECIS     0    /* "Local" parameter for datatype precision */
#define H5Z_NBIT_PARM_OFFSET     1    /* "Local" parameter for datatype offset */
#define H5Z_NBIT_PARM_TSIZE      2    /* "Local" parameter for datatype size */
#define H5Z_NBIT_PARM_NELMTS     3    /* "Local" parameter for number of elements in the chunk */
#define H5Z_NBIT_PARM_ORDER      4    /* "Local" parameter for datatype byte order */

/*-------------------------------------------------------------------------
 * Function:	H5Z_can_apply_nbit
 *
 * Purpose:	Check the parameters for nbit compression for validity and
 *              whether they fit a particular dataset.
 *
 * Note:        
 *              
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:  Quincey Koziol
 *              Monday, April  7, 2003 
 *
 * Modifications: 
 *              Xiaowen Wu
 *              Tuesday, December 21, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_can_apply_nbit(hid_t UNUSED dcpl_id, hid_t type_id, hid_t UNUSED space_id)
{
    unsigned dtype_size;                /* Datatype's size (in bytes) */
    H5T_order_t dtype_order;            /* Datatype's endianness order */
    herr_t ret_value=TRUE;              /* Return value */

    FUNC_ENTER_NOAPI(H5Z_can_apply_nbit, FAIL)

    /* Get datatype's size, for checking the "datatype size" */
    if((dtype_size = H5Tget_size(type_id)) == 0)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype size")

    /* Get datatype's endianness order */
    if((dtype_order = H5Tget_order(type_id)) == H5T_ORDER_ERROR)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "can't retrieve datatype endianness order")

    /* Range check datatype's endianness order */
    /* (Note: this may not handle non-atomic datatypes well) */
    if(dtype_order != H5T_ORDER_LE && dtype_order != H5T_ORDER_BE)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FALSE, "invalid datatype endianness order")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_can_apply_nbit() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_set_local_nbit
 *
 * Purpose:	Set the "local" dataset parameters for nbit compression.
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 7, 2003
 *
 * Modifications:
 *              Xiaowen Wu
 *              Tuesday, December 21, 2004 
 *                
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_set_local_nbit(hid_t dcpl_id, hid_t type_id, hid_t space_id)
{
    unsigned flags;         /* Filter flags */
    size_t cd_nelmts=H5Z_NBIT_USER_NPARMS;     /* Number of filter parameters */
    unsigned cd_values[H5Z_NBIT_TOTAL_NPARMS];  /* Filter parameters */
    hsize_t dims[H5O_LAYOUT_NDIMS];             /* Dataspace (i.e. chunk) dimensions */
    int ndims;                  /* Number of (chunk) dimensions */
    hssize_t npoints;           /* Number of points in the dataspace */
    H5T_order_t dtype_order;    /* Datatype's endianness order */
    int dtype_size;             /* Datatype's size (in bytes) */
    size_t dtype_precision;     /* Datatype's precision (in bits) */
    int dtype_offset;        /* Datatype's offset (in bits) */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5Z_set_local_nbit, FAIL)

    /* Get the filter's current parameters */
#ifdef H5_WANT_H5_V1_6_COMPAT
    if(H5Pget_filter_by_id(dcpl_id,H5Z_FILTER_NBIT,&flags,&cd_nelmts, cd_values,0,NULL)<0)
#else
    if(H5Pget_filter_by_id(dcpl_id,H5Z_FILTER_NBIT,&flags,&cd_nelmts, cd_values,0,NULL,NULL)<0)
#endif
	HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "can't get nbit parameters")

    /* Get datatype's size, for checking the "datatype size" */
    if((dtype_size=H5Tget_size(type_id))==0)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype size");

    /* Set "local" parameter for this dataset's "datatype size" */
    cd_values[H5Z_NBIT_PARM_TSIZE]=dtype_size;

    /* Get datatype's precision, for checking the "datatype precision"  */
    if((dtype_precision=H5Tget_precision(type_id))==0)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype precision");

    /* Set "local" parameter for this dataset's "datatype precision" */
    cd_values[H5Z_NBIT_PARM_PRECIS]=dtype_precision;

    /* Get datatype's offset, for checking the "datatype offset"  */
    if((dtype_offset=H5Tget_offset(type_id))<0)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype offset");

    /* Set "local" parameter for this dataset's "datatype offset" */
    cd_values[H5Z_NBIT_PARM_OFFSET]=dtype_offset;

    /* Get dimensions for dataspace */
    if ((ndims=H5Sget_simple_extent_dims(space_id, dims, NULL))<0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get dataspace dimensions")
    assert(ndims>0);

    /* Get total number of elements in the chunk */
    if ((npoints=H5Sget_simple_extent_npoints(space_id))<0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get number of points in the dataspace")

    /* Set "local" parameter for this dataset's "number of elements" */
    cd_values[H5Z_NBIT_PARM_NELMTS] = npoints;

    /* Get datatype's endianness order */
    if((dtype_order=H5Tget_order(type_id))==H5T_ORDER_ERROR)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype endianness order")

    /* Set the correct endianness for nbit */
    /* (Note: this may not handle non-atomic datatypes well) */
    switch(dtype_order) {
        case H5T_ORDER_LE:      /* Little-endian byte order */
            cd_values[H5Z_NBIT_PARM_ORDER] = 10;
            break;
        case H5T_ORDER_BE:      /* Big-endian byte order */
            cd_values[H5Z_NBIT_PARM_ORDER] = 20;
            break;
        default:
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype endianness order")
    } /* end switch */

    /* Modify the filter's parameters for this dataset */
    if(H5Pmodify_filter(dcpl_id, H5Z_FILTER_NBIT, flags, H5Z_NBIT_TOTAL_NPARMS, cd_values)<0)
	HGOTO_ERROR(H5E_PLINE, H5E_CANTSET, FAIL, "can't set local nbit parameters")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_set_local_nbit() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_nbit
 *
 * Purpose:	Implement an I/O filter for storing packed n-bit data
 *              
 * Return:	Success: Size of buffer filtered
 *		Failure: 0	
 *
 * Programmer:	Xiaowen Wu
 *              Tuesday, December 21, 2004
 *
 * Modifications:
 *              
 *-------------------------------------------------------------------------
 */
static size_t
H5Z_filter_nbit (unsigned flags, size_t cd_nelmts, const unsigned cd_values[], 
    size_t nbytes, size_t *buf_size, void **buf)
{
    size_t ret_value = 0;       /* return value */
    size_t size_out  = 0;       /* size of output buffer */
    size_t d_nelmts    = 0;       /* number of elements in the chunk */
    unsigned char *outbuf = NULL;    /* pointer to new output buffer */
    para nbit_param;          /* nbit parameter block */

    FUNC_ENTER_NOAPI(H5Z_filter_nbit, 0)

    /* check arguments */
    if (cd_nelmts!=5)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid nbit aggression level")

    /* copy the filter parameters into the nbit parameter block */
    H5_ASSIGN_OVERFLOW(nbit_param.precision,cd_values[H5Z_NBIT_PARM_PRECIS],unsigned,int);
    H5_ASSIGN_OVERFLOW(nbit_param.offset,cd_values[H5Z_NBIT_PARM_OFFSET],unsigned,int);
    H5_ASSIGN_OVERFLOW(nbit_param.sizeof_datatype,cd_values[H5Z_NBIT_PARM_TSIZE],unsigned,int);
    H5_ASSIGN_OVERFLOW(nbit_param.order,cd_values[H5Z_NBIT_PARM_ORDER],unsigned,int);

    /* copy a filter parameter to d_nelmts */
    H5_ASSIGN_OVERFLOW(d_nelmts,cd_values[H5Z_NBIT_PARM_NELMTS],unsigned,int);

    printf("\nprecision: %d", nbit_param.precision);
    printf("    offset: %d\n", nbit_param.offset);

    /* no need to compress or decompress 
    if (nbit_param.precision == nbit_param.sizeof_datatype * 8) { 
        ret_value = *buf_size;
        goto done;
    }*/

    /* input; decompress */
    if (flags & H5Z_FLAG_REVERSE) {
        size_out = d_nelmts * nbit_param.sizeof_datatype;

        /* allocate memory space for decompressed buffer */
        if(NULL==(outbuf = H5MM_malloc(size_out)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for nbit decompression")
  
        /* decompress the buffer */
        H5Z_nbit_decompress(outbuf, d_nelmts, *buf, nbytes, nbit_param);   
    }
    /* output; compress */
    else {
        if((d_nelmts * nbit_param.precision) % (sizeof(char) * 8) != 0)
            size_out  = (d_nelmts * nbit_param.precision) / (sizeof(char) * 8) + 1;
        else
            size_out  = (d_nelmts * nbit_param.precision) / (sizeof(char) * 8);

        /* allocate memory space for compressed buffer */
        if(NULL==(outbuf = H5MM_malloc(size_out)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for nbit compression")
  
        /* compress the buffer */
        H5Z_nbit_compress(*buf, d_nelmts, outbuf, size_out, nbit_param);
    }

    /* free the input buffer */
    H5MM_xfree(*buf);

    /* set return values */
    *buf = outbuf;
    outbuf = NULL;
    *buf_size = size_out;
    ret_value = size_out;

done: 
    if(outbuf)
        H5MM_xfree(outbuf);
    FUNC_LEAVE_NOAPI(ret_value)
}

/* assume one byte has 8 bit, padding bit is 0
   assume size of char is one byte
   assume one item of certain data type is stored continously in bytes 
   data type regardless what is treated on byte basis 
*/

void H5Z_nbit_next_char(int *j, int *buf_len)
{
   ++(*j); 
   *buf_len = 8 * sizeof(char);
}

void H5Z_nbit_byte_order(int *big_endian, int *little_endian)
{
   /* decide the machine's byte order */
   union {
      long l; 
      char c[sizeof(long)];
   } u;
   u.l = 1;
   if(u.c[0] == 1) 
      *little_endian = 1;
   else if(u.c[sizeof(long) - 1] == 1) 
      *big_endian = 1;
   else {
      printf("Abort: unkown byte order!\n");
      exit(1);
   }
}       

void H5Z_nbit_decompress_one_byte(void *data, int i, int k, int begin_i, int end_i, char *buffer, 
            int *j, int *buf_len, para p)
{
   int dat_len; /* dat_len is the number of bits to be copied in each data char */
   int datatype_len, char_offset;
   char val; /* value to be copied in each data char */

   datatype_len = p.sizeof_datatype * 8;

   /* initialize value and bits of char to be copied */
   val = buffer[*j];
   char_offset = 0;
   if(begin_i != end_i) { /* significant bits occupy >1 char */
      if(k == begin_i) 
         dat_len = 8 - (datatype_len - p.precision - p.offset) % 8;
      else if(k == end_i) {
         if(p.offset % 8 == 0)
            dat_len = 8;
         else
            dat_len = (datatype_len - p.offset) % 8;
         char_offset = 8 - dat_len;
      }
      else
         dat_len = 8;
   } else { /* all significant bits in one char */
      if(p.offset % 8 != 0)
         char_offset = p.offset % 8;
      dat_len = p.precision;
   }

   if(*buf_len > dat_len) { 
      ((char *)data)[i * p.sizeof_datatype + k] |=
      ((val >> (*buf_len - dat_len)) & ~(~0 << dat_len)) << char_offset;
      *buf_len -= dat_len;
   } else {
      ((char *)data)[i * p.sizeof_datatype + k] |=
      ((val & ~(~0 << *buf_len)) << (dat_len - *buf_len)) << char_offset;
      dat_len -= *buf_len;
      H5Z_nbit_next_char(j, buf_len);
      if(dat_len == 0) 
         return;
      val = buffer[*j];
   
      ((char *)data)[i * p.sizeof_datatype + k] |=
      ((val >> (*buf_len - dat_len)) & ~(~0 << dat_len)) << char_offset;
      *buf_len -= dat_len; 
   }
}

/* decompress buffer to original data form */
void H5Z_nbit_decompress(void *data, int data_size, char *buffer, int buffer_size, para p) 
{
   /* i: index of data, j: index of buffer, 
      buf_len: number of bits to be copied in current char */
   int i, j, buf_len; 
   /* begin_i: the index of byte having first significant bit
      end_i: the index of byte having last significant bit */
   int k, begin_i, end_i, big_endian, little_endian, datatype_len;

   printf("decompress is called \n");

   /* may not have to initialize to zeros */
   for(i = 0; i < data_size * p.sizeof_datatype; i++)  
      ((char *)data)[i] = 0; 
   
   /* decide the byte order of the machine */
   little_endian = big_endian = 0;
   H5Z_nbit_byte_order(&big_endian, &little_endian);

   datatype_len = p.sizeof_datatype * 8;

   /* initialization before the loop */ 
   j = 0; 
   buf_len = sizeof(char) * 8; 

   if(p.order == 10) { /* little endian */
      printf("\ndatatype is little-endian\n");
      /* calculate begin_i and end_i */
      if((p.precision + p.offset) % 8 != 0)
         begin_i = (p.precision + p.offset) / 8;
      else
         begin_i = (p.precision + p.offset) / 8 - 1;
      end_i = p.offset / 8;

      for(i = 0; i < data_size; i++) 
         for(k = begin_i; k >= end_i; k--)
            H5Z_nbit_decompress_one_byte(data, i, k, begin_i, end_i, buffer, &j, &buf_len, p);
   }

   if(p.order == 20) { /* big endian */
      printf("\ndatatype is big-endian\n");
      /* calculate begin_i and end_i */
      begin_i = (datatype_len - p.precision - p.offset) / 8;
      if(p.offset % 8 != 0)
         end_i = (datatype_len - p.offset) / 8;
      else
         end_i = (datatype_len - p.offset) / 8 - 1;

      for(i = 0; i < data_size; i++) 
         for(k = begin_i; k <= end_i; k++)
            H5Z_nbit_decompress_one_byte(data, i, k, begin_i, end_i, buffer, &j, &buf_len, p);
   } 
}

void H5Z_nbit_compress_one_byte(void *data, int i, int k, int begin_i, int end_i, char *buffer, 
                       int *j, int *buf_len, para p)
{
   int dat_len; /* dat_len is the number of bits to be copied in each data char */
   int datatype_len;
   char val; /* value to be copied in each data char */

   datatype_len = p.sizeof_datatype * 8;

   /* initialize value and bits of char to be copied */
   val = ((char *)data)[i * p.sizeof_datatype + k];
   if(begin_i != end_i) { /* significant bits occupy >1 char */
      if(k == begin_i) 
         dat_len = 8 - (datatype_len - p.precision - p.offset) % 8;
      else if(k == end_i) {
         if(p.offset % 8 == 0)
            dat_len = 8;
         else
            dat_len = (datatype_len - p.offset) % 8;
         val >>= 8 - dat_len;
      }
      else
         dat_len = 8;
   } else { /* all significant bits in one char */
      if(p.offset % 8 != 0)
         val >>= p.offset % 8;
      dat_len = p.precision;
   }

   if(*buf_len > dat_len) { 
      buffer[*j] |= (val & ~(~0 << dat_len)) << (*buf_len - dat_len);
      *buf_len -= dat_len;
   } else {
      buffer[*j] |= (val >> (dat_len - *buf_len)) & ~(~0 << *buf_len);
      dat_len -= *buf_len;
      H5Z_nbit_next_char(j, buf_len);
      if(dat_len == 0) 
         return;
      val &= ~(~0 << dat_len);
   
      buffer[*j] |= (val & ~(~0 << dat_len)) << (*buf_len - dat_len);
      *buf_len -= dat_len; 
   }
}

/* copy array of certain data type to buffer in compressed form
   return number of unused bits in the buffer */
void H5Z_nbit_compress(void *data, int data_size, char *buffer, int buffer_size, para p) {
   /* i: index of data, j: index of buffer, 
      buf_len: number of bits to be filled in current char */
   int i, j, buf_len; 
   /* begin_i: the index of byte having first significant bit
      end_i: the index of byte having last significant bit */ 
   int k, begin_i, end_i, big_endian, little_endian, datatype_len;

   printf("compress is called \n");

   for(j = 0; j < buffer_size; j++) /* initialize buffer to be zeros */ 
      buffer[j] = 0;

   /* decide the byte order of the machine */
   big_endian = little_endian = 0;
   H5Z_nbit_byte_order(&big_endian, &little_endian);

   datatype_len = p.sizeof_datatype * 8;
   /* initialization before the loop */ 
   j = 0; 
   buf_len = sizeof(char) * 8; 

   if(p.order == 10) { /* little endian */
      printf("\ndatatype is little-endian\n");
      /* calculate begin_i and end_i */
      if((p.precision + p.offset) % 8 != 0)
         begin_i = (p.precision + p.offset) / 8;
      else
         begin_i = (p.precision + p.offset) / 8 - 1;
      end_i = p.offset / 8;

      for(i = 0; i < data_size; i++) 
         for(k = begin_i; k >= end_i; k--) 
            H5Z_nbit_compress_one_byte(data, i, k, begin_i, end_i, buffer, &j, &buf_len, p);
   }

   if(p.order == 20) { /* big endian */
      printf("\ndatatype is big-endian\n");
      /* calculate begin_i and end_i */
      begin_i = (datatype_len - p.precision - p.offset) / 8;
      if(p.offset % 8 != 0)
         end_i = (datatype_len - p.offset) / 8;
      else
         end_i = (datatype_len - p.offset) / 8 - 1;

      for(i = 0; i < data_size; i++) 
         for(k = begin_i; k <= end_i; k++) 
            H5Z_nbit_compress_one_byte(data, i, k, begin_i, end_i, buffer, &j, &buf_len, p);
   }    
}
#endif /* H5_HAVE_FILTER_NZIP */

