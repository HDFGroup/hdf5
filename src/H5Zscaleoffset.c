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

/* currently this filter only handles integer datatype 
 * does not consider fill value
 */

#define H5Z_PACKAGE		/*suppress error about including H5Zpkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Ppublic.h"		/* Property lists			*/
#include "H5Oprivate.h"         /* Object headers                       */
#include "H5Tpublic.h"		/* Datatype functions			*/
#include "H5Zpkg.h"		/* Data filters				*/

#ifdef H5_HAVE_FILTER_SCALEOFFSET

/* Struct of parameters needed for compressing/decompressing one atomic datatype */ 
typedef struct {
   size_t size;
   unsigned order;
   unsigned minbits;
} parms_atomic; 

/* Local function prototypes */
static herr_t H5Z_can_apply_scaleoffset(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static herr_t H5Z_set_local_scaleoffset(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static size_t H5Z_filter_scaleoffset(unsigned flags, size_t cd_nelmts, const unsigned cd_values[], 
                                     size_t nbytes, size_t *buf_size, void **buf);
void H5Z_scaleoffset_precompress(void *data, unsigned d_nelmts, unsigned type, unsigned filavail,
                                 unsigned *minbits, unsigned long_long *minval);
void H5Z_scaleoffset_postdecompress(void *data, unsigned d_nelmts, unsigned type, unsigned filavail,
                                    unsigned long_long minval);
void H5Z_scaleoffset_next_byte(size_t *j, int *buf_len);
void H5Z_scaleoffset_decompress_one_byte(void *data, size_t data_offset, int k, int begin_i,
                             unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p);
void H5Z_scaleoffset_compress_one_byte(void *data, size_t data_offset, int k, int begin_i,
                           unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p);
void H5Z_scaleoffset_decompress_one_atomic(void *data, size_t data_offset, unsigned char *buffer,
                                           size_t *j, int *buf_len, parms_atomic p);
void H5Z_scaleoffset_compress_one_atomic(void *data, size_t data_offset, unsigned char *buffer,
                                         size_t *j, int *buf_len, parms_atomic p);
void H5Z_scaleoffset_decompress(void *data, unsigned d_nelmts, unsigned char *buffer,
                                parms_atomic p);
void H5Z_scaleoffset_compress(void *data, unsigned d_nelmts, unsigned char *buffer,
                              size_t buffer_size, parms_atomic p);

/* This message derives from H5Z */
H5Z_class_t H5Z_SCALEOFFSET[1] = {{
    H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
    H5Z_FILTER_SCALEOFFSET, /* Filter id number		*/
    1,              /* Assume encoder present: check before registering */
    1,              /* decoder_present flag (set to true) */
    "scaleoffset",		/* Filter name for debugging	*/
    H5Z_can_apply_scaleoffset,	/* The "can apply" callback     */
    H5Z_set_local_scaleoffset,  /* The "set local" callback     */
    H5Z_filter_scaleoffset,	/* The actual filter function	*/
}};

/* Local macros */
#define H5Z_SCALEOFFSET_USER_NPARMS     1    /* Number of parameters that users can set */
#define H5Z_SCALEOFFSET_TOTAL_NPARMS    7    /* Total number of parameters for filter */
#define H5Z_SCALEOFFSET_PARM_MINBITS    0    /* "User" parameter for minimum number of bits */
#define H5Z_SCALEOFFSET_PARM_NELMTS     1    /* "Local" parameter for number of elements in the chunk */
#define H5Z_SCALEOFFSET_PARM_CLASS      2    /* "Local" parameter for datatype class */
#define H5Z_SCALEOFFSET_PARM_SIZE       3    /* "Local" parameter for datatype size */
#define H5Z_SCALEOFFSET_PARM_TYPE       4    /* "Local" parameter for integer types */
#define H5Z_SCALEOFFSET_PARM_ORDER      5    /* "Local" parameter for datatype byte order */
#define H5Z_SCALEOFFSET_PARM_FILAVAIL   6    /* "Local" parameter for dataset fill value existence */

#define H5Z_SCALEOFFSET_CLS_INTEGER     0    /* Integer (datatype class) */
#define H5Z_SCALEOFFSET_CLS_FLOAT       1    /* Floating point (datatype class) */

#define H5Z_SCALEOFFSET_ORDER_LE        0    /* Little endian (datatype byte order) */
#define H5Z_SCALEOFFSET_ORDER_BE        1    /* Big endian (datatype byte order) */

#define H5Z_SCALEOFFSET_FILL_UNDEFINED  0    /* Fill value is not defined */
#define H5Z_SCALEOFFSET_FILL_DEFINED    1    /* Fill value is defined */

/* Local variables */
/* can not use pfill_val now
 * pfill_val: pointer to buffer containing fill value of dataset fill value if is defined
 */
static void *pfill_val;

enum H5Z_scaleoffset_type {t_uchar, t_ushort, t_uint, t_ulong, t_ulong_long,
                           t_schar, t_short, t_int, t_long, t_long_long};


/*-------------------------------------------------------------------------
 * Function:	H5Z_can_apply_scaleoffset
 *
 * Purpose:	Check the parameters for scaleoffset compression for 
 *              validity and whether they fit a particular dataset.
 *
 * Note:        
 *              
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:  Xiaowen Wu
 *              Friday, February 4, 2005
 *
 * Modifications: 
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_can_apply_scaleoffset(hid_t UNUSED dcpl_id, hid_t type_id, hid_t UNUSED space_id)
{
    H5T_class_t dtype_class;            /* Datatype's class */
    unsigned dtype_size;                /* Datatype's size (in bytes) */
    H5T_order_t dtype_order;            /* Datatype's endianness order */
    herr_t ret_value=TRUE;              /* Return value */

    FUNC_ENTER_NOAPI(H5Z_can_apply_scaleoffset, FAIL)

    /* Get datatype's class, for checking the "datatype class" */
    if((dtype_class = H5Tget_class(type_id)) == H5T_NO_CLASS )
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype class")

    /* Get datatype's size, for checking the "datatype size" */
    if((dtype_size = H5Tget_size(type_id)) == 0)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype size")

    /* Codes below apply to integer or floating point datatype */
    if(dtype_class == H5T_INTEGER || dtype_class == H5T_FLOAT) {
        /* Get datatype's endianness order */
        if((dtype_order = H5Tget_order(type_id)) == H5T_ORDER_ERROR)
	    HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "can't retrieve datatype endianness order")

        /* Range check datatype's endianness order */
        /* (Note: this may not handle non-atomic datatypes well) */
        if(dtype_order != H5T_ORDER_LE && dtype_order != H5T_ORDER_BE)
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype endianness order")
    } else
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "datatype class not supported by scaleoffset")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_can_apply_scaleoffset() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_set_local_scaleoffset
 *
 * Purpose:	Set the "local" dataset parameters for scaleoffset
 *              compression.
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:	Xiaowen Wu
 *              Friday, February 4, 2005
 *
 * Modifications:
 *                
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_set_local_scaleoffset(hid_t dcpl_id, hid_t type_id, hid_t space_id)
{
    unsigned flags;         /* Filter flags */
    size_t cd_nelmts=H5Z_SCALEOFFSET_USER_NPARMS;  /* Number of filter parameters */
    unsigned cd_values[H5Z_SCALEOFFSET_TOTAL_NPARMS]; /* Filter parameters */
    hssize_t npoints;               /* Number of points in the dataspace */
    H5T_class_t dtype_class;        /* Datatype's class */
    H5T_order_t dtype_order;        /* Datatype's endianness order */
    size_t dtype_size;              /* Datatype's size (in bytes) */
    H5T_sign_t dtype_sign;          /* Datatype's sign */
    enum H5Z_scaleoffset_type type; /* Integer type */
    H5D_fill_value_t status;        /* Status of fill value in property list */
    herr_t ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5Z_set_local_scaleoffset, FAIL)

    /* Get the filter's current parameters */
#ifdef H5_WANT_H5_V1_6_COMPAT
    if(H5Pget_filter_by_id(dcpl_id,H5Z_FILTER_SCALEOFFSET,&flags,&cd_nelmts,cd_values,0,NULL)<0)
#else
    if(H5Pget_filter_by_id(dcpl_id,H5Z_FILTER_SCALEOFFSET,&flags,&cd_nelmts,cd_values,0,NULL,NULL)<0)
#endif
	HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "can't get scaleoffset parameters")

    /* Get total number of elements in the chunk */
    if ((npoints=H5Sget_simple_extent_npoints(space_id))<0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get number of points in the dataspace")

    /* Set "local" parameter for this dataset's number of elements */
    H5_ASSIGN_OVERFLOW(cd_values[H5Z_SCALEOFFSET_PARM_NELMTS],npoints,hssize_t,unsigned);

    /* Get datatype's class */
    if((dtype_class=H5Tget_class(type_id))==H5T_NO_CLASS )
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype class")

    /* Set "local" parameter for datatype's class */
    switch(dtype_class) {
        case H5T_INTEGER: cd_values[H5Z_SCALEOFFSET_PARM_CLASS] = H5Z_SCALEOFFSET_CLS_INTEGER;
             break;
        case H5T_FLOAT: cd_values[H5Z_SCALEOFFSET_PARM_CLASS] = H5Z_SCALEOFFSET_CLS_FLOAT;
             break;
        default:
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "datatype class not supported by scaleoffset")
    } /* end switch */

    /* Get datatype's size */
    if((dtype_size=H5Tget_size(type_id))==0)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype size")

    /* Set "local" parameter for datatype size */
    cd_values[H5Z_SCALEOFFSET_PARM_SIZE] = dtype_size;

    /* Get datatype's sign */
    if((dtype_sign=H5Tget_sign(type_id))==H5T_SGN_ERROR)
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype sign")

    if(dtype_class==H5T_INTEGER) {
        if(dtype_sign==H5T_SGN_NONE) { /* unsigned integer */
            if(dtype_size == sizeof(unsigned char))
                type = t_uchar;
            else if(dtype_size == sizeof(unsigned short))
                type = t_ushort; 
            else if(dtype_size == sizeof(unsigned int))
                type = t_uint; 
            else if(dtype_size == sizeof(unsigned long)) 
                type = t_ulong;
            else if(dtype_size == sizeof(unsigned long_long))
                type = t_ulong_long; 
            else 
              HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "integer size not matched")
        }

        if(dtype_sign==H5T_SGN_2) { /* signed integer */
            if(dtype_size == sizeof(signed char))
                type = t_schar;
            else if(dtype_size == sizeof(short))
                type = t_short; 
            else if(dtype_size == sizeof(int))
                type = t_int; 
            else if(dtype_size == sizeof(long)) 
                type = t_long;
            else if(dtype_size == sizeof(long_long))
                type = t_long_long; 
            else 
              HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "integer size not matched")
        }

        /* Set "local" parameter for integer types */
        cd_values[H5Z_SCALEOFFSET_PARM_TYPE] = type;
    }
    
    /* Get datatype's endianness order */
    if((dtype_order=H5Tget_order(type_id))==H5T_ORDER_ERROR)
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype endianness order")

    /* Set "local" parameter for datatype endianness */
    switch(dtype_order) {
        case H5T_ORDER_LE:      /* Little-endian byte order */
            cd_values[H5Z_SCALEOFFSET_PARM_ORDER] = H5Z_SCALEOFFSET_ORDER_LE;
            break;
        case H5T_ORDER_BE:      /* Big-endian byte order */
            cd_values[H5Z_SCALEOFFSET_PARM_ORDER] = H5Z_SCALEOFFSET_ORDER_BE;
            break;
        default:
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype endianness order")
    } /* end switch */

    /* Check whether fill value is defined for dataset */
    if(H5Pfill_value_defined(dcpl_id, &status)<0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to determine if fill value is defined")

    /* Set local parameter for availability of fill value */
    if(status == H5D_FILL_VALUE_UNDEFINED)
        cd_values[H5Z_SCALEOFFSET_PARM_FILAVAIL] = H5Z_SCALEOFFSET_FILL_UNDEFINED;
    else {
        cd_values[H5Z_SCALEOFFSET_PARM_FILAVAIL] = H5Z_SCALEOFFSET_FILL_DEFINED;
#if 0
        /* Get dataset fill value */
        if(H5Pget_fill_value(dcpl_id, type_id, pfill_val)<0)
            HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get fill value")
#endif
    }

    /* Modify the filter's parameters for this dataset */
    if(H5Pmodify_filter(dcpl_id, H5Z_FILTER_SCALEOFFSET, flags, H5Z_SCALEOFFSET_TOTAL_NPARMS, cd_values)<0)
	HGOTO_ERROR(H5E_PLINE, H5E_CANTSET, FAIL, "can't set local scaleoffset parameters")

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5Z_set_local_scaleoffset() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_scaleoffset
 *
 * Purpose:	Implement an I/O filter for storing packed integer/float
 *              data using scale and offset method.
 *              
 * Return:	Success: Size of buffer filtered
 *		Failure: 0	
 *
 * Programmer:	Xiaowen Wu
 *              Monday, February 7, 2005
 *
 * Modifications:
 *              
 *-------------------------------------------------------------------------
 */
static size_t
H5Z_filter_scaleoffset (unsigned flags, size_t cd_nelmts, const unsigned cd_values[], 
    size_t nbytes, size_t *buf_size, void **buf)
{
    size_t ret_value = 0;          /* return value */
    size_t size_out  = 0;          /* size of output buffer */
    unsigned d_nelmts = 0;         /* number of data elements in the chunk */
    unsigned minbits = 0;          /* minimum number of bits to store values */
    unsigned long_long minval= 0;  /* minimum value of input buffer */
    unsigned char *outbuf = NULL;  /* pointer to new output buffer */
    parms_atomic p;                /* paramters needed for compress/decompress functions */
    unsigned buf_offset;        /* buffer offset because of parameters stored in file */
 	
    FUNC_ENTER_NOAPI(H5Z_filter_scaleoffset, 0)

    /* check arguments */
    if (cd_nelmts!=H5Z_SCALEOFFSET_TOTAL_NPARMS)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid scaleoffset number of paramters")

    /* copy a filter parameter to d_nelmts */
    d_nelmts = cd_values[H5Z_SCALEOFFSET_PARM_NELMTS];

    /* check and assign value to minimum number of bits if set by user */
    if(cd_values[H5Z_SCALEOFFSET_PARM_MINBITS]!=0) { 
        if(cd_values[H5Z_SCALEOFFSET_PARM_MINBITS]>cd_values[H5Z_SCALEOFFSET_PARM_SIZE]*8)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid scaleoffset minimum number of bits")
        minbits = cd_values[H5Z_SCALEOFFSET_PARM_MINBITS];
    }

    /* prepare paramters to pass to compress/decompress functions */
    p.size = cd_values[H5Z_SCALEOFFSET_PARM_SIZE];
    p.order = cd_values[H5Z_SCALEOFFSET_PARM_ORDER];

    /* input; decompress */
    if (flags & H5Z_FLAG_REVERSE) {
        /* get values of minbits and minval from input buffer */
        HDmemcpy(&minbits, *buf, sizeof(unsigned));
        HDmemcpy(&minval, (unsigned char*)(*buf)+sizeof(unsigned), sizeof(unsigned long_long));

        assert(minbits < p.size * 8);

        /* no need to process the data */
        if(minbits == cd_values[H5Z_SCALEOFFSET_PARM_SIZE] * 8) { 
            ret_value = *buf_size;
            goto done;
        }

        size_out = d_nelmts * p.size;

        /* allocate memory space for decompressed buffer */
        if(NULL==(outbuf = H5MM_malloc(size_out)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for scaleoffset decompression")
  
#if 0
        /* the data buffer all have same value */
        if(minbits == 0) {
        }
#endif

        p.minbits = minbits;

        /* decompress the buffer */
        buf_offset = sizeof(unsigned) + sizeof(unsigned long_long);
        H5Z_scaleoffset_decompress(outbuf, d_nelmts, (unsigned char*)(*buf)+buf_offset, p);   

        /* postprocess after decompress */
        H5Z_scaleoffset_postdecompress(outbuf, d_nelmts, cd_values[H5Z_SCALEOFFSET_PARM_TYPE], 
                                       cd_values[H5Z_SCALEOFFSET_PARM_FILAVAIL], minval);
    }
    /* output; compress */
    else { 
        /* preprocess before compress */
        H5Z_scaleoffset_precompress(*buf, d_nelmts, cd_values[H5Z_SCALEOFFSET_PARM_TYPE], 
                             cd_values[H5Z_SCALEOFFSET_PARM_FILAVAIL], &minbits, &minval);
        
        assert(minbits < p.size * 8);

        /* no need to process the data */
        if(minbits == p.size * 8) { 
            ret_value = *buf_size;
            goto done;
        }
#if 0
        /* the data buffer all have same value */
        if(minbits == 0) {
            *buf_size = 0;
            ret_value = 0;
            goto done;
        }
#endif
        p.minbits = minbits;

        /* calculate buffer size after compression, may be 1 bigger 
         * minbits and minval are stored in the front of the compressed buffer
         */
        size_out = nbytes * p.minbits / (p.size * 8) + 1 + 
                   sizeof(unsigned) + sizeof(unsigned long_long);

        /* allocate memory space for compressed buffer */
        if(NULL==(outbuf = H5MM_malloc(size_out)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for scaleoffset compression")

        /* first store minbits and minval in output buffer */
        HDmemcpy(outbuf, &minbits, sizeof(unsigned));
        HDmemcpy(outbuf+sizeof(unsigned), &minval, sizeof(unsigned long_long));

        /* compress the buffer */
        buf_offset = sizeof(unsigned) + sizeof(unsigned long_long);
        H5Z_scaleoffset_compress(*buf, d_nelmts, outbuf+buf_offset, size_out-buf_offset, p);
#if 0
        printf("After compress:\n");
        for(i = 0; i < size_out; i++) { 
           printf("%02x  ", ((unsigned char *)(outbuf))[i]);
           if((i+1)%4==0) printf("\n");
        }
#endif
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

/* ======== Scaleoffset Algorithm ===============================================
 * assume one byte has 8 bit
 * assume padding bit is 0
 * assume size of unsigned char is one byte
 * assume one data item of certain datatype is stored continously in bytes 
 * atomic datatype is treated on byte basis 
 */

void H5Z_scaleoffset_precompress(void *data, unsigned d_nelmts, unsigned type, unsigned filavail,
                                 unsigned *minbits, unsigned long_long *minval)
{
   unsigned i;
   unsigned long_long span; /* span of values of input buffer data */
   enum H5Z_scaleoffset_type t; /* integer type */

   t = type;
   if(type ==  t_uchar) {
      unsigned char *buf, min, max;
      buf = data; min = max = buf[0];
      for(i = 0; i < d_nelmts; i++) { /* find maximum and minimum values */
         if(buf[i] > max) max = buf[i];
         if(buf[i] < min) min = buf[i];
      }
      span = max - min + 1; *minval = min;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] -= min; 
   } 
   else if(type == t_ushort) {
      unsigned short *buf, min, max;
      buf = data; min = max = buf[0];
      for(i = 0; i < d_nelmts; i++) { /* find maximum and minimum values */
         if(buf[i] > max) max = buf[i];
         if(buf[i] < min) min = buf[i];
      }
      span = max - min + 1; *minval = min;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] -= min;
   }
   else if(type == t_uint) {
      unsigned int *buf, min, max;
      buf = data; min = max = buf[0];
      for(i = 0; i < d_nelmts; i++) { /* find maximum and minimum values */
         if(buf[i] > max) max = buf[i];
         if(buf[i] < min) min = buf[i];
      }
      span = max - min + 1; *minval = min;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] -= min;
   }
   else if(type == t_ulong) {
      unsigned long *buf, min, max;
      buf = data; min = max = buf[0];
      for(i = 0; i < d_nelmts; i++) { /* find maximum and minimum values */
         if(buf[i] > max) max = buf[i];
         if(buf[i] < min) min = buf[i];
      }
      span = max - min + 1; *minval = min;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] -= min;
   } 
   else if(type == t_ulong_long) {
      unsigned long_long *buf, min, max;
      buf = data; min = max = buf[0];
      for(i = 0; i < d_nelmts; i++) { /* find maximum and minimum values */
         if(buf[i] > max) max = buf[i];
         if(buf[i] < min) min = buf[i];
      }
      span = max - min + 1; *minval = min;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] -= min;
   } 
   else if(type == t_schar) {
      signed char *buf, min, max;
      buf = data; min = max = buf[0];
      for(i = 0; i < d_nelmts; i++) { /* find maximum and minimum values */
         if(buf[i] > max) max = buf[i];
         if(buf[i] < min) min = buf[i];
      }
      span = max - min + 1; *minval = min;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] -= min;
   } 
   else if(type == t_short) {
      short *buf, min, max;
      buf = data; min = max = buf[0];
      for(i = 0; i < d_nelmts; i++) { /* find maximum and minimum values */
         if(buf[i] > max) max = buf[i];
         if(buf[i] < min) min = buf[i];
      }
      span = max - min + 1; *minval = min;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] -= min;
   } 
   else if(type == t_int) {
      int *buf, min, max;
      buf = data; min = max = buf[0];
      for(i = 0; i < d_nelmts; i++) { /* find maximum and minimum values */
         if(buf[i] > max) max = buf[i];
         if(buf[i] < min) min = buf[i];
      }
      span = max - min + 1; *minval = min;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] -= min; 
   } else if(type == t_long) {
      long *buf, min, max;
      buf = data; min = max = buf[0];
      for(i = 0; i < d_nelmts; i++) { /* find maximum and minimum values */
         if(buf[i] > max) max = buf[i];
         if(buf[i] < min) min = buf[i];
      }
      span = max - min + 1; *minval = min;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] -= min;
   } 
   else if(type == t_long_long) {
      long_long *buf, min, max;
      buf = data; min = max = buf[0];
      for(i = 0; i < d_nelmts; i++) { /* find maximum and minimum values */
         if(buf[i] > max) max = buf[i];
         if(buf[i] < min) min = buf[i];
      }
      span = max - min + 1; *minval = min;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] -= min;
   }
#if 0
   /* if minbits not set by application, calculate minbits */
   if(*minbits == 0) {
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         *minbits = HDceil(log(span)*M_LOG2E);
      else
         *minbits = HDceil(log(span+1)*M_LOG2E);
   }
#endif
}

void H5Z_scaleoffset_postdecompress(void *data, unsigned d_nelmts, unsigned type, unsigned filavail,
                                    unsigned long_long minval)
{
   unsigned i;
   long_long sminval = *(long_long*)&minval; /* for signed integer types */
   enum H5Z_scaleoffset_type t; /* integer type */

   t = type;
   if(type == t_uchar) {
      unsigned char *buf = data;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] += minval;
   }
   else if(type == t_ushort) {
      unsigned short *buf = data;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] += minval;
   }
   else if(type == t_uint) {
      unsigned int *buf = data;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] += minval;
   }
   else if(type == t_ulong) {
      unsigned long *buf = data;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] += minval;
   }
   else if(type == t_ulong_long) {
      unsigned long_long *buf = data;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] += minval;
   }
   else if(type == t_schar) {
      signed char *buf = data;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] += sminval;
   } 
   else if(type == t_short) {
      short *buf = data;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] += sminval;
   }
   else if(type == t_int) {
      int *buf = data;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] += sminval;
   }
   else if(type == t_long) {
      long *buf = data;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] += sminval;
   }
   else if(type == t_long_long) {
      long_long *buf = data;
      if(filavail == H5Z_SCALEOFFSET_FILL_UNDEFINED)
         for(i = 0; i < d_nelmts; i++) buf[i] += sminval;
   } 
}

void H5Z_scaleoffset_next_byte(size_t *j, int *buf_len)
{
   ++(*j); 
   *buf_len = 8 * sizeof(unsigned char);
}

void H5Z_scaleoffset_decompress_one_byte(void *data, size_t data_offset, int k, int begin_i, 
unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p)
{
   int dat_len; /* dat_len is the number of bits to be copied in each data byte */
   unsigned char val; /* value to be copied in each data byte */

   /* initialize value and bits of unsigned char to be copied */
   val = buffer[*j];

   if(k == begin_i) 
      dat_len = p.minbits % 8;
   else 
      dat_len = 8;

   if(*buf_len > dat_len) { 
      ((unsigned char *)data)[data_offset + k] =
      ((val >> (*buf_len - dat_len)) & ~(~0 << dat_len));
      *buf_len -= dat_len;
   } else {
      ((unsigned char *)data)[data_offset + k] =
      ((val & ~(~0 << *buf_len)) << (dat_len - *buf_len));
      dat_len -= *buf_len;
      H5Z_scaleoffset_next_byte(j, buf_len);
      if(dat_len == 0) return;

      val = buffer[*j];
      ((unsigned char *)data)[data_offset + k] |=
      ((val >> (*buf_len - dat_len)) & ~(~0 << dat_len));
      *buf_len -= dat_len; 
   }
}

void H5Z_scaleoffset_decompress_one_atomic(void *data, size_t data_offset, unsigned char *buffer, 
                                           size_t *j, int *buf_len, parms_atomic p) 
{
   /* begin_i: the index of byte having first significant bit */
   int k, begin_i, dtype_len;

   assert(p.minbits > 0);

   dtype_len = p.size * 8;

   if(p.order == H5Z_SCALEOFFSET_ORDER_LE) { /* little endian */
      begin_i = p.size - 1 - (dtype_len - p.minbits) / 8;

      for(k = begin_i; k >= 0; k--) 
         H5Z_scaleoffset_decompress_one_byte(data, data_offset, k, begin_i, 
                                             buffer, j, buf_len, p);
   }

   if(p.order == H5Z_SCALEOFFSET_ORDER_BE) { /* big endian */
      begin_i = (dtype_len - p.minbits) / 8;

      for(k = begin_i; k <= p.size - 1; k++) 
         H5Z_scaleoffset_decompress_one_byte(data, data_offset, k, begin_i,  
                                             buffer, j, buf_len, p);
   } 
}

void H5Z_scaleoffset_decompress(void *data, unsigned d_nelmts, unsigned char *buffer, 
                                parms_atomic p) 
{
   /* i: index of data, j: index of buffer, 
      buf_len: number of bits to be filled in current byte */
   size_t i, j;
   int buf_len; 

   /* may not have to initialize to zeros */  
   for(i = 0; i < d_nelmts*p.size; i++)  
      ((unsigned char *)data)[i] = 0; 
   
   /* initialization before the loop */ 
   j = 0; 
   buf_len = sizeof(unsigned char) * 8; 

   /* decompress */
   for(i = 0; i < d_nelmts; i++) 
      H5Z_scaleoffset_decompress_one_atomic(data, i*p.size, buffer, &j, &buf_len, p);
}

void H5Z_scaleoffset_compress_one_byte(void *data, size_t data_offset, int k, int begin_i, 
unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p)
{
   int dat_len; /* dat_len is the number of bits to be copied in each data byte */
   unsigned char val; /* value to be copied in each data byte */

   /* initialize value and bits of unsigned char to be copied */
   val = ((unsigned char *)data)[data_offset + k];

   if(k == begin_i) 
      dat_len = p.minbits % 8;
   else
      dat_len = 8;

   if(*buf_len > dat_len) { 
      buffer[*j] |= (val & ~(~0 << dat_len)) << (*buf_len - dat_len);
      *buf_len -= dat_len;
   } else {
      buffer[*j] |= (val >> (dat_len - *buf_len)) & ~(~0 << *buf_len);
      dat_len -= *buf_len;
      H5Z_scaleoffset_next_byte(j, buf_len);
      if(dat_len == 0) return;

      buffer[*j] = (val & ~(~0 << dat_len)) << (*buf_len - dat_len);
      *buf_len -= dat_len; 
   }
}

void H5Z_scaleoffset_compress_one_atomic(void *data, size_t data_offset, unsigned char *buffer, 
                                         size_t *j, int *buf_len, parms_atomic p) 
{
   /* begin_i: the index of byte having first significant bit */
   int k, begin_i, dtype_len;

   assert(p.minbits > 0);

   dtype_len = p.size * 8;

   if(p.order == H5Z_SCALEOFFSET_ORDER_LE) { /* little endian */
      begin_i = p.size - 1 - (dtype_len - p.minbits) / 8;

      for(k = begin_i; k >= 0; k--) 
         H5Z_scaleoffset_compress_one_byte(data, data_offset, k, begin_i,  
                                           buffer, j, buf_len, p);
   }

   if(p.order == H5Z_SCALEOFFSET_ORDER_BE) { /* big endian */
      begin_i = (dtype_len - p.minbits) / 8;

      for(k = begin_i; k <= p.size - 1; k++) 
         H5Z_scaleoffset_compress_one_byte(data, data_offset, k, begin_i,  
                                           buffer, j, buf_len, p);
   }    
}

void H5Z_scaleoffset_compress(void *data, unsigned d_nelmts, unsigned char *buffer, 
                              size_t buffer_size, parms_atomic p) 
{
   /* i: index of data, j: index of buffer, 
      buf_len: number of bits to be filled in current byte */
   size_t i, j;
   int buf_len; 

   /* must initialize buffer to be zeros */
   for(j = 0; j < buffer_size; j++) 
      buffer[j] = 0;

   /* initialization before the loop */ 
   j = 0; 
   buf_len = sizeof(unsigned char) * 8; 

   /* compress */
   for(i = 0; i < d_nelmts; i++) 
       H5Z_scaleoffset_compress_one_atomic(data, i*p.size, buffer, &j, &buf_len, p);
}
#endif /* H5_HAVE_FILTER_SCALEOFFSET */
