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
   size_t size;        /* datatype size */
   uint32_t minbits;   /* minimum bits to compress one value of such datatype */
   unsigned mem_order; /* current memory endianness order */
} parms_atomic; 

enum H5Z_scaleoffset_type {t_uchar=1, t_ushort, t_uint, t_ulong, t_ulong_long,
                           t_schar, t_short, t_int, t_long, t_long_long};

/* Local function prototypes */
static herr_t H5Z_can_apply_scaleoffset(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static enum H5Z_scaleoffset_type H5Z_scaleoffset_get_type(unsigned dtype_size, unsigned dtype_sign);
static herr_t H5Z_scaleoffset_set_parms_fillval(hid_t dcpl_id, hid_t type_id, 
      enum H5Z_scaleoffset_type type, unsigned cd_values[], int need_convert);
static herr_t H5Z_set_local_scaleoffset(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static size_t H5Z_filter_scaleoffset(unsigned flags, size_t cd_nelmts, const unsigned cd_values[], 
                                     size_t nbytes, size_t *buf_size, void **buf);
static void H5Z_scaleoffset_convert(void *buf, unsigned d_nelmts, unsigned dtype_size);
static unsigned H5Z_scaleoffset_log2(unsigned long_long num);
static void 
H5Z_scaleoffset_precompress(void *data, unsigned d_nelmts, enum H5Z_scaleoffset_type type, 
       unsigned filavail, void *filval_buf, uint32_t *minbits, unsigned long_long *minval);
static void 
H5Z_scaleoffset_postdecompress(void *data, unsigned d_nelmts, enum H5Z_scaleoffset_type type, 
            unsigned filavail, void *filval_buf, uint32_t minbits, unsigned long_long minval);
static void H5Z_scaleoffset_next_byte(size_t *j, int *buf_len);
static void H5Z_scaleoffset_decompress_one_byte(unsigned char *data, size_t data_offset, int k, 
int begin_i, unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p, int dtype_len);
static void H5Z_scaleoffset_compress_one_byte(unsigned char *data, size_t data_offset, int k, 
int begin_i, unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p, int dtype_len);
static void H5Z_scaleoffset_decompress_one_atomic(unsigned char *data, size_t data_offset, 
                      unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p);
static void H5Z_scaleoffset_compress_one_atomic(unsigned char *data, size_t data_offset, 
                         unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p);
static void H5Z_scaleoffset_decompress(unsigned char *data, unsigned d_nelmts, 
                                       unsigned char *buffer, parms_atomic p);
static void H5Z_scaleoffset_compress(unsigned char *data, unsigned d_nelmts, unsigned char *buffer,
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
#define H5Z_SCALEOFFSET_TOTAL_NPARMS    20   /* Total number of parameters for filter */
#define H5Z_SCALEOFFSET_PARM_MINBITS    0    /* "User" parameter for minimum number of bits */
#define H5Z_SCALEOFFSET_PARM_NELMTS     1    /* "Local" parameter for number of elements in the chunk */
#define H5Z_SCALEOFFSET_PARM_CLASS      2    /* "Local" parameter for datatype class */
#define H5Z_SCALEOFFSET_PARM_SIZE       3    /* "Local" parameter for datatype size */
#define H5Z_SCALEOFFSET_PARM_SIGN       4    /* "Local" parameter for integer datatype sign */
#define H5Z_SCALEOFFSET_PARM_ORDER      5    /* "Local" parameter for datatype byte order */
#define H5Z_SCALEOFFSET_PARM_FILAVAIL   6    /* "Local" parameter for dataset fill value existence */
#define H5Z_SCALEOFFSET_PARM_FILVAL     7    /* "Local" parameter for start location to store dataset fill value */

#define H5Z_SCALEOFFSET_CLS_INTEGER     0    /* Integer (datatype class) */
#define H5Z_SCALEOFFSET_CLS_FLOAT       1    /* Floatig-point (datatype class) */

#define H5Z_SCALEOFFSET_SGN_NONE        0    /* Unsigned integer type */
#define H5Z_SCALEOFFSET_SGN_2           1    /* Two's complement signed integer type */

#define H5Z_SCALEOFFSET_ORDER_LE        0    /* Little endian (datatype byte order) */
#define H5Z_SCALEOFFSET_ORDER_BE        1    /* Big endian (datatype byte order) */

#define H5Z_SCALEOFFSET_FILL_UNDEFINED  0    /* Fill value is not defined */
#define H5Z_SCALEOFFSET_FILL_DEFINED    1    /* Fill value is defined */

/* Set the fill value parameter in cd_values[] for unsigned integer type */
#define H5Z_scaleoffset_set_filval_1(type, dcpl_id, type_id, cd_values, need_convert)\
{                                                                                    \
    type fill_val;                                                                   \
                                                                                     \
    /* Get dataset fill value */                                                     \
    if(H5Pget_fill_value(dcpl_id, type_id, &fill_val)<0)                             \
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get fill value")        \
                                                                                     \
    if(need_convert)                                                                 \
       H5Z_scaleoffset_convert(&fill_val, 1, sizeof(type));                          \
                                                                                     \
    /* Store the fill value as the last entry in cd_values[]                         \
     * Store byte by byte from least significant byte to most significant byte       \
     * Plenty of space left for the fill value (from index 7 to 19)                  \
     */                                                                              \
    for(i = 0; i < sizeof(type); i++)                                                \
        ((unsigned char *)&cd_values[H5Z_SCALEOFFSET_PARM_FILVAL])[i] =              \
        (fill_val & ((type)0xff << i*8)) >> i*8;                                     \
}

/* Set the fill value parameter in cd_values[] for signed integer type */
#define H5Z_scaleoffset_set_filval_2(type, dcpl_id, type_id, cd_values, need_convert)\
{                                                                                    \
    type fill_val;                                                                   \
                                                                                     \
    /* Get dataset fill value */                                                     \
    if(H5Pget_fill_value(dcpl_id, type_id, &fill_val)<0)                             \
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get fill value")        \
                                                                                     \
    if(need_convert)                                                                 \
       H5Z_scaleoffset_convert(&fill_val, 1, sizeof(type));                          \
                                                                                     \
    /* Store the fill value as the last entry in cd_values[]                         \
     * Store byte by byte from least significant byte to most significant byte       \
     * Plenty of space left for the fill value (from index 7 to 19)                  \
     */                                                                              \
    for(i = 0; i < sizeof(type); i++)                                                \
        ((unsigned char *)&cd_values[H5Z_SCALEOFFSET_PARM_FILVAL])[i] =              \
        (fill_val & ((unsigned type)0xff << i*8)) >> i*8;                            \
}

/* Set the fill value parameter in cd_values[] for character integer type */
#define H5Z_scaleoffset_set_filval_3(type, dcpl_id, type_id, cd_values, need_convert)\
{                                                                                    \
    type fill_val;                                                                   \
                                                                                     \
    /* Get dataset fill value */                                                     \
    if(H5Pget_fill_value(dcpl_id, type_id, &fill_val)<0)                             \
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get fill value")        \
                                                                                     \
    /* Store the fill value as the last entry in cd_values[] */                      \
    ((unsigned char *)&cd_values[H5Z_SCALEOFFSET_PARM_FILVAL])[0] = fill_val;        \
}

/* Get the fill value for certain type */
#define H5Z_scaleoffset_get_fill_val(i, type, filval_buf, filval, filval_mask)\
{                                                                             \
   /* retrieve fill value from corresponding positions of cd_values[]         \
    * retrieve them corresponding to how they are stored                      \
    */                                                                        \
   for(i = 0; i < sizeof(type); i++) {                                        \
      filval_mask = ((unsigned char *)filval_buf)[i];                         \
      filval_mask <<= i*8;                                                    \
      filval |= filval_mask;                                                  \
   }                                                                          \
}

/* Find maximum and minimum values of a buffer with fill value defined */
#define H5Z_scaleoffset_max_min_1(i, d_nelmts, buf, filval, max, min)\
{                                                                  \
   i = 0; while(i < d_nelmts && buf[i]== filval) i++;              \
   if(i < d_nelmts) min = max = buf[i];                            \
   for(; i < d_nelmts; i++) {                                      \
      if(buf[i] == filval) continue; /* ignore fill value */       \
      if(buf[i] > max) max = buf[i];                               \
      if(buf[i] < min) min = buf[i];                               \
   }                                                               \
}

/* Find maximum and minimum values of a buffer with fill value undefined */
#define H5Z_scaleoffset_max_min_2(i, d_nelmts, buf, max, min)\
{                                                            \
   min = max = buf[0];                                       \
   for(i = 0; i < d_nelmts; i++) {                           \
      if(buf[i] > max) max = buf[i];                         \
      if(buf[i] < min) min = buf[i];                         \
   }                                                         \
}

/* Find minimum value of a buffer with fill value defined */
#define H5Z_scaleoffset_min_1(i, d_nelmts, buf, filval, min) \
{                                                            \
   i = 0; while(i < d_nelmts && buf[i]== filval) i++;        \
   if(i < d_nelmts) min = buf[i];                            \
   for(; i < d_nelmts; i++) {                                \
      if(buf[i] == filval) continue; /* ignore fill value */ \
      if(buf[i] < min) min = buf[i];                         \
   }                                                         \
}

/* Find minimum value of a buffer with fill value undefined */
#define H5Z_scaleoffset_min_2(i, d_nelmts, buf, min)\
{                                                   \
   min = buf[0];                                    \
   for(i = 0; i < d_nelmts; i++)                    \
      if(buf[i] < min) min = buf[i];                \
}

/* Check range and do some operation for unsigned integer type */
#define H5Z_scaleoffset_check_1(type, max, min, minbits, minval)\
{                                                               \
   if(max - min > (type)(~(type)0 - 2)) {                       \
       *minbits = sizeof(type)*8;                               \
       *minval = min;                                           \
       return;                                                  \
   }                                                            \
}

/* Check range and do some operation for signed integer type */
#define H5Z_scaleoffset_check_2(type, max, min, minbits, minval)             \
{                                                                            \
   if((unsigned type)(max - min) > (unsigned type)(~(unsigned type)0 - 2)) { \
       *minbits = sizeof(type)*8;                                            \
       *minval = min;                                                        \
       return;                                                               \
   }                                                                         \
}

/* Precompress for unsigned integer type */
#define H5Z_scaleoffset_precompress_1(type, data, d_nelmts, filavail, filval_buf, minbits, minval)\
{                                                                                      \
   type *buf = data, min = 0, max = 0, span, filval = 0, filval_mask = 0;              \
                                                                                       \
   if(filavail == H5Z_SCALEOFFSET_FILL_DEFINED) { /* fill value defined */             \
      H5Z_scaleoffset_get_fill_val(i, type, filval_buf, filval, filval_mask);          \
      if(*minbits == 0) { /* minbits not set yet, calculate max, min, and minbits */   \
         H5Z_scaleoffset_max_min_1(i, d_nelmts, buf, filval, max, min)                 \
         H5Z_scaleoffset_check_1(type, max, min, minbits, minval)                      \
         span = max - min + 1;                                                         \
         *minbits = H5Z_scaleoffset_log2(span+1);                                      \
      } else /* minbits already set, only calculate min */                             \
         H5Z_scaleoffset_min_1(i, d_nelmts, buf, filval, min)                          \
      *minval = min;                                                                   \
      if(*minbits != sizeof(type)*8) /* change values if minbits != full precision */  \
         for(i = 0; i < d_nelmts; i++)                                                 \
            buf[i] = (buf[i] == filval)?(((type)1 << *minbits) - 1):(buf[i] - min);    \
   } else { /* fill value undefined */                                                 \
      if(*minbits == 0) { /* minbits not set yet, calculate max, min, and minbits */   \
         H5Z_scaleoffset_max_min_2(i, d_nelmts, buf, max, min)                         \
         H5Z_scaleoffset_check_1(type, max, min, minbits, minval)                      \
         span = max - min + 1;                                                         \
         *minbits = H5Z_scaleoffset_log2(span);                                        \
      } else /* minbits already set, only calculate min */                             \
         H5Z_scaleoffset_min_2(i, d_nelmts, buf, min)                                  \
      *minval = min;                                                                   \
      if(*minbits != sizeof(type)*8) /* change values if minbits != full precision */  \
         for(i = 0; i < d_nelmts; i++) buf[i] -= min;                                  \
   }                                                                                   \
}

/* Precompress for signed integer type */
#define H5Z_scaleoffset_precompress_2(type, data, d_nelmts, filavail, filval_buf, minbits, minval)\
{                                                                                            \
   type *buf = data, min = 0, max = 0, filval = 0, filval_mask = 0;                          \
   unsigned type span;                                                                       \
                                                                                             \
   if(filavail == H5Z_SCALEOFFSET_FILL_DEFINED) { /* fill value defined */                   \
      H5Z_scaleoffset_get_fill_val(i, type, filval_buf, filval, filval_mask);                \
      if(*minbits == 0) { /* minbits not set yet, calculate max, min, and minbits */         \
         H5Z_scaleoffset_max_min_1(i, d_nelmts, buf, filval, max, min)                       \
         H5Z_scaleoffset_check_2(type, max, min, minbits, minval)                            \
         span = max - min + 1;                                                               \
         *minbits = H5Z_scaleoffset_log2(span+1);                                            \
      } else /* minbits already set, only calculate min */                                   \
         H5Z_scaleoffset_min_1(i, d_nelmts, buf, filval, min)                                \
      *minval = min;                                                                         \
      if(*minbits != sizeof(type)*8) /* change values if minbits != full precision */        \
         for(i = 0; i < d_nelmts; i++)                                                       \
            buf[i] = (buf[i] == filval)?(((unsigned type)1 << *minbits) - 1):(buf[i] - min); \
   } else { /* fill value undefined */                                                       \
      if(*minbits == 0) { /* minbits not set yet, calculate max, min, and minbits */         \
         H5Z_scaleoffset_max_min_2(i, d_nelmts, buf, max, min)                               \
         H5Z_scaleoffset_check_2(type, max, min, minbits, minval)                            \
         span = max - min + 1;                                                               \
         *minbits = H5Z_scaleoffset_log2(span);                                              \
      } else /* minbits already set, only calculate min */                                   \
         H5Z_scaleoffset_min_2(i, d_nelmts, buf, min)                                        \
      *minval = min;                                                                         \
      if(*minbits != sizeof(type)*8) /* change values if minbits != full precision */        \
         for(i = 0; i < d_nelmts; i++) buf[i] -= min;                                        \
   }                                                                                         \
}

/* Postdecompress for unsigned integer type */
#define H5Z_scaleoffset_postdecompress_1(type, data, d_nelmts, filavail, filval_buf, minbits, minval)\
{                                                                                 \
   type *buf = data, filval = 0, filval_mask = 0;                                 \
                                                                                  \
   if(filavail == H5Z_SCALEOFFSET_FILL_DEFINED) { /* fill value defined */        \
      H5Z_scaleoffset_get_fill_val(i, type, filval_buf, filval, filval_mask)      \
      for(i = 0; i < d_nelmts; i++)                                               \
         buf[i] = (buf[i] == (((type)1 << minbits) - 1))?filval:(buf[i] + minval);\
   } else /* fill value undefined */                                              \
      for(i = 0; i < d_nelmts; i++) buf[i] += minval;                             \
}

/* Postdecompress for signed integer type */
#define H5Z_scaleoffset_postdecompress_2(type, data, d_nelmts, filavail, filval_buf, minbits, minval)\
{                                                                                          \
   type *buf = data, filval = 0, filval_mask = 0;                                          \
                                                                                           \
   if(filavail == H5Z_SCALEOFFSET_FILL_DEFINED) { /* fill value defined */                 \
      H5Z_scaleoffset_get_fill_val(i, type, filval_buf, filval, filval_mask)               \
      for(i = 0; i < d_nelmts; i++)                                                        \
         buf[i] = (buf[i] == (((unsigned type)1 << minbits) - 1))?filval:(buf[i] + minval);\
   } else /* fill value undefined */                                                       \
      for(i = 0; i < d_nelmts; i++) buf[i] += minval;                                      \
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_can_apply_scaleoffset
 *
 * Purpose:	Check the parameters for scaleoffset compression for 
 *              validity and whether they fit a particular dataset.
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

    if(dtype_class == H5T_INTEGER || dtype_class == H5T_FLOAT) {
        /* Get datatype's endianness order */
        if((dtype_order = H5Tget_order(type_id)) == H5T_ORDER_ERROR)
	    HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "can't retrieve datatype endianness order")

        /* Range check datatype's endianness order */
        if(dtype_order != H5T_ORDER_LE && dtype_order != H5T_ORDER_BE)
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype endianness order")
    } else
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "datatype class not supported by scaleoffset")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_can_apply_scaleoffset() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_scaleoffset_get_type
 *
 * Purpose:	Get the specific integer type based on datatype size and sign
 *              
 * Return:	Success: id number of integer type
 *		Failure: 0	
 *
 * Programmer:	Xiaowen Wu
 *              Saturday, February 26, 2005
 *
 * Modifications:
 *              
 *-------------------------------------------------------------------------
 */
static enum H5Z_scaleoffset_type H5Z_scaleoffset_get_type(unsigned dtype_size, unsigned dtype_sign)
{
    enum H5Z_scaleoffset_type type; /* integer type */
    unsigned ret_value;             /* return value */

    FUNC_ENTER_NOAPI(H5Z_scaleoffset_get_type, 0)

    if(dtype_sign==H5Z_SCALEOFFSET_SGN_NONE) { /* unsigned integer */
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
          HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, 0, "integer size not matched")
    }

    if(dtype_sign==H5Z_SCALEOFFSET_SGN_2) { /* signed integer */
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
          HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, 0, "integer size not matched")
    }

done:
    ret_value = type;
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_scaleoffset_set_parms_fillval
 *
 * Purpose:	Get the fill value of the dataset and store in cd_values[] 
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:  Xiaowen Wu
 *              Monday, March 7, 2005
 *
 * Modifications: 
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5Z_scaleoffset_set_parms_fillval(hid_t dcpl_id, hid_t type_id, 
      enum H5Z_scaleoffset_type type, unsigned cd_values[], int need_convert)
{
    unsigned i;                        /* index */
    herr_t ret_value=SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI(H5Z_scaleoffset_set_parms_fillval, FAIL)

    if(type == t_uchar)
        H5Z_scaleoffset_set_filval_3(unsigned char, dcpl_id, type_id, 
                                     cd_values, need_convert)
    else if(type == t_ushort) 
        H5Z_scaleoffset_set_filval_1(unsigned short, dcpl_id, type_id, 
                                     cd_values, need_convert)
    else if(type == t_uint) 
        H5Z_scaleoffset_set_filval_1(unsigned int, dcpl_id, type_id, 
                                     cd_values, need_convert)
    else if(type == t_ulong) 
        H5Z_scaleoffset_set_filval_1(unsigned long, dcpl_id, type_id, 
                                     cd_values, need_convert)
    else if(type == t_ulong_long) 
        H5Z_scaleoffset_set_filval_1(unsigned long_long, dcpl_id, type_id, 
                                     cd_values, need_convert)
    else if(type == t_schar) 
        H5Z_scaleoffset_set_filval_3(signed char, dcpl_id, type_id, 
                                     cd_values, need_convert)
    else if(type == t_short)
        H5Z_scaleoffset_set_filval_2(short, dcpl_id, type_id, 
                                     cd_values, need_convert)
    else if(type == t_int) 
        H5Z_scaleoffset_set_filval_2(int, dcpl_id, type_id, 
                                     cd_values, need_convert)
    else if(type == t_long) 
        H5Z_scaleoffset_set_filval_2(long, dcpl_id, type_id, 
                                     cd_values, need_convert)
    else if(type == t_long_long) 
        H5Z_scaleoffset_set_filval_2(long_long, dcpl_id, type_id, 
                                     cd_values, need_convert)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_scaleoffset_set_parms_fillval() */


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
    H5T_order_t mem_order;          /* Memory's endianness order */
    int need_convert = FALSE;       /* Flag indicating convertion of byte order */
    size_t dtype_size;              /* Datatype's size (in bytes) */
    H5T_sign_t dtype_sign;          /* Datatype's sign */
    enum H5Z_scaleoffset_type type; /* integer type */
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

    if(dtype_class==H5T_INTEGER) {
        /* Get datatype's sign */
        if((dtype_sign=H5Tget_sign(type_id))==H5T_SGN_ERROR)
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype sign")

        /* Set "local" parameter for integer datatype sign */
        switch(dtype_sign) {
            case H5T_SGN_NONE:
                cd_values[H5Z_SCALEOFFSET_PARM_SIGN] = H5Z_SCALEOFFSET_SGN_NONE;
                break;
            case H5T_SGN_2:
                cd_values[H5Z_SCALEOFFSET_PARM_SIGN] = H5Z_SCALEOFFSET_SGN_2;
                break;
            default:
                HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad integer sign")
        } /* end switch */
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

        /* Get memory's endianness order */
        if((mem_order=H5Tget_order(H5T_NATIVE_INT))==H5T_ORDER_ERROR)
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad H5T_NATIVE_INT endianness order")

        /* Check if memory byte order matches dataset datatype byte order */
        switch(mem_order) {
            case H5T_ORDER_LE:      /* memory is little-endian byte order */
                if(dtype_order == H5T_ORDER_BE)
                    need_convert = TRUE;
                break;
            case H5T_ORDER_BE:      /* memory is big-endian byte order */
                if(dtype_order == H5T_ORDER_LE)
                    need_convert = TRUE;
                break;
            default:
                HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad H5T_NATIVE_INT endianness order")
        } /* end switch */

        if(dtype_class==H5T_INTEGER) {
            /* Before getting fill value, get its integer type */
            if((type = H5Z_scaleoffset_get_type(cd_values[H5Z_SCALEOFFSET_PARM_SIZE], 
                                                cd_values[H5Z_SCALEOFFSET_PARM_SIGN]))==0)
                HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "cannot use C integer datatype for cast")

            /* Get dataset fill value and store in cd_values[] */
            if(H5Z_scaleoffset_set_parms_fillval(dcpl_id, type_id, type, cd_values, need_convert)<0)
                HGOTO_ERROR(H5E_PLINE, H5E_CANTSET, FAIL, "unable to set fill value")
        }
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
 * Purpose:	Implement an I/O filter for storing packed integer
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
    size_t ret_value = 0;           /* return value */
    size_t size_out  = 0;           /* size of output buffer */
    unsigned d_nelmts = 0;          /* number of data elements in the chunk */
    enum H5Z_scaleoffset_type type; /* integer type */
    H5T_order_t mem_order;          /* memory's endianness order */
    int need_convert = FALSE;       /* flag indicating convertion of byte order */
    uint32_t minbits = 0;           /* minimum number of bits to store values */
    unsigned long_long minval= 0;   /* minimum value of input buffer */
    unsigned char *outbuf = NULL;   /* pointer to new output buffer */
    unsigned buf_offset = 21;       /* buffer offset because of parameters stored in file */
    unsigned i;                     /* index */
    parms_atomic p;                 /* paramters needed for compress/decompress functions */
 	
    FUNC_ENTER_NOAPI(H5Z_filter_scaleoffset, 0)

    /* check arguments */
    if (cd_nelmts!=H5Z_SCALEOFFSET_TOTAL_NPARMS)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid scaleoffset number of paramters")

    /* do not process if datatype is floating-point, will change later */
    if(cd_values[H5Z_SCALEOFFSET_PARM_CLASS]==H5Z_SCALEOFFSET_CLS_FLOAT) {
            ret_value = *buf_size;
            goto done;
    }

    /* get memory's endianness order */
    if((mem_order=H5Tget_order(H5T_NATIVE_INT))==H5T_ORDER_ERROR)
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, 0, "bad H5T_NATIVE_INT endianness order")

    /* check if memory byte order matches dataset datatype byte order */
    switch(mem_order) {
        case H5T_ORDER_LE:      /* memory is little-endian byte order */
            if(cd_values[H5Z_SCALEOFFSET_PARM_ORDER] == H5Z_SCALEOFFSET_ORDER_BE)
                need_convert = TRUE;
            break;
        case H5T_ORDER_BE:      /* memory is big-endian byte order */
            if(cd_values[H5Z_SCALEOFFSET_PARM_ORDER] == H5Z_SCALEOFFSET_ORDER_LE)
                need_convert = TRUE;
            break;
        default:
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, 0, "bad H5T_NATIVE_INT endianness order")
    } /* end switch */

    /* check and assign value to minimum number of bits if set by user */
    if(cd_values[H5Z_SCALEOFFSET_PARM_MINBITS]!=0) { 
        if(cd_values[H5Z_SCALEOFFSET_PARM_MINBITS]>cd_values[H5Z_SCALEOFFSET_PARM_SIZE]*8)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "minimum number of bits exceeds maximum")

        /* no need to process data */
        if(cd_values[H5Z_SCALEOFFSET_PARM_MINBITS]==cd_values[H5Z_SCALEOFFSET_PARM_SIZE]*8) {
            ret_value = *buf_size;
            goto done;
        }
        minbits = cd_values[H5Z_SCALEOFFSET_PARM_MINBITS];
    }

    /* copy a filter parameter to d_nelmts */
    d_nelmts = cd_values[H5Z_SCALEOFFSET_PARM_NELMTS];

    /* prepare paramters to pass to compress/decompress functions */
    p.size = cd_values[H5Z_SCALEOFFSET_PARM_SIZE];
    p.mem_order = mem_order;

    /* input; decompress */
    if (flags & H5Z_FLAG_REVERSE) {
        /* retrieve values of minbits and minval from input compressed buffer
         * retrieve them corresponding to how they are stored during compression
         */
        uint32_t minbits_mask = 0;
        unsigned long_long minval_mask = 0;
        unsigned minval_size = 0;

        minbits = 0;
        for(i = 0; i < 4; i++) {
            minbits_mask = ((unsigned char *)*buf)[i];
            minbits_mask <<= i*8;
            minbits |= minbits_mask;
        }

        /* retrieval of minval takes into consideration situation where sizeof
         * unsigned long_long (datatype of minval) may change from compression
         * to decompression
         */
        minval_size = sizeof(unsigned long_long) <= ((unsigned char *)*buf)[4] ?
                      sizeof(unsigned long_long) : ((unsigned char *)*buf)[4];
        minval = 0;
        for(i = 0; i < minval_size; i++) {
            minval_mask = ((unsigned char *)*buf)[5+i];
            minval_mask <<= i*8;
            minval |= minval_mask;
        }

        assert(minbits <= p.size * 8);
        p.minbits = minbits;

        /* calculate size of output buffer after decompression */
        size_out = d_nelmts * p.size;

        /* allocate memory space for decompressed buffer */
        if(NULL==(outbuf = H5MM_malloc(size_out)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for scaleoffset decompression")

        /* special case: minbits equal to full precision */
        if(minbits == p.size * 8) { 
            HDmemcpy(outbuf, (unsigned char*)(*buf)+buf_offset, size_out);

            /* convert to dataset datatype endianness order if needed */
            if(need_convert) 
                H5Z_scaleoffset_convert(outbuf, d_nelmts, p.size);

            *buf = outbuf;
            outbuf = NULL;
            *buf_size = size_out;
            ret_value = size_out;
            goto done;
        }

        /* decompress the buffer if minbits not equal to zero */ 
        if(minbits != 0) 
            H5Z_scaleoffset_decompress(outbuf, d_nelmts, (unsigned char*)(*buf)+buf_offset, p);   
        else { 
            /* fill value is not defined and all data elements have the same value */  
            for(i = 0; i < size_out; i++) outbuf[i] = 0;
        }

        /* before postprocess, get integer type */
        if((type = H5Z_scaleoffset_get_type(cd_values[H5Z_SCALEOFFSET_PARM_SIZE], 
                                            cd_values[H5Z_SCALEOFFSET_PARM_SIGN]))==0)
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, 0, "cannot use C integer datatype for cast")

        /* postprocess after decompression */
        H5Z_scaleoffset_postdecompress(outbuf, d_nelmts, type, cd_values[H5Z_SCALEOFFSET_PARM_FILAVAIL],
                                       &cd_values[H5Z_SCALEOFFSET_PARM_FILVAL], minbits, minval);

        /* after postprocess, convert to dataset datatype endianness order if needed */
        if(need_convert) 
            H5Z_scaleoffset_convert(outbuf, d_nelmts, p.size);
    }
    /* output; compress */
    else { 
        assert(nbytes == d_nelmts * p.size);

        /* before preprocess, convert to memory endianness order if needed */
        if(need_convert) 
            H5Z_scaleoffset_convert(*buf, d_nelmts, p.size);

        /* before preprocess, get integer type */
        if((type = H5Z_scaleoffset_get_type(cd_values[H5Z_SCALEOFFSET_PARM_SIZE], 
                                            cd_values[H5Z_SCALEOFFSET_PARM_SIGN]))==0)
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, 0, "cannot use C integer datatype for cast")

        /* preprocess before compression */
        H5Z_scaleoffset_precompress(*buf, d_nelmts, type, cd_values[H5Z_SCALEOFFSET_PARM_FILAVAIL], 
                                    &cd_values[H5Z_SCALEOFFSET_PARM_FILVAL], &minbits, &minval);
        
        assert(minbits <= p.size * 8);

        /* calculate buffer size after compression
         * minbits and minval are stored in the front of the compressed buffer
         */
        p.minbits = minbits;
        size_out = buf_offset + nbytes * p.minbits / (p.size * 8) + 1; /* may be 1 larger */

        /* allocate memory space for compressed buffer */
        if(NULL==(outbuf = H5MM_malloc(size_out)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for scaleoffset compression")

        /* store minbits and minval in the front of output compressed buffer
         * store byte by byte from least significant byte to most significant byte
         * constant buffer size (21 bytes) is left for these two parameters
         * 4 bytes for minbits, 1 byte for size of minval, 16 bytes for minval
         */
        for(i = 0; i < 4; i++)
            ((unsigned char *)outbuf)[i] = (minbits & ((uint32_t)0xff << i*8)) >> i*8;

        ((unsigned char *)outbuf)[4] = sizeof(unsigned long_long);

        for(i = 0; i < sizeof(unsigned long_long); i++)
            ((unsigned char *)outbuf)[5+i] = (minval & ((unsigned long_long)0xff << i*8)) >> i*8;

        /* special case: minbits equal to full precision */
        if(minbits == p.size * 8) {
            HDmemcpy(outbuf+buf_offset, *buf, nbytes);
            *buf = outbuf;
            outbuf = NULL;
            *buf_size = buf_offset+nbytes;
            ret_value = *buf_size;
            goto done;
        }

        /* compress the buffer if minbits not equal to zero 
         * minbits equal to zero only when fill value is not defined and 
         * all data elements have the same value 
         */  
        if(minbits != 0)
            H5Z_scaleoffset_compress(*buf, d_nelmts, outbuf+buf_offset, size_out-buf_offset, p);
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

/* ============ Scaleoffset Algorithm ===============================================
 * assume one byte has 8 bit
 * assume padding bit is 0
 * assume size of unsigned char is one byte
 * assume one data item of certain datatype is stored continously in bytes 
 * atomic datatype is treated on byte basis 
 */


/* change byte order of input buffer either from little-endian to big-endian
 * or from big-endian to little-endian  2/21/2005 
 */
static void H5Z_scaleoffset_convert(void *buf, unsigned d_nelmts, unsigned dtype_size)
{
   unsigned i, j;
   unsigned char *buffer, temp;

   if(dtype_size == 1) return;

   buffer = buf;
   for(i = 0; i < d_nelmts * dtype_size; i += dtype_size)
      for(j = 0; j < dtype_size/2; j++) {
         /* swap pair of bytes */
         temp = buffer[i+j];
         buffer[i+j] = buffer[i+dtype_size-1-j];
         buffer[i+dtype_size-1-j] = temp;
      }
}

/* return ceiling of floating-point log2 function
 * receive unsigned integer as argument 3/10/2005 
 */
static unsigned H5Z_scaleoffset_log2(unsigned long_long num)
{
   unsigned v = 0;
   unsigned long_long lower_bound = 1; /* is power of 2, largest value <= num */
   unsigned long_long val = num;

   while(val >>= 1) { v++; lower_bound <<= 1; }

   if(num == lower_bound) return v;
   else                   return v+1;
}

static void 
H5Z_scaleoffset_precompress(void *data, unsigned d_nelmts, enum H5Z_scaleoffset_type type, 
       unsigned filavail, void *filval_buf, uint32_t *minbits, unsigned long_long *minval)
{
   unsigned i;

   if(type ==  t_uchar)
      H5Z_scaleoffset_precompress_1(unsigned char, data, d_nelmts, 
                                    filavail, filval_buf, minbits, minval)
   else if(type == t_ushort) 
      H5Z_scaleoffset_precompress_1(unsigned short, data, d_nelmts, 
                                    filavail, filval_buf, minbits, minval)
   else if(type == t_uint) 
      H5Z_scaleoffset_precompress_1(unsigned int, data, d_nelmts, 
                                    filavail, filval_buf, minbits, minval)
   else if(type == t_ulong)
      H5Z_scaleoffset_precompress_1(unsigned long, data, d_nelmts, 
                                    filavail, filval_buf, minbits, minval)
   else if(type == t_ulong_long) 
      H5Z_scaleoffset_precompress_1(unsigned long_long, data, d_nelmts, 
                                    filavail, filval_buf, minbits, minval)
   else if(type == t_schar) {
      signed char *buf = data, min = 0, max = 0, filval = 0, filval_mask = 0;                          
      unsigned char span;                                                                     
                                                                                             
      if(filavail == H5Z_SCALEOFFSET_FILL_DEFINED) { /* fill value defined */                   
         H5Z_scaleoffset_get_fill_val(i, signed char, filval_buf, filval, filval_mask);       
         if(*minbits == 0) { /* minbits not set yet, calculate max, min, and minbits */         
            H5Z_scaleoffset_max_min_1(i, d_nelmts, buf, filval, max, min)
            if((unsigned char)(max - min) > (unsigned char)(~(unsigned char)0 - 2)) {
               *minbits = sizeof(signed char)*8;
               *minval = min; return;
            }                        
            span = max - min + 1;                                                               
            *minbits = H5Z_scaleoffset_log2(span+1);                                            
         } else /* minbits already set, only calculate min */                                   
            H5Z_scaleoffset_min_1(i, d_nelmts, buf, filval, min)                                
         *minval = min;                                                                         
         if(*minbits != sizeof(signed char)*8) /* change values if minbits != full precision */        
            for(i = 0; i < d_nelmts; i++)                                                       
               buf[i] = (buf[i] == filval)?(((unsigned char)1 << *minbits) - 1):(buf[i] - min); 
      } else { /* fill value undefined */                                                       
         if(*minbits == 0) { /* minbits not set yet, calculate max, min, and minbits */         
            H5Z_scaleoffset_max_min_2(i, d_nelmts, buf, max, min)                               
            if((unsigned char)(max - min) > (unsigned char)(~(unsigned char)0 - 2)) {
               *minbits = sizeof(signed char)*8;
               *minval = min; return;
            }                            
            span = max - min + 1;                                                               
            *minbits = H5Z_scaleoffset_log2(span);                                              
         } else /* minbits already set, only calculate min */                                   
            H5Z_scaleoffset_min_2(i, d_nelmts, buf, min)                                        
         *minval = min;                                                                         
         if(*minbits != sizeof(signed char)*8) /* change values if minbits != full precision */        
            for(i = 0; i < d_nelmts; i++) buf[i] -= min;                                        
      }                                                                                         
   } 
   else if(type == t_short)
      H5Z_scaleoffset_precompress_2(short, data, d_nelmts, 
                                    filavail, filval_buf, minbits, minval)
   else if(type == t_int)
      H5Z_scaleoffset_precompress_2(int, data, d_nelmts, 
                                    filavail, filval_buf, minbits, minval)
   else if(type == t_long) 
      H5Z_scaleoffset_precompress_2(long, data, d_nelmts, 
                                    filavail, filval_buf, minbits, minval)
   else if(type == t_long_long) 
      H5Z_scaleoffset_precompress_2(long_long, data, d_nelmts, 
                                    filavail, filval_buf, minbits, minval)
}

static void 
H5Z_scaleoffset_postdecompress(void *data, unsigned d_nelmts, enum H5Z_scaleoffset_type type, 
           unsigned filavail, void *filval_buf, uint32_t minbits, unsigned long_long minval)
{
   unsigned i;
   long_long sminval = *(long_long*)&minval; /* for signed integer types */

   if(type == t_uchar) 
      H5Z_scaleoffset_postdecompress_1(unsigned char, data, d_nelmts, filavail, 
                                       filval_buf, minbits, minval)
   else if(type == t_ushort) 
      H5Z_scaleoffset_postdecompress_1(unsigned short, data, d_nelmts, filavail, 
                                       filval_buf, minbits, minval)
   else if(type == t_uint) 
      H5Z_scaleoffset_postdecompress_1(unsigned int, data, d_nelmts, filavail, 
                                       filval_buf, minbits, minval)
   else if(type == t_ulong) 
      H5Z_scaleoffset_postdecompress_1(unsigned long, data, d_nelmts, filavail, 
                                       filval_buf, minbits, minval)
   else if(type == t_ulong_long) 
      H5Z_scaleoffset_postdecompress_1(unsigned long_long, data, d_nelmts, filavail, 
                                       filval_buf, minbits, minval)
   else if(type == t_schar) {
      signed char *buf = data, filval = 0, filval_mask = 0;

      if(filavail == H5Z_SCALEOFFSET_FILL_DEFINED) { /* fill value defined */
         H5Z_scaleoffset_get_fill_val(i, signed char, filval_buf, filval, filval_mask)
         for(i = 0; i < d_nelmts; i++) 
            buf[i] = (buf[i] == (((unsigned char)1 << minbits) - 1))?filval:(buf[i] + sminval);
      } else /* fill value undefined */
         for(i = 0; i < d_nelmts; i++) buf[i] += sminval;
   } 
   else if(type == t_short) 
      H5Z_scaleoffset_postdecompress_2(short, data, d_nelmts, filavail, 
                                       filval_buf, minbits, sminval)
   else if(type == t_int) 
      H5Z_scaleoffset_postdecompress_2(int, data, d_nelmts, filavail, 
                                       filval_buf, minbits, sminval)
   else if(type == t_long) 
      H5Z_scaleoffset_postdecompress_2(long, data, d_nelmts, filavail, 
                                       filval_buf, minbits, sminval)
   else if(type == t_long_long) 
      H5Z_scaleoffset_postdecompress_2(long_long, data, d_nelmts, filavail, 
                                       filval_buf, minbits, sminval)
}

static void H5Z_scaleoffset_next_byte(size_t *j, int *buf_len)
{
   ++(*j); *buf_len = 8 * sizeof(unsigned char);
}

static void H5Z_scaleoffset_decompress_one_byte(unsigned char *data, size_t data_offset, int k, 
int begin_i, unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p, int dtype_len)
{
   int dat_len; /* dat_len is the number of bits to be copied in each data byte */
   unsigned char val; /* value to be copied in each data byte */

   /* initialize value and bits of unsigned char to be copied */
   val = buffer[*j];
   if(k == begin_i) 
      dat_len = 8 - (dtype_len - p.minbits) % 8;
   else 
      dat_len = 8;

   if(*buf_len > dat_len) { 
      data[data_offset + k] =
      ((val >> (*buf_len - dat_len)) & ~(~0 << dat_len));
      *buf_len -= dat_len;
   } else {
      data[data_offset + k] =
      ((val & ~(~0 << *buf_len)) << (dat_len - *buf_len));
      dat_len -= *buf_len;
      H5Z_scaleoffset_next_byte(j, buf_len);
      if(dat_len == 0) return;

      val = buffer[*j];
      data[data_offset + k] |=
      ((val >> (*buf_len - dat_len)) & ~(~0 << dat_len));
      *buf_len -= dat_len; 
   }
}

static void H5Z_scaleoffset_decompress_one_atomic(unsigned char *data, size_t data_offset, 
                           unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p) 
{
   /* begin_i: the index of byte having first significant bit */
   int k, begin_i, dtype_len;

   assert(p.minbits > 0);

   dtype_len = p.size * 8;

   if(p.mem_order == H5Z_SCALEOFFSET_ORDER_LE) { /* little endian */
      begin_i = p.size - 1 - (dtype_len - p.minbits) / 8;

      for(k = begin_i; k >= 0; k--) 
         H5Z_scaleoffset_decompress_one_byte(data, data_offset, k, begin_i, 
                                             buffer, j, buf_len, p, dtype_len);
   }

   if(p.mem_order == H5Z_SCALEOFFSET_ORDER_BE) { /* big endian */
      begin_i = (dtype_len - p.minbits) / 8;

      for(k = begin_i; k <= p.size - 1; k++) 
         H5Z_scaleoffset_decompress_one_byte(data, data_offset, k, begin_i,  
                                             buffer, j, buf_len, p, dtype_len);
   } 
}

static void H5Z_scaleoffset_decompress(unsigned char *data, unsigned d_nelmts, 
                                       unsigned char *buffer, parms_atomic p) 
{
   /* i: index of data, j: index of buffer, 
      buf_len: number of bits to be filled in current byte */
   size_t i, j;
   int buf_len; 

   /* must initialize to zeros */  
   for(i = 0; i < d_nelmts*p.size; i++) data[i] = 0; 
   
   /* initialization before the loop */ 
   j = 0; 
   buf_len = sizeof(unsigned char) * 8; 

   /* decompress */
   for(i = 0; i < d_nelmts; i++) 
      H5Z_scaleoffset_decompress_one_atomic(data, i*p.size, buffer, &j, &buf_len, p);
}

static void H5Z_scaleoffset_compress_one_byte(unsigned char *data, size_t data_offset, int k, 
int begin_i, unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p, int dtype_len)
{
   int dat_len; /* dat_len is the number of bits to be copied in each data byte */
   unsigned char val; /* value to be copied in each data byte */

   /* initialize value and bits of unsigned char to be copied */
   val = data[data_offset + k];
   if(k == begin_i) 
      dat_len = 8 - (dtype_len - p.minbits) % 8;
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

static void H5Z_scaleoffset_compress_one_atomic(unsigned char *data, size_t data_offset, 
                         unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p) 
{
   /* begin_i: the index of byte having first significant bit */
   int k, begin_i, dtype_len;

   assert(p.minbits > 0);

   dtype_len = p.size * 8;

   if(p.mem_order == H5Z_SCALEOFFSET_ORDER_LE) { /* little endian */
      begin_i = p.size - 1 - (dtype_len - p.minbits) / 8;

      for(k = begin_i; k >= 0; k--) 
         H5Z_scaleoffset_compress_one_byte(data, data_offset, k, begin_i,  
                                           buffer, j, buf_len, p, dtype_len);
   }

   if(p.mem_order == H5Z_SCALEOFFSET_ORDER_BE) { /* big endian */
      begin_i = (dtype_len - p.minbits) / 8;

      for(k = begin_i; k <= p.size - 1; k++) 
         H5Z_scaleoffset_compress_one_byte(data, data_offset, k, begin_i,  
                                           buffer, j, buf_len, p, dtype_len);
   }    
}

static void H5Z_scaleoffset_compress(unsigned char *data, unsigned d_nelmts, unsigned char *buffer, 
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
