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
#include "H5Fprivate.h"         /* File access                          */
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Ppublic.h"		/* Property lists			*/
#include "H5Tpublic.h"		/* Datatype functions			*/
#include "H5Zpkg.h"		/* Data filters				*/

#ifdef H5_HAVE_FILTER_NBIT

/* Struct of parameters needed for compressing one atomic datatype */
typedef struct {
   size_t sizeof_datatype;
   int order;
   int precision;
   int offset;
} parms_atomic;

/* Local function prototypes */
static herr_t H5Z_can_apply_nbit(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static herr_t H5Z_set_local_nbit(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static size_t H5Z_filter_nbit(unsigned flags, size_t cd_nelmts, const unsigned cd_values[], 
                              size_t nbytes, size_t *buf_size, void **buf);

void H5Z_calc_parms_Tatomic(void);
static herr_t H5Z_calc_parms_Tarray(hid_t type_id);
static herr_t H5Z_calc_parms_Tcompound(hid_t type_id);

static herr_t H5Z_set_parms_Tatomic(hid_t type_id, unsigned cd_values[]);
static herr_t H5Z_set_parms_Tarray(hid_t type_id, unsigned cd_values[]);
static herr_t H5Z_set_parms_Tcompound(hid_t type_id, unsigned cd_values[]);

void H5Z_nbit_next_byte(size_t *j, int *buf_len);
void H5Z_nbit_decompress_one_byte(void *data, size_t data_offset, int k, int begin_i, 
int end_i, unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p, int datatype_len);
void H5Z_nbit_compress_one_byte(void *data, size_t data_offset, int k, int begin_i, 
int end_i, unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p, int datatype_len);
void H5Z_nbit_decompress_one_atomic(void *data, size_t data_offset, unsigned char *buffer, 
                                    size_t *j, int *buf_len, parms_atomic p);
void H5Z_nbit_decompress_one_array(void *data, size_t data_offset, unsigned char *buffer, 
                                   size_t *j, int *buf_len, const unsigned parms[]); 
void H5Z_nbit_decompress_one_compound(void *data, size_t data_offset, unsigned char *buffer, 
                                      size_t *j, int *buf_len, const unsigned parms[]); 
void H5Z_nbit_decompress(void *data, hsize_t d_nelmts, unsigned char *buffer, 
                         const unsigned parms[]);
void H5Z_nbit_compress_one_atomic(void *data, size_t data_offset, unsigned char *buffer, 
                                  size_t *j, int *buf_len, parms_atomic p);
void H5Z_nbit_compress_one_array(void *data, size_t data_offset, unsigned char *buffer, 
                                 size_t *j, int *buf_len, const unsigned parms[]); 
void H5Z_nbit_compress_one_compound(void *data, size_t data_offset, unsigned char *buffer, 
                                    size_t *j, int *buf_len, const unsigned parms[]); 
void H5Z_nbit_compress(void *data, hsize_t d_nelmts, unsigned char *buffer, 
                       size_t *buffer_size, const unsigned parms[]);
 
/* Global variables
 * cd_values_index: index of array cd_values
 * cd_values_actual_nparms: number of valid entries in array cd_values
 * parms_index: index of array parms
 * compress_ratio_is_zero: flag indicating no need to do nbit filter
 */
static unsigned int cd_values_index = 0;
static unsigned int cd_values_actual_nparms = 0;
static unsigned int parms_index = 0;
static unsigned char compress_ratio_is_zero;

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
#define FALSE                    0
#define TRUE                     1

#define H5Z_NBIT_ATOMIC          1    /* Atomic datatype class for nbit */
#define H5Z_NBIT_ARRAY           2    /* Array datatype class for nbit */
#define H5Z_NBIT_COMPOUND        3    /* Compound datatype class for nbit */

#define H5Z_NBIT_USER_NPARMS     0     /* Number of parameters that users can set */
#define H5Z_NBIT_MAX_NPARMS      4096  /* Max number of parameters for filter */

#define H5Z_NBIT_ORDER_BE        20  /* "Local" parameter for datatype byte order */
#define H5Z_NBIT_ORDER_LE        10  /* "Local" parameter for datatype byte order */


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
 * Programmer:  Xiaowen Wu
 *              Tuesday, December 21, 2004
 *
 * Modifications: 
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_can_apply_nbit(hid_t UNUSED dcpl_id, hid_t type_id, hid_t UNUSED space_id)
{
    H5T_class_t dtype_class;            /* Datatype's class */
    unsigned dtype_size;                /* Datatype's size (in bytes) */
    H5T_order_t dtype_order;            /* Datatype's endianness order */
    herr_t ret_value=TRUE;              /* Return value */

    FUNC_ENTER_NOAPI(H5Z_can_apply_nbit, FAIL)

    /* Get datatype's class, for checking the "datatype class" */
    if((dtype_class = H5Tget_class(type_id)) == H5T_NO_CLASS )
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype class")

    /* Get datatype's size, for checking the "datatype size" */
    if((dtype_size = H5Tget_size(type_id)) == 0)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype size")

    /* codes below have to been updated to adjust to array and compound datatype */
    if(dtype_class == H5T_INTEGER || dtype_class == H5T_FLOAT) {
        /* Get datatype's endianness order */
        if((dtype_order = H5Tget_order(type_id)) == H5T_ORDER_ERROR)
	    HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "can't retrieve datatype endianness order")

        /* Range check datatype's endianness order */
        /* (Note: this may not handle non-atomic datatypes well) */
        if(dtype_order != H5T_ORDER_LE && dtype_order != H5T_ORDER_BE)
	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FALSE, "invalid datatype endianness order")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_can_apply_nbit() */


/*-------------------------------------------------------------------------
 * Function:    H5Z_calc_parms_Tatomic
 *
 * Purpose:     Calculate the number of parameters of cd_values[]
 *              of atomic datatype whose datatype class is integer 
 *              or floating point 
 *
 * Return:      Success: Non-negative
 *              Failure: Negative
 *
 * Programmer:  Xiaowen Wu
 *              Saturday, January 29, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void H5Z_calc_parms_Tatomic()
{
    /* Store datatype class code */
    ++cd_values_actual_nparms;
 
    /* Store datatype size */
    ++cd_values_actual_nparms;

    /* Store datatype endianness */
    ++cd_values_actual_nparms;

    /* Store datatype's precision */
    ++cd_values_actual_nparms;

    /* Store datatype's offset */
    ++cd_values_actual_nparms;
} 


/*-------------------------------------------------------------------------
 * Function:    H5Z_calc_parms_Tarray
 *
 * Purpose:     Calculate the number of parameters of cd_values[]
 *              for given a certain datatype identifier type_id
 *              if its datatype class is array datatype
 *
 * Return:      Success: Non-negative
 *              Failure: Negative
 *
 * Programmer:  Xiaowen Wu
 *              Wednesday, January 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5Z_calc_parms_Tarray(hid_t type_id)
{
    hid_t       dtype_base;        /* Array datatype's base datatype */
    H5T_class_t dtype_base_class;  /* Array datatype's base datatype's class */
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_calc_parms_Tarray, FAIL)

    /* Store datatype class code */
    ++cd_values_actual_nparms;
 
    /* Store array datatype's size */
    ++cd_values_actual_nparms;

    /* Get array datatype's base datatype */
    if((dtype_base=H5Tget_super(type_id))<0)
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad base datatype")

    /* Get base datatype's class */
    if((dtype_base_class=H5Tget_class(dtype_base))==H5T_NO_CLASS )
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad base datatype class")

    /* Calculate number of the rest parameters according to base datatype's class */
    switch(dtype_base_class) {
        case H5T_INTEGER:
        case H5T_FLOAT: H5Z_calc_parms_Tatomic();
             break;
        case H5T_ARRAY: H5Z_calc_parms_Tarray(dtype_base); 
             break;
        case H5T_COMPOUND:H5Z_calc_parms_Tcompound(dtype_base);
             break;
        default:
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "datatype class not supported by nbit")
    } /* end switch */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_calc_parms_Tarray() */


/*-------------------------------------------------------------------------
 * Function:    H5Z_calc_parms_Tcompound
 *
 * Purpose:     Calculate the number of parameters of cd_values[]
 *              for given a certain datatype identifier type_id
 *              if its datatype class is compound datatype
 *
 * Return:      Success: Non-negative
 *              Failure: Negative
 *
 * Programmer:  Xiaowen Wu
 *              Wednesday, January 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5Z_calc_parms_Tcompound(hid_t type_id)
{
    int         i;                  /* local index variable */    
    int         nmembers;           /* Compound datatype's number of members */
    hid_t       dtype_member;       /* Compound datatype's member datatype */
    H5T_class_t dtype_member_class; /* Compound datatype's member datatype's class */
    herr_t ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5Z_calc_parms_Tcompound, FAIL)

    /* Store compound datatype class code */
    ++cd_values_actual_nparms;
 
    /* Store compound datatype's size */
    ++cd_values_actual_nparms;

    /* Get number of members */
    if((nmembers=H5Tget_nmembers(type_id))<0)
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype number of members")

    /* Store number of members */
    ++cd_values_actual_nparms;

    /* For each member, calculate parameters */
    for(i = 0; i < nmembers; i++) {
        /* Get member datatype */
        if((dtype_member=H5Tget_member_type(type_id, (unsigned)i))<0)
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad member datatype")

        /* Get member datatype's class */
        if((dtype_member_class=H5Tget_member_class(type_id, (unsigned)i))<0)
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad member datatype class")

        /* Store member offset */
        ++cd_values_actual_nparms;

        /* Calculate parameters according to member's datatype class */
        switch(dtype_member_class) {
            case H5T_INTEGER:
            case H5T_FLOAT: H5Z_calc_parms_Tatomic();
                 break;
            case H5T_ARRAY: H5Z_calc_parms_Tarray(dtype_member); 
                 break;
            case H5T_COMPOUND:H5Z_calc_parms_Tcompound(dtype_member);
                 break;
            default:
                HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "datatype class not supported by nbit")
        } /* end switch */
    } /* end for */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_calc_params_Tcompound */


/*-------------------------------------------------------------------------
 * Function:    H5Z_set_parms_Tatomic
 *
 * Purpose:     Set the cd_values[] for given a certain datatype identifier 
 *              type_id if its datatype class is integer or floating point 
 *
 * Return:      Success: Non-negative
 *              Failure: Negative
 *
 * Programmer:  Xiaowen Wu
 *              Tuesday, January 11, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5Z_set_parms_Tatomic(hid_t type_id, unsigned cd_values[])
{
    H5T_order_t dtype_order;    /* Atomic datatype's endianness order */
    size_t dtype_size;          /* Atomic datatype's size (in bytes) */
    size_t dtype_precision;     /* Atomic datatype's precision (in bits) */
    int dtype_offset;           /* Atomic datatype's offset (in bits) */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5Z_set_parms_Tatomic, FAIL)

    /* Set datatype class code */
    cd_values[cd_values_index++] = H5Z_NBIT_ATOMIC;
 
    /* Get datatype's size */
    if((dtype_size=H5Tget_size(type_id))==0)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype size")

    /* Set "local" parameter for datatype size */
    cd_values[cd_values_index++] = dtype_size;

    /* Get datatype's endianness order */
    if((dtype_order=H5Tget_order(type_id))==H5T_ORDER_ERROR)
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype endianness order")

    /* Set "local" parameter for datatype endianness */
    switch(dtype_order) {
        case H5T_ORDER_LE:      /* Little-endian byte order */
            cd_values[cd_values_index++] = H5Z_NBIT_ORDER_LE;
            break;
        case H5T_ORDER_BE:      /* Big-endian byte order */
            cd_values[cd_values_index++] = H5Z_NBIT_ORDER_BE;
            break;
        default:
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype endianness order")
    } /* end switch */

    /* Get datatype's precision */
    if((dtype_precision=H5Tget_precision(type_id))==0)
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype precision")

    /* Set "local" parameter for datatype precision */
    cd_values[cd_values_index++] = dtype_precision;

    /* Get datatype's offset */
    if((dtype_offset=H5Tget_offset(type_id))<0)
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype offset")

    /* Set "local" parameter for datatype offset */
    cd_values[cd_values_index++] = dtype_offset;

    /* Check the compression ratio at this point */
    if(compress_ratio_is_zero) /* so far the compression ratio is zero */
       if(dtype_offset != 0 || dtype_precision != dtype_size * 8)
          compress_ratio_is_zero = FALSE;    
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_set_parms_Tatomic() */


/*-------------------------------------------------------------------------
 * Function:    H5Z_set_parms_Tarray
 *
 * Purpose:     Set the cd_values[] for given a certain datatype identifier 
 *              type_id if its datatype class is array datatype
 *
 * Return:      Success: Non-negative
 *              Failure: Negative
 *
 * Programmer:  Xiaowen Wu
 *              Tuesday, January 11, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5Z_set_parms_Tarray(hid_t type_id, unsigned cd_values[])
{
    hid_t       dtype_base;        /* Array datatype's base datatype */
    H5T_class_t dtype_base_class;  /* Array datatype's base datatype's class */
    size_t dtype_size;             /* Array datatype's size (in bytes) */
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_set_parms_Tarray, FAIL)

    /* Set datatype class code */
    cd_values[cd_values_index++] = H5Z_NBIT_ARRAY;
 
    /* Get array datatype's size */
    if((dtype_size=H5Tget_size(type_id))==0)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype size")

    /* Set "local" parameter for array datatype's size */
    cd_values[cd_values_index++]=dtype_size;

    /* Get array datatype's base datatype */
    if((dtype_base=H5Tget_super(type_id))<0)
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad base datatype")

    /* Get base datatype's class */
    if((dtype_base_class=H5Tget_class(dtype_base))==H5T_NO_CLASS )
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad base datatype class")

    /* Call appropriate function according to base datatype's class */
    switch(dtype_base_class) {
        case H5T_INTEGER:
        case H5T_FLOAT: H5Z_set_parms_Tatomic(dtype_base, cd_values);
             break;
        case H5T_ARRAY: H5Z_set_parms_Tarray(dtype_base, cd_values); 
             break;
        case H5T_COMPOUND:H5Z_set_parms_Tcompound(dtype_base, cd_values);
             break;
        default:
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "datatype class not supported by nbit")
    } /* end switch */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_set_parms_Tarray() */


/*-------------------------------------------------------------------------
 * Function:    H5Z_set_parms_Tcompound
 *
 * Purpose:     Set the cd_values[] for given a certain datatype identifier 
 *              type_id if its datatype class is compound datatype
 *
 * Return:      Success: Non-negative
 *              Failure: Negative
 *
 * Programmer:  Xiaowen Wu
 *              Tuesday, January 11, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5Z_set_parms_Tcompound(hid_t type_id, unsigned cd_values[])
{
    int         i;                  /* local index variable */
    int         nmembers;           /* Compound datatype's number of members */
    hid_t       dtype_member;       /* Compound datatype's member datatype */
    H5T_class_t dtype_member_class; /* Compound datatype's member datatype's class */
    size_t dtype_member_offset;     /* Compound datatype's member datatype's offset (in bytes) */
    size_t dtype_size;              /* Compound datatype's size (in bytes) */
    herr_t ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5Z_set_parms_Tcompound, FAIL)

    /* Set "local" parameter for compound datatype class code */
    cd_values[cd_values_index++] = H5Z_NBIT_COMPOUND;
 
    /* Get datatype's size */
    if((dtype_size=H5Tget_size(type_id))==0)
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype size")

    /* Set "local" parameter for compound datatype size */
    cd_values[cd_values_index++] = dtype_size;

    /* Get number of members */
    if((nmembers=H5Tget_nmembers(type_id))<0)
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype number of members")

    /* Set "local" parameter for number of members */
    cd_values[cd_values_index++] = nmembers;

    /* For each member, set parameters */
    for(i = 0; i < nmembers; i++) {
        /* Get member datatype */
        if((dtype_member=H5Tget_member_type(type_id, (unsigned)i))<0)
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad member datatype")

        /* Get member datatype's class */
        if((dtype_member_class=H5Tget_member_class(type_id, (unsigned)i))<0)
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad member datatype class")

        /* Get member offset, success if H5Tget_member_class() success */
        dtype_member_offset =  H5Tget_member_offset(type_id, (unsigned)i);

        /* Set "local" parameter for member offset */
        cd_values[cd_values_index++] = dtype_member_offset;

        /* Call appropriate function according to member's datatype class */
        switch(dtype_member_class) {
            case H5T_INTEGER:
            case H5T_FLOAT: H5Z_set_parms_Tatomic(dtype_member, cd_values);
                 break;
            case H5T_ARRAY: H5Z_set_parms_Tarray(dtype_member, cd_values); 
                 break;
            case H5T_COMPOUND:H5Z_set_parms_Tcompound(dtype_member, cd_values);
                 break;
            default:
                HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "datatype class not supported by nbit")
        } /* end switch */
    } /* end for */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_set_params_Tcompound */


/*-------------------------------------------------------------------------
 * Function:	H5Z_set_local_nbit
 *
 * Purpose:	Set the "local" dataset parameters for nbit compression.
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:	Xiaowen Wu
 *              Tuesday, January 11, 2004 
 *
 * Modifications:
 *                
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_set_local_nbit(hid_t dcpl_id, hid_t type_id, hid_t space_id)
{
    unsigned flags;         /* Filter flags */
    size_t cd_nelmts=H5Z_NBIT_USER_NPARMS;  /* Number of filter parameters */
    unsigned *cd_values = NULL;    /* Filter parameters */
    hssize_t npoints;              /* Number of points in the dataspace */
    H5T_class_t dtype_class;       /* Datatype's class */
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_set_local_nbit, FAIL)

    /* Get datatype's class */
    if((dtype_class = H5Tget_class(type_id)) == H5T_NO_CLASS )
	HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "bad datatype class")

    /* Calculate how many parameters will fill the cd_values array */
    cd_values_actual_nparms = 1; /* first parameter reserved for dataset's number of elements */
    switch(dtype_class) {
        case H5T_INTEGER:
        case H5T_FLOAT: H5Z_calc_parms_Tatomic();
             break;
        case H5T_ARRAY: H5Z_calc_parms_Tarray(type_id); 
             break;
        case H5T_COMPOUND:H5Z_calc_parms_Tcompound(type_id);
             break;
        default:
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "datatype class not supported by nbit")
    } /* end switch */
    
    /* Check if the number of parameters exceed what cd_values[] can store */
    if(cd_values_actual_nparms > H5Z_NBIT_MAX_NPARMS)
        HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "datatype needs too many nbit parameters")

    /* allocate memory space for decompressed buffer */
    if(NULL==(cd_values = H5MM_malloc(cd_values_actual_nparms*sizeof(unsigned))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for cd_values[]")

    /* Get the filter's current parameters */
#ifdef H5_WANT_H5_V1_6_COMPAT
    if(H5Pget_filter_by_id(dcpl_id,H5Z_FILTER_NBIT,&flags,&cd_nelmts, cd_values,0,NULL)<0)
#else
    if(H5Pget_filter_by_id(dcpl_id,H5Z_FILTER_NBIT,&flags,&cd_nelmts, cd_values,0,NULL,NULL)<0)
#endif
	HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "can't get nbit parameters")

    /* Get total number of elements in the chunk */
    if ((npoints=H5Sget_simple_extent_npoints(space_id))<0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get number of points in the dataspace")

    /* Initialize index for cd_values array */
    cd_values_index = 0;

    /* Set "local" parameter for this dataset's number of elements */
    cd_values[cd_values_index++] = npoints;

    /* Assume compression ratio is zero now, will be changed to FALSE later if not */
    compress_ratio_is_zero = TRUE;

    /* Call appropriate function according to the datatype class */
    switch(dtype_class) {
        case H5T_INTEGER:
        case H5T_FLOAT: H5Z_set_parms_Tatomic(type_id, cd_values);
             break;
        case H5T_ARRAY: H5Z_set_parms_Tarray(type_id, cd_values); 
             break;
        case H5T_COMPOUND:H5Z_set_parms_Tcompound(type_id, cd_values);
             break;
        default:
            HGOTO_ERROR(H5E_PLINE, H5E_BADTYPE, FAIL, "datatype class not supported by nbit")
    } /* end switch */

    /* Check if calculation of parameters matches with setting of parameters */
    assert(cd_values_actual_nparms==cd_values_index);
 
    /* Modify the filter's parameters for this dataset */
    if(H5Pmodify_filter(dcpl_id, H5Z_FILTER_NBIT, flags, cd_values_actual_nparms, cd_values)<0)
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
    size_t ret_value = 0;          /* return value */
    size_t size_out  = 0;          /* size of output buffer */
    hsize_t d_nelmts = 0;    /* number of data elements in the chunk */
    unsigned char *outbuf = NULL;  /* pointer to new output buffer */
    /*size_t i;*/

    FUNC_ENTER_NOAPI(H5Z_filter_nbit, 0)

    /* check arguments */
    if (cd_nelmts!=cd_values_actual_nparms)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid nbit aggression level")

    /* no need to do nbit compress or decompress */
    if (compress_ratio_is_zero) { 
        ret_value = *buf_size;
        goto done;
    }

    /* copy a filter parameter to d_nelmts */
    d_nelmts = cd_values[0];

    /* input; decompress */
    if (flags & H5Z_FLAG_REVERSE) {
        size_out = d_nelmts * cd_values[2];

        /* allocate memory space for decompressed buffer */
        if(NULL==(outbuf = H5MM_malloc(size_out)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for nbit decompression")
  
        /* decompress the buffer */
        H5Z_nbit_decompress(outbuf, d_nelmts, *buf, cd_values);   
    }
    /* output; compress */
    else { 
        /* difficult to calculate exact buffer size after compression */
        size_out = d_nelmts * cd_values[2];

        /* allocate memory space for compressed buffer */
        if(NULL==(outbuf = H5MM_malloc(size_out)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for nbit compression")
  
        /* compress the buffer */
        H5Z_nbit_compress(*buf, d_nelmts, outbuf, &size_out, cd_values);
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

/* ======== Nbit Algorithm ===============================================
 * assume one byte has 8 bit
 * assume padding bit is 0
 * assume size of unsigned char is one byte
 * assume one data item of certain datatype is stored continously in bytes 
 * atomic datatype is treated on byte basis 
 */

void H5Z_nbit_next_byte(size_t *j, int *buf_len)
{
   ++(*j); 
   *buf_len = 8 * sizeof(unsigned char);
}

void H5Z_nbit_decompress_one_byte(void *data, size_t data_offset, int k, int begin_i, 
int end_i, unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p, int datatype_len)
{
   int dat_len; /* dat_len is the number of bits to be copied in each data byte */
   int uchar_offset;
   unsigned char val; /* value to be copied in each data byte */

   /* initialize value and bits of unsigned char to be copied */
   val = buffer[*j];
   uchar_offset = 0;

   if(begin_i != end_i) { /* significant bits occupy >1 unsigned char */
      if(k == begin_i) 
         dat_len = 8 - (datatype_len - p.precision - p.offset) % 8;
      else if(k == end_i) {
         dat_len = 8 - p.offset %8;
         uchar_offset = 8 - dat_len;
      }
      else
         dat_len = 8;
   } else { /* all significant bits in one unsigned char */
      uchar_offset = p.offset % 8;
      dat_len = p.precision;
   }

   if(*buf_len > dat_len) { 
      ((unsigned char *)data)[data_offset + k] =
      ((val >> (*buf_len - dat_len)) & ~(~0 << dat_len)) << uchar_offset;
      *buf_len -= dat_len;
   } else {
      ((unsigned char *)data)[data_offset + k] =
      ((val & ~(~0 << *buf_len)) << (dat_len - *buf_len)) << uchar_offset;
      dat_len -= *buf_len;
      H5Z_nbit_next_byte(j, buf_len);
      if(dat_len == 0) return;

      val = buffer[*j];
      ((unsigned char *)data)[data_offset + k] |=
      ((val >> (*buf_len - dat_len)) & ~(~0 << dat_len)) << uchar_offset;
      *buf_len -= dat_len; 
   }
}

void H5Z_nbit_decompress_one_atomic(void *data, size_t data_offset, unsigned char *buffer, 
                                    size_t *j, int *buf_len, parms_atomic p) 
{
   /* begin_i: the index of byte having first significant bit
      end_i: the index of byte having last significant bit */
   int k, begin_i, end_i, datatype_len;

   datatype_len = p.sizeof_datatype * 8;

   if(p.order == H5Z_NBIT_ORDER_LE) { /* little endian */
      /* calculate begin_i and end_i */
      if((p.precision + p.offset) % 8 != 0)
         begin_i = (p.precision + p.offset) / 8;
      else
         begin_i = (p.precision + p.offset) / 8 - 1;
      end_i = p.offset / 8;

      for(k = begin_i; k >= end_i; k--) 
         H5Z_nbit_decompress_one_byte(data, data_offset, k, begin_i, end_i, 
                                      buffer, j, buf_len, p, datatype_len);
   }

   if(p.order == H5Z_NBIT_ORDER_BE) { /* big endian */
      /* calculate begin_i and end_i */
      begin_i = (datatype_len - p.precision - p.offset) / 8;
      if(p.offset % 8 != 0)
         end_i = (datatype_len - p.offset) / 8;
      else
         end_i = (datatype_len - p.offset) / 8 - 1;

      for(k = begin_i; k <= end_i; k++) 
         H5Z_nbit_decompress_one_byte(data, data_offset, k, begin_i, end_i, 
                                      buffer, j, buf_len, p, datatype_len);
   } 
}

void H5Z_nbit_decompress_one_array(void *data, size_t data_offset, unsigned char *buffer, 
                                   size_t *j, int *buf_len, const unsigned parms[]) 
{
   unsigned i, size, base_class, base_size, n, begin_index;
   parms_atomic p;

   size = parms[parms_index++];
   base_class = parms[parms_index++];

   switch(base_class) {
      case H5Z_NBIT_ATOMIC:
           p.sizeof_datatype = parms[parms_index++];
           p.order = parms[parms_index++];
           p.precision = parms[parms_index++];
           p.offset = parms[parms_index++];
           n = size/p.sizeof_datatype;
           for(i = 0; i < n; i++)
              H5Z_nbit_decompress_one_atomic(data, data_offset + i*p.sizeof_datatype,
                                             buffer, j, buf_len, p);
           break;
      case H5Z_NBIT_ARRAY:
           base_size = parms[parms_index]; /* read in advance */
           n = size/base_size; /* number of base_type elements inside the array datatype */
           begin_index = parms_index;
           for(i = 0; i < n; i++) {
              H5Z_nbit_decompress_one_array(data, data_offset + i*base_size,
                                            buffer, j, buf_len, parms);
              parms_index = begin_index;
           }
           break;
      case H5Z_NBIT_COMPOUND:
           base_size = parms[parms_index]; /* read in advance */
           n = size/base_size; /* number of base_type elements inside the array datatype */
           begin_index = parms_index;
           for(i = 0; i < n; i++) {
              H5Z_nbit_decompress_one_compound(data, data_offset + i*base_size,
                                               buffer, j, buf_len, parms);
              parms_index = begin_index;
           }
           break;
   } /* end switch */
}

void H5Z_nbit_decompress_one_compound(void *data, size_t data_offset, unsigned char *buffer, 
                                      size_t *j, int *buf_len, const unsigned parms[]) 
{
   unsigned i, nmembers, member_offset, member_class, size;
   parms_atomic p;

   size = parms[parms_index++];
   nmembers = parms[parms_index++];

   for(i = 0; i < nmembers; i++) {
      member_offset = parms[parms_index++];
      member_class = parms[parms_index++];
      switch(member_class) {
         case H5Z_NBIT_ATOMIC:
              p.sizeof_datatype = parms[parms_index++];
              p.order = parms[parms_index++];
              p.precision = parms[parms_index++];
              p.offset = parms[parms_index++];
              H5Z_nbit_decompress_one_atomic(data, data_offset + member_offset, 
                                             buffer, j, buf_len, p);
              break;
         case H5Z_NBIT_ARRAY:
              H5Z_nbit_decompress_one_array(data, data_offset + member_offset, 
                                            buffer, j, buf_len, parms);
              break;
         case H5Z_NBIT_COMPOUND:
              H5Z_nbit_decompress_one_compound(data, data_offset+member_offset, 
                                               buffer, j, buf_len, parms);
              break;
      } /* end switch */
   }
}

void H5Z_nbit_decompress(void *data, hsize_t d_nelmts, unsigned char *buffer, const unsigned parms[]) 
{
   /* i: index of data, j: index of buffer, 
      buf_len: number of bits to be filled in current byte */
   size_t i, j, size;
   int buf_len; 
   parms_atomic p;

   /* may not have to initialize to zeros */  
   for(i = 0; i < d_nelmts*parms[2]; i++)  
      ((unsigned char *)data)[i] = 0; 
   
   /* initialization before the loop */ 
   j = 0; 
   buf_len = sizeof(unsigned char) * 8; 

   switch(parms[1]) {
      case H5Z_NBIT_ATOMIC: 
           /* set the index before goto function call */
           p.sizeof_datatype = parms[2];
           p.order = parms[3];
           p.precision = parms[4];
           p.offset = parms[5];
           for(i = 0; i < d_nelmts; i++) {
              H5Z_nbit_decompress_one_atomic(data, i*p.sizeof_datatype, buffer, &j, &buf_len, p);
           }
           break;
      case H5Z_NBIT_ARRAY:
           size = parms[2];
           parms_index = 2; 
           for(i = 0; i < d_nelmts; i++) {
              H5Z_nbit_decompress_one_array(data, i*size, buffer, &j, &buf_len, parms);
              parms_index = 2;
           }
           break;
      case H5Z_NBIT_COMPOUND: 
           size = parms[2];
           parms_index = 2;
           for(i = 0; i < d_nelmts; i++) {
              H5Z_nbit_decompress_one_compound(data, i*size, buffer, &j, &buf_len, parms);
              parms_index = 2;
           }
           break;
   } /* end switch */
}

void H5Z_nbit_compress_one_byte(void *data, size_t data_offset, int k, int begin_i, 
int end_i, unsigned char *buffer, size_t *j, int *buf_len, parms_atomic p, int datatype_len)
{
   int dat_len; /* dat_len is the number of bits to be copied in each data byte */
   unsigned char val; /* value to be copied in each data byte */

   /* initialize value and bits of unsigned char to be copied */
   val = ((unsigned char *)data)[data_offset + k];
   if(begin_i != end_i) { /* significant bits occupy >1 unsigned char */
      if(k == begin_i) 
         dat_len = 8 - (datatype_len - p.precision - p.offset) % 8;
      else if(k == end_i) {
         dat_len = 8 - p.offset % 8;
         val >>= 8 - dat_len;
      }
      else
         dat_len = 8;
   } else { /* all significant bits in one unsigned char */
      val >>= p.offset % 8;
      dat_len = p.precision;
   }

   if(*buf_len > dat_len) { 
      buffer[*j] |= (val & ~(~0 << dat_len)) << (*buf_len - dat_len);
      *buf_len -= dat_len;
   } else {
      buffer[*j] |= (val >> (dat_len - *buf_len)) & ~(~0 << *buf_len);
      dat_len -= *buf_len;
      H5Z_nbit_next_byte(j, buf_len);
      if(dat_len == 0) return;

      buffer[*j] = (val & ~(~0 << dat_len)) << (*buf_len - dat_len);
      *buf_len -= dat_len; 
   }
}

void H5Z_nbit_compress_one_atomic(void *data, size_t data_offset, unsigned char *buffer, 
                                  size_t *j, int *buf_len, parms_atomic p) 
{
   /* begin_i: the index of byte having first significant bit
      end_i: the index of byte having last significant bit */ 
   int k, begin_i, end_i, datatype_len;

   datatype_len = p.sizeof_datatype * 8;

   if(p.order == H5Z_NBIT_ORDER_LE) { /* little endian */
      /* calculate begin_i and end_i */
      if((p.precision + p.offset) % 8 != 0)
         begin_i = (p.precision + p.offset) / 8;
      else
         begin_i = (p.precision + p.offset) / 8 - 1;
      end_i = p.offset / 8;

      for(k = begin_i; k >= end_i; k--) 
         H5Z_nbit_compress_one_byte(data, data_offset, k, begin_i, end_i, 
                                    buffer, j, buf_len, p, datatype_len);
   }

   if(p.order == H5Z_NBIT_ORDER_BE) { /* big endian */
      /* calculate begin_i and end_i */
      begin_i = (datatype_len - p.precision - p.offset) / 8;
      if(p.offset % 8 != 0)
         end_i = (datatype_len - p.offset) / 8;
      else
         end_i = (datatype_len - p.offset) / 8 - 1;

      for(k = begin_i; k <= end_i; k++) 
         H5Z_nbit_compress_one_byte(data, data_offset, k, begin_i, end_i, 
                                    buffer, j, buf_len, p, datatype_len);
   }    
}

void H5Z_nbit_compress_one_array(void *data, size_t data_offset, unsigned char *buffer, 
                                 size_t *j, int *buf_len, const unsigned parms[]) 
{
   unsigned i, size, base_class, base_size, n, begin_index;
   parms_atomic p;

   size = parms[parms_index++]; 
   base_class = parms[parms_index++];

   switch(base_class) {
      case H5Z_NBIT_ATOMIC:
           p.sizeof_datatype = parms[parms_index++];
           p.order = parms[parms_index++];
           p.precision = parms[parms_index++];
           p.offset = parms[parms_index++];
           n = size/p.sizeof_datatype;
           for(i = 0; i < n; i++)
              H5Z_nbit_compress_one_atomic(data, data_offset + i*p.sizeof_datatype, 
                                           buffer, j, buf_len, p);
           break;
      case H5Z_NBIT_ARRAY:
           base_size = parms[parms_index]; /* read in advance */
           n = size/base_size; /* number of base_type elements inside the array datatype */
           begin_index = parms_index;
           for(i = 0; i < n; i++) {
              H5Z_nbit_compress_one_array(data, data_offset + i*base_size, 
                                          buffer, j, buf_len, parms);
              parms_index = begin_index;
           }
           break;
      case H5Z_NBIT_COMPOUND:
           base_size = parms[parms_index]; /* read in advance */
           n = size/base_size; /* number of base_type elements inside the array datatype */
           begin_index = parms_index;
           for(i = 0; i < n; i++) {
              H5Z_nbit_compress_one_compound(data, data_offset + i*base_size, 
                                             buffer, j, buf_len, parms);
              parms_index = begin_index;
           }
           break;
   } /* end switch */
}

void H5Z_nbit_compress_one_compound(void *data, size_t data_offset, unsigned char *buffer, 
                                    size_t *j, int *buf_len, const unsigned parms[]) 
{
   unsigned i, nmembers, member_offset, member_class, size;
   parms_atomic p;

   size = parms[parms_index++];
   nmembers = parms[parms_index++];

   for(i = 0; i < nmembers; i++) {
      member_offset = parms[parms_index++];
      member_class = parms[parms_index++];

      switch(member_class) {
         case H5Z_NBIT_ATOMIC: 
              p.sizeof_datatype = parms[parms_index++];
              p.order = parms[parms_index++];
              p.precision = parms[parms_index++];
              p.offset = parms[parms_index++];
              H5Z_nbit_compress_one_atomic(data, data_offset + member_offset, 
                                           buffer, j, buf_len, p);
              break;
         case H5Z_NBIT_ARRAY: 
              H5Z_nbit_compress_one_array(data, data_offset + member_offset, 
                                          buffer, j, buf_len, parms);
              break;
         case H5Z_NBIT_COMPOUND:
              H5Z_nbit_compress_one_compound(data, data_offset+member_offset, 
                                             buffer, j, buf_len, parms);
              break;
      } /* end switch */
   }
}

void H5Z_nbit_compress(void *data, hsize_t d_nelmts, unsigned char *buffer, size_t *buffer_size, 
                       const unsigned parms[]) 
{
   /* i: index of data, j: index of buffer, 
      buf_len: number of bits to be filled in current byte */
   size_t i, j, size;
   int buf_len; 
   parms_atomic p;

   /* must initialize buffer to be zeros */
   for(j = 0; j < *buffer_size; j++) 
      buffer[j] = 0;

   /* initialization before the loop */ 
   j = 0; 
   buf_len = sizeof(unsigned char) * 8; 

   switch(parms[1]) {
      case H5Z_NBIT_ATOMIC: 
           /* set the index before goto function call */
           p.sizeof_datatype = parms[2];
           p.order = parms[3];
           p.precision = parms[4];
           p.offset = parms[5];

           for(i = 0; i < d_nelmts; i++) {
              H5Z_nbit_compress_one_atomic(data, i*p.sizeof_datatype, buffer, &j, &buf_len, p);
           }
           break;
      case H5Z_NBIT_ARRAY: 
           size = parms[2];
           parms_index = 2;
           for(i = 0; i < d_nelmts; i++) { 
              H5Z_nbit_compress_one_array(data, i*size, buffer, &j, &buf_len, parms);
              parms_index = 2;
           }
           break;
      case H5Z_NBIT_COMPOUND:
           size = parms[2];
           parms_index = 2;
           for(i = 0; i < d_nelmts; i++) { 
              H5Z_nbit_compress_one_compound(data, i*size, buffer, &j, &buf_len, parms);
              parms_index = 2;
           }
           break;
   } /* end switch */

   *buffer_size = j + 1; /* sometimes is catually j, but to be safe */
}
#endif /* H5_HAVE_FILTER_NZIP */
