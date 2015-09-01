/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5Pdapl.c
 *			October 27, 2008
 *			Neil Fortner <nfortne2@hdfgroup.org>
 *
 * Purpose:		Dataset access property list class routines
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions */
#include "H5Dprivate.h"		/* Datasets */
#include "H5Eprivate.h"		/* Error handling */
#include "H5Fprivate.h"		/* Files */
#include "H5Iprivate.h"		/* IDs */
#include "H5Ppkg.h"		/* Property lists */


/****************/
/* Local Macros */
/****************/

/* ========= Dataset Access properties ============ */
/* Definitions for size of raw data chunk cache(slots) */
#define H5D_ACS_DATA_CACHE_NUM_SLOTS_SIZE       sizeof(size_t)
#define H5D_ACS_DATA_CACHE_NUM_SLOTS_DEF        H5D_CHUNK_CACHE_NSLOTS_DEFAULT
#define H5D_ACS_DATA_CACHE_NUM_SLOTS_ENC        H5P__encode_size_t
#define H5D_ACS_DATA_CACHE_NUM_SLOTS_DEC        H5P__decode_size_t
/* Definition for size of raw data chunk cache(bytes) */
#define H5D_ACS_DATA_CACHE_BYTE_SIZE_SIZE       sizeof(size_t)
#define H5D_ACS_DATA_CACHE_BYTE_SIZE_DEF        H5D_CHUNK_CACHE_NBYTES_DEFAULT
#define H5D_ACS_DATA_CACHE_BYTE_SIZE_ENC        H5P__encode_size_t
#define H5D_ACS_DATA_CACHE_BYTE_SIZE_DEC        H5P__decode_size_t
/* Definition for preemption read chunks first */
#define H5D_ACS_PREEMPT_READ_CHUNKS_SIZE        sizeof(double)
#define H5D_ACS_PREEMPT_READ_CHUNKS_DEF         H5D_CHUNK_CACHE_W0_DEFAULT
#define H5D_ACS_PREEMPT_READ_CHUNKS_ENC         H5P__encode_double
#define H5D_ACS_PREEMPT_READ_CHUNKS_DEC         H5P__decode_double
/* Definitions for VDS view option */
#define H5D_ACS_VDS_VIEW_SIZE                   sizeof(H5D_vds_view_t)
#define H5D_ACS_VDS_VIEW_DEF                    H5D_VDS_LAST_AVAILABLE
#define H5D_ACS_VDS_VIEW_ENC                    H5P__dacc_vds_view_enc
#define H5D_ACS_VDS_VIEW_DEC                    H5P__dacc_vds_view_dec
/* Definitions for VDS printf gap */
#define H5D_ACS_VDS_PRINTF_GAP_SIZE             sizeof(hsize_t)
#define H5D_ACS_VDS_PRINTF_GAP_DEF              (hsize_t)0
#define H5D_ACS_VDS_PRINTF_GAP_ENC              H5P__encode_hsize_t
#define H5D_ACS_VDS_PRINTF_GAP_DEC              H5P__decode_hsize_t

/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Property class callbacks */
static herr_t H5P__dacc_reg_prop(H5P_genclass_t *pclass);

/* Property list callbacks */
static herr_t H5P__dacc_vds_view_enc(const void *value, void **pp, size_t *size);
static herr_t H5P__dacc_vds_view_dec(const void **pp, void *value);


/*********************/
/* Package Variables */
/*********************/

/* Dataset access property list class library initialization object */
const H5P_libclass_t H5P_CLS_DACC[1] = {{
    "dataset access",		/* Class name for debugging     */
    H5P_TYPE_DATASET_ACCESS,    /* Class type                   */

    &H5P_CLS_LINK_ACCESS_g,	/* Parent class                 */
    &H5P_CLS_DATASET_ACCESS_g,	/* Pointer to class             */
    &H5P_CLS_DATASET_ACCESS_ID_g,	/* Pointer to class ID          */
    &H5P_LST_DATASET_ACCESS_ID_g,	/* Pointer to default property list ID */
    H5P__dacc_reg_prop,		/* Default property registration routine */

    NULL,		        /* Class creation callback      */
    NULL,		        /* Class creation callback info */
    NULL,		        /* Class copy callback          */
    NULL,		        /* Class copy callback info     */
    NULL,		        /* Class close callback         */
    NULL 		        /* Class close callback info    */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5P__dacc_reg_prop
 *
 * Purpose:     Register the dataset access property list class's
 *              properties
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              October 27, 2008
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__dacc_reg_prop(H5P_genclass_t *pclass)
{
    size_t rdcc_nslots = H5D_ACS_DATA_CACHE_NUM_SLOTS_DEF;      /* Default raw data chunk cache # of slots */
    size_t rdcc_nbytes = H5D_ACS_DATA_CACHE_BYTE_SIZE_DEF;      /* Default raw data chunk cache # of bytes */
    double rdcc_w0 = H5D_ACS_PREEMPT_READ_CHUNKS_DEF;           /* Default raw data chunk cache dirty ratio */
    H5D_vds_view_t virtual_view = H5D_ACS_VDS_VIEW_DEF;         /* Default VDS view option */
    hsize_t printf_gap = H5D_ACS_VDS_PRINTF_GAP_DEF;            /* Default VDS printf gap */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Register the size of raw data chunk cache (elements) */
    if(H5P_register_real(pclass, H5D_ACS_DATA_CACHE_NUM_SLOTS_NAME, H5D_ACS_DATA_CACHE_NUM_SLOTS_SIZE, &rdcc_nslots, 
             NULL, NULL, NULL, H5D_ACS_DATA_CACHE_NUM_SLOTS_ENC, H5D_ACS_DATA_CACHE_NUM_SLOTS_DEC, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the size of raw data chunk cache(bytes) */
    if(H5P_register_real(pclass, H5D_ACS_DATA_CACHE_BYTE_SIZE_NAME, H5D_ACS_DATA_CACHE_BYTE_SIZE_SIZE, &rdcc_nbytes, 
             NULL, NULL, NULL, H5D_ACS_DATA_CACHE_BYTE_SIZE_ENC, H5D_ACS_DATA_CACHE_BYTE_SIZE_DEC, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the preemption for reading chunks */
    if(H5P_register_real(pclass, H5D_ACS_PREEMPT_READ_CHUNKS_NAME, H5D_ACS_PREEMPT_READ_CHUNKS_SIZE, &rdcc_w0, 
             NULL, NULL, NULL, H5D_ACS_PREEMPT_READ_CHUNKS_ENC, H5D_ACS_PREEMPT_READ_CHUNKS_DEC, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the VDS view option */
    if(H5P_register_real(pclass, H5D_ACS_VDS_VIEW_NAME, H5D_ACS_VDS_VIEW_SIZE, &virtual_view,
            NULL, NULL, NULL, H5D_ACS_VDS_VIEW_ENC, H5D_ACS_VDS_VIEW_DEC,
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the VDS printf gap */
    if(H5P_register_real(pclass, H5D_ACS_VDS_PRINTF_GAP_NAME, H5D_ACS_VDS_PRINTF_GAP_SIZE, &printf_gap,
            NULL, NULL, NULL, H5D_ACS_VDS_PRINTF_GAP_ENC, H5D_ACS_VDS_PRINTF_GAP_DEC,
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__dacc_reg_prop() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_chunk_cache
 *
 * Purpose:	Set the number of objects in the meta data cache and the
 *		maximum number of chunks and bytes in the raw data chunk cache.
 *      Once set, these values will override the values in the file access
 *      property list.  Each of thhese values can be individually unset
 *      (or not set at all) by passing the macros:
 *      H5D_CHUNK_CACHE_NCHUNKS_DEFAULT,
 *      H5D_CHUNK_CACHE_NSLOTS_DEFAULT, and/or
 *      H5D_CHUNK_CACHE_W0_DEFAULT
 *      as appropriate.
 *
 * 		The RDCC_W0 value should be between 0 and 1 inclusive and
 *		indicates how much chunks that have been fully read or fully
 *		written are favored for preemption.  A value of zero means
 *		fully read or written chunks are treated no differently than
 *		other chunks (the preemption is strictly LRU) while a value
 *		of one means fully read chunks are always preempted before
 *		other chunks.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Neil Fortner
 *              Monday, October 27, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_chunk_cache(hid_t dapl_id, size_t rdcc_nslots, size_t rdcc_nbytes, double rdcc_w0)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "izzd", dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0);

    /* Check arguments.  Note that we allow negative values - they are
     * considered to "unset" the property. */
    if(rdcc_w0 > (double)1.0f)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "raw data cache w0 value must be between 0.0 and 1.0 inclusive, or H5D_CHUNK_CACHE_W0_DEFAULT");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(dapl_id,H5P_DATASET_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set sizes */
    if(H5P_set(plist, H5D_ACS_DATA_CACHE_NUM_SLOTS_NAME, &rdcc_nslots) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data cache number of chunks");
    if(H5P_set(plist, H5D_ACS_DATA_CACHE_BYTE_SIZE_NAME, &rdcc_nbytes) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data cache byte size");
    if(H5P_set(plist, H5D_ACS_PREEMPT_READ_CHUNKS_NAME, &rdcc_w0) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set preempt read chunks");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_chunk_cache() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_chunk_cache
 *
 * Purpose:	Retrieves the maximum possible number of elements in the meta
 *		data cache and the maximum possible number of elements and
 *		bytes and the RDCC_W0 value in the raw data chunk cache.  Any
 *		(or all) arguments may be null pointers in which case the
 *		corresponding datum is not returned.  If these properties have
 *      not been set on this property list, the default values for a
 *      file access property list are returned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Neil Fortner
 *              Monday, October 27, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_chunk_cache(hid_t dapl_id, size_t *rdcc_nslots, size_t *rdcc_nbytes, double *rdcc_w0)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    H5P_genplist_t *def_plist;  /* Default file access property list */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "i*z*z*d", dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0);

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(dapl_id, H5P_DATASET_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get default file access plist */
    if (NULL == (def_plist = (H5P_genplist_t *)H5I_object(H5P_FILE_ACCESS_DEFAULT)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for default fapl ID");

    /* Get the properties.  If a property is set to the default value, the value
     * from the default fapl is used. */
    if (rdcc_nslots) {
        if (H5P_get(plist, H5D_ACS_DATA_CACHE_NUM_SLOTS_NAME, rdcc_nslots) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get data cache number of slots");
        if (*rdcc_nslots == H5D_CHUNK_CACHE_NSLOTS_DEFAULT)
            if (H5P_get(def_plist, H5F_ACS_DATA_CACHE_NUM_SLOTS_NAME, rdcc_nslots) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get default data cache number of slots");
    } /* end if */
    if (rdcc_nbytes) {
        if (H5P_get(plist, H5D_ACS_DATA_CACHE_BYTE_SIZE_NAME, rdcc_nbytes) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get data cache byte size");
        if (*rdcc_nbytes == H5D_CHUNK_CACHE_NBYTES_DEFAULT)
            if (H5P_get(def_plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, rdcc_nbytes) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get default data cache byte size");
    } /* end if */
    if (rdcc_w0) {
        if (H5P_get(plist, H5D_ACS_PREEMPT_READ_CHUNKS_NAME, rdcc_w0) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get preempt read chunks");
        if (*rdcc_w0 < 0)
            if (H5P_get(def_plist, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, rdcc_w0) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get default preempt read chunks");
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_chunk_cache() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_virtual_view
 *
 * Purpose:     Takes the access property list for the virtual dataset,
 *              dapl_id, and the flag, view, and sets the VDS view
 *              according to the flag value.  The view will include all
 *              data before the first missing mapped data found if the
 *              flag is set to H5D_VDS_FIRST_MISSING or to include all
 *              available mapped data if the flag is set to
 *              H5D_VDS_LAST_AVAIALBLE.  Missing mapped data will be
 *              filled with the fill value according to the VDS creation
 *              property settings.  For VDS with unlimited mappings, the
 *              view defines the extent.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              May 4, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_virtual_view(hid_t plist_id, H5D_vds_view_t view)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iDv", plist_id, view);

    /* Check argument */
    if((view != H5D_VDS_FIRST_MISSING) && (view != H5D_VDS_LAST_AVAILABLE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid bounds option")

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_DATASET_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Update property list */
    if(H5P_set(plist, H5D_ACS_VDS_VIEW_NAME, &view) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_virtual_view() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_virtual_view
 *
 * Purpose:     Takes the access property list for the virtual dataset,
 *              dapl_id, and gets the flag, view, set by the
 *              H5Pset_virtual_view call.  The possible values of view are
 *              H5D_VDS_FIRST_MISSING or H5D_VDS_LAST_AVAIALBLE. 
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              May 4, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_virtual_view(hid_t plist_id, H5D_vds_view_t *view)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Dv", plist_id, view);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_DATASET_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get value from property list */
    if(view)
        if(H5P_get(plist, H5D_ACS_VDS_VIEW_NAME, view) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_virtual_view() */


/*-------------------------------------------------------------------------
 * Function:    H5P__dacc_vds_view_enc
 *
 * Purpose:     Callback routine which is called whenever the vds view
 *              property in the dataset access property list is encoded.
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Neil Fortner
 *              Tuesday, May 5, 2015
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__dacc_vds_view_enc(const void *value, void **_pp, size_t *size)
{
    const H5D_vds_view_t *view = (const H5D_vds_view_t *)value; /* Create local alias for values */
    uint8_t **pp = (uint8_t **)_pp;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(view);
    HDassert(size);

    if(NULL != *pp)
        /* Encode EDC property */
        *(*pp)++ = (uint8_t)*view;

    /* Size of EDC property */
    (*size)++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__dacc_vds_view_enc() */


/*-------------------------------------------------------------------------
 * Function:    H5P__dacc_vds_view_dec
 *
 * Purpose:     Callback routine which is called whenever the vds view
 *              property in the dataset access property list is encoded.
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Neil Fortner
 *              Tuesday, May 5, 2015
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__dacc_vds_view_dec(const void **_pp, void *_value)
{
    H5D_vds_view_t *view = (H5D_vds_view_t *)_value;
    const uint8_t **pp = (const uint8_t **)_pp;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(pp);
    HDassert(*pp);
    HDassert(view);

    /* Decode EDC property */
    *view = (H5D_vds_view_t)*(*pp)++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__dacc_vds_view_dec() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_virtual_printf_gap
 *
 * Purpose:     Sets the access property list for the virtual dataset,
 *              dapl_id, to instruct the library to stop looking for the
 *              mapped data stored in the files and/or datasets with the
 *              printf-style names after not finding gap_size files and/or
 *              datasets.  The found source files and datasets will
 *              determine the extent of the unlimited VDS with the printf
 *              -style mappings.
 *
 *              For example, if regularly spaced blocks of VDS are mapped
 *              to datasets with the names d-1, d-2, d-3, ..., d-N, ...,
 *              and d-2 dataset is missing and gap_size is set to 0, then
 *              VDS will contain only data found in d-1.  If d-2 and d-3
 *              are missing and gap_size is set to 2, then VDS will
 *              contain the data from d-1, d-3, ..., d-N, ....  The blocks
 *              that are mapped to d-2 and d-3 will be filled according to
 *              the VDS fill value setting.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              May 21, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_virtual_printf_gap(hid_t plist_id, hsize_t gap_size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ih", plist_id, gap_size);

    /* Check argument */
    if(gap_size == HSIZE_UNDEF)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid printf gap size")

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_DATASET_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Update property list */
    if(H5P_set(plist, H5D_ACS_VDS_PRINTF_GAP_NAME, &gap_size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_virtual_printf_gap() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_virtual_printf_gap
 *
 * Purpose:     Gets the maximum number of missing printf-style files
 *              and/or datasets for determining the extent of the
 *              unlimited VDS, gap_size, using the access property list
 *              for the virtual dataset, dapl_id.  The default library
 *              value for gap_size is 0.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              May 21, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_virtual_printf_gap(hid_t plist_id, hsize_t *gap_size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*h", plist_id, gap_size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_DATASET_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get value from property list */
    if(gap_size)
        if(H5P_get(plist, H5D_ACS_VDS_PRINTF_GAP_NAME, gap_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_virtual_printf_gap() */

