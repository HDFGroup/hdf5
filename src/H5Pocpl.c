/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:   H5Pocpl.c
 *            Nov 28 2006
 *            Quincey Koziol
 *
 * Purpose:   Object creation property list class routines
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#define H5O_PACKAGE /*suppress error about including H5Opkg	  */
#define H5P_PACKAGE /*suppress error about including H5Ppkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions			*/
#include "H5Eprivate.h"  /* Error handling		  	*/
#include "H5Iprivate.h"  /* IDs			  		*/
#include "H5Opkg.h"      /* Object headers			*/
#include "H5Ppkg.h"      /* Property lists		  	*/
#include "H5PLprivate.h" /* Dynamic plugin			*/
#include "H5Zprivate.h"  /* Filter pipeline			*/

/****************/
/* Local Macros */
/****************/

/* ========= Object Creation properties ============ */
/* Definitions for the max. # of attributes to store compactly */
#define H5O_CRT_ATTR_MAX_COMPACT_SIZE sizeof(unsigned)
/* Definitions for the min. # of attributes to store densely */
#define H5O_CRT_ATTR_MIN_DENSE_SIZE sizeof(unsigned)
/* Definitions for object header flags */
#define H5O_CRT_OHDR_FLAGS_SIZE sizeof(uint8_t)
/* Definitions for filter pipeline */
#define H5O_CRT_PIPELINE_SIZE sizeof(H5O_pline_t)
#define H5O_CRT_PIPELINE_CMP  H5P__ocrt_pipeline_cmp

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
static herr_t H5P__ocrt_reg_prop(H5P_genclass_t *pclass);
static herr_t H5P__ocrt_copy(hid_t new_plist_t, hid_t old_plist_t, void *copy_data);
static herr_t H5P__ocrt_close(hid_t dxpl_id, void *close_data);

/* Property callbacks */
static int H5P__ocrt_pipeline_cmp(const void *value1, const void *value2, size_t size);

/* Local routines */
static herr_t H5P__set_filter(H5P_genplist_t *plist, H5Z_filter_t filter, unsigned int flags,
                              size_t cd_nelmts, const unsigned int cd_values[/*cd_nelmts*/]);

/*********************/
/* Package Variables */
/*********************/

/* Object creation property list class library initialization object */
const H5P_libclass_t H5P_CLS_OCRT[1] = {{
    "object create",        /* Class name for debugging     */
    H5P_TYPE_OBJECT_CREATE, /* Class type                   */

    &H5P_CLS_ROOT_g,             /* Parent class                 */
    &H5P_CLS_OBJECT_CREATE_g,    /* Pointer to class             */
    &H5P_CLS_OBJECT_CREATE_ID_g, /* Pointer to class ID          */
    NULL,                        /* Pointer to default property list ID   */
    H5P__ocrt_reg_prop,          /* Default property registration routine */

    NULL,            /* Class creation callback      */
    NULL,            /* Class creation callback info */
    H5P__ocrt_copy,  /* Class copy callback          */
    NULL,            /* Class copy callback info     */
    H5P__ocrt_close, /* Class close callback         */
    NULL             /* Class close callback info    */
}};

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/*-------------------------------------------------------------------------
 * Function:    H5P__ocrt_reg_prop
 *
 * Purpose:     Initialize the object creation property list class
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              November 28, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__ocrt_reg_prop(H5P_genclass_t *pclass)
{
    unsigned attr_max_compact =
        H5O_CRT_ATTR_MAX_COMPACT_DEF; /* Default max. compact attribute storage settings */
    unsigned attr_min_dense = H5O_CRT_ATTR_MIN_DENSE_DEF; /* Default min. dense attribute storage settings */
    uint8_t  ohdr_flags     = H5O_CRT_OHDR_FLAGS_DEF;     /* Default object header flag settings */
    H5O_pline_t pline       = H5O_CRT_PIPELINE_DEF;       /* Default I/O pipeline setting */
    herr_t      ret_value   = SUCCEED;                    /* Return value */

    FUNC_ENTER_STATIC

    /* Register max. compact attribute storage property */
    if (H5P_register_real(pclass, H5O_CRT_ATTR_MAX_COMPACT_NAME, H5O_CRT_ATTR_MAX_COMPACT_SIZE,
                          &attr_max_compact, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register min. dense attribute storage property */
    if (H5P_register_real(pclass, H5O_CRT_ATTR_MIN_DENSE_NAME, H5O_CRT_ATTR_MIN_DENSE_SIZE, &attr_min_dense,
                          NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register object header flags property */
    if (H5P_register_real(pclass, H5O_CRT_OHDR_FLAGS_NAME, H5O_CRT_OHDR_FLAGS_SIZE, &ohdr_flags, NULL, NULL,
                          NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the pipeline property */
    if (H5P_register_real(pclass, H5O_CRT_PIPELINE_NAME, H5O_CRT_PIPELINE_SIZE, &pline, NULL, NULL, NULL,
                          NULL, NULL, H5O_CRT_PIPELINE_CMP, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__ocrt_reg_prop() */

/*-------------------------------------------------------------------------
 * Function:       H5P__ocrt_copy
 *
 * Purpose:        Callback routine which is called whenever any object
 *                 creation property list is copied.  This routine copies
 *                 the properties from the old list to the new list.
 *
 * Return:         Success:        Non-negative
 *                 Failure:        Negative
 *
 * Programmer:     Neil Fortner
 *                 Monday, September 21, 2009
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P__ocrt_copy(hid_t dst_plist_id, hid_t src_plist_id, void H5_ATTR_UNUSED *copy_data)
{
    H5O_pline_t     src_pline, dst_pline; /* Source & destination pipelines */
    H5P_genplist_t *src_plist;            /* Pointer to source property list */
    H5P_genplist_t *dst_plist;            /* Pointer to destination property list */
    herr_t          ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_STATIC

    /* Verify property list IDs */
    if (NULL == (dst_plist = (H5P_genplist_t *)H5I_object(dst_plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an object creation property list")
    if (NULL == (src_plist = (H5P_genplist_t *)H5I_object(src_plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an object creation property list")

    /* Get the link pipeline property from the old property list */
    if (H5P_get(src_plist, H5O_CRT_PIPELINE_NAME, &src_pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline")

    /* Make copy of link pipeline */
    if (NULL == H5O_msg_copy(H5O_PLINE_ID, &src_pline, &dst_pline))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't copy link pipeline")

    /* Set the link pipeline property for the destination property list */
    if (H5P_set(dst_plist, H5O_CRT_PIPELINE_NAME, &dst_pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__ocrt_copy() */

/*-------------------------------------------------------------------------
 * Function:	H5P__ocrt_close
 *
 * Purpose:	Callback routine which is called whenever any object create
 *              property list is closed.  This routine performs any generic
 *              cleanup needed on the properties the library put into the list.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:     Neil Fortner
 *                 Monday, September 21, 2009
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P__ocrt_close(hid_t dcpl_id, void H5_ATTR_UNUSED *close_data)
{
    H5O_pline_t     pline;               /* I/O pipeline */
    H5P_genplist_t *plist;               /* Property list */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    if (NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an object creation property list")

    /* Get the link pipeline property from the old property list */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline")

    /* Clean up any values set for the link pipeline */
    if (H5O_msg_reset(H5O_PLINE_ID, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't release pipeline info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__ocrt_close() */

/*-------------------------------------------------------------------------
 * Function:	H5Pset_attr_phase_change
 *
 * Purpose:    Sets the cutoff values for indexes storing attributes
 *              in object headers for this file.  If more than max_compact
 *              attributes are in an object header, the attributes will be
 *              moved to a heap and indexed with a B-tree.
 *              Likewise, an object header containing fewer than min_dense
 *              attributes will be converted back to storing the attributes
 *              directly in the object header.
 *
 *              If the max_compact is zero then attributes for this object will
 *              never be stored in the object header but will be always be
 *              stored in a heap.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * Programmer:    Quincey Koziol
 *        Tuesday, November 28, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_attr_phase_change(hid_t plist_id, unsigned max_compact, unsigned min_dense)
{
    H5P_genplist_t *plist;               /* Property list pointer */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iIuIu", plist_id, max_compact, min_dense);

    /* Range check values */
    if (max_compact < min_dense)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "max compact value must be >= min dense value")
    if (max_compact > 65535)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "max compact value must be < 65536")
    if (min_dense > 65535)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "min dense value must be < 65536")

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set property values */
    if (H5P_set(plist, H5O_CRT_ATTR_MAX_COMPACT_NAME, &max_compact) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set max. # of compact attributes in property list")
    if (H5P_set(plist, H5O_CRT_ATTR_MIN_DENSE_NAME, &min_dense) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set min. # of dense attributes in property list")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_attr_phase_change */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_attr_phase_change
 *
 * Purpose:    Gets the phase change values for attribute storage
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * Programmer:    Quincey Koziol
 *        Tuesday, November 28, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_attr_phase_change(hid_t plist_id, unsigned *max_compact, unsigned *min_dense)
{
    H5P_genplist_t *plist;               /* Property list pointer */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*Iu*Iu", plist_id, max_compact, min_dense);

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get values */
    if (max_compact) {
        if (H5P_get(plist, H5O_CRT_ATTR_MAX_COMPACT_NAME, max_compact) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get max. # of compact attributes")
    } /* end if */
    if (min_dense) {
        if (H5P_get(plist, H5O_CRT_ATTR_MIN_DENSE_NAME, min_dense) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get min. # of dense attributes")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_attr_phase_change() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_attr_creation_order
 *
 * Purpose:     Set the flags for creation order of attributes on an object
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              February  6, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_attr_creation_order(hid_t plist_id, unsigned crt_order_flags)
{
    H5P_genplist_t *plist;               /* Property list pointer */
    uint8_t         ohdr_flags;          /* Object header flags */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", plist_id, crt_order_flags);

    /* Check for bad combination of flags */
    if (!(crt_order_flags & H5P_CRT_ORDER_TRACKED) && (crt_order_flags & H5P_CRT_ORDER_INDEXED))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "tracking creation order is required for index")

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get object header flags */
    if (H5P_get(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object header flags")

    /* Mask off previous attribute creation order flag settings */
    ohdr_flags &= (uint8_t) ~(H5O_HDR_ATTR_CRT_ORDER_TRACKED | H5O_HDR_ATTR_CRT_ORDER_INDEXED);

    /* Update with new attribute creation order flags */
    ohdr_flags = (uint8_t)(ohdr_flags |
                           ((crt_order_flags & H5P_CRT_ORDER_TRACKED) ? H5O_HDR_ATTR_CRT_ORDER_TRACKED : 0));
    ohdr_flags = (uint8_t)(ohdr_flags |
                           ((crt_order_flags & H5P_CRT_ORDER_INDEXED) ? H5O_HDR_ATTR_CRT_ORDER_INDEXED : 0));

    /* Set object header flags */
    if (H5P_set(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set object header flags")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_attr_creation_order() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_attr_creation_order
 *
 * Purpose:     Returns the flags indicating creation order is tracked/indexed
 *              for attributes on an object.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              February  6, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_attr_creation_order(hid_t plist_id, unsigned *crt_order_flags)
{
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Iu", plist_id, crt_order_flags);

    /* Get values */
    if (crt_order_flags) {
        H5P_genplist_t *plist;      /* Property list pointer */
        uint8_t         ohdr_flags; /* Object header flags */

        /* Reset the value to return */
        *crt_order_flags = 0;

        /* Get the plist structure */
        if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

        /* Get object header flags */
        if (H5P_get(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object header flags")

        /* Set creation order flags to return */
        *crt_order_flags |= (ohdr_flags & H5O_HDR_ATTR_CRT_ORDER_TRACKED) ? H5P_CRT_ORDER_TRACKED : 0;
        *crt_order_flags |= (ohdr_flags & H5O_HDR_ATTR_CRT_ORDER_INDEXED) ? H5P_CRT_ORDER_INDEXED : 0;
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_attr_creation_order() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_obj_track_times
 *
 * Purpose:     Set whether the birth, access, modification & change times for
 *              an object are stored.
 *
 *              Birth time is the time the object was created.  Access time is
 *              the last time that metadata or raw data was read from this
 *              object.  Modification time is the last time the data for
 *              this object was changed (either writing raw data to a dataset
 *              or inserting/modifying/deleting a link in a group).  Change
 *              time is the last time the metadata for this object was written
 *              (adding/modifying/deleting an attribute on an object, extending
 *              the size of a dataset, etc).
 *
 *              If these times are not tracked, they will be reported as
 *              12:00 AM UDT, Jan. 1, 1970 (i.e. 0 seconds past the UNIX
 *              epoch) when queried.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              March  1, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_obj_track_times(hid_t plist_id, hbool_t track_times)
{
    H5P_genplist_t *plist;               /* Property list pointer */
    uint8_t         ohdr_flags;          /* Object header flags */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ib", plist_id, track_times);

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get object header flags */
    if (H5P_get(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object header flags")

    /* Mask off previous time tracking flag settings */
    ohdr_flags &= (uint8_t)~H5O_HDR_STORE_TIMES;

    /* Update with new time tracking flag */
    ohdr_flags = (uint8_t)(ohdr_flags | (track_times ? H5O_HDR_STORE_TIMES : 0));

    /* Set object header flags */
    if (H5P_set(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set object header flags")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_obj_track_times() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_obj_track_times
 *
 * Purpose:     Returns whether times are tracked for an object.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              March  1, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_obj_track_times(hid_t plist_id, hbool_t *track_times)
{
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*b", plist_id, track_times);

    /* Get values */
    if (track_times) {
        H5P_genplist_t *plist;      /* Property list pointer */
        uint8_t         ohdr_flags; /* Object header flags */

        /* Get the plist structure */
        if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

        /* Get object header flags */
        if (H5P_get(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object header flags")

        /* Set track times flag to return */
        *track_times = (hbool_t)((ohdr_flags & H5O_HDR_STORE_TIMES) ? TRUE : FALSE);
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_obj_track_times() */

/*-------------------------------------------------------------------------
 * Function:    H5P_modify_filter
 *
 * Purpose:    Modifies the specified FILTER in the
 *        transient or permanent output filter pipeline
 *        depending on whether PLIST is a dataset creation or dataset
 *        transfer property list.  The FLAGS argument specifies certain
 *        general properties of the filter and is documented below.
 *        The CD_VALUES is an array of CD_NELMTS integers which are
 *        auxiliary data for the filter.  The integer vlues will be
 *        stored in the dataset object header as part of the filter
 *        information.
 *
 *         The FLAGS argument is a bit vector of the following fields:
 *
 *         H5Z_FLAG_OPTIONAL(0x0001)
 *        If this bit is set then the filter is optional.  If the
 *        filter fails during an H5Dwrite() operation then the filter
 *        is just excluded from the pipeline for the chunk for which it
 *        failed; the filter will not participate in the pipeline
 *        during an H5Dread() of the chunk.  If this bit is clear and
 *        the filter fails then the entire I/O operation fails.
 *      If this bit is set but encoding is disabled for a filter,
 *      attempting to write will generate an error.
 *
 * Note:    This function currently supports only the permanent filter
 *        pipeline.  That is, PLIST_ID must be a dataset creation
 *        property list.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * Programmer:    Quincey Koziol
 *              Wednesday, October 17, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_modify_filter(H5P_genplist_t *plist, H5Z_filter_t filter, unsigned flags, size_t cd_nelmts,
                  const unsigned cd_values[/*cd_nelmts*/])
{
    H5O_pline_t pline;
    herr_t      ret_value = SUCCEED; /* return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Get the pipeline property to modify */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline")

    /* Modify the filter parameters of the I/O pipeline */
    if (H5Z_modify(&pline, filter, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add filter to pipeline")

    /* Put the I/O pipeline information back into the property list */
    if (H5P_set(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_modify_filter() */

/*-------------------------------------------------------------------------
 * Function:    H5Pmodify_filter
 *
 * Purpose:     Modifies the specified FILTER in the
 *              transient or permanent output filter pipeline
 *              depending on whether PLIST is a dataset creation or dataset
 *              transfer property list.  The FLAGS argument specifies certain
 *              general properties of the filter and is documented below.
 *              The CD_VALUES is an array of CD_NELMTS integers which are
 *              auxiliary data for the filter.  The integer vlues will be
 *              stored in the dataset object header as part of the filter
 *              information.
 *
 *              The FLAGS argument is a bit vector of the following fields:
 *
 *              H5Z_FLAG_OPTIONAL(0x0001)
 *              If this bit is set then the filter is optional.  If the
 *              filter fails during an H5Dwrite() operation then the filter
 *              is just excluded from the pipeline for the chunk for which it
 *              failed; the filter will not participate in the pipeline
 *              during an H5Dread() of the chunk.  If this bit is clear and
 *              the filter fails then the entire I/O operation fails.
 *      If this bit is set but encoding is disabled for a filter,
 *      attempting to write will generate an error.
 *
 * Note:        This function currently supports only the permanent filter
 *              pipeline.  That is, PLIST_ID must be a dataset creation
 *              property list.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Friday, April  5, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pmodify_filter(hid_t plist_id, H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts,
                 const unsigned int cd_values[/*cd_nelmts*/])
{
    H5P_genplist_t *plist;               /* Property list */
    herr_t          ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "iZfIuz*[a3]Iu", plist_id, filter, flags, cd_nelmts, cd_values);

    /* Check args */
    if (filter < 0 || filter > H5Z_FILTER_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid filter identifier")
    if (flags & ~((unsigned)H5Z_FLAG_DEFMASK))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags")
    if (cd_nelmts > 0 && !cd_values)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no client data values supplied")

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Modify the filter parameters of the I/O pipeline */
    if (H5P_modify_filter(plist, filter, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't modify filter")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pmodify_filter() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_filter
 *
 * Purpose:    Adds the specified FILTER and corresponding properties to the
 *        end of the data or link output filter pipeline
 *        depending on whether PLIST is a dataset creation or group
 *        creation property list.  The FLAGS argument specifies certain
 *        general properties of the filter and is documented below.
 *        The CD_VALUES is an array of CD_NELMTS integers which are
 *        auxiliary data for the filter.  The integer vlues will be
 *        stored in the dataset object header as part of the filter
 *        information.
 *
 *         The FLAGS argument is a bit vector of the following fields:
 *
 *         H5Z_FLAG_OPTIONAL(0x0001)
 *        If this bit is set then the filter is optional.  If the
 *        filter fails during an H5Dwrite() operation then the filter
 *        is just excluded from the pipeline for the chunk for which it
 *        failed; the filter will not participate in the pipeline
 *        during an H5Dread() of the chunk.  If this bit is clear and
 *        the filter fails then the entire I/O operation fails.
 *      If this bit is set but encoding is disabled for a filter,
 *      attempting to write will generate an error.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * Programmer:    Robb Matzke
 *              Wednesday, April 15, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_filter(hid_t plist_id, H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts,
              const unsigned int cd_values[/*cd_nelmts*/])
{
    H5P_genplist_t *plist;               /* Property list */
    herr_t          ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "iZfIuz*[a3]Iu", plist_id, filter, flags, cd_nelmts, cd_values);

    /* Check args */
    if (filter < 0 || filter > H5Z_FILTER_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid filter identifier")
    if (flags & ~((unsigned)H5Z_FLAG_DEFMASK))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags")
    if (cd_nelmts > 0 && !cd_values)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no client data values supplied")

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Call the private function */
    if (H5P__set_filter(plist, filter, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "failed to call private function")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_filter() */

/*-------------------------------------------------------------------------
 * Function:    H5P__set_filter
 *
 * Purpose:    Adds the specified FILTER and corresponding properties to the
 *        end of the data or link output filter pipeline
 *        depending on whether PLIST is a dataset creation or group
 *        creation property list.  The FLAGS argument specifies certain
 *        general properties of the filter and is documented below.
 *        The CD_VALUES is an array of CD_NELMTS integers which are
 *        auxiliary data for the filter.  The integer vlues will be
 *        stored in the dataset object header as part of the filter
 *        information.
 *
 *         The FLAGS argument is a bit vector of the following fields:
 *
 *         H5Z_FLAG_OPTIONAL(0x0001)
 *        If this bit is set then the filter is optional.  If the
 *        filter fails during an H5Dwrite() operation then the filter
 *        is just excluded from the pipeline for the chunk for which it
 *        failed; the filter will not participate in the pipeline
 *        during an H5Dread() of the chunk.  If this bit is clear and
 *        the filter fails then the entire I/O operation fails.
 *        If this bit is set but encoding is disabled for a filter,
 *        attempting to write will generate an error.
 *
 *              If the filter is not registered, this function tries to load
 *              it dynamically during run time.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * Programmer:    Robb Matzke
 *              Wednesday, April 15, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__set_filter(H5P_genplist_t *plist, H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts,
                const unsigned int cd_values[/*cd_nelmts*/])
{
    H5O_pline_t pline;               /* Filter pipeline */
    htri_t      filter_avail;        /* Filter availability */
    herr_t      ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Check if filter is already available */
    if ((filter_avail = H5Z_filter_avail(filter)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't check filter availability")

    /* Get the pipeline property to append to */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline")

    /* Add the filter to the I/O pipeline */
    if (H5Z_append(&pline, filter, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add filter to pipeline")

    /* Put the I/O pipeline information back into the property list */
    if (H5P_set(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__set_filter() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_nfilters
 *
 * Purpose:    Returns the number of filters in the data or link
 *        pipeline depending on whether PLIST_ID is a dataset creation
 *        or group creation property list.  In each pipeline the
 *        filters are numbered from zero through N-1 where N is the
 *        value returned by this function.  During output to the file
 *        the filters of a pipeline are applied in increasing order
 *        (the inverse is true for input).
 *
 * Return:    Success:    Number of filters or zero if there are none.
 *
 *        Failure:    Negative
 *
 * Programmer:    Robb Matzke
 *              Tuesday, August  4, 1998
 *
 *-------------------------------------------------------------------------
 */
int
H5Pget_nfilters(hid_t plist_id)
{
    H5P_genplist_t *plist;     /* Property list */
    H5O_pline_t     pline;     /* Filter pipeline */
    int             ret_value; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("Is", "i", plist_id);

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the pipeline property to query */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline")

    /* Set return value */
    ret_value = (int)(pline.nused);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_nfilters */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_filter2
 *
 * Purpose:    This is the query counterpart of H5Pset_filter() and returns
 *        information about a particular filter number in a permanent
 *        or transient pipeline depending on whether PLIST_ID is a
 *        dataset creation or transfer property list.  On input,
 *        CD_NELMTS indicates the number of entries in the CD_VALUES
 *        array allocated by the caller while on exit it contains the
 *        number of values defined by the filter.  FILTER_CONFIG is a bit
 *      field contaning encode/decode flags from H5Zpublic.h.  The IDX
 *      should be a value between zero and N-1 as described for
 *      H5Pget_nfilters() and the function will return failure if the
 *      filter number is out of range.
 *
 * Return:    Success:    Filter identification number.
 *
 *        Failure:    H5Z_FILTER_ERROR (Negative)
 *
 * Programmer:    Robb Matzke
 *              Wednesday, April 15, 1998
 *
 *-------------------------------------------------------------------------
 */
H5Z_filter_t
H5Pget_filter2(hid_t plist_id, unsigned idx, unsigned int *flags /*out*/, size_t *cd_nelmts /*in_out*/,
               unsigned cd_values[] /*out*/, size_t namelen, char name[] /*out*/,
               unsigned *filter_config /*out*/)
{
    H5P_genplist_t *         plist;     /* Property list */
    H5O_pline_t              pline;     /* Filter pipeline */
    const H5Z_filter_info_t *filter;    /* Pointer to filter information */
    H5Z_filter_t             ret_value; /* return value */

    FUNC_ENTER_API(H5Z_FILTER_ERROR)
    H5TRACE8("Zf", "iIux*zxzxx", plist_id, idx, flags, cd_nelmts, cd_values, namelen, name, filter_config);

    /* Check args */
    if (cd_nelmts || cd_values) {
        /*
         * It's likely that users forget to initialize this on input, so
         * we'll check that it has a reasonable value.  The actual number
         * is unimportant because the H5O layer will detect when a message
         * is too large.
         */
        if (cd_nelmts && *cd_nelmts > 256)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR,
                        "probable uninitialized *cd_nelmts argument")
        if (cd_nelmts && *cd_nelmts > 0 && !cd_values)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "client data values not supplied")

        /*
         * If cd_nelmts is null but cd_values is non-null then just ignore
         * cd_values
         */
        if (!cd_nelmts)
            cd_values = NULL;
    } /* end if */

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, H5Z_FILTER_ERROR, "can't find object for ID")

    /* Get the pipeline property to query */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5Z_FILTER_ERROR, "can't get pipeline")

    /* Check index */
    if (idx >= pline.nused)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "filter number is invalid")

    /* Set pointer to particular filter to query */
    filter = &pline.filter[idx];

    /* Get filter information */
    if (H5P_get_filter(filter, flags, cd_nelmts, cd_values, namelen, name, filter_config) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5Z_FILTER_ERROR, "can't get filter info")

    /* Set return value */
    ret_value = filter->id;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_filter2() */

/*-------------------------------------------------------------------------
 * Function:    H5P_get_filter_by_id
 *
 * Purpose:    This is an additional query counterpart of H5Pset_filter() and
 *              returns information about a particular filter in a permanent
 *        or transient pipeline depending on whether PLIST_ID is a
 *        dataset creation or transfer property list.  On input,
 *        CD_NELMTS indicates the number of entries in the CD_VALUES
 *        array allocated by the caller while on exit it contains the
 *        number of values defined by the filter.  FILTER_CONFIG is a bit
 *      field contaning encode/decode flags from H5Zpublic.h.  The ID
 *      should be the filter ID to retrieve the parameters for.  If the
 *      filter is not set for the property list, an error will be returned.
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Wednesday, October 17, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_get_filter_by_id(H5P_genplist_t *plist, H5Z_filter_t id, unsigned int *flags /*out*/,
                     size_t *cd_nelmts /*in_out*/, unsigned cd_values[] /*out*/, size_t namelen,
                     char name[] /*out*/, unsigned *filter_config)
{
    H5O_pline_t        pline;               /* Filter pipeline */
    H5Z_filter_info_t *filter;              /* Pointer to filter information */
    herr_t             ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Get pipeline info */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline")

    /* Get pointer to filter in pipeline */
    if (NULL == (filter = H5Z_filter_info(&pline, id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter ID is invalid")

    /* Get filter information */
    if (H5P_get_filter(filter, flags, cd_nelmts, cd_values, namelen, name, filter_config) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get filter info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_get_filter_by_id() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_filter_by_id2
 *
 * Purpose:    This is an additional query counterpart of H5Pset_filter() and
 *              returns information about a particular filter in a permanent
 *        or transient pipeline depending on whether PLIST_ID is a
 *        dataset creation or transfer property list.  On input,
 *        CD_NELMTS indicates the number of entries in the CD_VALUES
 *        array allocated by the caller while on exit it contains the
 *        number of values defined by the filter.  FILTER_CONFIG is a bit
 *      field contaning encode/decode flags from H5Zpublic.h.  The ID
 *      should be the filter ID to retrieve the parameters for.  If the
 *      filter is not set for the property list, an error will be returned.
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Friday, April  5, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_filter_by_id2(hid_t plist_id, H5Z_filter_t id, unsigned int *flags /*out*/,
                     size_t *cd_nelmts /*in_out*/, unsigned cd_values[] /*out*/, size_t namelen,
                     char name[] /*out*/, unsigned *filter_config)
{
    H5P_genplist_t *         plist;               /* Property list */
    H5O_pline_t              pline;               /* Filter pipeline */
    const H5Z_filter_info_t *filter;              /* Pointer to filter information */
    herr_t                   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "iZfx*zxzx*Iu", plist_id, id, flags, cd_nelmts, cd_values, namelen, name, filter_config);

    /* Check args */
    if (cd_nelmts || cd_values) {
        /*
         * It's likely that users forget to initialize this on input, so
         * we'll check that it has a reasonable value.  The actual number
         * is unimportant because the H5O layer will detect when a message
         * is too large.
         */
        if (cd_nelmts && *cd_nelmts > 256)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "probable uninitialized *cd_nelmts argument")
        if (cd_nelmts && *cd_nelmts > 0 && !cd_values)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "client data values not supplied")

        /*
         * If cd_nelmts is null but cd_values is non-null then just ignore
         * cd_values
         */
        if (!cd_nelmts)
            cd_values = NULL;
    } /* end if */

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get filter information */
    if (H5P_get_filter_by_id(plist, id, flags, cd_nelmts, cd_values, namelen, name, filter_config) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get filter info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_filter_by_id2() */

/*-------------------------------------------------------------------------
 * Function:    H5Pall_filters_avail
 *
 * Purpose:    This is a query routine to verify that all the filters set
 *              in the dataset creation property list are available currently.
 *
 * Return:    Success:    TRUE if all filters available, FALSE if one or
 *                              more filters not currently available.
 *        Failure:    FAIL on error
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, April  8, 2003
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5Pall_filters_avail(hid_t plist_id)
{
    H5P_genplist_t *plist;     /* Property list */
    H5O_pline_t     pline;     /* Filter pipeline */
    htri_t          ret_value; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("t", "i", plist_id);

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the pipeline property to query */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline")

    /* Check if all filters are available */
    if ((ret_value = H5Z_all_filters_avail(&pline)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "can't check pipeline information")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pall_filters_avail() */

/*-------------------------------------------------------------------------
 * Function:    H5P_filter_in_pline
 *
 * Purpose:    Check whether the filter is in the pipeline of the object
 *              creation property list.
 *
 * Return:    TRUE:        found
 *        FALSE:        not found
 *              FAIL:         error
 *
 * Programmer:    Raymond Lu
 *              26 April 2013
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5P_filter_in_pline(H5P_genplist_t *plist, H5Z_filter_t id)
{
    H5O_pline_t pline;               /* Filter pipeline */
    htri_t      ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Get pipeline info */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline")

    /* Check if the file is in the pipeline */
    if ((ret_value = H5Z_filter_in_pline(&pline, id)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTCOMPARE, FAIL, "can't find filter")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_get_filter_by_id() */

/*-------------------------------------------------------------------------
 * Function: H5Premove_filter
 *
 * Purpose: Deletes a filter from the dataset creation property list;
 *  deletes all filters if FILTER is H5Z_FILTER_NONE
 *
 * Return: Non-negative on success/Negative on failure
 *
 * Programmer:  Pedro Vicente
 *              January 26, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Premove_filter(hid_t plist_id, H5Z_filter_t filter)
{
    H5P_genplist_t *plist;               /* Property list */
    H5O_pline_t     pline;               /* Filter pipeline */
    herr_t          ret_value = SUCCEED; /* return value          */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iZf", plist_id, filter);

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the pipeline property to modify */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline")

    /* Check if there are any filters */
    if (pline.filter) {
        /* Delete filter */
        if (H5Z_delete(&pline, filter) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't delete filter")

        /* Put the I/O pipeline information back into the property list */
        if (H5P_set(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Premove_filter() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_deflate
 *
 * Purpose:     Sets the compression method for a dataset or group link
 *              filter pipeline (depending on whether PLIST_ID is a dataset
 *              creation or group creation property list) to H5Z_FILTER_DEFLATE
 *              and the compression level to LEVEL which should be a value
 *              between zero and nine, inclusive.  Lower compression levels
 *              are faster but result in less compression.  This is the same
 *              algorithm as used by the GNU gzip program.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:    Robb Matzke
 *              Wednesday, April 15, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_deflate(hid_t plist_id, unsigned level)
{
    H5P_genplist_t *plist;               /* Property list */
    H5O_pline_t     pline;               /* Filter pipeline */
    herr_t          ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", plist_id, level);

    /* Check arguments */
    if (level > 9)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid deflate level")

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the pipeline property to append to */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline")

    /* Add the filter */
    if (H5Z_append(&pline, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, (size_t)1, &level) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add deflate filter to pipeline")

    /* Put the I/O pipeline information back into the property list */
    if (H5P_set(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_deflate() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_fletcher32
 *
 * Purpose:     Sets Fletcher32 checksum of EDC for a dataset creation
 *              property list or group creation property list.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:    Raymond Lu
 *              Dec 19, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fletcher32(hid_t plist_id)
{
    H5P_genplist_t *plist;               /* Property list */
    H5O_pline_t     pline;               /* Filter pipeline */
    herr_t          ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", plist_id);

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the pipeline property to append to */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline")

    /* Add the Fletcher32 checksum as a filter */
    if (H5Z_append(&pline, H5Z_FILTER_FLETCHER32, H5Z_FLAG_MANDATORY, (size_t)0, NULL) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add fletcher32 filter to pipeline")

    /* Put the I/O pipeline information back into the property list */
    if (H5P_set(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fletcher32() */

/*-------------------------------------------------------------------------
 * Function:    H5P__get_filter
 *
 * Purpose:    Internal component of H5Pget_filter & H5Pget_filter_id
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * Programmer:    Quincey Koziol
 *              Monday, October 23, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_get_filter(const H5Z_filter_info_t *filter, unsigned int *flags /*out*/, size_t *cd_nelmts /*in_out*/,
               unsigned cd_values[] /*out*/, size_t namelen, char name[] /*out*/,
               unsigned *filter_config /*out*/)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check arguments */
    HDassert(filter);

    /* Filter flags */
    if (flags)
        *flags = filter->flags;

    /* Filter parameters */
    if (cd_values) {
        size_t i; /* Local index variable */

        for (i = 0; i < filter->cd_nelmts && i < *cd_nelmts; i++)
            cd_values[i] = filter->cd_values[i];
    } /* end if */

    /* Number of filter parameters */
    if (cd_nelmts)
        *cd_nelmts = filter->cd_nelmts;

    /* Filter name */
    if (namelen > 0 && name) {
        const char *s = filter->name;

        /* If there's no name on the filter, use the class's filter name */
        if (!s) {
            H5Z_class2_t *cls = H5Z_find(filter->id);

            if (cls)
                s = cls->name;
        } /* end if */

        /* Check for actual name */
        if (s) {
            HDstrncpy(name, s, namelen);
            name[namelen - 1] = '\0';
        } /* end if */
        else {
            /* Check for unknown library filter */
            /* (probably from a future version of the library) */
            if (filter->id < 256) {
                HDstrncpy(name, "Unknown library filter", namelen);
                name[namelen - 1] = '\0';
            } /* end if */
            else
                name[0] = '\0';
        } /* end if */
    }     /* end if */

    /* Filter configuration (assume filter ID has already been checked) */
    if (filter_config)
        H5Z_get_filter_info(filter->id, filter_config);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P_get_filter() */

/*-------------------------------------------------------------------------
 * Function:       H5P__ocrt_pipeline_cmp
 *
 * Purpose:        Callback routine which is called whenever a filter pipeline
 *                 property in a property list is compared.
 *
 * Return:         positive if VALUE1 is greater than VALUE2, negative if
 *                      VALUE2 is greater than VALUE1 and zero if VALUE1 and
 *                      VALUE2 are equal.
 *
 * Programmer:     Quincey Koziol
 *                 Wednesday, January 7, 2004
 *
 *-------------------------------------------------------------------------
 */
static int
H5P__ocrt_pipeline_cmp(const void *_pline1, const void *_pline2, size_t H5_ATTR_UNUSED size)
{
    const H5O_pline_t *pline1 = (const H5O_pline_t *)_pline1, /* Create local aliases for values */
        *pline2               = (const H5O_pline_t *)_pline2;
    int    cmp_value;     /* Value from comparison */
    herr_t ret_value = 0; /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(pline1);
    HDassert(pline2);
    HDassert(size == sizeof(H5O_pline_t));

    /* Check the number of used pipeline entries */
    if (pline1->nused < pline2->nused)
        HGOTO_DONE(-1);
    if (pline1->nused > pline2->nused)
        HGOTO_DONE(1);

    /* Check the filter entry information */
    if (pline1->filter == NULL && pline2->filter != NULL)
        HGOTO_DONE(-1);
    if (pline1->filter != NULL && pline2->filter == NULL)
        HGOTO_DONE(1);
    if (pline1->filter != NULL && pline1->nused > 0) {
        size_t u; /* Local index variable */

        /* Loop through all filters, comparing them */
        for (u = 0; u < pline1->nused; u++) {
            /* Check the ID of the filter */
            if (pline1->filter[u].id < pline2->filter[u].id)
                HGOTO_DONE(-1);
            if (pline1->filter[u].id > pline2->filter[u].id)
                HGOTO_DONE(1);

            /* Check the flags for the filter */
            if (pline1->filter[u].flags < pline2->filter[u].flags)
                HGOTO_DONE(-1);
            if (pline1->filter[u].flags > pline2->filter[u].flags)
                HGOTO_DONE(1);

            /* Check the name of the filter */
            if (pline1->filter[u].name == NULL && pline2->filter[u].name != NULL)
                HGOTO_DONE(-1);
            if (pline1->filter[u].name != NULL && pline2->filter[u].name == NULL)
                HGOTO_DONE(1);
            if (pline1->filter[u].name != NULL)
                if ((cmp_value = HDstrcmp(pline1->filter[u].name, pline2->filter[u].name)) != 0)
                    HGOTO_DONE(cmp_value);

            /* Check the number of parameters for the filter */
            if (pline1->filter[u].cd_nelmts < pline2->filter[u].cd_nelmts)
                HGOTO_DONE(-1);
            if (pline1->filter[u].cd_nelmts > pline2->filter[u].cd_nelmts)
                HGOTO_DONE(1);

            /* Check the filter parameter information */
            if (pline1->filter[u].cd_values == NULL && pline2->filter[u].cd_values != NULL)
                HGOTO_DONE(-1);
            if (pline1->filter[u].cd_values != NULL && pline2->filter[u].cd_values == NULL)
                HGOTO_DONE(1);
            if (pline1->filter[u].cd_values != NULL && pline1->filter[u].cd_nelmts > 0) {
                size_t v; /* Local index variable */

                /* Loop through all parameters, comparing them */
                for (v = 0; v < pline1->filter[u].cd_nelmts; v++) {
                    /* Check each parameter for the filter */
                    if (pline1->filter[u].cd_values[v] < pline2->filter[u].cd_values[v])
                        HGOTO_DONE(-1);
                    if (pline1->filter[u].cd_values[v] > pline2->filter[u].cd_values[v])
                        HGOTO_DONE(1);
                } /* end for */
            }     /* end if */
        }         /* end for */
    }             /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__ocrt_pipeline_cmp() */

#ifndef H5_NO_DEPRECATED_SYMBOLS

/*-------------------------------------------------------------------------
 * Function:    H5Pget_filter1
 *
 * Purpose:    This is the query counterpart of H5Pset_filter() and returns
 *        information about a particular filter number in a permanent
 *        or transient pipeline depending on whether PLIST_ID is a
 *        dataset creation or transfer property list.  On input,
 *        CD_NELMTS indicates the number of entries in the CD_VALUES
 *        array allocated by the caller while on exit it contains the
 *        number of values defined by the filter.  The IDX
 *      should be a value between zero and N-1 as described for
 *      H5Pget_nfilters() and the function will return failure if the
 *      filter number is out of range.
 *
 * Return:    Success:    Filter identification number.
 *
 *        Failure:    H5Z_FILTER_ERROR (Negative)
 *
 * Programmer:    Robb Matzke
 *              Wednesday, April 15, 1998
 *
 *-------------------------------------------------------------------------
 */
H5Z_filter_t
H5Pget_filter1(hid_t plist_id, unsigned idx, unsigned int *flags /*out*/, size_t *cd_nelmts /*in_out*/,
               unsigned cd_values[] /*out*/, size_t namelen, char name[] /*out*/)
{
    H5O_pline_t              pline;     /* Filter pipeline */
    const H5Z_filter_info_t *filter;    /* Pointer to filter information */
    H5P_genplist_t *         plist;     /* Property list pointer */
    H5Z_filter_t             ret_value; /* return value */

    FUNC_ENTER_API(H5Z_FILTER_ERROR)
    H5TRACE7("Zf", "iIux*zxzx", plist_id, idx, flags, cd_nelmts, cd_values, namelen, name);

    /* Check args */
    if (cd_nelmts || cd_values) {
        /*
         * It's likely that users forget to initialize this on input, so
         * we'll check that it has a reasonable value.  The actual number
         * is unimportant because the H5O layer will detect when a message
         * is too large.
         */
        if (cd_nelmts && *cd_nelmts > 256)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR,
                        "probable uninitialized *cd_nelmts argument")
        if (cd_nelmts && *cd_nelmts > 0 && !cd_values)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "client data values not supplied")

        /*
         * If cd_nelmts is null but cd_values is non-null then just ignore
         * cd_values
         */
        if (!cd_nelmts)
            cd_values = NULL;
    } /* end if */

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, H5Z_FILTER_ERROR, "can't find object for ID")

    /* Get pipeline info */
    if (H5P_get(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5Z_FILTER_ERROR, "can't get pipeline")

    /* Check more args */
    if (idx >= pline.nused)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "filter number is invalid")

    /* Set pointer to particular filter to query */
    filter = &pline.filter[idx];

    /* Get filter information */
    if (H5P_get_filter(filter, flags, cd_nelmts, cd_values, namelen, name, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5Z_FILTER_ERROR, "can't get filter info")

    /* Set return value */
    ret_value = filter->id;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_filter1() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_filter_by_id1
 *
 * Purpose:    This is an additional query counterpart of H5Pset_filter() and
 *              returns information about a particular filter in a permanent
 *        or transient pipeline depending on whether PLIST_ID is a
 *        dataset creation or transfer property list.  On input,
 *        CD_NELMTS indicates the number of entries in the CD_VALUES
 *        array allocated by the caller while on exit it contains the
 *        number of values defined by the filter.  The ID
 *      should be the filter ID to retrieve the parameters for.  If the
 *      filter is not set for the property list, an error will be returned.
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Friday, April  5, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_filter_by_id1(hid_t plist_id, H5Z_filter_t id, unsigned int *flags /*out*/,
                     size_t *cd_nelmts /*in_out*/, unsigned cd_values[] /*out*/, size_t namelen,
                     char name[] /*out*/)
{
    H5P_genplist_t *plist;               /* Property list pointer */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "iZfx*zxzx", plist_id, id, flags, cd_nelmts, cd_values, namelen, name);

    /* Check args */
    if (cd_nelmts || cd_values) {
        /*
         * It's likely that users forget to initialize this on input, so
         * we'll check that it has a reasonable value.  The actual number
         * is unimportant because the H5O layer will detect when a message
         * is too large.
         */
        if (cd_nelmts && *cd_nelmts > 256)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "probable uninitialized *cd_nelmts argument")
        if (cd_nelmts && *cd_nelmts > 0 && !cd_values)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "client data values not supplied")

        /*
         * If cd_nelmts is null but cd_values is non-null then just ignore
         * cd_values
         */
        if (!cd_nelmts)
            cd_values = NULL;
    } /* end if */

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get filter info */
    if (H5P_get_filter_by_id(plist, id, flags, cd_nelmts, cd_values, namelen, name, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get filter info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_filter_by_id1() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */
