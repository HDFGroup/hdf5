/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:   H5Pocpl.c
 *
 * Purpose:   Object creation property list class routines
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5O_FRIEND     /*suppress error about including H5Opkg      */
#define H5Z_FRIEND     /*suppress error about including H5Zpkg      */
#include "H5Pmodule.h" /* This source code file is part of the H5P module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                        */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5MMprivate.h" /* Memory management                        */
#include "H5Opkg.h"      /* Object headers                           */
#include "H5Ppkg.h"      /* Property lists                           */
#include "H5VMprivate.h" /* Vector Functions                         */
#include "H5Zprivate.h"  /* Filter pipeline                          */
#include "H5Zpkg.h"      /* Filter internals (H5Z_entry_t)           */

/****************/
/* Local Macros */
/****************/

/* ========= Object Creation properties ============ */
/* Definitions for the max. # of attributes to store compactly */
#define H5O_CRT_ATTR_MAX_COMPACT_SIZE sizeof(unsigned)
#define H5O_CRT_ATTR_MAX_COMPACT_ENC  H5P__encode_unsigned
#define H5O_CRT_ATTR_MAX_COMPACT_DEC  H5P__decode_unsigned
/* Definitions for the min. # of attributes to store densely */
#define H5O_CRT_ATTR_MIN_DENSE_SIZE sizeof(unsigned)
#define H5O_CRT_ATTR_MIN_DENSE_ENC  H5P__encode_unsigned
#define H5O_CRT_ATTR_MIN_DENSE_DEC  H5P__decode_unsigned
/* Definitions for object header flags */
#define H5O_CRT_OHDR_FLAGS_SIZE sizeof(uint8_t)
#define H5O_CRT_OHDR_FLAGS_ENC  H5P__encode_uint8_t
#define H5O_CRT_OHDR_FLAGS_DEC  H5P__decode_uint8_t
/* Definitions for filter pipeline */
#define H5O_CRT_PIPELINE_SIZE  sizeof(H5O_pline_t)
#define H5O_CRT_PIPELINE_SET   H5P__ocrt_pipeline_set
#define H5O_CRT_PIPELINE_GET   H5P__ocrt_pipeline_get
#define H5O_CRT_PIPELINE_ENC   H5P__ocrt_pipeline_enc
#define H5O_CRT_PIPELINE_DEC   H5P__ocrt_pipeline_dec
#define H5O_CRT_PIPELINE_DEL   H5P__ocrt_pipeline_del
#define H5O_CRT_PIPELINE_COPY  H5P__ocrt_pipeline_copy
#define H5O_CRT_PIPELINE_CMP   H5P__ocrt_pipeline_cmp
#define H5O_CRT_PIPELINE_CLOSE H5P__ocrt_pipeline_close

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

/* Property callbacks */
static herr_t H5P__ocrt_pipeline_enc(const void *value, void **_pp, size_t *size);
static herr_t H5P__ocrt_pipeline_dec(const void **_pp, void *value);
static herr_t H5P__ocrt_pipeline_set(hid_t prop_id, const char *name, size_t size, void *value);
static herr_t H5P__ocrt_pipeline_get(hid_t prop_id, const char *name, size_t size, void *value);
static herr_t H5P__ocrt_pipeline_del(hid_t prop_id, const char *name, size_t size, void *value);
static herr_t H5P__ocrt_pipeline_copy(const char *name, size_t size, void *value);
static int    H5P__ocrt_pipeline_cmp(const void *value1, const void *value2, size_t size);
static herr_t H5P__ocrt_pipeline_close(const char *name, size_t size, void *value);

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

    NULL, /* Class creation callback      */
    NULL, /* Class creation callback info */
    NULL, /* Class copy callback          */
    NULL, /* Class copy callback info     */
    NULL, /* Class close callback         */
    NULL  /* Class close callback info    */
}};

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* Property value defaults */
static const unsigned H5O_def_attr_max_compact_g =
    H5O_CRT_ATTR_MAX_COMPACT_DEF; /* Default max. compact attribute storage settings */
static const unsigned H5O_def_attr_min_dense_g =
    H5O_CRT_ATTR_MIN_DENSE_DEF; /* Default min. dense attribute storage settings */
static const uint8_t H5O_def_ohdr_flags_g = H5O_CRT_OHDR_FLAGS_DEF; /* Default object header flag settings */
static const H5O_pline_t H5O_def_pline_g  = H5O_CRT_PIPELINE_DEF;   /* Default I/O pipeline setting */

/*-------------------------------------------------------------------------
 * Function:    H5P__ocrt_reg_prop
 *
 * Purpose:     Initialize the object creation property list class
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__ocrt_reg_prop(H5P_genclass_t *pclass)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Register max. compact attribute storage property */
    if (H5P__register_real(pclass, H5O_CRT_ATTR_MAX_COMPACT_NAME, H5O_CRT_ATTR_MAX_COMPACT_SIZE,
                           &H5O_def_attr_max_compact_g, NULL, NULL, NULL, H5O_CRT_ATTR_MAX_COMPACT_ENC,
                           H5O_CRT_ATTR_MAX_COMPACT_DEC, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

    /* Register min. dense attribute storage property */
    if (H5P__register_real(pclass, H5O_CRT_ATTR_MIN_DENSE_NAME, H5O_CRT_ATTR_MIN_DENSE_SIZE,
                           &H5O_def_attr_min_dense_g, NULL, NULL, NULL, H5O_CRT_ATTR_MIN_DENSE_ENC,
                           H5O_CRT_ATTR_MIN_DENSE_DEC, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

    /* Register object header flags property */
    if (H5P__register_real(pclass, H5O_CRT_OHDR_FLAGS_NAME, H5O_CRT_OHDR_FLAGS_SIZE, &H5O_def_ohdr_flags_g,
                           NULL, NULL, NULL, H5O_CRT_OHDR_FLAGS_ENC, H5O_CRT_OHDR_FLAGS_DEC, NULL, NULL, NULL,
                           NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

    /* Register the pipeline property */
    if (H5P__register_real(pclass, H5O_CRT_PIPELINE_NAME, H5O_CRT_PIPELINE_SIZE, &H5O_def_pline_g, NULL,
                           H5O_CRT_PIPELINE_SET, H5O_CRT_PIPELINE_GET, H5O_CRT_PIPELINE_ENC,
                           H5O_CRT_PIPELINE_DEC, H5O_CRT_PIPELINE_DEL, H5O_CRT_PIPELINE_COPY,
                           H5O_CRT_PIPELINE_CMP, H5O_CRT_PIPELINE_CLOSE) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__ocrt_reg_prop() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_attr_phase_change
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_attr_phase_change(hid_t plist_id, unsigned max_compact, unsigned min_dense)
{
    H5P_genplist_t *plist;               /* Property list pointer */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Range check values */
    if (max_compact < min_dense)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "max compact value must be >= min dense value");
    if (max_compact > 65535)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "max compact value must be < 65536");
    if (min_dense > 65535)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "min dense value must be < 65536");

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, false)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Set property values */
    if (H5P_set(plist, H5O_CRT_ATTR_MAX_COMPACT_NAME, &max_compact) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set max. # of compact attributes in property list");
    if (H5P_set(plist, H5O_CRT_ATTR_MIN_DENSE_NAME, &min_dense) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set min. # of dense attributes in property list");

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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_attr_phase_change(hid_t plist_id, unsigned *max_compact /*out*/, unsigned *min_dense /*out*/)
{
    H5P_genplist_t *plist;               /* Property list pointer */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, true)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get values */
    if (max_compact) {
        if (H5P_get(plist, H5O_CRT_ATTR_MAX_COMPACT_NAME, max_compact) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get max. # of compact attributes");
    } /* end if */
    if (min_dense) {
        if (H5P_get(plist, H5O_CRT_ATTR_MIN_DENSE_NAME, min_dense) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get min. # of dense attributes");
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_attr_creation_order(hid_t plist_id, unsigned crt_order_flags)
{
    H5P_genplist_t *plist;               /* Property list pointer */
    uint8_t         ohdr_flags;          /* Object header flags */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check for bad combination of flags */
    if (!(crt_order_flags & H5P_CRT_ORDER_TRACKED) && (crt_order_flags & H5P_CRT_ORDER_INDEXED))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "tracking creation order is required for index");

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, false)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get object header flags */
    if (H5P_get(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object header flags");

    /* Mask off previous attribute creation order flag settings */
    ohdr_flags &= (uint8_t) ~(H5O_HDR_ATTR_CRT_ORDER_TRACKED | H5O_HDR_ATTR_CRT_ORDER_INDEXED);

    /* Update with new attribute creation order flags */
    ohdr_flags = (uint8_t)(ohdr_flags |
                           ((crt_order_flags & H5P_CRT_ORDER_TRACKED) ? H5O_HDR_ATTR_CRT_ORDER_TRACKED : 0));
    ohdr_flags = (uint8_t)(ohdr_flags |
                           ((crt_order_flags & H5P_CRT_ORDER_INDEXED) ? H5O_HDR_ATTR_CRT_ORDER_INDEXED : 0));

    /* Set object header flags */
    if (H5P_set(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set object header flags");

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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_attr_creation_order(hid_t plist_id, unsigned *crt_order_flags /*out*/)
{
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)

    /* Get values */
    if (crt_order_flags) {
        H5P_genplist_t *plist;      /* Property list pointer */
        uint8_t         ohdr_flags; /* Object header flags */

        /* Reset the value to return */
        *crt_order_flags = 0;

        /* Get the plist structure */
        if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, true)))
            HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

        /* Get object header flags */
        if (H5P_get(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object header flags");

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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_obj_track_times(hid_t plist_id, bool track_times)
{
    H5P_genplist_t *plist;               /* Property list pointer */
    uint8_t         ohdr_flags;          /* Object header flags */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, false)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get object header flags */
    if (H5P_get(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object header flags");

    /* Mask off previous time tracking flag settings */
    ohdr_flags &= (uint8_t)~H5O_HDR_STORE_TIMES;

    /* Update with new time tracking flag */
    ohdr_flags = (uint8_t)(ohdr_flags | (track_times ? H5O_HDR_STORE_TIMES : 0));

    /* Set object header flags */
    if (H5P_set(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set object header flags");

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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_obj_track_times(hid_t plist_id, bool *track_times /*out*/)
{
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)

    /* Get values */
    if (track_times) {
        H5P_genplist_t *plist;      /* Property list pointer */
        uint8_t         ohdr_flags; /* Object header flags */

        /* Get the plist structure */
        if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, true)))
            HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

        /* Get object header flags */
        if (H5P_get(plist, H5O_CRT_OHDR_FLAGS_NAME, &ohdr_flags) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object header flags");

        /* Set track times flag to return */
        *track_times = (bool)((ohdr_flags & H5O_HDR_STORE_TIMES) ? true : false);
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
 *        auxiliary data for the filter.  The integer values will be
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
 *-------------------------------------------------------------------------
 */
herr_t
H5P_modify_filter(H5P_genplist_t *plist, H5Z_filter_t filter, unsigned flags, size_t cd_nelmts,
                  bool keep_config, const unsigned cd_values[/*cd_nelmts*/])
{
    H5O_pline_t pline;
    herr_t      ret_value = SUCCEED; /* return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Get the pipeline property to modify */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Modify the filter parameters of the I/O pipeline */
    if (H5Z_modify(&pline, filter, flags, cd_nelmts, keep_config, cd_values) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add filter to pipeline");

    /* Put the I/O pipeline information back into the property list */
    if (H5P_poke(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");

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
 *              auxiliary data for the filter.  The integer values will be
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pmodify_filter(hid_t plist_id, H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts,
                 const unsigned int cd_values[/*cd_nelmts*/])
{
    H5P_genplist_t *plist;               /* Property list */
    herr_t          ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if (filter < 0 || filter > H5Z_FILTER_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid filter identifier");
    if (flags & ~((unsigned)H5Z_FLAG_DEFMASK))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags");
    if (cd_nelmts > 0 && !cd_values)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no client data values supplied");

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, false)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Modify the filter parameters of the I/O pipeline.  The caller is
     * replacing cd_values directly, so any stored configuration string no
     * longer describes this entry: keep_config = false. */
    if (H5P_modify_filter(plist, filter, flags, cd_nelmts, false, cd_values) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't modify filter");

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
 *        auxiliary data for the filter.  The integer values will be
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_filter(hid_t plist_id, H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts,
              const unsigned int cd_values[/*cd_nelmts*/])
{
    H5P_genplist_t *plist;               /* Property list */
    herr_t          ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if (filter < 0 || filter > H5Z_FILTER_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid filter identifier");
    if (flags & ~((unsigned)H5Z_FLAG_DEFMASK))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags");
    if (cd_nelmts > 0 && !cd_values)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no client data values supplied");

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, false)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Call the private function */
    if (H5P__set_filter(plist, filter, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "failed to call private function");

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
 *        auxiliary data for the filter.  The integer values will be
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
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__set_filter(H5P_genplist_t *plist, H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts,
                const unsigned int cd_values[/*cd_nelmts*/])
{
    H5O_pline_t pline;               /* Filter pipeline */
    htri_t      filter_avail;        /* Filter availability */
    herr_t      ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check if filter is already available */
    if ((filter_avail = H5Z_filter_avail(filter)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't check filter availability");

    /* Get the pipeline property to append to */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Add the filter to the I/O pipeline */
    if (H5Z_append(&pline, filter, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add filter to pipeline");

    /* Put the I/O pipeline information back into the property list */
    if (H5P_poke(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");

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
 *-------------------------------------------------------------------------
 */
int
H5Pget_nfilters(hid_t plist_id)
{
    H5P_genplist_t *plist;     /* Property list */
    H5O_pline_t     pline;     /* Filter pipeline */
    int             ret_value; /* return value */

    FUNC_ENTER_API(FAIL)

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, true)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get the pipeline property to query */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

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
 *      field containing encode/decode flags from H5Zpublic.h.  The IDX
 *      should be a value between zero and N-1 as described for
 *      H5Pget_nfilters() and the function will return failure if the
 *      filter number is out of range.
 *
 * Return:    Success:    Filter identification number.
 *
 *        Failure:    H5Z_FILTER_ERROR (Negative)
 *
 *-------------------------------------------------------------------------
 */
H5Z_filter_t
H5Pget_filter2(hid_t plist_id, unsigned idx, unsigned int *flags /*out*/, size_t *cd_nelmts /*in,out*/,
               unsigned cd_values[] /*out*/, size_t namelen, char name[] /*out*/,
               unsigned *filter_config /*out*/)
{
    H5P_genplist_t          *plist;     /* Property list */
    H5O_pline_t              pline;     /* Filter pipeline */
    const H5Z_filter_info_t *filter;    /* Pointer to filter information */
    H5Z_filter_t             ret_value; /* return value */

    FUNC_ENTER_API(H5Z_FILTER_ERROR)

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
                        "probable uninitialized *cd_nelmts argument");
        if (cd_nelmts && *cd_nelmts > 0 && !cd_values)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "client data values not supplied");

        /*
         * If cd_nelmts is null but cd_values is non-null then just ignore
         * cd_values
         */
        if (!cd_nelmts)
            cd_values = NULL;
    } /* end if */

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, true)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, H5Z_FILTER_ERROR, "can't find object for ID");

    /* Get the pipeline property to query */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5Z_FILTER_ERROR, "can't get pipeline");

    /* Check index */
    if (idx >= pline.nused)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "filter number is invalid");

    /* Set pointer to particular filter to query */
    filter = &pline.filter[idx];

    /* Get filter information */
    if (H5P__get_filter(filter, flags, cd_nelmts, cd_values, namelen, name, filter_config) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5Z_FILTER_ERROR, "can't get filter info");

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
 *      field containing encode/decode flags from H5Zpublic.h.  The ID
 *      should be the filter ID to retrieve the parameters for.  If the
 *      filter is not set for the property list, an error will be returned.
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_get_filter_by_id(H5P_genplist_t *plist, H5Z_filter_t id, unsigned int *flags /*out*/,
                     size_t *cd_nelmts /*in,out*/, unsigned cd_values[] /*out*/, size_t namelen,
                     char name[] /*out*/, unsigned *filter_config)
{
    H5O_pline_t        pline;               /* Filter pipeline */
    H5Z_filter_info_t *filter;              /* Pointer to filter information */
    herr_t             ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Get pipeline info */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Get pointer to filter in pipeline */
    if (NULL == (filter = H5Z_filter_info(&pline, id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter ID is invalid");

    /* Get filter information */
    if (H5P__get_filter(filter, flags, cd_nelmts, cd_values, namelen, name, filter_config) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get filter info");

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
 *      field containing encode/decode flags from H5Zpublic.h.  The ID
 *      should be the filter ID to retrieve the parameters for.  If the
 *      filter is not set for the property list, an error will be returned.
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_filter_by_id2(hid_t plist_id, H5Z_filter_t id, unsigned int *flags /*out*/,
                     size_t *cd_nelmts /*in,out*/, unsigned cd_values[] /*out*/, size_t namelen,
                     char name[] /*out*/, unsigned *filter_config /*out*/)
{
    H5P_genplist_t *plist;               /* Property list */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if (id < 0 || id > H5Z_FILTER_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter ID value out of range");
    if (cd_nelmts || cd_values) {
        /*
         * It's likely that users forget to initialize this on input, so
         * we'll check that it has a reasonable value.  The actual number
         * is unimportant because the H5O layer will detect when a message
         * is too large.
         */
        if (cd_nelmts && *cd_nelmts > 256)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "probable uninitialized *cd_nelmts argument");
        if (cd_nelmts && *cd_nelmts > 0 && !cd_values)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "client data values not supplied");

        /*
         * If cd_nelmts is null but cd_values is non-null then just ignore
         * cd_values
         */
        if (!cd_nelmts)
            cd_values = NULL;
    } /* end if */

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, true)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get filter information */
    if (H5P_get_filter_by_id(plist, id, flags, cd_nelmts, cd_values, namelen, name, filter_config) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get filter info");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_filter_by_id2() */

/*-------------------------------------------------------------------------
 * Function:    H5Pall_filters_avail
 *
 * Purpose:    This is a query routine to verify that all the filters set
 *              in the dataset creation property list are available currently.
 *
 * Return:    Success:    true if all filters available, false if one or
 *                              more filters not currently available.
 *        Failure:    FAIL on error
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

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, true)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get the pipeline property to query */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Check if all filters are available */
    if ((ret_value = H5Z_all_filters_avail(&pline)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "can't check pipeline information");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pall_filters_avail() */

/*-------------------------------------------------------------------------
 * Function:    H5P_filter_in_pline
 *
 * Purpose:    Check whether the filter is in the pipeline of the object
 *              creation property list.
 *
 * Return:    true:        found
 *        false:        not found
 *              FAIL:         error
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
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Check if the file is in the pipeline */
    if ((ret_value = H5Z_filter_in_pline(&pline, id)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTCOMPARE, FAIL, "can't find filter");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_filter_in_pline() */

/*-------------------------------------------------------------------------
 * Function: H5Premove_filter
 *
 * Purpose: Deletes a filter from the dataset creation property list;
 *  deletes all filters if FILTER is H5Z_FILTER_ALL
 *
 * Return: Non-negative on success/Negative on failure
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

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, false)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get the pipeline property to modify */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Check if there are any filters */
    if (pline.filter) {
        /* Delete filter */
        if (H5Z_delete(&pline, filter) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't delete filter");

        /* Put the I/O pipeline information back into the property list */
        if (H5P_poke(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_deflate(hid_t plist_id, unsigned level)
{
    H5P_genplist_t *plist;               /* Property list */
    H5O_pline_t     pline;               /* Filter pipeline */
    herr_t          ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)

    /* Check arguments */
    if (level > 9)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid deflate level");

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, false)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get the pipeline property to append to */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Add the filter */
    if (H5Z_append(&pline, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, (size_t)1, &level) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add deflate filter to pipeline");

    /* Put the I/O pipeline information back into the property list */
    if (H5P_poke(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");

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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fletcher32(hid_t plist_id)
{
    H5P_genplist_t *plist;               /* Property list */
    H5O_pline_t     pline;               /* Filter pipeline */
    herr_t          ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, false)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get the pipeline property to append to */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Add the Fletcher32 checksum as a filter */
    if (H5Z_append(&pline, H5Z_FILTER_FLETCHER32, H5Z_FLAG_MANDATORY, (size_t)0, NULL) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add fletcher32 filter to pipeline");

    /* Put the I/O pipeline information back into the property list */
    if (H5P_poke(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");

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
 *-------------------------------------------------------------------------
 */
herr_t
H5P__get_filter(const H5Z_filter_info_t *filter, unsigned int *flags /*out*/, size_t *cd_nelmts /*in,out*/,
                unsigned cd_values[] /*out*/, size_t namelen, char name[] /*out*/,
                unsigned *filter_config /*out*/)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Check arguments */
    assert(filter);

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
            H5Z_entry_t *entry_p = NULL;

            H5Z_find_entry(true, filter->id, &entry_p);
            if (entry_p)
                s = entry_p->base.name;
        } /* end if */

        /* Check for actual name */
        if (s) {
            strncpy(name, s, namelen);
            name[namelen - 1] = '\0';
        } /* end if */
        else {
            /* No name and no stored name: fall back to decimal filter ID */
            snprintf(name, namelen, "%d", (int)filter->id);
        } /* end else */
    }     /* end if */

    /* Filter configuration (assume filter ID has already been checked) */
    if (filter_config)
        H5Z_get_filter_info(filter->id, filter_config);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__get_filter() */

/*-------------------------------------------------------------------------
 * Function:    H5P__ocrt_pipeline_set
 *
 * Purpose:     Copies an I/O pipeline property when it's set for a property list
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__ocrt_pipeline_set(hid_t H5_ATTR_UNUSED prop_id, const char H5_ATTR_UNUSED *name,
                       size_t H5_ATTR_UNUSED size, void *value)
{
    H5O_pline_t *pline = (H5O_pline_t *)value; /* Create local aliases for values */
    H5O_pline_t  new_pline;
    herr_t       ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(value);

    /* Make copy of I/O pipeline */
    if (NULL == H5O_msg_copy(H5O_PLINE_ID, pline, &new_pline))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy I/O pipeline");

    /* Copy new I/O pipeline message over old one */
    *pline = new_pline;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__ocrt_pipeline_set() */

/*-------------------------------------------------------------------------
 * Function:    H5P__ocrt_pipeline_get
 *
 * Purpose:     Copies a layout property when it's retrieved from a property list
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__ocrt_pipeline_get(hid_t H5_ATTR_UNUSED prop_id, const char H5_ATTR_UNUSED *name,
                       size_t H5_ATTR_UNUSED size, void *value)
{
    H5O_pline_t *pline = (H5O_pline_t *)value; /* Create local aliases for values */
    H5O_pline_t  new_pline;
    herr_t       ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(value);

    /* Make copy of I/O pipeline */
    if (NULL == H5O_msg_copy(H5O_PLINE_ID, pline, &new_pline))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy I/O pipeline");

    /* Copy new I/O pipeline message over old one */
    *pline = new_pline;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__ocrt_pipeline_get() */

/*-------------------------------------------------------------------------
 * Function:       H5P__ocrt_pipeline_enc
 *
 * Purpose:        Callback routine which is called whenever the pipeline
 *                 property in the dataset access property list is
 *                 decoded.
 *
 * Return:       Success:    Non-negative
 *           Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__ocrt_pipeline_enc(const void *value, void **_pp, size_t *size)
{
    const H5O_pline_t *pline = (const H5O_pline_t *)value;
    uint8_t          **pp    = (uint8_t **)_pp;
    size_t             u; /* Local index variable */

    FUNC_ENTER_PACKAGE_NOERR

    assert(pline);
    assert(size);
    HDcompile_assert(sizeof(size_t) <= sizeof(uint64_t));

    if (NULL != *pp) {
        unsigned enc_size;
        uint64_t enc_value;

        /* Encode size of unsigned */
        *(*pp)++ = (uint8_t)sizeof(unsigned);

        /* encode nused value */
        enc_value = (uint64_t)pline->nused;
        enc_size  = H5VM_limit_enc_size(enc_value);
        assert(enc_size < 256);
        *(*pp)++ = (uint8_t)enc_size;
        UINT64ENCODE_VAR(*pp, enc_value, enc_size);

        /* encode each pipeline */
        for (u = 0; u < pline->nused; u++) {
            unsigned v; /* Local index variable */

            /* encode filter ID */
            INT32ENCODE(*pp, pline->filter[u].id);

            /* encode filter flags */
            H5_ENCODE_UNSIGNED(*pp, pline->filter[u].flags);

            /* encode filter name if it exists */
            if (NULL != pline->filter[u].name) {
                /* encode true indicating that it exits */
                *(*pp)++ = (uint8_t) true;

                /* encode filter name */
                H5MM_memcpy(*pp, (uint8_t *)(pline->filter[u].name), H5Z_COMMON_NAME_LEN);
                *pp += H5Z_COMMON_NAME_LEN;
            } /* end if */
            else
                /* encode false indicating that it does not exist */
                *(*pp)++ = (uint8_t) false;

            /* encode cd_nelmts */
            enc_value = (uint64_t)pline->filter[u].cd_nelmts;
            enc_size  = H5VM_limit_enc_size(enc_value);
            assert(enc_size < 256);
            *(*pp)++ = (uint8_t)enc_size;
            UINT64ENCODE_VAR(*pp, enc_value, enc_size);

            /* encode all values */
            for (v = 0; v < pline->filter[u].cd_nelmts; v++)
                H5_ENCODE_UNSIGNED(*pp, pline->filter[u].cd_values[v]);

            /* encode the verbatim config string inline (a self-contained
             * encoded DCPL carries the string, not a file locator) */
            if (NULL != pline->filter[u].config) {
                uint64_t config_len = (uint64_t)strlen(pline->filter[u].config);

                *(*pp)++ = (uint8_t) true;
                enc_size = H5VM_limit_enc_size(config_len);
                *(*pp)++ = (uint8_t)enc_size;
                UINT64ENCODE_VAR(*pp, config_len, enc_size);
                H5MM_memcpy(*pp, pline->filter[u].config, (size_t)config_len);
                *pp += config_len;
            } /* end if */
            else
                *(*pp)++ = (uint8_t) false;
        } /* end for */
    }     /* end if */

    /* calculate size required for encoding */
    *size += 1;
    *size += (1 + H5VM_limit_enc_size((uint64_t)pline->nused));
    for (u = 0; u < pline->nused; u++) {
        *size += (sizeof(int32_t) + sizeof(unsigned) + 1);
        if (NULL != pline->filter[u].name)
            *size += H5Z_COMMON_NAME_LEN;
        *size += (1 + H5VM_limit_enc_size((uint64_t)pline->filter[u].cd_nelmts));
        *size += pline->filter[u].cd_nelmts * sizeof(unsigned);
        *size += 1; /* has_config flag */
        if (NULL != pline->filter[u].config) {
            uint64_t config_len = (uint64_t)strlen(pline->filter[u].config);
            *size += (1 + H5VM_limit_enc_size(config_len) + (size_t)config_len);
        }
    } /* end for */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__ocrt_pipeline_enc() */

/*-------------------------------------------------------------------------
 * Function:       H5P__ocrt_pipeline_dec
 *
 * Purpose:        Callback routine which is called whenever the pipeline
 *                 property in the dataset access property list is
 *                 decoded.
 *
 * Return:       Success:    Non-negative
 *           Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__ocrt_pipeline_dec(const void **_pp, void *_value)
{
    H5O_pline_t    *pline = (H5O_pline_t *)_value; /* Property to set */
    const uint8_t **pp    = (const uint8_t **)_pp;
    size_t          nused;               /* Number of filters used for pipeline */
    unsigned        enc_size;            /* Size of encoded value (in bytes) */
    uint64_t        enc_value;           /* Value to encode */
    size_t          u;                   /* Local index variable */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    HDcompile_assert(sizeof(size_t) <= sizeof(uint64_t));

    /* Decode the size of size_t */
    enc_size = *(*pp)++;
    if (enc_size != sizeof(unsigned))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "unsigned value can't be decoded");

    /* decode nused */
    enc_size = *(*pp)++;
    assert(enc_size < 256);
    UINT64DECODE_VAR(*pp, enc_value, enc_size);
    nused = (size_t)enc_value;

    /* Set property default value */
    memset(pline, 0, sizeof(H5O_pline_t));
    *pline = H5O_def_pline_g;

    for (u = 0; u < nused; u++) {
        H5Z_filter_info_t filter;        /* Filter info, for pipeline */
        uint8_t           has_name;      /* Flag to indicate whether filter has a name */
        uint8_t           has_config;    /* Flag to indicate whether filter has a config string */
        char             *config = NULL; /* Decoded verbatim config string */
        unsigned          v;             /* Local index variable */

        /* decode filter id */
        INT32DECODE(*pp, filter.id);

        /* decode filter flags */
        H5_DECODE_UNSIGNED(*pp, filter.flags);

        /* decode value indicating if the name is encoded.  The name itself
         * is not retained here -- H5Z_append() always sets the appended
         * entry's name to NULL and it is picked up later (e.g. from the
         * filter's canonical name) -- so just skip over the encoded field
         * rather than allocating a copy that would never be used or freed. */
        has_name = *(*pp)++;
        if (has_name)
            *pp += H5Z_COMMON_NAME_LEN;
        filter.name = NULL;

        /* decode num elements */
        enc_size = *(*pp)++;
        assert(enc_size < 256);
        UINT64DECODE_VAR(*pp, enc_value, enc_size);
        filter.cd_nelmts = (size_t)enc_value;

        if (filter.cd_nelmts) {
            if (NULL == (filter.cd_values = (unsigned *)H5MM_malloc(sizeof(unsigned) * filter.cd_nelmts)))
                HGOTO_ERROR(H5E_PLIST, H5E_CANTALLOC, FAIL, "memory allocation failed for cd_values");
        } /* end if */
        else
            filter.cd_values = NULL;

        /* decode values */
        for (v = 0; v < filter.cd_nelmts; v++)
            H5_DECODE_UNSIGNED(*pp, filter.cd_values[v]);

        /* decode the verbatim config string, if present */
        has_config = *(*pp)++;
        if (has_config) {
            uint64_t config_len;

            enc_size = *(*pp)++;
            assert(enc_size < 256);
            UINT64DECODE_VAR(*pp, config_len, enc_size);
            if (NULL == (config = (char *)H5MM_malloc((size_t)config_len + 1))) {
                filter.cd_values = (unsigned *)H5MM_xfree(filter.cd_values);
                HGOTO_ERROR(H5E_PLIST, H5E_CANTALLOC, FAIL, "memory allocation failed for config string");
            }
            H5MM_memcpy(config, *pp, (size_t)config_len);
            config[config_len] = '\0';
            *pp += config_len;
        }

        /* Add the filter to the I/O pipeline */
        if (H5Z_append(pline, filter.id, filter.flags, filter.cd_nelmts, filter.cd_values) < 0) {
            H5MM_xfree(config);
            filter.cd_values = (unsigned *)H5MM_xfree(filter.cd_values);
            HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add filter to pipeline");
        }

        /* Attach the decoded config string to the just-appended entry */
        pline->filter[pline->nused - 1].config = config;

        /* Free cd_values, if it was allocated */
        filter.cd_values = (unsigned *)H5MM_xfree(filter.cd_values);
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5P__ocrt_pipeline_dec() */

/*-------------------------------------------------------------------------
 * Function:    H5P__ocrt_pipeline_del
 *
 * Purpose:     Frees memory used to store the I/O pipeline property
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__ocrt_pipeline_del(hid_t H5_ATTR_UNUSED prop_id, const char H5_ATTR_UNUSED *name,
                       size_t H5_ATTR_UNUSED size, void *value)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(value);

    /* Reset the old I/O pipeline */
    if (H5O_msg_reset(H5O_PLINE_ID, value) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTRESET, FAIL, "can't release I/O pipeline message");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__ocrt_pipeline_del() */

/*--------------------------------------------------------------------------
 * Function:    H5P__ocrt_pipeline_copy
 *
 * Purpose:     Copy the I/O pipeline property
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 *--------------------------------------------------------------------------
 */
static herr_t
H5P__ocrt_pipeline_copy(const char H5_ATTR_UNUSED *name, size_t H5_ATTR_UNUSED size, void *value)
{
    H5O_pline_t *pline = (H5O_pline_t *)value; /* Create local aliases for values */
    H5O_pline_t  new_pline;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(pline);

    /* Make copy of I/O pipeline */
    if (NULL == H5O_msg_copy(H5O_PLINE_ID, pline, &new_pline))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy I/O pipeline");

    /* Copy new I/O pipeline message over old one */
    *pline = new_pline;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__ocrt_pipeline_copy() */

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
 *-------------------------------------------------------------------------
 */
static int
H5P__ocrt_pipeline_cmp(const void *_pline1, const void *_pline2, size_t H5_ATTR_UNUSED size)
{
    const H5O_pline_t *pline1 = (const H5O_pline_t *)_pline1, /* Create local aliases for values */
        *pline2               = (const H5O_pline_t *)_pline2;
    int    cmp_value;     /* Value from comparison */
    herr_t ret_value = 0; /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    /* Sanity check */
    assert(pline1);
    assert(pline2);
    assert(size == sizeof(H5O_pline_t));

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
                if ((cmp_value = strcmp(pline1->filter[u].name, pline2->filter[u].name)) != 0)
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

/*-------------------------------------------------------------------------
 * Function:    H5P__ocrt_pipeline_close
 *
 * Purpose:     Frees memory used to store the I/O pipeline property
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__ocrt_pipeline_close(const char H5_ATTR_UNUSED *name, size_t H5_ATTR_UNUSED size, void *value)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(value);

    /* Reset the old I/O pipeline */
    if (H5O_msg_reset(H5O_PLINE_ID, value) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTRESET, FAIL, "can't release I/O pipeline message");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__ocrt_pipeline_close() */

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
 *-------------------------------------------------------------------------
 */
H5Z_filter_t
H5Pget_filter1(hid_t plist_id, unsigned idx, unsigned int *flags /*out*/, size_t *cd_nelmts /*in,out*/,
               unsigned cd_values[] /*out*/, size_t namelen, char name[] /*out*/)
{
    H5O_pline_t              pline;     /* Filter pipeline */
    const H5Z_filter_info_t *filter;    /* Pointer to filter information */
    H5P_genplist_t          *plist;     /* Property list pointer */
    H5Z_filter_t             ret_value; /* return value */

    FUNC_ENTER_API(H5Z_FILTER_ERROR)

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
                        "probable uninitialized *cd_nelmts argument");
        if (cd_nelmts && *cd_nelmts > 0 && !cd_values)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "client data values not supplied");

        /*
         * If cd_nelmts is null but cd_values is non-null then just ignore
         * cd_values
         */
        if (!cd_nelmts)
            cd_values = NULL;
    } /* end if */

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, true)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, H5Z_FILTER_ERROR, "can't find object for ID");

    /* Get pipeline info */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5Z_FILTER_ERROR, "can't get pipeline");

    /* Check more args */
    if (idx >= pline.nused)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "filter number is invalid");

    /* Set pointer to particular filter to query */
    filter = &pline.filter[idx];

    /* Get filter information */
    if (H5P__get_filter(filter, flags, cd_nelmts, cd_values, namelen, name, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5Z_FILTER_ERROR, "can't get filter info");

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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_filter_by_id1(hid_t plist_id, H5Z_filter_t id, unsigned int *flags /*out*/,
                     size_t *cd_nelmts /*in,out*/, unsigned cd_values[] /*out*/, size_t namelen,
                     char name[] /*out*/)
{
    H5P_genplist_t *plist;               /* Property list pointer */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if (id < 0 || id > H5Z_FILTER_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter ID value out of range");
    if (cd_nelmts || cd_values) {
        /*
         * It's likely that users forget to initialize this on input, so
         * we'll check that it has a reasonable value.  The actual number
         * is unimportant because the H5O layer will detect when a message
         * is too large.
         */
        if (cd_nelmts && *cd_nelmts > 256)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "probable uninitialized *cd_nelmts argument");
        if (cd_nelmts && *cd_nelmts > 0 && !cd_values)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "client data values not supplied");

        /*
         * If cd_nelmts is null but cd_values is non-null then just ignore
         * cd_values
         */
        if (!cd_nelmts)
            cd_values = NULL;
    } /* end if */

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, true)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get filter info */
    if (H5P_get_filter_by_id(plist, id, flags, cd_nelmts, cd_values, namelen, name, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get filter info");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_filter_by_id1() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

/*-------------------------------------------------------------------------
 * Function:    H5Pappend_filter
 *
 * Purpose:     Configures the filter identified by FILTER and appends it to
 *              the pipeline on PLIST_ID.  PARAMS selects the configuration
 *              mode via its type field:
 *
 *              H5Z_PARAMS_CDVALUES - pass raw cd_values directly (same as
 *                                    H5Pset_filter).
 *              H5Z_PARAMS_STRING   - invoke the filter's set_config callback
 *                                    with the key=value string.
 *              NULL                - equivalent to CDVALUES with cd_nelmts=0.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Since:       3.0.0
 *-------------------------------------------------------------------------
 */
herr_t
H5Pappend_filter(hid_t plist_id, H5Z_filter_t filter, unsigned int flags, const H5Z_params_t *params)
{
    H5P_genplist_t *plist;
    H5O_pline_t     pline;
    H5Z_entry_t    *entry               = NULL; /* filter registry entry; set in STRING path */
    const unsigned *cd_values           = NULL;
    unsigned       *allocated_cd_values = NULL; /* owns heap mem for string path */
    char           *fi_name_heap        = NULL; /* non-NULL if fi->name was H5MM_strdup'd here */
    const char     *retain_config       = NULL; /* canonical string to persist (STRING path only) */
    char           *canon_config        = NULL; /* owns the buffer retain_config points into */
    char           *fi_config_heap      = NULL; /* non-NULL if fi->config was H5MM_strdup'd here */
    size_t          cd_nelmts           = 0;
    herr_t          ret_value           = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Validate filter ID and flags */
    if (filter < 0 || filter > H5Z_FILTER_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid filter identifier");
    if (flags & ~((unsigned)H5Z_FLAG_DEFMASK))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags");

    if (params == NULL || params->type == H5Z_PARAMS_CDVALUES) {
        /* Raw cd_values path - behaves identically to H5Pset_filter */
        if (params) {
            cd_nelmts = params->u.raw.cd_nelmts;
            cd_values = params->u.raw.cd_values;
            if (cd_nelmts > 0 && cd_values == NULL)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cd_values is NULL but cd_nelmts > 0");
        }
    }
    else if (params->type == H5Z_PARAMS_STRING) {
        /* String path - invoke set_config to translate into cd_values.
         *
         * NULL or "" means
         * "no parameters" - the filter plugin is still located so we can
         * inspect its class record.
         *   - If the filter has no set_config callback, the filter is
         *     appended with cd_nelmts = 0 and no callback runs.
         *   - If the filter has set_config, the callback is invoked with
         *     params = NULL.  Parameterless or fully-defaulted filters can
         *     return success; filters that strictly require parameters
         *     (e.g., Scale-Offset) can return H5E_BADVALUE so the failure
         *     surfaces here at H5Pappend_filter time, not later in
         *     H5Dcreate / set_local / I/O.
         */
        const char *param_str    = params->u.str;
        bool        empty_input  = (!param_str || *param_str == '\0');
        size_t      cd_nelmts2   = 0;
        size_t      alloc_nelmts = 0;

        if (!empty_input && strlen(param_str) > H5Z_CONFIG_STRING_MAX)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params string exceeds H5Z_CONFIG_STRING_MAX");

        /* Trigger dynamic plugin load if filter is not already registered */
        {
            htri_t filter_avail;
            if ((filter_avail = H5Z_filter_avail(filter)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't check filter availability");
            if (!filter_avail)
                HGOTO_ERROR(H5E_PLINE, H5E_NOFILTER, FAIL, "filter not found; register or load it first");
        }

        /* Get the internal entry to access v3 callbacks */
        if (H5Z_find_entry(false, filter, &entry) < 0 || entry == NULL)
            HGOTO_ERROR(H5E_PLINE, H5E_NOTFOUND, FAIL, "filter entry not found after availability check");

        if (!entry->set_config) {
            /* No set_config: empty input is accepted as "no parameters";
             * a non-empty parameter string has nowhere to go and is an error. */
            if (empty_input) {
                cd_nelmts = 0;
                cd_values = NULL;
                goto append_to_pipeline;
            }
            HGOTO_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL,
                        "filter does not support string configuration (no set_config callback)");
        }

        /* Persist the caller's parameter string so it can be recovered
         * losslessly (pipeline v3) without loading the plugin.  The string is
         * canonicalised first -- outer braces stripped, hex-float literals
         * rewritten to %.16e decimal -- so the persisted bytes are a valid
         * TOML v1.0.0 document and can be parsed by readers that are not the
         * HDF5 library.  Both normalisations preserve the value exactly.
         * An empty input stores nothing. */
        if (!empty_input) {
            if (NULL == (canon_config = H5Z_canonicalize_params(param_str)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't canonicalize filter parameter string");
            retain_config = canon_config;
        }

        /* set_config is present: invoke it.  Normalise an empty input to
         * params = NULL so callbacks only need to handle one form. */
        {
            const char *cfg_str = empty_input ? NULL : param_str;

            /* Both passes supply stack addresses; the H5Z_set_config_func_t
             * contract (documented on the typedef) guarantees cd_nelmts != NULL. */

            /* Pass 1: determine cd_nelmts (size-query; cd_values is NULL) */
            if (entry->set_config(cfg_str, &flags, &cd_nelmts, NULL, 0) < 0)
                HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "set_config size-query call failed");

            if (cd_nelmts > H5Z_MAX_CD_NELMTS)
                HGOTO_ERROR(H5E_PLINE, H5E_BADVALUE, FAIL,
                            "cd_nelmts from set_config exceeds H5Z_MAX_CD_NELMTS (%u)", H5Z_MAX_CD_NELMTS);

            /* Allocate cd_values (at least 1 to avoid zero-length alloc) */
            alloc_nelmts = cd_nelmts ? cd_nelmts : 1;
            if (NULL == (allocated_cd_values = (unsigned *)H5MM_malloc(alloc_nelmts * sizeof(unsigned))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for cd_values");
            cd_values = allocated_cd_values;

            cd_nelmts2 = cd_nelmts;

            /* Pass 2: populate cd_values */
            if (entry->set_config(cfg_str, &flags, &cd_nelmts2, allocated_cd_values, alloc_nelmts) < 0)
                HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "set_config populate call failed");

            if (cd_nelmts2 != cd_nelmts)
                HGOTO_ERROR(H5E_PLINE, H5E_BADVALUE, FAIL,
                            "set_config returned different cd_nelmts on second call (contract violation)");

            /* Re-validate flags: the callback may have modified them */
            if (flags & ~((unsigned)H5Z_FLAG_DEFMASK))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "set_config callback returned invalid flags");
        }
    }
    else {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unrecognised H5Z_params_t type field");
    }

append_to_pipeline:
    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, false)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get the pipeline property */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Append the filter */
    if (H5Z_append(&pline, filter, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add filter to pipeline");

    /* Persist the filter's canonical name into the pipeline record so that
     * H5Pget_filter2 and h5dump can display it even when the plugin is not
     * loaded at read time.  For the STRING path `entry` is already set; for
     * the CDVALUES path perform a single speculative lookup here. */
    {
        H5Z_filter_info_t *fi = &pline.filter[pline.nused - 1];

        if (entry == NULL)
            (void)H5Z_find_entry(true, filter, &entry);

        if (entry && entry->base.name) {
            size_t nlen = strlen(entry->base.name) + 1;
            if (nlen <= H5Z_COMMON_NAME_LEN) {
                memcpy(fi->_name, entry->base.name, nlen);
                fi->name = fi->_name;
            }
            else {
                /* Name is longer than the internal buffer.  strdup it and
                 * track the pointer so we can free it if H5P_poke fails
                 * below (ownership transfers to the plist on success). */
                fi->name = (char *)H5MM_strdup(entry->base.name);
                if (fi->name == NULL) {
                    strncpy(fi->_name, entry->base.name, H5Z_COMMON_NAME_LEN - 1);
                    fi->_name[H5Z_COMMON_NAME_LEN - 1] = '\0';
                    fi->name                           = fi->_name;
                }
                else
                    fi_name_heap = fi->name;
            }
        }

        /* Persist the verbatim configuration string on the entry.  Track the
         * allocation so it can be freed if H5P_poke fails below (ownership
         * transfers to the plist on success). */
        if (retain_config) {
            if (NULL == (fi->config = (char *)H5MM_strdup(retain_config)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                            "memory allocation failed for filter config string");
            fi_config_heap = fi->config;
        }
    }

    /* Store updated pipeline back in property list */
    if (H5P_poke(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");

done:
    if (ret_value < 0) {
        H5MM_xfree(fi_name_heap);
        H5MM_xfree(fi_config_heap);
    }
    H5MM_xfree(canon_config);
    H5MM_xfree(allocated_cd_values);
    FUNC_LEAVE_API(ret_value)
} /* end H5Pappend_filter() */

/*-------------------------------------------------------------------------
 * Function:    H5Pmodify_filter_by_idx
 *
 * Purpose:     Replaces the configuration of the filter already at position
 *              FILTER_IDX in PLIST_ID's pipeline, leaving its position and
 *              filter ID unchanged.  PARAMS is interpreted exactly as
 *              H5Pappend_filter interprets it:
 *
 *              H5Z_PARAMS_STRING   - resolved through the filter's
 *                                    set_config callback; both the resulting
 *                                    cd_values and the canonical string
 *                                    replace the entry's current ones, so the
 *                                    entry keeps a stored configuration
 *                                    string.
 *              H5Z_PARAMS_CDVALUES - the cd_values array replaces the
 *                                    entry's current one and any stored
 *                                    string is cleared, matching
 *                                    H5Pmodify_filter.
 *              NULL                - equivalent to CDVALUES with
 *                                    cd_nelmts = 0.
 *
 *              This exists because H5Pmodify_filter takes only raw
 *              cd_values and therefore always clears the stored string,
 *              silently downgrading an entry from an exact, plugin-free
 *              configuration string to get_config reconstruction.  The only
 *              alternative was H5Premove_filter plus a fresh
 *              H5Pappend_filter, which moves the entry to the end of the
 *              pipeline and so changes filter order.
 *
 *              Addressing is by index rather than by filter ID because a
 *              pipeline may legally contain the same filter ID more than
 *              once, where an ID-addressed modify silently edits the first
 *              match.  The index is the same one H5Pget_filter2 and
 *              H5Pget_filter_params_by_idx use.
 *
 *              On failure the entry is left exactly as it was: the new
 *              cd_values and string are staged and swapped in only after
 *              set_config succeeds, so a rejected edit cannot leave an entry
 *              holding a string that disagrees with its cd_values.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Since:       3.0.0
 *-------------------------------------------------------------------------
 */
herr_t
H5Pmodify_filter_by_idx(hid_t plist_id, unsigned filter_idx, unsigned flags, const H5Z_params_t *params)
{
    H5P_genplist_t    *plist;
    H5O_pline_t        pline;
    H5Z_filter_info_t *fi;
    H5Z_entry_t       *entry               = NULL;
    const unsigned    *cd_values           = NULL;
    unsigned          *allocated_cd_values = NULL; /* owns heap mem for string path */
    char              *canon_config        = NULL; /* owns the buffer retain_config points into */
    const char        *retain_config       = NULL; /* canonical string to persist (STRING path only) */
    unsigned          *staged_cd_values    = NULL; /* staged replacement for fi->cd_values */
    char              *staged_config       = NULL; /* staged replacement for fi->config    */
    size_t             cd_nelmts           = 0;
    H5Z_filter_t       filter;
    herr_t             ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Validate flags */
    if (flags & ~((unsigned)H5Z_FLAG_DEFMASK))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags");

    /* Get the plist and its pipeline.  The entry's existing filter ID selects
     * the class whose set_config resolves a parameter string, so the index
     * must be validated before params can be interpreted. */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, false)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");
    if (filter_idx >= pline.nused)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter index out of range");

    fi     = &pline.filter[filter_idx];
    filter = fi->id;

    if (params == NULL || params->type == H5Z_PARAMS_CDVALUES) {
        /* Raw cd_values path - behaves identically to H5Pmodify_filter */
        if (params) {
            cd_nelmts = params->u.raw.cd_nelmts;
            cd_values = params->u.raw.cd_values;
            if (cd_nelmts > 0 && cd_values == NULL)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cd_values is NULL but cd_nelmts > 0");
        }
    }
    else if (params->type == H5Z_PARAMS_STRING) {
        /* String path - invoke set_config to translate into cd_values, using
         * the same two-pass protocol as H5Pappend_filter. */
        const char *param_str    = params->u.str;
        bool        empty_input  = (!param_str || *param_str == '\0');
        size_t      cd_nelmts2   = 0;
        size_t      alloc_nelmts = 0;

        if (!empty_input && strlen(param_str) > H5Z_CONFIG_STRING_MAX)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params string exceeds H5Z_CONFIG_STRING_MAX");

        /* Trigger dynamic plugin load if filter is not already registered */
        {
            htri_t filter_avail;
            if ((filter_avail = H5Z_filter_avail(filter)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't check filter availability");
            if (!filter_avail)
                HGOTO_ERROR(H5E_PLINE, H5E_NOFILTER, FAIL, "filter not found; register or load it first");
        }

        /* Get the internal entry to access v3 callbacks */
        if (H5Z_find_entry(false, filter, &entry) < 0 || entry == NULL)
            HGOTO_ERROR(H5E_PLINE, H5E_NOTFOUND, FAIL, "filter entry not found after availability check");

        if (!entry->set_config) {
            /* No set_config: empty input means "no parameters"; a non-empty
             * parameter string has nowhere to go and is an error. */
            if (!empty_input)
                HGOTO_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL,
                            "filter does not support string configuration (no set_config callback)");
            cd_nelmts = 0;
            cd_values = NULL;
        }
        else {
            const char *cfg_str = empty_input ? NULL : param_str;

            /* Canonicalise the string that will be persisted, for the same
             * reasons as in H5Pappend_filter (valid TOML v1.0.0 on disk). */
            if (!empty_input) {
                if (NULL == (canon_config = H5Z_canonicalize_params(param_str)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                                "can't canonicalize filter parameter string");
                retain_config = canon_config;
            }

            /* Pass 1: determine cd_nelmts (size-query; cd_values is NULL) */
            if (entry->set_config(cfg_str, &flags, &cd_nelmts, NULL, 0) < 0)
                HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "set_config size-query call failed");

            if (cd_nelmts > H5Z_MAX_CD_NELMTS)
                HGOTO_ERROR(H5E_PLINE, H5E_BADVALUE, FAIL,
                            "cd_nelmts from set_config exceeds H5Z_MAX_CD_NELMTS (%u)", H5Z_MAX_CD_NELMTS);

            /* Allocate cd_values (at least 1 to avoid zero-length alloc) */
            alloc_nelmts = cd_nelmts ? cd_nelmts : 1;
            if (NULL == (allocated_cd_values = (unsigned *)H5MM_malloc(alloc_nelmts * sizeof(unsigned))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for cd_values");
            cd_values = allocated_cd_values;

            cd_nelmts2 = cd_nelmts;

            /* Pass 2: populate cd_values */
            if (entry->set_config(cfg_str, &flags, &cd_nelmts2, allocated_cd_values, alloc_nelmts) < 0)
                HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "set_config populate call failed");

            if (cd_nelmts2 != cd_nelmts)
                HGOTO_ERROR(H5E_PLINE, H5E_BADVALUE, FAIL,
                            "set_config returned different cd_nelmts on second call (contract violation)");

            /* Re-validate flags: the callback may have modified them */
            if (flags & ~((unsigned)H5Z_FLAG_DEFMASK))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "set_config callback returned invalid flags");
        }
    }
    else {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unrecognised H5Z_params_t type field");
    }

    /* ---- Stage every allocation before disturbing the entry, so that an
     * allocation failure here leaves the entry exactly as it was. ---- */
    if (cd_nelmts > H5Z_COMMON_CD_VALUES) {
        if (NULL == (staged_cd_values = (unsigned *)H5MM_malloc(cd_nelmts * sizeof(unsigned))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for filter parameters");
        H5MM_memcpy(staged_cd_values, cd_values, cd_nelmts * sizeof(unsigned));
    }
    if (retain_config) {
        if (NULL == (staged_config = (char *)H5MM_strdup(retain_config)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for filter config string");
    }

    /* ---- Commit: nothing below can fail. ---- */

    /* Release what the entry currently owns */
    if (fi->cd_values != NULL && fi->cd_values != fi->_cd_values)
        H5MM_xfree(fi->cd_values);
    H5MM_xfree(fi->config);

    fi->flags     = flags;
    fi->cd_nelmts = cd_nelmts;

    if (cd_nelmts == 0)
        fi->cd_values = NULL;
    else if (staged_cd_values) {
        fi->cd_values    = staged_cd_values; /* ownership transfers to the entry */
        staged_cd_values = NULL;
    }
    else {
        /* Fits the entry's internal buffer */
        fi->cd_values = fi->_cd_values;
        H5MM_memcpy(fi->cd_values, cd_values, cd_nelmts * sizeof(unsigned));
    }

    /* NULL for the CDVALUES path, which clears the stored string */
    fi->config    = staged_config; /* ownership transfers to the entry */
    staged_config = NULL;

    /* Store the updated pipeline back in the property list */
    if (H5P_poke(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");

done:
    H5MM_xfree(staged_cd_values);
    H5MM_xfree(staged_config);
    H5MM_xfree(canon_config);
    H5MM_xfree(allocated_cd_values);
    FUNC_LEAVE_API(ret_value)
} /* end H5Pmodify_filter_by_idx() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_filter_params_by_idx
 *
 * Purpose:     Return the human-readable parameter string for the filter
 *              at pipeline index IDX.  If the filter has a get_config
 *              callback it is invoked; otherwise a fallback of the form
 *              "cd_values=v0:v1:..." is produced.
 *
 *              Call with params_buf NULL to obtain the required buffer
 *              size in *params_len, then allocate and call again.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Since:       3.0.0
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_filter_params_by_idx(hid_t plist_id, unsigned idx, char *params_buf, size_t params_buf_size,
                            size_t *params_len)
{
    H5P_genplist_t          *plist;
    H5O_pline_t              pline;
    const H5Z_filter_info_t *filter;
    H5Z_entry_t             *entry     = NULL;
    char                    *tmp_buf   = NULL;
    herr_t                   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Validate args */
    if (params_buf != NULL && params_buf_size == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params_buf_size must be > 0 when params_buf is non-NULL");
    if (params_buf == NULL && params_len == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "at least one of params_buf or params_len must be non-NULL");

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, true)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get the pipeline */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Validate index */
    if (idx >= pline.nused)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter index out of range");

    filter = &pline.filter[idx];

    /* Source 1 (highest fidelity): a verbatim configuration string stored on
     * the entry.  This is byte-for-byte lossless and needs no plugin, so it
     * short-circuits both the get_config and cd_values paths below. */
    if (filter->config) {
        size_t needed = strlen(filter->config);

        if (params_len)
            *params_len = needed;

        if (params_buf && params_buf_size > 0) {
            size_t copy_len = (needed < params_buf_size - 1) ? needed : params_buf_size - 1;
            H5MM_memcpy(params_buf, filter->config, copy_len);
            params_buf[copy_len] = '\0';
        }

        HGOTO_DONE(SUCCEED);
    }

    /* Trigger a plugin load if the filter isn't registered yet -- mirrors
     * H5Zget_filter_class_info()'s use of H5Z_filter_avail() to get get_config
     * available on the first query, not just subsequent ones. Availability
     * itself is ignored here; the fallback path below handles filters that
     * remain unavailable after this attempt. */
    (void)H5Z_filter_avail(filter->id);

    /* Attempt to find the entry (try plugin load if not yet registered) */
    (void)H5Z_find_entry(true, filter->id, &entry);

    if (entry && entry->get_config) {
        /* --- Two-pass get_config --- */
        size_t needed = 0;
        size_t out_size;

        /* Pass 1: size query */
        if (entry->get_config(filter->flags, filter->cd_nelmts, filter->cd_values, NULL, &needed) < 0)
            HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "get_config size-query call failed");

        if (params_len)
            *params_len = needed;

        if (params_buf && params_buf_size > 0) {
            /* Allocate a temp buffer of exact size and fill it */
            if (NULL == (tmp_buf = (char *)H5MM_malloc(needed + 1)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

            /* Pass the buffer's total capacity (including the NUL slot) so that
             * callbacks can use snprintf(buf, *buf_size, ...) safely, consistent
             * with standard C buffer-size conventions. */
            out_size = needed + 1;
            if (entry->get_config(filter->flags, filter->cd_nelmts, filter->cd_values, tmp_buf, &out_size) <
                0)
                HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "get_config populate call failed");

            /* out_size is now the chars-written count (excl. NUL) returned by the
             * callback; clamp before NUL-terminating in case a buggy callback
             * returns a count equal to the full capacity (needed+1). */
            if (out_size > needed)
                out_size = needed;
            tmp_buf[out_size] = '\0';

            /* Copy (truncating if params_buf is smaller) */
            {
                size_t copy_len = (out_size < params_buf_size - 1) ? out_size : params_buf_size - 1;
                H5MM_memcpy(params_buf, tmp_buf, copy_len);
                params_buf[copy_len] = '\0';
            }
        } /* end if params_buf */
    }     /* end if get_config */
    else {
        /* --- Fallback: "cd_values=v0:v1:..." --- */
        size_t true_len = 0; /* actual string length, uncapped */
        size_t i;
        int    n;

        /* Compute the true needed length without writing (C99 snprintf-to-NULL) */
        if (filter->cd_nelmts > 0) {
            n = snprintf(NULL, 0, "cd_values=%u", filter->cd_values[0]);
            if (n > 0)
                true_len = (size_t)n;
            for (i = 1; i < filter->cd_nelmts; i++) {
                n = snprintf(NULL, 0, ":%u", filter->cd_values[i]);
                if (n > 0)
                    true_len += (size_t)n;
            }
        }

        if (params_len)
            *params_len = true_len;

        if (params_buf && params_buf_size > 0) {
            size_t pos = 0;

            if (NULL == (tmp_buf = (char *)H5MM_malloc(true_len + 1)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for fallback buffer");

            if (filter->cd_nelmts > 0) {
                n   = snprintf(tmp_buf, true_len + 1, "cd_values=%u", filter->cd_values[0]);
                pos = (n > 0) ? (size_t)n : 0;
                for (i = 1; i < filter->cd_nelmts; i++) {
                    n = snprintf(tmp_buf + pos, true_len + 1 - pos, ":%u", filter->cd_values[i]);
                    if (n > 0)
                        pos += (size_t)n;
                }
            }
            tmp_buf[pos] = '\0';

            /* Copy to caller's buffer; truncate if smaller than needed */
            {
                size_t copy_len = (true_len < params_buf_size - 1) ? true_len : params_buf_size - 1;
                H5MM_memcpy(params_buf, tmp_buf, copy_len);
                params_buf[copy_len] = '\0';
            }
        }
    } /* end else (fallback) */

done:
    H5MM_xfree(tmp_buf);
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_filter_params_by_idx() */
