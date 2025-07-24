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
#include "H5Pmodule.h" /* This source code file is part of the H5P module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                        */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5MMprivate.h" /* Memory management                        */
#include "H5Opkg.h"      /* Object headers                           */
#include "H5Dprivate.h"  /* Dataset functions                        */
#include "H5Ppkg.h"      /* Property lists                           */
#include "H5VMprivate.h" /* Vector Functions                         */
#include "H5Zprivate.h"  /* Filter pipeline                          */

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
static herr_t H5P__set_filter1(H5P_genplist_t *plist, H5Z_filter_t filter, unsigned int flags,
                               size_t cd_nelmts, const unsigned int cd_values[/*cd_nelmts*/]);
static herr_t H5P__set_filter2(H5P_genplist_t *plist, H5_section_type_t sec_type, H5Z_filter_t filter,
                               unsigned int flags, size_t cd_nelmts,
                               const unsigned int cd_values[/*cd_nelmts*/]);

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
H5Pset_obj_track_times(hid_t plist_id, hbool_t track_times)
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
H5Pget_obj_track_times(hid_t plist_id, hbool_t *track_times /*out*/)
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
H5P_modify_filter(H5P_genplist_t *plist, H5Z_filter_t id, H5_section_type_t sec_type, unsigned flags,
                  size_t cd_nelmts, const unsigned cd_values[/*cd_nelmts*/])
{
    H5O_pline_t pline;
    herr_t      ret_value = SUCCEED; /* return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Get the pipeline property to modify */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    if (pline.version < H5O_PLINE_VERSION_3) {

        /* Get pointer to filter in pipeline */
        /* Modify the filter parameters of the I/O pipeline */
        if (H5Z_modify(pline.filter, pline.nused, id, flags, cd_nelmts, cd_values) < 0)
            HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to modify filter parameters in pipeline");
    }
    else {
        assert(pline.version == H5O_PLINE_VERSION_3);
        H5Z_stc_filter_sect_t *filt_sect = NULL;
        unsigned               i;

        for (i = 0, filt_sect = &pline.filt_sects[0]; i < pline.tot_filt_nsects; i++, filt_sect++) {
            if (filt_sect->seq_sect == (size_t)sec_type)
                break;
        }
        if (i >= pline.tot_filt_nsects)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "no filter defined for sec_type");

        /* Modify the filter parameters of the I/O pipeline */
        if (H5Z_modify(filt_sect->filter, filt_sect->nused, id, flags, cd_nelmts, cd_values) < 0)
            HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to modify filter parameters in pipeline");
    }

    /* Put the I/O pipeline information back into the property list */
    if (H5P_poke(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_modify_filter() */

/*-------------------------------------------------------------------------
 * Function:    H5Pmodify_filter1
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
H5Pmodify_filter1(hid_t plist_id, H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts,
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

    /* Modify the filter parameters of the I/O pipeline */
    if (H5P_modify_filter(plist, filter, H5_SECTION_UNKNOWN, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't modify filter");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pmodify_filter1() */

/*-------------------------------------------------------------------------
 * Function:    H5Pmodify_filter2
 *
 * Purpose:     Modifies the specified FILTER in the filter pipeline for a
 *              specified SECTION_NUMBER of the structured chunk.
 *
 *              The PLIST_ID argument is either a dataset or group creation
 *              creation property list.
 *
 *              The SECTION_NUMBER argument is the specified section of the
 *              structured chunk
 *
 *              The FILTER arguments specifies the filter identifier
 *
 *              The FLAGS argument specifies certain
 *              general properties of the filter and is documented below.
 *
 *              The BUF_SIZE is the size in bytes of BUF which points to
 *              a buffer with auxiliary data for the filter.  The values
 *              will be stored in the dataset object header as part of the
 *              filter information.
 *
 *              The FLAGS argument is a bit vector of the following fields:
 *
 *              H5Z_FLAG_OPTIONAL(0x0001)
 *              If this bit is set then the filter is optional.
 *              If the filter fails during an H5Dwrite() operation then the filter
 *              is just excluded from the pipeline for the chunk for which it
 *              failed; the filter will not participate in the pipeline
 *              during an H5Dread() of the chunk.
 *              If this bit is clear and the filter fails then the entire
 *              I/O operation fails.
 *              If this bit is set but encoding is disabled for a filter,
 *              attempting to write will generate an error.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pmodify_filter2(hid_t plist_id, H5_section_type_t sec_type, H5Z_filter_t filter, unsigned int flags,
                  size_t cd_nelmts, const unsigned int cd_values[/*cd_nelmts*/])
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

    /* Modify the filter parameters of the I/O pipeline */
    if (H5P_modify_filter(plist, filter, sec_type, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't modify filter");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pmodify_filter2() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_filter1
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
H5Pset_filter1(hid_t plist_id, H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts,
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
    if (H5P__set_filter1(plist, filter, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "failed to call private function");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_filter1() */

/*-------------------------------------------------------------------------
 * Function:    H5P__set_filter1
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
H5P__set_filter1(H5P_genplist_t *plist, H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts,
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
} /* end H5P__set_filter1() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_filter2
 *
 * Purpose:    Adds the specified FILTER and corresponding properties to the
 *        end of the data or link output filter pipeline
 *
 *        The PLIST_ID argument is a dataset creation or group
 *        creation property list.
 *
 *        The SEC_TYPE argument is the type identifier for a section
 *        in structured chunk to which the filter is applied.
 *
 *        The FILTER argument is the filter identifier for the filter
 *        to be added to the pipeline.
 *
 *        The FLAGS argument specifies certain
 *        general properties of the filter and is documented below.
 *
 *        The CD_VALUES is an array of CD_NELMTS integers which are
 *        auxiliary data for the filter.  The integer values will be
 *        stored in the dataset object header as part of the filter
 *        information.
 *
 *        The FLAGS argument is a bit vector of the following fields:
 *
 *        H5Z_FLAG_OPTIONAL(0x0001)
 *        If this bit is set then the filter is optional.  If the
 *        filter fails during an H5Dwrite() operation then the filter
 *        is just excluded from the pipeline for the chunk for which it
 *        failed; the filter will not participate in the pipeline
 *        during an H5Dread() of the chunk.
 *
 *        If this bit is clear and the filter fails then the entire I/O
 *        operation fails.
 *
 *        If this bit is set but encoding is disabled for a filter,
 *        attempting to write will generate an error.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_filter2(hid_t plist_id, H5_section_type_t sec_type, H5Z_filter_t filter, unsigned int flags,
               size_t cd_nelmts, const unsigned int cd_values[/*cd_nelmts*/])
{
    H5P_genplist_t *plist;               /* Property list */
    herr_t          ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if (sec_type < 0 || sec_type > H5_SECTION_NUM)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid section ");

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
    if (H5P__set_filter2(plist, sec_type, filter, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "failed to call private function");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_filter2() */

/*-------------------------------------------------------------------------
 * Function:    H5P__set_filter2
 *
 * Purpose:    Adds the specified FILTER and corresponding properties to the
 *        end of the data or link output filter pipeline.
 *
 *        The SECTION_NUMBER argument specified the section of the
 *        structured chunk to which the filter is applied.
 *
 *        The FILTER argument is the filter identifier for the filter
 *        to be added to the pipeline.
 *
 *        The FLAGS argument specifies certain
 *        general properties of the filter and is documented below.
 *
 *        The BUF argument points to a buffer of BUF_SIZE bytes which
 *        contains auxiliary data for the filter.  The values will be
 *        stored in the dataset object header as part of the filter
 *        information.
 *
 *        The FLAGS argument is a bit vector of the following fields:
 *
 *        H5Z_FLAG_OPTIONAL(0x0001)
 *        If this bit is set then the filter is optional.  If the
 *        filter fails during an H5Dwrite() operation then the filter
 *        is just excluded from the pipeline for the chunk for which it
 *        failed; the filter will not participate in the pipeline

 *        during an H5Dread() of the chunk.
 *
 *        If this bit is clear and the filter fails then the entire I/O
 *        operation fails.
 *
 *        If this bit is set but encoding is disabled for a filter,
 *        attempting to write will generate an error.
 *
 *        If the filter is not registered, this function tries to load
 *        it dynamically during run time.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__set_filter2(H5P_genplist_t *plist, H5_section_type_t sec_type, H5Z_filter_t filter_id,
                 unsigned int flags, size_t cd_nelmts, const unsigned int cd_values[/*cd_nelmts*/])
{
    H5O_pline_t            pline;        /* Filter pipeline */
    htri_t                 filter_avail; /* Filter availability */
    H5Z_stc_filter_sect_t *filt_sect = NULL;
    bool                   found     = false;
    unsigned               i         = 0;
    herr_t                 ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check if filter is already available */
    if ((filter_avail = H5Z_filter_avail(filter_id)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't check filter availability");

    /* Get the pipeline property to append to */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* For structured chunk, pline version should be 3 */
    if (pline.version != H5O_PLINE_VERSION_3)
        pline.version = H5O_PLINE_VERSION_3;

    for (i = 0; i < pline.tot_filt_nsects; i++)
        if (pline.filt_sects[i].seq_sect == (size_t)sec_type) {
            found = true;
            break;
        }

    filt_sect = &pline.filt_sects[i];

    /* Add the filter to the I/O pipeline for the section */
    if (H5Z_append_filter(filter_id, flags, cd_nelmts, cd_values, &filt_sect->filter, &filt_sect->nused,
                          &filt_sect->nalloc) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add filter to pipeline");

    /* Add the new filter to the pipeline */
    if (!found) {
        pline.tot_filt_nsects++;
        filt_sect->seq_sect = (size_t)sec_type;
    }

    /* Put the I/O pipeline information back into the property list */
    if (H5P_poke(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__set_filter2() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_nfilters1
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
H5Pget_nfilters1(hid_t plist_id)
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
} /* end H5Pget_nfilters1 */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_nfilters2
 *
 * Purpose:    Returns the number of filters in the data or link
 *        pipeline depending on whether PLIST_ID is a dataset creation
 *        or group creation property list.
 *        In each pipeline the filters are numbered from zero through
 *        N-1 where N is the value returned by this function in the
 *        parameter NUM_FILTERS.
 *        During output to the file the filters of a pipeline are applied
 *        in increasing order (the inverse is true for input).
 *
 *        The argument SEC_TYPE is the type identifier for a section
 *        in the structured chunk.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_nfilters2(hid_t plist_id, H5_section_type_t sec_type, int *num_filters)
{
    H5P_genplist_t *plist; /* Property list */
    H5O_pline_t     pline; /* Filter pipeline */
    unsigned        i;
    int             num       = 0;
    herr_t          ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE, true)))
        HGOTO_ERROR(H5E_ID, H5E_BADID, FAIL, "can't find object for ID");

    /* Get the pipeline property to query */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Set return value */
    ret_value = 0;
    for (i = 0; i < pline.tot_filt_nsects; i++) {
        if (pline.filt_sects[i].seq_sect == (size_t)sec_type) {
            num = (int)(pline.filt_sects[i].nused);
            break;
        }
    }

    if (num_filters)
        *num_filters = num;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_nfilters2 */

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
 * Function:    H5Pget_filter3
 *
 * Purpose:    This is the query counterpart of H5Pset_filter2().
 *        It retrieves information for a filter in the pipeline for a
 *        specified section of the structured chunk.
 *        The information returned is about a particular filter number in
 *        the filter pipeline depending on whether PLIST_ID is a
 *        dataset creation or group creation property list.
 *
 *        On input:
 *        SEC_TYPE is the type identifier for a section in the structured
 *        chunk.
 *
 *        IDX should be a value between zero and N-1 as described
 *        for H5Pget_nfilters2().
 *
 *        CD_NELMTS indicates the number of entries in the CD_VALUES
 *        array allocated by the caller.  On exit it contains the
 *        number of values defined by the filter.
 *
 *        FILTER_CONFIG is a bit field containing encode/decode flags from H5Zpublic.h.
 *
 *        The function will return failure if the filter number is out of range.
 *
 * Return:    Success:    Filter identification number.
 *            Failure:    H5Z_FILTER_ERROR (Negative)
 *
 *-------------------------------------------------------------------------
 */
H5Z_filter_t
H5Pget_filter3(hid_t plist_id, H5_section_type_t sec_type, unsigned idx, unsigned int *flags /*out*/,
               size_t *cd_nelmts /*in,out*/, unsigned cd_values[] /*out*/, size_t namelen /*in*/,
               char name[] /*out*/, unsigned *filter_config /*out*/)
{
    H5P_genplist_t          *plist;  /* Property list */
    const H5Z_filter_info_t *filter; /* Pointer to filter information */
    H5O_pline_t              pline;  /* Filter pipeline */
    unsigned                 i;
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

    for (i = 0; i < pline.tot_filt_nsects; i++) {
        if (pline.filt_sects[i].seq_sect == (size_t)sec_type) {
            if (idx >= pline.filt_sects[i].nused)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR,
                            "filter number for sec_type is invalid");
            break;
        }
    }
    if (i == pline.tot_filt_nsects)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "no filter defined for sec_type");

    /* Set pointer to particular filter to query */
    filter = &pline.filt_sects[i].filter[idx];

    if (H5P__get_filter(filter, flags, cd_nelmts, cd_values, namelen, name, filter_config) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5Z_FILTER_ERROR, "can't get filter info");

    /* For new just return 0 */
    ret_value = filter->id;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_filter3() */

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
H5P_get_filter_by_id(H5P_genplist_t *plist, H5Z_filter_t id, H5_section_type_t sec_type,
                     unsigned int *flags /*out*/, size_t *cd_nelmts /*in,out*/, unsigned cd_values[] /*out*/,
                     size_t namelen, char name[] /*out*/, unsigned *filter_config)
{
    H5O_pline_t        pline;               /* Filter pipeline */
    H5Z_filter_info_t *filter;              /* Pointer to filter information */
    herr_t             ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Get pipeline info */
    if (H5P_peek(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    if (pline.version < H5O_PLINE_VERSION_3) {

        /* Get pointer to filter in pipeline */
        if (NULL == (filter = H5Z_filter_info(pline.filter, pline.nused, id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter ID is invalid");
    }
    else {
        assert(pline.version == H5O_PLINE_VERSION_3);
        H5Z_stc_filter_sect_t *filt_sect = NULL;
        unsigned               i;

        for (i = 0, filt_sect = &pline.filt_sects[0]; i < pline.tot_filt_nsects; i++, filt_sect++) {
            if (filt_sect->seq_sect == (size_t)sec_type)
                break;
        }
        if (i >= pline.tot_filt_nsects)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "no filter defined for sect_type");

        /* Get pointer to filter in pipeline */
        if (NULL == (filter = H5Z_filter_info(filt_sect->filter, filt_sect->nused, id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter ID is invalid");
    }

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
    if (H5P_get_filter_by_id(plist, id, H5_SECTION_UNKNOWN, flags, cd_nelmts, cd_values, namelen, name,
                             filter_config) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get filter info");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_filter_by_id2() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_filter_by_id3
 *
 * Purpose:    This is an additional query counterpart of H5Pset_filter2().
 *             It retrieves information for a filter specified by its identifier
 *             in the pipeline for a specified section of the structured chunk.
 *             The information returned is about about a particular filter
 *             identifier in the filter pipeline depending on whether
 *             PLIST_ID is a dataset creation or group creation property list.
 *
 *             On input:
 *             SECTION_NUMBER is the specified section of the structured chunk
 *
 *             ID is the filter ID to retrieve the parameters for
 *
 *             BUF_SIZE indicates the size in bytes of the buffer pointed to
 *             by BUF which is allocated by the caller.  On exit, it contains
 *             size of the values defined by the filter.
 *
 *             FILTER_CONFIG is a bit field containing encode/decode flags
 *             from H5Zpublic.h.
 *
 *             If the filter is not set for the property list, an error will be returned.
 *
 * Return:    Success:    Non-negative
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_filter_by_id3(hid_t plist_id, H5_section_type_t sec_type, H5Z_filter_t id, unsigned int *flags /*out*/,
                     size_t *cd_nelmts /*in,out*/, unsigned cd_values[] /*out*/, size_t namelen /*in*/,
                     char name[] /*out*/, unsigned *filter_config /*out*/)
{
    H5P_genplist_t *plist;               /* Property list */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(H5Z_FILTER_ERROR)

    /* Check args */
    if (sec_type < 0 || sec_type > H5_SECTION_NUM)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid section ");

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
    if (H5P_get_filter_by_id(plist, id, sec_type, flags, cd_nelmts, cd_values, namelen, name, filter_config) <
        0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get filter info");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_filter_by_id3() */

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

    if (pline.version < H5O_PLINE_VERSION_3) {

        /* Check if all filters are available */
        if ((ret_value = H5Z_all_filters_avail(pline.filter, pline.nused)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "can't check pipeline information");
    }
    else {
        assert(pline.version == H5O_PLINE_VERSION_3);
        H5Z_stc_filter_sect_t *filt_sect = NULL;
        unsigned               i;

        /* Check if all filters are available */
        for (i = 0, filt_sect = &pline.filt_sects[0]; i < pline.tot_filt_nsects; i++, filt_sect++) {
            if ((ret_value = H5Z_all_filters_avail(filt_sect->filter, filt_sect->nused)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "can't check pipeline information");
            if (ret_value == false)
                break;
        }
    }

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

    if (pline.version < H5O_PLINE_VERSION_3) {

        /* Get pointer to filter in pipeline */
        if ((ret_value = H5Z_filter_in_pline(pline.filter, pline.nused, id)) < 0)
            HGOTO_ERROR(H5E_PLINE, H5E_CANTCOMPARE, FAIL, "can't find filter");
    }
    else {
        assert(pline.version == H5O_PLINE_VERSION_3);
        H5Z_stc_filter_sect_t *filt_sect = NULL;
        unsigned               i;

        /* Check if filter is in the pipeline for each section */
        for (i = 0, filt_sect = &pline.filt_sects[0]; i < pline.tot_filt_nsects; i++, filt_sect++) {
            if ((ret_value = H5Z_filter_in_pline(filt_sect->filter, filt_sect->nused, id)) < 0)
                HGOTO_ERROR(H5E_PLINE, H5E_CANTCOMPARE, FAIL, "can't find filter");
            if (ret_value)
                break;
        }
        if (i >= pline.tot_filt_nsects)
            ret_value = false;
    }

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
H5Premove_filter1(hid_t plist_id, H5Z_filter_t filter_id)
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
    if (pline.filter && pline.nused) {
        /* Delete filter */
        if (H5Z_delete(&pline, filter_id, H5_SECTION_UNKNOWN) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't delete filter");

        /* Put the I/O pipeline information back into the property list */
        if (H5P_poke(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Premove_filter1() */

/*-------------------------------------------------------------------------
 * Function: H5Premove_filter2
 *
 * Purpose: Deletes a filter for a specified section of the structured chunk
 *          from the dataset creation or group creation property list;
 *          deletes all filters if FILTER is H5Z_FILTER_ALL
 *
 * Return: Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
/* HERE */
herr_t
H5Premove_filter2(hid_t plist_id, H5_section_type_t sec_type, H5Z_filter_t filter_id)
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
    if (pline.tot_filt_nsects) {
        /* Delete filter */
        if (H5Z_delete(&pline, filter_id, sec_type) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't delete filter");

        /* Put the I/O pipeline information back into the property list */
        if (H5P_poke(plist, H5O_CRT_PIPELINE_NAME, &pline) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Premove_filter2() */

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
            H5Z_class3_t *cls;

            H5Z_find(true, filter->id, &cls);
            if (cls)
                s = cls->name;
        } /* end if */

        /* Check for actual name */
        if (s) {
            strncpy(name, s, namelen);
            name[namelen - 1] = '\0';
        } /* end if */
        else {
            /* Check for unknown library filter */
            /* (probably from a future version of the library) */
            if (filter->id < 256) {
                strncpy(name, "Unknown library filter", namelen);
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
        H5Z_filter_info_t filter;   /* Filter info, for pipeline */
        uint8_t           has_name; /* Flag to indicate whether filter has a name */
        unsigned          v;        /* Local index variable */

        /* decode filter id */
        INT32DECODE(*pp, filter.id);

        /* decode filter flags */
        H5_DECODE_UNSIGNED(*pp, filter.flags);

        /* decode value indicating if the name is encoded */
        has_name = *(*pp)++;
        if (has_name) {
            /* decode name */
            filter.name = H5MM_xstrdup((const char *)(*pp));
            *pp += H5Z_COMMON_NAME_LEN;
        } /* end if */
        else
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

        /* Add the filter to the I/O pipeline */
        if (H5Z_append(pline, filter.id, filter.flags, filter.cd_nelmts, filter.cd_values) < 0)
            HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add filter to pipeline");

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
    if (H5P_get_filter_by_id(plist, id, H5_SECTION_UNKNOWN, flags, cd_nelmts, cd_values, namelen, name,
                             NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get filter info");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_filter_by_id1() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */
