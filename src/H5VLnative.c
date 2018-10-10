/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:	The native VOL driver where access is to a single HDF5 file 
 *              using HDF5 VFDs. 
 */

#define H5A_FRIEND              /* Suppress error about including H5Apkg    */
#define H5D_FRIEND              /* Suppress error about including H5Dpkg    */
#define H5F_FRIEND              /* Suppress error about including H5Fpkg    */
#define H5G_FRIEND              /* Suppress error about including H5Gpkg    */
#define H5L_FRIEND              /* Suppress error about including H5Lpkg    */
#define H5O_FRIEND              /* Suppress error about including H5Opkg    */
#define H5R_FRIEND              /* Suppress error about including H5Rpkg    */
#define H5T_FRIEND              /* Suppress error about including H5Tpkg    */


#include "H5private.h"          /* Generic Functions                        */
#include "H5Apkg.h"             /* Attributes                               */
#include "H5Dpkg.h"             /* Datasets                                 */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5Fpkg.h"             /* Files                                    */
#include "H5Gpkg.h"             /* Groups                                   */
#include "H5Iprivate.h"         /* IDs                                      */
#include "H5Lpkg.h"             /* Links                                    */
#include "H5MFprivate.h"        /* File memory management                   */
#include "H5MMprivate.h"        /* Memory management                        */
#include "H5Opkg.h"             /* Object headers                           */
#include "H5Pprivate.h"         /* Property lists                           */
#include "H5Rpkg.h"             /* References                               */
#include "H5SMprivate.h"        /* Shared Object Header Messages            */
#include "H5Tpkg.h"             /* Datatypes                                */
#include "H5VLprivate.h"        /* VOL drivers                              */
#include "H5VLnative.h"         /* Native VOL driver                        */

/*
 * The VOL driver identification number.
 */
static hid_t H5VL_NATIVE_ID_g = H5I_INVALID_HID;


/* Prototypes */
static H5F_t *H5VL_native_get_file(void *obj, H5I_type_t type);
static herr_t H5VL__native_term(void);

/* Atrribute callbacks */
static void *H5VL_native_attr_create(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req);
static void *H5VL_native_attr_open(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t aapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_native_attr_read(void *attr, hid_t dtype_id, void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_native_attr_write(void *attr, hid_t dtype_id, const void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_native_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_attr_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_attr_close(void *attr, hid_t dxpl_id, void **req);

/* Dataset callbacks */
static void *H5VL_native_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
static void *H5VL_native_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_native_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                       hid_t file_space_id, hid_t plist_id, void *buf, void **req);
static herr_t H5VL_native_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                        hid_t file_space_id, hid_t plist_id, const void *buf, void **req);
static herr_t H5VL_native_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_dataset_specific(void *dset, H5VL_dataset_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_dataset_optional(void *dset, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_dataset_close(void *dset, hid_t dxpl_id, void **req);

/* File callbacks */
static void *H5VL_native_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
static void *H5VL_native_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_native_file_get(void *file, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_file_specific(void *file, H5VL_file_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_file_optional(void *file, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_file_close(void *file, hid_t dxpl_id, void **req);

/* Group callbacks */
static void *H5VL_native_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
static void *H5VL_native_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_native_group_get(void *obj, H5VL_group_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_group_specific(void *dset, H5VL_group_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_group_close(void *grp, hid_t dxpl_id, void **req);

/* Link callbacks */
static herr_t H5VL_native_link_create(H5VL_link_create_type_t create_type, void *obj, 
                                      H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_native_link_copy(void *src_obj, H5VL_loc_params_t loc_params1,
                                    void *dst_obj, H5VL_loc_params_t loc_params2,
                                    hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_native_link_move(void *src_obj, H5VL_loc_params_t loc_params1,
                                    void *dst_obj, H5VL_loc_params_t loc_params2,
                                    hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_native_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_link_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);

/* Object callbacks */
static void *H5VL_native_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, hid_t dxpl_id, void **req);
static herr_t H5VL_native_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name, 
                                      void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name, 
                                      hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_native_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_object_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_object_optional(void *obj, hid_t dxpl_id, void **req, va_list arguments);

/* Datatype callbacks */
static void *H5VL_native_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req);
static void *H5VL_native_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_native_datatype_get(void *dt, H5VL_datatype_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_datatype_specific(void *dt, H5VL_datatype_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_native_datatype_close(void *dt, hid_t dxpl_id, void **req);

/* Native VOL driver class struct */
static H5VL_class_t H5VL_native_cls_g = {
    H5VL_NATIVE_VERSION,                            /* version      */
    H5VL_NATIVE_VALUE,                              /* value        */
    H5VL_NATIVE_NAME,                               /* name         */
    NULL,                                           /* initialize   */
    NULL,                                           /* terminate    */
    (size_t)0,                                      /* fapl size    */
    NULL,                                           /* fapl copy    */
    NULL,                                           /* fapl free    */
    {   /* attribute_cls */
        H5VL_native_attr_create,                    /* create       */
        H5VL_native_attr_open,                      /* open         */
        H5VL_native_attr_read,                      /* read         */
        H5VL_native_attr_write,                     /* write        */
        H5VL_native_attr_get,                       /* get          */
        H5VL_native_attr_specific,                  /* specific     */
        NULL,                                       /* optional     */
        H5VL_native_attr_close                      /* close        */
    },
    {   /* dataset_cls */
        H5VL_native_dataset_create,                 /* create       */
        H5VL_native_dataset_open,                   /* open         */
        H5VL_native_dataset_read,                   /* read         */
        H5VL_native_dataset_write,                  /* write        */
        H5VL_native_dataset_get,                    /* get          */
        H5VL_native_dataset_specific,               /* specific     */
        H5VL_native_dataset_optional,               /* optional     */
        H5VL_native_dataset_close                   /* close        */
    },
    {   /* datatype_cls */
        H5VL_native_datatype_commit,                /* commit       */
        H5VL_native_datatype_open,                  /* open         */
        H5VL_native_datatype_get,                   /* get          */
        H5VL_native_datatype_specific,              /* specific     */
        NULL,                                       /* optional     */
        H5VL_native_datatype_close                  /* close        */
    },
    {   /* file_cls */
        H5VL_native_file_create,                    /* create       */
        H5VL_native_file_open,                      /* open         */
        H5VL_native_file_get,                       /* get          */
        H5VL_native_file_specific,                  /* specific     */
        H5VL_native_file_optional,                  /* optional     */
        H5VL_native_file_close                      /* close        */
    },
    {   /* group_cls */
        H5VL_native_group_create,                   /* create       */
        H5VL_native_group_open,                     /* open         */
        H5VL_native_group_get,                      /* get          */
        H5VL_native_group_specific,                 /* specific     */
        NULL,                                       /* optional     */
        H5VL_native_group_close                     /* close        */
    },
    {   /* link_cls */
        H5VL_native_link_create,                    /* create       */
        H5VL_native_link_copy,                      /* copy         */
        H5VL_native_link_move,                      /* move         */
        H5VL_native_link_get,                       /* get          */
        H5VL_native_link_specific,                  /* specific     */
        NULL                                        /* optional     */
    },
    {   /* object_cls */
        H5VL_native_object_open,                    /* open         */
        H5VL_native_object_copy,                    /* copy         */
        H5VL_native_object_get,                     /* get          */
        H5VL_native_object_specific,                /* specific     */
        H5VL_native_object_optional                 /* optional     */
    },
    {   /* async_cls */
        NULL,                                       /* cancel       */
        NULL,                                       /* test         */
        NULL                                        /* wait         */
    },
    NULL                                            /* optional     */
};


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_init
 *
 * Purpose:     Initialize this VOL driver by registering it with the
 *              library.
 *
 * Return:      Success:    The ID for the native driver
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_native_init(void)
{
    hid_t ret_value = H5I_INVALID_HID;            /* Return value */

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Register the native VOL driver, if it isn't already */
    if(NULL == H5I_object_verify(H5VL_NATIVE_ID_g, H5I_VOL)) {
        if((H5VL_NATIVE_ID_g = H5VL_register((const H5VL_class_t *)&H5VL_native_cls_g, 
                                          sizeof(H5VL_class_t), TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTINSERT, H5I_INVALID_HID, "can't create ID for native VOL driver")
    }

    /* Set return value */
    ret_value = H5VL_NATIVE_ID_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_init() */

/* XXX (VOL_MERGE): TEMPORARY */
hid_t
H5VL_native_get_driver_id(void)
{
    return H5VL_NATIVE_ID_g;
}


/*---------------------------------------------------------------------------
 * Function:    H5VL__native_term
 *
 * Purpose:     Shut down the native VOL
 *
 * Returns:     SUCCEED (Can't fail)
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5VL__native_term(void)
{
    FUNC_ENTER_STATIC_NOERR

    /* Reset VFL ID */
    H5VL_NATIVE_ID_g = H5I_INVALID_HID;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__native_term() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_native
 *
 * Purpose:     Modify the file access property list to use the H5VL_NATIVE
 *              driver defined in this source file.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_native(hid_t fapl_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t          ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", fapl_id);

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    ret_value = H5P_set_vol(plist, H5VL_NATIVE_ID_g, NULL);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_native() */


/*---------------------------------------------------------------------------
 * Function:    H5VL_native_get_file
 *
 * Purpose:     Utility routine to get file struct for an object via the
 *              native VOL driver.
 *
 * Returns:     SUCCESS:    A pointer to the H5F_t struct for the file
 *                          associated with the object.
 *              FAILURE:    NULL
 *
 *---------------------------------------------------------------------------
 */
static H5F_t *
H5VL_native_get_file(void *obj, H5I_type_t type)
{
    H5F_t      *ret_value  = NULL;         /* File pointer             */
    H5O_loc_t  *oloc       = NULL;         /* Object location for ID   */

    FUNC_ENTER_NOAPI_NOINIT

    switch(type) {
        case H5I_FILE:
            ret_value = (H5F_t *)obj;
            break;
        case H5I_GROUP:
            {
                H5G_t	*grp;
                grp = (H5G_t *)obj;
                oloc = H5G_oloc(grp);
                break;
            }
        case H5I_DATATYPE:
            {
                H5T_t	*dt;
                dt = (H5T_t *)obj;
                oloc = H5T_oloc(dt);
                break;
            }
        case H5I_DATASET:
            {
                H5D_t	*dset;
                dset = (H5D_t *)obj;
                oloc = H5D_oloc(dset);
                break;
            }

        case H5I_ATTR:
            {
                H5A_t	*attr;
                attr = (H5A_t *)obj;
                oloc = H5A_oloc(attr);
                break;
            }
        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_DATASPACE:
        case H5I_REFERENCE:
        case H5I_VFL:
        case H5I_VOL:
        case H5I_GENPROP_CLS:
        case H5I_GENPROP_LST:
        case H5I_ERROR_CLASS:
        case H5I_ERROR_MSG:
        case H5I_ERROR_STACK:
        case H5I_NTYPES:
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")
    }

    if(!ret_value) {
        if (!oloc)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "object is not assocated with a file")
        ret_value = oloc->file;
    }
    if(!ret_value)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "object is not associated with a file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5VL_native_get_file */


/*---------------------------------------------------------------------------
 * Function:    H5VL_native_register
 *
 * Purpose:     Utility routine to register an ID with the native VOL driver
 *              as an auxilary object.
 *
 * Returns:     Success:    An ID for the object
 *              Failure:    H5I_INVALID_HID
 *
 *---------------------------------------------------------------------------
 */
hid_t
H5VL_native_register(H5I_type_t type, void *obj, hbool_t app_ref)
{
    hid_t    ret_value = H5I_INVALID_HID;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(obj);

    /* If the datatype is already VOL-managed, the datatype's vol_obj
     * field will get clobbered later, so disallow this.
     */
    if(type == H5I_DATATYPE)
        if(((H5T_t *)obj)->vol_obj != NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "can only get an ID for an uncommitted datatype")

    /* Get an ID for the object */
    if((ret_value = H5VL_object_register(obj, type, H5VL_NATIVE_ID_g, app_ref)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to get an ID for the object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_register() */


/*---------------------------------------------------------------------------
 * Function:    H5VL_native_unregister
 *
 * Purpose:     Utility routine to decrement ref count on Native VOL driver
 *              objects.
 *
 * Returns:     SUCCEED/FAIL
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VL_native_unregister(hid_t obj_id)
{
    H5VL_object_t  *vol_obj     = NULL;
    herr_t          ret_value   = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the driver pointer */
    if(NULL == (vol_obj = (H5VL_object_t *)H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid ID")

    /* free object */
    if(H5VL_free_object(vol_obj) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "unable to free VOL object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_unregister() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_attr_create
 *
 * Purpose:	Creates an attribute on an object.
 *
 * Return:	Success:	attr id. 
 *		Failure:	NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_attr_create(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, 
                        hid_t H5_ATTR_UNUSED aapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5G_loc_t       loc;                /* Object location */
    H5G_loc_t       obj_loc;            /* Location used to open group */
    hbool_t         loc_found = FALSE;  
    H5P_genplist_t  *plist;             /* Property list pointer */
    hid_t           type_id, space_id;
    H5T_t	    *type, *dt;         /* Datatype to use for attribute */
    H5S_t	    *space;             /* Dataspace to use for attribute */
    H5A_t           *attr = NULL;
    void            *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* get creation properties */
    if(H5P_get(plist, H5VL_PROP_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id")
    if(H5P_get(plist, H5VL_PROP_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id")

    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")
    if(0 == (H5F_INTENT(loc.oloc->file) & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_ARGS, H5E_WRITEERROR, NULL, "no write intent on file")

    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a datatype")
    /* If this is a named datatype, get the driver pointer to the datatype */
    type = H5T_get_actual_type(dt);

    if(NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a data space")

    if(loc_params.type == H5VL_OBJECT_BY_SELF) {
        /* H5Acreate */
        /* Go do the real work for attaching the attribute to the dataset */
        if(NULL == (attr = H5A__create(&loc, attr_name, type, space, acpl_id)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to create attribute")
    }
    else if(loc_params.type == H5VL_OBJECT_BY_NAME) {
        /* H5Acreate_by_name */
        if(NULL == (attr = H5A__create_by_name(&loc, loc_params.loc_data.loc_by_name.name, attr_name, type, space, acpl_id)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to create attribute")
    }
    else {
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "unknown attribute create parameters")
    }
    ret_value = (void *)attr;

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, NULL, "can't free location") 
   FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_attr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_attr_open
 *
 * Purpose:	Opens a attr inside a native h5 file.
 *
 * Return:	Success:	attr id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_attr_open(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, 
                      hid_t H5_ATTR_UNUSED aapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5G_loc_t    loc;             /* Object location */
    H5A_t        *attr = NULL;    /* Attribute opened */
    void         *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    if(loc_params.type == H5VL_OBJECT_BY_SELF) {
        /* H5Aopen */
        /* Open the attribute */
        if(NULL == (attr = H5A__open(&loc, attr_name)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to open attribute: '%s'", attr_name)
    }
    else if(loc_params.type == H5VL_OBJECT_BY_NAME) {
        /* H5Aopen_by_name */
        /* Open the attribute on the object header */
        if(NULL == (attr = H5A__open_by_name(&loc, loc_params.loc_data.loc_by_name.name, attr_name)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "can't open attribute")
    }
    else if(loc_params.type == H5VL_OBJECT_BY_IDX) {
        /* H5Aopen_by_idx */
        /* Open the attribute in the object header */
        if(NULL == (attr = H5A__open_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, 
                                           loc_params.loc_data.loc_by_idx.idx_type, 
                                           loc_params.loc_data.loc_by_idx.order, 
                                           loc_params.loc_data.loc_by_idx.n)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to open attribute")
    }
    else {
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "unknown attribute open parameters")
    }

    ret_value = (void *)attr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_attr_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_attr_read
 *
 * Purpose:	Reads in data from attribute.
 *
 *              Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_attr_read(void *attr, hid_t dtype_id, void *buf, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5T_t *mem_type;            /* Memory datatype */
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (mem_type = (H5T_t *)H5I_object_verify(dtype_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    /* Go write the actual data to the attribute */
    if((ret_value = H5A__read((H5A_t*)attr, mem_type, buf)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "unable to read attribute")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_attr_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_attr_write
 *
 * Purpose:	Writes out data to attribute.
 *
 *              Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_attr_write(void *attr, hid_t dtype_id, const void *buf, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5T_t *mem_type;            /* Memory datatype */
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (mem_type = (H5T_t *)H5I_object_verify(dtype_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    /* Go write the actual data to the attribute */
    if((ret_value = H5A__write((H5A_t*)attr, mem_type, buf)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL, "unable to write attribute")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_attr_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_attr_get
 *
 * Purpose:	Gets certain information about an attribute
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(get_type) {
        /* H5Aget_space */
        case H5VL_ATTR_GET_SPACE:
            {
                hid_t	*ret_id = va_arg(arguments, hid_t *);
                H5A_t   *attr = (H5A_t *)obj;

                if((*ret_id = H5A_get_space(attr)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get space ID of attribute")
                break;
            }
        /* H5Aget_type */
        case H5VL_ATTR_GET_TYPE:
            {
                hid_t	*ret_id = va_arg(arguments, hid_t *);
                H5A_t   *attr = (H5A_t *)obj;

                if((*ret_id = H5A__get_type(attr)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of attribute")
                break;
            }
        /* H5Aget_create_plist */
        case H5VL_ATTR_GET_ACPL:
            {
                hid_t	*ret_id = va_arg(arguments, hid_t *);
                H5A_t   *attr = (H5A_t *)obj;

                if((*ret_id = H5A__get_create_plist(attr)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get creation property list for attr")

                break;
            }
        /* H5Aget_name */
        case H5VL_ATTR_GET_NAME:
            {
                H5VL_loc_params_t loc_params = va_arg(arguments, H5VL_loc_params_t);
                size_t	buf_size = va_arg(arguments, size_t);
                char    *buf = va_arg(arguments, char *);
                ssize_t	*ret_val = va_arg(arguments, ssize_t *);
                H5A_t   *attr = NULL;

                if(H5VL_OBJECT_BY_SELF == loc_params.type) {
                    attr = (H5A_t *)obj;
                    /* Call private function in turn */
                    if(0 > (*ret_val = H5A__get_name(attr, buf_size, buf)))
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get attribute name")
                }
                else if(H5VL_OBJECT_BY_IDX == loc_params.type) {
                    H5G_loc_t loc;
                    
                    /* check arguments */
                    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                    /* Open the attribute on the object header */
                    if(NULL == (attr = H5A__open_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, 
                                                       loc_params.loc_data.loc_by_idx.idx_type, 
                                                       loc_params.loc_data.loc_by_idx.order, 
                                                       loc_params.loc_data.loc_by_idx.n)))
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

                    /* Get the length of the name */
                    *ret_val = (ssize_t)HDstrlen(attr->shared->name);

                    /* Copy the name into the user's buffer, if given */
                    if(buf) {
                        HDstrncpy(buf, attr->shared->name, MIN((size_t)(*ret_val + 1), buf_size));
                        if((size_t)(*ret_val) >= buf_size)
                            buf[buf_size - 1]='\0';
                    } /* end if */

                    /* Release resources */
                    if(attr && H5A__close(attr) < 0)
                        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")
                }
                else
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get name of attr")

                break;
            }
        /* H5Aget_info */
        case H5VL_ATTR_GET_INFO:
            {
                H5VL_loc_params_t loc_params = va_arg(arguments, H5VL_loc_params_t);
                H5A_info_t   *ainfo = va_arg(arguments, H5A_info_t *);
                H5A_t   *attr = NULL;

                if(H5VL_OBJECT_BY_SELF == loc_params.type) {
                    attr = (H5A_t *)obj;
                    if(H5A__get_info(attr, ainfo) < 0)
                        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get attribute info")
                }
                else if(H5VL_OBJECT_BY_NAME == loc_params.type) {
                    char *attr_name = va_arg(arguments, char *);
                    H5G_loc_t loc;
                    
                    /* check arguments */
                    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                    /* Open the attribute on the object header */
                    if(NULL == (attr = H5A__open_by_name(&loc, loc_params.loc_data.loc_by_name.name, 
                                                        attr_name)))
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

                    /* Get the attribute information */
                    if(H5A__get_info(attr, ainfo) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to get attribute info")

                    /* Release resources */
                    if(attr && H5A__close(attr) < 0)
                        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")
                }
                else if(H5VL_OBJECT_BY_IDX == loc_params.type) {
                    H5G_loc_t loc;
                    
                    /* check arguments */
                    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                    /* Open the attribute on the object header */
                    if(NULL == (attr = H5A__open_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, 
                                                       loc_params.loc_data.loc_by_idx.idx_type, 
                                                       loc_params.loc_data.loc_by_idx.order, 
                                                       loc_params.loc_data.loc_by_idx.n)))
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

                    /* Get the attribute information */
                    if(H5A__get_info(attr, ainfo) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to get attribute info")

                    /* Release resources */
                    if(attr && H5A__close(attr) < 0)
                        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")
                }
                else
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get name of attr")

                break;
            }
        case H5VL_ATTR_GET_STORAGE_SIZE:
            {
                hsize_t *ret = va_arg(arguments, hsize_t *);
                H5A_t   *attr = (H5A_t *)obj;

                /* Set return value */
                *ret = attr->shared->data_size;
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from attr")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_attr_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_attr_specific
 *
 * Purpose:	Specific operations for attributes
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2014
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_attr_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_attr_specific_t specific_type, 
                          hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5G_loc_t   loc;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get location for passed-in object */
    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    /* XXX: Do I need to clean this up? */

    switch(specific_type) {
        case H5VL_ATTR_DELETE:
            {
                char    *attr_name = va_arg(arguments, char *);

                if(H5VL_OBJECT_BY_SELF == loc_params.type) {
                    /* H5Adelete */
                    /* Delete the attribute from the location */
                    if(H5O__attr_remove(loc.oloc, attr_name) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")
                }
                else if(H5VL_OBJECT_BY_NAME == loc_params.type) {
                    /* H5Adelete_by_name */
                    /* Delete the attribute */
                    if(H5A__delete_by_name(&loc, loc_params.loc_data.loc_by_name.name, attr_name) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")
                }
                else if(H5VL_OBJECT_BY_IDX == loc_params.type) {
                    /* H5Adelete_by_idx */
                    /* Delete the attribute from the location */
                    if(H5A__delete_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, loc_params.loc_data.loc_by_idx.idx_type,
                            loc_params.loc_data.loc_by_idx.order, loc_params.loc_data.loc_by_idx.n) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")
                }
                else
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown attribute remove parameters")
                break;
            }
        case H5VL_ATTR_EXISTS:
            {
                const char *attr_name = va_arg(arguments, const char *);
                htri_t	*ret = va_arg(arguments, htri_t *);

                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Aexists */
                    /* Check if the attribute exists */
                    if((*ret = H5O__attr_exists(loc.oloc, attr_name)) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to determine if attribute exists")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Aexists_by_name */
                    /* Check if the attribute exists */
                    if((*ret = H5A__exists_by_name(loc, loc_params.loc_data.loc_by_name.name, attr_name)) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to determine if attribute exists")
                }
                else
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown parameters")
                break;
            }
        case H5VL_ATTR_ITER:
            {
                H5_index_t          idx_type    = va_arg(arguments, H5_index_t);
                H5_iter_order_t     order       = va_arg(arguments, H5_iter_order_t);
                hsize_t            *idx         = va_arg(arguments, hsize_t *);
                H5A_operator2_t     op          = va_arg(arguments, H5A_operator2_t);
                void               *op_data     = va_arg(arguments, void *);

                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Aiterate2 */

                    /* Iterate over attributes */
                    if((ret_value = H5A__iterate(&loc, ".", idx_type, order, idx, op, op_data)) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error iterating over attributes")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Aiterate_by_name */

                    /* Iterate over attributes by name */
                    if((ret_value = H5A__iterate(&loc, loc_params.loc_data.loc_by_name.name, idx_type, order, idx, op, op_data)) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "attribute iteration failed");
                }
                else
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown parameters")
                break;
            }
        /* H5Arename/rename_by_name */
        case H5VL_ATTR_RENAME:
            {
                const char *old_name  = va_arg(arguments, const char *);
                const char *new_name  = va_arg(arguments, const char *);

                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Arename */
                    /* Call attribute rename routine */
                    if(H5O__attr_rename(loc.oloc, old_name, new_name) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTRENAME, FAIL, "can't rename attribute")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Arename_by_name */
                    /* Call attribute rename routine */
                    if(H5A__rename_by_name(loc, loc_params.loc_data.loc_by_name.name, old_name, new_name) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTRENAME, FAIL, "can't rename attribute")
                }
                else
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown attribute rename parameters")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid specific operation")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_attr_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_attr_close
 *
 * Purpose:	Closes an attribute.
 *
 * Return:	Success:	0
 *		Failure:	-1, attr not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_attr_close(void *attr, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5A__close((H5A_t*)attr) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close attribute")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_attr_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_dataset_create
 *
 * Purpose:     Creates a dataset in a native HDF5 file
 *
 * Return:      Success:    Pointer to a dataset struct
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, 
                           hid_t dapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    H5G_loc_t	    loc;                 /* Object location to insert dataset into */
    hid_t           type_id = H5I_INVALID_HID;
    hid_t           space_id = H5I_INVALID_HID;
    hid_t           lcpl_id = H5I_INVALID_HID;
    H5D_t          *dset = NULL;        /* New dataset's info */
    const H5S_t    *space;              /* Dataspace for dataset */
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* Get creation properties */
    if(H5P_get(plist, H5VL_PROP_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id")
    if(H5P_get(plist, H5VL_PROP_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id")
    if(H5P_get(plist, H5VL_PROP_DSET_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id")

    /* Check arguments */
    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")
    if(H5I_DATATYPE != H5I_get_type(type_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a datatype ID")
    if(NULL == (space = (const H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a dataspace ID")

    /* H5Dcreate_anon */
    if(NULL == name) {
        /* build and open the new dataset */
        if(NULL == (dset = H5D__create(loc.oloc->file, type_id, space, dcpl_id, dapl_id)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to create dataset")
    }
    /* H5Dcreate2 */
    else {
        /* Create the new dataset & get its ID */
        if(NULL == (dset = H5D__create_named(&loc, name, type_id, space, lcpl_id, 
                                             dcpl_id, dapl_id)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to create dataset")
    }
    ret_value = (void *)dset;

done:
    if(NULL == name) {
        /* Release the dataset's object header, if it was created */
        if(dset) {
            H5O_loc_t *oloc;         /* Object location for dataset */

            /* Get the new dataset's object location */
            if(NULL == (oloc = H5D_oloc(dset)))
                HDONE_ERROR(H5E_DATASET, H5E_CANTGET, NULL, "unable to get object location of dataset")

            /* Decrement refcount on dataset's object header in memory */
            if(H5O_dec_rc_by_loc(oloc) < 0)
                HDONE_ERROR(H5E_DATASET, H5E_CANTDEC, NULL, "unable to decrement refcount on newly created object")
        }
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_dataset_open
 *
 * Purpose:     Opens a dataset in a native HDF5 file.
 *
 * Return:      Success:    Pointer to a dataset struct
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, 
                         hid_t dapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5D_t       *dset = NULL;
    H5G_loc_t	 loc;		        /* Object location of group */
    void         *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    /* Open the dataset */
    if(NULL == (dset = H5D__open_name(&loc, name, dapl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, NULL, "unable to open dataset")

    ret_value = (void *)dset;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_dataset_read
 *
 * Purpose:     Reads raw data from a dataset into a buffer.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_dataset_read(void *obj, hid_t mem_type_id, hid_t mem_space_id,
                         hid_t file_space_id, hid_t H5_ATTR_UNUSED dxpl_id, void *buf, void H5_ATTR_UNUSED **req)
{
    H5D_t         *dset = (H5D_t *)obj;
    const H5S_t   *mem_space = NULL;
    const H5S_t   *file_space = NULL;
    herr_t         ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments */
    if(NULL == dset->oloc.file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "dataset is not associated with a file")

    /* Get validated dataspace pointers */
    if(H5S_get_validated_dataspace(mem_space_id, &mem_space) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "could not get a validated dataspace from mem_space_id")
    if(H5S_get_validated_dataspace(file_space_id, &file_space) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "could not get a validated dataspace from file_space_id")

    /* Read raw data */
    if(H5D__read(dset, mem_type_id, mem_space, file_space, buf/*out*/) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_read() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_dataset_write
 *
 * Purpose:     Writes raw data from a buffer into a dataset.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_dataset_write(void *obj, hid_t mem_type_id, hid_t mem_space_id,
                          hid_t file_space_id, hid_t H5_ATTR_UNUSED dxpl_id, const void *buf, void H5_ATTR_UNUSED **req)
{
    H5D_t           *dset = (H5D_t *)obj;
    const H5S_t     *mem_space = NULL;
    const H5S_t     *file_space = NULL;
    herr_t           ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    if(NULL == dset->oloc.file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "dataset is not associated with a file")

    /* Get validated dataspace pointers */
    if(H5S_get_validated_dataspace(mem_space_id, &mem_space) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "could not get a validated dataspace from mem_space_id")
    if(H5S_get_validated_dataspace(file_space_id, &file_space) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "could not get a validated dataspace from file_space_id")

    /* Write the data */
    if(H5D__write(dset, mem_type_id, mem_space, file_space, buf) < 0) 
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_dataset_get
 *
 * Purpose:     Gets certain information about a dataset
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_dataset_get(void *obj, H5VL_dataset_get_t get_type, hid_t H5_ATTR_UNUSED dxpl_id, 
                        void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5D_t       *dset = (H5D_t *)obj;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(get_type) {
        /* H5Dget_space */
        case H5VL_DATASET_GET_SPACE:
            {
                hid_t	*ret_id = va_arg(arguments, hid_t *);

                if((*ret_id = H5D__get_space(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get space ID of dataset")

                break;
            }
            /* H5Dget_space_statuc */
        case H5VL_DATASET_GET_SPACE_STATUS:
            {
                H5D_space_status_t *allocation = va_arg(arguments, H5D_space_status_t *);

                /* Read data space address and return */
                if(H5D__get_space_status(dset, allocation) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get space status")

                break;
            }
            /* H5Dget_type */
        case H5VL_DATASET_GET_TYPE:
            {
                hid_t	*ret_id = va_arg(arguments, hid_t *);

                if((*ret_id = H5D__get_type(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of dataset")

                break;
            }
            /* H5Dget_create_plist */
        case H5VL_DATASET_GET_DCPL:
            {
                hid_t	*ret_id = va_arg(arguments, hid_t *);

                if((*ret_id = H5D_get_create_plist(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get creation property list for dataset")

                break;
            }
            /* H5Dget_access_plist */
        case H5VL_DATASET_GET_DAPL:
            {
                hid_t	*ret_id = va_arg(arguments, hid_t *);

                if((*ret_id = H5D_get_access_plist(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get access property list for dataset")

                break;
            }
            /* H5Dget_storage_size */
        case H5VL_DATASET_GET_STORAGE_SIZE:
            {
                hsize_t *ret = va_arg(arguments, hsize_t *);

                /* Set return value */
                if(H5D__get_storage_size(dset, ret) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get size of dataset's storage")
                break;
            }
            /* H5Dget_offset */
        case H5VL_DATASET_GET_OFFSET:
            {
                haddr_t *ret = va_arg(arguments, haddr_t *);

                /* Set return value */
                *ret = H5D__get_offset(dset);
                if(!H5F_addr_defined(*ret))
                    *ret = HADDR_UNDEF;
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from dataset")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_dataset_specific
 *
 * Purpose:     Specific operations for datasets
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_dataset_specific(void *obj, H5VL_dataset_specific_t specific_type, 
                             hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5D_t       *dset = (H5D_t *)obj;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(specific_type) {
        /* H5Dspecific_space */
        case H5VL_DATASET_SET_EXTENT:
            {
                const hsize_t *size = va_arg(arguments, const hsize_t *); 

                if(H5D__set_extent(dset, size) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set extent of dataset")
                break;
            }
        case H5VL_DATASET_FLUSH:
            {
                hid_t dset_id = va_arg(arguments, hid_t);

                /* Flush the dataset */
                if(H5D__flush(dset, dset_id) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTFLUSH, FAIL, "unable to flush dataset")

                break;
            }
        case H5VL_DATASET_REFRESH:
            {
                hid_t dset_id = va_arg(arguments, hid_t);

                /* Refresh the dataset */
                if((H5D__refresh(dset_id, dset)) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTLOAD, FAIL, "unable to refresh dataset")

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid specific operation")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_dataset_optional
 *
 * Purpose:     Perform a driver specific operation on a native dataset
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_dataset_optional(void *obj, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5D_t *dset = NULL;             /* Dataset */
    H5VL_dataset_optional_t optional_type = va_arg(arguments, H5VL_dataset_optional_t);
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(optional_type) {
        case H5VL_DATASET_FORMAT_CONVERT:
            {
                dset = (H5D_t *)obj;

                switch(dset->shared->layout.type) {
                    case H5D_CHUNKED:
                        /* Convert the chunk indexing type to version 1 B-tree if not */
                        if(dset->shared->layout.u.chunk.idx_type != H5D_CHUNK_IDX_BTREE)
                            if((H5D__format_convert(dset)) < 0)
                                HGOTO_ERROR(H5E_DATASET, H5E_CANTLOAD, FAIL, "unable to downgrade chunk indexing type for dataset")
                        break;

                    case H5D_CONTIGUOUS:
                    case H5D_COMPACT:
                        /* Downgrade the layout version to 3 if greater than 3 */
                        if(dset->shared->layout.version > H5O_LAYOUT_VERSION_DEFAULT)
                            if((H5D__format_convert(dset)) < 0)
                                HGOTO_ERROR(H5E_DATASET, H5E_CANTLOAD, FAIL, "unable to downgrade layout version for dataset")
                        break;

                    case H5D_VIRTUAL:
                        /* Nothing to do even though layout is version 4 */
                        break;

                    case H5D_LAYOUT_ERROR:
                    case H5D_NLAYOUTS:
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset layout type")

                    default: 
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "unknown dataset layout type")
                } /* end switch */

                break;
            }
        case H5VL_DATASET_GET_CHUNK_INDEX_TYPE:
            {
                H5D_chunk_index_t *idx_type = va_arg(arguments, H5D_chunk_index_t *);

                dset = (H5D_t *)obj;

                /* Make sure the dataset is chunked */
                if(H5D_CHUNKED != dset->shared->layout.type)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a chunked dataset")

                /* Get the chunk indexing type */
                *idx_type = dset->shared->layout.u.chunk.idx_type;

                break;
            }
        case H5VL_DATASET_GET_CHUNK_STORAGE_SIZE:
            {
                hsize_t *offset = va_arg(arguments, hsize_t *);
                hsize_t *chunk_nbytes = va_arg(arguments, hsize_t *);

                dset = (H5D_t *)obj;

                /* Make sure the dataset is chunked */
                if(H5D_CHUNKED != dset->shared->layout.type)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a chunked dataset")

                /* Call private function */
                if(H5D__get_chunk_storage_size(dset, offset, chunk_nbytes) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get storage size of chunk")

                break;
            }
        case H5VL_DATASET_CHUNK_READ:
            {
                const       hsize_t *offset     = va_arg(arguments, hsize_t *);
                uint32_t   *filters             = va_arg(arguments, uint32_t *);
                void       *buf                 = va_arg(arguments, void *);
                hsize_t     offset_copy[H5O_LAYOUT_NDIMS];  /* Internal copy of chunk offset */

                dset = (H5D_t *)obj;

                /* Check arguments */
                if(NULL == dset->oloc.file)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "dataset is not associated with a file")
                if(H5D_CHUNKED != dset->shared->layout.type)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a chunked dataset")

                /* Copy the user's offset array so we can be sure it's terminated properly.
                 * (we don't want to mess with the user's buffer).
                 */
                if(H5D__get_offset_copy(dset, offset, offset_copy) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "failure to copy offset array")

                /* Read the raw chunk */
                if(H5D__chunk_direct_read(dset, offset_copy, filters, buf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read unprocessed chunk data")

                break;
            }
        case H5VL_DATASET_CHUNK_WRITE:
            {
                uint32_t        filters             = va_arg(arguments, uint32_t);
                const  hsize_t *offset              = va_arg(arguments, const hsize_t *);
                uint32_t        data_size_32        = va_arg(arguments, uint32_t);
                const void     *buf                 = va_arg(arguments, const void *);
                hsize_t         offset_copy[H5O_LAYOUT_NDIMS];  /* Internal copy of chunk offset */

                dset = (H5D_t *)obj;

                /* Check arguments */
                if(NULL == dset->oloc.file)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "dataset is not associated with a file")
                if(H5D_CHUNKED != dset->shared->layout.type)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a chunked dataset")

                /* Copy the user's offset array so we can be sure it's terminated properly.
                 * (we don't want to mess with the user's buffer).
                 */
                if(H5D__get_offset_copy(dset, offset, offset_copy) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "failure to copy offset array")

                /* Write chunk */
                if(H5D__chunk_direct_write(dset, filters, offset_copy, data_size_32, buf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write unprocessed chunk data")

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid optional operation")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_dataset_close
 *
 * Purpose:     Closes a dataset.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL (dataset will not be closed)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_dataset_close(void *dset, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5D_close((H5D_t*)dset) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "can't close dataset")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_file_create
 *
 * Purpose:     Creates a file as a native HDF5 file.
 *
 * Return:      Success:    A pointer to an H5F_t file struct
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, 
                        hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5F_t *new_file = NULL;
    void  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Adjust bit flags by turning on the creation bit and making sure that
     * the EXCL or TRUNC bit is set.  All newly-created files are opened for
     * reading and writing.
     */
    if(0 == (flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC)))
        flags |= H5F_ACC_EXCL;	 /*default*/
    flags |= H5F_ACC_RDWR | H5F_ACC_CREAT;

    /* Create the file */ 
    if(NULL == (new_file = H5F_open(name, flags, fcpl_id, fapl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create file")

    new_file->id_exists = TRUE;
    ret_value = (void *)new_file;

done:
    if(NULL == ret_value && new_file) 
        if(H5F__close(new_file) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_file_open
 *
 * Purpose:     Opens a file as a native HDF5 file.
 *
 * Return:      Success:    A pointer to an H5F_t file struct
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5F_t *new_file = NULL;
    void  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Open the file */ 
    if(NULL == (new_file = H5F_open(name, flags, H5P_FILE_CREATE_DEFAULT, fapl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")

    new_file->id_exists = TRUE;
    ret_value = (void *)new_file;

done:
    if(NULL == ret_value && new_file && H5F_try_close(new_file, NULL) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_file_get
 *
 * Purpose:     Gets certain data about a file
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_file_get(void *obj, H5VL_file_get_t get_type, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5F_t       *f = NULL;              /* File struct */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(get_type) {
        /* H5Fget_access_plist */
        case H5VL_FILE_GET_FAPL:
            {
                H5P_genplist_t *new_plist;              /* New property list */
                hid_t          *plist_id = va_arg(arguments, hid_t *);

                f = (H5F_t *)obj;

                /* Retrieve the file's access property list */
                if((*plist_id = H5F_get_access_plist(f, TRUE)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file access property list")

                if(NULL == (new_plist = (H5P_genplist_t *)H5I_object(*plist_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")
                break;
            }
        /* H5Fget_create_plist */
        case H5VL_FILE_GET_FCPL:
            {
                H5P_genplist_t *plist;      /* Property list */
                hid_t          *plist_id = va_arg(arguments, hid_t *);

                f = (H5F_t *)obj;
                if(NULL == (plist = (H5P_genplist_t *)H5I_object(f->shared->fcpl_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

                /* Create the property list object to return */
                if((*plist_id = H5P_copy_plist(plist, TRUE)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "unable to copy file creation properties")

                break;
            }
        /* H5Fget_obj_count */
        case H5VL_FILE_GET_OBJ_COUNT:
            {
                unsigned    types = va_arg(arguments, unsigned);
                ssize_t    *ret = va_arg(arguments, ssize_t *);
                size_t      obj_count = 0;      /* Number of opened objects */

                f = (H5F_t *)obj;
                /* Perform the query */
                if(H5F_get_obj_count(f, types, TRUE, &obj_count) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "H5F_get_obj_count failed")

                /* Set the return value */
                *ret = (ssize_t)obj_count;
                break;
            }
        /* H5Fget_obj_ids */
        case H5VL_FILE_GET_OBJ_IDS:
            {
                unsigned    types = va_arg(arguments, unsigned);
                size_t      max_objs = va_arg(arguments, size_t);
                hid_t      *oid_list = va_arg(arguments, hid_t *);
                ssize_t    *ret = va_arg(arguments, ssize_t *);
                size_t      obj_count = 0;      /* Number of opened objects */

                f = (H5F_t *)obj;
                /* Perform the query */
                if(H5F_get_obj_ids(f, types, max_objs, oid_list, TRUE, &obj_count) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "H5F_get_obj_ids failed")

                /* Set the return value */
                *ret = (ssize_t)obj_count;
                break;
            }
        /* H5Fget_intent */
        case H5VL_FILE_GET_INTENT:
            {
                unsigned *intent_flags = va_arg(arguments, unsigned *);

                f = (H5F_t *)obj;

                /* HDF5 uses some flags internally that users don't know about.
                 * Simplify things for them so that they only get either H5F_ACC_RDWR
                 * or H5F_ACC_RDONLY and any SWMR flags.
                 */
                if(H5F_INTENT(f) & H5F_ACC_RDWR) {
                    *intent_flags = H5F_ACC_RDWR;

                    /* Check for SWMR write access on the file */
                    if(H5F_INTENT(f) & H5F_ACC_SWMR_WRITE)
                        *intent_flags |= H5F_ACC_SWMR_WRITE;
                }
                else {
                    *intent_flags = H5F_ACC_RDONLY;

                    /* Check for SWMR read access on the file */
                    if(H5F_INTENT(f) & H5F_ACC_SWMR_READ)
                        *intent_flags |= H5F_ACC_SWMR_READ;
                }

                break;
            }
        /* H5Fget_name */
        case H5VL_FILE_GET_NAME:
            {
                H5I_type_t  type = va_arg(arguments, H5I_type_t);
                size_t      size = va_arg(arguments, size_t);
                char       *name = va_arg(arguments, char *);
                ssize_t    *ret  = va_arg(arguments, ssize_t *);
                size_t      len;

                if(NULL == (f = H5VL_native_get_file(obj, type)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                len = HDstrlen(H5F_OPEN_NAME(f));

                if(name) {
                    HDstrncpy(name, H5F_OPEN_NAME(f), MIN(len + 1,size));
                    if(len >= size)
                        name[size-1]='\0';
                }

                /* Set the return value for the API call */
                *ret = (ssize_t)len;
                break;
            }
        /* H5Iget_file_id */
        case H5VL_OBJECT_GET_FILE:
            {
                H5I_type_t  type = va_arg(arguments, H5I_type_t);
                void      **ret = va_arg(arguments, void **);

                if(NULL == (f = H5VL_native_get_file(obj, type)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
                f->id_exists = TRUE;
                *ret = (void*)f;
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information")
    } /* end switch */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_file_specific
 *
 * Purpose:     Perform an operation
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_file_specific(void *obj, H5VL_file_specific_t specific_type, hid_t H5_ATTR_UNUSED dxpl_id, 
                          void H5_ATTR_UNUSED **req, va_list arguments)
{
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(specific_type) {
        /* H5Fflush */
        case H5VL_FILE_FLUSH:
            {
                H5I_type_t      type = va_arg(arguments, H5I_type_t);
                H5F_scope_t     scope = va_arg(arguments, H5F_scope_t);
                H5F_t	       *f = NULL;              /* File to flush */

                /* Get the file for the object */
                if(NULL == (f = H5VL_native_get_file(obj, type)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                /* Nothing to do if the file is read only. This determination is
                 * made at the shared open(2) flags level, implying that opening a
                 * file twice, once for read-only and once for read-write, and then
                 * calling H5Fflush() with the read-only handle, still causes data
                 * to be flushed.
                 */
                 if(H5F_ACC_RDWR & H5F_INTENT(f)) {
                     /* Flush other files, depending on scope */
                     if(H5F_SCOPE_GLOBAL == scope) {
                         /* Call the flush routine for mounted file hierarchies */
                         if(H5F_flush_mounts(f) < 0)
                             HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush mounted file hierarchy")
                     }
                     else {
                         /* Call the flush routine, for this file */
                         if(H5F__flush(f) < 0)
                             HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush file's cached information")
                     }
                 }
                break;
            }
        /* H5Fmount */
        case H5VL_FILE_MOUNT:
            {
                H5I_type_t  type       = va_arg(arguments, H5I_type_t);
                const char *name       = va_arg(arguments, const char *);
                H5F_t      *child      = va_arg(arguments, H5F_t *);
                hid_t       plist_id   = va_arg(arguments, hid_t);
                H5G_loc_t   loc;

                if(H5G_loc_real(obj, type, &loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                /* Do the mount */
                if(H5F__mount(&loc, name, child, plist_id) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_MOUNT, FAIL, "unable to mount file")

                break;
            }
        /* H5Funmount */
        case H5VL_FILE_UNMOUNT:
            {
                H5I_type_t  type       = va_arg(arguments, H5I_type_t);
                const char *name       = va_arg(arguments, const char *);
                H5G_loc_t   loc;

                if(H5G_loc_real(obj, type, &loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                /* Unmount */
                if(H5F__unmount(&loc, name) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_MOUNT, FAIL, "unable to unmount file")

                break;
            }
        /* H5Fis_accessible */
        case H5VL_FILE_IS_ACCESSIBLE:
            {
                hid_t       fapl_id = va_arg(arguments, hid_t);
                const char *name    = va_arg(arguments, const char *);
                htri_t     *ret     = va_arg(arguments, htri_t *);

                /* Call private routine */
                if((*ret = H5F__is_hdf5(name, fapl_id)) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "error in HDF5 file check")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid specific operation")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_file_optional
 *
 * Purpose:     Perform a driver specific operation on a native file
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_file_optional(void *obj, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5F_t *f = NULL;           /* File */
    H5VL_file_optional_t optional_type = va_arg(arguments, H5VL_file_optional_t);
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(optional_type) {
        /* H5Fget_filesize */
        case H5VL_FILE_GET_SIZE:
            {
                haddr_t     max_eof_eoa;            /* Maximum of the EOA & EOF */
                haddr_t     base_addr;              /* Base address for the file */
                hsize_t    *size = va_arg(arguments, hsize_t *);

                f = (H5F_t *)obj;

                /* Go get the actual file size */
                if(H5F__get_max_eof_eoa(f, &max_eof_eoa) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "file can't get max eof/eoa ")

                base_addr = H5FD_get_base_addr(f->shared->lf);

                if(size)
                    *size = (hsize_t)(max_eof_eoa + base_addr);     /* Convert relative base address for file to absolute address */

                break;
            }
        /* H5Fget_file_image */
        case H5VL_FILE_GET_FILE_IMAGE:
            {
                void       *buf_ptr   = va_arg(arguments, void *);
                ssize_t    *ret       = va_arg(arguments, ssize_t *);
                size_t      buf_len   = va_arg(arguments, size_t );

                f = (H5F_t *)obj;
                /* Do the actual work */
                if((*ret = H5F__get_file_image(f, buf_ptr, buf_len)) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "get file image failed")
                break;
            }
        /* H5Fget_freespace */
        case H5VL_FILE_GET_FREE_SPACE:
            {
                hsize_t	tot_space;	/* Amount of free space in the file */
                hssize_t    *ret = va_arg(arguments, hssize_t *);

                f = (H5F_t *)obj;
                /* Go get the actual amount of free space in the file */
                if(H5MF_get_freespace(f, &tot_space, NULL) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to check free space for file")
                *ret = (hssize_t)tot_space;
                break;
            }
        case H5VL_FILE_GET_FREE_SECTIONS:
            {
                H5F_sect_info_t *sect_info = va_arg(arguments, H5F_sect_info_t *);
                ssize_t         *ret       = va_arg(arguments, ssize_t *);
                H5F_mem_t       type       = va_arg(arguments, H5F_mem_t);
                size_t          nsects     = va_arg(arguments, size_t);

                f = (H5F_t *)obj;
                /* Go get the free-space section information in the file */
                if((*ret = H5MF_get_free_sections(f, type, nsects, sect_info)) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to check free space for file")
                break;
            }
        /* H5Fget_info2 */
        case H5VL_FILE_GET_INFO:
            {
                H5I_type_t  type   = va_arg(arguments, H5I_type_t);
                H5F_info2_t *finfo = va_arg(arguments, H5F_info2_t *);

                /* Get the file struct. This call is careful to not return the file pointer
                 * for the top file in a mount hierarchy.
                 */
                if(NULL == (f = H5VL_native_get_file(obj, type)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "could not get a file struct")

                /* Get the file info */
                if(H5F__get_info(f, finfo) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to retrieve file info")

                break;
            }
        /* H5Fget_mdc_config */
        case H5VL_FILE_GET_MDC_CONF:
            {
                H5AC_cache_config_t *config_ptr = va_arg(arguments, H5AC_cache_config_t *);

                f = (H5F_t *)obj;
                /* Go get the resize configuration */
                if(H5AC_get_cache_auto_resize_config(f->shared->cache, config_ptr) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_auto_resize_config() failed.")
                break;
            }
        /* H5Fget_mdc_hit_rate */
        case H5VL_FILE_GET_MDC_HR:
            {
                double *hit_rate_ptr = va_arg(arguments, double *);

                f = (H5F_t *)obj;
                /* Go get the current hit rate */
                if(H5AC_get_cache_hit_rate(f->shared->cache, hit_rate_ptr) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_hit_rate() failed.")
                break;
            }
        /* H5Fget_mdc_size */
        case H5VL_FILE_GET_MDC_SIZE:
            {
                size_t *max_size_ptr        = va_arg(arguments, size_t *);
                size_t *min_clean_size_ptr  = va_arg(arguments, size_t *);
                size_t *cur_size_ptr        = va_arg(arguments, size_t *); 
                int    *cur_num_entries_ptr = va_arg(arguments, int *); 
                uint32_t cur_num_entries;

                f = (H5F_t *)obj;
                /* Go get the size data */
                if(H5AC_get_cache_size(f->shared->cache, max_size_ptr, min_clean_size_ptr, 
                                       cur_size_ptr, &cur_num_entries) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_size() failed.")

                if(cur_num_entries_ptr != NULL)
                    *cur_num_entries_ptr = (int)cur_num_entries;
                break;
            }
        /* H5Fget_vfd_handle */
        case H5VL_FILE_GET_VFD_HANDLE:
            {
                void **file_handle  = va_arg(arguments, void **);
                hid_t  fapl_id      = va_arg(arguments, hid_t);

                f = (H5F_t *)obj;

                /* Retrieve the VFD handle for the file */
                if(H5F_get_vfd_handle(f, fapl_id, file_handle) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't retrieve VFD handle")
                break;
            }
        /* H5Fclear_elink_file_cache */
        case H5VL_FILE_CLEAR_ELINK_CACHE:
            {
                f = (H5F_t *)obj;

                /* Release the EFC */
                if(f->shared->efc)
                    if(H5F__efc_release(f->shared->efc) < 0)
                        HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't release external file cache")
                break;
            }
        /* H5Freopen */
        case H5VL_FILE_REOPEN:
            {
                void   **ret        = va_arg(arguments, void **);
                H5F_t  *new_file    = NULL;

                f = (H5F_t *)obj;

                /* Reopen the file through the VOL driver */
                if(NULL == (new_file = H5F__reopen(f)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to reopen file")

                new_file->id_exists = TRUE;

                *ret = (void *)new_file;
                break;
            }
        /* H5Freset_mdc_hit_rate_stats */
        case H5VL_FILE_RESET_MDC_HIT_RATE:
            {
                f = (H5F_t *)obj;
                /* Reset the hit rate statistic */
                if(H5AC_reset_cache_hit_rate_stats(f->shared->cache) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "can't reset cache hit rate")
                break;
            }
        case H5VL_FILE_SET_MDC_CONFIG:
            {
                H5AC_cache_config_t *config_ptr = va_arg(arguments, H5AC_cache_config_t *);

                f = (H5F_t *)obj;
                /* set the resize configuration  */
                if(H5AC_set_cache_auto_resize_config(f->shared->cache, config_ptr) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "H5AC_set_cache_auto_resize_config() failed")
                break;
            }
        case H5VL_FILE_GET_METADATA_READ_RETRY_INFO:
            {
                H5F_retry_info_t *info = va_arg(arguments, H5F_retry_info_t *);

                f = (H5F_t *)obj;

                if(H5F_get_metadata_read_retry_info(f, info) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "can't get metadata read retry info")

                break;
            }
        case H5VL_FILE_START_SWMR_WRITE:
            {
                f = (H5F_t *)obj;

                if(H5F__start_swmr_write(f) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "can't start SWMR write")

                break;
            }
        case H5VL_FILE_START_MDC_LOGGING:
            {
                f = (H5F_t *)obj;

                /* Call mdc logging function */
                if(H5C_start_logging(f->shared->cache) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_LOGFAIL, FAIL, "unable to start mdc logging")

                break;
            }
        case H5VL_FILE_STOP_MDC_LOGGING:
            {
                f = (H5F_t *)obj;

                /* Call mdc logging function */
                if(H5C_stop_logging(f->shared->cache) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_LOGFAIL, FAIL, "unable to stop mdc logging")

                break;
            }
        case H5VL_FILE_GET_MDC_LOGGING_STATUS:
            {
                hbool_t *is_enabled				= va_arg(arguments, hbool_t *);
                hbool_t *is_currently_logging	= va_arg(arguments, hbool_t *);

                f = (H5F_t *)obj;

                /* Call mdc logging function */
                if(H5C_get_logging_status(f->shared->cache, is_enabled, is_currently_logging) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_LOGFAIL, FAIL, "unable to get logging status")

                break;
            }
        case H5VL_FILE_SET_LATEST_FORMAT:
            {
                HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid optional operation")
                break;
            }
        case H5VL_FILE_FORMAT_CONVERT:
            {
                f = (H5F_t *)obj;

                /* Convert the format */
                if(H5F__format_convert(f) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTCONVERT, FAIL, "can't convert file format")

                break;
            }
        case H5VL_FILE_RESET_PAGE_BUFFERING_STATS:
            {
                f = (H5F_t *)obj;

                /* Sanity check */
                if(NULL == f->shared->page_buf)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "page buffering not enabled on file")

                /* Reset the statistics */
                if (H5PB_reset_stats(f->shared->page_buf) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't reset stats for page buffering")

                break;
            }
        case H5VL_FILE_GET_PAGE_BUFFERING_STATS:
            {
                unsigned *accesses      = va_arg(arguments, unsigned *);
                unsigned *hits          = va_arg(arguments, unsigned *);
                unsigned *misses        = va_arg(arguments, unsigned *);
                unsigned *evictions     = va_arg(arguments, unsigned *);
                unsigned *bypasses      = va_arg(arguments, unsigned *);
                
                f = (H5F_t *)obj;

                /* Sanity check */
                if(NULL == f->shared->page_buf)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "page buffering not enabled on file")

                /* Get the statistics */
                if(H5PB_get_stats(f->shared->page_buf, accesses, hits, misses, evictions, bypasses) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't retrieve stats for page buffering")

                break;
            }
        case H5VL_FILE_GET_MDC_IMAGE_INFO:
            {
                HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid optional operation")
                break;
            }

        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid optional operation")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_file_close
 *
 * Purpose:     Closes a file.
 *
 * Return:      SUCCEED/FAIL (the file will not be closed on failure)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_file_close(void *file, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    int     nref;
    H5F_t   *f          = (H5F_t *)file;
    hid_t   file_id     = H5I_INVALID_HID;
    herr_t  ret_value   = SUCCEED;                  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* This routine should only be called when a file ID's ref count drops to zero */
    HDassert(H5F_ID_EXISTS(f));

    /* Flush file if this is the last reference to this id and we have write
     * intent, unless it will be flushed by the "shared" file being closed.
     * This is only necessary to replicate previous behaviour, and could be
     * disabled by an option/property to improve performance.
     */
    if((H5F_NREFS(f) > 1) && (H5F_INTENT(f) & H5F_ACC_RDWR)) {
        /* Get the file ID corresponding to the H5F_t struct */
        if(H5I_find_id(f, H5I_FILE, &file_id) < 0 || H5I_INVALID_HID == file_id)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "invalid atom")
        /* Get the number of references outstanding for this file ID */
        if((nref = H5I_get_ref(file_id, FALSE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get ID ref count")
        if(nref == 1)
            if(H5F__flush(f) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
    }

    /* Close the file */
    if(H5F__close(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "can't close file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_group_create
 *
 * Purpose:     Creates a group inside a native h5 file.
 *
 * Return:      Success:    Pointer to a group struct
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, 
                         hid_t H5_ATTR_UNUSED gapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5P_genplist_t *plist;              /* Property list pointer        */
    H5G_loc_t       loc;                /* Location to create group     */
    H5G_t          *grp = NULL;         /* New group created            */
    hid_t           lcpl_id;
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the property list structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* Get creation properties */
    if(H5P_get(plist, H5VL_PROP_GRP_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id")

    /* Set up the location */
    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    /* if name is NULL then this is from H5Gcreate_anon */
    if(name == NULL) {
        H5G_obj_create_t gcrt_info;         /* Information for group creation */

        /* Set up group creation info */
        gcrt_info.gcpl_id = gcpl_id;
        gcrt_info.cache_type = H5G_NOTHING_CACHED;
        HDmemset(&gcrt_info.cache, 0, sizeof(gcrt_info.cache));

        /* Create the new group & get its ID */
        if(NULL == (grp = H5G__create(loc.oloc->file, &gcrt_info)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to create group")            
    }
    /* otherwise it's from H5Gcreate */
    else {
        /* Create the new group & get its ID */
        if(NULL == (grp = H5G__create_named(&loc, name, lcpl_id, gcpl_id)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to create group")
    }
    ret_value = (void *)grp;

done:
    if(name == NULL) {
        /* Release the group's object header, if it was created */
        if(grp) {
            H5O_loc_t *oloc;         /* Object location for group */

            /* Get the new group's object location */
            if(NULL == (oloc = H5G_oloc(grp)))
                HDONE_ERROR(H5E_SYM, H5E_CANTGET, NULL, "unable to get object location of group")

            /* Decrement refcount on group's object header in memory */
            if(H5O_dec_rc_by_loc(oloc) < 0)
                HDONE_ERROR(H5E_SYM, H5E_CANTDEC, NULL, "unable to decrement refcount on newly created object")
         }
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_group_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_group_open
 *
 * Purpose:     Opens a group inside a native h5 file.
 *
 * Return:      Success:    Pointer to a group struct
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, 
                       hid_t H5_ATTR_UNUSED gapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5G_loc_t       loc;                /* Location to open group   */
    H5G_t          *grp = NULL;         /* New group opend          */
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Set up the location */
    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    /* Open the group */
    if((grp = H5G__open_name(&loc, name)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open group")

    ret_value = (void *)grp;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_group_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_group_get
 *
 * Purpose:     Gets data about a group
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_group_get(void *obj, H5VL_group_get_t get_type, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(get_type) {
        /* H5Gget_create_plist */
        case H5VL_GROUP_GET_GCPL:
            {
                hid_t      *new_gcpl_id     = va_arg(arguments, hid_t *);
                H5G_t      *grp             = (H5G_t *)obj;

                if((*new_gcpl_id = H5G_get_create_plist(grp)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get creation property list for group")
                break;
            }
        /* H5Gget_info */
        case H5VL_GROUP_GET_INFO:
            {
                H5VL_loc_params_t   loc_params  = va_arg(arguments, H5VL_loc_params_t);
                H5G_info_t         *group_info  = va_arg(arguments, H5G_info_t *);
                H5G_loc_t           loc;

                if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                if(loc_params.type == H5VL_OBJECT_BY_SELF) {
                    /* H5Gget_info */

                    /* Retrieve the group's information */
                    if(H5G__obj_info(loc.oloc, group_info) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info")
                }
                else if (loc_params.type == H5VL_OBJECT_BY_NAME) {
                    /* H5Gget_info_by_name */

                    /* Retrieve the group's information */
                    if(H5G__get_info_by_name(&loc, loc_params.loc_data.loc_by_name.name, group_info) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_IDX) {
                    /* H5Gget_info_by_idx */

                    /* Retrieve the group's information */
                    if(H5G__get_info_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, loc_params.loc_data.loc_by_idx.idx_type,
                            loc_params.loc_data.loc_by_idx.order, loc_params.loc_data.loc_by_idx.n, group_info) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info")
                }
                else {
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown get info parameters")
                }
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from group")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_group_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_group_specific
 *
 * Purpose:     Specific operations for groups
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_group_specific(void *obj, H5VL_group_specific_t specific_type, 
                             hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5G_t       *grp = (H5G_t *)obj;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(specific_type) {
        case H5VL_GROUP_FLUSH:
            {
                hid_t group_id = va_arg(arguments, hid_t);

                /* Flush object's metadata to file */
                if(H5O_flush_common(&grp->oloc, group_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTFLUSH, FAIL, "unable to flush group")

                break;
            }
        case H5VL_GROUP_REFRESH:
            {
                hid_t group_id = va_arg(arguments, hid_t);

                /* Call private function to refresh group object */
                if((H5O_refresh_metadata(group_id, grp->oloc)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, FAIL, "unable to refresh group")

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid specific operation")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_group_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_group_close
 *
 * Purpose:     Closes a group.
 *
 * Return:      SUCCEED/FAIL (the group will not be closed on failure)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_group_close(void *grp, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_close((H5G_t *)grp) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close group")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_group_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_link_create
 *
 * Purpose:	Creates an hard/soft/UD/external links.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_link_create(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params,
                        hid_t lcpl_id, hid_t H5_ATTR_UNUSED lapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5P_genplist_t   *plist;                     /* Property list pointer */
    herr_t           ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    switch(create_type) {
        case H5VL_LINK_CREATE_HARD:
            {
                H5G_loc_t    cur_loc;
                H5G_loc_t    link_loc;
                void         *cur_obj;
                H5VL_loc_params_t cur_params;

                if(H5P_get(plist, H5VL_PROP_LINK_TARGET, &cur_obj) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current location id")
                if(H5P_get(plist, H5VL_PROP_LINK_TARGET_LOC_PARAMS, &cur_params) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current name")

                if(NULL != cur_obj && H5G_loc_real(cur_obj, cur_params.obj_type, &cur_loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
                if(NULL != obj && H5G_loc_real(obj, loc_params.obj_type, &link_loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                /* H5Lcreate_hard */
                if(H5VL_OBJECT_BY_NAME == cur_params.type) {
                    H5G_loc_t *cur_loc_p, *link_loc_p;

                    /* Set up current & new location pointers */
                    cur_loc_p = &cur_loc;
                    link_loc_p = &link_loc;
                    if(NULL == cur_obj)
                        cur_loc_p = link_loc_p;
                    else if(NULL == obj)
                        link_loc_p = cur_loc_p;
                    else if(cur_loc_p->oloc->file != link_loc_p->oloc->file)
                        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should be in the same file.")

                    /* Create the link */
                    if((ret_value = H5L_create_hard(cur_loc_p, cur_params.loc_data.loc_by_name.name, 
                                                    link_loc_p, loc_params.loc_data.loc_by_name.name, lcpl_id)) < 0)
                        HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")
                }
                else { /* H5Olink */
                    /* Link to the object */
                    if(H5L_link(&link_loc, loc_params.loc_data.loc_by_name.name, &cur_loc, lcpl_id) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to create link")
                }
                break;
            }
        case H5VL_LINK_CREATE_SOFT:
            {
                char        *target_name;
                H5G_loc_t   link_loc;               /* Group location for new link */

                if(H5G_loc_real(obj, loc_params.obj_type, &link_loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                if(H5P_get(plist, H5VL_PROP_LINK_TARGET_NAME, &target_name) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for targe name")

                /* Create the link */
                if((ret_value = H5L_create_soft(target_name, &link_loc, loc_params.loc_data.loc_by_name.name, lcpl_id)) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")
                break;
            }
        case H5VL_LINK_CREATE_UD:
            {
                H5G_loc_t   link_loc;               /* Group location for new link */
                H5L_type_t link_type;
                void *udata;
                size_t udata_size;

                if(H5G_loc_real(obj, loc_params.obj_type, &link_loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                if(H5P_get(plist, H5VL_PROP_LINK_TYPE, &link_type) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for link type")
                if(H5P_get(plist, H5VL_PROP_LINK_UDATA, &udata) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for udata")
                if(H5P_get(plist, H5VL_PROP_LINK_UDATA_SIZE, &udata_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for udata size")

                /* Create link */
                if(H5L__create_ud(&link_loc, loc_params.loc_data.loc_by_name.name, udata, udata_size, 
                                 link_type, lcpl_id) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")
                break;
            }
        default:
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "invalid link creation call")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_link_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_link_copy
 *
 * Purpose:	Renames an object within an HDF5 file and copies it to a new
 *              group.  The original name SRC is unlinked from the group graph
 *              and then inserted with the new name DST (which can specify a
 *              new path for the object) as an atomic operation. The names
 *              are interpreted relative to SRC_LOC_ID and
 *              DST_LOC_ID, which are either file IDs or group ID.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_link_copy(void *src_obj, H5VL_loc_params_t loc_params1, 
                      void *dst_obj, H5VL_loc_params_t loc_params2,
                      hid_t lcpl_id, hid_t H5_ATTR_UNUSED lapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5G_loc_t	src_loc, *src_loc_p;
    H5G_loc_t	dst_loc, *dst_loc_p;
    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL != src_obj && H5G_loc_real(src_obj, loc_params1.obj_type, &src_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    if(NULL != dst_obj && H5G_loc_real(dst_obj, loc_params2.obj_type, &dst_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    /* Set up src & dst location pointers */
    src_loc_p = &src_loc;
    dst_loc_p = &dst_loc;
    if(NULL == src_obj)
        src_loc_p = dst_loc_p;
    else if(NULL == dst_obj)
        dst_loc_p = src_loc_p;

    /* Copy the link */
    if(H5L_move(src_loc_p, loc_params1.loc_data.loc_by_name.name, 
                dst_loc_p, loc_params2.loc_data.loc_by_name.name, 
                TRUE, lcpl_id) < 0)
        HGOTO_ERROR(H5E_LINK, H5E_CANTCOPY, FAIL, "unable to copy link")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_link_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_link_move
 *
 * Purpose:	Renames an object within an HDF5 file and moves it to a new
 *              group.  The original name SRC is unlinked from the group graph
 *              and then inserted with the new name DST (which can specify a
 *              new path for the object) as an atomic operation. The names
 *              are interpreted relative to SRC_LOC_ID and
 *              DST_LOC_ID, which are either file IDs or group ID.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_link_move(void *src_obj, H5VL_loc_params_t loc_params1, 
                      void *dst_obj, H5VL_loc_params_t loc_params2,
                      hid_t lcpl_id, hid_t H5_ATTR_UNUSED lapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5G_loc_t	src_loc, *src_loc_p;
    H5G_loc_t	dst_loc, *dst_loc_p;
    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL != src_obj && H5G_loc_real(src_obj, loc_params1.obj_type, &src_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    if(NULL != dst_obj && H5G_loc_real(dst_obj, loc_params2.obj_type, &dst_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    /* Set up src & dst location pointers */
    src_loc_p = &src_loc;
    dst_loc_p = &dst_loc;
    if(NULL == src_obj)
        src_loc_p = dst_loc_p;
    else if(NULL == dst_obj)
        dst_loc_p = src_loc_p;

    /* Move the link */
    if(H5L_move(src_loc_p, loc_params1.loc_data.loc_by_name.name, 
                dst_loc_p, loc_params2.loc_data.loc_by_name.name, 
                FALSE, lcpl_id) < 0)
        HGOTO_ERROR(H5E_LINK, H5E_CANTMOVE, FAIL, "unable to move link")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_link_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_link_get
 *
 * Purpose:	Gets certain data about a link
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, 
                     hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5G_loc_t	loc;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    switch(get_type) {
        /* H5Lget_info/H5Lget_info_by_idx */
        case H5VL_LINK_GET_INFO:
            {
                H5L_info_t *linfo  = va_arg(arguments, H5L_info_t *);

                /* Get the link information */
                if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Lget_info */
                    if(H5L_get_info(&loc, loc_params.loc_data.loc_by_name.name, linfo) < 0)
                        HGOTO_ERROR(H5E_LINK, H5E_NOTFOUND, FAIL, "unable to get link info")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Lget_info_by_idx */

                    if(H5L_get_info_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, loc_params.loc_data.loc_by_idx.idx_type,
                            loc_params.loc_data.loc_by_idx.order, loc_params.loc_data.loc_by_idx.n, linfo) < 0)
                        HGOTO_ERROR(H5E_LINK, H5E_NOTFOUND, FAIL, "unable to get link info")
                }
                else
                    HGOTO_ERROR(H5E_LINK, H5E_NOTFOUND, FAIL, "unable to get link info")
                break;
            }
        /* H5Lget_name_by_idx */
        case H5VL_LINK_GET_NAME:
            {
                char       *name   = va_arg(arguments, char *);
                size_t      size   = va_arg(arguments, size_t);
                ssize_t    *ret    = va_arg(arguments, ssize_t *);

                /* Get the link name */
                if((*ret = H5L_get_name_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, loc_params.loc_data.loc_by_idx.idx_type,
                            loc_params.loc_data.loc_by_idx.order, loc_params.loc_data.loc_by_idx.n, name, size)) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_NOTFOUND, FAIL, "unable to get link info")

                break;
            }
        /* H5Lget_val/H5Lget_val_by_idx */
        case H5VL_LINK_GET_VAL:
            {
                void       *buf    = va_arg(arguments, void *);
                size_t     size    = va_arg(arguments, size_t);

                /* Get the link information */
                if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Lget_val */
                    if(H5L_get_val(&loc, loc_params.loc_data.loc_by_name.name, buf, size) < 0)
                        HGOTO_ERROR(H5E_LINK, H5E_NOTFOUND, FAIL, "unable to get link value")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Lget_val_by_idx */

                    if(H5L_get_val_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, loc_params.loc_data.loc_by_idx.idx_type,
                            loc_params.loc_data.loc_by_idx.order, loc_params.loc_data.loc_by_idx.n, buf, size) < 0)
                        HGOTO_ERROR(H5E_LINK, H5E_NOTFOUND, FAIL, "unable to get link val")                    
                }
                else
                    HGOTO_ERROR(H5E_LINK, H5E_NOTFOUND, FAIL, "unable to get link val")

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from link")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_link_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_link_specific
 *
 * Purpose:	Specific operations with links
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_link_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_link_specific_t specific_type, 
                          hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(specific_type) {
        case H5VL_LINK_EXISTS:
            {
                htri_t *ret = va_arg(arguments, htri_t *);
                H5G_loc_t loc;

                if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                /* Check for the existence of the link */
                if((*ret = H5L_exists(&loc, loc_params.loc_data.loc_by_name.name)) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_NOTFOUND, FAIL, "unable to specific link info")
                break;
            }
        case H5VL_LINK_ITER:
            {
                H5G_loc_t loc;
                hbool_t recursive = va_arg(arguments, int);                     
                H5_index_t idx_type = va_arg(arguments, H5_index_t);
                H5_iter_order_t order = va_arg(arguments, H5_iter_order_t);
                hsize_t *idx_p = va_arg(arguments, hsize_t *);
                H5L_iterate_t op = va_arg(arguments, H5L_iterate_t);
                void *op_data = va_arg(arguments, void *);

                /* Get the location */
                if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

                /* Visit or iterate over the links */
                if(loc_params.type == H5VL_OBJECT_BY_SELF) {
                    if(recursive) {
                        /* H5Lvisit */
                        if((ret_value = H5G_visit(&loc, ".", idx_type, order, op, op_data)) < 0)
                            HGOTO_ERROR(H5E_LINK, H5E_BADITER, FAIL, "link visitation failed")
                    }
                    else {
                        /* H5Literate */
                        if((ret_value = H5L_iterate(&loc, ".", idx_type, order, idx_p, op, op_data)) < 0)
                            HGOTO_ERROR(H5E_LINK, H5E_BADITER, FAIL, "error iterating over links")
                    }
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) {
                    if(recursive) {
                        /* H5Lvisit_by_name */
                        if((ret_value = H5G_visit(&loc, loc_params.loc_data.loc_by_name.name, idx_type, order, op, op_data)) < 0)
                            HGOTO_ERROR(H5E_LINK, H5E_BADITER, FAIL, "link visitation failed")
                    }
                    else {
                        /* H5Literate_by_name */
                        if((ret_value = H5L_iterate(&loc, loc_params.loc_data.loc_by_name.name, idx_type, order, idx_p, op, op_data)) < 0)
                            HGOTO_ERROR(H5E_LINK, H5E_BADITER, FAIL, "error iterating over links")
                    }
                }
                else{
                    HGOTO_ERROR(H5E_LINK, H5E_UNSUPPORTED, FAIL, "unknown link iterate params")
                }

                break;
            }
        case H5VL_LINK_DELETE:
            {
                H5G_loc_t loc;

                if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                /* Unlink */
                if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Ldelete */
                    if(H5L_delete(&loc, loc_params.loc_data.loc_by_name.name) < 0)
                        HGOTO_ERROR(H5E_LINK, H5E_CANTDELETE, FAIL, "unable to delete link")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Ldelete_by_idx */

                    if(H5L_delete_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, loc_params.loc_data.loc_by_idx.idx_type,
                            loc_params.loc_data.loc_by_idx.order, loc_params.loc_data.loc_by_idx.n) < 0)
                        HGOTO_ERROR(H5E_LINK, H5E_CANTDELETE, FAIL, "unable to delete link")
                }
                else
                    HGOTO_ERROR(H5E_LINK, H5E_CANTDELETE, FAIL, "unable to delete link")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid specific operation")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_link_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_open
 *
 * Purpose:	Opens a object inside a native h5 file.
 *
 * Return:	Success:	object id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, 
                        hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5G_loc_t   loc;
    void       *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    switch(loc_params.type) {
        case H5VL_OBJECT_BY_NAME:
            {
                /* Open the object */
                if(NULL == (ret_value = H5O_open_name(&loc, loc_params.loc_data.loc_by_name.name, opened_type)))
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, NULL, "unable to open object by name")
                break;
            }
        case H5VL_OBJECT_BY_IDX:
            {
                /* Open the object */
                if(NULL == (ret_value = H5O_open_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, loc_params.loc_data.loc_by_idx.idx_type,
                        loc_params.loc_data.loc_by_idx.order, loc_params.loc_data.loc_by_idx.n, opened_type)))
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, NULL, "unable to open object by index")
                break;
            }
        case H5VL_OBJECT_BY_ADDR:
            {
                /* Open the object */
                if(NULL == (ret_value = H5O_open_by_addr(&loc, loc_params.loc_data.loc_by_addr.addr, opened_type)))
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, NULL, "unable to open object by address")
                break;
            }
        case H5VL_OBJECT_BY_REF:
            {
                hid_t temp_id = H5I_INVALID_HID;
                H5F_t *file = NULL;

                /* Get the file pointer from the entry */
                file = loc.oloc->file;

                /* Create reference */
                if((temp_id = H5R__dereference(file, loc_params.loc_data.loc_by_ref.lapl_id, 
                                              loc_params.loc_data.loc_by_ref.ref_type, 
                                              loc_params.loc_data.loc_by_ref._ref)) < 0)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_CANTOPENOBJ, NULL, "unable to dereference object")

                *opened_type = H5I_get_type(temp_id);
                if(NULL == (ret_value = H5I_remove(temp_id)))
                    HDONE_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open object")
                break;
            }
        case H5VL_OBJECT_BY_SELF:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "unknown open parameters")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_copy
 *
 * Purpose:	Copys a object inside a native h5 file.
 *
 * Return:	Success:	object id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL_native_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name, 
                        void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name, 
                        hid_t ocpypl_id, hid_t lcpl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5G_loc_t	src_loc;                /* Source object group location */
    H5G_loc_t	dst_loc;                /* Destination group location */
    herr_t      ret_value = FAIL;
    
    FUNC_ENTER_NOAPI_NOINIT

    /* get location for objects */
    if(H5G_loc_real(src_obj, loc_params1.obj_type, &src_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    if(H5G_loc_real(dst_obj, loc_params2.obj_type, &dst_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    /* Open the object */
    if((ret_value = H5O_copy(&src_loc, src_name, &dst_loc, dst_name, ocpypl_id, lcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, FAIL, "unable to copy object")    
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_object_get
 *
 * Purpose:     Gets certain data about an object
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, 
                       hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */
    H5G_loc_t	loc;                    /* Location of group */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    switch(get_type) {
        /* H5Rget_region */
        case H5VL_REF_GET_REGION:
            {
                hid_t       *ret                    =  va_arg(arguments, hid_t *);
                H5R_type_t  H5_ATTR_UNUSED ref_type =  va_arg(arguments, H5R_type_t);
                void        *ref                    =  va_arg(arguments, void *);
                H5S_t       *space = NULL;    /* Dataspace object */

                /* Get the dataspace with the correct region selected */
                if((space = H5R__get_region(loc.oloc->file, ref)) == NULL)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, FAIL, "unable to retrieve region")

                /* Atomize */
                if((*ret = H5I_register(H5I_DATASPACE, space, TRUE)) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")

                break;
            }
        /* H5Rget_obj_type2 */
        case H5VL_REF_GET_TYPE:
            {
                H5O_type_t  *obj_type  =  va_arg(arguments, H5O_type_t *);
                H5R_type_t  ref_type   =  va_arg(arguments, H5R_type_t);
                void        *ref       =  va_arg(arguments, void *);

                /* Get the object information */
                if(H5R__get_obj_type(loc.oloc->file, ref_type, ref, obj_type) < 0)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, FAIL, "unable to determine object type")
                break;
            }
        /* H5Rget_name */
        case H5VL_REF_GET_NAME:
            {
                ssize_t     *ret       = va_arg(arguments, ssize_t *);
                char        *name      = va_arg(arguments, char *);
                size_t      size       = va_arg(arguments, size_t);
                H5R_type_t  ref_type   = va_arg(arguments, H5R_type_t);
                void        *ref       = va_arg(arguments, void *);

                /* Get name */
                if((*ret = H5R__get_name(loc.oloc->file, ref_type, ref, name, size)) < 0)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, FAIL, "unable to determine object path")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from object")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_object_specific
 *
 * Purpose:     Perform a driver specific operation for an objectibute
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_object_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_object_specific_t specific_type, 
                        hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5G_loc_t    loc;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    switch(specific_type) {
        /* H5Oincr_refcount / H5Odecr_refcount */
        case H5VL_OBJECT_CHANGE_REF_COUNT:
            {
                int update_ref  = va_arg(arguments, int);
                H5O_loc_t  *oloc = loc.oloc;

                if(H5O_link(oloc, update_ref) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "modifying object link count failed")

                break;
            }
        /* H5Oexists_by_name */
        case H5VL_OBJECT_EXISTS:
            {
                htri_t *ret = va_arg(arguments, htri_t *);

                if(loc_params.type == H5VL_OBJECT_BY_NAME) {
                    /* Check if the object exists */
                    if((*ret = H5G_loc_exists(&loc, loc_params.loc_data.loc_by_name.name)) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "unable to determine if '%s' exists", 
                                    loc_params.loc_data.loc_by_name.name)
                }
                else {
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown object exists parameters")
                }
                break;
            }
        case H5VL_OBJECT_VISIT:
            {
                H5_index_t idx_type     = va_arg(arguments, H5_index_t);
                H5_iter_order_t order   = va_arg(arguments, H5_iter_order_t);
                H5O_iterate_t op        = va_arg(arguments, H5O_iterate_t);
                void *op_data           = va_arg(arguments, void *);
                unsigned fields         = va_arg(arguments, unsigned);

                /* Call internal object visitation routine */
                if(loc_params.type == H5VL_OBJECT_BY_SELF) {
                    /* H5Ovisit */
                    if((ret_value = H5O__visit(&loc, ".", idx_type, order, op, op_data, fields)) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_BADITER, FAIL, "object visitation failed")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) {
                    /* H5Ovisit_by_name */
                    if((ret_value = H5O__visit(&loc, loc_params.loc_data.loc_by_name.name, idx_type, order, op, op_data, fields)) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_BADITER, FAIL, "object visitation failed")
                }
                else
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown object visit params");
                break;
            }
        case H5VL_OBJECT_FLUSH:
            {
                hid_t   oid     = va_arg(arguments, hid_t);

                /* Flush the object's metadata */
                if(H5O_flush(loc.oloc, oid) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTFLUSH, FAIL, "unable to flush object")

                break;
            }
        case H5VL_OBJECT_REFRESH:
            {
                hid_t                   oid         = va_arg(arguments, hid_t);
                H5O_loc_t              *oloc        = loc.oloc;

                /* Refresh the metadata */
                if(H5O_refresh_metadata(oid, *oloc) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to refresh object")

                break;
            }
        case H5VL_REF_CREATE:
            {
                void        *ref      = va_arg(arguments, void *);
                const char  *name     = va_arg(arguments, char *);
                H5R_type_t  ref_type  = va_arg(arguments, H5R_type_t);
                hid_t       space_id  = va_arg(arguments, hid_t);
                H5S_t       *space = NULL;   /* Pointer to dataspace containing region */
                
                if(space_id != (-1) && (NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE))))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

                /* Create reference */
                if(H5R__create(ref, &loc, name, ref_type, space) < 0)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCREATE, FAIL, "unable to create reference")

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't recognize this operation type")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_optional
 *
 * Purpose:	Perform a driver specific operation for an objectibute
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_object_optional(void *obj, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_object_optional_t optional_type = va_arg(arguments, H5VL_object_optional_t);
    H5VL_loc_params_t loc_params = va_arg(arguments, H5VL_loc_params_t);
    H5G_loc_t	loc;                    /* Location of group */
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    switch(optional_type) {
        /* H5Oget_info / H5Oget_info_by_name / H5Oget_info_by_idx */
        case H5VL_OBJECT_GET_INFO:
            {
                H5O_info_t  *obj_info = va_arg(arguments, H5O_info_t *);
                unsigned fields         = va_arg(arguments, unsigned);

                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Oget_info */
                    /* Retrieve the object's information */
                    if(H5G_loc_info(&loc, ".", obj_info, fields) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "object not found")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Oget_info_by_name */
                    /* Retrieve the object's information */
                    if(H5G_loc_info(&loc, loc_params.loc_data.loc_by_name.name, obj_info, fields) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "object not found")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Oget_info_by_idx */
                    H5G_loc_t   obj_loc;                /* Location used to open group */
                    H5G_name_t  obj_path;            	/* Opened object group hier. path */
                    H5O_loc_t   obj_oloc;            	/* Opened object object location */

                    /* Set up opened group location to fill in */
                    obj_loc.oloc = &obj_oloc;
                    obj_loc.path = &obj_path;
                    H5G_loc_reset(&obj_loc);

                    /* Find the object's location, according to the order in the index */
                    if(H5G_loc_find_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, 
                                           loc_params.loc_data.loc_by_idx.idx_type, 
                                           loc_params.loc_data.loc_by_idx.order, 
                                           loc_params.loc_data.loc_by_idx.n, &obj_loc/*out*/) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "group not found")

                    /* Retrieve the object's information */
                    if(H5O_get_info(obj_loc.oloc, obj_info, fields) < 0) {
                        H5G_loc_free(&obj_loc);
                        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't retrieve object info")
                    }

                    /* Release the object location */
                    if(H5G_loc_free(&obj_loc) < 0)
                        HDONE_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "can't free location")
                }
                else {
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown get info parameters")
                }
                break;
            }
        /* H5Oget_comment / H5Oget_comment_by_name */
        case H5VL_OBJECT_GET_COMMENT:
            {
                char     *comment =  va_arg(arguments, char *);
                size_t   bufsize  =  va_arg(arguments, size_t);
                ssize_t  *ret     =  va_arg(arguments, ssize_t *);

                /* Retrieve the object's comment */
                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Oget_comment */
                    if((*ret = H5G_loc_get_comment(&loc, ".", comment/*out*/, bufsize)) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "object not found")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Oget_comment_by_name */
                    if((*ret = H5G_loc_get_comment(&loc, loc_params.loc_data.loc_by_name.name, comment/*out*/, bufsize)) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "object not found")
                }
                else {
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown set_coment parameters")
                }
                break;
            }
        /* H5Oset_comment */
        case H5VL_OBJECT_SET_COMMENT:
            {
                const char    *comment  = va_arg(arguments, char *);

                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Oset_comment */
                    /* (Re)set the object's comment */
                    if(H5G_loc_set_comment(&loc, ".", comment) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "object not found")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Oset_comment_by_name */
                    /* (Re)set the object's comment */
                    if(H5G_loc_set_comment(&loc, loc_params.loc_data.loc_by_name.name, comment) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "object not found")
                }
                else {
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown set_coment parameters")
                }
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't perform this operation on object");       
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_datatype_commit
 *
 * Purpose:	Commits a datatype inside a native h5 file.
 *
 * Return:	Success:	datatype id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, 
                            hid_t lcpl_id, hid_t tcpl_id, hid_t H5_ATTR_UNUSED tapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5G_loc_t	loc;                    /* Location to commit datatype */
    H5T_t	*dt;                    /* Datatype for ID */
    H5T_t	*type = NULL;           /* copy of the original type which will be committed */
    void        *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a datatype")

    /* Check arguments.  We cannot commit an immutable type because H5Tclose()
     * normally fails on such types (try H5Tclose(H5T_NATIVE_INT)) but closing
     * a named type should always succeed.
     */
    if(H5T_STATE_NAMED == dt->shared->state || H5T_STATE_OPEN == dt->shared->state)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "datatype is already committed")
    if(H5T_STATE_IMMUTABLE == dt->shared->state)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "datatype is immutable")

    /* Check for a "sensible" datatype to store on disk */
    if(H5T_is_sensible(dt) <= 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "datatype is not sensible")

    /* Copy the datatype - the copied one will be the type that is
     * committed, and attached to original datatype above the VOL
     * layer
     */
    if(NULL == (type = H5T_copy(dt, H5T_COPY_TRANSIENT)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to copy");

    /* Commit the datatype */
    if(NULL != name) {
        /* H5Tcommit */
        if(H5T__commit_named(&loc, name, type, lcpl_id, tcpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to commit datatype")
    }
    else {
        /* H5Tcommit_anon */
        if(H5T__commit_anon(loc.oloc->file, type, tcpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to commit datatype")
    }
    ret_value = (void *)type;

done:
    if(NULL == ret_value && type)
        H5T_close(type);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_datatype_commit() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_datatype_open
 *
 * Purpose:     Opens a named datatype inside a native h5 file.
 *
 * Return:      Success:    datatype pointer
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, 
                          hid_t H5_ATTR_UNUSED tapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5T_t       *type = NULL;           /* Datatype opened in file */
    H5G_loc_t    loc;                   /* Group location of object to open */
    void        *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    /* Open the datatype */
    if(NULL == (type = H5T__open_name(&loc, name)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTOPENOBJ, NULL, "unable to open named datatype")

    type->vol_obj = NULL;

    ret_value = (void *)type;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_datatype_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_datatype_get
 *
 * Purpose:	Gets certain information about a datatype
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_datatype_get(void *obj, H5VL_datatype_get_t get_type, 
                         hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5T_t       *dt = (H5T_t *)obj;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        case H5VL_DATATYPE_GET_BINARY:
            {
                ssize_t *nalloc = va_arg(arguments, ssize_t *);
                void *buf = va_arg(arguments, void *);
                size_t size = va_arg(arguments, size_t);

                if(H5T_encode(dt, (unsigned char *)buf, &size) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't determine serialized length of datatype")

                *nalloc = (ssize_t) size;
                break;
            }
        /* H5Tget_create_plist */
        case H5VL_DATATYPE_GET_TCPL:
            {
                hid_t *ret_id = va_arg(arguments, hid_t *);

                if(H5I_INVALID_HID == (*ret_id = H5T__get_create_plist(dt)))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get object creation info");

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from datatype")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_datatype_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_native_datatype_specific
 *
 * Purpose:     Specific operations for datatype
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_datatype_specific(void *obj, H5VL_datatype_specific_t specific_type, 
                             hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5T_t       *dt = (H5T_t *)obj;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(specific_type) {
        case H5VL_DATATYPE_FLUSH:
            {
                hid_t type_id = va_arg(arguments, hid_t);

                /* To flush metadata and invoke flush callback if there is */
                if (H5O_flush_common(&dt->oloc, type_id) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFLUSH, FAIL, "unable to flush datatype")

                break;
            }
        case H5VL_DATATYPE_REFRESH:
            {
                hid_t type_id = va_arg(arguments, hid_t);

                /* Call private function to refresh datatype object */
                if ((H5O_refresh_metadata(type_id, dt->oloc)) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTLOAD, FAIL, "unable to refresh datatype")

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid specific operation")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_datatype_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_datatype_close
 *
 * Purpose:	Closes an datatype.
 *
 * Return:	Success:	0
 *		Failure:	-1, datatype not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_datatype_close(void *dt, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5T_close((H5T_t*)dt) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close datatype")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_datatype_close() */

