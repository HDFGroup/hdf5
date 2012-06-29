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

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              January, 2012
 *
 * Purpose:	The native VOL plugin where access is to a single HDF5 file 
 *              using HDF5 VFDs. 
 */

#define H5A_PACKAGE		/*suppress error about including H5Apkg	  */
#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */
#define H5L_PACKAGE		/*suppress error about including H5Lpkg   */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */
#define H5R_PACKAGE		/*suppress error about including H5Rpkg	  */
#define H5T_PACKAGE		/*suppress error about including H5Tpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_native_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"             /* Attribute pkg                        */
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5Dpkg.h"             /* Dataset pkg                          */
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5Fpkg.h"             /* File pkg                             */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HGprivate.h"	/* Global Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"         /* links                                */
#include "H5Lpkg.h"             /* links headers			*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Rpkg.h"		/* References   			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/
#include "H5Tpkg.h"		/* Datatypes				*/
#include "H5Tprivate.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLnative.h"         /* Native VOL plugin			*/

/* The driver identification number, initialized at runtime */
static hid_t H5VL_NATIVE_g = 0;

/* Prototypes */
static H5F_t *H5VL_native_get_file(void *obj, H5I_type_t type);
static herr_t H5VL_native_get_loc(void *obj, H5I_type_t type, H5G_loc_t *loc);
static herr_t H5VL_native_term(void);

/* Atrribute callbacks */
static void *H5VL_native_attr_create(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t req);
static void *H5VL_native_attr_open(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t aapl_id, hid_t req);
static herr_t H5VL_native_attr_read(void *attr, hid_t dtype_id, void *buf, hid_t req);
static herr_t H5VL_native_attr_write(void *attr, hid_t dtype_id, const void *buf, hid_t req);
static herr_t H5VL_native_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_native_attr_remove(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t req);
static herr_t H5VL_native_attr_close(void *attr, hid_t req);

/* Datatype callbacks */
static void *H5VL_native_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t req);
static void *H5VL_native_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id, hid_t req);
static ssize_t H5VL_native_datatype_get_binary(void *obj, unsigned char *buf, size_t size, hid_t req);
static herr_t H5VL_native_datatype_close(void *dt, hid_t req);

/* Dataset callbacks */
static void *H5VL_native_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req);
static void *H5VL_native_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t req);
static herr_t H5VL_native_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                       hid_t file_space_id, hid_t plist_id, void *buf, hid_t req);
static herr_t H5VL_native_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                        hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req);
static herr_t H5VL_native_dataset_set_extent(void *dset, const hsize_t size[], hid_t req);
static herr_t H5VL_native_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_native_dataset_close(void *dset, hid_t req);

/* File callbacks */
static void *H5VL_native_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req);
static void *H5VL_native_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t req);
static herr_t H5VL_native_file_flush(void *obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, hid_t req);
static herr_t H5VL_native_file_get(void *file, H5VL_file_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_native_file_misc(void *file, H5VL_file_misc_t misc_type, hid_t req, va_list arguments);
static herr_t H5VL_native_file_optional(void *file, H5VL_file_optional_t optional_type, hid_t req, va_list arguments);
static herr_t H5VL_native_file_close(void *file, hid_t req);

/* Group callbacks */
static void *H5VL_native_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t req);
static void *H5VL_native_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t req);
static herr_t H5VL_native_group_get(void *obj, H5VL_group_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_native_group_close(void *grp, hid_t req);

/* Link callbacks */
static herr_t H5VL_native_link_create(H5VL_link_create_type_t create_type, void *obj, 
                                      H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t req);
static herr_t H5VL_native_link_move(void *src_obj, H5VL_loc_params_t loc_params1,
                                    void *dst_obj, H5VL_loc_params_t loc_params2,
                                    hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t req);
static herr_t H5VL_native_link_iterate(void *obj, H5VL_loc_params_t loc_params, hbool_t recursive, 
                                       H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, 
                                       H5L_iterate_t op, void *op_data, hid_t req);
static herr_t H5VL_native_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_native_link_remove(void *obj, H5VL_loc_params_t loc_params, hid_t req);

/* Object callbacks */
static void *H5VL_native_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, hid_t req);
static herr_t H5VL_native_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name, 
                                      void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name, 
                                      hid_t ocpypl_id, hid_t lcpl_id, hid_t req);
static herr_t H5VL_native_object_visit(void *obj, H5VL_loc_params_t loc_params, H5_index_t idx_type, 
                                       H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t req);
//static herr_t H5VL_native_object_lookup(hid_t loc_id, H5VL_loc_type_t lookup_type, void **location, hid_t req, va_list arguments);
//static herr_t H5VL_native_object_free_loc(void *location, hid_t req);
static herr_t H5VL_native_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_native_object_misc(void *obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, hid_t req, va_list arguments);
static herr_t H5VL_native_object_optional(void *obj, H5VL_loc_params_t loc_params, H5VL_object_optional_t optional_type, hid_t req, va_list arguments);
static herr_t H5VL_native_object_close(void *obj, H5VL_loc_params_t loc_params, hid_t req);

static H5VL_class_t H5VL_native_g = {
    "native",					/* name */
    NULL,                           /* initialize */
    H5VL_native_term,                           /* terminate */
    {                                           /* attribute_cls */
        H5VL_native_attr_create,                /* create */
        H5VL_native_attr_open,                  /* open */
        H5VL_native_attr_read,                  /* read */
        H5VL_native_attr_write,                 /* write */
        H5VL_native_attr_get,                   /* get */
        H5VL_native_attr_remove,                /* remove */
        H5VL_native_attr_close                  /* close */
    },
    {                                           /* datatype_cls */
        H5VL_native_datatype_commit,            /* commit */
        H5VL_native_datatype_open,              /* open */
        H5VL_native_datatype_get_binary,          /* get_size */
        H5VL_native_datatype_close              /* close */
    },
    {                                           /* dataset_cls */
        H5VL_native_dataset_create,             /* create */
        H5VL_native_dataset_open,               /* open */
        H5VL_native_dataset_read,               /* read */
        H5VL_native_dataset_write,              /* write */
        H5VL_native_dataset_set_extent,         /* set extent */
        H5VL_native_dataset_get,                /* get */
        H5VL_native_dataset_close               /* close */
    },
    {                                           /* file_cls */
        H5VL_native_file_create,                /* create */
        H5VL_native_file_open,                  /* open */
        H5VL_native_file_flush,                 /* flush */
        H5VL_native_file_get,                   /* get */
        H5VL_native_file_misc,                  /* misc */
        H5VL_native_file_optional,              /* optional */
        H5VL_native_file_close                  /* close */
    },
    {                                           /* group_cls */
        H5VL_native_group_create,               /* create */
        H5VL_native_group_open,                 /* open */
        H5VL_native_group_get,                  /* get */
        H5VL_native_group_close                 /* close */
    },
    {                                           /* link_cls */
        H5VL_native_link_create,                /* create */
        H5VL_native_link_move,                  /* move */
        H5VL_native_link_iterate,               /* iterate */
        H5VL_native_link_get,                   /* get */
        H5VL_native_link_remove                 /* remove */
    },
    {                                           /* object_cls */
        H5VL_native_object_open,                /* open */
        H5VL_native_object_copy,                /* copy */
        H5VL_native_object_visit,               /* visit */
        //H5VL_native_object_lookup,              /* lookup */
        //H5VL_native_object_free_loc,            /* free location */
        H5VL_native_object_get,                 /* get */
        H5VL_native_object_misc,                /* misc */
        H5VL_native_object_optional,            /* optional */
        H5VL_native_object_close                /* close */
    }
};


/*--------------------------------------------------------------------------
NAME
   H5VL_native_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5VL_native_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_native_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL_native_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5VL_native_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_init
 *
 * Purpose:	Initialize this vol plugin by registering the driver with the
 *		library.
 *
 * Return:	Success:	The ID for the native plugin.
 *		Failure:	Negative.
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
H5VL_class_t *
H5VL_native_init(void)
{
    H5VL_class_t *ret_value = NULL;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set return value */
    ret_value = &H5VL_native_g;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_init() */


/*---------------------------------------------------------------------------
 * Function:	H5VL_native_term
 *
 * Purpose:	Shut down the VOL plugin
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5VL_native_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Reset VOL ID */
    H5VL_NATIVE_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_native_term() */


/*---------------------------------------------------------------------------
 * Function:	H5VL_native_get_loc
 *
 * Purpose:	utility routine to get object location
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2012
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5VL_native_get_loc(void *obj, H5I_type_t type, H5G_loc_t *loc)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    switch(type) {
        case H5I_FILE:
            /* Construct a group location for root group of the file */
            if(H5G_root_loc((H5F_t *)obj, loc) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unable to create location for file")
            break;
        case H5I_GROUP:
            if(NULL == (loc->oloc = H5G_oloc((H5G_t *)obj)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location of group")
            if(NULL == (loc->path = H5G_nameof((H5G_t *)obj)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path of group")
            break;
        case H5I_DATATYPE:
            if(NULL == (loc->oloc = H5T_oloc((H5T_t *)obj)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location of datatype")
            if(NULL == (loc->path = H5T_nameof((H5T_t *)obj)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path of datatype")
#if 0
                H5T_t *tmp_dt = (H5T_t *)obj;
                H5T_t *dt = (H5T_t *)(tmp_dt->vol_obj);
                if(NULL == (loc->oloc = H5T_oloc(dt)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location of datatype")
                if(NULL == (loc->path = H5T_nameof(dt)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path of datatype")
#endif
            break;
        case H5I_DATASET:
            if(NULL == (loc->oloc = H5D_oloc((H5D_t *)obj)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location of dataset")
             if(NULL == (loc->path = H5D_nameof((H5D_t *)obj)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path of dataset")
            break;
        case H5I_ATTR:
            if(NULL == (loc->oloc = H5A_oloc((H5A_t *)obj)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location of attribute")
            if(NULL == (loc->path = H5A_nameof((H5A_t *)obj)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path of attribute")
            break;
        case H5I_FILE_PRIVATE:
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
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    } /* end switch */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5VL_native_get_loc */


/*---------------------------------------------------------------------------
 * Function:	H5VL_native_get_file
 *
 * Purpose:	utility routine to get file object
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2012
 *
 *---------------------------------------------------------------------------
 */
static H5F_t *
H5VL_native_get_file(void *obj, H5I_type_t type)
{
    H5F_t	*ret_value = NULL;      /* File to flush */
    H5O_loc_t	*oloc = NULL;           /* Object location for ID */

    FUNC_ENTER_NOAPI_NOINIT

    switch(type) {
        case H5I_FILE:
            ret_value = (H5F_t *) obj;
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
        case H5I_FILE_PRIVATE:
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
    } /* end switch */

    if(!ret_value) {
	if(!oloc)
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "object is not assocated with a file")
	ret_value = oloc->file;
    } /* end if */
    if(!ret_value)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "object is not associated with a file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}/* H5VL_native_get_file */


/*---------------------------------------------------------------------------
 * Function:	H5VL_native_register_aux
 *
 * Purpose:	utility routine to register the native VOL plugin to an ID
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2012
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VL_native_register_aux(hid_t obj_id)
{
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Build the vol plugin struct */
    if(NULL == (vol_plugin = (H5VL_t *)H5MM_calloc(sizeof(H5VL_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    vol_plugin->cls = &H5VL_native_g;
    vol_plugin->nrefs = 1;

    switch(H5I_get_type(obj_id)) {
        case H5I_FILE:
            if (H5I_register_aux(obj_id, vol_plugin, (H5I_free2_t)H5F_close_file) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
            break;
        case H5I_GROUP:
            if (H5I_register_aux(obj_id, vol_plugin, (H5I_free2_t)H5G_close_group) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
            break;
        case H5I_DATATYPE:
            if (H5I_register_aux(obj_id, vol_plugin, (H5I_free2_t)H5T_close_datatype) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
            break;
        case H5I_DATASET:
            if (H5I_register_aux(obj_id, vol_plugin, (H5I_free2_t)H5D_close_dataset) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
            break;
        case H5I_ATTR:
            if (H5I_register_aux(obj_id, vol_plugin, (H5I_free2_t)H5A_close_attr) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
            break;
        case H5I_FILE_PRIVATE:
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
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    } /* end switch */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5VL_native_register_aux */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_native
 *
 * Purpose:	Modify the file access property list to use the H5VL_NATIVE
 *		plugin defined in this source file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_native(hid_t fapl_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", fapl_id);

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    ret_value = H5P_set_vol(plist, &H5VL_native_g, NULL);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_native() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_attr_create
 *
 * Purpose:	Creates an attribute on an object.
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
H5VL_native_attr_create(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, 
                        hid_t UNUSED aapl_id, hid_t UNUSED req)
{
    H5G_loc_t       loc;                /* Object location */
    H5G_loc_t       obj_loc;            /* Location used to open group */
    hbool_t         loc_found = FALSE;  
    H5P_genplist_t  *plist;             /* Property list pointer */
    hid_t           type_id, space_id;
    H5T_t	    *type;              /* Datatype to use for attribute */
    H5S_t	    *space;             /* Dataspace to use for attribute */
    H5A_t           *attr = NULL;
    void            *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* get creation properties */
    if(H5P_get(plist, H5VL_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id")
    if(H5P_get(plist, H5VL_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id")

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")
    if(0 == (H5F_INTENT(loc.oloc->file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_ARGS, H5E_WRITEERROR, NULL, "no write intent on file")
    if(NULL == (type = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a type")
    if(NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a data space")

    if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Acreate */
        /* Go do the real work for attaching the attribute to the dataset */
        if(NULL == (attr = H5A_create(&loc, attr_name, type, space, acpl_id, H5AC_dxpl_id)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to create attribute")
    }
    else if (loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Acreate_by_name */
        H5G_name_t          obj_path;            	/* Opened object group hier. path */
        H5O_loc_t           obj_oloc;            	/* Opened object object location */

        /* Set up opened group location to fill in */
        obj_loc.oloc = &obj_oloc;
        obj_loc.path = &obj_path;
        H5G_loc_reset(&obj_loc);

        /* Find the object's location */
        if(H5G_loc_find(&loc, loc_params.loc_data.loc_by_name.name, &obj_loc/*out*/, 
                        loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, NULL, "object not found")
        loc_found = TRUE;

        /* Go do the real work for attaching the attribute to the dataset */
        if(NULL == (attr = H5A_create(&obj_loc, attr_name, type, space, acpl_id, H5AC_dxpl_id)))
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
                      hid_t UNUSED aapl_id, hid_t UNUSED req)
{
    H5G_loc_t    loc;             /* Object location */
    H5A_t        *attr = NULL;    /* Attribute opened */
    void         *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Aopen */
        /* Read in attribute from object header */
        if(NULL == (attr = H5O_attr_open_by_name(loc.oloc, attr_name, H5AC_ind_dxpl_id)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to load attribute info from object header for attribute: '%s'", attr_name)
        /* Finish initializing attribute */
        if(H5A_open_common(&loc, attr) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to initialize attribute")
    }
    else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Aopen_by_name */
        /* Open the attribute on the object header */
        if(NULL == (attr = H5A_open_by_name(&loc, loc_params.loc_data.loc_by_name.name, attr_name, 
                                            loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "can't open attribute")
    }
    else if(loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Aopen_by_idx */
        /* Open the attribute in the object header */
        if(NULL == (attr = H5A_open_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, 
                                           loc_params.loc_data.loc_by_idx.idx_type, 
                                           loc_params.loc_data.loc_by_idx.order, 
                                           loc_params.loc_data.loc_by_idx.n, 
                                           loc_params.loc_data.loc_by_idx.plist_id, H5AC_ind_dxpl_id)))
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
H5VL_native_attr_read(void *attr, hid_t dtype_id, void *buf, hid_t UNUSED req)
{
    H5T_t *mem_type;            /* Memory datatype */
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (mem_type = (H5T_t *)H5I_object_verify(dtype_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    /* Go write the actual data to the attribute */
    if((ret_value = H5A_read((H5A_t*)attr, mem_type, buf, H5AC_dxpl_id)) < 0)
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
H5VL_native_attr_write(void *attr, hid_t dtype_id, const void *buf, hid_t UNUSED req)
{
    H5T_t *mem_type;            /* Memory datatype */
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (mem_type = (H5T_t *)H5I_object_verify(dtype_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    /* Go write the actual data to the attribute */
    if((ret_value = H5A_write((H5A_t*)attr, mem_type, buf, H5AC_dxpl_id)) < 0)
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
H5VL_native_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t UNUSED req, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Aexists/exists_by_name */
        case H5VL_ATTR_EXISTS:
            {
                H5VL_loc_params_t loc_params  = va_arg (arguments, H5VL_loc_params_t);
                char    *attr_name      = va_arg (arguments, char *);
                htri_t	*ret       = va_arg (arguments, htri_t *);
                H5G_loc_t loc;

                /* check arguments */
                if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Aexists */
                    /* Check if the attribute exists */
                    if((*ret = H5O_attr_exists(loc.oloc, attr_name, H5AC_ind_dxpl_id)) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to determine if attribute exists")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Aopen_by_name */
                    /* Check if the attribute exists */
                    if((*ret = H5A_exists_by_name(loc, loc_params.loc_data.loc_by_name.name, 
                                                  attr_name, 
                                                  loc_params.loc_data.loc_by_name.plist_id)) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to determine if attribute exists")
                }
                else {
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown parameters")
                }
                break;
            }
        /* H5Aget_space */
        case H5VL_ATTR_GET_SPACE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5A_t   *attr = (H5A_t *)obj;

                if((*ret_id = H5A_get_space(attr)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get space ID of attribute")
                break;
            }
        /* H5Aget_type */
        case H5VL_ATTR_GET_TYPE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5A_t   *attr = (H5A_t *)obj;

                if((*ret_id = H5A_get_type(attr)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of attribute")
                break;
            }
        /* H5Aget_create_plist */
        case H5VL_ATTR_GET_ACPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5A_t   *attr = (H5A_t *)obj;

                if((*ret_id = H5A_get_create_plist(attr)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get creation property list for attr")

                break;
            }
        /* H5Aget_name */
        case H5VL_ATTR_GET_NAME:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                size_t	buf_size = va_arg (arguments, size_t);
                char    *buf = va_arg (arguments, char *);
                ssize_t	*ret_val = va_arg (arguments, ssize_t *);
                H5A_t   *attr = NULL;

                if(H5VL_OBJECT_BY_SELF == loc_params.type) {
                    attr = (H5A_t *)obj;
                    /* Call private function in turn */
                    if(0 > (*ret_val = H5A_get_name(attr, buf_size, buf)))
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get attribute name")
                }
                else if(H5VL_OBJECT_BY_IDX == loc_params.type) {
                    H5G_loc_t loc;
                    
                    /* check arguments */
                    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                    /* Open the attribute on the object header */
                    if(NULL == (attr = H5A_open_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, 
                                                       loc_params.loc_data.loc_by_idx.idx_type, 
                                                       loc_params.loc_data.loc_by_idx.order, 
                                                       loc_params.loc_data.loc_by_idx.n, 
                                                       loc_params.loc_data.loc_by_idx.plist_id, 
                                                       H5AC_ind_dxpl_id)))
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

                    /* Get the length of the name */
                    *ret_val = (ssize_t)HDstrlen(attr->shared->name);

                    /* Copy the name into the user's buffer, if given */
                    if(buf) {
                        HDstrncpy(buf, attr->shared->name, MIN((size_t)(ret_value + 1), buf_size));
                        if((size_t)ret_value >= buf_size)
                            buf[buf_size - 1]='\0';
                    } /* end if */

                    /* Release resources */
                    if(attr && H5A_close(attr) < 0)
                        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")
                }
                else
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get name of attr")

                break;
            }
        /* H5Aget_info */
        case H5VL_ATTR_GET_INFO:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                H5A_info_t   *ainfo = va_arg (arguments, H5A_info_t *);
                H5A_t   *attr = NULL;

                if(H5VL_OBJECT_BY_SELF == loc_params.type) {
                    attr = (H5A_t *)obj;
                    if(H5A_get_info(attr, ainfo) < 0)
                        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get attribute info")
                }
                else if(H5VL_OBJECT_BY_NAME == loc_params.type) {
                    char *attr_name = va_arg (arguments, char *);
                    H5G_loc_t loc;
                    
                    /* check arguments */
                    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                    /* Open the attribute on the object header */
                    if(NULL == (attr = H5A_open_by_name(&loc, loc_params.loc_data.loc_by_name.name, 
                                                        attr_name, 
                                                        loc_params.loc_data.loc_by_name.plist_id, 
                                                        H5AC_ind_dxpl_id)))
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

                    /* Get the attribute information */
                    if(H5A_get_info(attr, ainfo) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to get attribute info")

                    /* Release resources */
                    if(attr && H5A_close(attr) < 0)
                        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")
                }
                else if(H5VL_OBJECT_BY_IDX == loc_params.type) {
                    H5G_loc_t loc;
                    
                    /* check arguments */
                    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                    /* Open the attribute on the object header */
                    if(NULL == (attr = H5A_open_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, 
                                                       loc_params.loc_data.loc_by_idx.idx_type, 
                                                       loc_params.loc_data.loc_by_idx.order, 
                                                       loc_params.loc_data.loc_by_idx.n, 
                                                       loc_params.loc_data.loc_by_idx.plist_id, 
                                                       H5AC_ind_dxpl_id)))
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

                    /* Get the attribute information */
                    if(H5A_get_info(attr, ainfo) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to get attribute info")

                    /* Release resources */
                    if(attr && H5A_close(attr) < 0)
                        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")
                }
                else
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get name of attr")

                break;
            }
        case H5VL_ATTR_GET_STORAGE_SIZE:
            {
                hsize_t *ret = va_arg (arguments, hsize_t *);
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
 * Function:	H5VL_native_attr_remove
 *
 * Purpose:	Deletes an attribute from a location
 *
 * Return:	Success:	0
 *		Failure:	-1, attr not deleted.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL_native_attr_remove(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, 
                        hid_t UNUSED req)
{
    H5G_loc_t   loc;                    /* Object location */
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Adelete */
        /* Delete the attribute from the location */
        if(H5O_attr_remove(loc.oloc, attr_name, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")
    }
    else if (loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Adelete_by_name */
        /* Set up opened group location to fill in */
        obj_loc.oloc = &obj_oloc;
        obj_loc.path = &obj_path;
        H5G_loc_reset(&obj_loc);

        /* Find the object's location */
        if(H5G_loc_find(&loc, loc_params.loc_data.loc_by_name.name, &obj_loc/*out*/, 
                        loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
        loc_found = TRUE;

        /* Delete the attribute from the location */
        if(H5O_attr_remove(obj_loc.oloc, attr_name, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")
    }
    if(loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Adelete_by_idx */
        /* Set up opened group location to fill in */
        obj_loc.oloc = &obj_oloc;
        obj_loc.path = &obj_path;
        H5G_loc_reset(&obj_loc);

        /* Find the object's location */
        if(H5G_loc_find(&loc, loc_params.loc_data.loc_by_idx.name, &obj_loc/*out*/, 
                        loc_params.loc_data.loc_by_idx.plist_id, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
        loc_found = TRUE;

        /* Delete the attribute from the location */
        if(H5O_attr_remove_by_idx(obj_loc.oloc, loc_params.loc_data.loc_by_idx.idx_type, 
                                  loc_params.loc_data.loc_by_idx.order, 
                                  loc_params.loc_data.loc_by_idx.n, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")
    }
    else {
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown attribute remove parameters")
    }

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_attr_remove() */


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
H5VL_native_attr_close(void *attr, hid_t UNUSED req)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5A_close((H5A_t*)attr) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close attribute")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_attr_close() */


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
                            hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t UNUSED req)
{
    H5G_loc_t	loc;                    /* Location to commit datatype */
    H5T_t	*dt;                    /* Datatype for ID */
    H5T_t	*type;                  /* copy of the original type which will be committed */
    void        *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a datatype")

    /* Copy the datatype - the copied one will be the type that is
       committed, and attached to original datatype above the VOL
       layer*/
    if(NULL == (type = H5T_copy(dt, H5T_COPY_TRANSIENT)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to copy");

    if (NULL != name) { /* H5Tcommit */
        /* Commit the type */
        if(H5T__commit_named(&loc, name, type, lcpl_id, tcpl_id, tapl_id, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to commit datatype")
    }
    else { /* H5Tcommit_anon */
        /* Commit the type */
        if(H5T__commit(loc.oloc->file, type, tcpl_id, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to commit datatype")

        /* Release the datatype's object header */
        {
            H5O_loc_t *oloc;         /* Object location for datatype */

            /* Get the new committed datatype's object location */
            if(NULL == (oloc = H5T_oloc(type)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, NULL, "unable to get object location of committed datatype")

            /* Decrement refcount on committed datatype's object header in memory */
            if(H5O_dec_rc_by_loc(oloc, H5AC_dxpl_id) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDEC, NULL, "unable to decrement refcount on newly created object")
        } /* end if */
    }
    ret_value = (void *)type;
    /* Increment reference count on atom because the commit callback in the native implementation
       return the same object as the type ID itself
    if(H5I_inc_ref(type_id, TRUE) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTSET, FAIL, "incrementing type ID failed")
    */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_datatype_commit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_datatype_open
 *
 * Purpose:	Opens a named datatype inside a native h5 file.
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
H5VL_native_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, 
                          hid_t tapl_id, hid_t UNUSED req)
{
    H5T_t       *type = NULL;           /* Datatype opened in file */
    H5G_loc_t	 loc;                   /* Group location of object to open */
    H5G_name_t   path;            	/* Datatype group hier. path */
    H5O_loc_t    oloc;            	/* Datatype object location */
    H5O_type_t   obj_type;              /* Type of object at location */
    H5G_loc_t    type_loc;              /* Group object for datatype */
    hbool_t      obj_found = FALSE;     /* Object at 'name' found */
    hid_t        dxpl_id = H5AC_dxpl_id; /* dxpl to use to open datatype */
    void        *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

   /* Set up datatype location to fill in */
    type_loc.oloc = &oloc;
    type_loc.path = &path;
    H5G_loc_reset(&type_loc);

    /*
     * Find the named datatype object header and read the datatype message
     * from it.
     */
    if(H5G_loc_find(&loc, name, &type_loc/*out*/, tapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_NOTFOUND, NULL, "not found")
    obj_found = TRUE;

    /* Check that the object found is the correct type */
    if(H5O_obj_type(&oloc, &obj_type, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, NULL, "can't get object type")
    if(obj_type != H5O_TYPE_NAMED_DATATYPE)
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, NULL, "not a named datatype")

    /* Open it */
    if(NULL == (type = H5T_open(&type_loc, dxpl_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTOPENOBJ, NULL, "unable to open named datatype")

    ret_value = (void *)type;
done:
    if(NULL == type)
        if(obj_found && H5F_addr_defined(type_loc.oloc->addr))
            H5G_loc_free(&type_loc);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_datatype_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_datatype_get_binary
 *
 * Purpose:	gets size required to encode the datatype
 *
 * Return:	Success:	datatype id. 
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5VL_native_datatype_get_binary(void *obj, unsigned char *buf, size_t size, hid_t UNUSED req)
{
    H5T_t       *type = (H5T_t *)obj;
    size_t       nalloc = size;
    ssize_t      ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5T_encode(type, buf, &nalloc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't determine serialized length of datatype")

    ret_value = (ssize_t) nalloc;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_datatype_get_binary() */


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
H5VL_native_datatype_close(void *dt, hid_t UNUSED req)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5T_close((H5T_t*)dt) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close datatype")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_datatype_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_create
 *
 * Purpose:	Creates a dataset inside a native h5 file.
 *
 * Return:	Success:	dataset id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, 
                           hid_t dapl_id, hid_t UNUSED req)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    H5G_loc_t	   loc;                 /* Object location to insert dataset into */
    hid_t          type_id, space_id, lcpl_id;
    H5D_t	   *dset = NULL;        /* New dataset's info */
    const H5S_t    *space;              /* Dataspace for dataset */
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* get creation properties */
    if(H5P_get(plist, H5VL_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id")
    if(H5P_get(plist, H5VL_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id")
    if(H5P_get(plist, H5VL_DSET_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id")

    /* Check arguments */
    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")
    if(H5I_DATATYPE != H5I_get_type(type_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a datatype ID")
    if(NULL == (space = (const H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a dataspace ID")

    /* H5Dcreate_anon */
    if (NULL == name) {
        /* build and open the new dataset */
        if(NULL == (dset = H5D__create(loc.oloc->file, type_id, space, dcpl_id, dapl_id, H5AC_dxpl_id)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to create dataset")
    }
    /* H5Dcreate2 */
    else {
        /* Create the new dataset & get its ID */
        if(NULL == (dset = H5D__create_named(&loc, name, type_id, space, lcpl_id, 
                                             dcpl_id, dapl_id, H5AC_dxpl_id)))
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
            if(H5O_dec_rc_by_loc(oloc, H5AC_dxpl_id) < 0)
                HDONE_ERROR(H5E_DATASET, H5E_CANTDEC, NULL, "unable to decrement refcount on newly created object")
        } /* end if */
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_open
 *
 * Purpose:	Opens a dataset inside a native h5 file.
 *
 * Return:	Success:	dataset id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, 
                         hid_t dapl_id, hid_t UNUSED req)
{
    H5D_t       *dset = NULL;
    H5G_loc_t	 loc;		        /* Object location of group */
    H5G_loc_t	 dset_loc;		/* Object location of dataset */
    H5G_name_t   path;            	/* Dataset group hier. path */
    H5O_loc_t    oloc;            	/* Dataset object location */
    H5O_type_t   obj_type;              /* Type of object at location */
    hbool_t      loc_found = FALSE;     /* Location at 'name' found */
    hid_t        dxpl_id = H5AC_dxpl_id;    /* dxpl to use to open datset */
    void         *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    /* Set up dataset location to fill in */
    dset_loc.oloc = &oloc;
    dset_loc.path = &path;
    H5G_loc_reset(&dset_loc);

    /* Find the dataset object */
    if(H5G_loc_find(&loc, name, &dset_loc, dapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, NULL, "not found")
    loc_found = TRUE;

    /* Check that the object found is the correct type */
    if(H5O_obj_type(&oloc, &obj_type, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, NULL, "can't get object type")
    if(obj_type != H5O_TYPE_DATASET)
        HGOTO_ERROR(H5E_DATASET, H5E_BADTYPE, NULL, "not a dataset")

    /* Open the dataset */
    if(NULL == (dset = H5D_open(&dset_loc, dapl_id, dxpl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "can't open dataset")

    ret_value = (void *)dset;

done:
    if(NULL == dset && loc_found && H5G_loc_free(&dset_loc) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, NULL, "can't free location")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_read
 *
 * Purpose:	Reads raw data from a dataset into a buffer.
 *
 * Return:	Success:	0
 *		Failure:	-1, dataset not readd.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_dataset_read(void *obj, hid_t mem_type_id, hid_t mem_space_id,
                         hid_t file_space_id, hid_t plist_id, void *buf, hid_t UNUSED req)
{
    H5D_t         *dset = (H5D_t *)obj;
    const H5S_t   *mem_space = NULL;
    const H5S_t   *file_space = NULL;
    char           fake_char;
    herr_t         ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    if(NULL == dset->oloc.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(H5S_ALL != mem_space_id) {
	if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(mem_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    } /* end if */
    if(H5S_ALL != file_space_id) {
	if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(file_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    } /* end if */

    if(!buf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

    /* If the buffer is nil, and 0 element is selected, make a fake buffer.
     * This is for some MPI package like ChaMPIon on NCSA's tungsten which
     * doesn't support this feature.
     */
    if(!buf)
        buf = &fake_char;

    /* read raw data */
    if(H5D__read(dset, mem_type_id, mem_space, file_space, plist_id, buf/*out*/) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_write
 *
 * Purpose:	Writes raw data from a buffer into a dataset.
 *
 * Return:	Success:	0
 *		Failure:	-1, dataset not writed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_dataset_write(void *obj, hid_t mem_type_id, hid_t mem_space_id,
                          hid_t file_space_id, hid_t dxpl_id, const void *buf, hid_t UNUSED req)
{
    H5D_t         *dset = (H5D_t *)obj;
    const H5S_t   *mem_space = NULL;
    const H5S_t   *file_space = NULL;
    char           fake_char;
    herr_t         ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    if(NULL == dset->oloc.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(H5S_ALL != mem_space_id) {
	if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(mem_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    } /* end if */
    if(H5S_ALL != file_space_id) {
	if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(file_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    } /* end if */

    if(!buf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

    /* If the buffer is nil, and 0 element is selected, make a fake buffer.
     * This is for some MPI package like ChaMPIon on NCSA's tungsten which
     * doesn't support this feature.
     */
    if(!buf)
        buf = &fake_char;

    /* write raw data */
    if(H5D__write(dset, mem_type_id, mem_space, file_space, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_set_extent
 *
 * Purpose:	Set Extent of dataset
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
H5VL_native_dataset_set_extent(void *obj, const hsize_t size[], hid_t UNUSED req)
{
    H5D_t         *dset = (H5D_t *)obj;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Private function */
    if(H5D__set_extent(dset, size, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set extend dataset")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_set_extent() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_get
 *
 * Purpose:	Gets certain information about a dataset
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
H5VL_native_dataset_get(void *obj, H5VL_dataset_get_t get_type, hid_t UNUSED req, va_list arguments)
{
    H5D_t       *dset = (H5D_t *)obj;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Dget_space */
        case H5VL_DATASET_GET_SPACE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5D_get_space(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get space ID of dataset")

                break;
            }
            /* H5Dget_space_statuc */
        case H5VL_DATASET_GET_SPACE_STATUS:
            {
                H5D_space_status_t *allocation = va_arg (arguments, H5D_space_status_t *);

                /* Read data space address and return */
                if(H5D__get_space_status(dset, allocation, H5AC_ind_dxpl_id) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get space status")

                break;
            }
            /* H5Dget_type */
        case H5VL_DATASET_GET_TYPE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5D_get_type(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of dataset")

                break;
            }
            /* H5Dget_create_plist */
        case H5VL_DATASET_GET_DCPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5D_get_create_plist(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get creation property list for dataset")

                break;
            }
            /* H5Dget_access_plist */
        case H5VL_DATASET_GET_DAPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5D_get_access_plist(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get access property list for dataset")

                break;
            }
            /* H5Dget_storage_size */
        case H5VL_DATASET_GET_STORAGE_SIZE:
            {
                hsize_t *ret = va_arg (arguments, hsize_t *);

                /* Set return value */
                if(H5D__get_storage_size(dset, H5AC_ind_dxpl_id, ret) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, 0, "can't get size of dataset's storage")
                break;
            }
            /* H5Dget_offset */
        case H5VL_DATASET_GET_OFFSET:
            {
                haddr_t *ret = va_arg (arguments, haddr_t *);

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
 * Function:	H5VL_native_dataset_close
 *
 * Purpose:	Closes a dataset.
 *
 * Return:	Success:	0
 *		Failure:	-1, dataset not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_dataset_close(void *dset, hid_t UNUSED req)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5D_close((H5D_t*)dset) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "can't close dataset")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_create
 *
 * Purpose:	Creates a file as a native HDF5 file.
 *
 * Return:	Success:	the file id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t UNUSED req)
{
    H5F_t *new_file = NULL;
    hid_t file_id = FAIL;
    void  *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Adjust bit flags by turning on the creation bit and making sure that
     * the EXCL or TRUNC bit is set.  All newly-created files are opened for
     * reading and writing.
     */
    if (0==(flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC)))
	flags |= H5F_ACC_EXCL;	 /*default*/
    flags |= H5F_ACC_RDWR | H5F_ACC_CREAT;

    /* Create the file */ 
    if(NULL == (new_file = H5F_open(name, flags, fcpl_id, fapl_id, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create file")
    /* Get an atom for the file */
    if((file_id = H5I_register(H5I_FILE_PRIVATE, new_file, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, NULL, "unable to atomize file handle")
    /* store a pointer to the VOL class in the file structure */
    new_file->vol_cls = &H5VL_native_g;
    new_file->file_id = file_id;

    ret_value = (void *)new_file;

done:
    if(file_id < 0 && new_file && H5F_try_close(new_file) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_open
 *
 * Purpose:	Opens a file as a native HDF5 file.
 *
 * Return:	Success:	file id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t UNUSED req)
{
    H5F_t *new_file = NULL;
    hid_t file_id = FAIL;
    void  *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Open the file */ 
    if(NULL == (new_file = H5F_open(name, flags, H5P_FILE_CREATE_DEFAULT, fapl_id, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")
    /* Get an atom for the file */
    if((file_id = H5I_register(H5I_FILE_PRIVATE, new_file, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, NULL, "unable to atomize file handle")
    /* store a pointer to the VOL class in the file structure */
    new_file->vol_cls = &H5VL_native_g;
    new_file->file_id = file_id;

    ret_value = (void *)new_file;

done:
    if(file_id < 0 && new_file && H5F_try_close(new_file) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_flush
 *
 * Purpose:	Flushs a native HDF5 file.
 *
 * Return:	Success:	0
 *		Failure:	-1, file not flushed.
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_file_flush(void *obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, hid_t UNUSED req)
{
    H5F_t	*f = NULL;              /* File to flush */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (NULL == (f = H5VL_native_get_file(obj, loc_params.obj_type))) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    }

    /* Flush the file */
    /*
     * Nothing to do if the file is read only.	This determination is
     * made at the shared open(2) flags level, implying that opening a
     * file twice, once for read-only and once for read-write, and then
     * calling H5Fflush() with the read-only handle, still causes data
     * to be flushed.
     */
    if(H5F_ACC_RDWR & H5F_INTENT(f)) {
        /* Flush other files, depending on scope */
        if(H5F_SCOPE_GLOBAL == scope) {
            /* Call the flush routine for mounted file hierarchies */
            if(H5F_flush_mounts(f, H5AC_dxpl_id) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush mounted file hierarchy")
        } /* end if */
        else {
            /* Call the flush routine, for this file */
            if(H5F_flush(f, H5AC_dxpl_id, FALSE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush file's cached information")
        } /* end else */
    } /* end if */ 

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_get
 *
 * Purpose:	Gets certain data about a file
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_file_get(void *obj, H5VL_file_get_t get_type, hid_t UNUSED req, va_list arguments)
{
    H5F_t *f = NULL;  /* File struct */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Fget_access_plist */
        case H5VL_FILE_GET_FAPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                f = (H5F_t *)obj;
                /* Retrieve the file's access property list */
                if((*plist_id = H5F_get_access_plist(f, TRUE)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file access property list")
                break;
            }
        /* H5Fget_create_plist */
        case H5VL_FILE_GET_FCPL:
            {
                H5P_genplist_t *plist;      /* Property list */
                hid_t *plist_id = va_arg (arguments, hid_t *);

                f = (H5F_t *)obj;
                if(NULL == (plist = (H5P_genplist_t *)H5I_object(f->shared->fcpl_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

                /* Create the property list object to return */
                if((*plist_id = H5P_copy_plist(plist, TRUE)) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to copy file creation properties")

                break;
            }
        /* H5Fget_obj_count */
        case H5VL_FILE_GET_OBJ_COUNT:
            {
                ssize_t *ret = va_arg (arguments, ssize_t *);
                unsigned types = va_arg (arguments, unsigned);
                size_t  obj_count = 0;      /* Number of opened objects */

                f = (H5F_t *)obj;
                /* Perform the query */
                if(H5F_get_obj_count(f, types, TRUE, &obj_count) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_BADITER, FAIL, "H5F_get_obj_count failed")

                /* Set the return value */
                *ret = (ssize_t)obj_count;
                break;
            }
        /* H5Fget_obj_ids */
        case H5VL_FILE_GET_OBJ_IDS:
            {
                unsigned types = va_arg (arguments, unsigned);
                size_t max_objs = va_arg (arguments, size_t);
                hid_t *oid_list = va_arg (arguments, hid_t *);
                ssize_t *ret = va_arg (arguments, ssize_t *);
                size_t  obj_count = 0;      /* Number of opened objects */

                f = (H5F_t *)obj;
                /* Perform the query */
                if(H5F_get_obj_ids(f, types, max_objs, oid_list, TRUE, &obj_count) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_BADITER, FAIL, "H5F_get_obj_ids failed")

                /* Set the return value */
                *ret = (ssize_t)obj_count;
                break;
            }
        /* H5Fget_intent */
        case H5VL_FILE_GET_INTENT:
            {
                unsigned *ret = va_arg (arguments, unsigned *);

                f = (H5F_t *)obj;
                /* HDF5 uses some flags internally that users don't know about.
                 * Simplify things for them so that they only get either H5F_ACC_RDWR
                 * or H5F_ACC_RDONLY.
                 */
                if(H5F_INTENT(f) & H5F_ACC_RDWR)
                    *ret = H5F_ACC_RDWR;
                else
                    *ret = H5F_ACC_RDONLY;
                break;
            }
        /* H5Fget_name */
        case H5VL_FILE_GET_NAME:
            {
                H5I_type_t type = va_arg (arguments, H5I_type_t);
                size_t     size = va_arg (arguments, size_t);
                char      *name = va_arg (arguments, char *);
                ssize_t   *ret  = va_arg (arguments, ssize_t *);
                size_t     len;

                if (NULL == (f = H5VL_native_get_file(obj, type))) {
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
                }

                len = HDstrlen(H5F_OPEN_NAME(f));

                if(name) {
                    HDstrncpy(name, H5F_OPEN_NAME(f), MIN(len + 1,size));
                    if(len >= size)
                        name[size-1]='\0';
                } /* end if */

                /* Set the return value for the API call */
                *ret = (ssize_t)len;
                break;
            }
        /* H5I_get_file_id */
        case H5VL_OBJECT_GET_FILE:
            {
                H5I_type_t type = va_arg (arguments, H5I_type_t);
                hbool_t    app_ref = va_arg (arguments, hbool_t);
                void      **ret = va_arg (arguments, void **);
                H5F_t     *file = NULL;
                hid_t      file_id;
                H5G_loc_t  loc;       /* Location of object */

                if (H5VL_native_get_loc(obj, type, &loc) < 0) {
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
                }
                /* Get the file ID for the object */
                if((file_id = H5F_get_id(loc.oloc->file, app_ref)) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get file ID")

                /* return the file object */
                if(NULL == (file = (H5F_t *)H5I_object(file_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
                *ret = (void*)file;
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information")
    } /* end switch */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_misc
 *
 * Purpose:	Perform an operation
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
H5VL_native_file_misc(void *obj, H5VL_file_misc_t misc_type, hid_t UNUSED req, va_list arguments)
{
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (misc_type) {
        /* H5Fmount */
        case H5VL_FILE_MOUNT:
            {
                H5I_type_t  type       = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);
                H5F_t      *child      = va_arg (arguments, H5F_t *);
                hid_t       plist_id   = va_arg (arguments, hid_t);
                H5G_loc_t   loc;

                if (H5VL_native_get_loc(obj, type, &loc) < 0) {
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
                }
                /* Do the mount */
                if(H5F_mount(&loc, name, child, plist_id, H5AC_dxpl_id) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_MOUNT, FAIL, "unable to mount file")

                break;
            }
        /* H5Fmount */
        case H5VL_FILE_UNMOUNT:
            {
                H5I_type_t  type       = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);
                H5G_loc_t   loc;

                if (H5VL_native_get_loc(obj, type, &loc) < 0) {
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
                }
                /* Unmount */
                if (H5F_unmount(&loc, name, H5AC_dxpl_id) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_MOUNT, FAIL, "unable to unmount file")

                break;
            }
        /* H5Fis_accessible */
        case H5VL_FILE_IS_ACCESSIBLE:
            {
                hid_t UNUSED fapl_id  = va_arg (arguments, hid_t);
                const char *name    = va_arg (arguments, const char *);
                htri_t     *ret     = va_arg (arguments, htri_t *);

                /* Call private routine */
                if((*ret = H5F_is_hdf5(name)) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "unable to open file")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't recognize this operation type")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_misc() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_optional
 *
 * Purpose:	Perform a plugin specific operation on a native file
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_file_optional(void *obj, H5VL_file_optional_t optional_type, hid_t UNUSED req, va_list arguments)
{
    H5F_t        *f = NULL;           /* File */
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (optional_type) {
        /* H5Fget_filesize */
        case H5VL_FILE_GET_SIZE:
            {
                haddr_t    eof;                     /* End of file address */
                hsize_t    *ret = va_arg (arguments, hsize_t *);

                f = (H5F_t *)obj;
                /* Go get the actual file size */
                if(HADDR_UNDEF == (eof = H5FDget_eof(f->shared->lf)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get file size")
                *ret = (hsize_t)eof;
                break;
            }
        /* H5Fget_file_image */
        case H5VL_FILE_GET_FILE_IMAGE:
            {
                void       *buf_ptr   = va_arg (arguments, void *);
                ssize_t    *ret       = va_arg (arguments, ssize_t *);
                size_t      buf_len   = va_arg (arguments, size_t );

                f = (H5F_t *)obj;
                /* Do the actual work */
                if((*ret = H5F_get_file_image(f, buf_ptr, buf_len)) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "get file image failed")
                break;
            }
        /* H5Fget_freespace */
        case H5VL_FILE_GET_FREE_SPACE:
            {
                hsize_t	tot_space;	/* Amount of free space in the file */
                hssize_t    *ret = va_arg (arguments, hssize_t *);

                f = (H5F_t *)obj;
                /* Go get the actual amount of free space in the file */
                if(H5MF_get_freespace(f, H5AC_ind_dxpl_id, &tot_space, NULL) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to check free space for file")
                *ret = (hssize_t)tot_space;
                break;
            }
        case H5VL_FILE_GET_FREE_SECTIONS:
            {
                H5F_sect_info_t *sect_info = va_arg (arguments, H5F_sect_info_t *);
                ssize_t         *ret       = va_arg (arguments, ssize_t *);
                H5F_mem_t       type       = va_arg (arguments, H5F_mem_t);
                size_t          nsects     = va_arg (arguments, size_t);

                f = (H5F_t *)obj;
                /* Go get the free-space section information in the file */
                if((*ret = H5MF_get_free_sections(f, H5AC_ind_dxpl_id, 
                                                  type, nsects, sect_info)) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to check free space for file")
                break;
            }
        /* H5Fget_info2 */
        case H5VL_FILE_GET_INFO:
            {
                H5I_type_t  type   = va_arg (arguments, H5I_type_t);
                H5F_info2_t *finfo = va_arg (arguments, H5F_info2_t *);

                if (NULL == (f = H5VL_native_get_file(obj, type))) {
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
                }

                /* For file IDs, get the file object directly */
                /* (This prevents the H5G_loc() call from returning the file pointer for
                 * the top file in a mount hierarchy)
                 */
                HDassert(f->shared);

                /* Reset file info struct */
                HDmemset(finfo, 0, sizeof(*finfo));

                /* Get the size of the superblock and any superblock extensions */
                if(H5F_super_size(f, H5AC_ind_dxpl_id, &finfo->super.super_size, 
                                  &finfo->super.super_ext_size) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "Unable to retrieve superblock sizes")

                /* Get the size of any persistent free space */
                if(H5MF_get_freespace(f, H5AC_ind_dxpl_id, &finfo->free.tot_space, 
                                      &finfo->free.meta_size) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "Unable to retrieve free space information")

                /* Check for SOHM info */
                if(H5F_addr_defined(f->shared->sohm_addr))
                    if(H5SM_ih_size(f, H5AC_ind_dxpl_id, &finfo->sohm.hdr_size, &finfo->sohm.msgs_info) < 0)
                        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "Unable to retrieve SOHM index & heap storage info")

                /* Set version # fields */
                finfo->super.version = f->shared->sblock->super_vers;
                finfo->sohm.version = f->shared->sohm_vers;
                finfo->free.version = HDF5_FREESPACE_VERSION;
                break;
            }
        /* H5Fget_mdc_config */
        case H5VL_FILE_GET_MDC_CONF:
            {
                H5AC_cache_config_t *config_ptr = va_arg (arguments, H5AC_cache_config_t *);

                f = (H5F_t *)obj;
                /* Go get the resize configuration */
                if(H5AC_get_cache_auto_resize_config(f->shared->cache, config_ptr) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_auto_resize_config() failed.")
                break;
            }
        /* H5Fget_mdc_hit_rate */
        case H5VL_FILE_GET_MDC_HR:
            {
                double *hit_rate_ptr = va_arg (arguments, double *);

                f = (H5F_t *)obj;
                /* Go get the current hit rate */
                if(H5AC_get_cache_hit_rate(f->shared->cache, hit_rate_ptr) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_hit_rate() failed.")
                break;
            }
        /* H5Fget_mdc_size */
        case H5VL_FILE_GET_MDC_SIZE:
            {
                size_t *max_size_ptr        = va_arg (arguments, size_t *);
                size_t *min_clean_size_ptr  = va_arg (arguments, size_t *);
                size_t *cur_size_ptr        = va_arg (arguments, size_t *); 
                int    *cur_num_entries_ptr = va_arg (arguments, int *); 
                int32_t cur_num_entries;

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
                void **file_handle = va_arg (arguments, void **);
                hid_t  fapl        = va_arg (arguments, hid_t);

                f = (H5F_t *)obj;
                /* Retrieve the VFD handle for the file */
                if(H5F_get_vfd_handle(f, fapl, file_handle) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't retrieve VFD handle")
                break;
            }
        /* H5Fclear_elink_file_cache */
        case H5VL_FILE_CLEAR_ELINK_CACHE:
            {
                f = (H5F_t *)obj;
                /* Release the EFC */
                if(f->shared->efc)
                    if(H5F_efc_release(f->shared->efc) < 0)
                        HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't release external file cache")
                break;
            }
        /* H5Freopen */
        case H5VL_FILE_REOPEN:
            {
                void   **ret = va_arg (arguments, void **);
                H5F_t  *new_file = NULL;

                f = (H5F_t *)obj;
                if(NULL == (new_file = H5F_reopen(f)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to reopen file")
                new_file->vol_cls = &H5VL_native_g;
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
                H5AC_cache_config_t *config_ptr = va_arg (arguments, H5AC_cache_config_t *);

                f = (H5F_t *)obj;
                /* set the resize configuration  */
                if(H5AC_set_cache_auto_resize_config(f->shared->cache, config_ptr) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "H5AC_set_cache_auto_resize_config() failed.")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't recognize this operation type")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_close
 *
 * Purpose:	Closes a file.
 *
 * Return:	Success:	0
 *		Failure:	-1, file not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_file_close(void *file, hid_t UNUSED req)
{
    int nref;
    H5F_t *f = (H5F_t *)file;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Flush file if this is the last reference to this id and we have write
     * intent, unless it will be flushed by the "shared" file being closed.
     * This is only necessary to replicate previous behaviour, and could be
     * disabled by an option/property to improve performance. */
    if((f->shared->nrefs > 1) && (H5F_INTENT(f) & H5F_ACC_RDWR)) {
        if((nref = H5I_get_ref(f->file_id, FALSE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get ID ref count")
        if(nref == 1)
            if(H5F_flush(f, H5AC_dxpl_id, FALSE) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
    } /* end if */

    /*
     * Decrement reference count on atom.  When it reaches zero the file will
     * be closed.
     */
    if(H5I_dec_app_ref(f->file_id) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTCLOSEFILE, FAIL, "decrementing file ID failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_group_create
 *
 * Purpose:	Creates a group inside a native h5 file.
 *
 * Return:	Success:	group id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, 
                         hid_t gapl_id, hid_t UNUSED req)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    H5G_loc_t      loc;                 /* Location to create group */
    H5G_t	   *grp = NULL;         /* New group created */
    hid_t          lcpl_id;
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* get creation properties */
    if(H5P_get(plist, H5VL_GRP_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id")

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    /* if name is NULL then this is from H5Gcreate_anon */
    if (name == NULL) {
        H5G_obj_create_t gcrt_info;         /* Information for group creation */
        /* Set up group creation info */
        gcrt_info.gcpl_id = gcpl_id;
        gcrt_info.cache_type = H5G_NOTHING_CACHED;
        HDmemset(&gcrt_info.cache, 0, sizeof(gcrt_info.cache));

        /* Create the new group & get its ID */
        if(NULL == (grp = H5G__create(loc.oloc->file, &gcrt_info, H5AC_dxpl_id)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to create group")            
    }
    /* otherwise it's from H5Gcreate */
    else {
        /* Create the new group & get its ID */
        if(NULL == (grp = H5G__create_named(&loc, name, lcpl_id, gcpl_id, gapl_id, H5AC_dxpl_id)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to create group")
    }
    ret_value = (void *)grp;

done:
    if (name == NULL) {
        /* Release the group's object header, if it was created */
        if(grp) {
            H5O_loc_t *oloc;         /* Object location for group */

            /* Get the new group's object location */
            if(NULL == (oloc = H5G_oloc(grp)))
                HDONE_ERROR(H5E_SYM, H5E_CANTGET, NULL, "unable to get object location of group")

            /* Decrement refcount on group's object header in memory */
            if(H5O_dec_rc_by_loc(oloc, H5AC_dxpl_id) < 0)
                HDONE_ERROR(H5E_SYM, H5E_CANTDEC, NULL, "unable to decrement refcount on newly created object")
         } /* end if */
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_group_open
 *
 * Purpose:	Opens a group inside a native h5 file.
 *
 * Return:	Success:	group id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_native_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t UNUSED req)
{
    H5G_loc_t	    loc;                /* Location to open group */
    H5G_t	   *grp = NULL;         /* New group opend */
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    /* Open the group */
    if((grp = H5G__open_name(&loc, name, gapl_id, H5AC_dxpl_id)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open group")

    ret_value = (void *)grp;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_group_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_group_get
 *
 * Purpose:	Gets certain data about a group
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_group_get(void *obj, H5VL_group_get_t get_type, hid_t UNUSED req, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Gget_create_plist */
        case H5VL_GROUP_GET_GCPL:
            {
                hid_t *new_gcpl_id = va_arg (arguments, hid_t *);
                H5G_t *grp = (H5G_t *)obj;

                if((*new_gcpl_id = H5G_get_create_plist(grp)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get creation property list for group")
                break;
            }
        /* H5Gget_info */
        case H5VL_GROUP_GET_INFO:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                H5G_info_t  *grp_info = va_arg (arguments, H5G_info_t *);
                H5G_loc_t    loc;

                if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Gget_info */
                    /* Retrieve the group's information */
                    if(H5G__obj_info(loc.oloc, grp_info, H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info")
                }
                else if (loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Gget_info_by_name */
                    H5G_loc_t   grp_loc;                /* Location used to open group */
                    H5G_name_t  grp_path;            	/* Opened object group hier. path */
                    H5O_loc_t   grp_oloc;            	/* Opened object object location */

                    /* Set up opened group location to fill in */
                    grp_loc.oloc = &grp_oloc;
                    grp_loc.path = &grp_path;
                    H5G_loc_reset(&grp_loc);

                    /* Find the group object */
                    if(H5G_loc_find(&loc, loc_params.loc_data.loc_by_name.name, &grp_loc/*out*/, 
                                    loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")

                    /* Retrieve the group's information */
                    if(H5G__obj_info(grp_loc.oloc, grp_info/*out*/, H5AC_ind_dxpl_id) < 0) {
                        H5G_loc_free(&grp_loc);
                        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info")
                    }

                    /* Release the object location */
                    if(H5G_loc_free(&grp_loc) < 0)
                        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")
                }
                else if (loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Gget_info_by_idx */
                    H5G_loc_t   grp_loc;                /* Location used to open group */
                    H5G_name_t  grp_path;            	/* Opened object group hier. path */
                    H5O_loc_t   grp_oloc;            	/* Opened object object location */

                    /* Set up opened group location to fill in */
                    grp_loc.oloc = &grp_oloc;
                    grp_loc.path = &grp_path;
                    H5G_loc_reset(&grp_loc);

                    /* Find the object's location, according to the order in the index */
                    if(H5G_loc_find_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, 
                                           loc_params.loc_data.loc_by_idx.idx_type, 
                                           loc_params.loc_data.loc_by_idx.order, 
                                           loc_params.loc_data.loc_by_idx.n, &grp_loc/*out*/, 
                                           loc_params.loc_data.loc_by_idx.plist_id, 
                                           H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")

                    /* Retrieve the group's information */
                    if(H5G__obj_info(grp_loc.oloc, grp_info/*out*/, H5AC_ind_dxpl_id) < 0) {
                        H5G_loc_free(&grp_loc);
                        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info")
                    }

                    /* Release the object location */
                    if(H5G_loc_free(&grp_loc) < 0)
                        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")
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
 * Function:	H5VL_native_group_close
 *
 * Purpose:	Closes a group.
 *
 * Return:	Success:	0
 *		Failure:	-1, group not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_group_close(void *grp, hid_t UNUSED req)
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
                        hid_t lcpl_id, hid_t lapl_id, hid_t UNUSED req)
{
    H5P_genplist_t   *plist;                     /* Property list pointer */
    herr_t           ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    switch (create_type) {
        case H5VL_LINK_CREATE_HARD:
            {
                H5G_loc_t    cur_loc;
                H5G_loc_t    link_loc;
                void         *cur_obj;
                H5VL_loc_params_t cur_params;

                if(H5P_get(plist, H5VL_LINK_TARGET, &cur_obj) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current location id")
                if(H5P_get(plist, H5VL_LINK_TARGET_LOC_PARAMS, &cur_params) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current name")

                if (NULL != cur_obj && H5VL_native_get_loc(cur_obj, cur_params.obj_type, &cur_loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
                if (NULL != obj && H5VL_native_get_loc(obj, loc_params.obj_type, &link_loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                /* H5Lcreate_hard */
                if (H5VL_OBJECT_BY_NAME == cur_params.type) {
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
                                                    link_loc_p, loc_params.loc_data.loc_by_name.name,
                                                    lcpl_id, lapl_id, H5AC_dxpl_id)) < 0)
                        HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")
                }
                else { /* H5Olink */
                    /* Link to the object */
                    if(H5L_link(&link_loc, loc_params.loc_data.loc_by_name.name, &cur_loc, lcpl_id, 
                                lapl_id, H5AC_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")
                }
                break;
            }
        case H5VL_LINK_CREATE_SOFT:
            {
                char        *target_name;
                H5G_loc_t   link_loc;               /* Group location for new link */

                if (H5VL_native_get_loc(obj, loc_params.obj_type, &link_loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                if(H5P_get(plist, H5VL_LINK_TARGET_NAME, &target_name) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for targe name")

                /* Create the link */
                if((ret_value = H5L_create_soft(target_name, &link_loc, loc_params.loc_data.loc_by_name.name, 
                                                lcpl_id, lapl_id, H5AC_dxpl_id)) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")
                break;
            }
        case H5VL_LINK_CREATE_UD:
            {
                H5G_loc_t   link_loc;               /* Group location for new link */
                H5L_type_t link_type;
                void *udata;
                size_t udata_size;

                if (H5VL_native_get_loc(obj, loc_params.obj_type, &link_loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

                if(H5P_get(plist, H5VL_LINK_TYPE, &link_type) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for link type")
                if(H5P_get(plist, H5VL_LINK_UDATA, &udata) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for udata")
                if(H5P_get(plist, H5VL_LINK_UDATA_SIZE, &udata_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for udata size")

                /* Create link */
                if(H5L_create_ud(&link_loc, loc_params.loc_data.loc_by_name.name, udata, udata_size, 
                                 link_type, lcpl_id, lapl_id, H5AC_dxpl_id) < 0)
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
                      hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t UNUSED req)
{
    H5G_loc_t	src_loc, *src_loc_p;
    H5G_loc_t	dst_loc, *dst_loc_p;
    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (NULL != src_obj && H5VL_native_get_loc(src_obj, loc_params1.obj_type, &src_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    if (NULL != dst_obj && H5VL_native_get_loc(dst_obj, loc_params2.obj_type, &dst_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    /* Set up src & dst location pointers */
    src_loc_p = &src_loc;
    dst_loc_p = &dst_loc;
    if(NULL == src_obj)
        src_loc_p = dst_loc_p;
    else if(NULL == dst_obj)
        dst_loc_p = src_loc_p;

    /* Move/Copy the link */
    if(H5L_move(src_loc_p, loc_params1.loc_data.loc_by_name.name, 
                dst_loc_p, loc_params2.loc_data.loc_by_name.name, 
                copy_flag, lcpl_id, lapl_id, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_LINK, H5E_CANTMOVE, FAIL, "unable to move link")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_link_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_link_iterate
 *
 * Purpose:	Iterates through links in a group
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5VL_native_link_iterate(void *obj, H5VL_loc_params_t loc_params, hbool_t recursive, 
                                       H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx_p, 
                                       H5L_iterate_t op, void *op_data, hid_t UNUSED req)
{
    H5G_loc_t	loc;
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    if (!recursive) {
        H5G_link_iterate_t lnk_op;  /* Link operator */
        hsize_t     last_lnk;       /* Index of last object looked at */
        hsize_t	idx;            /* Internal location to hold index */

        /* Set up iteration beginning/end info */
        idx = (idx_p == NULL ? 0 : *idx_p);
        last_lnk = 0;

        /* Build link operator info */
        lnk_op.op_type = H5G_LINK_OP_NEW;
        lnk_op.op_func.op_new = op;

        /* Iterate over the links */
        if(loc_params.type == H5VL_OBJECT_BY_SELF) {
            if((ret_value = H5G_iterate(&loc, ".", idx_type, order, idx, &last_lnk, &lnk_op, op_data, 
                                        H5P_LINK_ACCESS_DEFAULT, H5AC_ind_dxpl_id)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "link iteration failed")
        }
        else if(loc_params.type == H5VL_OBJECT_BY_NAME) { 
            if((ret_value = H5G_iterate(&loc, loc_params.loc_data.loc_by_name.name, 
                                        idx_type, order, idx, &last_lnk, &lnk_op, op_data, 
                                        loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "link iteration failed")
        }
        else {
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown link iterate params")
        }

        /* Set the index we stopped at */
        if(idx_p)
            *idx_p = last_lnk;
    }
    else {
        /* Call internal group visitation routine */
        if(loc_params.type == H5VL_OBJECT_BY_SELF) {
            if((ret_value = H5G_visit(&loc, ".", idx_type, order, op, op_data, 
                                      H5P_LINK_ACCESS_DEFAULT, H5AC_ind_dxpl_id)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "link visitation failed")
        }
        else if(loc_params.type == H5VL_OBJECT_BY_NAME) { 
            if((ret_value = H5G_visit(&loc, loc_params.loc_data.loc_by_name.name, 
                                      idx_type, order, op, op_data, 
                                      loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "link visitation failed")
        }
        else {
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown link visit params")
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_link_iterate() */


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
                     hid_t UNUSED req, va_list arguments)
{
    H5G_loc_t	loc;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    switch (get_type) {
        /* H5Lexists */
        case H5VL_LINK_EXISTS:
            {
                htri_t     *ret    = va_arg (arguments, htri_t *);

                /* Check for the existence of the link */
                if((*ret = H5L_exists(&loc, loc_params.loc_data.loc_by_name.name, 
                                      loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link info")
                break;
            }
        /* H5Lget_info/H5Lget_info_by_idx */
        case H5VL_LINK_GET_INFO:
            {
                H5L_info_t *linfo  = va_arg (arguments, H5L_info_t *);

                /* Get the link information */
                if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Lget_info */
                    if(H5L_get_info(&loc, loc_params.loc_data.loc_by_name.name, linfo, 
                                    loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link info")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Lget_info_by_idx */
                    H5L_trav_gibi_t udata;              /* User data for callback */

                    /* Set up user data for retrieving information */
                    udata.idx_type = loc_params.loc_data.loc_by_idx.idx_type;
                    udata.order = loc_params.loc_data.loc_by_idx.order;
                    udata.n = loc_params.loc_data.loc_by_idx.n;
                    udata.dxpl_id = H5AC_ind_dxpl_id;
                    udata.linfo = linfo;
                    if(H5L_get_info_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, &udata, 
                                           loc_params.loc_data.loc_by_idx.plist_id, H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link info")
                }
                else
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link info")
                break;
            }
        /* H5Lget_name_by_idx */
        case H5VL_LINK_GET_NAME:
            {
                char       *name   = va_arg (arguments, char *);
                size_t      size   = va_arg (arguments, size_t);
                ssize_t    *ret    = va_arg (arguments, ssize_t *);
                H5L_trav_gnbi_t udata;              /* User data for callback */

                /* Set up user data for callback */
                udata.idx_type = loc_params.loc_data.loc_by_idx.idx_type;
                udata.order = loc_params.loc_data.loc_by_idx.order;
                udata.n = loc_params.loc_data.loc_by_idx.n;
                udata.dxpl_id = H5AC_ind_dxpl_id;
                udata.name = name;
                udata.size = size;
                udata.name_len = -1;

                /* Get the link name */
                if(H5L_get_name_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, &udata, 
                                       loc_params.loc_data.loc_by_idx.plist_id, H5AC_ind_dxpl_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link info")

                *ret = udata.name_len;
                break;
            }
        /* H5Lget_val/H5Lget_val_by_idx */
        case H5VL_LINK_GET_VAL:
            {
                void       *buf    = va_arg (arguments, void *);
                size_t     size    = va_arg (arguments, size_t);

                /* Get the link information */
                if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Lget_val */
                    if(H5L_get_val(&loc, loc_params.loc_data.loc_by_name.name, buf, size, 
                                   loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link value")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Lget_val_by_idx */
                    H5L_trav_gvbi_t udata;              /* User data for callback */

                    /* Set up user data for retrieving information */
                    udata.idx_type = loc_params.loc_data.loc_by_idx.idx_type;
                    udata.order = loc_params.loc_data.loc_by_idx.order;
                    udata.n = loc_params.loc_data.loc_by_idx.n;
                    udata.dxpl_id = H5AC_ind_dxpl_id;
                    udata.buf = buf;
                    udata.size = size;

                    if(H5L_get_val_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, &udata, 
                                          loc_params.loc_data.loc_by_idx.plist_id, H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link val")                    
                }
                else
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link val")

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from link")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_link_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_link_remove
 *
 * Purpose:	Removes the specified NAME from the group graph and
 *		decrements the link count for the object to which NAME
 *		points.  If the link count reaches zero then all file-space
 *		associated with the object will be reclaimed (but if the
 *		object is open, then the reclamation of the file space is
 *		delayed until all handles to the object are closed).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL_native_link_remove(void *obj, H5VL_loc_params_t loc_params, hid_t UNUSED req)
{
    H5G_loc_t       loc;                /* Object location */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    /* Unlink */
    if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Ldelete */
        if(H5L_delete(&loc, loc_params.loc_data.loc_by_name.name, 
                      loc_params.loc_data.loc_by_name.plist_id, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_LINK, H5E_CANTDELETE, FAIL, "unable to delete link")
    }
    else if(loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Ldelete_by_idx */
        H5L_trav_rmbi_t udata;              /* User data for callback */

        /* Set up user data for unlink operation */
        udata.idx_type = loc_params.loc_data.loc_by_idx.idx_type;
        udata.order = loc_params.loc_data.loc_by_idx.order;
        udata.n = loc_params.loc_data.loc_by_idx.n;
        udata.dxpl_id = H5AC_dxpl_id;

        if(H5L_delete_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, &udata, 
                             loc_params.loc_data.loc_by_idx.plist_id, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_LINK, H5E_CANTDELETE, FAIL, "unable to delete link")
    }
    else
        HGOTO_ERROR(H5E_LINK, H5E_CANTDELETE, FAIL, "unable to delete link")
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_link_remove() */


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
                        hid_t UNUSED req)
{
    H5G_loc_t   loc;
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'name' found */
    hid_t       temp_id = FAIL;
    void       *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

    switch (loc_params.type) {
        case H5VL_OBJECT_BY_NAME:
            {
                /* Open the object */
                if((temp_id = H5O_open_name(&loc, loc_params.loc_data.loc_by_name.name, 
                                            loc_params.loc_data.loc_by_name.plist_id, TRUE)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open object")
                break;
            }
        case H5VL_OBJECT_BY_IDX:
            {
                /* Set up opened group location to fill in */
                obj_loc.oloc = &obj_oloc;
                obj_loc.path = &obj_path;
                H5G_loc_reset(&obj_loc);

                /* Find the object's location, according to the order in the index */
                if(H5G_loc_find_by_idx(&loc, loc_params.loc_data.loc_by_idx.name, 
                                       loc_params.loc_data.loc_by_idx.idx_type, 
                                       loc_params.loc_data.loc_by_idx.order, loc_params.loc_data.loc_by_idx.n, 
                                       &obj_loc/*out*/, loc_params.loc_data.loc_by_idx.plist_id, 
                                       H5AC_dxpl_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "group not found")
                loc_found = TRUE;

                /* Open the object */
                if((temp_id = H5O_open_by_loc(&obj_loc, loc_params.loc_data.loc_by_name.plist_id, 
                                                H5AC_dxpl_id, TRUE)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open object")
                break;
            }
        case H5VL_OBJECT_BY_ADDR:
            {
                if(!H5F_addr_defined(loc_params.loc_data.loc_by_addr.addr))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no address supplied")

                /* Set up opened group location to fill in */
                obj_loc.oloc = &obj_oloc;
                obj_loc.path = &obj_path;
                H5G_loc_reset(&obj_loc);
                obj_loc.oloc->addr = loc_params.loc_data.loc_by_addr.addr;
                obj_loc.oloc->file = loc.oloc->file;
                H5G_name_reset(obj_loc.path);       /* objects opened through this routine don't have a path name */

                /* Open the object */
                if((temp_id = H5O_open_by_loc(&obj_loc, H5P_LINK_ACCESS_DEFAULT, 
                                                H5AC_dxpl_id, TRUE)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open object")
                break;
            }
        case H5VL_OBJECT_BY_REF:
            {
                H5F_t *file = NULL;

                /* Get the file pointer from the entry */
                file = loc.oloc->file;

                /* Create reference */
                if((temp_id = H5R_dereference(file, loc_params.loc_data.loc_by_ref.plist_id, 
                                                H5AC_dxpl_id, 
                                                loc_params.loc_data.loc_by_ref.ref_type, 
                                                loc_params.loc_data.loc_by_ref._ref, 
                                                TRUE)) < 0)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, NULL, "unable to dereference object")
                break;
            }
        case H5VL_OBJECT_BY_SELF:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "unknown open parameters")
    }

    *opened_type = H5I_get_type (temp_id);
    if (NULL == (ret_value = H5I_remove(temp_id))) {
        HDONE_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open object")
    }
done:
    /* Release the object location if we failed after copying it */
    if(temp_id < 0 && loc_found)
        if(H5G_loc_free(&obj_loc) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, NULL, "can't free location")
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
                        hid_t ocpypl_id, hid_t lcpl_id, hid_t UNUSED req)
{
    H5G_loc_t	src_loc;                /* Source object group location */
    H5G_loc_t	dst_loc;                /* Destination group location */
    hid_t       ret_value = FAIL;
    
    FUNC_ENTER_NOAPI_NOINIT

    /* get location for objects */
    if (H5VL_native_get_loc(src_obj, loc_params1.obj_type, &src_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    if (H5VL_native_get_loc(dst_obj, loc_params2.obj_type, &dst_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    /* Open the object */
    if((ret_value = H5O_copy(&src_loc, src_name, &dst_loc, dst_name, ocpypl_id, lcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, FAIL, "unable to copy object")    
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_copy() */




/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_visit
 *
 * Purpose:	Iterates through all objects linked to an object
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5VL_native_object_visit(void *obj, H5VL_loc_params_t loc_params, H5_index_t idx_type,
                                       H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t UNUSED req)
{
    H5G_loc_t    loc;
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    /* Call internal object visitation routine */
    if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Ovisit */
        if((ret_value = H5O_visit(&loc, ".", idx_type, order, op, op_data, 
                                  H5P_LINK_ACCESS_DEFAULT, H5AC_ind_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "object visitation failed")
    }
    else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Ovisit_by_name */
        if((ret_value = H5O_visit(&loc, loc_params.loc_data.loc_by_name.name, idx_type, order, 
                                  op, op_data, loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "object visitation failed")
    }
    else {
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown object visit params")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_visit() */

#if 0

/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_lookup
 *
 * Purpose:	Lookup the object location in the file
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
H5VL_native_object_lookup(hid_t loc_id, H5VL_loc_type_t lookup_type, void **loc_token, 
                          hid_t UNUSED req, va_list arguments)
{
    H5G_loc_t	loc;
    H5G_loc_t   *obj_loc;
    H5G_loc_t   **location = (H5G_loc_t **)loc_token;
    haddr_t     obj_addr;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    *location = (H5G_loc_t *) H5MM_malloc (sizeof (H5G_loc_t));

    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

    obj_loc = *location;
    obj_loc->oloc = (H5O_loc_t *) H5MM_malloc (sizeof (H5O_loc_t));
    obj_loc->path = (H5G_name_t *) H5MM_malloc (sizeof (H5G_name_t));
    H5G_loc_reset(obj_loc);

    switch (lookup_type) {
        case H5VL_OBJECT_BY_SELF:
            obj_loc->oloc->addr = loc.oloc->addr;
            obj_loc->oloc->file = loc.oloc->file;
            obj_loc->oloc->holding_file = loc.oloc->holding_file;
            obj_loc->path = loc.path;
            break;
        case H5VL_OBJECT_BY_NAME:
            {
                char        *name   = va_arg (arguments, char *);
                hid_t       lapl_id = va_arg (arguments, hid_t);

                HDassert(name && *name);

                /* Find the object's location */
                if((ret_value = H5G_loc_find(&loc, name, obj_loc, lapl_id, H5AC_dxpl_id)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
                break;
            }
        case H5VL_OBJECT_BY_IDX:
            {
                char            *group_name   = va_arg (arguments, char *);
                H5_index_t      idx_type      = va_arg (arguments, H5_index_t);
                H5_iter_order_t order         = va_arg (arguments, H5_iter_order_t);
                hsize_t         n             = va_arg (arguments, hsize_t);
                hid_t           lapl_id       = va_arg (arguments, hid_t);

                /* Find the object's location, according to the order in the index */
                if((ret_value = H5G_loc_find_by_idx(&loc, group_name, idx_type, order, n,
                                                    obj_loc/*out*/, lapl_id, H5AC_dxpl_id)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")
                break;
            }
        case H5VL_OBJECT_BY_ADDR:
            {
                obj_addr = va_arg (arguments, haddr_t);
                obj_loc->oloc->addr = obj_addr;
                obj_loc->oloc->file = loc.oloc->file;
                obj_loc->oloc->holding_file = loc.oloc->holding_file;
                break;
            }
        case H5VL_OBJECT_BY_REF:
            {
                H5R_type_t      ref_type      = va_arg (arguments, H5R_type_t);
                void            *_ref         = va_arg (arguments, void *);
                H5F_t *file = NULL; /* File object */

                /* Get the file pointer from the entry */
                file = loc.oloc->file;

                HDassert(_ref);
                HDassert(ref_type > H5R_BADTYPE || ref_type < H5R_MAXTYPE);
                HDassert(file);

                switch(ref_type) {
                    case H5R_OBJECT:
                        obj_loc->oloc->addr = *(const hobj_ref_t *)_ref; /* Only object references currently supported */
                        obj_loc->oloc->file = loc.oloc->file;
                        obj_loc->oloc->holding_file = loc.oloc->holding_file;
                        break;

                    case H5R_DATASET_REGION:
                        {
                            H5HG_t hobjid;  /* Heap object ID */
                            uint8_t *buf;   /* Buffer to store serialized selection in */
                            const uint8_t *p;           /* Pointer to OID to store */

                            /* Get the heap ID for the dataset region */
                            p = (const uint8_t *)_ref;
                            H5F_addr_decode(file, &p, &(hobjid.addr));
                            INT32DECODE(p, hobjid.idx);

                            /* Get the dataset region from the heap (allocate inside routine) */
                            if(NULL == (buf = (uint8_t *)H5HG_read(file, H5AC_dxpl_id, 
                                                                   &hobjid, NULL, NULL)))
                                HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, FAIL, "Unable to read dataset region information")

                            /* Get the object oid for the dataset */
                            p = buf;
                            //H5F_addr_decode(file, &p, &(obj_addr));
                            H5F_addr_decode(file, &p, &(obj_loc->oloc->addr));
                            obj_loc->oloc->file = file;
                            /* Free the buffer allocated in H5HG_read() */
                            H5MM_xfree(buf);
                        } /* end case */
                        break;

                    case H5R_BADTYPE:
                    case H5R_MAXTYPE:
                    default:
                        HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
                } /* end switch */
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't lookup this object")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_lookup() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_free_loc
 *
 * Purpose:	Free the location token
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL_native_object_free_loc(void *location, hid_t UNUSED req)
{
    H5G_loc_t   *obj_loc = (H5G_loc_t *)location;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc_free(obj_loc) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")

    if (NULL != obj_loc->oloc) {
        H5MM_free(obj_loc->oloc);
    }
    if (NULL != obj_loc->path) {
        H5MM_free(obj_loc->path);
    }
    if (NULL != obj_loc) {
        H5MM_free(obj_loc);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_free_loc() */
#endif


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_misc
 *
 * Purpose:	Perform a plugin specific operation for an objectibute
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
H5VL_native_object_misc(void *obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, 
                        hid_t UNUSED req, va_list arguments)
{
    H5G_loc_t    loc;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    switch (misc_type) {
        /* H5Arename/rename_by_name */
        case H5VL_ATTR_RENAME:
            {
                const char    *old_name  = va_arg (arguments, const char *);
                const char    *new_name  = va_arg (arguments, const char *);

                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Arename */
                    /* Call attribute rename routine */
                    if(H5O_attr_rename(loc.oloc, H5AC_dxpl_id, old_name, new_name) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTRENAME, FAIL, "can't rename attribute")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Arename_by_name */
                    /* Call attribute rename routine */
                    if(H5A_rename_by_name(loc, loc_params.loc_data.loc_by_name.name, old_name,
                                          new_name, loc_params.loc_data.loc_by_name.plist_id) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTRENAME, FAIL, "can't rename attribute")
                }
                else {
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown attribute rename parameters")
                }
                break;
            }
        /* H5Oincr_refcount / H5Odecr_refcount */
        case H5VL_OBJECT_CHANGE_REF_COUNT:
            {
                int update_ref  = va_arg (arguments, int);
                H5O_loc_t  *oloc = loc.oloc;

                /* Get the object's oloc so we can adjust its link count 
                if((oloc = H5O_get_loc(loc_id)) == NULL)
                    HGOTO_ERROR(H5E_ATOM, H5E_BADVALUE, FAIL, "unable to get object location from ID")
                */

                if(H5O_link(oloc, update_ref, H5AC_dxpl_id) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "modifying object link count failed")

                break;
            }
        /* H5Oset_comment */
        case H5VL_OBJECT_SET_COMMENT:
            {
                const char    *comment  = va_arg (arguments, char *);

                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Oset_comment */
                    /* (Re)set the object's comment */
                    if(H5G_loc_set_comment(&loc, ".", comment, H5P_LINK_ACCESS_DEFAULT, H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Oset_comment_by_name */
                    /* (Re)set the object's comment */
                    if(H5G_loc_set_comment(&loc, loc_params.loc_data.loc_by_name.name, comment, 
                                           loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
                }
                else {
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown set_coment parameters")
                }
                break;
            }
        case H5VL_REF_CREATE:
            {
                void        *ref      = va_arg (arguments, void *);
                const char  *name     = va_arg (arguments, char *);
                H5R_type_t  ref_type  = va_arg (arguments, H5R_type_t);
                hid_t       space_id  = va_arg (arguments, hid_t);
                H5S_t       *space = NULL;   /* Pointer to dataspace containing region */
                
                if(space_id != (-1) && (NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE))))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

                /* Create reference */
                if((ret_value = H5R_create(ref, &loc, name, ref_type, space, H5AC_dxpl_id)) < 0)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to create reference")

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't recognize this operation type")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_misc() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_optional
 *
 * Purpose:	Perform a plugin specific operation for an objectibute
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
H5VL_native_object_optional(void UNUSED *obj, H5VL_loc_params_t UNUSED loc_params, 
                            H5VL_object_optional_t optional_type, hid_t UNUSED req, va_list UNUSED arguments)
{
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (optional_type) {
        case H5VL_OPTIONAL:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't perform this operation on object");       
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_get
 *
 * Purpose:	Gets certain data about a file
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
H5VL_native_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, 
                       hid_t UNUSED req, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */
    H5G_loc_t	loc;                    /* Location of group */

    FUNC_ENTER_NOAPI_NOINIT

    if (H5VL_native_get_loc(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    switch (get_type) {
        /* H5Oexists_by_name */
        case H5VL_OBJECT_EXISTS:
            {
                htri_t	  *ret      = va_arg (arguments, htri_t *);

                if (loc_params.type == H5VL_OBJECT_BY_NAME) {
                    /* Check if the object exists */
                    if((*ret = H5G_loc_exists(&loc, loc_params.loc_data.loc_by_name.name, 
                                              loc_params.loc_data.loc_by_name.plist_id, H5AC_dxpl_id)) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "unable to determine if '%s' exists", 
                                    loc_params.loc_data.loc_by_name.name)
                }
                else {
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown object exists parameters")
                }
                break;
            }
        /* H5Oget_info / H5Oget_info_by_name / H5Oget_info_by_idx */
        case H5VL_OBJECT_GET_INFO:
            {
                H5O_info_t  *obj_info = va_arg (arguments, H5O_info_t *);

                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Oget_info */
                    /* Retrieve the object's information */
                    if(H5G_loc_info(&loc, ".", TRUE, obj_info, H5P_LINK_ACCESS_DEFAULT, 
                                    H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
                }
                else if (loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Oget_info_by_name */
                    /* Retrieve the object's information */
                    if(H5G_loc_info(&loc, loc_params.loc_data.loc_by_name.name, TRUE, obj_info, 
                                    loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
                }
                else if (loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Oget_info_by_idx */
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
                                           loc_params.loc_data.loc_by_idx.n, &obj_loc/*out*/, 
                                           loc_params.loc_data.loc_by_idx.plist_id, 
                                           H5AC_ind_dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")

                    /* Retrieve the object's information */
                    if(H5O_get_info(obj_loc.oloc, H5AC_ind_dxpl_id, TRUE, obj_info) < 0) {
                        H5G_loc_free(&obj_loc);
                        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve object info")
                    }

                    /* Release the object location */
                    if(H5G_loc_free(&obj_loc) < 0)
                        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")
                }
                else {
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown get info parameters")
                }
                break;
            }
        /* H5Oget_comment / H5Oget_comment_by_name */
        case H5VL_OBJECT_GET_COMMENT:
            {
                char     *comment =  va_arg (arguments, char *);
                size_t   bufsize  =  va_arg (arguments, size_t);
                ssize_t  *ret     =  va_arg (arguments, ssize_t *);

                /* Retrieve the object's comment */
                if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Oget_comment */
                    if((*ret = H5G_loc_get_comment(&loc, ".", comment/*out*/, bufsize, 
                                                   H5P_LINK_ACCESS_DEFAULT, H5AC_ind_dxpl_id)) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
                }
                else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Oget_comment_by_name */
                    if((*ret = H5G_loc_get_comment(&loc, loc_params.loc_data.loc_by_name.name, comment/*out*/, bufsize, 
                                                   loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id)) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
                }
                else {
                    HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown set_coment parameters")
                }
                break;
            }
        /* H5Rget_region */
        case H5VL_REF_GET_REGION:
            {
                hid_t       *ret     =  va_arg (arguments, hid_t *);
                H5R_type_t  UNUSED ref_type =  va_arg (arguments, H5R_type_t);
                void        *ref     =  va_arg (arguments, void *);
                H5S_t       *space = NULL;    /* Dataspace object */

                /* Get the dataspace with the correct region selected */
                if((space = H5R_get_region(loc.oloc->file, H5AC_ind_dxpl_id, ref)) == NULL)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCREATE, FAIL, "unable to create dataspace")

                /* Atomize */
                if((*ret = H5I_register (H5I_DATASPACE, space, TRUE)) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")

                break;
            }
        /* H5Rget_obj_type2 */
        case H5VL_REF_GET_TYPE:
            {
                H5O_type_t  *obj_type  =  va_arg (arguments, H5O_type_t *);
                H5R_type_t  ref_type   =  va_arg (arguments, H5R_type_t);
                void        *ref       =  va_arg (arguments, void *);

                /* Get the object information */
                if(H5R_get_obj_type(loc.oloc->file, H5AC_ind_dxpl_id, ref_type, ref, obj_type) < 0)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to determine object type")
                break;
            }
        /* H5Rget_name */
        case H5VL_REF_GET_NAME:
            {
                ssize_t     *ret       = va_arg (arguments, ssize_t *);
                char        *name      = va_arg (arguments, char *);
                size_t      size       = va_arg (arguments, size_t);
                H5R_type_t  ref_type   = va_arg (arguments, H5R_type_t);
                void        *ref       = va_arg (arguments, void *);
                //H5F_t       *file;        /* File object */

                /* Get the file pointer from the entry 
                file = loc.oloc->file;
                */
                /* Get name */
                if((*ret = H5R_get_name(&loc, H5P_DEFAULT, H5AC_dxpl_id, ref_type, ref, name, size)) < 0)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to determine object path")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from object")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_close
 *
 * Purpose:	Closes a object.
 *
 * Return:	Success:	0
 *		Failure:	-1, object not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_object_close(void UNUSED *obj, H5VL_loc_params_t loc_params, hid_t UNUSED req)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the type of the object and close it in the correct way */
    switch(loc_params.obj_type) {
        case H5I_GROUP:
        case H5I_DATATYPE:
        case H5I_DATASET:
            break;
        case H5I_FILE_PRIVATE:
        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_FILE:
        case H5I_DATASPACE:
        case H5I_ATTR:
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
            HGOTO_ERROR(H5E_ARGS, H5E_CANTRELEASE, FAIL, "not a valid file object ID (dataset, group, or datatype)")
        break;
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_close() */
