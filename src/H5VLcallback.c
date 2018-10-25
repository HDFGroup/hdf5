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
 * Purpose:     The Virtual Object Layer as described in documentation.
 *              The pupose is to provide an abstraction on how to access the
 *              underlying HDF5 container, whether in a local file with
 *              a specific file format, or remotely on other machines, etc...
 */


/****************/
/* Module Setup */
/****************/

#include "H5VLmodule.h"         /* This source code file is part of the H5VL module */


/***********/
/* Headers */
/***********/

#include "H5private.h"          /* Generic Functions                                */
#include "H5Eprivate.h"         /* Error handling                                   */
#include "H5Fprivate.h"		/* File access				            */
#include "H5Iprivate.h"         /* IDs                                              */
#include "H5MMprivate.h"        /* Memory management                                */
#include "H5Pprivate.h"         /* Property lists                                   */
#include "H5VLpkg.h"            /* Virtual Object Layer                             */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5VL_copy_plugin_info
 *
 * Purpose:     Copy the VOL info for a plugin
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_copy_plugin_info(const H5VL_class_t *plugin, void **dst_info,
    const void *src_info)
{
    void *new_plugin_info = NULL;       /* Copy of plugin info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(plugin);

    /* Check for actual source info */
    if(src_info) {
        /* Allow the plugin to copy or do it ourselves */
        if(plugin->info_copy) {
            if(NULL == (new_plugin_info = (plugin->info_copy)(src_info)))
                HGOTO_ERROR(H5E_VOL, H5E_CANTCOPY, FAIL, "plugin info copy callback failed")
        } /* end if */
        else if(plugin->info_size > 0) {
            if(NULL == (new_plugin_info = H5MM_malloc(plugin->info_size)))
                HGOTO_ERROR(H5E_VOL, H5E_CANTALLOC, FAIL, "plugin info allocation failed")
            HDmemcpy(new_plugin_info, src_info, plugin->info_size);
        } /* end else-if */
        else
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "no way to copy plugin info")
    } /* end if */

    /* Set the plugin info for the copy */
    *dst_info = new_plugin_info;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_copy_plugin_info() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_cmp_plugin_info
 *
 * Purpose:     Compare VOL info for a plugin
 *
 * Return:      Positive if VALUE1 is greater than VALUE2, negative if
 *              VALUE2 is greater than VALUE1 and zero if VALUE1 and
 *              VALUE2 are equal.
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_cmp_plugin_info(const H5VL_class_t *plugin, const void *info1,
    const void *info2)
{
    int cmp_value;              /* Value from comparison */
    int ret_value = 0;          /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(plugin);

    /* Use the class's info comparison routine to compare the info objects,
     * if there is a a callback, otherwise just compare the info objects as
     * memory buffers
     */
    if(plugin->info_cmp) {
        if(0 != (cmp_value = (plugin->info_cmp)(info1, info2)))
            HGOTO_DONE(cmp_value);
    } /* end if */
    else {
        if(info1 == NULL && info2 != NULL)
            HGOTO_DONE(-1);
        if(info1 != NULL && info2 == NULL)
            HGOTO_DONE(1);
        if(info1) {
            HDassert(plugin->info_size > 0);
            if(0 != (cmp_value = HDmemcmp(info1, info2, plugin->info_size)))
                HGOTO_DONE(cmp_value);
        } /* end if */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_cmp_plugin_info() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_free_plugin_info
 *
 * Purpose:     Free VOL info for a plugin
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_free_plugin_info(const H5VL_class_t *plugin, void *info)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(plugin);

    /* Only free info object, if it's non-NULL */
    if(info) {
        /* Allow the plugin to free info or do it ourselves */
        if(plugin->info_free) {
            if((plugin->info_free)(info) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "plugin info free request failed")
        } /* end if */
        else
            H5MM_xfree(info);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_plugin_info() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_get_wrap_ctx
 *
 * Purpose:     Retrieve the VOL object wrapping context for a plugin
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_get_wrap_ctx(const H5VL_class_t *plugin, void *obj, void **wrap_ctx)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(plugin);
    HDassert(obj);
    HDassert(wrap_ctx);

    /* Allow the plugin to copy or do it ourselves */
    if(plugin->get_wrap_ctx) {
        /* Sanity check */
        HDassert(plugin->free_wrap_ctx);

        /* Invoke plugin's callback */
        if((plugin->get_wrap_ctx)(obj, wrap_ctx) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "plugin wrap context callback failed")
    } /* end if */
    else
        *wrap_ctx = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_get_wrap_ctx() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_free_wrap_ctx
 *
 * Purpose:     Free object wrapping context for a plugin
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_free_wrap_ctx(const H5VL_class_t *plugin, void *wrap_ctx)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(plugin);

    /* Only free wrap context, if it's non-NULL */
    if(wrap_ctx) {
        /* Free the plugin's object wrapping context */
        if((plugin->free_wrap_ctx)(wrap_ctx) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "plugin wrap context free request failed")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_plugin_info() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_wrap_object
 *
 * Purpose:     Wrap an object with plugin
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_wrap_object(const H5VL_class_t *plugin, void *wrap_ctx, void *obj)
{
    void *ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Sanity checks */
    HDassert(plugin);
    HDassert(obj);

    /* Only wrap object if there's a wrap context */
    if(wrap_ctx) {
        /* Ask the plugin to wrap the object */
        if(NULL == (ret_value = (plugin->wrap_object)(obj, wrap_ctx)))
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, NULL, "can't wrap object")
    } /* end if */
    else
        ret_value = obj;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_wrap_object() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_attr_create
 *
 * Purpose:     Creates an attribute through the VOL
 *
 * Return:      Success: pointer to the new attribute 
 *
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_attr_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                 hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;  /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if (NULL == cls->attr_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'attr create' method")

    /* call the corresponding VOL create callback */
    if (NULL == (ret_value = (cls->attr_cls.create) 
                (obj, loc_params, name, acpl_id, aapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_open
 *
 * Purpose:	Opens an attribute through the VOL
 *
 * Return:      Success: pointer to the new attr. 
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_attr_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
               hid_t aapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;  /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == cls->attr_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'attr open' method")

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (cls->attr_cls.open) 
                (obj, loc_params, name, aapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "attribute open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_read
 *
 * Purpose:	Reads data from attr through the VOL
 *
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VL_attr_read(void *attr, const H5VL_class_t *cls, hid_t mem_type_id, void *buf, 
                      hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->attr_cls.read)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr read' method")
    if((ret_value = (cls->attr_cls.read)(attr, mem_type_id, buf, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "read failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_write
 *
 * Purpose:	Writes data to attr through the VOL
 *
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VL_attr_write(void *attr, const H5VL_class_t *cls, hid_t mem_type_id, const void *buf, 
                       hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->attr_cls.write)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr write' method")
    if((ret_value = (cls->attr_cls.write)(attr, mem_type_id, buf, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "write failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_get
 *
 * Purpose:	Get specific information about the attribute through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_get(void *obj, const H5VL_class_t *cls, H5VL_attr_get_t get_type, 
              hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->attr_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr get' method")

    va_start (arguments, req);
    if((ret_value = (cls->attr_cls.get)(obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_specific
 *
 * Purpose:	specific operation on attributes through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, 
                   H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->attr_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->attr_cls.specific)(obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute attribute specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->attr_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->attr_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute attribute optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_attr_close
 *
 * Purpose:     Closes an attribute through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_close(void *attr, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    HDassert(attr);
    HDassert(cls);

    FUNC_ENTER_NOAPI(FAIL)
            
    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->attr_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr close' method")

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->attr_cls.close)(attr, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_create
 *
 * Purpose:	Creates a dataset through the VOL
 *
 * Return:      Success: pointer to dataset
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_dataset_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                    hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == cls->dataset_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'dataset create' method")

    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (cls->dataset_cls.create)
                (obj, loc_params, name, dcpl_id, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_open
 *
 * Purpose:	Opens a dataset through the VOL
 *
 * Return:      Success: pointer to dataset 
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_dataset_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                  hid_t dapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == cls->dataset_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'dset open' method")

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (cls->dataset_cls.open)
                (obj, loc_params, name, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "dataset open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_read
 *
 * Purpose:	Reads data from dataset through the VOL
*
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_dataset_read(void *dset, const H5VL_class_t *cls, hid_t mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, void *buf,
    void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->dataset_cls.read)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dataset read' method")
    if((ret_value = (cls->dataset_cls.read)(dset, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_READERROR, FAIL, "read failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_write
 *
 * Purpose:	Writes data from dataset through the VOL
 *
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_dataset_write(void *dset, const H5VL_class_t *cls, hid_t mem_type_id, hid_t mem_space_id, 
                   hid_t file_space_id, hid_t plist_id, const void *buf, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->dataset_cls.write)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dataset write' method")
    if((ret_value = (cls->dataset_cls.write)
        (dset, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_WRITEERROR, FAIL, "write failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_get
 *
 * Purpose:	Get specific information about the dataset through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_get(void *dset, const H5VL_class_t *cls, H5VL_dataset_get_t get_type, 
                 hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->dataset_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dataset get' method")

    va_start (arguments, req);
    if((ret_value = (cls->dataset_cls.get)(dset, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_specific
 *
 * Purpose:	specific operation on datasets through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_specific(void *obj, const H5VL_class_t *cls, H5VL_dataset_specific_t specific_type, 
                       hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->dataset_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dataset specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->dataset_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute dataset specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->dataset_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dataset optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->dataset_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute dataset optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_dataset_close
 *
 * Purpose:     Closes a dataset through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_close(void *dset, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(dset);
    HDassert(cls);

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->dataset_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dset close' method")

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->dataset_cls.close)(dset, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_create
 *
 * Purpose:	Creates a file through the VOL
 *
 * Return:      Success: pointer to file. 
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_file_create(const H5VL_class_t *cls, const char *name, unsigned flags, hid_t fcpl_id, 
                 hid_t fapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == cls->file_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'file create' method")
    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (cls->file_cls.create)(name, flags, fcpl_id, fapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_open
 *
 * Purpose:	Opens a file through the VOL.
 *
 * Return:      Success: pointer to file. 
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_file_open(const H5VL_class_t *cls, const char *name, unsigned flags, hid_t fapl_id, 
               hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == cls->file_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'file open' method")
    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (cls->file_cls.open)(name, flags, fapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_get
 *
 * Purpose:	Get specific information about the file through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_get(void *file, const H5VL_class_t *cls, H5VL_file_get_t get_type, 
              hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->file_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'file get' method")

    va_start(arguments, req);
    if((ret_value = (cls->file_cls.get)(file, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end(arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_specific
 *
 * Purpose:	perform File specific operations through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_specific(void *file, const H5VL_class_t *cls, H5VL_file_specific_t specific_type, 
                   hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(specific_type == H5VL_FILE_IS_ACCESSIBLE) {
        H5P_genplist_t     *plist;          /* Property list pointer */
        H5VL_plugin_prop_t  plugin_prop;    /* Property for VOL plugin ID & info */
        va_list             tmp_args;       /* argument list passed from the API call */
        hid_t               fapl_id;

        va_start (tmp_args, req);
        fapl_id = va_arg (tmp_args, hid_t);
        va_end (tmp_args);

        /* get the VOL info from the fapl */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

        if(H5P_peek(plist, H5F_ACS_VOL_DRV_NAME, &plugin_prop) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get VOL plugin info")

        if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_prop.plugin_id, H5I_VOL)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

        va_start (arguments, req);
        if((ret_value = (cls->file_cls.specific)
            (file, specific_type, dxpl_id, req, arguments)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "specific failed")
        va_end (arguments);
    }
    else {
        if(NULL == cls->file_cls.specific)
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'file specific' method")

        va_start (arguments, req);
        if((ret_value = (cls->file_cls.specific)
            (file, specific_type, dxpl_id, req, arguments)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "specific failed")
        va_end (arguments);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_optional
 *
 * Purpose:	perform a plugin specific operation
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_optional(void *file, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->file_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'file optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->file_cls.optional)(file, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "optional failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_file_close
 *
 * Purpose:     Closes a file through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_close(void *file, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(file);
    HDassert(cls);

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->file_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'file close' method")

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->file_cls.close)(file, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEFILE, FAIL, "close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_create
 *
 * Purpose:	Creates a group through the VOL
 *
 * Return:      Success: pointer to new group.
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_group_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                  hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == cls->group_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'group create' method")

    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (cls->group_cls.create)
                (obj, loc_params, name, gcpl_id, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_open
 *
 * Purpose:	Opens a group through the VOL
 *
 * Return:      Success: pointer to new group.
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_group_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                hid_t gapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    if(NULL == cls->group_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'group open' method")

    if(NULL == (ret_value = (cls->group_cls.open)
                (obj, loc_params, name, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_get
 *
 * Purpose:	Get specific information about the group through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_get(void *obj, const H5VL_class_t *cls, H5VL_group_get_t get_type, 
               hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->group_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'group get' method")

    va_start (arguments, req);
    if((ret_value = (cls->group_cls.get)
        (obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_specific
 *
 * Purpose:	specific operation on groups through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_specific(void *obj, const H5VL_class_t *cls, H5VL_group_specific_t specific_type, 
                       hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->group_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'group specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->group_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute group specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->group_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'group optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->group_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute group optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_group_close
 *
 * Purpose:     Closes a group through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_close(void *grp, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    HDassert(grp);
    HDassert(cls);

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->group_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'group close' method")

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->group_cls.close)(grp, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_create
 *
 * Purpose:	Creates a hard link through the VOL
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_create(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params, 
                 const H5VL_class_t *cls, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == cls->link_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link create' method")
    /* call the corresponding VOL create callback */
    if((ret_value = (cls->link_cls.create)
        (create_type, obj, loc_params, lcpl_id, lapl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "link create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_copy
 *
 * Purpose:	Copys a link from src to dst.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_link_copy(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
               H5VL_loc_params_t loc_params2, const H5VL_class_t *cls,  
               hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check if the corresponding VOL copy callback exists */
    if(NULL == cls->link_cls.copy)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link copy' method")

    /* call the corresponding VOL copy callback */
    if((ret_value = (cls->link_cls.copy)
        (src_obj, loc_params1, dst_obj, loc_params2, lcpl_id, 
         lapl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "link copy failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_move
 *
 * Purpose:	Moves a link from src to dst.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_link_move(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
               H5VL_loc_params_t loc_params2, const H5VL_class_t *cls,  
               hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check if the corresponding VOL move callback exists */
    if(NULL == cls->link_cls.move)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link move' method")

    /* call the corresponding VOL move callback */
    if((ret_value = (cls->link_cls.move)
        (src_obj, loc_params1, dst_obj, loc_params2, lcpl_id, 
         lapl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "link move failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_get
 *
 * Purpose:	Get specific information about the link through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_get(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, H5VL_link_get_t get_type, 
              hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->link_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link get' method")

    va_start (arguments, req);
    if((ret_value = (cls->link_cls.get)
        (obj, loc_params, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_specific
 *
 * Purpose:	specific operation on links through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, 
                   H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->link_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->link_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute link specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->link_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->link_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute link optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_open
 *
 * Purpose:	Opens a object through the VOL
 *
 * Return:      Success: User ID of the new object. 
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_object_open(void *obj, H5VL_loc_params_t params, const H5VL_class_t *cls, H5I_type_t *opened_type,
                 hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL open callback exists */
    if(NULL == cls->object_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'object open' method")

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (cls->object_cls.open)
                (obj, params, opened_type, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_copy
 *
 * Purpose:	Copies an object to another destination through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const H5VL_class_t *cls1, const char *src_name, 
                 void *dst_obj, H5VL_loc_params_t loc_params2, const H5VL_class_t *cls2, const char *dst_name, 
                 hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Make sure that the VOL plugins are the same */
    if (cls1->value != cls2->value)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Objects are accessed through different VOL plugins and can't be linked")

    if(NULL == cls1->object_cls.copy)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'object copy' method")

    if((ret_value = (cls1->object_cls.copy)
        (src_obj, loc_params1, src_name, dst_obj, loc_params2, dst_name, ocpypl_id, 
         lcpl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "copy failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_get
 *
 * Purpose:	Get specific information about the object through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_get(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, H5VL_object_get_t get_type, 
                hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->object_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'object get' method")

    va_start (arguments, req);
    if((ret_value = (cls->object_cls.get)
        (obj, loc_params, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_specific
 *
 * Purpose:	specific operation on objects through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, 
                     H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->object_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'object specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->object_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "specific failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->object_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'object optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->object_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute object optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_commit
 *
 * Purpose:	Commits a datatype to the file through the VOL
 *
 * Return:      Success: Positive
 *
 *		Failure: Negative
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                     hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, 
                     hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL commit callback exists */
    if(NULL == cls->datatype_cls.commit)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'datatype commit' method")

    /* call the corresponding VOL commit callback */
    if(NULL == (ret_value = (cls->datatype_cls.commit) 
        (obj, loc_params, name, type_id, lcpl_id, tcpl_id, tapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "commit failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_commit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_open
 *
 * Purpose:	Opens a named datatype through the VOL
 *
 * Return:      Success: User ID of the datatype. 
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_datatype_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                   hid_t tapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == cls->datatype_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "no datatype open callback")

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (cls->datatype_cls.open)
                (obj, loc_params, name, tapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_datatype_get
 *
 * Purpose:     Get specific information about the datatype through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_get(void *obj, const H5VL_class_t *cls, H5VL_datatype_get_t get_type, 
                  hid_t dxpl_id, void **req, ...)
{
    va_list     arguments;             /* argument list passed from the API call */
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->datatype_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'datatype get' method")
    va_start (arguments, req);

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->datatype_cls.get)(obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_specific
 *
 * Purpose:	specific operation on datatypes through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_specific(void *obj, const H5VL_class_t *cls, H5VL_datatype_specific_t specific_type, 
                       hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->datatype_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'datatype specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->datatype_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute datatype specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->datatype_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'datatype optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->datatype_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute datatype optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_datatype_close
 *
 * Purpose:     Closes a datatype through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_close(void *dt, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->datatype_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'datatype close' method")

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->datatype_cls.close)(dt, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_request_cancel
 *
 * Purpose:     Cancels an asynchronous request through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_cancel(void **req, const H5VL_class_t *cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(req);
    HDassert(cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->async_cls.cancel)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'async cancel' method")

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->async_cls.cancel)(req, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request cancel failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_cancel() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_request_test
 *
 * Purpose:     Tests an asynchronous request through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_test(void **req, const H5VL_class_t *cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(req);
    HDassert(cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->async_cls.test)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'async test' method")

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->async_cls.test)(req, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request test failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_test() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_request_wait
 *
 * Purpose:     Waits on an asychronous request through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_wait(void **req, const H5VL_class_t *cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(req);
    HDassert(cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->async_cls.wait)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'async wait' method")

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->async_cls.wait)(req, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request wait failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_wait() */


