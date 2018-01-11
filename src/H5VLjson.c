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
 * Programmer:  Frank Willmore
 *              November, 2017
 *
 * Purpose:     An implementation of a VOL plugin to access HDF5 data in a
 *              JSON-oriented manner
 */

#define H5A_FRIEND      /* Suppress error about including H5Apkg */
#define H5D_FRIEND      /* Suppress error about including H5Dpkg */
#define H5F_FRIEND      /* Suppress error about including H5Fpkg */
#define H5G_FRIEND      /* Suppress error about including H5Gpkg */
#define H5I_FRIEND      /* Suppress error about including H5Ipkg */
#define H5L_FRIEND      /* Suppress error about including H5Lpkg */
#define H5O_FRIEND      /* Suppress error about including H5Opkg */
#define H5R_FRIEND      /* Suppress error about including H5Rpkg */
#define H5T_FRIEND      /* Suppress error about including H5Tpkg */

#include "H5private.h"    /* Generic Functions     */
#include "H5Apkg.h"       /* Attribute package     */
#include "H5Dpkg.h"       /* Dataset package       */
#include "H5Fpkg.h"       /* File package          */
#include "H5Gpkg.h"       /* Group package         */
#include "H5Ipkg.h"       /* Identifier package    */
#include "H5Lpkg.h"       /* Link package          */
#include "H5Opkg.h"       /* Object header package */
#include "H5Rpkg.h"       /* Reference package     */
#include "H5Tpkg.h"       /* Datatype package      */
#include "H5VLprivate.h"  /* VOL plugins           */
#include "H5VLjson.h"     /* JSON VOL plugin       */

#include "H5MMprivate.h"  /* Memory management     */

/*
 * The vol identification number.
 */
static hid_t H5VL_JSON_g = -1;

/* Internal initialization/termination functions which are called by
 * the public functions H5VLjson_init() and H5VLjson_term() */
static herr_t H5VL_json_init(void);
static herr_t H5VL_json_term(hid_t vtpl_id);

/* JSON VOL Attribute callbacks */
static void  *H5VL_json_attr_create(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req);
static void  *H5VL_json_attr_open(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t aapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_json_attr_read(void *attr, hid_t dtype_id, void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_json_attr_write(void *attr, hid_t dtype_id, const void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_json_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_attr_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_attr_close(void *attr, hid_t dxpl_id, void **req);

/* JSON VOL Dataset callbacks */
static void  *H5VL_json_dataset_create(void *_dataset, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
static void  *H5VL_json_dataset_open(void *_dataset, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_json_dataset_read(void *_dataset, hid_t mem_type_id, hid_t mem_space_id,
                                       hid_t file_space_id, hid_t dxpl_id, void *buf, void **req);
static herr_t H5VL_json_dataset_write(void *_dataset, hid_t mem_type_id, hid_t mem_space_id,
                                      hid_t file_space_id, hid_t dxpl_id, const void *buf, void **req);
static herr_t H5VL_json_dataset_get(void *_dataset, H5VL_dataset_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_dataset_specific(void *_dataset, H5VL_dataset_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_dataset_close(void *_dataset, hid_t dxpl_id, void **req);

/* JSON VOL Datatype callbacks */
static void  *H5VL_json_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req);
static void  *H5VL_json_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_json_datatype_get(void *dt, H5VL_datatype_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_datatype_close(void *dt, hid_t dxpl_id, void **req);

/* JSON VOL File callbacks */
static void  *H5VL_json_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
static void  *H5VL_json_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_json_file_get(void *_file, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_file_specific(void *_file, H5VL_file_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_file_optional(void *_file, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_file_close(void *_file, hid_t dxpl_id, void **req);

/* JSON VOL Group callbacks */
static void  *H5VL_json_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
static void  *H5VL_json_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_json_group_get(void *obj, H5VL_group_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_group_close(void *grp, hid_t dxpl_id, void **req);

/* JSON VOL Link callbacks */
static herr_t H5VL_json_link_create(H5VL_link_create_type_t create_type, void *obj,
                                    H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_json_link_copy(void *src_obj, H5VL_loc_params_t loc_params1,
                                  void *dst_obj, H5VL_loc_params_t loc_params2,
                                  hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_json_link_move(void *src_obj, H5VL_loc_params_t loc_params1,
                                  void *dst_obj, H5VL_loc_params_t loc_params2,
                                  hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_json_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_link_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);

/* JSON VOL Object callbacks */
static void  *H5VL_json_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, hid_t dxpl_id, void **req);
static herr_t H5VL_json_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name,
                                    void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name,
                                    hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_json_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_object_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_json_object_optional(void *obj, hid_t dxpl_id, void **req, va_list arguments);

/*** Helper functions ***/

/* generate UUIDs */
static herr_t h5json_uuid_generate(h5json_uuid_t uuid);
/* express time as UTC */
static herr_t h5json_get_utc_string_from_time(time_t t, char *time_buf);

/* converting between dataspace/datatype and their jansson representations */
static hid_t H5VL_json_jansson_to_dataspace(json_t* shape);
static hid_t H5VL_json_jansson_to_datatype(json_t* type);
static json_t* H5VL_json_datatype_to_jansson(hid_t datatype);
static json_t* H5VL_json_dataspace_to_jansson(hid_t dataspace);

/* write buffered values for an attribute or dataset to a jansson array */
static herr_t H5VL_json_write_value(json_t* value_array, hid_t dtype_id, hid_t space_id, void* buffer);
static herr_t H5VL_json_read_value(json_t* value_array, hid_t dtype_id, hid_t space_id, void* _buffer);

/* delete a link */
static herr_t H5VL_json_delete_link_from_containing_group(H5VL_json_object_t* domain, h5json_uuid_t containing_group_uuid, json_t* link);
static herr_t H5VL_json_insert_link_into_group(H5VL_json_object_t* domain, h5json_uuid_t containing_group_uuid, json_t* link);

/* locate a group */
static herr_t H5VL_json_create_new_group(H5VL_json_object_t* domain, const char* name, h5json_uuid_t *current_uuid, hbool_t create_intermediate);
static json_t* H5VL_json_find_object_by_name(H5VL_json_object_t* domain, const char* name, h5json_uuid_t *current_uuid, json_t** collection, h5json_uuid_t *containing_group_uuid);

/* creating a Dataset */
static herr_t H5VL_json_parse_dataset_create_options(void *parent_obj, const char *name, hid_t dcpl, char *create_request_body);
static herr_t H5VL_json_parse_dataset_create_shape_and_maxdims(hid_t space_id, char *shape_body, char *maxdims_body);
static herr_t H5VL_json_parse_dataset_creation_properties(hid_t dcpl_id, char *creation_properties_body);

/* creating an Attribute */
static herr_t H5VL_json_parse_attribute_create_options(H5VL_json_object_t *parent_obj, const char *name, hid_t acpl, char *create_request_body);

/*** VOL class ***/


static H5VL_class_t H5VL_json_g = {
    HDF5_VOL_JSON_VERSION,            /* Version number                 */
    H5_VOL_JSON,                      /* Plugin value                   */
    "JSON",                           /* Plugin name                    */
    NULL,                             /* Plugin initialization function */
    H5VL_json_term,                   /* Plugin termination function    */
    0,                                /* Plugin info FAPL size          */
    NULL,                             /* Plugin FAPL copy function      */
    NULL,                             /* Plugin FAPL free function      */
    {
        H5VL_json_attr_create,        /* Attribute create function      */
        H5VL_json_attr_open,          /* Attribute open function        */
        H5VL_json_attr_read,          /* Attribute read function        */
        H5VL_json_attr_write,         /* Attribute write function       */
        H5VL_json_attr_get,           /* Attribute get function         */
        H5VL_json_attr_specific,      /* Attribute specific function    */
        NULL,                         /* Attribute optional function    */
        H5VL_json_attr_close          /* Attribute close function       */
    },
    {
        H5VL_json_dataset_create,     /* Dataset create function        */
        H5VL_json_dataset_open,       /* Dataset open function          */
        H5VL_json_dataset_read,       /* Dataset read function          */
        H5VL_json_dataset_write,      /* Dataset write function         */
        H5VL_json_dataset_get,        /* Dataset get function           */
        H5VL_json_dataset_specific,   /* Dataset specific function      */
        NULL,                         /* Dataset optional function      */
        H5VL_json_dataset_close       /* Dataset close function         */
    },
    {
        H5VL_json_datatype_commit,    /* Datatype commit function       */
        H5VL_json_datatype_open,      /* Datatype open function         */
        H5VL_json_datatype_get,       /* Datatype get function          */
        NULL,                         /* Datatype specific function     */
        NULL,                         /* Datatype optional function     */
        H5VL_json_datatype_close      /* Datatype close function        */
    },
    {
        H5VL_json_file_create,        /* File create function           */
        H5VL_json_file_open,          /* File open function             */
        H5VL_json_file_get,           /* File get function              */
        H5VL_json_file_specific,      /* File specific function         */
        H5VL_json_file_optional,      /* File optional function         */
        H5VL_json_file_close          /* File close function            */
    },
    {
        H5VL_json_group_create,       /* Group create function          */
        H5VL_json_group_open,         /* Group open function            */
        H5VL_json_group_get,          /* Group get function             */
        NULL,                         /* Group specific function        */
        NULL,                         /* Group optional function        */
        H5VL_json_group_close         /* Group close function           */
    },
    {
        H5VL_json_link_create,        /* Link create function           */
        H5VL_json_link_copy,          /* Link copy function             */
        H5VL_json_link_move,          /* Link move function             */
        H5VL_json_link_get,           /* Link get function              */
        H5VL_json_link_specific,      /* Link specific function         */
        NULL                          /* Link optional function         */
    },
    {
        H5VL_json_object_open,        /* Object open function           */
        H5VL_json_object_copy,        /* Object copy function           */
        H5VL_json_object_get,         /* Object get function            */
        H5VL_json_object_specific,    /* Object specific function       */
        H5VL_json_object_optional     /* Object optional function       */
    },
    {
        NULL,
        NULL,
        NULL
    },
    NULL
};

#if 0
//FTW some debugging utils
void FTW_dump_dataspace(hid_t dataspace, const char* message)
{
    printf("__________________________________________________\n");
    printf("FTW_dump_dataspace(): %s\n", message);
    printf("FTW_dump_dataspace(): space id = %ld\n", dataspace);
    int ndims = H5Sget_simple_extent_ndims( dataspace );
    hsize_t* _dims = malloc(ndims * sizeof(hsize_t));
    hsize_t* _maxdims = malloc(ndims * sizeof(hsize_t));
    H5Sget_simple_extent_dims(dataspace, _dims, _maxdims );
    printf("got ndims = %d, dims[0] = %d, maxdims[0] = %d\n", ndims, _dims[0], _maxdims[0]);
    free(_dims);
    free(_maxdims);
    printf("__________________________________________________\n");
}

//FTW generic to grab object json, but needs to get VOL object from library object. 
char* H5VLjson_dumps(void* _object)
{
    return json_dumps(((H5VL_json_object_t*)_object)->object_json, JSON_INDENT(4));
}
#endif


/*-------------------------------------------------------------------------
 * Function:    H5VLjson_init
 *
 * Purpose:     Initialize the JSON VOL plugin by 
 *              registering the plugin with the library
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Frank Willmore 
 *              November, 2017
 */
herr_t
H5VLjson_init(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE0("e","");

    /* Check if already initialized */
    if (H5VL_JSON_g >= 0)
        HGOTO_DONE(SUCCEED)

    /* FTW: JANSSON allows registration of specific malloc and free functions to be used, this would be the place to do so. */

    /* Register the plugin with the library */
    if (H5VL_json_init() < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to initialize JSON VOL plugin")

done:
    /* Cleanup if JSON VOL plugin initialization failed */
    if (ret_value < 0) 
    {
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5VLjson_init() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_init
 *
 * Purpose:     Register the JSON VOL plugin with the library
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Frank Willmore
 *              November, 2017
 */
static herr_t
H5VL_json_init(void)
{
    herr_t ret_value = SUCCEED;

#ifdef PLUGIN_DEBUG
    printf("initializing JSON VOL\n");
#endif

    FUNC_ENTER_NOAPI(FAIL)

    /* Register the JSON VOL plugin, if it isn't already registered */
    if (NULL == H5I_object_verify(H5VL_JSON_g, H5I_VOL)) {
        if ((H5VL_JSON_g = H5VL_register((const H5VL_class_t *) &H5VL_json_g, sizeof(H5VL_class_t), TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTINSERT, FAIL, "can't create ID for JSON VOL plugin")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_init() */


/*-------------------------------------------------------------------------
 * Function:    H5VLjson_term
 *
 * Purpose:     Shut down the JSON VOL plugin
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Frank Willmore
 *              November, 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLjson_term(void)
{
    herr_t ret_value = SUCCEED;

#ifdef PLUGIN_DEBUG
    printf("terminating JSON VOL\n");
#endif

    FUNC_ENTER_API(FAIL)
    H5TRACE0("e","");

    if (H5VL_json_term(-1) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CLOSEERROR, FAIL, "can't close JSON VOL plugin")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLjson_term() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_term
 *
 * Purpose:     Shut down the JSON VOL plugin
 *
 * Return:      SUCCEED (can't fail)
 *
 * Programmer:  Frank Willmore
 *              December, 2017
 */
static herr_t
H5VL_json_term(hid_t H5_ATTR_UNUSED vtpl_id)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Reset ID */
    H5VL_JSON_g = -1;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_json_term() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_json_vol
 *
 * Purpose:     Modify the file access property list to use the JSON VOL
 *              plugin
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Frank Willmore
 *              October, 2017
 */
herr_t
H5Pset_fapl_json_vol(hid_t fapl_id)
{
    H5P_genplist_t *plist;
    herr_t          ret_value;

#ifdef PLUGIN_DEBUG
    printf("setting fapl json vol\n");
#endif

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", fapl_id);

    if (H5VL_JSON_g < 0)
        HGOTO_ERROR(H5E_VOL, H5E_UNINITIALIZED, FAIL, "JSON VOL plugin not initialized")

    if (H5P_DEFAULT == fapl_id)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set JSON VOL plugin for default property list")

    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    if ((ret_value = H5P_set_vol(plist, H5VL_JSON_g, NULL)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to set JSON VOL plugin in FAPL")

done:
    FUNC_LEAVE_API(ret_value)

} /* end H5Pset_fapl_json_vol() */


/****************************************
 *         VOL plugin callbacks         *
 ****************************************/

 /*** Attributes API ***/



/* -----------------------------------------------------------
 * Function:    H5VL_json_attr_create 
 *
 * Purpose:     Creates a local representation of an attribute, plus a 
 *              holder on the JSON server 
 *
 * Return:      Non-negative, valid attribute handile on success/Negative on 
 *              failure
 *
 * Programmer:  Frank Willmore 
 *              September, 2017
 */
static void *
H5VL_json_attr_create(void *_parent, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id,
                      hid_t H5_ATTR_UNUSED aapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *parent = (H5VL_json_object_t *) _parent;
    H5VL_json_object_t *attribute = NULL;
    H5P_genplist_t     *plist = NULL;
    htri_t              search_ret;
    hid_t               type_id, space_id, lcpl_id;
    void               *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Attribute create call with following parameters:\n");
    printf("  - Attribute Name: %s\n", attr_name);
    printf("  - AAPL: %ld\n", aapl_id);
    printf("  - Parent Object UUID: %s\n", parent->object_uuid);
    printf("  - Parent Object Type: %d\n", parent->obj_type);
#endif

    /* There's always one... */
    HDassert(strlen(attr_name) < ATTR_NAME_MAX_LENGTH);

    /* Check for write access */
    if(!(parent->domain->u.file.intent & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "no write intent on file")

    //FTW verify parent is group or dataset, attribute is not implemented for other types yet
    /*  || H5I_FILE == parent->obj_type || H5I_DATATYPE == parent->obj_type */
    HDassert(( H5I_GROUP == parent->obj_type 
            || H5I_FILE == parent->obj_type 
            || H5I_DATASET == parent->obj_type) 
            && "parent object not a dataset, file, datatype, or group");

    /* Get the acpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* Allocate and setup internal Attribute struct */
    if (NULL == (attribute = (H5VL_json_object_t *) H5MM_malloc(sizeof(*attribute))))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, NULL, "couldn't allocate attribute object")
    attribute->obj_type = H5I_ATTR;

    /* Attribute is not first-class object and has no UUID */
    strcpy(attribute->object_uuid, "attributes have no UUID"); 

    /* Store pointer to domain/file that the opened Attribute is within */
    attribute->domain = parent->domain; 

    /* Get the acpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* The attribute's parent object is, of course, the passed parent object */
    attribute->u.attribute.parent_obj = parent;

    /* Store the attribute name in the newly allocated object. */
    size_t attr_name_len = strlen(attr_name);
    HDassert (attr_name_len < ATTR_NAME_MAX_LENGTH);
    strcpy(attribute->u.attribute.attr_name, attr_name);

    /* finish setting up the library object */

    /* Get the type ID */
    if (H5Pget(acpl_id, H5VL_PROP_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for datatype ID")
    if ((attribute->u.attribute.dtype_id = H5Tcopy(type_id)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, NULL, "failed to copy datatype")

    /* Get the space ID */
    if (H5Pget(acpl_id, H5VL_PROP_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for dataspace ID")
    if ((attribute->u.attribute.space_id = H5Scopy(space_id)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, NULL, "failed to copy dataspace")

    /*** JANSSON object ***/

    /* get the attribute_collection from the parent object */
    json_t* attribute_collection = json_object_get(parent->object_json, "attributes");
    json_t* attribute_json = json_object();
    json_array_append_new(attribute_collection, attribute_json);

    /* flesh up the attribute_json object */
    json_object_set_new(attribute_json, "name", json_string(attr_name));
    json_object_set_new(attribute_json, "shape", H5VL_json_dataspace_to_jansson(space_id));
    json_object_set_new(attribute_json, "type", H5VL_json_datatype_to_jansson(type_id));

    /*** value ***/

    /* create an empty array */
    json_object_set_new(attribute_json, "value", json_array());

    /* FTW default value is set to json null. If H5D_FILL_TIME_IFSET were 
     * implemented, a fill value *could* be provided here. */

    /* Use the parent obj to find the group to find the linklist where the new dataset id needs to be added. */
    json_t* parent_uuid;
    switch (parent->obj_type)
    {
        case H5I_FILE:
            parent_uuid = parent->object_uuid;
            break;
        case H5I_GROUP:
            parent_uuid = parent->object_uuid;
            break;
        case H5I_DATASET:
            parent_uuid = parent->object_uuid;
            break;
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype of attribute parent")
    } /* end switch */

#ifdef PLUGIN_DEBUG
    printf("Attribute H5VL_json_object_t fields:\n");
    printf("  - Attribute UUID: %s\n", attribute->object_uuid);
    printf("  - Attribute Object type: %d\n", attribute->obj_type);
#endif

    /* all is okay after request so set the return value */
    ret_value = (void *) attribute;

done:
    
    /* Clean up allocated dataset object if there was an issue */
    if (attribute && !ret_value)
        if (H5VL_json_attr_close(attribute, FAIL, NULL) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTCLOSEOBJ, NULL, "unable to close attribute")

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_attr_create() */


/*------------------------------------------------------------
 * Function:    H5VL_json_attr_open
j *
 * Purpose:     Open an existing attribute object and retrieve dataspace and datatype info. 
 *
 * Return:      Non-negative, valid attribute handile on success/Negative on failure
 *
 * Programmer:  Frank Willmore 
 *              September, 2017
 *
 */

static void *
H5VL_json_attr_open(void *_parent, H5VL_loc_params_t loc_params, const char *attr_name,
                    hid_t H5_ATTR_UNUSED aapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *parent = (H5VL_json_object_t *) _parent;
    H5VL_json_object_t *attribute = NULL;
    htri_t              search_ret;
    void               *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Attribute open call with following parameters:\n");
    printf("  - Attribute Name: %s\n", attr_name);
    printf("  - AAPL: %ld\n", aapl_id);
    printf("  - Parent Object Type: %d\n", parent->obj_type);
#endif

    HDassert((H5I_GROUP == parent->obj_type 
            || H5I_DATASET == parent->obj_type 
            || H5I_FILE == parent->obj_type)
            && "parent object not a dataset, file, or group");

    /* Allocate and setup internal Attribute struct */
    if (NULL == (attribute = (H5VL_json_object_t *) H5MM_malloc(sizeof(*attribute))))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, NULL, "couldn't allocate attribute object")

    /* Finish setting up allocated Attribute struct */
    attribute->obj_type = H5I_ATTR;
    attribute->domain = parent->domain; /* Store pointer to file that the opened Attribute is within */

    /* The attribute's parent object is, of course, the passed parent object */
    attribute->u.attribute.parent_obj = parent;

    /* Store the attribute name in the newly allocated object. */
    size_t attr_name_len = strlen(attr_name);
    HDassert (attr_name_len < ATTR_NAME_MAX_LENGTH);
    strcpy(attribute->u.attribute.attr_name, attr_name);

#ifdef PLUGIN_DEBUG
    /* Attribute requires the parent UUID and attribute name to do it's business */
    printf("Got UUID for attribute parent: %s\n", attribute->u.attribute.parent_obj->object_uuid);
    printf("Got attribute_name for attribute: %s\n", attribute->u.attribute.attr_name);
#endif

    /* iterate the colection to find the named attribute 
     * and set a pointer to it in JANSSON */
    json_t* attribute_collection = json_object_get(parent->object_json, "attributes");
    const char* index;
    json_t* value_in_array;
    hbool_t found = false;
    json_array_foreach(attribute_collection, index, value_in_array) 
    {
        /* block of code that uses key and value */
        char* name = json_string_value(json_object_get(value_in_array, "name"));
        if (strcmp(attr_name, name) == 0) 
        {
            attribute->object_json = value_in_array;
            found = true;
            break;
        }
    }

    if (!found) HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, NULL, "Unable to locate attribute.")

    json_t* type = json_object_get(attribute->object_json, "type");
    HDassert(type != NULL);
    json_t* shape = json_object_get(attribute->object_json, "shape");
    HDassert(shape != NULL);
    json_t* value = json_object_get(attribute->object_json, "value");
    HDassert(value != NULL);

    attribute->u.attribute.space_id = H5VL_json_jansson_to_dataspace(shape);
    attribute->u.attribute.dtype_id = H5VL_json_jansson_to_datatype(type);
    strcpy(attribute->u.attribute.attr_name, attr_name);

    ret_value = (void *) attribute;

done:
    /* Clean up allocated attribute object if there was an issue */
    if (attribute && !ret_value)
        H5MM_xfree(attribute);

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_attr_open() */


static herr_t
H5VL_json_attr_read(void *_attribute, hid_t dtype_id, void *buf, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *attribute = (H5VL_json_object_t *) _attribute;
    H5T_class_t         dtype_class;
    size_t              dtype_size;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Attribute read call with following parameters:\n");
    printf("  - Attribute parent UUID: %s\n", attribute->u.attribute.parent_obj->object_uuid);
    printf("  - Attribute name: %s\n", attribute->u.attribute.attr_name);
    printf("  - DXPL ID: %ld\n", dxpl_id);
    printf("  - is default dxpl: %s\n", (dxpl_id == H5P_DEFAULT) ? "true" : "false");
#endif

    /* check for valid datatype */
    if (H5T_NO_CLASS == (dtype_class = H5Tget_class(dtype_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

//FTW    if ((is_variable_str = H5Tis_variable_str(dtype_id)) < 0)
//FTW        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

    if (0 == (dtype_size = H5Tget_size(dtype_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

    /* for attribute, the mem_space selection is always H5S_ALL */
    hid_t space_id = H5Scopy(attribute->u.attribute.space_id);
    HDassert(space_id);
    H5Sselect_all(space_id);

    /*** reading the value ***/

    json_t* value = json_object_get(attribute->object_json, "value");
    H5VL_json_read_value(value, dtype_id, space_id, buf);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_attr_read() */


static herr_t 
H5VL_json_attr_write(void *_attribute, hid_t dtype_id, const void *buf, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *attribute = (H5VL_json_object_t *) _attribute;
    H5T_class_t         dtype_class;
    htri_t              is_variable_str;
    size_t              dtype_size;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Attribute write call with following parameters:\n");
    printf("  - Attribute parent UUID: %s\n", attribute->u.attribute.parent_obj->object_uuid);
    printf("  - Attribute name: %s\n", attribute->u.attribute.attr_name);
    printf("  - DXPL ID: %ld\n", dxpl_id);
    printf("  - is default dxpl: %s\n", (dxpl_id == H5P_DEFAULT) ? "true" : "false");
#endif

    /* Check for write access */
    if(!(attribute->domain->u.file.intent & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "no write intent on file")

    /* check for valid write buffer and object type */
    HDassert(buf);
    HDassert(H5I_ATTR == attribute->obj_type && "not an attribute");

    /* check for valid datatype */
    if (H5T_NO_CLASS == (dtype_class = H5Tget_class(dtype_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

    if ((is_variable_str = H5Tis_variable_str(dtype_id)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

    if (0 == (dtype_size = H5Tget_size(dtype_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

    /* for attribute, the mem_space selection is always H5S_ALL */
    hid_t space_id = H5Scopy(attribute->u.attribute.space_id);
    HDassert(space_id);
    H5Sselect_all(space_id);

    /*** writing the value ***/

    json_t* value = json_object_get(attribute->object_json, "value");
    H5VL_json_write_value(value, dtype_id, space_id, buf);

done:

    if (H5Sclose(space_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTCLOSEOBJ, FAIL, "can't close space")

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_attr_write() */


static herr_t
H5VL_json_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_attr_get() */


static herr_t
H5VL_json_attr_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_attr_specific_t specific_type,
                        hid_t dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_attr_specific() */


/* -----------------------------------------------------------
 * Function:    H5VL_json_attr_close 
 *
 * Purpose:     Close attribute and free memory. There is no interaction with 
 *              server, whose state is unchanged. 
 *
 * Return:      Non-negative, valid attribute handile on success/Negative on 
 *              failure
 *
 * Programmer:  Frank Willmore 
 *              September, 2017
 */

static herr_t
H5VL_json_attr_close(void *_attr, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *attr = (H5VL_json_object_t *) _attr;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Attribute close call with following parameters:\n");
    printf("  - parent UUID: %s\n", attr->u.attribute.parent_obj->object_uuid);
    printf("  - attribute name: %s\n", attr->u.attribute.attr_name);
    printf("  - DXPL: %ld\n\n", dxpl_id);
#endif

    HDassert(H5I_ATTR == attr->obj_type && "not an attribute");

    if (attr->u.attribute.dtype_id != FAIL && H5Tclose(attr->u.attribute.dtype_id) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close datatype")
    if (attr->u.attribute.space_id != FAIL && H5Sclose(attr->u.attribute.space_id) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close dataspace")

    H5MM_xfree(attr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_attr_close() */


/*** Datatypes API ***/



//FTW datatypes not started, 22 Dec 2017
static void *
H5VL_json_datatype_commit(void *obj, H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name, hid_t type_id,
                          hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *parent = (H5VL_json_object_t *) obj;
    H5VL_json_object_t *new_datatype = NULL;
    void               *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Datatype commit call with following parameters:\n");
    printf("  - Name: %s\n", name);
    printf("  - Type ID: %ld\n", type_id);
    printf("  - LCPL: %ld\n", lcpl_id);
    printf("  - TCPL: %ld\n", tcpl_id);
    printf("  - TAPL: %ld\n", tapl_id);
    printf("  - DXPL: %ld\n", dxpl_id);
    printf("  - Parent Object UUID: %s\n", parent->object_uuid);
    printf("  - Parent Object type: %d\n\n", parent->obj_type);
#endif

    /* Check for write access */
    if(!(parent->domain->u.file.intent & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "no write intent on file")

    HDassert((H5I_FILE == parent->obj_type || H5I_GROUP == parent->obj_type)
            && "parent object not a file or group");

    if (NULL == (new_datatype = (H5VL_json_object_t *) H5MM_malloc(sizeof(*new_datatype))))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, NULL, "can't allocate datatype object")

    new_datatype->obj_type = H5I_DATATYPE;
    new_datatype->domain = parent->domain; /* Store pointer to file that the newly-committed datatype is in */
    new_datatype->u.datatype.dtype_id = FAIL;
    new_datatype->u.datatype.tcpl_id = FAIL;

    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, NULL, "datatype_commit not yet implemented.")

#ifdef PLUGIN_DEBUG
    printf("Datatype H5VL_json_object_t fields:\n");
    printf("  - Datatype UUID: %s\n", new_datatype->object_uuid);
    printf("  - Datatype Object type: %d\n", new_datatype->obj_type);
    //printf("  - Datatype Parent Domain path: %s\n", new_datatype->domain->u.file.filepath_name);
#endif

    ret_value = (void *) new_datatype;

done:

    /* Clean up allocated datatype object if there was an issue */
    if (new_datatype && !ret_value)
        if (H5VL_json_datatype_close(new_datatype, FAIL, NULL) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, NULL, "unable to close datatype")

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_datatype_commit() */


static void *
H5VL_json_datatype_open(void *_parent, H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
                        hid_t tapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *parent = (H5VL_json_object_t *) _parent;
    H5VL_json_object_t *datatype = NULL;
    htri_t              search_ret;
    void               *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Datatype open call with following parameters:\n");
    printf("  - Name: %s\n", name);
    printf("  - TAPL: %ld\n", tapl_id);
    printf("  - DXPL: %ld\n", dxpl_id);
    printf("  - Parent object type: %d\n", parent->obj_type);
#endif

    HDassert((H5I_FILE == parent->obj_type || H5I_GROUP == parent->obj_type)
            && "parent object not a file or group");

    /* Allocate and setup internal Datatype struct */
    if (NULL == (datatype = (H5VL_json_object_t *) H5MM_malloc(sizeof(*datatype))))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, NULL, "can't allocate datatype object")

    datatype->obj_type = H5I_DATATYPE;
    datatype->domain = parent->domain;
    datatype->u.datatype.dtype_id = FAIL;
    datatype->u.datatype.tcpl_id = FAIL;

        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, NULL, "datatype open not implemented")

    /* Traverse links until the named Datatype is found */
    //    search_ret = H5VL_json_find_link_by_path(parent, name, yajl_copy_object_URI_parse_callback, NULL, datatype->URI);
    //    if (!search_ret || search_ret < 0)
    //        HGOTO_ERROR(H5E_DATATYPE, H5E_PATH, NULL, "unable to locate datatype by path")

    /* Set up the actual datatype by converting the string representation into an hid_t */
    //    if (H5VL_json_parse_datatype(datatype) < 0)
    //        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, NULL, "unable to parse dataset's datatype")

#ifdef PLUGIN_DEBUG
    printf("Datatype H5VL_json_object_t fields:\n");
    printf("  - Datatype UUID: %s\n", datatype->object_uuid);
    printf("  - Datatype object type: %d\n", datatype->obj_type);
    //    printf("  - Datatype Parent Domain path: %s\n\n", datatype->domain->u.file.filepath_name);
#endif

    ret_value = (void *) datatype;

done:
    /* Clean up allocated datatype object if there was an issue */
    if (datatype && !ret_value)
        if (H5VL_json_datatype_close(datatype, FAIL, NULL) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, NULL, "unable to close datatype")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_datatype_open() */


static herr_t
H5VL_json_datatype_get(void *_dtype, H5VL_datatype_get_t get_type, hid_t H5_ATTR_UNUSED dxpl_id,
                       void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_json_object_t *dtype = (H5VL_json_object_t *) _dtype;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Datatype get call with following parameters:\n");
    printf("  - Get Type: %d\n", get_type);
    printf("  - Datatype object UUID: %s\n", dtype->object_uuid);
#endif

    HDassert(H5I_DATATYPE == dtype->obj_type && "not a datatype");

    switch (get_type) {
        case H5VL_DATATYPE_GET_BINARY:
        {
            ssize_t *nalloc = va_arg(arguments, ssize_t *);
            void    *buf = va_arg(arguments, void *);
            size_t   size = va_arg(arguments, size_t);

            if (H5Tencode(dtype->u.datatype.dtype_id, buf, &size) < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't determine serialized length of datatype")

            *nalloc = (ssize_t) size;

            break;
        } /* H5VL_DATATYPE_GET_BINARY */

        /* XXX: Unable to support until property lists are ironed out with HSDS */
        /* H5Tget_create_plist */
        case H5VL_DATATYPE_GET_TCPL:
        {
            hid_t *plist_id = va_arg(arguments, hid_t *);

            /* Retrieve the datatype's creation property list */
            if((*plist_id = H5Pcopy(dtype->u.datatype.tcpl_id)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get datatype creation property list")

            break;
        } /* H5VL_DATATYPE_GET_TCPL */

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get this type of information from datatype")
    } /* end switch */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_datatype_get() */


static herr_t
H5VL_json_datatype_close(void *_dtype, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *dtype = (H5VL_json_object_t *) _dtype;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Datatype close call with following parameters:\n");
    printf("  - UUID: %s\n", dtype->object_uuid);
#endif

    HDassert(H5I_DATATYPE == dtype->obj_type && "not a datatype");

    if (dtype->u.datatype.dtype_id != FAIL && H5Tclose(dtype->u.datatype.dtype_id) < 0)
        HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "can't close datatype")

    H5MM_xfree(dtype);

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_datatype_close() */


/*** Datasets API ***/



/*-------------------------------------------------------------------------
 * Function:    H5VL_json_dataset_create
 *
 * Purpose:     Creates an HDF5 dataset using the JSON API
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Frank Willmore
 *              December, 2017
 *
 */
static void *
H5VL_json_dataset_create(void *_parent, H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name, hid_t dcpl_id,
                         hid_t dapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t* parent = (H5VL_json_object_t *) _parent;
    H5VL_json_object_t* new_dataset = NULL;
    H5P_genplist_t*     plist = NULL;
    hid_t               space_id, type_id;
    void*               ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Dataset create call with following parameters:\n");
    printf("  - Name: %s\n", name);
    printf("  - DCPL: %ld\n", dcpl_id);
    printf("  - DAPL: %ld\n", dapl_id);
    printf("  - DXPL: %ld\n", dxpl_id);
    printf("  - Parent Object UUID: %s\n", parent->object_uuid);
    printf("  - Parent Object Type: %d\n\n", parent->obj_type);
#endif

    /* Check for write access */
    if(!(parent->domain->u.file.intent & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "no write intent on file")

    HDassert((H5I_FILE == parent->obj_type || H5I_GROUP == parent->obj_type)
            && "parent object not a file or group");

    /* Allocate and setup internal Dataset struct */
    if (NULL == (new_dataset = (H5VL_json_object_t *) H5MM_malloc(sizeof(*new_dataset))))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, NULL, "can't allocate dataset object")

    new_dataset->obj_type = H5I_DATASET;
    new_dataset->domain = parent->domain; /* Store pointer to file that the newly-created dataset is in */
    new_dataset->u.dataset.dtype_id = FAIL;
    new_dataset->u.dataset.space_id = FAIL;

    /* Get the type and shape for the dataset from the dcpl.
     * The attributes field will be empty [] and the value field will be null. */
    if (NULL == (plist = (H5P_genplist_t *) H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't get dataset creation property list")

    if (H5P_get(plist, H5VL_PROP_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property list value for datatype ID")
    if (H5P_get(plist, H5VL_PROP_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property list value for space ID")

    if ((new_dataset->u.dataset.dtype_id = H5Tcopy(type_id)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, NULL, "failed to copy datatype")
    if ((new_dataset->u.dataset.space_id = H5Scopy(space_id)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, NULL, "failed to copy dataspace")

    /* create a new uuid for this dataset */
    h5json_uuid_generate(new_dataset->object_uuid);

    /* Now the persistent JANSSON representation */

    /* create a new JANSSON object and attach it to the library object */
    new_dataset->object_json = json_object();

    /* grab the dataset collection from the Domain/File */
    json_t* dataset_collection = json_object_get(new_dataset->domain->u.file.json_file_object, "datasets");
    json_object_set_new(dataset_collection, new_dataset->object_uuid, new_dataset->object_json);

    /* flesh out these fields */

    /* fill in the dataset fields */
    json_t* attributes = json_array();
    json_object_set_new(new_dataset->object_json, "attributes", attributes);

    /* shape:  First get the dataspace class/type */
    json_object_set_new(new_dataset->object_json, "shape", H5VL_json_dataspace_to_jansson(space_id));

    /* type: get the datatype info */
    json_object_set_new(new_dataset->object_json, "type", H5VL_json_datatype_to_jansson(type_id));
    
    /*** value ***/
    json_t* value_array = json_array();
    json_object_set_new(new_dataset->object_json, "value", value_array);

    /* default value is empty array. If H5D_FILL_TIME_IFSET then 
     * use the fill value provided. */

    /* get the dims from the space */
    int n_dims = H5Sget_simple_extent_ndims( space_id );
    hsize_t* dims = (hsize_t*)H5MM_malloc(sizeof(hsize_t) * n_dims);
    hsize_t* maxdims = (hsize_t*)H5MM_malloc(sizeof(hsize_t) * n_dims);
    H5Sget_simple_extent_dims(space_id, dims, maxdims );

    unsigned long n_points = dims[0];
    for (unsigned d=1; d<n_dims; d++) n_points *= dims[d];

    int* buffer = (int*)H5MM_malloc(n_points * sizeof(int));

    H5MM_xfree(dims);
    H5MM_xfree(maxdims);

    H5D_fill_time_t fill_time;
    if (H5Pget_fill_time(dcpl_id, &fill_time) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve fill time property")
    
    if (fill_time != H5D_FILL_TIME_NEVER)
    {
        H5D_fill_value_t fill_status;

        if (H5Pfill_value_defined(dcpl_id, &fill_status) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve the fill value defined status")

        long fill_value = NULL;
        herr_t err = H5Pget_fill_value( dcpl_id, H5T_NATIVE_INT, &fill_value );
        
        for (unsigned i=0; i<n_points; i++) buffer[i] = fill_value;

        /* write the fill value */
        H5VL_json_write_value(value_array, type_id, space_id, buffer);
    }

    /*** done with fill value ***/

    H5MM_xfree(buffer);

    /* Use the parent obj to find the group to find the linklist where the new dataset id needs to be added. */
    json_t* parent_uuid;
    switch (parent->obj_type)
    {
        case H5I_FILE:
            /* if starting with file, grab id of the root group, and move on. */
            parent_uuid = json_string_value(json_object_get(new_dataset->domain->u.file.json_file_object, "root"));
            break;
        case H5I_GROUP:
            parent_uuid = parent->object_uuid;
            break;
        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "Not a valid parent object type.")
    }

    /* insert a new link into the groups linklist */
    
    json_t* group_hashtable = json_object_get(new_dataset->domain->u.file.json_file_object, "groups");
    json_t* group = json_object_get(group_hashtable, parent_uuid);

    /* create the link */
    json_t* link = json_object();
    json_object_set_new(link, "class", json_string("H5L_TYPE_HARD"));
    json_object_set_new(link, "title", json_string(name));
    json_object_set_new(link, "collection", json_string("datasets"));
    json_object_set_new(link, "id", json_string(new_dataset->object_uuid)); 

    /* add the link to the group's link collection */
    json_t* link_collection = json_object_get(group, "links");
    json_array_append(link_collection, link); 

#ifdef PLUGIN_DEBUG
    printf("Dataset H5VL_json_object_t fields:\n");
    printf("  - Dataset UUID: %s\n", new_dataset->object_uuid);
    printf("  - Dataset Object type: %d\n", new_dataset->obj_type);
#endif

    ret_value = (void *) new_dataset;

done:
    /* Clean up allocated dataset object if there was an issue */
    if (new_dataset && !ret_value)
        if (H5VL_json_dataset_close(new_dataset, FAIL, NULL) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, NULL, "unable to close dataset")

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_dataset_open
 *
 * Purpose:     Opens an HDF5 dataset using the JSON API
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Frank Willmore
 *              November, 2017
 *
 */
static void *
H5VL_json_dataset_open(void *_parent, H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
                       hid_t dapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *parent = (H5VL_json_object_t *) _parent;
    H5VL_json_object_t *dataset = NULL;
    htri_t              search_ret;
    void               *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Dataset open call with following parameters:\n");
    printf("  - Dataset Name: %s\n", name);
    printf("  - DAPL: %ld\n", dapl_id);
    printf("  - DXPL: %ld\n", dxpl_id);
    printf("  - Parent Object Type: %d\n", parent->obj_type);
#endif

    HDassert((H5I_FILE == parent->obj_type || H5I_GROUP == parent->obj_type)
            && "parent object not a file or group");

    /* Allocate and setup internal Dataset struct */
    if (NULL == (dataset = (H5VL_json_object_t *) H5MM_malloc(sizeof(*dataset))))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, NULL, "can't allocate dataset object")

    dataset->obj_type = H5I_DATASET;
    dataset->domain = parent->domain; 

    /* Get the named dataset in JANSSON */
    json_t* groups = json_object_get(parent->object_json, "groups");
    json_t* parent_group = json_object_get(groups, parent->object_uuid);
    json_t* parent_group_links = json_object_get(parent_group, "links");

    const char *index;
    json_t *value;
    h5json_uuid_t* uuid = NULL;

    json_array_foreach(parent_group_links, index, value) 
    {
        /* block of code that uses key and value */
        h5json_uuid_t* title = json_string_value(json_object_get(value, "title"));
        if (strcmp(title, name) == 0) 
        {
            uuid = (h5json_uuid_t*)json_string_value(json_object_get(value, "id"));
            strncpy(dataset->object_uuid, uuid, sizeof(h5json_uuid_t));
            break;
        }
    }

    if (uuid == NULL)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, NULL, "unable to locate link to dataset")

    /* the link is found. Go ahead and open the dataset object. */
    json_t* datasets = json_object_get(dataset->domain->u.file.json_file_object, "datasets");
    dataset->object_json = json_object_get(datasets, uuid);

    /*** Set up a library object Dataspace for the opened Dataset ***/

    json_t* shape = json_object_get(dataset->object_json, "shape");
    hid_t dataspace = H5VL_json_jansson_to_dataspace(shape);
    HDassert(dataspace >= 0);
    dataset->u.dataset.space_id = dataspace;

    /**** datatype ****/

    json_t* type = json_object_get(dataset->object_json, "type");
    hid_t datatype = H5VL_json_jansson_to_datatype(type);
    dataset->u.dataset.dtype_id = datatype;

#ifdef PLUGIN_DEBUG
    printf("Dataset H5VL_json_object_t fields:\n");
    printf("  - Dataset Object type: %d\n", dataset->obj_type);
    printf("  - Dataset Object UUID: %s\n", dataset->object_uuid);
#endif

    ret_value = (void *) dataset;

done:
    /* Clean up allocated dataset object if there was an issue */
    if (dataset && !ret_value)
        if (H5VL_json_dataset_close(dataset, FAIL, NULL) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, NULL, "unable to close dataset")

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_dataset_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_dataset_read
 *
 * Purpose:     Reads an HDF5 dataset using the JSON API
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Frank Willmore
 *              January, 2018
 *
 */
static herr_t
H5VL_json_dataset_read(void *_dataset, hid_t mem_type_id, hid_t mem_space_id,
                       hid_t file_space_id, hid_t dxpl_id, void *buf, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *dataset = (H5VL_json_object_t *) _dataset;
    H5T_class_t         dtype_class;
    hbool_t             must_close_memspace = FALSE;//, must_close_filespace = FALSE;
    size_t              read_data_size, dtype_size;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(buf);
    HDassert(H5I_DATASET == dataset->obj_type && "not a dataset");

#ifdef PLUGIN_DEBUG
    printf("Receive Dataset read call with following parameters:\n");
    printf("  - Dataset UUID: %s\n", dataset->object_uuid);
    printf("  - mem_type_id: %ld\n", mem_space_id);
    printf("  - is all mem: %s\n", (mem_space_id == H5S_ALL) ? "yes" : "no");
    printf("  - file_space_id: %ld\n", file_space_id);
    printf("  - is all file: %s\n", (file_space_id == H5S_ALL) ? "yes" : "no");
#endif
    
    /* check for valid datatype */
    if (H5T_NO_CLASS == (dtype_class = H5Tget_class(mem_type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

    if (0 == (dtype_size = H5Tget_size(mem_type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

    if (H5S_ALL == mem_space_id) 
    {
        /* Set up a valid memory dataspace to use for the dataset read */
        mem_space_id = H5Scopy(dataset->u.dataset.space_id);
        H5Sselect_all(mem_space_id);
        must_close_memspace = true;
    } /* end if */
    else 
    {
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "only H5S_ALL supported.")
    }

    /*** reading the value ***/

    json_t* value = json_object_get(dataset->object_json, "value");
    H5VL_json_read_value(value, mem_type_id, mem_space_id, buf);

done:
    if (must_close_memspace)
        if (H5Sclose(mem_space_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close memspace")
#if 0
    if (must_close_filespace)
        if (H5Sclose(file_space_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close filespace")
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_dataset_read() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_dataset_write
 *
 * Purpose:     Writes an HDF5 dataset using the JSON API
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Frank Willmore
 *              December, 2017
 *
 */
static herr_t
H5VL_json_dataset_write(void *_dataset, hid_t mem_type_id, hid_t mem_space_id,
                        hid_t file_space_id, hid_t dxpl_id, const void *buf, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *dataset = (H5VL_json_object_t *) _dataset;
    H5T_class_t         dtype_class;
    hssize_t            mem_select_npoints, file_select_npoints;
    hbool_t             must_close_memspace = FALSE, must_close_filespace = FALSE;
    htri_t              is_variable_str;
    size_t              dtype_size;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for write access */
    if(!(dataset->domain->u.file.intent & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "no write intent on file")

    HDassert(buf);
    HDassert(H5I_DATASET == dataset->obj_type && "not a dataset");

#ifdef PLUGIN_DEBUG
    printf("Received Dataset write call with following parameters:\n");
    printf("  - Dataset UUID: %s\n", dataset->object_uuid);
    printf("  - mem_space_id: %ld\n", mem_space_id);
    printf("  - is all mem: %s\n", (mem_space_id == H5S_ALL) ? "true" : "false");
    printf("  - file_space_id: %ld\n", file_space_id);
    printf("  - is all file: %s\n", (file_space_id == H5S_ALL) ? "true" : "false");
    printf("  - DXPL ID: %ld\n", dxpl_id);
    printf("  - is default dxpl: %s\n", (dxpl_id == H5P_DEFAULT) ? "true" : "false");
#endif

    /* FTW: currently only supporting full writes */
    if ((mem_space_id != H5S_ALL) || (file_space_id != H5S_ALL))
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "invalid dataspace, only H5S_ALL currently supported.")
        
#if 0
    if (H5S_ALL == mem_space_id) {
        /* Set up a valid memory dataspace to use for the dataset read */
        mem_space_id = H5Scopy(dataset->u.dataset.space_id);
        H5Sselect_all(mem_space_id);
        must_close_memspace = true;
    } /* end if */

    if (H5S_ALL == file_space_id) {
        /* Set up a valid file dataspace to use for the dataset read */
        file_space_id = H5Scopy(dataset->u.dataset.space_id);
        H5Sselect_all(file_space_id);
        must_close_filespace = true;
    } /* end if */

    /* Verify that the number of selected points matches */
    if ((mem_select_npoints = H5Sget_select_npoints(mem_space_id)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "invalid dataspace")
    if ((file_select_npoints = H5Sget_select_npoints(file_space_id)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "invalid dataspace")
    HDassert((mem_select_npoints == file_select_npoints) && "memory selection num points != file selection num points");
#endif

    /* get datatype class */
    if (H5T_NO_CLASS == (dtype_class = H5Tget_class(mem_type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

    if (0 == (dtype_size = H5Tget_size(mem_type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

    json_t* value_array = json_object_get(dataset->object_json, "value");
    H5VL_json_write_value(value_array, dataset->u.dataset.dtype_id, dataset->u.dataset.space_id, buf);

done:

#if 0 
    if (must_close_memspace)
        if (H5Sclose(mem_space_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close memspace")
    if (must_close_filespace)
        if (H5Sclose(file_space_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close filespace")
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_dataset_write() */


static herr_t
H5VL_json_dataset_get(void *_dataset, H5VL_dataset_get_t get_type, hid_t H5_ATTR_UNUSED dxpl_id,
                      void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_json_object_t *dataset = (H5VL_json_object_t *) _dataset;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Dataset get call with following parameters:\n");
    printf("  - Get Type: %d\n", get_type);
    printf("  - Dataset object UUID: %s\n", dataset->object_uuid);
#endif

    HDassert(H5I_DATASET == dataset->obj_type && "not a dataset");

    switch (get_type) {
        /* H5Dget_access_plist */
        case H5VL_DATASET_GET_DAPL:
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "get DAPL unsupported")
        /* H5Dget_create_plist */
        case H5VL_DATASET_GET_DCPL:
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "get DCPL unsupported")

        /* H5Dget_offset */
        case H5VL_DATASET_GET_OFFSET:
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "get dataset offset unsupported")

        /* H5Dget_space */
        case H5VL_DATASET_GET_SPACE:
        {
            hid_t *ret_id = va_arg(arguments, hid_t *);

            if ((*ret_id = H5Scopy(dataset->u.dataset.space_id)) < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get dataspace of dataset")

            break;
        } /* H5VL_DATASET_GET_SPACE */

        /* H5Dget_space_status */
        case H5VL_DATASET_GET_SPACE_STATUS:
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "get dataset space status unsupported")

        /* H5Dget_storage_size */
        case H5VL_DATASET_GET_STORAGE_SIZE:
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "get dataset storage size unsupported")

        /* H5Dget_type */
        case H5VL_DATASET_GET_TYPE:
        {
            hid_t *ret_id = va_arg(arguments, hid_t *);

            if ((*ret_id = H5Tcopy(dataset->u.dataset.dtype_id)) < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype of dataset")

            break;
        } /* H5VL_DATASET_GET_TYPE */

        default:
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get this type of information from dataset")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_dataset_get() */


static herr_t
H5VL_json_dataset_specific(void *_dataset, H5VL_dataset_specific_t specific_type,
                          hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_json_object_t *dataset = (H5VL_json_object_t *) _dataset;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Dataset-specific call with following parameters:\n");
    printf("  - Specific type: %d\n", specific_type);
    printf("  - Dataset object UUID: %s\n", dataset->object_uuid);
#endif

    /* Check for write access */
    if(!(dataset->domain->u.file.intent & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "no write intent on file")

    HDassert(H5I_DATASET == dataset->obj_type && "not a dataset");

    switch (specific_type) {
        /* H5Dset_extent */
        case H5VL_DATASET_SET_EXTENT:
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "set dataset extent unsupported")
            break;

        default:
            HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "unknown operation")
    } /* end switch */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_dataset_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_dataset_close
 *
 * Purpose:     Closing dataset releases resources and renders its hid unusable.
 *
 * Return:
 *
 * Programmer:  Frank Willmore
 *              October, 2017
 *
 */
static herr_t
H5VL_json_dataset_close(void *_dataset, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *dataset = (H5VL_json_object_t *) _dataset;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Dataset close call with following parameters:\n");
    printf("  - UUID: %s\n", dataset->object_uuid);
    printf("  - DXPL: %ld\n\n", dxpl_id);
#endif

    HDassert(H5I_DATASET == dataset->obj_type && "not a dataset");

    if (dataset->u.dataset.dtype_id != FAIL && H5Tclose(dataset->u.dataset.dtype_id) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close datatype")
    if (dataset->u.dataset.space_id != FAIL && H5Sclose(dataset->u.dataset.space_id) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close dataspace")

    H5MM_xfree(dataset);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_dataset_close() */


/*** File API ***/



/*-------------------------------------------------------------------------
 * Function:    H5VL_json_file_create
 *
 * Purpose:     Creates an HDF5 file using the JSON API
 *
 * Return:
 *
 * Programmer:  Frank Willmore
 *              October, 2017
 *
 *              XXX: Currently the fcpl, fapl and dxpl are ignored, as the
 *              JSON API does not have special support for them
 */
static void*
H5VL_json_file_create(const char *name, unsigned flags, hid_t fcpl_id, 
                      hid_t fapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t* new_file = NULL;
    size_t              name_length;
    void*               ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received File create call with following parameters:\n");
    printf("  - Name: %s\n", name);
    printf("  - Flags: %u\n", flags);
    printf("  - FCPL: %ld\n", fcpl_id);
    printf("  - FAPL: %ld\n", fapl_id);
    printf("  - DXPL: %ld\n\n", dxpl_id);
#endif

    /* Allocate and setup internal VOL File struct */
    if (NULL == (new_file = (H5VL_json_object_t *) H5MM_malloc(sizeof(*new_file))))
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate file object")

#ifdef GENERATE_FILESYSTEM_OBJECT
    switch (flags & (H5F_ACC_EXCL | H5F_ACC_TRUNC))
    {
        case H5F_ACC_EXCL:
            ; /* this semi-colon is required by a subtlety in C grammar */
            struct stat buffer;   
            /* stat the file. If it exists, stat returns zero */
            if (stat(name, &buffer) == 0) 
                HGOTO_ERROR(H5E_FILE, H5E_FILEEXISTS, NULL, "file already exists")
            break;
        case H5F_ACC_TRUNC:
            /* store the reference to the filesystem FILE object */
            new_file->u.file.filesystem_file_object = fopen(name, "w");
            break;
        default:
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "invalid file create flags")
    } /* end switch */
#endif

    new_file->obj_type = H5I_FILE;
    new_file->u.file.intent = H5F_ACC_RDWR;

    /* Set up the JANSSON file object and store the reference in VOL File struct */

    json_t* new_file_object = json_object();
    HDassert(new_file_object);
    new_file->u.file.json_file_object = new_file_object;

    /* create an uuid for the file object itself */
    h5json_uuid_generate(new_file->object_uuid);
    assert(json_object_set_new(new_file_object, "id", json_string(new_file->object_uuid)) == 0);

    /* create the root group */

    /* setting root group uuid same as file uuid to make passing file as a location easier to handle */
    json_t *root_uuid_json_string = json_string(new_file->object_uuid);
    HDassert(json_object_set_new(new_file_object, "root", json_string(new_file->object_uuid)) == 0);

    /* FTW:The userblock is specified in fcpl, which is currently ignored. 
     * Adding empty userblock and specifying userblock size as zero. 
     */
    json_t* userblock = json_array(); /* it's a byte array */
    json_object_set_new(new_file_object, "userblock", userblock);

    /* add userblockSize */
    json_t* userblockSize = json_integer(0);
    json_object_set_new(new_file_object, "userblockSize", userblockSize);

    /* add creationPropoerties... since no creation properties are relevant to 
     * JSON VOL, just inserting a JSON null value.
     */
    json_object_set_new(new_file_object, "fcpl", json_null());

    /* get time for created, lastModified */
    time_t t = time(NULL);

    /* create groups hashtable and insert into the file object */
    json_t* groups_hashtable = json_object();
    json_object_set_new(new_file_object, "groups", groups_hashtable);

    /* build the root group object and insert into groups hashtable object */
    json_t* root_group = json_object();
    json_object_set_new(groups_hashtable, new_file->object_uuid, root_group);

    /* populate the root group with the rest of the components */
    json_t* hdf5_path_name_array = json_array();
    json_object_set_new(root_group, "alias", hdf5_path_name_array);

    json_object_set_new(root_group, "attributes", json_array());

    json_t* link_collection = json_array();
    json_object_set_new(root_group, "links", link_collection);

    /* created */
    char created_UTC_string[64];
    HDassert(h5json_get_utc_string_from_time(t, created_UTC_string) >= 0);
    json_t *created_UTC_json_string = json_string(created_UTC_string);
    HDassert(json_object_set_new(new_file_object, "created", created_UTC_json_string) == 0);
    json_object_set_new(root_group, "created", created_UTC_json_string); 

    /* lastModified */
    char lastModified_UTC_string[64];
    HDassert(h5json_get_utc_string_from_time(t, lastModified_UTC_string) >= 0);
    json_t *lastModified_UTC_json_string = json_string(lastModified_UTC_string);
    HDassert(json_object_set_new(new_file_object, "lastModified", lastModified_UTC_json_string) == 0);
    json_object_set_new(root_group, "lastModified", lastModified_UTC_json_string); 

    json_t* creationProperties = json_object();
    json_object_set_new(root_group, "creationProperties", creationProperties); 

    /* done with groups hashtable */
    
    /* create and insert empty datasets hashtable */
    json_t* datasets_hashtable = json_object();
    HDassert(json_object_set_new(new_file_object, "datasets", datasets_hashtable) == 0);

    /* create and insert empty attributes collection */
    json_object_set_new(new_file_object, "attributes", json_array());

    /* create and insert empty datatypes hashtable */
    json_t* datatypes_hashtable = json_object();
    HDassert(json_object_set_new(new_file_object, "datatypes", datatypes_hashtable) == 0);

    /* create and insert empty file_driver_info object. */
    json_t* file_driver_info = json_object();
    HDassert(json_object_set_new(new_file_object, "driverInfo", file_driver_info) == 0);

    /* create and insert api_version object. */
    json_t* api_version = json_string(HDF5_JSON_API_VERSION);
    HDassert(json_object_set_new(new_file_object, "apiVersion", api_version) == 0);
  
#ifdef PLUGIN_DEBUG
    printf("File H5VL_json_object_t fields:\n");
    printf("  - File Object type: %d\n\n", new_file->obj_type);
#endif

    /* file contains reference to itself as its own container */
    new_file->domain = new_file;

    /* set up the JANSSON file object and store the reference */
    new_file->u.file.json_file_object = new_file_object;
    /* need to set the object_json for VOL object as well */
    new_file->object_json = new_file_object;

    ret_value = (void*) new_file;

done:

    /* Clean up allocated file object if there was an issue */
    if (new_file && !ret_value)
        if (H5VL_json_file_close(new_file, FAIL, NULL) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEOBJ, NULL, "unable to close file")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_file_create() */


static void*
H5VL_json_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *file = NULL;
    size_t              name_length;
    void               *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("\n");
    printf("Received File open call with following parameters:\n");
    printf("  - Name: %s\n", name);
    printf("  - Flags: %u\n", flags);
    printf("  - FAPL: %ld\n", fapl_id);
    printf("  - DXPL: %ld\n\n", dxpl_id);
#endif

    /* Allocate and setup internal File struct */
    if (NULL == (file = (H5VL_json_object_t *) H5MM_malloc(sizeof(*file))))
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate file object")

    file->obj_type = H5I_FILE;
    file->u.file.intent = flags;

#ifndef GENERATE_FILESYSTEM_OBJECT
    HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "Your JSON VOL hasn't been complied to allow interaction with filesystem. ")
#endif

    /* Open FILE object on the filesystem with reqested intent */
    switch (flags & (H5F_ACC_RDWR | H5F_ACC_RDONLY))
    {
        case H5F_ACC_RDWR:
            file->u.file.filesystem_file_object = fopen(name, "r+");
        break; 

        case  H5F_ACC_RDONLY:
            file->u.file.filesystem_file_object = fopen(name, "r");
        break;

        default:
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "Invalid file open mode")
    }

    /* Read the contents of the file into buffer, then create JANSSON object */
    struct stat file_status;
    fstat(fileno(file->u.file.filesystem_file_object), &file_status);
    printf("got file size %ld\n", file_status.st_size);
    char *json_buffer = (char*)(H5MM_malloc(file_status.st_size + 1));
    fread(json_buffer, sizeof(char), file_status.st_size, file->u.file.filesystem_file_object);
    json_buffer[file_status.st_size]='\0';

#ifdef PLUGIN_DEBUG
    printf("File open json_buffer: \n%s\n", json_buffer);
#endif

    /* digest the buffer to form JANSSON object */
    json_error_t error;
    json_t* document = json_loads(json_buffer, 0, &error);
    HDassert(document); /* make sure it worked */
    H5MM_xfree(json_buffer);

    /* validate the JSON file which was read ... empty file should contain at least:
     *
     *         "id" ":" identifier ","
     *         "root" ":" id_reference ","
     *         "groups" ":" "{" group_hashtable "}" ","
     *         "apiVersion" ":" "1.0.0"
     */

    h5json_uuid_t* id = (h5json_uuid_t*)json_string_value(json_object_get(document, "id"));
    assert(id);
    json_t* root = json_object_get(document, "root");
    assert(root);
    json_t* groups = json_object_get(document, "groups");
    assert(groups);
    json_t* apiVersion = json_object_get(document, "apiVersion");
    assert(apiVersion);

#ifdef PLUGIN_DEBUG
    printf("Validated JSON document: \n");
    printf("  - id: %s\n", id);
    printf("  - root: %s\n", json_string_value(root));
    printf("  - groups: %s\n", json_dumps(groups, JSON_INDENT(4)));
    printf("  - apiVersion: %s\n", json_string_value(apiVersion));
#endif

    /* set up the JANSSON file object and store the reference */
    file->u.file.json_file_object = document;
    /* need to set the object_json for VOL object as well */
    file->object_json = document;

    /* This uuid gets *copied* to the library object */
    strncpy(file->object_uuid, id, sizeof(h5json_uuid_t));

#ifdef PLUGIN_DEBUG
    printf("File H5VL_json_object_t fields:\n");
    printf("  - File Object type: %d\n\n", file->obj_type);
#endif

    /* file contains reference to itself as its own container */
    file->domain = file;

    ret_value = (void *) file;

done:

    /* Clean up allocated file object if there was an issue */
    if (file && !ret_value)
        if (H5VL_json_file_close(file, FAIL, NULL) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEOBJ, NULL, "unable to close file")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_file_open() */


static herr_t
H5VL_json_file_get(void *obj, H5VL_file_get_t get_type, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_json_object_t *file = (H5VL_json_object_t *) obj;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received File get call with following parameters:\n");
    printf("  - Get Type: %d\n", get_type);
    printf("  - File object UUID: %s\n", file->object_uuid);
#endif

    HDassert(H5I_FILE == file->obj_type && "not a file");

    switch (get_type) {
        /* H5Fget_access_plist */
        case H5VL_FILE_GET_FAPL:
            HGOTO_ERROR(H5E_FILE, H5E_UNSUPPORTED, FAIL, "get FAPL unsupported")
        /* H5Fget_create_plist */
        case H5VL_FILE_GET_FCPL:
            HGOTO_ERROR(H5E_FILE, H5E_UNSUPPORTED, FAIL, "get FCPL unsupported")

        /* H5Fget_intent */
        case H5VL_FILE_GET_INTENT:
        {
            unsigned *ret_intent = va_arg(arguments, unsigned *);

            *ret_intent = file->u.file.intent;

            break;
        } /* H5VL_FILE_GET_INTENT */

        /* H5Fget_name */
        case H5VL_FILE_GET_NAME:
        {
            /* XXX: */
            break;
        } /* H5VL_FILE_GET_NAME */

        /* H5Fget_obj_count */
        case H5VL_FILE_GET_OBJ_COUNT:
            HGOTO_ERROR(H5E_FILE, H5E_UNSUPPORTED, FAIL, "get file object count unsupported")

        /* H5Fget_obj_ids */
        case H5VL_FILE_GET_OBJ_IDS:
            HGOTO_ERROR(H5E_FILE, H5E_UNSUPPORTED, FAIL, "get file object IDs unsupported")

        case H5VL_OBJECT_GET_FILE:
            HGOTO_ERROR(H5E_FILE, H5E_UNSUPPORTED, FAIL, "get file unsupported")

        default:
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get this type of information from file")
    } /* end switch */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_file_get() */


static herr_t
H5VL_json_file_specific(void *obj, H5VL_file_specific_t specific_type, hid_t H5_ATTR_UNUSED dxpl_id,
                        void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_json_object_t *file = (H5VL_json_object_t *) obj;
    char                specific_request_header[HOST_HEADER_MAX_LENGTH] = "Host: ";
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received File-specific call with following parameters:\n");
    printf("  - Specific Type: %d\n", specific_type);
    printf("  - File object UUID: %s\n", file->object_uuid);
#endif

    HDassert(H5I_FILE == file->obj_type && "not a file");

    switch (specific_type) {
        case H5VL_FILE_FLUSH:
        case H5VL_FILE_IS_ACCESSIBLE:
        case H5VL_FILE_MOUNT:
        case H5VL_FILE_UNMOUNT:
            HGOTO_ERROR(H5E_FILE, H5E_UNSUPPORTED, FAIL, "unsupported operation")
            break;
        default:
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "unknown operation")
    } /* end switch */

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_file_specific() */


static herr_t
H5VL_json_file_optional(void *obj, hid_t dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_file_optional() */


static herr_t
H5VL_json_file_close(void *file, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *_file = (H5VL_json_object_t *) file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

#ifdef PLUGIN_DEBUG
    printf("Received File close call with following parameters:\n");
    printf("  - DXPL: %ld\n", dxpl_id);
    printf("  - UUID: %s\n", _file->object_uuid);
    printf("  - JSON content begin: \n\n%s\n\n", json_dumps(_file->u.file.json_file_object, JSON_INDENT(4)));
    printf("  - JSON content end. \n\n");
#endif

    HDassert(H5I_FILE == _file->obj_type && "not a file");

#ifdef GENERATE_FILESYSTEM_OBJECT
    /* start at the beginning of the file */
    HDassert(fseek(_file->u.file.filesystem_file_object, 0, SEEK_SET) == 0);
    /* now dump/encode: */
    fprintf(_file->u.file.filesystem_file_object, "%s\n", json_dumps(_file->u.file.json_file_object, JSON_INDENT(4)));

    /* close file. */ 
    fclose(_file->u.file.filesystem_file_object); 
#endif

    /* free the Jansson ojbect. */ 
    HDassert(json_object_clear(_file->u.file.json_file_object) == 0); 

    H5MM_xfree(_file);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_json_file_close() */


/*** Groups API ***/



/*-------------------------------------------------------------------------
 * Function:    H5VL_json_group_create
 *
 * Purpose:     Creates an HDF5 Group using the JSON API
 *
 * Return:      pointer to new VOL group object
 *
 * Programmer:  Frank Willmore
 *              November, 2017
 *
 */

static void *
H5VL_json_group_create(void *_parent, H5VL_loc_params_t H5_ATTR_UNUSED loc_params, 
                       const char *name, hid_t gcpl_id, hid_t gapl_id,
                       hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t* parent = (H5VL_json_object_t *) _parent;
    H5VL_json_object_t* new_group = NULL;
    void*               ret_value = NULL;
    h5json_uuid_t       current_uuid = "This uuid has yet not been created.";
    json_t*             current_group = NULL;
    json_t*             groups_in_file = NULL;    
    h5json_uuid_t       new_group_uuid;
    json_t*             new_group_json;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Group create call with following parameters:\n");
    printf("  - Name: %s\n", name);
    printf("  - GCPL: %ld\n", gcpl_id);
    printf("  - GAPL: %ld\n", gapl_id);
    printf("  - DXPL: %ld\n", dxpl_id);
    printf("  - Parent Object Type: %d\n\n", parent->obj_type);
#endif

    /* Check for write access */
    if(!(parent->domain->u.file.intent & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "no write intent on file")

    /* Allocate mem for the library group object */
    if (NULL == (new_group = (H5VL_json_object_t *) H5MM_malloc(sizeof(*new_group))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, NULL, "can't allocate group object")

    /* set the domain for the new_group to be same as parent_domain */
    new_group->domain = parent->domain;
    new_group->obj_type = H5I_GROUP;
    new_group->object_uuid[0] = NULL; 

    /* extract the starting point / current_uuid */
    switch (parent->obj_type) 
    {
        case H5I_FILE:
            /* if starting with a file, use the domain uuid. */
            strncpy(current_uuid, new_group->domain->object_uuid, sizeof(h5json_uuid_t));
            break;
        case H5I_GROUP:
            /* For a group, copy the parent uuid */
            strncpy(current_uuid, parent->object_uuid, sizeof(h5json_uuid_t));
            break;
        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "Not a valid parent object type.")
    }
    HDassert(current_uuid && "parent uuid not found. ");

    /* create the JANSSON representation */
    hbool_t create_intermediate = TRUE;
    H5VL_json_create_new_group(new_group->domain, name, current_uuid, create_intermediate);

    /* copy the final current_uuid and pointer to current_group into the VOL object */
    strncpy(new_group->object_uuid, current_uuid, sizeof(h5json_uuid_t));

    /* set the pointer into the JANSSON object from the VOL object */
    new_group->object_json = json_object_get(groups_in_file, new_group->object_uuid);

#ifdef PLUGIN_DEBUG
    printf("  - Finished adding group %s... \n", name);
    printf("  - With uuid %s... \n", (new_group->object_uuid));
    printf("  - File now reads:\n\n");
    json_dumpf(parent->domain->u.file.json_file_object, stdout, JSON_INDENT(4));
#endif

    /* Everything is happy thus far, so go ahead and set ret_value */
    ret_value = (void *) new_group;

done:

    /* Clean up allocated group object if there was an issue */
    if (new_group && !ret_value) //FTW : new_group != ret_value would be simpler?
    {
        if (H5VL_json_group_close(new_group, FAIL, NULL) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTCLOSEOBJ, NULL, "unable to close group")
    }

    FUNC_LEAVE_NOAPI(ret_value);

} /* end H5VL_json_group_create() */


static void *
H5VL_json_group_open(void *_parent, H5VL_loc_params_t loc_params, const char *name,
                     hid_t gapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t* parent = (H5VL_json_object_t *) _parent;
    H5VL_json_object_t* group = NULL;
    htri_t              search_ret;
    void*               ret_value = NULL;
    json_t*             groups_in_file;
    h5json_uuid_t*      current_uuid;
    json_t*             current_group;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Group open call with following parameters:\n");
    printf("  - Name: %s\n", name);
    printf("  - GAPL: %ld\n", gapl_id);
    printf("  - DXPL: %ld\n", dxpl_id);
    printf("  - Parent: %ld\n", parent);
    printf("  - Parent Object Type: %d\n", parent->obj_type);
    printf("  - Parent UUID: %s\n", parent->object_uuid);
#endif

    HDassert((H5I_FILE == parent->obj_type || H5I_GROUP == parent->obj_type)
            && "parent object not a file or group");

    /* Allocate and setup internal Group struct */
    if (NULL == (group = (H5VL_json_object_t *) H5MM_malloc(sizeof(*group))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, NULL, "can't allocate group object")

    group->obj_type = H5I_GROUP;
    group->domain = parent->domain; /* Store pointer to file that the opened Group is within */ 

    /* handle the JANSSON representation: */
    groups_in_file = json_object_get(parent->domain->u.file.json_file_object, "groups"); 

    /* get the list of tokens in the provided name/path */
    const char s[2] = "/";
    unsigned token_index = 0;
    char copy_of_name[1024];
    char *tokens[256];
    strcpy(copy_of_name, name);
    for (tokens[token_index] = strtok(copy_of_name, s); 
         tokens[token_index++] != NULL; 
         tokens[token_index] = strtok(NULL, s));
    unsigned n_tokens = token_index - 1;

    /* finally, find starting point and traverse the links to get the object_uuid */
    if ((parent->obj_type == H5I_FILE) || (n_tokens == 0)) 
    {
        /* borrow reference to root group as starting point */
        current_uuid = (h5json_uuid_t*)json_string_value(json_object_get(parent->domain->u.file.json_file_object, "root"));
    }
    else /* borrow reference to provided group */
    {
        current_uuid = &(group->object_uuid); 
    }

    current_group = json_object_get(groups_in_file, current_uuid);
    HDassert(current_uuid && "Couldn't get uuid for provided location.");
    HDassert(current_group && "Couldn't get group for provided location.");

    /*  group-spotting: search the path/tokens provided, create as needed and allowed. 
        The last token is our target, and library object for it created and returned. */
   
    /* if there are zero tokens, i.e. opening root group, then this loop never runs */
    for(token_index=0; token_index<n_tokens; token_index++)
    {
        size_t index;
        json_t* link;
        hbool_t found = FALSE;
        json_t* current_links;

        /* search the links for the current token */
        current_links = json_object_get(current_group, "links");

        json_array_foreach(current_links, index, link) 
        {
            /* block of code that uses index and link */
            json_t* title = json_object_get(link, "title");
            if (json_equal(title, json_string(tokens[token_index])))
            {
                /* found it. */
                found = TRUE;
                /* set the current group pointer to point to the new one before moving on to the next token */
                current_uuid = (h5json_uuid_t*)json_string_value(json_object_get(link, "id"));
                current_group = json_object_get(groups_in_file, current_uuid);

                break; /* breaks out of enclosing json_array_foreach() macro */
            }
        } /* end of loop over link array */

        if (!found) /* can't find link in path, so fail */
        {
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, NULL, "Cannot open link which does not exist.")
        }
        
     } /* for token index */

#ifdef PLUGIN_DEBUG
    printf("Group H5VL_json_object_t fields:\n");
    printf("  - Group Object type: %d\n", group->obj_type);
    printf("  - Group uuid: %s\n\n", (current_uuid));
#endif

    /* The last token is the group we're actually opening... so copy the ID. */
    strncpy(group->object_uuid, current_uuid, sizeof(h5json_uuid_t));

    ret_value = (void *) group;

done:
    /* Clean up allocated group object if there was an issue */
    if (group && !ret_value)
        if (H5VL_json_group_close(group, FAIL, NULL) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTCLOSEOBJ, NULL, "unable to close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_group_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_group_get
 *
 * Purpose: Gets certain data about a group
 *
 * Return:  Non-negative on success/Negative on failure
 *
 * Programmer: ??? 
 *             Month, Year
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_json_group_get(void *_group, H5VL_group_get_t get_type, hid_t dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_json_object_t *group = (H5VL_json_object_t *) _group;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Group get call with following parameters:\n");
    printf("  - Get Type: %d\n", get_type);
    printf("  - Group object UUID: %s\n", group->object_uuid);
#endif

    HDassert(H5I_GROUP == group->obj_type && "not a group");

    switch (get_type) {
        /* XXX: Unable to support until property lists are ironed out with HSDS */
        /* H5Gget_create_plist */
        case H5VL_GROUP_GET_GCPL:
            HGOTO_ERROR(H5E_SYM, H5E_UNSUPPORTED, FAIL, "get GCPL unsupported")

        /* H5Gget_info */
        case H5VL_GROUP_GET_INFO:
            HGOTO_ERROR(H5E_SYM, H5E_UNSUPPORTED, FAIL, "get group info unsupported")

        default:
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get this type of information from group");
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_group_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_group_close
 *
 * Purpose: Release resources allocated for library group object
 *
 * Return:  Non-negative on success/Negative on failure
 *
 * Programmer:  Frank Willmore
 *              November, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_json_group_close(void *_group, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *group = (H5VL_json_object_t *) _group;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

#ifdef PLUGIN_DEBUG
    printf("Received Group close call with following parameters:\n");
    printf("  - DXPL: %ld\n", dxpl_id);
    printf("  - UUID: %s\n", group->object_uuid);
#endif

    /* checking group is group */
    HDassert(H5I_GROUP == group->obj_type && "not a group");

    /* free the library object */
    H5MM_xfree(group);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_json_group_close() */


/*** Links API ***/



/*-------------------------------------------------------------------------
 * Function:    H5VL_json_link_create
 *
 * Purpose: Create hard and soft links
 *
 * Return:  Non-negative on success/Negative on failure
 *
 * Programmer:  Frank Willmore
 *              January, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_json_link_create(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params,
                      hid_t lcpl_id, hid_t lapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t* new_link_location = (H5VL_json_object_t *) obj;
    H5P_genplist_t*     lcpl = NULL;
    size_t              link_path_length;
    char*               link_path_copy = NULL;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Link create call with following parameters:\n");
    printf("  - New Link create_type: %d (hard = %d, soft = %d)\n", create_type, H5VL_LINK_CREATE_HARD, H5VL_LINK_CREATE_SOFT);
    printf("  - New link to be placed in location (group or file) with UUID: %s\n", new_link_location->object_uuid);
    printf("  - New link location has loc_params.obj_type: %d (H5I_type_t group = %d/file = %d)\n", loc_params.obj_type, H5I_GROUP, H5I_FILE);
    printf("  - loc_params.type: %d (H5VL_loc_type_t)\n", loc_params.type);
    printf("  - H5VL_loc_type_t values are: H5VL_OBJECT_BY_SELF = %d, H5VL_OBJECT_BY_NAME = %d, H5VL_OBJECT_BY_IDX = %d, H5VL_OBJECT_BY_ADDR = %d\n", 
           H5VL_OBJECT_BY_SELF, H5VL_OBJECT_BY_NAME, H5VL_OBJECT_BY_IDX, H5VL_OBJECT_BY_ADDR );
    printf("  - New Link Name/loc_params.loc_data.loc_by_name.name: %s\n", loc_params.loc_data.loc_by_name.name);
#endif

    /* Check for write access */
    if(!(new_link_location->domain->u.file.intent & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "no write intent on file")

    HDassert((H5I_FILE == new_link_location->obj_type || H5I_GROUP == new_link_location->obj_type)
            && "link location object not a file or group");
    HDassert((loc_params.loc_data.loc_by_name.name)
            && "link name not specified ");

    if (NULL == (lcpl = (H5P_genplist_t *) H5I_object(lcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    switch (create_type) {
        case H5VL_LINK_CREATE_HARD:
        {
            H5VL_json_object_t  link_target;
            H5VL_loc_params_t   target_loc_params;
            htri_t              search_ret;
            void               *link_loc_obj;

            /* Retrieve the target object and loc params from the LCPL */
            if (H5P_get(lcpl, H5VL_PROP_LINK_TARGET, &link_loc_obj) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for link target object")
            if (H5P_get(lcpl, H5VL_PROP_LINK_TARGET_LOC_PARAMS, &target_loc_params) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for link target object loc params")

#ifdef PLUGIN_DEBUG
            printf("The following two pieces of information identify the link source:\n");
            printf("  - UUID for location containing link source: %s\n", ((H5VL_json_object_t *) link_loc_obj)->object_uuid);
            printf("  - Name of source for Link: %s\n", target_loc_params.loc_data.loc_by_name.name);
#endif

            /* use the provided group or file UUID and name of object to locate the object */
            json_t* collection; /* determine collection from the link to the original */
            h5json_uuid_t copy_of_uuid; /* copy the uuid because it gets modified */
            strncpy(copy_of_uuid, ((H5VL_json_object_t *) link_loc_obj)->object_uuid, sizeof(h5json_uuid_t));
            json_t* the_object = H5VL_json_find_object_by_name(new_link_location->domain, target_loc_params.loc_data.loc_by_name.name, &copy_of_uuid, &collection, NULL);
            h5json_uuid_t the_object_uuid; 
            strncpy(the_object_uuid, json_string_value(json_object_get(the_object, "id")), sizeof(h5json_uuid_t));

            /* create a new link object */
            json_t* new_link = json_object();
            json_object_set_new(new_link, "class", json_string("H5L_TYPE_HARD"));
            json_object_set_new(new_link, "title", json_string(loc_params.loc_data.loc_by_name.name));
            json_object_set_new(new_link, "collection", collection);
            json_object_set_new(new_link, "id", json_string(the_object_uuid));

            /* insert the link */
            herr_t err = H5VL_json_insert_link_into_group(new_link_location->domain, new_link_location->object_uuid, new_link);

            break;
        } /* H5VL_LINK_CREATE_HARD */

        case H5VL_LINK_CREATE_SOFT:
        {
            const char *link_target;

            if (H5P_get(lcpl, H5VL_PROP_LINK_TARGET_NAME, &link_target) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for link target")

#ifdef PLUGIN_DEBUG
            printf("  - Soft link target: %s\n", link_target);
#endif

            /* locate the group (using the helper function) where the new link is to be placed */
            json_t* groups_in_file = json_object_get(new_link_location->domain->object_json, "groups");
            json_t* destination_group = json_object_get(groups_in_file, new_link_location->object_uuid);
            json_t* destination_group_links = json_object_get(destination_group, "links");

            /* insert a json_t* link object into that group's link collection. */
            json_t* new_link = json_object();
            json_object_set_new(new_link, "class", json_string("H5L_TYPE_SOFT"));
            json_object_set_new(new_link, "title", json_string(loc_params.loc_data.loc_by_name.name));
            json_object_set_new(new_link, "h5path", json_string(link_target));
            json_array_append_new(destination_group_links, new_link);

            break;

        } /* H5VL_LINK_CREATE_SOFT */

        case H5VL_LINK_CREATE_UD:
        {
            HGOTO_ERROR(H5E_LINK, H5E_UNSUPPORTED, FAIL, "User-Defined/External links are unsupported.")
            break;

        } /* H5VL_LINK_CREATE_UD */

        default:
            HGOTO_ERROR(H5E_LINK, H5E_BADVALUE, FAIL, "Invalid link create type")
    } /* end switch */

done:

    if (link_path_copy)
        H5MM_xfree(link_path_copy);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_link_create() */


static herr_t
H5VL_json_link_copy(void *_src_obj, H5VL_loc_params_t loc_params1,
                    void *_dst_obj, H5VL_loc_params_t loc_params2,
                    hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    herr_t ret_value = SUCCEED;
    H5VL_json_object_t*   src_obj = (H5VL_json_object_t*)_src_obj;
    H5VL_json_object_t*   dst_obj = (H5VL_json_object_t*)_dst_obj;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
            printf("Received link_copy request with the following:\n");
            printf("  - UUID for location containing link source: %s\n", src_obj->object_uuid);
            printf("  - UUID for location containing link dest: %s\n", dst_obj->object_uuid);
            printf("  - Name from loc_params1 (src): %s\n", loc_params1.loc_data.loc_by_name.name);
            printf("  - Name from loc_params2 (dst): %s\n", loc_params2.loc_data.loc_by_name.name);
#endif

    /* find the link */
    h5json_uuid_t current_uuid;
    h5json_uuid_t containing_group_uuid;
    strncpy(current_uuid, src_obj->object_uuid, sizeof(current_uuid));
    json_t* link = H5VL_json_find_object_by_name(src_obj->domain, loc_params1.loc_data.loc_by_name.name, &current_uuid, NULL, &containing_group_uuid);

    /* need a deep copy because new link will have different name */
    json_t* copy_of_link = json_deep_copy(link); 

    /* if it exists, move it, otherwise fail */
    if (link != NULL)
    {
        /* change name of link */
        json_object_del(copy_of_link, "title");
        json_object_set_new(copy_of_link, "title", json_string(loc_params2.loc_data.loc_by_name.name));

        /* add link to new group */
        H5VL_json_insert_link_into_group(dst_obj->domain, dst_obj->object_uuid, copy_of_link);
    }
    else
        HGOTO_ERROR(H5E_LINK, H5E_BADVALUE, FAIL, "Can not move link which does not exist.");

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_link_copy() */


static herr_t
H5VL_json_link_move(void *_src_obj, H5VL_loc_params_t loc_params1,
                    void *_dst_obj, H5VL_loc_params_t loc_params2,
                    hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    herr_t ret_value = SUCCEED;
    H5VL_json_object_t*   src_obj = (H5VL_json_object_t*)_src_obj;
    H5VL_json_object_t*   dst_obj = (H5VL_json_object_t*)_dst_obj;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
            printf("Received link_move request with the following:\n");
            printf("  - UUID for location containing link source: %s\n", src_obj->object_uuid);
            printf("  - UUID for location containing link dest: %s\n", dst_obj->object_uuid);
            printf("  - Name from loc_params1 (src): %s\n", loc_params1.loc_data.loc_by_name.name);
            printf("  - Name from loc_params2 (dst): %s\n", loc_params2.loc_data.loc_by_name.name);
#endif

    /* find the link */
    h5json_uuid_t current_uuid;
    h5json_uuid_t containing_group_uuid;
    strncpy(current_uuid, src_obj->object_uuid, sizeof(current_uuid));
    json_t* link = H5VL_json_find_object_by_name(src_obj->domain, loc_params1.loc_data.loc_by_name.name, &current_uuid, NULL, &containing_group_uuid);

    /* if it exists, move it, otherwise fail */
    if (link != NULL)
    {
        /* change name of link */
        json_object_del(link, "title");
        json_object_set_new(link, "title", json_string(loc_params2.loc_data.loc_by_name.name));

        /* move link to new group */
        H5VL_json_insert_link_into_group(dst_obj->domain, dst_obj->object_uuid, link);
        H5VL_json_delete_link_from_containing_group(src_obj->domain, containing_group_uuid, link);
    }
    else
        HGOTO_ERROR(H5E_LINK, H5E_BADVALUE, FAIL, "Can not move link which does not exist.");

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_link_move() */


static herr_t
H5VL_json_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type,
                   hid_t dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_json_object_t *link = (H5VL_json_object_t *) obj;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Link get call with following parameters:\n");
    printf("  - Get type: %d\n", get_type);
#endif

    switch (get_type) {
        /* H5Lget_info */
        case H5VL_LINK_GET_INFO:
            HGOTO_ERROR(H5E_LINK, H5E_UNSUPPORTED, FAIL, "get link info unsupported")

        /* H5Lget_name */
        case H5VL_LINK_GET_NAME:
        {
            /* XXX: */
            break;
        } /* H5VL_LINK_GET_NAME */

        /* H5Lget_val */
        case H5VL_LINK_GET_VAL:
        {
            /* XXX: */
            break;
        } /* H5VL_LINK_GET_VAL */

        default:
            HGOTO_ERROR(H5E_LINK, H5E_CANTGET, FAIL, "can't get this type of information from link")
    } /* end switch */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_link_get() */


static herr_t
H5VL_json_link_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_link_specific_t specific_type,
                        hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_json_object_t *loc_obj = (H5VL_json_object_t *) obj;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("FTW Received Link-specific call with following parameters:\n");
    printf("  - Specific type: %d\n", specific_type);
    printf("  - Link UUID: %s\n", loc_obj->object_uuid);
    printf("  - loc_obj: %ld\n", loc_obj);
    printf("File currently reads: \n%s\n", json_dumps(loc_obj->domain->object_json, JSON_INDENT(4)));
#endif

    HDassert((H5I_FILE == loc_obj->obj_type || H5I_GROUP == loc_obj->obj_type)
                && "parent object not a file or group");

    switch (specific_type) 
    {
        /* H5Ldelete */
        case H5VL_LINK_DELETE:
        {

#ifdef PLUGIN_DEBUG
    printf("FTW Link-specific call is delete, with following parameters:\n");
    printf("  - Link container UUID: %s\n", loc_obj->object_uuid);
    printf("  - Link name:           %s\n", loc_params.loc_data.loc_by_name.name);
#endif
            /* find the link */
            h5json_uuid_t current_uuid;
            h5json_uuid_t containing_group_uuid;
            strncpy(current_uuid, loc_obj->object_uuid, sizeof(current_uuid));
            json_t* link = H5VL_json_find_object_by_name(loc_obj->domain, loc_params.loc_data.loc_by_name.name, &current_uuid, NULL, &containing_group_uuid);

            /* if it exists, delete it, otherwise fail */
            if (link != NULL)
            {
                H5VL_json_delete_link_from_containing_group(loc_obj->domain, containing_group_uuid, link);
            }
            else
            {
                HGOTO_ERROR(H5E_LINK, H5E_BADVALUE, FAIL, "Can not delete link which does not exist.");
            }
             
            break;
        }

        /* H5Lexists */
        case H5VL_LINK_EXISTS:
        {
            htri_t *ret = va_arg (arguments, htri_t *);
            json_t* collection; /*unused*/
            h5json_uuid_t current_uuid;
            strncpy(current_uuid, loc_obj->object_uuid, sizeof(current_uuid));
            json_t* link = H5VL_json_find_object_by_name(loc_obj->domain, loc_params.loc_data.loc_by_name.name, &current_uuid, &collection, NULL);
            *ret = (link != NULL);
            break;
        } /* H5VL_LINK_EXISTS */

        /* H5Literate/visit (_by_name) */
        case H5VL_LINK_ITER:
            HGOTO_ERROR(H5E_LINK, H5E_UNSUPPORTED, FAIL, "unsupported operation")
            break;

        default:
            HGOTO_ERROR(H5E_LINK, H5E_BADVALUE, FAIL, "unknown operation");

    } /* end switch */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5VL_json_link_specific() */


/*** Objects API ***/



static void *
H5VL_json_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type,
                      hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *parent = (H5VL_json_object_t *) obj;
    H5I_type_t          obj_type = H5I_UNINIT;
    htri_t              search_ret;
    void               *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Object open call with following parameters:\n");
    printf("  - Path: %s\n", loc_params.loc_data.loc_by_name.name);
    printf("  - DXPL: %ld\n", dxpl_id);
    printf("  - Parent Object Type: %d\n", parent->obj_type);
#endif

    HDassert((H5I_FILE == parent->obj_type || H5I_GROUP == parent->obj_type)
                && "parent object not a file or group");

    /* XXX: Currently only opening objects by name is supported */
    HDassert(H5VL_OBJECT_BY_NAME == loc_params.type && "loc_params type not H5VL_OBJECT_BY_NAME");

    //    search_ret = H5VL_json_find_link_by_path(parent, loc_params.loc_data.loc_by_name.name,
    //            get_link_type_callback, NULL, &obj_type);
    if (!search_ret || search_ret < 0)
        HGOTO_ERROR(H5E_LINK, H5E_CANTOPENOBJ, NULL, "unable to find object by name")

    switch (obj_type) {
        case H5I_DATATYPE:
            if (NULL == (ret_value = H5VL_json_datatype_open(parent, loc_params, loc_params.loc_data.loc_by_name.name,
                    ((H5VL_OBJECT_BY_NAME == loc_params.type) && (loc_params.loc_data.loc_by_name.lapl_id != H5P_DEFAULT))
                    ? loc_params.loc_data.loc_by_name.lapl_id : H5P_DATATYPE_ACCESS_DEFAULT, dxpl_id, req)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTOPENOBJ, NULL, "can't open datatype")
            break;

        case H5I_DATASET:
            if (NULL == (ret_value = H5VL_json_dataset_open(parent, loc_params, loc_params.loc_data.loc_by_name.name,
                    ((H5VL_OBJECT_BY_NAME == loc_params.type) && (loc_params.loc_data.loc_by_name.lapl_id != H5P_DEFAULT))
                    ? loc_params.loc_data.loc_by_name.lapl_id : H5P_DATASET_ACCESS_DEFAULT, dxpl_id, req)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, NULL, "can't open dataset")
            break;

        case H5I_GROUP:
            if (NULL == (ret_value = H5VL_json_group_open(parent, loc_params, loc_params.loc_data.loc_by_name.name,
                    ((H5VL_OBJECT_BY_NAME == loc_params.type) && (loc_params.loc_data.loc_by_name.lapl_id != H5P_DEFAULT))
                    ? loc_params.loc_data.loc_by_name.lapl_id : H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req)))
                HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "can't open group")
            break;

        case H5I_ATTR:
        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_FILE:
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
            HGOTO_ERROR(H5E_ARGS, H5E_CANTOPENOBJ, NULL, "invalid object type")
    } /* end switch */

    if (opened_type) *opened_type = obj_type;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_object_open() */


static herr_t
H5VL_json_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name,
                      void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name,
                      hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_object_copy() */


static herr_t
H5VL_json_object_get(void *_obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type,
                     hid_t dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_json_object_t* obj = (H5VL_json_object_t*) _obj;
    herr_t ret_value = FAIL;
    //herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) 
    {
        case H5VL_REF_GET_CONTENT:
        {
            /* json_dumps allocates mem and returns pointer to char* */
            char* buffer = json_dumps(obj->object_json, JSON_INDENT(4)); 
            req = (void**)&buffer;
            break;
        }
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTOPENOBJ, NULL, "invalid object type")
    }

    ret_value = SUCCEED;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_object_get() */


static herr_t
H5VL_json_object_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_object_specific_t specific_type,
                          hid_t dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_object_specific() */


static herr_t
H5VL_json_object_optional(void *obj, hid_t dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_object_optional() */


/************************************
 *         Helper functions         *
 ************************************/


/*-------------------------------------------------------------------------
 * Function:    h5json_get_utc_string
 *
 * Purpose:     make system call and generate string rep of current time
 *
 * Return:      Non-negative on success/Negative on failure
 *              uuid is IN/OUT
 *
 * Programmer:  Frank Willmore
 *              November, 2017
 *
 * Example:     h5json_get_utc_string_from_time(time(NULL), char *time_buf)
 *              will write the *current* time to time_buf in UTC
 *
 */
static herr_t h5json_get_utc_string_from_time(time_t t, char *time_buf)
{
    herr_t ret_value = SUCCEED;

    char *days[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
    char *months[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

    struct tm gmt = *gmtime(&t);
    sprintf(time_buf, "%s %s %d %d:%d:%d UTC %d", days[gmt.tm_wday], months[gmt.tm_mon], gmt.tm_mday, gmt.tm_hour, gmt.tm_min, gmt.tm_sec, gmt.tm_year + 1900);

    return SUCCEED;
}
    

/*-------------------------------------------------------------------------
 * Function:    h5json_uuid_generate
 *
 * Purpose:     Generate a 128-bit UUID from /dev/urandom
 *
 * Return:      Non-negative on success/Negative on failure
 *              uuid is IN/OUT
 *
 * Programmer:  Frank Willmore
 *              November, 2017
 *
 */
static herr_t h5json_uuid_generate(h5json_uuid_t uuid)
{ 
    herr_t ret_value = SUCCEED;

    FILE* urandom = fopen("/dev/urandom", "r");

    unsigned char* buffer = (unsigned char*)malloc(16);
    size_t bytes_read = fread(buffer, (size_t)1, (size_t)16, urandom);

    sprintf(uuid, "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x", 
           buffer[0], buffer[1], buffer[2], buffer[3], 
           buffer[4], buffer[5], buffer[6], buffer[7], 
           buffer[8], buffer[9], buffer[10], buffer[11], 
           buffer[12], buffer[13], buffer[14], buffer[15]); 
    uuid[36] = '\0';

done:

    free(buffer);
    fclose(urandom);

    return SUCCEED;
} 


/*-------------------------------------------------------------------------
 * Function:    h5json_new_uuid_json_object()`
 *
 * Purpose:     Generate a json_t* string object which wraps a new 
 *              128-bit UUID from /dev/urandom
 *
 * Return:      json_t* object which wraps a new UUID string
 *
 * Programmer:  Frank Willmore
 *              November, 2017
 *
 */
static json_t* h5json_new_uuid_json_object()
{
    h5json_uuid_t uuid; /* stack variable, heap variable will be returned and persist */
    HDassert((h5json_uuid_generate(uuid) == SUCCEED) && "could not generate new uuid.");
    json_t* ret_val = json_string(uuid);
    HDassert((ret_val != NULL) && "could not generate new json_string uuid.");
    return ret_val;
}


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_jansson_to_dataspace()`
 *
 * Purpose:     Convert a 'shape' stored in JANSSON to a dataspace.
 *
 * Return:      new dataspace hid
 *
 * Programmer:  Frank Willmore
 *              December, 2017
 *
 */
hid_t H5VL_json_jansson_to_dataspace(json_t* shape)
{
    hid_t   ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    json_t* class = json_object_get(shape, "class");
    json_t* dims = json_object_get(shape, "dims");
    json_t* maxdims = json_object_get(shape, "maxdims");
    hid_t dataspace = NULL; 

    if (strcmp(json_string_value(class), "H5S_SIMPLE") == 0)
    {
        hsize_t rank = json_array_size(dims);
        hsize_t* current_dims = H5MM_malloc(sizeof(hsize_t) * rank);
        hsize_t* maximum_dims = H5MM_malloc(sizeof(hsize_t) * rank);
        hsize_t index;
        for (index=0; index<rank; index++) 
        {
            current_dims[index] = json_integer_value(json_array_get(dims, index));
            maximum_dims[index] = json_integer_value(json_array_get(maxdims, index));
        }

        dataspace = H5Screate_simple((int)rank, current_dims, maximum_dims);

        H5MM_free(current_dims);
        H5MM_free(maximum_dims);
    }
    else
    {
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, NULL, "non-simple dataspace classes not implemented. ");
    }

    HDassert(dataspace >= 0);
    ret_value = dataspace;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end dataspace function */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_jansson_to_datatype()`
 *
 * Purpose:     Convert a 'type' stored in JANSSON to a datatype.
 *
 * Return:      new datatype hid
 *
 * Programmer:  Frank Willmore
 *              December, 2017
 *
 */
hid_t H5VL_json_jansson_to_datatype(json_t* type)
{
    hid_t   ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    json_t* datatype_class = json_object_get(type, "class");

    hid_t datatype = NULL;
    char* datatype_class_str = json_string_value(datatype_class);

    /* switch over datatypes */
    if (strcmp(datatype_class_str, "H5T_STRING") == 0)
    {
        //datatype = H5Tcopy(H5T_STRING);
        datatype = H5Tcopy(H5T_NATIVE_CHAR);
    }
    else if (strcmp(datatype_class_str, "H5T_INTEGER") == 0)
    {        
        /* H5T_NATIVE_LLONG storage class will be long long,
         * the largest supported native type and big enough to store 
         * any integer value. In JSON, it's rendered in base 10. */
        datatype = H5Tcopy(H5T_NATIVE_LLONG);
    }
    else
    {
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, NULL, "Unkown datatype.")
    }

    HDassert(datatype >= 0);
    ret_value = datatype;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end datatype function */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_datatype_to_jansson()`
 *
 * Purpose:     Convert a datatype to it's JANSSON representation.
 *
 * Return:      new datatype json_t*
 *
 * Programmer:  Frank Willmore
 *              December, 2017
 *
 */
json_t* H5VL_json_datatype_to_jansson(hid_t type_id)
{
    json_t*   ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    json_t* type = json_object();
    H5T_class_t type_class = H5Tget_class(type_id);
    
    switch (type_class)
    {
        case H5T_INTEGER:
            json_object_set_new(type, "class", json_string("H5T_INTEGER")); 
            break;

        case H5T_FLOAT:
            json_object_set_new(type, "class", json_string("H5T_FLOAT")); 
            break;
            
        case H5T_STRING:
            json_object_set_new(type, "class", json_string("H5T_STRING")); 
            break;
            
        case H5T_BITFIELD:
            json_object_set_new(type, "class", json_string("H5T_BITFIELD")); 
            break;
            
        case H5T_OPAQUE:
            json_object_set_new(type, "class", json_string("H5T_OPAQUE")); 
            break;
            
        case H5T_COMPOUND:
            json_object_set_new(type, "class", json_string("H5T_COMPOUND")); 
            break;
            
        case H5T_REFERENCE:
            json_object_set_new(type, "class", json_string("H5T_REFERENCE")); 
            break;
            
        case H5T_ENUM:
            json_object_set_new(type, "class", json_string("H5T_ENUM")); 
            break;
            
        case H5T_VLEN:
            json_object_set_new(type, "class", json_string("H5T_VLEN")); 
            break;
            
        case H5T_ARRAY:
            json_object_set_new(type, "class", json_string("H5T_ARRAY")); 
            break;
        
        default:
            HGOTO_ERROR(H5E_ATTR, H5E_CANTCREATE, FAIL, "Type class not supported.");
    }

    ret_value = type;

done:

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_dataspace_to_jansson()`
 *
 * Purpose:     Convert a dataspace to it's JANSSON representation.
 *
 * Return:      new dataspace json_t*
 *
 * Programmer:  Frank Willmore
 *              December, 2017
 *
 */
json_t* H5VL_json_dataspace_to_jansson(hid_t space_id)
{
    json_t*   ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* shape:  First get the dataspace class/type */
    json_t* shape = json_object();
    htri_t is_simple = H5Sis_simple( space_id );
    HDassert(is_simple >= 0);

    if (is_simple) 
    {
        /* insert the class */
        json_object_set_new(shape, "class", json_string("H5S_SIMPLE"));

        /* get the dims and max dims */
        hsize_t  ndims   = H5Sget_simple_extent_ndims(space_id);
        hsize_t* dims    = (hsize_t*)H5MM_malloc(sizeof(hsize_t) * ndims);
        hsize_t* maxdims = (hsize_t*)H5MM_malloc(sizeof(hsize_t) * ndims);
        H5Sget_simple_extent_dims(space_id, dims, maxdims);

        json_t* dims_json_array = json_array();
        json_object_set_new(shape, "dims", dims_json_array);
        json_t* maxdims_json_array = json_array();
        json_object_set_new(shape, "maxdims", maxdims_json_array);

        unsigned i = 0;
        for (i=0; i<ndims; i++) 
        {
            json_array_append(dims_json_array, json_integer(dims[i]));
            json_array_append(maxdims_json_array, json_integer(maxdims[i]));
        }

        H5MM_free(dims);
        H5MM_free(maxdims);
    } 
    else /* null or scalar */
    {
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "FTW: datasdet create with null / scalar dataspace not yet implemented.\n");
    }

    ret_value = shape;

done:

    FUNC_LEAVE_NOAPI(ret_value)
}


/* ----------------------------------------------------------------------------
 * Function:    H5VL_json_write_value
 *
 * Purpose:     Helper function to write data from a buffer to a JANSSON object
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * IN:          pointer to JANSSON value array
 *              pointer to allocated buffer 
 *
 * OUT:         value_array containing the data written from buffer
 *
 * Programmer:  Frank Willmore 
 *              December, 2017
 *
 */
static herr_t 
H5VL_json_write_value(json_t* value_array, hid_t dtype_id, hid_t space_id, void* buffer)
{
    herr_t            ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    /*** writing the value ***/

    // get the value object out of the json
    printf("attribute value before write = *%s* \n", json_dumps(value_array, JSON_INDENT(4)));

    // switch on datatype object, write vals to the json array
    H5T_class_t datatype_class = H5Tget_class(dtype_id);
    size_t datatype_size = H5Tget_size(dtype_id);

    //hsize_t buffer_cursor = buf;

    // get the dims from the space
    int n_dims = H5Sget_simple_extent_ndims( space_id );
    hsize_t* dims = (hsize_t*)H5MM_malloc(sizeof(hsize_t) * n_dims);
    hsize_t* maxdims = (hsize_t*)H5MM_malloc(sizeof(hsize_t) * n_dims);
    H5Sget_simple_extent_dims(space_id, dims, maxdims );

    switch (n_dims)
    {
        case 1:
        {
            for (unsigned i = 0; i < dims[0]; i++)
            {
                int _value =  ((int*)buffer)[i];
                json_t* int_value = json_integer(_value);
                json_array_remove(value_array, i);
                json_array_insert_new(value_array, i, int_value);
            }
            break;
        }
        case 2:
        {
            HGOTO_ERROR(H5E_ATTR, H5E_BADVALUE, FAIL, "two dimensional spaces not supported.")
            break;
        }
        default:
            HGOTO_ERROR(H5E_ATTR, H5E_BADVALUE, FAIL, "rank > two dimensional spaces not supported.")
    }

    H5MM_xfree(dims);
    H5MM_xfree(maxdims);

    ret_value = SUCCEED;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5VL_json_write_value() */


/* ----------------------------------------------------------------------------
 * Function:    H5VL_json_read_value
 *
 * Purpose:     Helper function to read data from a JSON object into a buffer
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * IN:          pointer to value array
 *              pointer to allocated buffer 
 *
 * OUT:         _buffer containing the data read from array
 *
 * Programmer:  Frank Willmore 
 *              December, 2017
 *
 */
static herr_t 
H5VL_json_read_value(json_t* value_array, hid_t dtype_id, hid_t space_id, void* _buffer)
{
    herr_t            ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    H5T_class_t datatype_class = H5Tget_class(dtype_id);
    size_t datatype_size = H5Tget_size(dtype_id);

    /* get the dims from the space */
    int n_dims = H5Sget_simple_extent_ndims( space_id );
    hsize_t* dims = (hsize_t*)H5MM_malloc(sizeof(hsize_t) * n_dims);
    hsize_t* maxdims = (hsize_t*)H5MM_malloc(sizeof(hsize_t) * n_dims);
    H5Sget_simple_extent_dims(space_id, dims, maxdims );

    /*** reading the value ***/
    if (datatype_class = H5T_NATIVE_INT)
    {
        int* buffer = (int*)_buffer; 

    /* XXX:FTW this works... but should generalize for multidimensional array */
        switch (n_dims)
        {
            case 1:
            {
                /* array is a JSON array */
                size_t index;
                json_t *_value;

                json_array_foreach(value_array, index, _value) 
                {
                    json_int_t value = json_integer_value(_value);
                    buffer[index] = (int)value;
                    /* XXX:FTW need to handle datatypes, but can't do datatype = int; */
                }
                break;
            }
            case 2:
            {
                HGOTO_ERROR(H5E_ATTR, H5E_BADVALUE, FAIL, "two dimensional spaces not supported.")
                break;
            }
            default:
                HGOTO_ERROR(H5E_ATTR, H5E_BADVALUE, FAIL, "rank > two dimensional spaces not supported.")
        } /* end switch(n_dims) */
    }
    else
    {
           HGOTO_ERROR(H5E_ATTR, H5E_BADVALUE, FAIL, "Datatype not supported.")
    } /* end if-else(datatype_class) */

    H5MM_xfree(dims);
    H5MM_xfree(maxdims);

    ret_value = SUCCEED;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5VL_json_read_value() */


/* ----------------------------------------------------------------------------
 * Function:    H5VL_json_create_new_group
 *
 * Purpose:     Helper function to traverse links and find group of interest 
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * IN:          groups_in_file / contains json for groups collection
 *              name / name of group to be located/created 
 *              create_intermediate / whether intermediate groups are created
 *
 * IN-OUT:      current_uuid / will contain location from whichto start search,
 *              and will return contiaing the uuid of the new group. 
 *              It is NOT CONSTANT!
 *
 * Programmer:  Frank Willmore 
 *              November, 2017
 * 
*/

static herr_t 
H5VL_json_create_new_group(H5VL_json_object_t* domain, const char* name, h5json_uuid_t *current_uuid, hbool_t create_intermediate)
{
    herr_t            ret_value = FAIL;
    json_t*         current_group = NULL;
    h5json_uuid_t       new_group_uuid;
    json_t*             new_group_json;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("H5VL_json_create_new_group():\n");
    printf("  - Name of new group:              %s\n", name);
    printf("  - Starting from group with uuid:  %s\n", current_uuid);
    printf("  - Create intermediate groups?:    %d\n", create_intermediate);
#endif

    json_t* groups_in_file = json_object_get(domain->u.file.json_file_object, "groups");

    /* group-spotting: search the path/tokens provided, create as needed and allowed. 
       The last token is our target, and library object for it created and returned. */

    /* get the list of tokens in the provided name/path */
    const char s[2] = "/";
    unsigned token_index = 0;
    char copy_of_name[1024];
    char *tokens[256];
    strcpy(copy_of_name, name);
    for (tokens[token_index] = strtok(copy_of_name, s); 
         tokens[token_index++] != NULL; 
         tokens[token_index] = strtok(NULL, s));
    unsigned n_tokens = token_index - 1;

    for(token_index=0; token_index<n_tokens; token_index++)
    {
        /* Locate the current/cursor group in Jansson and get links */
        current_group = json_object_get(groups_in_file, current_uuid);

        /* search links of current group for current token */
        char *current_token = tokens[token_index];
        json_t* links_for_current_group = json_object_get(current_group, "links");
        size_t index;
        json_t *link;
        hbool_t found = FALSE;
        json_array_foreach(links_for_current_group, index, link) 
        {
            /* block of code that uses index and link */
            json_t* title = json_object_get(link, "title");
            if (!strcmp(json_string_value(title), current_token))
            {
                found = TRUE; /* found it. */
                break; /* break out of enclosing json_array_foreach() macro */
            }
        } /* end of loop over link array */

        /* after searching all links, these are the scenarios: */

        /* 1 - last token is found --> group already exists, so fail */
        if((token_index == n_tokens - 1) && found) 
        {
        // FTW outstanding issue: Group create fails silently here, error message not returned. 
        // FTW This mesage prints but no error handling done. 
printf("Found link %s, should fail here.\n", current_token);
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, NULL, "Cannot create link which already exists.")
        }

        /* 2 - intermediate token found, so move on */
        if ((token_index < (n_tokens - 1)) && found)
        {
            current_uuid = (h5json_uuid_t*)json_string_value(json_object_get(link, "id"));
            continue; 
        }

        /* 3 - intermediate or final token not found, so create the group in JANSSON. 
               (or fail for intermediate, depending on gcpl, when implemented) */
        if (!found)       
        {
            /* generate uuid for the new or intermediate group */
            h5json_uuid_generate(new_group_uuid);

            /* create a new group in the groups hashtable */ 
            new_group_json = json_object();
            json_object_set_new(groups_in_file, new_group_uuid, new_group_json);

            /* grab the links from current group, insert a new/empty link object to the new group */
            json_t* link_list = json_object_get(current_group, "links");
            HDassert(link_list != NULL);
            json_t* new_link = json_object();
            HDassert((json_array_append(link_list, new_link) == 0));

            json_object_set_new(new_link, "class", json_string("H5L_TYPE_HARD")); /* FTW: need to get link class from pl? */
            json_object_set_new(new_link, "title", json_string(tokens[token_index]));
            json_object_set_new(new_link, "collection", json_string("groups"));
            json_object_set_new(new_link, "id", json_string(new_group_uuid));

             /* populate fields in that new group */
            json_t *hdf5_path_name_array = json_array(); /* create new, empty array */ 
            json_object_set_new(new_group_json, "alias", hdf5_path_name_array);

            json_t *attribute_collection = json_array(); /* create new, empty array */
            json_object_set_new(new_group_json, "attributes", attribute_collection);

            json_t *link_collection = json_array(); /* create new, empty array */ 
            json_object_set_new(new_group_json, "links", link_collection);

            time_t t = time(NULL);

            /* created */
            char created_UTC_string[64];
            HDassert(h5json_get_utc_string_from_time(t, created_UTC_string) >= 0);
            HDassert(json_object_set_new(new_group_json, "created", json_string(created_UTC_string)) == 0);

            /* lastModified */
            char lastModified_UTC_string[64];
            HDassert(h5json_get_utc_string_from_time(t, lastModified_UTC_string) >= 0);
            HDassert(json_object_set_new(new_group_json, "lastModified", json_string(lastModified_UTC_string)) == 0);

            json_t *creationProperties = json_object(); /* create new, empty array */
            json_object_set_new(new_group_json, "creationProperties",creationProperties);
            
            /* attributes */ 
            json_object_set_new(new_group_json, "attributes", json_array());

            /* set the current group pointer to point to the next one before moving to the next token */
            strncpy(current_uuid, new_group_uuid, sizeof(h5json_uuid_t));

        } /* end of !found */

    } /* end of tokens */

    ret_value = SUCCEED;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5VL_json_create_new_group() */


/* ----------------------------------------------------------------------------
 * Function:    H5VL_json_find_object_by_name()
 *
 * Purpose:     Helper function to traverse links and find the group of interest
 *
 * Return:      pointer to the jansson object 
 **
 * IN:          - domain
 *              - name / name of object to be located
 * IN-OUT:      - current_uuid / uuid of group to begin search / may be modified
 *              - collection / pointer to json_t* which upon return contains a jansson string,
 *                the value of which is "datasets", "datatypes", or "groups".
 *                If a NULL value is supplied, no action
 *              - containing_group_uuid / if a valid pointer is supplied, the uuid for the 
 *                group containing the object of interest will be returned.
 *
 * Programmer:  Frank Willmore 
 *              November, 2017
 * 
 */

static json_t* 
H5VL_json_find_object_by_name(H5VL_json_object_t* domain, const char* name, h5json_uuid_t *current_uuid, json_t** collection, h5json_uuid_t *containing_group_uuid)
{
    json_t*            ret_value = NULL;
    json_t*            current_group = NULL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("H5VL_json_find_object_by_name():      \n");
    printf("  - Locating object:                %s\n", name);
    printf("  - Starting from group with uuid:  %s\n", current_uuid);
#endif

    json_t* groups_in_file = json_object_get(domain->u.file.json_file_object, "groups");

    /* pointer to the containing group's json uuid */
    json_t* containing_group_uuid_json = NULL;

    /* group-spotting: search the path/tokens provided, create as needed and allowed. 
       The last token is our target, and library object for it created and returned. */

    /* get the list of tokens in the provided name/path */
    const char s[2] = "/";
    unsigned token_index = 0;
    char copy_of_name[1024];
    char* tokens[256];
    strcpy(copy_of_name, name);
    for (tokens[token_index] = strtok(copy_of_name, s); 
         tokens[token_index++] != NULL; 
         tokens[token_index] = strtok(NULL, s));
    unsigned n_tokens = token_index - 1;

    for(token_index=0; token_index<n_tokens; token_index++)
    {
        /* Locate the current/cursor group in Jansson and get links */
        current_group = json_object_get(groups_in_file, current_uuid);

        /* search links of current group for current token */
        char *current_token = tokens[token_index];
        json_t* links_for_current_group = json_object_get(current_group, "links");
        hbool_t found = FALSE;
        size_t index;
        json_t* link;
        json_array_foreach(links_for_current_group, index, link) 
        {
            /* iterate over links */

            json_t* title = json_object_get(link, "title");
            if (!strcmp(json_string_value(title), current_token))
            {
                found = TRUE; /* found it. */
                break; /* break out of enclosing json_array_foreach() macro */
            }
        } /* end of loop over link array */

        /*** after searching all links, there are three scenarios: ***/

        /* 1 - last token is found --> link exists so return */
        if((token_index == n_tokens - 1) && found) 
        {
            if (containing_group_uuid != NULL)
            {
                if (containing_group_uuid_json == NULL) 
                {
                    /* this means it was found on first pass 
                     * so copy the current_uuid IN param */
                    strncpy(containing_group_uuid, current_uuid, sizeof(h5json_uuid_t));
                    printf("copying current_uuid to containing: %s\n", containing_group_uuid);
                }
                else
                {
                    strncpy(containing_group_uuid, json_string_value(containing_group_uuid_json), sizeof(h5json_uuid_t));
                }
            }

            if (collection != NULL) 
            {
                *collection = json_object_get(link, "collection");
            }

            ret_value = link;
            break; /* break out of loop over tokens */
        }

        /* 2 - intermediate token found, so move on to next token */
        if ((token_index < (n_tokens - 1)) && found)
        {
            containing_group_uuid_json = json_object_get(link, "id");
            current_uuid = (h5json_uuid_t*)json_string_value(containing_group_uuid_json);
            continue; /* go on to next token */
        }

        /* 3 - intermediate or final token not found. */
        if (!found)       
        {

            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, NULL, "Cannot find link.")

        } /* end of !found */

    } /* end of tokens */

done:

    FUNC_LEAVE_NOAPI(ret_value)

}


/* ----------------------------------------------------------------------------
 * Function:    H5VL_json_delete_link_from_containing_group()
 *
 * Purpose:     Helper function to locate an object, decrement it's reference
 *              count, and remove the link from it's containing group.
 *
 * Returns:     SUCCESS or FAIL 
 *
 * IN:          domain - top-level object container
 *              containing_group_uuid - UUID of group containing link
 *              link - link to be deleted
 *              
 * Programmer:  Frank Willmore 
 *              January, 2017
 * 
 */

static herr_t
H5VL_json_delete_link_from_containing_group(H5VL_json_object_t* domain, h5json_uuid_t containing_group_uuid, json_t* link)
{
    herr_t ret_value = FAIL;
    char* object_collection_name;
    h5json_uuid_t object_uuid;

    FUNC_ENTER_NOAPI_NOINIT

    /* manage reference count */
    object_collection_name = json_string_value(json_object_get(link, "collection"));
    strncpy(object_uuid, json_string_value(json_object_get(link, "id")), sizeof(h5json_uuid_t)); 
    json_t* object_collection = json_object_get(domain->object_json, object_collection_name);
    json_t* object_of_interest = json_object_get(object_collection, object_uuid);

    json_decref(object_of_interest);

    json_t* groups_in_file = json_object_get(domain->object_json, "groups");
    json_t* group_of_interest = json_object_get(groups_in_file, containing_group_uuid);
    json_t* links_in_group = json_object_get(group_of_interest, "links");

    /* array is a JSON array */
    size_t index;
    json_t *value;

    json_array_foreach(links_in_group, index, value) 
    {
        /* block of code that uses index and value */
        if (json_equal(value, link)) 
        {
            break;    
        }
    }

    /* For robustness, remove should be done outside of the loop */
    json_array_remove(links_in_group, index);

    ret_value = SUCCEED;

done:

    FUNC_LEAVE_NOAPI(ret_value)

}


/* ----------------------------------------------------------------------------
 * Function:    H5VL_json_insert_link_into_group()
 *
 * Purpose:     Helper function to locate an object, increment it's reference
 *              count, and insert the link into a containing group.
 *
 * Returns:     SUCCESS or FAIL 
 *
 * IN:          domain - top-level object container
 *              containing_group_uuid - UUID of group to contain link
 *              link - link to be inserted
 *              
 * Programmer:  Frank Willmore 
 *              January, 2017
 * 
 */

static herr_t
H5VL_json_insert_link_into_group(H5VL_json_object_t* domain, h5json_uuid_t containing_group_uuid, json_t* link)
{
    herr_t ret_value = FAIL;
    char* object_collection_name;
    h5json_uuid_t object_uuid;

    FUNC_ENTER_NOAPI_NOINIT

    /* manage reference count */
    object_collection_name = json_string_value(json_object_get(link, "collection"));
    strncpy(object_uuid, json_string_value(json_object_get(link, "id")), sizeof(h5json_uuid_t)); 
    json_t* object_collection = json_object_get(domain->object_json, object_collection_name);
    json_t* object_of_interest = json_object_get(object_collection, object_uuid);
    json_incref(object_of_interest);

    json_t* groups_in_file = json_object_get(domain->object_json, "groups");
    json_t* group_of_interest = json_object_get(groups_in_file, containing_group_uuid);
    json_t* links_in_group = json_object_get(group_of_interest, "links");

    json_array_append(links_in_group, link);

    ret_value = SUCCEED;

done:

    FUNC_LEAVE_NOAPI(ret_value)

}

