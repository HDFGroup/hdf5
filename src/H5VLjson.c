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
 * Purpose: An implementation of a VOL plugin to access HDF5 data in a
 *          JSON-oriented manner
 */
/* XXX: Replace as much stack-allocated memory with dynamically growing memory as possible */
/* XXX: Eventually replace CURL PUT calls with CURLOPT_UPLOAD calls */

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

#define PREDEFINED_DATATYPE_NAME_MAX_LENGTH 20

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

/* helper function to generate UUIDs */
static herr_t h5json_uuid_generate(h5json_uuid_t uuid);
/* helper function to express time as UTC */
static herr_t h5json_get_utc_string_from_time(time_t t, char *time_buf);

/* Helper functions for converting between dataspace/datatype and their jansson representations */
static hid_t H5VL_json_jansson_to_dataspace(json_t* shape);
static hid_t H5VL_json_jansson_to_datatype(json_t* type);
static json_t* H5VL_json_datatype_to_jansson(hid_t datatype);
static json_t* H5VL_json_dataspace_to_jansson(hid_t dataspace);

/* write buffered values for an attribute or dataset to a jansson array */
static herr_t H5VL_json_write_value(json_t* value_array, hid_t dtype_id, hid_t space_id, void* buffer);
static herr_t H5VL_json_read_value(json_t* value_array, hid_t dtype_id, hid_t space_id, void* _buffer);

/* delete a link */
static herr_t H5VL_json_delete_link_from_containing_group(H5VL_json_object_t* domain, h5json_uuid_t containing_group_uuid, json_t* link);

//FTW Jordan's stuff

/* Alternate, more portable version of the basename function which doesn't modify its argument */
static const char *get_basename(const char *path);

/* Set of callbacks for H5VL_json_parse_response() */
static herr_t yajl_copy_object_URI_parse_callback(char *HTTP_response, void *callback_data_in, void *callback_data_out);
static herr_t get_link_type_callback(char *HTTP_response, void *callback_data_in, void *callback_data_out);

/* Conversion functions to convert a JSON-format string to an HDF5 Datatype or vice versa */
static const char *H5VL_json_convert_predefined_datatype_to_string(hid_t type_id);
static const char *H5VL_json_convert_datatype_class_to_string(hid_t type_id);
static herr_t      H5VL_json_convert_datatype_to_string(hid_t type_id, char **type_body, size_t *type_body_len, hbool_t nested);
static hid_t       H5VL_json_convert_string_to_datatype(const char *type);

/* Helper function to retrieve the datatype properties of the given H5VL_json_object_t
 * and set up a datatype hid_t for the object, which must be a Dataset, Datatype or
 * an Attribute
 */
static herr_t H5VL_json_parse_datatype(H5VL_json_object_t *object);

/* Helper function to setup a Dataspace when opening an existing Dataset or Attribute */
static herr_t H5VL_json_parse_dataspace(H5VL_json_object_t *object);

/* Helper function to convert a selection within an HDF5 Dataspace into a JSON-format string */
static herr_t H5VL_json_convert_dataspace_selection_to_string(hid_t space_id, char *selection_string, hbool_t req_param);

/* Helper function to convert a data buffer into a JSON array when using variable-length types */
static herr_t H5VL_json_convert_data_buffer_to_json_array(const void *buf, hid_t mem_type_id, hid_t mem_space_id, char **out_body, size_t *out_body_len);

/* Helper function to locate a group */
static herr_t H5VL_json_create_new_group(H5VL_json_object_t* domain, const char* name, h5json_uuid_t *current_uuid, hbool_t create_intermediate);
static json_t* H5VL_json_find_object_by_name(H5VL_json_object_t* domain, const char* name, h5json_uuid_t *current_uuid, json_t** collection, h5json_uuid_t *containing_group_uuid);

/* Helper functions for creating a Dataset */
//static herr_t H5VL_json_parse_dataset_create_options(void *parent_obj, const char *name, hid_t dcpl, char *create_request_body);
//static herr_t H5VL_json_parse_dataset_create_shape_and_maxdims(hid_t space_id, char *shape_body, char *maxdims_body);
//static herr_t H5VL_json_parse_dataset_creation_properties(hid_t dcpl_id, char *creation_properties_body);

/* Helper functions for creating an Attribute */
//static herr_t H5VL_json_parse_attribute_create_options(H5VL_json_object_t *parent_obj, const char *name, hid_t acpl, char *create_request_body);


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

    /* default value is set to json null. If H5D_FILL_TIME_IFSET were 
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
// This function will fill the given buffer with data from the value array.
printf("FTW attribute json value = %s\n", json_dumps(value, JSON_INDENT(4)));
H5VL_json_read_value(value, dtype_id, space_id, buf);
printf("FTW after H5VL_json_read_value\n");

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
//    printf("  - parent UUID: %s\n", attr->u.attribute.parent_obj->object_uuid);
//    printf("  - attribute name: %s\n", attr->u.attribute.attr_name);
    printf("  - DXPL: %ld\n\n", dxpl_id);
#endif

printf("FTW attr = %lu\n", attr);
    HDassert(H5I_ATTR == attr->obj_type && "not an attribute");
printf("FTW\n");

    if (attr->u.attribute.dtype_id != FAIL && H5Tclose(attr->u.attribute.dtype_id) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close datatype")
    if (attr->u.attribute.space_id != FAIL && H5Sclose(attr->u.attribute.space_id) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close dataspace")
printf("FTW\n");

    H5MM_xfree(attr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_attr_close() */


//FTW datatypes not started, 22 Dec 2017
static void *
H5VL_json_datatype_commit(void *obj, H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name, hid_t type_id,
                          hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *parent = (H5VL_json_object_t *) obj;
    H5VL_json_object_t *new_datatype = NULL;
//FTW    char                commit_request_host_header[HOST_HEADER_MAX_LENGTH] = "Host: ";
//FTW    char               *commit_request_body = NULL;
//FTW    char               *datatype_body = NULL;
//FTW    char               *link_body = NULL;
//FTW    char                temp_url[URL_MAX_LENGTH];
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
//FTW    printf("  - Parent Object URI: %s\n", parent->URI);
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

#if 0
    /* Form the request body to commit the Datatype */
    if (NULL == (commit_request_body = (char *) H5MM_malloc(REQUEST_BODY_MAX_LENGTH)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, NULL, "can't allocate space for datatype commit request body")

    if (H5VL_json_convert_datatype_to_string(type_id, &datatype_body, NULL, FALSE) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, NULL, "can't convert datatype to string representation")

    /* Only create a link for the Datatype if this isn't an H5Tcommit_anon call */
    if (name) {
        if (NULL == (link_body = (char *) H5MM_malloc(DATATYPE_CREATE_LINK_BODY_MAX_LENGTH)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, NULL, "can't allocate space for datatype link body")

        snprintf(link_body, DATATYPE_CREATE_LINK_BODY_MAX_LENGTH,
                 "\"link\": {"
                     "\"id\": \"%s\", "
                     "\"name\": \"%s\""
                 "}",
                 parent->URI,
                 get_basename(name));
    } /* end if */
    snprintf(commit_request_body, REQUEST_BODY_MAX_LENGTH, "{ %s%s%s }",
             datatype_body,
             link_body ? ", " : "",
             link_body ? link_body : "");

    /* Setup the "Host: " header */
    curl_headers = curl_slist_append(curl_headers, strncat(commit_request_host_header, parent->domain->u.file.filepath_name, HOST_HEADER_MAX_LENGTH));

    /* Disable use of Expect: 100 Continue HTTP response */
    curl_headers = curl_slist_append(curl_headers, "Expect:");

    /* Redirect from base URL to "/datatypes" to commit the datatype */
    snprintf(temp_url, URL_MAX_LENGTH, "%s/datatypes", base_URL);

    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, curl_headers);
    curl_easy_setopt(curl, CURLOPT_POST, 1);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, commit_request_body);
    curl_easy_setopt(curl, CURLOPT_URL, temp_url);

    CURL_PERFORM(curl, H5E_DATATYPE, H5E_BADVALUE, NULL);

    /* Store the newly-committed Datatype's URI */
    if (H5VL_json_parse_response(response_buffer.buffer, NULL, new_datatype->URI, yajl_copy_object_URI_parse_callback) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, NULL, "can't parse committed datatype's URI")
#endif

#ifdef PLUGIN_DEBUG
    printf("Datatype H5VL_json_object_t fields:\n");
//FTW    printf("  - Datatype URI: %s\n", new_datatype->URI);
    printf("  - Datatype Object type: %d\n", new_datatype->obj_type);
//FTW    printf("  - Datatype Parent Domain path: %s\n", new_datatype->domain->u.file.filepath_name);
#endif

    ret_value = (void *) new_datatype;

done:
#ifdef PLUGIN_DEBUG
//FTW    printf("Datatype commit request body: %s\n\n", commit_request_body);
//FTW    printf("Datatype commit response buffer: %s\n\n", response_buffer.buffer);
#endif

#if 0
    if (commit_request_body)
        H5MM_xfree(commit_request_body);
    if (datatype_body)
        H5MM_xfree(datatype_body);
    if (link_body)
        H5MM_xfree(link_body);
#endif

    /* Clean up allocated datatype object if there was an issue */
    if (new_datatype && !ret_value)
        if (H5VL_json_datatype_close(new_datatype, FAIL, NULL) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, NULL, "unable to close datatype")

    /* Restore cURL URL to the base URL */
//    curl_easy_setopt(curl, CURLOPT_URL, base_URL);

//    if (curl_headers) {
//        curl_slist_free_all(curl_headers);
//        curl_headers = NULL;
//    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_datatype_commit() */


static void *
H5VL_json_datatype_open(void *obj, H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
                        hid_t tapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *parent = (H5VL_json_object_t *) obj;
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

    /* Traverse links until the named Datatype is found */
//    search_ret = H5VL_json_find_link_by_path(parent, name, yajl_copy_object_URI_parse_callback, NULL, datatype->URI);
//    if (!search_ret || search_ret < 0)
//        HGOTO_ERROR(H5E_DATATYPE, H5E_PATH, NULL, "unable to locate datatype by path")


    /* Set up the actual datatype by converting the string representation into an hid_t */
//    if (H5VL_json_parse_datatype(datatype) < 0)
//        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, NULL, "unable to parse dataset's datatype")

#ifdef PLUGIN_DEBUG
    printf("Datatype H5VL_json_object_t fields:\n");
//    printf("  - Datatype URI: %s\n", datatype->URI);
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
H5VL_json_datatype_get(void *obj, H5VL_datatype_get_t get_type, hid_t H5_ATTR_UNUSED dxpl_id,
                       void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_json_object_t *dtype = (H5VL_json_object_t *) obj;
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
H5VL_json_datatype_close(void *dt, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_json_object_t *_dtype = (H5VL_json_object_t *) dt;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Datatype close call with following parameters:\n");
//    printf("  - URI: %s\n", _dtype->URI);
#endif

    HDassert(H5I_DATATYPE == _dtype->obj_type && "not a datatype");

    if (_dtype->u.datatype.dtype_id != FAIL && H5Tclose(_dtype->u.datatype.dtype_id) < 0)
        HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "can't close datatype")

    H5MM_xfree(_dtype);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_datatype_close() */


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

printf("Got n_points = %lu\n", n_points);
    int* buffer = (int*)H5MM_malloc(n_points * sizeof(int));

    H5MM_xfree(dims);
    H5MM_xfree(maxdims);

    H5D_fill_time_t fill_time;
    if (H5Pget_fill_time(dcpl_id, &fill_time) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve fill time property")
printf("FTW got fill_time = %d\n", fill_time);

#if 0
    if (H5D_FILL_TIME_IFSET != fill_time)
        snprintf(fill_time_body, DATASET_CREATE_FILL_TIME_BODY_MAX_LENGTH,
                 ", \"fillTime\": \"H5D_FILL_TIME_%s\"",
                 H5D_FILL_TIME_ALLOC == fill_time ? "ALLOC" : "NEVER");
#endif
    
    if (fill_time != H5D_FILL_TIME_NEVER)
    {
printf("Fill time not never, so filling.\n");

        H5D_fill_value_t fill_status;

        if (H5Pfill_value_defined(dcpl_id, &fill_status) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve the fill value defined status")

printf("FTW got fill_status = %d\n", fill_status);

        long fill_value = NULL;
        herr_t err = H5Pget_fill_value( dcpl_id, H5T_NATIVE_INT, &fill_value );
        
printf("FTW got fill_value = %d\n", fill_value);

        for (unsigned i=0; i<n_points; i++) buffer[i] = fill_value;

//        if (H5D_FILL_VALUE_DEFAULT != fill_status) {
//            strcat(fill_value_body, ", \"fillValue\": ");

//            if (H5D_FILL_VALUE_UNDEFINED == fill_status) {
//                strcat(fill_value_body, "null");
//            } /* end if */
//            else {
                /* XXX: Support for fill values */
//            } /* end else */
//        } /* end if */

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
    // This function will fill the given buffer with data from the value array.
    printf("FTW dataset json value = %s\n", json_dumps(value, JSON_INDENT(4)));
    H5VL_json_read_value(value, mem_type_id, mem_space_id, buf);
    printf("FTW after H5VL_json_read_value\n");

//FTW ignore file_space for now
#if 0
    if (H5S_ALL == file_space_id) {
        /* Set up a valid file dataspace to use for the dataset read */
        file_space_id = H5Scopy(dataset->u.dataset.space_id);
        H5Sselect_all(file_space_id);
        must_close_filespace = true;
    } /* end if */
    else {
        if (NULL == (selection_body = (char *) H5MM_malloc(DIMENSION_ARRAY_MAX_LENGTH)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate space for selection body")
        selection_body[0] = '\0';

        if (H5VL_json_convert_dataspace_selection_to_string(file_space_id, selection_body, TRUE) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "can't convert dataspace to string representation")
    } /* end else */

    /* Verify that the number of selected points matches */
    if ((mem_select_npoints = H5Sget_select_npoints(mem_space_id)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "invalid dataspace")
    if ((file_select_npoints = H5Sget_select_npoints(file_space_id)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "invalid dataspace")
    HDassert((mem_select_npoints == file_select_npoints) && "memory selection num points != file selection num points");

    if (0 == (dtype_size = H5Tget_size(mem_type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

    if ((is_variable_str = H5Tis_variable_str(mem_type_id)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")
#endif

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
        /* XXX: Unable to support until property lists are ironed out with HSDS */
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
 *              FTW: Implement the case of (not) creating intermediate
 *              groups if this property is set in the LCPL
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
//    new_group->object_json = new_group_json;
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
 * Programmer:  Jordan Henderson
 *              March, 2017
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

    /* Lifecycle: Free JANSSON group_uuid object */
    json_decref(group->object_uuid);

    /* free the library object */
    H5MM_xfree(group);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_json_group_close() */


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
            printf("Got UUID of the object of interest = %s\n", the_object_uuid);
            printf("Wubba Lubba Dub Dub!\n");

            /* locate the group (using the helper function) where the new link is to be placed */
            json_t* groups_in_file = json_object_get(new_link_location->domain->object_json, "groups");
            json_t* destination_group = json_object_get(groups_in_file, new_link_location->object_uuid);
            json_t* destination_group_links = json_object_get(destination_group, "links");

            /* insert a json_t* link object into that group's link collection. */
            json_t* new_link = json_object();
            json_object_set_new(new_link, "class", json_string("H5L_TYPE_HARD"));
            json_object_set_new(new_link, "title", json_string(loc_params.loc_data.loc_by_name.name));
            /* increase the ref count, since we're increasing the ref count to the object. */
//FTW something fishy, make sure I'm incref'ing correct object
            json_object_set_new(new_link, "collection", json_incref(collection));
            json_object_set_new(new_link, "id", json_string(the_object_uuid));
            json_array_append_new(destination_group_links, new_link);

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
H5VL_json_link_copy(void *src_obj, H5VL_loc_params_t loc_params1,
                    void *dst_obj, H5VL_loc_params_t loc_params2,
                    hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_link_copy() */


static herr_t
H5VL_json_link_move(void *src_obj, H5VL_loc_params_t loc_params1,
                    void *dst_obj, H5VL_loc_params_t loc_params2,
                    hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_link_move() */


static herr_t
H5VL_json_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type,
                   hid_t dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_json_object_t *link = (H5VL_json_object_t *) obj;
//FTW    hbool_t             curl_perform = FALSE;
//FTW    char                get_request_header[HOST_HEADER_MAX_LENGTH] = "Host: ";
//FTW    char                temp_url[URL_MAX_LENGTH];
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("Received Link get call with following parameters:\n");
    printf("  - Get type: %d\n", get_type);
//    printf("  - Link URI: %s\n", link->URI);
//    printf("  - Link File: %s\n\n", link->domain->u.file.filepath_name);
#endif

#if 0
    if (curl_perform) {
        /* Setup the "Host: " header */
        curl_headers = curl_slist_append(curl_headers, strncat(get_request_header, link->domain->u.file.filepath_name, HOST_HEADER_MAX_LENGTH - 7));

        /* Disable use of Expect: 100 Continue HTTP response */
        curl_headers = curl_slist_append(curl_headers, "Expect:");

        /* Redirect from base URL to "/groups/<>/links/<name>" to get information about the link */
        /* snprintf(temp_url, URL_MAX_LENGTH, "%s/groups/%s/links/%s", base_URL, link->obj.); */

        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, curl_headers);
        curl_easy_setopt(curl, CURLOPT_HTTPGET, 1);
        curl_easy_setopt(curl, CURLOPT_URL, temp_url);

        CURL_PERFORM(curl, H5E_LINK, H5E_CANTGET, FAIL);
    } /* end if */
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
#ifdef PLUGIN_DEBUG
//    printf("Link Get response buffer: %s\n\n", response_buffer.buffer);
#endif

#if 0
    if (curl_perform) {
        /* Restore cURL URL to the base URL */
        curl_easy_setopt(curl, CURLOPT_URL, base_URL);

        if (curl_headers) {
            curl_slist_free_all(curl_headers);
            curl_headers = NULL;
        } /* end if */
    } /* end if */
#endif

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
#endif
            /* find the link */
            h5json_uuid_t current_uuid;
            h5json_uuid_t containing_group_uuid;
            strncpy(current_uuid, loc_obj->object_uuid, sizeof(current_uuid));
            json_t* link = H5VL_json_find_object_by_name(loc_obj->domain, loc_params.loc_data.loc_by_name.name, &current_uuid, NULL, &containing_group_uuid);
printf("FTW link to be deleted: \n %s \n", json_dumps(link, JSON_INDENT(4))); 
printf("FTW in containing group %s\n", containing_group_uuid);
H5VL_json_delete_link_from_containing_group(loc_obj->domain, containing_group_uuid, link);
            /* if it exists, delete it, otherwise fail */
            if (link != NULL)
            {
                
//FTW WIP above is finding the correct link. 
printf("FTW group containing link of interest: \n %s \n", json_dumps(loc_obj->object_json, JSON_INDENT(4))); 
json_t* group_links = json_object_get(loc_obj->object_json, "links");
printf("FTW group links: \n %s \n", json_dumps(group_links, JSON_INDENT(4))); 

                /* iterate the link array to find the link to be deleted */
                size_t index;
                json_t *value;
                json_array_foreach(group_links, index, value) 
                {
                    /* block of code that uses index and value */
                    if (strcmp(json_string_value(value), current_uuid) == 0) 
                    {
                        /* found link; delete it and break loop */
printf("FTW removing link %s\n", json_dumps(value, JSON_INDENT(4)));
                        json_array_remove(group_links, index);
                        break;
                    }
                }
            }
            else
                HGOTO_ERROR(H5E_LINK, H5E_BADVALUE, FAIL, "Can not delete link which does not exist.");
             
            break;
        }

        /* H5Lexists */
        case H5VL_LINK_EXISTS:
        {
            htri_t *ret = va_arg (arguments, htri_t *);
            json_t* collection; /*unused*/
            h5json_uuid_t current_uuid;
            strncpy(current_uuid, loc_obj->object_uuid, sizeof(current_uuid));
//            json_t* link = H5VL_json_find_object_by_name(loc_obj->domain, loc_params.loc_data.loc_by_name.name, current_uuid, &collection, NULL);
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
herr_t h5json_get_utc_string_from_time(time_t t, char *time_buf)
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

//    FUNC_ENTER_NOAPI_NOINIT

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
//    FUNC_LEAVE_NOAPI(ret_value)
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


/* XXX: Potentially modify to deal with the trailing slash case */
static const char*
get_basename(const char *path)
{
    char *substr = strrchr(path, '/');
    return substr ? substr + 1 : path;
} /* end get_basename() */


static herr_t
get_link_type_callback(char *HTTP_response, void H5_ATTR_UNUSED *callback_data_in, void *callback_data_out)
{
    const char *soft_link_class_keys[] = { "link", "class", (const char *) 0 };
    const char *link_collection_keys[] = { "link", "collection", (const char *) 0 };
    yajl_val    parse_tree, key_obj;
    char       *parsed_string;

    if (NULL == (parse_tree = yajl_tree_parse(HTTP_response, NULL, 0))) return FAIL;

    /* To handle the awkward case of soft and external links, which do not return an "ID",
     * first check for the link class field and short circuit if it is found to be
     * equal to "H5L_TYPE_SOFT"
     */
    if (NULL != (key_obj = yajl_tree_get(parse_tree, soft_link_class_keys, yajl_t_string))) {
        char *link_type;

        if (NULL == (link_type = YAJL_GET_STRING(key_obj))) {
            yajl_tree_free(parse_tree);
            return FAIL;
        } /* end if */

        if (!strcmp(link_type, "H5L_TYPE_SOFT") || !strcmp(link_type, "H5L_TYPE_EXTERNAL") ||
                !strcmp(link_type, "H5L_TYPE_UD")) {
            yajl_tree_free(parse_tree);
            return SUCCEED;
        } /* end if */
    } /* end if */

    /* Retrieve the object's type */
    if (NULL == (key_obj = yajl_tree_get(parse_tree, link_collection_keys, yajl_t_string))) {
        yajl_tree_free(parse_tree);
        return FAIL;
    } /* end if */

    if (!YAJL_IS_STRING(key_obj)) {
        yajl_tree_free(parse_tree);
        return FAIL;
    } /* end if */

    if (NULL == (parsed_string = YAJL_GET_STRING(key_obj))) {
        yajl_tree_free(parse_tree);
        return FAIL;
    } /* end if */

    if (!strcmp(parsed_string, "groups"))
        *((H5I_type_t *) callback_data_out) = H5I_GROUP;
    else if (!strcmp(parsed_string, "datasets"))
        *((H5I_type_t *) callback_data_out) = H5I_DATASET;
    else if (!strcmp(parsed_string, "datatypes"))
        *((H5I_type_t *) callback_data_out) = H5I_DATATYPE;

    yajl_tree_free(parse_tree);

    return SUCCEED;
} /* end get_link_type_callback() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_find_link_by_path
 *
 * Purpose:     Given the name of a Link, traverse links until the named
 *              Link is found. If link_found_callback is specified, the
 *              callback will be performed on the supplied arguments and
 *              will be passed the HTTP response given by the final GET
 *              request in the chain (corresponding to the found link).
 *
 *              Note that this function is not generally meant to be called
 *              when looking for the root group "/". The function handles
 *              this special case by just making a GET request on the root
 *              group of the file corresponding to the parent object's
 *              domain pointer, calls the callback and supplies the HTTP
 *              response, and returns TRUE that it found the link. This
 *              is mostly due to the fact that whereas a GET Link operation
 *              (for any path that isn't "/") returns the ID of a link
 *              under the JSON keys:
 *
 *              "link" -> "id",
 *
 *              a GET Group operation (solely for the path "/") returns the
 *              ID of the root group under the JSON key:
 *
 *              "id",
 *
 *              which can cause the link_found callback to fail when it
 *              tries to retrieve and copy back the object's URI.
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  Jordan Henderson
 *              March, 2017
 */
/* XXX: If leading slash not supplied, loop is skipped and wrong ID is returned */
#if 0
static htri_t
H5VL_json_find_link_by_path(H5VL_json_object_t *parent_obj, const char *link_path,
    herr_t (*link_found_callback)(char *, void *, void *), void *callback_data_in, void *callback_data_out)
{
    const char *target_link_name;
    size_t      link_path_len;
    char       *current_link_name;
    char       *temp_path = NULL;
//FTW    char        curr_URI[URI_MAX_LENGTH];
//FTW    char        host_header[HOST_HEADER_MAX_LENGTH] = "Host: ";
//FTW    char        temp_URL[URL_MAX_LENGTH];
    htri_t      ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(parent_obj);
    HDassert(link_path);
    HDassert((H5I_FILE == parent_obj->obj_type || H5I_GROUP == parent_obj->obj_type)
                && "parent object not a file or group");

    /* Make a modifiable copy of the link path to keep track of the current object while
     * iterating through links */
    link_path_len = strlen(link_path);
    if (NULL == (temp_path = (char *) H5MM_malloc(link_path_len + 1)))
        HGOTO_ERROR(H5E_LINK, H5E_CANTALLOC, FAIL, "can't allocate space for copy of link path")
    strncpy(temp_path, link_path, link_path_len);
    temp_path[link_path_len] = '\0';

    /* Get the basename of the link path to find the actual name of the link being searched for */
    target_link_name = get_basename(link_path);
#endif

#if 0
    /* Use the parent object URI as the first URI to start iterating through links from */
//    strncpy(curr_obj_URI, parent_obj->URI, URI_MAX_LENGTH);
//printf("not getting value for curr_obj_URI here:  %s\n", curr_obj_URI);
    /* Handle the case where the root group is being searched for */
    if (!strcmp(temp_path, "/")) {
        snprintf(temp_URL, URL_MAX_LENGTH, "%s/groups/%s", base_URL, parent_obj->domain->URI);

        if (link_found_callback && H5VL_json_parse_response(response_buffer.buffer,
                callback_data_in, callback_data_out, link_found_callback) < 0)
            HGOTO_ERROR(H5E_LINK, H5E_CALLBACK, FAIL, "unable to perform callback operation")

        HGOTO_DONE(TRUE);
    } /* end if */

    /* XXX: Handle the case where "." is being searched for */
    if (!strcmp(temp_path, ".")) {

    } /* end if */
#endif

#if 0
    if (*temp_path == '/')
        /* XXX: Use the file object's URI as the first URI to start iterating through links from */
        strncpy(curr_URI, parent_obj);
    else
        /* Use the parent object's URI as the first URI to start iterating through links from */
        strncpy(curr_URI, parent_obj->URI, URI_MAX_LENGTH);

    current_link_name = strtok(temp_path, "./\\");

    while (current_link_name) {

        /* At this point, iteration could either be in the middle of the link path, or
         * could have arrived at the last link name in the path.
         *
         * If iteration is in the middle of the link path, the standard callback should
         * be called so that H5VL_json_parse_response will parse the HTTP response for
         * this GET request, grab the URI of the link for which information was retrieved
         * on this iteration, and copy the URI of the link back here to curr_URI so that
         * iteration can continue searching from this link.
         *
         * If, however, iteration has arrived at the last link name in the link path,
         * the optional user callback should be called if specified. This way, the final
         * iteration can do special processing on the user's side without having to call
         * H5VL_json_parse_response twice.
         */

    /* If specified, copy the final URI into the URI field of the given H5VL_json_object_t struct */
        if (strcmp(current_link_name, target_link_name)) {
            /* There is still an additional link in the full link path beyond the current
             * one being processed. Parse HTTP response and store the URI of the retrieved
             * link, if it is a hard link, so that the next iteration will continue
             * traversing through links.
             *
             * Note that the parse function has no way of determining ahead of time whether it is parsing
             * the response of a hard or soft link. However, the parse callback must determine this
             * in some manner so that it will not throw an error when looking for the "id" field in the
             * response (soft links don't return an ID like hard links do). Therefore, the callback checks
             * for the link class field and short circuits if it determines the link is a soft link. - JTH
             */
            if (H5VL_json_parse_response(response_buffer.buffer, NULL, curr_URI, yajl_copy_object_URI_parse_callback) < 0)
                HGOTO_ERROR(H5E_LINK, H5E_NONE_MINOR, FAIL, "unable to parse response")
        } /* end if */
        else {
            /* Iteration has arrived at the last link in the full link path. At this point,
             * the optionally supplied callback can be called safely, or the function can
             * just return, signalling that the link was found.
             */
            if (link_found_callback && H5VL_json_parse_response(response_buffer.buffer,
                    callback_data_in, callback_data_out, link_found_callback) < 0)
                HGOTO_ERROR(H5E_LINK, H5E_CALLBACK, FAIL, "unable to perform callback operation")

            ret_value = TRUE;
        } /* end else */

        current_link_name = strtok(NULL, "./\\");
    } /* end while */
//printf("Got link_data = %d\n", link_data);
//printf("Got curr_obj_URI = %s\n", curr_obj_URI);

done:
    if (temp_path)
        H5MM_free(temp_path);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_find_link_by_path() */
#endif


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_convert_predefined_datatype_to_string
 *
 * Purpose:     Given a predefined Datatype, returns a string
 *              representation of that Datatype.
 *
 * Return:      The string representation of the Datatype given by type_id,
 *              if type_id is a valid Datatype; NULL otherwise
 *
 * Programmer:  Jordan Henderson
 *              April, 2017
 */
static const char *
H5VL_json_convert_predefined_datatype_to_string(hid_t type_id)
{
    H5T_class_t  type_class = H5T_NO_CLASS;
    H5T_order_t  type_order = H5T_ORDER_NONE;
    H5T_sign_t   type_sign = H5T_SGN_NONE;
    static char  type_name[PREDEFINED_DATATYPE_NAME_MAX_LENGTH];
    size_t       type_size;
    char        *ret_value = type_name;

    FUNC_ENTER_NOAPI_NOINIT

    if (H5T_NO_CLASS == (type_class = H5Tget_class(type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, NULL, "invalid datatype")

    if (!(type_size = H5Tget_size(type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, NULL, "invalid datatype size")

    if (H5T_ORDER_ERROR == (type_order = H5Tget_order(type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, NULL, "invalid datatype ordering")

    if (type_class == H5T_INTEGER)
        if (H5T_SGN_ERROR == (type_sign = H5Tget_sign(type_id)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, NULL, "invalid datatype sign")

    snprintf(type_name, PREDEFINED_DATATYPE_NAME_MAX_LENGTH,
             "H5T_%s_%s%zu%s",
             (type_class == H5T_INTEGER) ? "STD" : "IEEE",
             (type_class == H5T_FLOAT) ? "F" : (type_sign == H5T_SGN_NONE) ? "U" : "I",
             type_size * 8,
             (type_order == H5T_ORDER_LE) ? "LE" : "BE");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_convert_predefined_datatype_to_string() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_convert_datatype_class_to_string
 *
 * Purpose:     Given a Datatype, returns a string representation of the
 *              class of the Datatype
 *
 * Return:      The string representation of the class of the Datatype
 *              given by type_id, if type_id is a valid Datatype; NULL otherwise
 *
 * Programmer:  Jordan Henderson
 *              March, 2017
 */
static const char *
H5VL_json_convert_datatype_class_to_string(hid_t type_id)
{
    const char *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    switch (H5Tget_class(type_id)) {
        case H5T_INTEGER:   HGOTO_DONE("H5T_INTEGER")
        case H5T_FLOAT:     HGOTO_DONE("H5T_FLOAT")
        case H5T_STRING:    HGOTO_DONE("H5T_STRING")
        case H5T_BITFIELD:  HGOTO_DONE("H5T_BITFIELD")
        case H5T_OPAQUE:    HGOTO_DONE("H5T_OPAQUE")
        case H5T_COMPOUND:  HGOTO_DONE("H5T_COMPOUND")
        case H5T_REFERENCE: HGOTO_DONE("H5T_REFERENCE")
        case H5T_ENUM:      HGOTO_DONE("H5T_ENUM")
        case H5T_VLEN:      HGOTO_DONE("H5T_VLEN")
        case H5T_ARRAY:     HGOTO_DONE("H5T_ARRAY")
        case H5T_TIME:      HGOTO_DONE("H5T_TIME")
        case H5T_NCLASSES:
        case H5T_NO_CLASS:
        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, NULL, "invalid datatype")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_convert_datatype_class_to_string() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_convert_datatype_to_string
 *
 * Purpose:     Given a datatype, this function creates a JSON-formatted
 *              string representation of the datatype.
 *
 *              Can be called recursively for the case of Array and
 *              Compound Datatypes. The parameter 'nested' should always be
 *              supplied as FALSE, as the function itself handles the
 *              correct passing of the parameter when processing nested
 *              datatypes (such as the base type for an Array datatype).
 *
 * Return:      Non-negative on success/negative on failure
 *
 * Programmer:  Jordan Henderson
 *              July, 2017
 */
static herr_t
H5VL_json_convert_datatype_to_string(hid_t type_id, char **type_body, size_t *type_body_len, hbool_t nested)
{
    const char  *leading_string = "\"type\": "; /* Leading string for all datatypes */
    hsize_t     *array_dims = NULL;
    size_t       initial_string_len = strlen(leading_string);
    size_t       out_string_len = 0;
    size_t       bytes_to_print = 0;            /* Used to calculate whether the datatype body buffer needs to be grown */
    size_t       positive_ptrdiff = 0;
    size_t       type_size;
    size_t       i;
    hid_t        type_base_class = FAIL;
    hid_t        compound_member = FAIL;
    void        *enum_value = NULL;
    char        *enum_value_name = NULL;
    char        *enum_mapping = NULL;
    char        *array_base_type = NULL;
    char       **compound_member_strings = NULL;
    char        *compound_member_name = NULL;
    char        *out_string = NULL;
    char        *out_string_curr_pos;           /* The "current position" pointer used to print to the appropriate place
                                                  in the buffer and not overwrite important leading data */
    int          bytes_printed = 0;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(type_body);

    /* Determine whether the caller has allocated initial space to use */
    if (type_body_len && *type_body_len > 0) {
        out_string = *type_body;
        out_string_len = *type_body_len;
    } /* end if */
    else {
        if (NULL == (out_string = (char *) H5MM_malloc(DATATYPE_BODY_DEFAULT_SIZE)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "can't allocate space for datatype body string")
        out_string_len = DATATYPE_BODY_DEFAULT_SIZE;
    } /* end else */

    /* Keep track of the current position in the resulting string so everything
     * gets added smoothly
     */
    out_string_curr_pos = out_string;

#ifdef PLUGIN_DEBUG
    printf("  - Initial datatype-to-string buffer size is %zu\n\n", out_string_len);
#endif

    /* Make sure the buffer is at least large enough to hold the leading "type" string */
    CHECKED_REALLOC(out_string, out_string_len, initial_string_len + 1,
            out_string_curr_pos, H5E_DATATYPE, FAIL);

#ifdef PLUGIN_DEBUG
    printf("  - After re-alloc check, buffer is %zu bytes\n\n", out_string_len);
#endif

    /* Add the leading "'type': " string */
    if (!nested) {
        strncpy(out_string, leading_string, out_string_len);
        out_string_curr_pos += initial_string_len;
    } /* end if */

    if (!(type_size = H5Tget_size(type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

    switch (H5Tget_class(type_id)) {
        case H5T_INTEGER:
        case H5T_FLOAT:
        {
            const char         *class_name;
            const char         *type_name;
            const char * const  fmt_string = "{"
                                                 "\"class\": \"%s\", "
                                                 "\"base\": \"%s\""
                                             "}";

            if (NULL == (class_name = H5VL_json_convert_datatype_class_to_string(type_id)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

            if (NULL == (type_name = H5VL_json_convert_predefined_datatype_to_string(type_id)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

            /* Check whether the buffer needs to be grown */
            bytes_to_print = strlen(class_name) + strlen(type_name) + (strlen(fmt_string) - 4) + 1;

            H5_CHECKED_ASSIGN(positive_ptrdiff, size_t, out_string_curr_pos - out_string, ptrdiff_t);
            CHECKED_REALLOC(out_string, out_string_len, positive_ptrdiff + bytes_to_print, out_string_curr_pos, H5E_DATATYPE, FAIL);

#ifdef PLUGIN_DEBUG
            printf("  - After re-alloc check, buffer is %zu bytes\n\n", out_string_len);
#endif

            if ((bytes_printed = snprintf(out_string_curr_pos, out_string_len - positive_ptrdiff, fmt_string, class_name, type_name)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_SYSERRSTR, FAIL, "snprintf error")

            out_string_curr_pos += bytes_printed;

            break;
        } /* H5T_INTEGER */ /* H5T_FLOAT */

        case H5T_STRING:
        {
            htri_t is_vlen;

            if ((is_vlen = H5Tis_variable_str(type_id)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "unable to determine if datatype is variable-length string")

            /* Build the Datatype body by appending the character set for the string type,
             * any type of string padding, and the length of the string */
            /* Note: currently only H5T_CSET_ASCII is supported for the character set and
             * only H5T_STR_NULLPAD is supported for string padding, but these may change
             * in the future
             */
            if (is_vlen) {
                const char * const fmt_string = "{"
                                                    "\"class\": \"H5T_STRING\", "
                                                    "\"charSet\": \"%s\", "
                                                    "\"strPad\": \"%s\", "
                                                    "\"length\": \"H5T_VARIABLE\""
                                                "}";

                bytes_to_print = (strlen(fmt_string) - 4) + 1;

                H5_CHECKED_ASSIGN(positive_ptrdiff, size_t, out_string_curr_pos - out_string, ptrdiff_t);
                CHECKED_REALLOC(out_string, out_string_len, positive_ptrdiff + bytes_to_print, out_string_curr_pos, H5E_DATATYPE, FAIL);

#ifdef PLUGIN_DEBUG
                printf("  - After re-alloc check, buffer is %zu bytes\n\n", out_string_len);
#endif

                if ((bytes_printed = snprintf(out_string_curr_pos, out_string_len - initial_string_len,
                                              fmt_string, "H5T_CSET_ASCII", "H5T_STR_NULLTERM")) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_SYSERRSTR, FAIL, "snprintf error")

                out_string_curr_pos += bytes_printed;
            } /* end if */
            else {
                const char * const fmt_string = "{"
                                                    "\"class\": \"H5T_STRING\", "
                                                    "\"charSet\": \"%s\", "
                                                    "\"strPad\": \"%s\", "
                                                    "\"length\": %zu"
                                                "}";

                bytes_to_print = (strlen(fmt_string) - 7) + 1;

                H5_CHECKED_ASSIGN(positive_ptrdiff, size_t, out_string_curr_pos - out_string, ptrdiff_t);
                CHECKED_REALLOC(out_string, out_string_len, positive_ptrdiff + bytes_to_print, out_string_curr_pos, H5E_DATATYPE, FAIL);

#ifdef PLUGIN_DEBUG
                printf("  - After re-alloc check, buffer is %zu bytes\n\n", out_string_len);
#endif

                if ((bytes_printed = snprintf(out_string_curr_pos, out_string_len - initial_string_len,
                                              fmt_string, "H5T_CSET_ASCII", "H5T_STR_NULLPAD", type_size)) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_SYSERRSTR, FAIL, "snprintf error")

                out_string_curr_pos += bytes_printed;
            } /* end else */

            break;
        } /* H5T_STRING */

        case H5T_COMPOUND:
        {
            const char         *compound_type_leading_string = "{\"class\": \"H5T_COMPOUND\", \"fields\": [";
            size_t              compound_type_leading_strlen = strlen(compound_type_leading_string);
            size_t              complete_section_len;
            int                 nmembers;
            const char * const  fmt_string = "{"
                                                 "\"name\": \"%s\", "
                                                 "%s"
                                             "}%s";

            if ((nmembers = H5Tget_nmembers(type_id)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't retrieve number of members in compound datatype")

            if (NULL == (compound_member_strings = (char **) H5MM_malloc(((size_t) nmembers + 1) * sizeof(*compound_member_strings))))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "can't allocate space for compound datatype member strings")

            for (i = 0; i < (size_t) nmembers + 1; i++)
                compound_member_strings[i] = NULL;

            H5_CHECKED_ASSIGN(positive_ptrdiff, size_t, out_string_curr_pos - out_string, ptrdiff_t);
            CHECKED_REALLOC(out_string, out_string_len, positive_ptrdiff + compound_type_leading_strlen + 1,
                    out_string_curr_pos, H5E_DATATYPE, FAIL);

#ifdef PLUGIN_DEBUG
            printf("  - After re-alloc check, buffer is %zu bytes\n\n", out_string_len);
#endif

            strncpy(out_string_curr_pos, compound_type_leading_string, compound_type_leading_strlen);
            out_string_curr_pos += compound_type_leading_strlen;

            for (i = 0; i < (size_t) nmembers; i++) {
                if ((compound_member = H5Tget_member_type(type_id, (unsigned) i)) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get compound datatype member")

                if (H5VL_json_convert_datatype_to_string(compound_member, &compound_member_strings[i], NULL, FALSE) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't convert compound datatype member to string representation")

                if (NULL == (compound_member_name = H5Tget_member_name(type_id, (unsigned) i)))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get compound datatype member name")

#ifdef PLUGIN_DEBUG
                printf("  - Compound Datatype member %zu name: %s\n", i, compound_member_name);
                printf("  - Compound Datatype member %zu: %s\n\n", i, compound_member_strings[i]);
#endif

                /* Check whether the buffer needs to be grown */
                bytes_to_print = strlen(compound_member_name) + strlen(compound_member_strings[i])
                        + (strlen(fmt_string) - 6) + (i < (size_t) nmembers - 1 ? 2 : 0) + 1;

                H5_CHECKED_ASSIGN(positive_ptrdiff, size_t, out_string_curr_pos - out_string, ptrdiff_t);
                CHECKED_REALLOC(out_string, out_string_len, positive_ptrdiff + bytes_to_print,
                        out_string_curr_pos, H5E_DATATYPE, FAIL);

#ifdef PLUGIN_DEBUG
                printf("  - After re-alloc check, buffer is %zu bytes\n\n", out_string_len);
#endif

                if ((bytes_printed = snprintf(out_string_curr_pos, out_string_len - positive_ptrdiff,
                                              fmt_string, compound_member_name, compound_member_strings[i],
                                              i < (size_t) nmembers - 1 ? ", " : "")) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_SYSERRSTR, FAIL, "snprintf error")

                out_string_curr_pos += bytes_printed;

                if (H5Tclose(compound_member) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "can't close datatype")
                if (H5free_memory(compound_member_name) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't free compound datatype member name buffer")
                compound_member = FAIL;
                compound_member_name = NULL;
            } /* end for */

            H5_CHECKED_ASSIGN(complete_section_len, size_t, out_string_curr_pos - out_string + 2 + 1, ptrdiff_t);
            CHECKED_REALLOC(out_string, out_string_len, complete_section_len, out_string_curr_pos, H5E_DATATYPE, FAIL);

#ifdef PLUGIN_DEBUG
            printf("  - After re-alloc check, buffer is %zu bytes\n\n", out_string_len);
#endif

            strcat(out_string_curr_pos, "]}");

            break;
        } /* H5T_COMPOUND */

        case H5T_ENUM:
        {
            const char         *base_type_name;
            size_t              enum_mapping_length = 0;
            char               *mapping_curr_pos;
            int                 enum_nmembers;
            const char * const  mapping_fmt_string = "\"%s\": %lld%s";
            const char * const  fmt_string = "{"
                                                 "\"class\": \"H5T_ENUM\", "
                                                 "\"base\": {"
                                                     "\"class\": \"H5T_INTEGER\", "
                                                     "\"base\": \"%s\""
                                                 "}, "
                                                 "\"mapping\": {"
                                                     "%s"
                                                 "}"
                                             "}";

            if ((enum_nmembers = H5Tget_nmembers(type_id)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "unable to get number of members of enumerated type")

            if (NULL == (enum_value = H5MM_malloc(type_size)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "unable to allocate space for enum member value")

            if (NULL == (enum_mapping = (char *) H5MM_malloc(ENUM_MAPPING_DEFAULT_SIZE)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "unable to allocate space for enum mapping")
            enum_mapping_length = ENUM_MAPPING_DEFAULT_SIZE;

#ifdef PLUGIN_DEBUG
            printf("  - Enum mapping string buffer initial length is %zu bytes\n\n", enum_mapping_length);
#endif

            for (i = 0, mapping_curr_pos = enum_mapping; i < (size_t) enum_nmembers; i++) {
                if (NULL == (enum_value_name = H5Tget_member_name(type_id, (unsigned) i)))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "unable to get name of enum member")

                if (H5Tget_member_value(type_id, (unsigned) i, enum_value) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve value of enum member")

                /* Check if the mapping buffer needs to grow */
                bytes_to_print = strlen(enum_value_name) + MAX_NUM_LENGTH + (strlen(mapping_fmt_string) - 8)
                        + (i < (size_t) enum_nmembers - 1 ? 2 : 0) + 1;

                H5_CHECKED_ASSIGN(positive_ptrdiff, size_t, mapping_curr_pos - enum_mapping, ptrdiff_t);
                CHECKED_REALLOC(enum_mapping, enum_mapping_length, positive_ptrdiff + bytes_to_print,
                        mapping_curr_pos, H5E_DATATYPE, FAIL);

                /* Append the name of this enum mapping value and its corresponding numeric value to the mapping list */
                /* XXX: Need to cast the enum_value to the appropriate size type */
                if ((bytes_printed = snprintf(mapping_curr_pos, enum_mapping_length - positive_ptrdiff,
                                              mapping_fmt_string, enum_value_name, *((long long int *) enum_value),
                                              i < (size_t) enum_nmembers - 1 ? ", " : "")) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_SYSERRSTR, FAIL, "snprintf error")

                mapping_curr_pos += bytes_printed;

                if (H5free_memory(enum_value_name) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "unable to free memory allocated for enum member name")
                enum_value_name = NULL;
            } /* end for */

#ifdef PLUGIN_DEBUG
            printf("  - After re-alloc checks, mapping buffer is %zu bytes\n\n", enum_mapping_length);
#endif

            if ((type_base_class = H5Tget_super(type_id)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "cant get base datatype for enum type")

            if (NULL == (base_type_name = H5VL_json_convert_predefined_datatype_to_string(type_base_class)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype")

            /* Check whether the buffer needs to be grown */
            bytes_to_print = strlen(base_type_name) + strlen(enum_mapping) + (strlen(fmt_string) - 4) + 1;

            H5_CHECKED_ASSIGN(positive_ptrdiff, size_t, out_string_curr_pos - out_string, ptrdiff_t);
            CHECKED_REALLOC(out_string, out_string_len, positive_ptrdiff + bytes_to_print,
                    out_string_curr_pos, H5E_DATATYPE, FAIL);

#ifdef PLUGIN_DEBUG
            printf("  - After re-alloc check, buffer is %zu bytes\n\n", out_string_len);
#endif

            /* Build the Datatype body by appending the base integer type class for the enum
             * and the mapping values to map from numeric values to
             * string representations.
             */
            if ((bytes_printed = snprintf(out_string_curr_pos, out_string_len - positive_ptrdiff,
                                          fmt_string, base_type_name, enum_mapping)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_SYSERRSTR, FAIL, "snprintf error")

            out_string_curr_pos += bytes_printed;

            break;
        } /* H5T_ENUM */

        case H5T_ARRAY:
        {
            char               *array_shape_curr_pos;
            char                array_shape[DIMENSION_ARRAY_MAX_LENGTH] = "";
            int                 ndims;
            const char * const  fmt_string = "{"
                                                 "\"class\": \"H5T_ARRAY\", "
                                                 "\"base\": %s, "
                                                 "\"dims\": %s"
                                             "}";

            if ((ndims = H5Tget_array_ndims(type_id)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "can't get array datatype number of dimensions")

            if (NULL == (array_dims = (hsize_t *) H5MM_malloc((size_t) ndims * sizeof(*array_dims))))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "can't allocate space for array datatype dimensions")

            if (H5Tget_array_dims(type_id, array_dims) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get array datatype dimensions")

            strcat(array_shape, "[ ");
            array_shape_curr_pos = array_shape + 2;

            /* Setup the shape of the array Datatype */
            for (i = 0; i < (size_t) ndims; i++) {
                if (i > 0) {
                    strcat(array_shape, ", ");
                    array_shape_curr_pos += 2;
                } /* end if */

                if ((bytes_printed = snprintf(array_shape_curr_pos, MAX_NUM_LENGTH, "%llu", array_dims[i])) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_SYSERRSTR, FAIL, "snprintf error")

                array_shape_curr_pos += bytes_printed;
            } /* end for */

            strcat(array_shape, " ]");

            /* Get the class and name of the base datatype */
            if ((type_base_class = H5Tget_super(type_id)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get base datatype for array type")

            if (H5VL_json_convert_datatype_to_string(type_base_class, &array_base_type, 0, TRUE) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't convert datatype to string representation")

            /* Check whether the buffer needs to be grown */
            bytes_to_print = strlen(array_base_type) + strlen(array_shape) + (strlen(fmt_string) - 4) + 1;

            H5_CHECKED_ASSIGN(positive_ptrdiff, size_t, out_string_curr_pos - out_string, ptrdiff_t);
            CHECKED_REALLOC(out_string, out_string_len, positive_ptrdiff + bytes_to_print,
                    out_string_curr_pos, H5E_DATATYPE, FAIL);

#ifdef PLUGIN_DEBUG
            printf("  - After re-alloc check, buffer is %zu bytes\n\n", out_string_len);
#endif

            /* Build the Datatype body by appending the array type class and base type and dimensions of the array */
            if ((bytes_printed = snprintf(out_string_curr_pos, out_string_len - positive_ptrdiff,
                                          fmt_string, array_base_type, array_shape)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_SYSERRSTR, FAIL, "snprintf error")

            out_string_curr_pos += bytes_printed;

            break;
        } /* H5T_ARRAY */

        case H5T_BITFIELD:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported datatype")
            break;
        case H5T_OPAQUE:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported datatype")
            break;
        case H5T_REFERENCE:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported datatype")
            break;
        case H5T_VLEN:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported datatype")
            break;
        case H5T_TIME:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported datatype")
            break;
        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "invalid datatype")
    } /* end switch */

#ifdef PLUGIN_DEBUG
    printf("  - Final datatype-to-string buffer size is %zu\n\n", out_string_len);
#endif

    *type_body = out_string;
    if (type_body_len) *type_body_len = out_string_len;

done:
    if (type_base_class >= 0)
        H5Tclose(type_base_class);
    if (compound_member >= 0)
        H5Tclose(compound_member);
    if (compound_member_name)
        H5free_memory(compound_member_name);
    if (compound_member_strings) {
        for (i = 0; compound_member_strings[i]; i++)
            H5MM_xfree(compound_member_strings[i]);
        H5MM_xfree(compound_member_strings);
    } /* end if */
    if (array_base_type)
        H5MM_xfree(array_base_type);
    if (array_dims)
        H5MM_xfree(array_dims);
    if (enum_value)
        H5MM_xfree(enum_value);
    if (enum_value_name)
        H5free_memory(enum_value_name);
    if (enum_mapping)
        H5MM_xfree(enum_mapping);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_convert_datatype_to_string() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_convert_string_to_datatype
 *
 * Purpose:     Given a JSON string representation of a datatype, creates
 *              and returns an hid_t for the datatype using H5Tcreate().
 *
 *              Can be called recursively for the case of Array and
 *              Compound Datatypes. The parameter 'nested' should always be
 *              supplied as FALSE, as the function itself handles the
 *              correct passing of the parameter when processing nested
 *              datatypes (such as the base type for an Array datatype).
 *
 *              NOTE: Support for Compound Datatypes is quite ugly. In
 *              order to be able to support Compound of Compound datatypes,
 *              Compound of Array, etc., as well as arbitrary whitespace
 *              inside the JSON string, all without modifying the string,
 *              sacrifices in performance had to be made. Not supporting
 *              these features would diminish the usefulness of this
 *              feature as a general JSON string-to-Datatype conversion
 *              function.
 *
 *              As an example of where performance dips, consider the
 *              recursive processing of a Compound Datatype. For each field
 *              in the Compound Datatype, the datatype string must be
 *              searched for the beginning and end of the field's datatype
 *              section, which corresponds to the JSON "type" key. This
 *              process generally involves at least a string search each
 *              for the beginning and end of the section, but possibly
 *              more if there are multiply nested types inside the field.
 *              E.g. a field of Compound w/ a field of Compound could take
 *              two or more string searches for both the beginning and end
 *              of the section. After the beginning and end of the datatype
 *              section for the given field has been found, this substring
 *              is then copied into a separate buffer so it can be
 *              recursively processed without modifying the original
 *              datatype string. Finally, after all of the recursive
 *              processing has been done and the singular field of the
 *              Compound Datatype has been converted from a string into an
 *              HDF5 Datatype, processing moves on to the next field in the
 *              Compound Datatype.
 *
 *
 * Return:      The identifier for the new datatype, which must be closed
 *              with H5Tclose(), if successful. Returns negative otherwise
 *
 * Programmer:  Jordan Henderson
 *              July, 2017
 */
/* XXX: Update to support opening of all datatypes */
static hid_t
H5VL_json_convert_string_to_datatype(const char *type)
{
    const char  *class_keys[] = { "class", (const char *) 0 };
    const char  *type_base_keys[] = { "base", (const char *) 0 };
    yajl_val     parse_tree = NULL, key_obj = NULL;
    size_t       i;
    hid_t        datatype = FAIL;
    hid_t       *compound_member_type_array = NULL;
    hid_t        enum_base_type = FAIL;
    char       **compound_member_names = NULL;
    char        *datatype_class = NULL;
    char        *array_base_type_substring = NULL;
    char        *tmp_cmpd_type_buffer = NULL;
    char        *tmp_enum_base_type_buffer = NULL;
    hid_t        ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("  - Converting String-to-Datatype buffer %s to hid_t\n", type);
#endif

    /* Retrieve the datatype class */
    if (NULL == (parse_tree = yajl_tree_parse(type, NULL, 0)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to parse datatype from string representation")

    if (NULL == (key_obj = yajl_tree_get(parse_tree, class_keys, yajl_t_string)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to parse datatype from string representation")

    if (NULL == (datatype_class = YAJL_GET_STRING(key_obj)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to parse datatype from string representation")

    /* Create the appropriate datatype or copy an existing one */
    if (!strcmp(datatype_class, "H5T_INTEGER")) {
        hbool_t  is_predefined = TRUE;
        char    *type_base = NULL;

        if (NULL == (key_obj = yajl_tree_get(parse_tree, type_base_keys, yajl_t_string)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve datatype base type")

        if (NULL == (type_base = YAJL_GET_STRING(key_obj)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve datatype base type")

        if (is_predefined) {
            hbool_t  is_unsigned;
            hid_t    predefined_type = FAIL;
            char    *type_base_ptr = type_base + 8;

#ifdef PLUGIN_DEBUG
            printf("    Predefined Integer type sign: %c\n", *type_base_ptr);
#endif

            is_unsigned = (*type_base_ptr == 'U') ? TRUE : FALSE;

            switch (*(type_base_ptr + 1)) {
                /* 8-bit integer */
                case '8':
#ifdef PLUGIN_DEBUG
                    printf("    8-bit Integer type\n");
#endif

                    if (*(type_base_ptr + 2) == 'L') {
                        /* Litle-endian */
#ifdef PLUGIN_DEBUG
                        printf("    Little-endian - %s\n", is_unsigned ? "unsigned" : "signed");
#endif

                        predefined_type = is_unsigned ? H5T_STD_U8LE : H5T_STD_I8LE;
                    } /* end if */
                    else {
#ifdef PLUGIN_DEBUG
                        printf("    Big-endian - %s\n", is_unsigned ? "unsigned" : "signed");
#endif

                        predefined_type = is_unsigned ? H5T_STD_U8BE : H5T_STD_I8BE;
                    } /* end else */

                    break;

                /* 16-bit integer */
                case '1':
#ifdef PLUGIN_DEBUG
                    printf("    16-bit Integer type\n");
#endif

                    if (*(type_base_ptr + 3) == 'L') {
                        /* Litle-endian */
#ifdef PLUGIN_DEBUG
                        printf("    Little-endian - %s\n", is_unsigned ? "unsigned" : "signed");
#endif

                        predefined_type = is_unsigned ? H5T_STD_U16LE : H5T_STD_I16LE;
                    } /* end if */
                    else {
#ifdef PLUGIN_DEBUG
                        printf("    Big-endian - %s\n", is_unsigned ? "unsigned" : "signed");
#endif

                        predefined_type = is_unsigned ? H5T_STD_U16BE : H5T_STD_I16BE;
                    } /* end else */

                    break;

                /* 32-bit integer */
                case '3':
#ifdef PLUGIN_DEBUG
                    printf("    32-bit Integer type\n");
#endif

                    if (*(type_base_ptr + 3) == 'L') {
                        /* Litle-endian */
#ifdef PLUGIN_DEBUG
                        printf("    Little-endian - %s\n", is_unsigned ? "unsigned" : "signed");
#endif

                        predefined_type = is_unsigned ? H5T_STD_U32LE : H5T_STD_I32LE;
                    } /* end if */
                    else {
#ifdef PLUGIN_DEBUG
                        printf("    Big-endian - %s\n", is_unsigned ? "unsigned" : "signed");
#endif

                        predefined_type = is_unsigned ? H5T_STD_U32BE : H5T_STD_I32BE;
                    } /* end else */

                    break;

                /* 64-bit integer */
                case '6':
#ifdef PLUGIN_DEBUG
                    printf("    64-bit Integer type\n");
#endif

                    if (*(type_base_ptr + 3) == 'L') {
                        /* Litle-endian */
#ifdef PLUGIN_DEBUG
                        printf("    Little-endian - %s\n", is_unsigned ? "unsigned" : "signed");
#endif

                        predefined_type = is_unsigned ? H5T_STD_U64LE : H5T_STD_I64LE;
                    } /* end if */
                    else {
#ifdef PLUGIN_DEBUG
                        printf("    Big-endian - %s\n", is_unsigned ? "unsigned" : "signed");
#endif

                        predefined_type = is_unsigned ? H5T_STD_U64BE : H5T_STD_I64BE;
                    } /* end else */

                    break;
                default:
                    HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "unknown predefined integer datatype")
            } /* end switch */

            if ((datatype = H5Tcopy(predefined_type)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL, "unable to copy predefined integer datatype")
        } /* end if */
        else {
            /* XXX: Need support for non-predefined integer types */
        } /* end else */
    } /* end if */
    else if (!strcmp(datatype_class, "H5T_FLOAT")) {
        hbool_t  is_predefined = TRUE;
        hid_t    predefined_type = FAIL;
        char    *type_base = NULL;

        if (NULL == (key_obj = yajl_tree_get(parse_tree, type_base_keys, yajl_t_string)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve datatype base type")

        if (NULL == (type_base = YAJL_GET_STRING(key_obj)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve datatype base type")

        if (is_predefined) {
            char *type_base_ptr = type_base + 10;

#ifdef PLUGIN_DEBUG
            printf("    Predefined Float type\n");
#endif

            switch (*type_base_ptr) {
                /* 32-bit floating point */
                case '3':
#ifdef PLUGIN_DEBUG
                    printf("    32-bit Floating Point - %s\n", (*(type_base_ptr + 2) == 'L') ? "Little-endian" : "Big-endian");
#endif

                    /* Determine whether the floating point type is big- or little-endian */
                    predefined_type = (*(type_base_ptr + 2) == 'L') ? H5T_IEEE_F32LE : H5T_IEEE_F32BE;

                    break;

                /* 64-bit floating point */
                case '6':
#ifdef PLUGIN_DEBUG
                    printf("    64-bit Floating Point - %s\n", (*(type_base_ptr + 2) == 'L') ? "Little-endian" : "Big-endian");
#endif

                    predefined_type = (*(type_base_ptr + 2) == 'L') ? H5T_IEEE_F64LE : H5T_IEEE_F64BE;

                    break;
                default:
                    HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "unknown predefined floating-point datatype")
            } /* end switch */

            if ((datatype = H5Tcopy(predefined_type)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL, "unable to copy predefined floating-point datatype")
        } /* end if */
        else {
            /* XXX: need support for non-predefined float types */
        } /* end else */
    } /* end if */
    else if (!strcmp(datatype_class, "H5T_STRING")) {
        const char *length_keys[] = { "length", (const char *) 0 };
        const char *charSetKeys[] = { "charSet", (const char *) 0 };
        const char *strPadKeys[] = { "strPad", (const char *) 0 };
        long long   fixed_length = 0;
        hbool_t     is_variable_str;
        char       *charSet = NULL;
        char       *strPad = NULL;

        /* Retrieve the string datatype's length and check if it's a variable-length string */
        if (NULL == (key_obj = yajl_tree_get(parse_tree, length_keys, yajl_t_any)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve string datatype length")

        is_variable_str = YAJL_IS_STRING(key_obj);


        /* Retrieve and check the string datatype's character set */
        if (NULL == (key_obj = yajl_tree_get(parse_tree, charSetKeys, yajl_t_string)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve string datatype character set")

        if (NULL == (charSet = YAJL_GET_STRING(key_obj)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve string datatype character set")

        /* Currently, only H5T_CSET_ASCII character set is supported */
        if (strcmp(charSet, "H5T_CSET_ASCII"))
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported character set for string datatype")


        /* Retrieve and check the string datatype's string padding */
        if (NULL == (key_obj = yajl_tree_get(parse_tree, strPadKeys, yajl_t_string)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve string datatype padding")

        if (NULL == (strPad = YAJL_GET_STRING(key_obj)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve string datatype padding")

        /* Currently, only H5T_STR_NULLPAD string padding is supported for fixed-length strings
         * and H5T_STR_NULLTERM for variable-length strings */
        if (strcmp(strPad, is_variable_str ? "H5T_STR_NULLTERM" : "H5T_STR_NULLPAD"))
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported string padding for string datatype")


        /* Retrieve the length if the datatype is a fixed-length string */
        if (!is_variable_str) fixed_length = YAJL_GET_INTEGER(key_obj);
        if (fixed_length < 0) HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype length")

        if ((datatype = H5Tcreate(H5T_STRING, is_variable_str ? H5T_VARIABLE : (size_t) fixed_length)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCREATE, FAIL, "unable to create datatype")

        if (H5Tset_cset(datatype, H5T_CSET_ASCII) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCREATE, FAIL, "can't set character set for dataset string datatype")

        if (H5Tset_strpad(datatype, is_variable_str ? H5T_STR_NULLTERM : H5T_STR_NULLPAD) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCREATE, FAIL, "can't set string padding for dataset string datatype")
    } /* end if */
    else if (!strcmp(datatype_class, "H5T_OPAQUE")) {
        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported datatype - opaque")
    } /* end if */
    else if (!strcmp(datatype_class, "H5T_COMPOUND")) {
        const char *field_keys[] = { "fields", (const char *) 0 };
        size_t      tmp_cmpd_type_buffer_size;
        size_t      total_type_size = 0;
        size_t      current_offset = 0;
        char       *type_section_ptr = NULL;

        /* Retrieve the compound member fields array */
        if (NULL == (key_obj = yajl_tree_get(parse_tree, field_keys, yajl_t_array)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve compound datatype members array")

        if (NULL == (compound_member_type_array = (hid_t *) H5MM_malloc(YAJL_GET_ARRAY(key_obj)->len * sizeof(*compound_member_type_array))))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "unable to allocate compound datatype")
        for (i = 0; i < YAJL_GET_ARRAY(key_obj)->len; i++) compound_member_type_array[i] = FAIL;

        if (NULL == (compound_member_names = (char **) H5MM_malloc(YAJL_GET_ARRAY(key_obj)->len * sizeof(*compound_member_names))))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "unable to allocate compound datatype member names array")
        for (i = 0; i < YAJL_GET_ARRAY(key_obj)->len; i++) compound_member_names[i] = NULL;

        if (NULL == (tmp_cmpd_type_buffer = (char *) H5MM_malloc(DATATYPE_BODY_DEFAULT_SIZE)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "unable to allocate temporary buffer for storing type information")
        tmp_cmpd_type_buffer_size = DATATYPE_BODY_DEFAULT_SIZE;

        /* Retrieve the names of all of the members of the Compound Datatype */
        for (i = 0; i < YAJL_GET_ARRAY(key_obj)->len; i++) {
            yajl_val compound_member_field;
            size_t   j;

            if (NULL == (compound_member_field = YAJL_GET_ARRAY(key_obj)->values[i]))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get compound field member %zu information", i)

            for (j = 0; j < YAJL_GET_OBJECT(compound_member_field)->len; j++) {
                if (!strcmp(YAJL_GET_OBJECT(compound_member_field)->keys[j], "name"))
                    if (NULL == (compound_member_names[i] = YAJL_GET_STRING(YAJL_GET_OBJECT(compound_member_field)->values[j])))
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get compound field member %zu name", j)
            } /* end for */
        } /* end for */

        if (NULL == (type_section_ptr = strstr(type, "\"fields\"")))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't find \"fields\" information section in datatype string")

        /* For each field in the Compound Datatype, store the field's name and extract out its
         * "type" substring for recursive processing. Convert that substring into an hid_t and
         * store it for later insertion once the Compound Datatype has been created.
         */
        for (i = 0; i < YAJL_GET_ARRAY(key_obj)->len; i++) {
            size_t  type_section_len = 0;
            char   *type_section_end = NULL;
            char   *last_brace = NULL;
            char   *symbol_ptr = NULL;

            /* Find the beginning of the "type" section for this Compound Datatype member */
            if (NULL == (type_section_ptr = strstr(type_section_ptr, "\"type\"")))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't find \"type\" information section in datatype string")
            if (NULL == (type_section_ptr = strstr(type_section_ptr, "{")))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"type\" information section in datatype string")

            symbol_ptr = type_section_ptr;

            /* Find the end of the "type" section for this Compound Datatype member */
            /* XXX: Still needs refactoring. The "name" section of a member may not come first, throwing another wrench
             * into the string searching. We'll have to look and see which section comes first, the next "type" section
             * or the next "name" section and go from that. Take into special account the fact that "name" or "type" could
             * be part of a member name as well.
             */
            if (i < YAJL_GET_ARRAY(key_obj)->len - 1) {
                char *next_type_name_ptr;

                /* Locate the "name" section of the next Compound Datatype member so we know where to stop
                 * when finding the end of the "type" section for the current Compound Datatype member
                 */
                if (NULL == (next_type_name_ptr = strstr(type_section_ptr, compound_member_names[i + 1])))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't find \"name\" information for next compound datatype member")

                /* Locate the ending brace for the Compound Datatype member, then take the immediately preceding brace
                 * as the end of the member's "type" section
                 */
                while ((symbol_ptr = strstr(symbol_ptr, "},")) && (symbol_ptr < next_type_name_ptr))
                    last_brace = symbol_ptr++;
                if (!last_brace) HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"type\" information section in datatype string")

                symbol_ptr = type_section_ptr;
                while ((symbol_ptr = strstr(symbol_ptr, "}")) && (symbol_ptr < last_brace))
                    type_section_end = symbol_ptr++;
            } /* end if */
            else {
                char *last_bracket_ptr;

                /* This is the last member of the Compound Datatype, so there's no need to look for the following
                 * member's name, as in the case above. Simply search for the ending bracket of the "fields" section
                 * and take the immediately preceding brace to be the end of the "type" section
                 */
                if (NULL == (last_bracket_ptr = strrchr(type, ']')))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"type\" information section in datatype string")

                /* Locate the ending brace for the Compound Datatype member, then take the immediately preceding brace
                 * as the end of the member's "type" section
                 */
                while ((symbol_ptr = strstr(symbol_ptr, "}")) && (symbol_ptr < last_bracket_ptr))
                    last_brace = symbol_ptr++;
                if (!last_brace) HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"type\" information section in datatype string")

                symbol_ptr = type_section_ptr;
                while ((symbol_ptr = strstr(symbol_ptr, "}")) && (symbol_ptr < last_brace))
                    type_section_end = symbol_ptr++;
            } /* end else */

            if (!type_section_end) HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"type\" information section in datatype string")
            H5_CHECKED_ASSIGN(type_section_len, size_t, type_section_end - type_section_ptr + 1, ptrdiff_t);

#ifdef PLUGIN_DEBUG
            printf("  - Compound Datatype member %zu name: %s\n", i, compound_member_names[i]);
            printf("  - Compound datatype member %zu type string len: %zu\n", i, type_section_len);
#endif

            /* Grow the temporary buffer if needed */
//FTW            if (type_section_len + 1 > tmp_cmpd_type_buffer_size)
//FTW                CHECKED_REALLOC_NO_PTR(tmp_cmpd_type_buffer, tmp_cmpd_type_buffer_size, type_section_len + 1, H5E_DATATYPE, FAIL);
//FTW this call is suddenly causing problems... 26.12.2017

#ifdef PLUGIN_DEBUG
            printf("  - After re-alloc check, buffer is size %zu\n", tmp_cmpd_type_buffer_size);
#endif

            HDmemcpy(tmp_cmpd_type_buffer, type_section_ptr, type_section_len);
            tmp_cmpd_type_buffer[type_section_len] = '\0';

            if ((compound_member_type_array[i] = H5VL_json_convert_string_to_datatype(tmp_cmpd_type_buffer)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "unable to convert compound datatype member %zu from string representation", i)

            total_type_size += H5Tget_size(compound_member_type_array[i]);
        } /* end for */

        if ((datatype = H5Tcreate(H5T_COMPOUND, total_type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCREATE, FAIL, "unable to create compound datatype")

        /* Insert all fields into the Compound Datatype */
        for (i = 0; i < YAJL_GET_ARRAY(key_obj)->len; i++) {
            if (H5Tinsert(datatype, compound_member_names[i], current_offset, compound_member_type_array[i]) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINSERT, FAIL, "unable to insert compound datatype member")
            current_offset += H5Tget_size(compound_member_type_array[i]);
        } /* end for */
    } /* end if */
    else if (!strcmp(datatype_class, "H5T_ARRAY")) {
        const char *dims_keys[] = { "dims", (const char *) 0 };
        hsize_t     dims[DATASPACE_MAX_RANK];
        size_t      base_type_substring_len = 0;
        char       *base_type_substring_ptr = NULL;
        hid_t       base_type_id = FAIL;

        /* Retrieve the array dimensions */
        if (NULL == (key_obj = yajl_tree_get(parse_tree, dims_keys, yajl_t_array)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve array datatype dimensions")

        for (i = 0; i < YAJL_GET_ARRAY(key_obj)->len; i++) {
            if (YAJL_IS_INTEGER(YAJL_GET_ARRAY(key_obj)->values[i]))
                dims[i] = (hsize_t) YAJL_GET_INTEGER(YAJL_GET_ARRAY(key_obj)->values[i]);
        } /* end for */

#ifdef PLUGIN_DEBUG
        printf("  - Array datatype dimensions: [");
        for (i = 0; i < YAJL_GET_ARRAY(key_obj)->len; i++) {
            if (i > 0) printf(", ");
            printf("%llu", dims[i]);
        }
        printf("]\n\n");
#endif

        /* Find the location of the "base type" substring and extract it away from the json of the type information
         * by pulling out everything between the matching pair of '{' and '}' */
        if (NULL == (base_type_substring_ptr = strstr(type, "\"base\"")))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't find \"base\" type information in datatype string")
        if (NULL == (base_type_substring_ptr = strstr(base_type_substring_ptr, "{")))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"base\" type information in datatype string")

        /* To find the end of the "base type" section, a quick solution is to repeatedly search for
         * '{' symbols, matching this with the same number of searches for '}', and taking the final
         * '}' to be the end of the section.
         */
        {
            size_t  key_braces_found = 0;
            char   *ptr = base_type_substring_ptr;
            char   *endptr = ptr;

            while ((ptr = strstr(ptr, "{"))) { key_braces_found++; ptr++; }
            ptr = base_type_substring_ptr;
            while (key_braces_found && (ptr = strstr(ptr, "}"))) { key_braces_found--; endptr = ptr++; }

            H5_CHECKED_ASSIGN(base_type_substring_len, size_t, endptr - base_type_substring_ptr + 1, ptrdiff_t);
        }

#ifdef PLUGIN_DEBUG
        printf("  - Array base type substring len: %zu\n", base_type_substring_len);
        fflush(stdout);
#endif

        if (NULL == (array_base_type_substring = (char *) H5MM_malloc(base_type_substring_len + 1)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "unable to allocate space for array base type substring")

        HDmemcpy(array_base_type_substring, base_type_substring_ptr, base_type_substring_len);
        array_base_type_substring[base_type_substring_len] = '\0';

#ifdef PLUGIN_DEBUG
        printf("  - Array base type substring: %s\n\n", array_base_type_substring);
        fflush(stdout);
#endif

        /* Convert the string representation of the array's base datatype to an hid_t */
        if ((base_type_id = H5VL_json_convert_string_to_datatype(array_base_type_substring)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "unable to convert string representation of array base datatype to a usable form")

        if ((datatype = H5Tarray_create(base_type_id, (unsigned) i, dims)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCREATE, FAIL, "creating array datatype failed")
    } /* end if */
    else if (!strcmp(datatype_class, "H5T_ENUM")) {
        const char *mapping_keys[] = { "mapping", (const char *) 0 };
        size_t      base_section_len;
        char       *base_section_ptr = NULL;
        char       *base_section_end = NULL;

        /* Locate the "base" section for the enum datatype and recursively process it */
        if (NULL == (base_section_ptr = strstr(type, "\"base\"")))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted datatype string - missing \"base\" datatype section")
        if (NULL == (base_section_ptr = strstr(base_section_ptr, "{")))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"base\" datatype section in datatype string")
        if (NULL == (base_section_end = strstr(base_section_ptr, "}")))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"base\" datatype section in datatype string")

        H5_CHECKED_ASSIGN(base_section_len, size_t, base_section_end - base_section_ptr + 1, ptrdiff_t);
        if (NULL == (tmp_enum_base_type_buffer = (char *) H5MM_malloc(base_section_len + 1)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "unable to allocate space for enum base datatype temporary buffer")

#ifdef PLUGIN_DEBUG
        printf("  - Allocated %zu bytes for enum base datatype section\n", base_section_len);
#endif

        HDmemcpy(tmp_enum_base_type_buffer, base_section_ptr, base_section_len);
        tmp_enum_base_type_buffer[base_section_len] = '\0';

        if ((enum_base_type = H5VL_json_convert_string_to_datatype(tmp_enum_base_type_buffer)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "unable to convert enum datatype's base datatype section from string into datatype")

        if ((datatype = H5Tenum_create(enum_base_type)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCREATE, FAIL, "unable to create datatype")

        if (NULL == (key_obj = yajl_tree_get(parse_tree, mapping_keys, yajl_t_object)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve enum mapping from enum string representation")

        /* Retrieve the name and value of each member in the enum mapping */
        for (i = 0; i < YAJL_GET_OBJECT(key_obj)->len; i++) {
            long long val;

            if (!YAJL_IS_INTEGER(YAJL_GET_OBJECT(key_obj)->values[i]))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "enum member %zu value is not an integer", i)

            val = YAJL_GET_INTEGER(YAJL_GET_OBJECT(key_obj)->values[i]);

            /* XXX: The insert call may potentially fail or produce incorrect results depending on the base
             * integer type of the enum datatype. In this case, the insert call always tries to pull data from
             * a long long.
             */
            if (H5Tenum_insert(datatype, YAJL_GET_OBJECT(key_obj)->keys[i], (void *) &val) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINSERT, FAIL, "unable to insert member into enum datatype")
        } /* end for */
    } /* end if */
    else if (!strcmp(datatype_class, "H5T_REFERENCE")) {
        /* XXX: Support for reference types */
        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported datatype - reference")
    } /* end if */
    else
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "unknown datatype class")

    ret_value = datatype;

#ifdef PLUGIN_DEBUG
    printf("  - Converted String-to-Datatype buffer to hid_t ID %ld\n\n", datatype);
#endif

done:
    if (ret_value < 0 && datatype >= 0) {
        if (H5Tclose(datatype) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "can't close datatype")
        if (compound_member_type_array) {
            while (FAIL != compound_member_type_array[i])
                if (H5Tclose(compound_member_type_array[i]) < 0)
                    HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "can't close datatype")
        } /* end if */
    } /* end if */

    if (array_base_type_substring)
        H5MM_xfree(array_base_type_substring);
    if (compound_member_type_array)
        H5MM_xfree(compound_member_type_array);
    if (compound_member_names)
        H5MM_xfree(compound_member_names);
    if (tmp_cmpd_type_buffer)
        H5MM_xfree(tmp_cmpd_type_buffer);
    if (tmp_enum_base_type_buffer)
        H5MM_xfree(tmp_enum_base_type_buffer);
    if (FAIL != enum_base_type)
        if (H5Tclose(enum_base_type) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "unable to close enum base datatype")

    if (parse_tree)
        yajl_tree_free(parse_tree);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_convert_string_to_datatype() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_json_parse_datatype
 *
 * Purpose:     Given an existing H5VL_json_object_t object, retrieves its
 *              datatype properties using the JSON API and sets up a
 *              datatype for internal client-side use
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jordan Henderson
 *              July, 2017
 *
 */
static herr_t
H5VL_json_parse_datatype(H5VL_json_object_t *object)
{
    hid_t   datatype = FAIL;
    char    temp_url[URL_MAX_LENGTH];
    char    host_header[HOST_HEADER_MAX_LENGTH] = "Host: ";
    char   *type_section_ptr = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(object);

    /* Setup the "Host: " header */
//    curl_headers = curl_slist_append(curl_headers, strncat(host_header, object->domain->u.file.filepath_name, HOST_HEADER_MAX_LENGTH));

    /* Disable use of Expect: 100 Continue HTTP response */
//    curl_headers = curl_slist_append(curl_headers, "Expect:");

    switch (object->obj_type) {
        case H5I_DATASET:
            /* Redirect cURL to "/datasets/<id>" to get the datatype of the dataset */
//            snprintf(temp_url, URL_MAX_LENGTH, "%s/datasets/%s/type", base_URL, object->URI);
            break;

        case H5I_DATATYPE:
            /* Redirect cURL to "/datatypes/<id>" to get the string representation of the datatype */
//            snprintf(temp_url, URL_MAX_LENGTH, "%s/datatypes/%s", base_URL, object->URI);
            break;

        case H5I_ATTR:
            break;

        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_FILE:
        case H5I_GROUP:
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
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid object type")
    } /* end switch */

#ifdef PLUGIN_DEBUG
//    printf("Accessing link: %s\n\n", temp_url);
#endif

//    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, curl_headers);
//    curl_easy_setopt(curl, CURLOPT_HTTPGET, 1);
//    curl_easy_setopt(curl, CURLOPT_URL, temp_url);

//    CURL_PERFORM(curl, H5E_DATASET, H5E_CANTGET, FAIL);

    /* Find the beginning brace of the "type" section by first searching for "type", then looking for the
     * following brace
     */
//    if (NULL == (type_section_ptr = strstr(response_buffer.buffer, "\"type\"")))
//        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't find \"type\" information section in datatype string")
//    if (NULL == (type_section_ptr = strstr(type_section_ptr, "{")))
//        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"type\" information section in datatype string")

    /* Truncate the response buffer so that only the "type" information is included */
    /* XXX: This will have to be generalized to work with servers other than HSDS due to
     * the "hrefs"-specific information
     */
//    {
//        char *hrefs_section_ptr = NULL;
//        char *symbol_ptr = type_section_ptr;
//        char *section_end = NULL;

//        if (NULL == (hrefs_section_ptr = strstr(response_buffer.buffer, "\"hrefs\"")))
//            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't find \"hrefs\" information in datatype string")

//        if (hrefs_section_ptr < type_section_ptr) {
//            char *last_brace;

//            if (NULL == (last_brace = strrchr(symbol_ptr, '}')))
//                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"type\" information section in datatype string")

//            while ((symbol_ptr = strstr(symbol_ptr, "}")) && (symbol_ptr < last_brace))
//                section_end = symbol_ptr++;

//            if (!section_end) HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"type\" information section in datatype string")
//            *(section_end + 1) = '\0';
//        } /* end if */
//        else {
            /* "hrefs" section is located after the "type" section in the buffer. Search for the last occurence of
             * "}," up to the "hrefs" section, which should mark the end of the "type" section and replace the comma
             * with a NUL character
             */
//            while ((symbol_ptr = strstr(symbol_ptr, "},")) && (symbol_ptr < hrefs_section_ptr))
//                section_end = symbol_ptr++;

//            if (!section_end) HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "incorrectly formatted \"hrefs\" information section in datatype string")
//            *(section_end + 1) = '\0';
//        } /* end else */
//    }

    /* XXX: Be very careful about memory leaks caused when closing datasets of nested array of array datatypes and so on */
    if ((datatype = H5VL_json_convert_string_to_datatype(type_section_ptr)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't convert string representation to datatype")

    switch (object->obj_type) {
        case H5I_DATASET:
            object->u.dataset.dtype_id = datatype;
            break;

        case H5I_DATATYPE:
            object->u.datatype.dtype_id = datatype;
            break;

        case H5I_ATTR:
            object->u.attribute.dtype_id = datatype;
            break;

        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_FILE:
        case H5I_GROUP:
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
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid object type")
    } /* end switch */

done:
#ifdef PLUGIN_DEBUG
//    printf("Link access response buffer: %s\n\n", response_buffer.buffer);
#endif

    if (ret_value < 0 && datatype >= 0)
        if (H5Tclose(datatype) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "unable to close datatype")

//    if (curl_headers) {
//        curl_slist_free_all(curl_headers);
//        curl_headers = NULL;
//    } /* end if */

    /* Reset cURL URL to base URL */
//    curl_easy_setopt(curl, CURLOPT_URL, base_URL);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_parse_datatype() */


static herr_t
H5VL_json_convert_dataspace_selection_to_string(hid_t space_id, char *selection_string, hbool_t req_param)
{
    hsize_t start[DATASPACE_MAX_RANK];
    hsize_t stride[DATASPACE_MAX_RANK];
    hsize_t count[DATASPACE_MAX_RANK];
    size_t  i;
    int     bytes_printed;
    int     ndims;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(selection_string);

    if (NULL == H5I_object_verify(space_id, H5I_DATASPACE))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "not a dataspace")

    if ((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't retrieve dataspace number of dims")

    HDmemset(start, 0, sizeof(start));
    HDmemset(stride, 0, sizeof(stride));
    HDmemset(count, 0, sizeof(count));

    if (req_param) {
        /* Format the selection according to the 'select' request/query parameter. This is composed
         * of N triplets, one for each dimension of the dataset, and looks like:
         *
         * [X:Y:Z, X:Y:Z, ...]
         *
         * where X is the starting coordinate of the selection, Y is the ending coordinate of
         * the selection, and Z is the stride of the selection in that dimension. This formatting
         * is used when reading from a dataset and when performing a write to a Dataset with
         * a fixed-length Datatype. In this case, a binary data transfer can be used, so the
         * selection is specified as a request parameter instead of as JSON keys.
         */
        switch (H5Sget_select_type(space_id)) {
            case H5S_SEL_ALL:
            case H5S_SEL_NONE:
                break;
            case H5S_SEL_POINTS:
                break;
            case H5S_SEL_HYPERSLABS:
                if (H5Sget_regular_hyperslab(space_id, start, stride, count, NULL) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "can't get hyperslab selection")

                strcat(selection_string++, "[");
                for (i = 0; i < (size_t) ndims; i++) {
                    if (i > 0) strcat(selection_string++, ",");

                    if ((bytes_printed = sprintf(selection_string,
                                                 "%llu:%llu:%llu",
                                                 start[i],
                                                 start[i] + (stride[i] * count[i]),
                                                 stride[i]
                                         )) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_SYSERRSTR, FAIL, "sprintf error")

                    selection_string += bytes_printed;
                } /* end for */
                strcat(selection_string, "]");

                break;

            case H5S_SEL_ERROR:
            case H5S_SEL_N:
            default:
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid selection type")
        } /* end switch */
    } /* end if */
    else {
        /* Format the dataspace selection according to the 'start', 'stop' and 'step' keys
         * in a JSON request body. This looks like:
         *
         * "start": X, X, ...,
         * "stop": Y, Y, ...,
         * "step": Z, Z, ...
         *
         * This formatting is used when performing a write on a Dataset where the Datatype is
         * not a fixed-length datatype. In this case, binary data transfer cannot be used and
         * a JSON request has to be made.
         */
        switch (H5Sget_select_type(space_id)) {
            case H5S_SEL_ALL:
            case H5S_SEL_NONE:
                break;
            case H5S_SEL_POINTS:
                break;
            case H5S_SEL_HYPERSLABS:
            {
                char start_body[DATASET_WRITE_START_BODY_MAX_LENGTH] = "[", *start_body_curr_pos = start_body + 1;
                char stop_body[DATASET_WRITE_STOP_BODY_MAX_LENGTH] = "[", *stop_body_curr_pos = stop_body + 1;
                char step_body[DATASET_WRITE_STEP_BODY_MAX_LENGTH] = "[", *step_body_curr_pos = step_body + 1;

                if (H5Sget_regular_hyperslab(space_id, start, stride, count, NULL) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "can't get hyperslab selection")

                for (i = 0; i < (size_t) ndims; i++) {
                    if (i > 0) {
                        strcat(start_body_curr_pos++, ",");
                        strcat(stop_body_curr_pos++, ",");
                        strcat(step_body_curr_pos++, ",");
                    } /* end if */

                    if ((bytes_printed = sprintf(start_body_curr_pos, "%llu", start[i])) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_SYSERRSTR, FAIL, "sprintf error")
                    start_body_curr_pos += bytes_printed;

                    /* XXX: stop body may be wrong */
                    if ((bytes_printed = sprintf(stop_body_curr_pos, "%llu", start[i] + (stride[i] * count[i]))) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_SYSERRSTR, FAIL, "sprintf error")
                    stop_body_curr_pos += bytes_printed;

                    if ((bytes_printed = sprintf(step_body_curr_pos, "%llu", stride[i])) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_SYSERRSTR, FAIL, "sprintf error")
                    step_body_curr_pos += bytes_printed;
                } /* end for */

                strcat(start_body_curr_pos, "]");
                strcat(stop_body_curr_pos, "]");
                strcat(step_body_curr_pos, "]");

                snprintf(selection_string,
                        DIMENSION_ARRAY_MAX_LENGTH,
                        "\"start\": %s,"
                        "\"stop\": %s,"
                        "\"step\": %s",
                        start_body,
                        stop_body,
                        step_body);

                break;
            } /* H5S_SEL_HYPERSLABS */

            case H5S_SEL_ERROR:
            case H5S_SEL_N:
            default:
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid selection type")
        } /* end switch(H5Sget_select_type()) */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_convert_dataspace_selection_to_string() */


#if 0
static herr_t
H5VL_json_parse_dataset_create_options(void *parent_obj, const char *name, hid_t dcpl, char *create_request_body)
{
    H5VL_json_object_t *pobj = (H5VL_json_object_t *) parent_obj;
    hbool_t             shape_is_specified = false;
    hbool_t             maxdims_is_specified = false;
    hid_t               type_id, space_id, lcpl_id;
    char               *datatype_body = NULL;
    char                shape_body[DATASET_CREATE_SHAPE_BODY_MAX_LENGTH] = "";
    char                maxdims_body[DATASET_CREATE_MAXDIMS_BODY_MAX_LENGTH] = "";
    char               *creation_properties_body = NULL;
    char               *link_body = NULL;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(create_request_body);
    HDassert((H5I_FILE == pobj->obj_type || H5I_GROUP == pobj->obj_type)
                && "parent object not a file or group");

    /* Get the type ID */
    if (H5Pget(dcpl, H5VL_PROP_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for datatype ID")

    /* Get the space ID */
    if (H5Pget(dcpl, H5VL_PROP_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for dataspace ID")

    /* Get the Link Creation property list ID */
    if (H5Pget(dcpl, H5VL_PROP_DSET_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for link creation property list ID")

    /* Form the Datatype portion of the Dataset create request */
    if (H5VL_json_convert_datatype_to_string(type_id, &datatype_body, NULL, FALSE) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "unable to convert datatype to string representation")

    /* If the Dataspace of the Dataset was specified, parse it. Otherwise, use defaults */
    if (H5P_DEFAULT != space_id) {
        /* Form the Dataset shape portion of the Dataset create request */
        if (H5VL_json_parse_dataset_create_shape_and_maxdims(space_id, shape_body, maxdims_body) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "unable to parse Dataset shape parameters")

        /* Determine whether the Dataset shape and maximum size of each dimension are specified.
         * Note that max_dims may not be specified if the dataspace of the Dataset is specified
         * as H5S_NULL
         */
        shape_is_specified = strcmp(shape_body, "");
        maxdims_is_specified = strcmp(maxdims_body, "") && NULL == strstr(shape_body, "H5S_NULL");
    } /* end if */

    if (H5P_DATASET_CREATE_DEFAULT != dcpl) {
        if (NULL == (creation_properties_body = (char *) H5MM_malloc(DATASET_CREATE_PROPERTIES_BODY_MAX_LENGTH)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate space for dataset creation properties body")
        creation_properties_body[0] = '\0';

        /* Form the Dataset Creation Properties portion of the Dataset create request */
        if (H5VL_json_parse_dataset_creation_properties(dcpl, creation_properties_body) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "unable to parse Dataset Creation Properties")
    } /* end if */

    /* Only create a link for the Dataset if this isn't an H5Dcreate_anon call */
    if (name) {
        if (NULL == (link_body = (char *) H5MM_malloc(DATASET_CREATE_LINK_BODY_MAX_LENGTH)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate space for dataset link body")

        /* Form the Dataset Creation Link portion of the Dataset create request */
//        snprintf(link_body, DATASET_CREATE_LINK_BODY_MAX_LENGTH, "\"link\": {" "\"id\": \"%s\", " "\"name\": \"%s\"" "}", pobj->URI, get_basename(name));
    } /* end if */

    snprintf(create_request_body, REQUEST_BODY_MAX_LENGTH, "{ %s%s%s%s%s%s%s%s%s }",
             datatype_body,                                            /* Add the required Dataset Datatype description */
             shape_is_specified ? ", " : "",                           /* Add separator for Dataset shape section, if specified */
             shape_is_specified ? shape_body : "",                     /* Add the Dataset Shape description, if specified */
             maxdims_is_specified ? ", " : "",                         /* Add separator for Max Dims section, if specified */
             maxdims_is_specified ? maxdims_body : "",                 /* Add the Dataset Maximum Dimension Size section, if specified */
             creation_properties_body ? ", " : "",                     /* Add separator for Dataset Creation properties section, if specified */
             creation_properties_body ? creation_properties_body : "", /* Add the Dataset Creation properties section, if specified */
             link_body ? ", " : "",                                    /* Add separator for Link Creation section, if specified */
             link_body ? link_body : "");                              /* Add the Link Creation section, if specified */

done:
    if (link_body)
        H5MM_xfree(link_body);

    if (creation_properties_body)
        H5MM_xfree(creation_properties_body);

    if (datatype_body)
        H5MM_xfree(datatype_body);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_parse_dataset_create_options() */
#endif


#if 0
static herr_t
H5VL_json_parse_dataset_creation_properties(hid_t dcpl, char *creation_properties_body)
{
    H5D_alloc_time_t  alloc_time;
    H5D_fill_time_t   fill_time;
    unsigned          crt_order_flags;
    unsigned          max_compact, min_dense;
    char              alloc_time_body[DATASET_CREATE_ALLOC_TIME_BODY_MAX_LENGTH] = "";
    char              attribute_creation_order_body[DATASET_CREATE_CREATION_ORDER_BODY_MAX_LENGTH] = "";
    char              attribute_phase_change_body[DATASET_CREATE_ATTRIBUTE_PHASE_CHANGE_BODY_MAX_LENGTH] = "";
    char              fill_time_body[DATASET_CREATE_FILL_TIME_BODY_MAX_LENGTH] = "";
    char              fill_value_body[] = "";
    char             *filters_body = NULL;
    char              layout_body[DATASET_CREATE_LAYOUT_BODY_MAX_LENGTH] = "";
    char              track_times_body[DATASET_CREATE_TRACK_TIMES_BODY_MAX_LENGTH] = "";
    int               bytes_printed;
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(creation_properties_body);

    if (H5Pget_alloc_time(dcpl, &alloc_time) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve alloc time property")

    /* Note: At least one creation property needs to be guaranteed to be set so that each additional
     * property can be safely appended to the list with a leading comma to separate it from the
     * other properties. Without the guarantee of at least one set property, the result can be
     * a missing or hanging comma, depending on the combinations of set/unset properties,
     * which may result in server request errors. In this case, simply the Dataset space allocation
     * time property is chosen to always be set.
     */
    switch (alloc_time) {
        case H5D_ALLOC_TIME_DEFAULT:
            strcat(alloc_time_body, "\"allocTime\": \"H5D_ALLOC_TIME_DEFAULT\"");
            break;
        case H5D_ALLOC_TIME_EARLY:
            strcat(alloc_time_body, "\"allocTime\": \"H5D_ALLOC_TIME_EARLY\"");
            break;
        case H5D_ALLOC_TIME_INCR:
            strcat(alloc_time_body, "\"allocTime\": \"H5D_ALLOC_TIME_INCR\"");
            break;
        case H5D_ALLOC_TIME_LATE:
            strcat(alloc_time_body, "\"allocTime\": \"H5D_ALLOC_TIME_LATE\"");
            break;
        case H5D_ALLOC_TIME_ERROR:
        default:
            HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "invalid dataset space alloc time")
    } /* end switch */

    if (H5Pget_attr_creation_order(dcpl, &crt_order_flags) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve attribute creation order property")

    if (0 != crt_order_flags)
        snprintf(attribute_creation_order_body, DATASET_CREATE_CREATION_ORDER_BODY_MAX_LENGTH,
                 ", \"attributeCreationOrder\": \"H5P_CRT_ORDER_%s\"",
                 (H5P_CRT_ORDER_INDEXED | H5P_CRT_ORDER_TRACKED) == crt_order_flags ? "INDEXED" : "TRACKED");

    if (H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve attribute phase change property")

    if (DATASET_CREATE_MAX_COMPACT_ATTRIBUTES_DEFAULT != max_compact
            || DATASET_CREATE_MIN_DENSE_ATTRIBUTES_DEFAULT != min_dense)
        snprintf(attribute_phase_change_body, DATASET_CREATE_ATTRIBUTE_PHASE_CHANGE_BODY_MAX_LENGTH,
                 ", \"attributePhaseChange\": {"
                       "\"maxCompact\": %u, "
                       "\"minDense\": %u"
                   "}",
                 max_compact,
                 min_dense);

    if (H5Pget_fill_time(dcpl, &fill_time) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve fill time property")

    if (H5D_FILL_TIME_IFSET != fill_time)
        snprintf(fill_time_body, DATASET_CREATE_FILL_TIME_BODY_MAX_LENGTH,
                 ", \"fillTime\": \"H5D_FILL_TIME_%s\"",
                 H5D_FILL_TIME_ALLOC == fill_time ? "ALLOC" : "NEVER");

    /* Get the fill value property */
    {
        H5D_fill_value_t fill_status;

        if (H5Pfill_value_defined(dcpl, &fill_status) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve the fill value defined status")

        if (H5D_FILL_VALUE_DEFAULT != fill_status) {
            strcat(fill_value_body, ", \"fillValue\": ");

            if (H5D_FILL_VALUE_UNDEFINED == fill_status) {
                strcat(fill_value_body, "null");
            } /* end if */
            else {
                /* XXX: Support for fill values */
            } /* end else */
        } /* end if */
    }

    {
        int nfilters;

        if ((nfilters = H5Pget_nfilters(dcpl))) {
            unsigned  filter_config;
            unsigned  flags;
            unsigned  cd_values[FILTER_MAX_CD_VALUES];
            size_t    cd_nelmts = FILTER_MAX_CD_VALUES;
            size_t    filter_namelen = FILTER_NAME_MAX_LENGTH;
            size_t    i;
            char      filter_name[FILTER_NAME_MAX_LENGTH];
            char     *curr_pos;

            if (NULL == (filters_body = (char *) H5MM_malloc(DATASET_CREATE_FILTERS_BODY_MAX_LENGTH)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate space for dataset creation filters body")

            strcat(filters_body, "\"filters\": [ ");
            curr_pos = filters_body + 15;

            for (i = 0; i < (size_t) nfilters; i++) {
                if (i > 0) {
                    strcat(filters_body, ", ");
                    curr_pos += 2;
                } /* end if */

                switch (H5Pget_filter2(dcpl, (unsigned) i, &flags, &cd_nelmts, cd_values, filter_namelen, filter_name, &filter_config)) {
                    case H5Z_FILTER_DEFLATE:
                        if ((bytes_printed = snprintf(curr_pos, DATASET_CREATE_FILTER_ENTRY_MAX_LENGTH,
                                                      "{"
                                                          "\"class\": \"H5Z_FILTER_DEFLATE\", "
                                                          "\"id\": %d, "
                                                          "\"level\": %d"
                                                      "}",
                                                      H5Z_FILTER_DEFLATE,
                                                      cd_values[0]
                                             )) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_SYSERRSTR, FAIL, "snprintf error")

                        curr_pos += bytes_printed;
                        break;

                    case H5Z_FILTER_SHUFFLE:
                        if ((bytes_printed = snprintf(curr_pos, DATASET_CREATE_FILTER_ENTRY_MAX_LENGTH,
                                                      "{"
                                                          "\"class\": \"H5Z_FILTER_SHUFFLE\", "
                                                          "\"id\": %d"
                                                      "}",
                                                      H5Z_FILTER_SHUFFLE
                                             )) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_SYSERRSTR, FAIL, "snprintf error")

                        curr_pos += bytes_printed;
                        break;

                    case H5Z_FILTER_FLETCHER32:
                        if ((bytes_printed = snprintf(curr_pos, DATASET_CREATE_FILTER_ENTRY_MAX_LENGTH,
                                                      "{"
                                                          "\"class\": \"H5Z_FILTER_FLETCHER32\", "
                                                          "\"id\": %d"
                                                      "}",
                                                      H5Z_FILTER_FLETCHER32
                                             )) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_SYSERRSTR, FAIL, "snprintf error")

                        curr_pos += bytes_printed;
                        break;

                    case H5Z_FILTER_SZIP:
                        /* XXX: SZIP filter shouldn't default to NN_OPTION_MASK when unsupported mask types are specified */
                        if ((bytes_printed = snprintf(curr_pos, DATASET_CREATE_FILTER_ENTRY_MAX_LENGTH,
                                                      "{"
                                                          "\"class\": \"H5Z_FILTER_SZIP\", "
                                                          "\"id\": %d, "
                                                          "\"bitsPerPixel\": %u, "
                                                          "\"coding\": \"%s\", "
                                                          "\"pixelsPerBlock\": %u, "
                                                          "\"pixelsPerScanline\": %u"
                                                      "}",
                                                      H5Z_FILTER_SZIP,
                                                      cd_values[H5Z_SZIP_PARM_BPP],
                                                      cd_values[H5Z_SZIP_PARM_MASK] == H5_SZIP_EC_OPTION_MASK ?
                                                              "H5_SZIP_EC_OPTION_MASK" : "H5_SZIP_NN_OPTION_MASK",
                                                      cd_values[H5Z_SZIP_PARM_PPB],
                                                      cd_values[H5Z_SZIP_PARM_PPS]
                                             )) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_SYSERRSTR, FAIL, "snprintf error")

                        curr_pos += bytes_printed;
                        break;

                    case H5Z_FILTER_NBIT:
                        if ((bytes_printed = snprintf(curr_pos, DATASET_CREATE_FILTER_ENTRY_MAX_LENGTH,
                                                      "{"
                                                          "\"class\": \"H5Z_FILTER_NBIT\", "
                                                          "\"id\": %d"
                                                      "}",
                                                      H5Z_FILTER_NBIT
                                             )) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_SYSERRSTR, FAIL, "snprintf error")

                        curr_pos += bytes_printed;
                        break;

                    case H5Z_FILTER_SCALEOFFSET:
                        if ((bytes_printed = snprintf(curr_pos, DATASET_CREATE_FILTER_ENTRY_MAX_LENGTH,
                                                      "{"
                                                          "\"class\": \"H5Z_FILTER_SCALEOFFSET\", "
                                                          "\"id\": %d, "
                                                          "\"scaleType\": \"%s\", "
                                                          "\"scaleOffset\": %u"
                                                      "}",
                                                      H5Z_FILTER_SCALEOFFSET,
                                                      "", /* XXX: */
                                                      cd_values[1]
                                             )) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_SYSERRSTR, FAIL, "snprintf error")

                        curr_pos += bytes_printed;
                        break;

                    case LZF_FILTER_ID: /* LZF Filter */
                        if ((bytes_printed = snprintf(curr_pos, DATASET_CREATE_FILTER_ENTRY_MAX_LENGTH,
                                                      "{"
                                                          "\"class\": \"H5Z_FILTER_LZF\", "
                                                          "\"id\": %d"
                                                      "}",
                                                      LZF_FILTER_ID
                                             )) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_SYSERRSTR, FAIL, "snprintf error")

                        curr_pos += bytes_printed;
                        break;

                    default: /* User-defined filter */
                        /* XXX: Implement ID and parameter retrieval */
                        if ((bytes_printed = snprintf(curr_pos, DATASET_CREATE_FILTER_ENTRY_MAX_LENGTH,
                                                      "{"
                                                          "\"class\": \"H5Z_FILTER_USER\", "
                                                          "\"id\": %d, "
                                                          "\"parameters\": %s"
                                                      "}",
                                                      0,
                                                      ""
                                             )) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_SYSERRSTR, FAIL, "snprintf error")

                        curr_pos += bytes_printed;

                        break;

                    case H5Z_FILTER_ERROR:
                        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "invalid filter specified")
                } /* end switch */
            } /* end for */

            strcat(filters_body, " ]");
        } /* end if */
    }

    switch (H5Pget_layout(dcpl)) {
        case H5D_COMPACT:
            strcat(layout_body, ", \"layout\": { \"class\": \"H5D_COMPACT\" }");
            break;
        case H5D_CONTIGUOUS:
            /* XXX: Add support for external storage */
            strcat(layout_body, ", \"layout\": { \"class\": \"H5D_CONTIGUOUS\" }");
            break;
        case H5D_CHUNKED:
        {
            hsize_t chunk_dims[H5O_LAYOUT_NDIMS];
            size_t  i;
            char    dims_array[DIMENSION_ARRAY_MAX_LENGTH] = "";
            char   *curr_pos;
            int     ndims;

            if ((ndims = H5Pget_chunk(dcpl, H5O_LAYOUT_NDIMS, chunk_dims)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve dataset chunk dimensionality")

            strcat(dims_array, "[ ");
            curr_pos = dims_array + 2;

            for (i = 0; i < (size_t) ndims; i++) {
                if (i > 0) {
                    strcat(dims_array, ", ");
                    curr_pos += 2;
                } /* end if */

                if ((bytes_printed = snprintf(layout_body, DATASET_CREATE_LAYOUT_BODY_MAX_LENGTH, "%llu", chunk_dims[i])) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_SYSERRSTR, FAIL, "snprintf error")

                curr_pos += bytes_printed;
            } /* end for */
            strcat(dims_array, " ]");

            snprintf(layout_body, DATASET_CREATE_LAYOUT_BODY_MAX_LENGTH,
                     ", \"layout\": { \"class\": \"H5D_CHUNKED\", \"dims\": %s }", dims_array);

            break;
        } /* H5D_CHUNKED */

        case H5D_VIRTUAL:
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unsupported dataset layout: Virtual")

        case H5D_LAYOUT_ERROR:
        case H5D_NLAYOUTS:
        default:
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve dataset layout property")
    } /* end switch */

    {
        hbool_t track_times;

        if (H5Pget_obj_track_times(dcpl, &track_times) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to retrieve object time tracking property")

        if (track_times)
            strcat(track_times_body, ", \"trackTimes\": \"true\"");
        else
            strcat(track_times_body, ", \"trackTimes\": \"false\"");
    }

    snprintf(creation_properties_body, DATASET_CREATE_PROPERTIES_BODY_MAX_LENGTH,
             "\"creation_properties\": {"
                 "%s"        /* Dataset space allocation time property */
                 "%s"        /* Dataset attribute creation order property */
                 "%s"        /* Dataset attribute phase change property */
                 "%s"        /* Dataset fill time property */
                 "%s"        /* Dataset fill value property */
                 "%s"        /* Dataset filters property */
                 "%s"        /* Dataset layout property */
                 "%s"        /* Dataset track times property */
             "}",
             alloc_time_body,
             attribute_creation_order_body,
             attribute_phase_change_body,
             fill_time_body,
             fill_value_body,
             filters_body ? filters_body : "",
             layout_body,
             track_times_body);

done:
    if (filters_body)
        H5MM_xfree(filters_body);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_json_parse_dataset_creation_properties() */
#endif

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

// OUT: array containing data from buffer

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


/* H5VL_json_find_object_by_name()
 * Helper function to traverse links and find the group of interest 
 *
 * IN:      - domain
 *          - name / name of object to be located
 * IN-OUT:  - current_uuid / uuid of group to begin search / may be modified
 *          - collection / pointer to json_t* which upon return contains a jansson string,
 *            the value of which is "datasets", "datatypes", or "groups".
 *            If a NULL value is supplied, no action
 *          - containing_group_uuid / if a valid pointer is supplied, the uuid for the 
 *            group containing the object of interest will be returned.
 * returns:   pointer to the jansson object 
 *
*/

static 
json_t* H5VL_json_find_object_by_name(H5VL_json_object_t* domain, const char* name, h5json_uuid_t *current_uuid, json_t** collection, h5json_uuid_t *containing_group_uuid)
{
    json_t*            ret_value = NULL;
    json_t*            current_group = NULL;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef PLUGIN_DEBUG
    printf("H5VL_json_find_object_by_name():      \n");
    printf("  - Locating object:                %s\n", name);
    printf("  - Starting from group with uuid:  %s\n", current_uuid);
//    printf("  - File currently reads:\n\n");
//    json_dumpf(domain->u.file.json_file_object, stdout, JSON_INDENT(4));
//    printf("  - domain->u.file.json_file_object %s\n", json_dumps(domain->u.file.json_file_object, JSON_INDENT(4)));
#endif

    json_t* groups_in_file = json_object_get(domain->u.file.json_file_object, "groups");
//    printf("  - groups_in_file:                 %s\n", json_dumps(groups_in_file, JSON_INDENT(4)));

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
printf("current group uuid = *%s*\n", current_uuid);
//FTW WIP find the link in the group and remove it?? why am I in this routine?

        /* search links of current group for current token */
        char *current_token = tokens[token_index];
        json_t* links_for_current_group = json_object_get(current_group, "links");
        hbool_t found = FALSE;
        size_t index;
        json_t* link;
        json_array_foreach(links_for_current_group, index, link) 
        {
            /* block of code that uses index and link */
//json_object_set_new(link, "ftw", json_string("woowee!"));            
//json_t* ftw = json_object_get(link, "ftw");
printf("iterating links: link = *%s*\n", json_dumps(link, JSON_INDENT(4)));
//printf("ftw = *%s*\n", json_dumps(ftw, JSON_INDENT(4)));
//FTW this is giving null. WTF?

//printf("link = *%s*\n", json_dumps(link, JSON_INDENT(4)));
            json_t* title = json_object_get(link, "title");
printf("string value of title = *%s*\n", json_string_value(title));
// null avove is causing this to fail.
            if (!strcmp(json_string_value(title), current_token))
            {
printf("Found link of interest, token = %s\n", current_token);
                found = TRUE; /* found it. */
                break; /* break out of enclosing json_array_foreach() macro */
            }
        } /* end of loop over link array */

json_t* title = json_object_get(link, "title");
printf("After link loop, left with string value of title = *%s*\n", json_string_value(title));
// got this far

        /* after searching all links, these are the scenarios: */

        /* 1 - last token is found --> link exists so return */
        if((token_index == n_tokens - 1) && found) 
        {
printf("last token %s found.\n", tokens[token_index]);
            if (containing_group_uuid != NULL)
            {
printf("here 1 %s\n", json_string_value(containing_group_uuid_json));
if (containing_group_uuid_json == NULL) // this means it was found on first pass
{
    strncpy(containing_group_uuid, current_uuid, sizeof(h5json_uuid_t));
    printf("copying current_uuid to containing: %s\n", containing_group_uuid);
}
else
                strncpy(containing_group_uuid, json_string_value(containing_group_uuid_json), sizeof(h5json_uuid_t));
printf("here 2\n");
            }

            if (collection != NULL) 
            {
printf("here 3\n");
                *collection = json_object_get(link, "collection");
printf("here 4\n");
            }

            ret_value = link;
            break; /* break out of loop over tokens */
        }

        /* 2 - intermediate token found, so move on to next token */
        if ((token_index < (n_tokens - 1)) && found)
        {
printf("intermediate token %s found.\n", tokens[token_index]);
            containing_group_uuid_json = json_object_get(link, "id");
//            current_uuid = (h5json_uuid_t*)json_string_value(json_object_get(link, "id"));
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

static herr_t
H5VL_json_delete_link_from_containing_group(H5VL_json_object_t* domain, h5json_uuid_t containing_group_uuid, json_t* link)
{
    herr_t ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

printf("Deleting link %s from group %s\n", json_dumps(link, JSON_INDENT(4)), containing_group_uuid);
//printf("<<<not yet implemented>>>\n");

    json_t* groups_in_file = json_object_get(domain->object_json, "groups");
    json_t* group_of_interest = json_object_get(groups_in_file, containing_group_uuid);
    json_t* links_in_group = json_object_get(group_of_interest, "links");

printf("links_in_group originally reads *%s*\n", json_dumps(links_in_group, JSON_INDENT(4)));

    /* array is a JSON array */
    size_t index;
    json_t *value;

    json_array_foreach(links_in_group, index, value) 
    {
        /* block of code that uses index and value */
        if (json_equal(value, link)) 
        {
printf("breaking out of loop at index = %ld, value = %s\n", index, json_dumps(value, JSON_INDENT(4)));
            break;    
        }
    }

    json_array_remove(links_in_group, index);

printf("links_in_group now reads *%s*\n", json_dumps(links_in_group, JSON_INDENT(4)));

    ret_value = SUCCEED;

done:

    FUNC_LEAVE_NOAPI(ret_value)

}

