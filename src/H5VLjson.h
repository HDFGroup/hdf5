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
 *              October, 2017
 *
 * Purpose: The private header file for the JSON VOL plugin.
 */

#ifndef H5VLjson_H
#define H5VLjson_H

#include <libgen.h>
#include <jansson.h>
#include <curl/curl.h>
#include <yajl/yajl_tree.h>

#include "H5VLjson_public.h"

#define HDF5_VOL_JSON_VERSION 1        /* Version number of the JSON VOL plugin */
#define HDF5_JSON_API_VERSION "1.0.0"  /* API Version for HDF5-JSON production rules */

#ifdef __cplusplus
extern "C" {
#endif

/* XXX: Eliminate as many "MAX_LENGTH" defines as possible and turn them into dynamic allocations */
#define DATASET_WRITE_BODY_MAX_LENGTH 47185920 /* Default maximum dataset write value body size is 45MB */
#define REQUEST_BODY_MAX_LENGTH       52428800 /* Default maximum request size is 50MB */
#define REQUEST_BODY_DEFAULT_LENGTH   2048 /* Default size of the body of a request to the server */

#define HOST_HEADER_MAX_LENGTH    2048
#define URL_MAX_LENGTH            2048
#define URI_MAX_LENGTH            2048
#define ATTR_NAME_MAX_LENGTH      2048
#define GROUP_BASENAME_MAX_LENGTH 2048

#define DATASPACE_MAX_RANK 32

/* Maximum length of a large unsigned value used in places such as specifying
 * the size of each dimension of a Dataset */
#define MAX_NUM_LENGTH 20

/* Defines for Dataset operations */
#define DATASET_CREATE_ATTRIBUTE_PHASE_CHANGE_BODY_MAX_LENGTH 64
#define DATASET_CREATE_STRING_LENGTH_SPECIFIER_MAX_LENGTH     128
#define DATASET_CREATE_CREATION_ORDER_BODY_MAX_LENGTH         60
#define DATASET_CREATE_MAX_COMPACT_ATTRIBUTES_DEFAULT         8
#define DATASET_CREATE_MIN_DENSE_ATTRIBUTES_DEFAULT           6
#define DATASET_CREATE_TRACK_TIMES_BODY_MAX_LENGTH            32
#define DATASET_CREATE_PROPERTIES_BODY_MAX_LENGTH             1024
#define DATASET_CREATE_ALLOC_TIME_BODY_MAX_LENGTH             45
#define DATASET_CREATE_FILL_TIME_BODY_MAX_LENGTH              45
#define DATASET_CREATE_DATATYPE_BODY_MAX_LENGTH               ENUM_MAPPING_ENTRY_MAX_LENGTH * ENUM_MAPPING_MAX_ENTRIES + 256
#define DATASET_CREATE_ENUM_MAPPING_MAX_LENGTH                ENUM_MAPPING_MAX_ENTRIES * ENUM_MAPPING_ENTRY_MAX_LENGTH
#define DATASET_CREATE_MAXDIMS_BODY_MAX_LENGTH                1024
#define DATASET_CREATE_LAYOUT_BODY_MAX_LENGTH                 256
#define DATASET_CREATE_SHAPE_BODY_MAX_LENGTH                  1024
#define DATASET_CREATE_LINK_BODY_MAX_LENGTH                   1024
#define ENUM_MAPPING_ENTRY_VALUE_MAX_LENGTH                   MAX_NUM_LENGTH
#define ENUM_MAPPING_ENTRY_NAME_MAX_LENGTH                    256
#define ENUM_MAPPING_ENTRY_MAX_LENGTH                         ENUM_MAPPING_ENTRY_NAME_MAX_LENGTH + ENUM_MAPPING_ENTRY_VALUE_MAX_LENGTH + 10
#define DIMENSION_ARRAY_MAX_LENGTH                            (DATASPACE_MAX_RANK * MAX_NUM_LENGTH) + (2 * (DATASPACE_MAX_RANK - 1)) + 4
#define ENUM_MAPPING_MAX_ENTRIES                              1024

/* Defines for Attribute operations */
#define ATTRIBUTE_CREATE_ATTRIBUTE_PHASE_CHANGE_BODY_MAX_LENGTH 64
#define ATTRIBUTE_CREATE_STRING_LENGTH_SPECIFIER_MAX_LENGTH     128
#define ATTRIBUTE_CREATE_CREATION_ORDER_BODY_MAX_LENGTH         60
#define ATTRIBUTE_CREATE_MAX_COMPACT_ATTRIBUTES_DEFAULT         8
#define ATTRIBUTE_CREATE_MIN_DENSE_ATTRIBUTES_DEFAULT           6
#define ATTRIBUTE_CREATE_TRACK_TIMES_BODY_MAX_LENGTH            32
#define ATTRIBUTE_CREATE_PROPERTIES_BODY_MAX_LENGTH             1024
#define ATTRIBUTE_CREATE_ALLOC_TIME_BODY_MAX_LENGTH             45
#define ATTRIBUTE_CREATE_FILL_TIME_BODY_MAX_LENGTH              45
#define ATTRIBUTE_CREATE_DATATYPE_BODY_MAX_LENGTH               ENUM_MAPPING_ENTRY_MAX_LENGTH * ENUM_MAPPING_MAX_ENTRIES + 256
#define ATTRIBUTE_CREATE_ENUM_MAPPING_MAX_LENGTH                ENUM_MAPPING_MAX_ENTRIES * ENUM_MAPPING_ENTRY_MAX_LENGTH
#define ATTRIBUTE_CREATE_MAXDIMS_BODY_MAX_LENGTH                1024
#define ATTRIBUTE_CREATE_LAYOUT_BODY_MAX_LENGTH                 256
#define ATTRIBUTE_CREATE_SHAPE_BODY_MAX_LENGTH                  1024
#define ATTRIBUTE_CREATE_VALUE_BODY_MAX_LENGTH                  65536
#define ATTRIBUTE_CREATE_LINK_BODY_MAX_LENGTH                   1024

/* Defines for Datatype operations */
#define DATATYPE_CREATE_LINK_BODY_MAX_LENGTH 1024
#define DATATYPE_BODY_DEFAULT_SIZE           512
#define ENUM_MAPPING_DEFAULT_SIZE            4096

/* Defines for Hyperslab selections when writing/reading data */
#define DATASET_WRITE_START_BODY_MAX_LENGTH DIMENSION_ARRAY_MAX_LENGTH
#define DATASET_WRITE_STOP_BODY_MAX_LENGTH  DATASET_WRITE_START_BODY_MAX_LENGTH
#define DATASET_WRITE_STEP_BODY_MAX_LENGTH  DATASET_WRITE_STOP_BODY_MAX_LENGTH

/* Defines for the user of filters */
#define DATASET_CREATE_FILTERS_BODY_MAX_LENGTH DATASET_CREATE_FILTER_ENTRY_MAX_LENGTH * DATASET_CREATE_MAX_FILTERS
#define DATASET_CREATE_FILTER_ENTRY_MAX_LENGTH 256
#define DATASET_CREATE_MAX_FILTERS             32
#define FILTER_NAME_MAX_LENGTH                 256
#define FILTER_MAX_CD_VALUES                   32
#define LZF_FILTER_ID                          32000 /* The HDF5 Library could potentially add 'H5Z_FILTER_LZF' in the future */

// FTW replace this with json_t* that wraps it. Easier to manipulate/compare.
typedef char h5json_uuid_t[37];

//FTW this can be wrapped into struct def below
typedef struct H5VL_json_object_t H5VL_json_object_t;
// FTW : This can go away once we eliminate attribute as an object type 
typedef struct H5VL_json_attr_t H5VL_json_attr_t;

typedef struct H5VL_json_file_t {
    unsigned  intent;   
//    char*     filepath_name;
    json_t*   json_file_object;         /* represents in-memory structure */
    FILE*     filesystem_file_object;   /* holds object place on filesystem */
//    json_t*   root_group_uuid;        /* do we need this? */
} H5VL_json_file_t;

typedef struct H5VL_json_datatype_t {
    hid_t dtype_id;
    hid_t tcpl_id;
} H5VL_json_datatype_t;

typedef struct H5VL_json_group_t {
//FTW    char               basename[GROUP_BASENAME_MAX_LENGTH];
} H5VL_json_group_t;

typedef struct H5VL_json_dataset_t {
    hid_t space_id;
    hid_t dtype_id;
} H5VL_json_dataset_t;

typedef struct H5VL_json_attr_t {
    H5VL_json_object_t *parent_obj;
    hid_t               space_id;
    hid_t               dtype_id;
    char                attr_name[ATTR_NAME_MAX_LENGTH];
} H5VL_json_attr_t;

typedef struct object_union_t {
        H5VL_json_datatype_t datatype;
        H5VL_json_dataset_t  dataset;
        H5VL_json_group_t    group;
        H5VL_json_attr_t     attribute;
        H5VL_json_file_t     file;
} object_union_t;

typedef struct H5VL_json_object_t {
    H5VL_json_object_t *domain; /* containing_file */ 
    H5I_type_t          obj_type;
//    json_t*             object_uuid; /* identify the object within type lists in file */
    h5json_uuid_t       object_uuid; /* identify the object within type lists in file */
    json_t*             object_json; /* a pointer into the object within the file object */
//    char                URI[URI_MAX_LENGTH]; //FTW: URI will go away, functionally replaced by object_uuid
    object_union_t      u;
} H5VL_json_object_t;

#ifdef __cplusplus
}
#endif

#endif /* H5VLjson_H */
