/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef _H5VLprivate_H
#define _H5VLprivate_H

/* Include package's public header */
#include "H5VLpublic.h"         /* Generic Functions                    */

/* Private headers needed by this file */

/**************************/
/* Library Private Macros */
/**************************/

#define H5_REQUEST_NULL         NULL
#define H5_EVENT_STACK_NULL     ((hid_t)-1)

/****************************/
/* Library Private Typedefs */
/****************************/

/* Internal struct to track VOL driver information for objects */
typedef struct H5VL_t {
    const H5VL_class_t *vol_cls;        /* constant driver class info                           */
    int                 nrefs;          /* number of references by objects using this struct    */
    hid_t               vol_id;         /* identifier for the VOL class                         */
} H5VL_t;

/* Internal vol object structure returned to the API */
typedef struct H5VL_object_t {
    void               *vol_obj;        /* pointer to object created by driver                  */
    H5VL_t             *vol_info;       /* pointer to VOL info struct                           */
} H5VL_object_t;

/* Internal structure to hold the driver ID & info for FAPLs */
typedef struct H5VL_driver_prop_t {
    hid_t               driver_id;      /* VOL driver's ID                                      */
    const void         *driver_info;    /* VOL driver info, for open callbacks                  */
} H5VL_driver_prop_t;

/*****************************/
/* Library Private Variables */
/*****************************/

/******************************/
/* Library Private Prototypes */
/******************************/

/* XXX: These names should all be verbs */
H5_DLL herr_t H5VL_init(void);
H5_DLL hid_t H5VL_register_id(H5I_type_t type, void *object, H5VL_t *vol_driver, hbool_t app_ref);
H5_DLL herr_t H5VL_free_object(H5VL_object_t *obj);
H5_DLL hid_t H5VL_register(const void *cls, size_t size, hbool_t app_ref);
H5_DLL hid_t H5VL_object_register(void *obj, H5I_type_t obj_type, hid_t driver_id, hbool_t app_ref);
H5_DLL ssize_t H5VL_get_driver_name(hid_t id, char *name/*out*/, size_t size);
H5_DLL H5VL_object_t *H5VL_get_object(hid_t id);
H5_DLL void *H5VL_object(hid_t id);
H5_DLL void *H5VL_object_verify(hid_t id, H5I_type_t obj_type);
H5_DLL void *H5VL_driver_object(H5VL_object_t *obj);

H5_DLL herr_t H5VL_datatype_get(void *dt, const H5VL_class_t *vol_cls, H5VL_datatype_get_t get_type, hid_t dxpl_id, void **req, ...);

H5_DLL herr_t H5VL_file_close(void *file, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req);

/* XXX: Try to put these in the appropriate package private files */
H5_DLL herr_t H5F_close_file(void *file);

/* Asynchronous functions */
H5_DLL herr_t H5VL_request_cancel(void **req, const H5VL_class_t *vol_cls, H5ES_status_t *status);
H5_DLL herr_t H5VL_request_test(void **req, const H5VL_class_t *vol_cls, H5ES_status_t *status);
H5_DLL herr_t H5VL_request_wait(void **req, const H5VL_class_t *vol_cls, H5ES_status_t *status);

/* XXX: These belong in a private native driver header, not here... */
H5_DLL hid_t H5VL_native_register(H5I_type_t type, void *obj, hbool_t app_ref);
H5_DLL herr_t H5VL_native_unregister(hid_t obj_id);
#endif /* _H5VLprivate_H */

