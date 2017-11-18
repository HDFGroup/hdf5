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

H5_DLL herr_t H5VL_init(void);
H5_DLL hid_t  H5VL_register(const void *cls, size_t size, hbool_t app_ref);
H5_DLL herr_t H5VL_free_object(H5VL_object_t *obj);

H5_DLL herr_t H5VL_file_close(void *file, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req);

/* XXX: Try to put this in H5Fprivate.h */
H5_DLL herr_t H5F_close_file(void *file);

#endif /* _H5VLprivate_H */

