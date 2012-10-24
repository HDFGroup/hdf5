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
 *              August, 2012
 */
#ifndef _H5VLmdserver_H
#define _H5VLmdserver_H

#include "H5private.h"          /* Generic Functions                    */
#include "H5Dprivate.h"         /* Datasets                             */
#include "H5Fprivate.h"         /* File access                          */
#include "H5Iprivate.h"         /* IDs                                  */
#include "H5Oprivate.h"         /* Objects                              */
#include "H5Sprivate.h"         /* Dataspace                            */
#include "H5Tprivate.h"         /* Datatypes                            */
#include "H5VLprivate.h"        /* VOL plugins                          */

#ifdef H5_HAVE_PARALLEL

#define MDS_RANK            0
#define H5VL_MDS_LISTEN_TAG 352
#define H5VL_MDS_SEND_TAG   356

/* the object structure of the MDS VOL plugin */
typedef struct H5VL_mds_object_t {
    H5I_type_t obj_type;
    hid_t      obj_id; /* the ID of the object that is created at the MDS side */
    H5F_t      *raw_file; /* the raw data file that this object belongs to */
    /*
    union {
        struct H5VL_mds_file_t file;
        struct H5VL_mds_dset_t dset;
    }u;
    */
} H5VL_mds_object_t;

typedef struct H5VL_mds_file_t {
    H5VL_mds_object_t common; /* common stuff, must be first */
    H5F_t  *raw_file; /* the raw data file that is created by the client */
} H5VL_mds_file_t;

typedef struct H5VL_mds_dset_t {
    H5VL_mds_object_t common;  /* common stuff, must be first  */
    H5D_t *dset; /* the lightweight dataset struct created by the client */
} H5VL_mds_dset_t;

typedef struct H5VL_mds_dtype_t {
    H5VL_mds_object_t common; /* common stuff, must be first  */
    H5T_t *dtype; /* the uncommitted datatype struct for the client */
}H5VL_mds_dtype_t;

#define NUM_MDS_OPS 15

/* Operation types for the MDS */
typedef enum H5VL_mds_op_type_t {
    H5VL_MDS_FILE_CREATE,
    H5VL_MDS_FILE_OPEN,
    H5VL_MDS_FILE_FLUSH,
    H5VL_MDS_FILE_CLOSE,
    H5VL_MDS_DSET_CREATE,
    H5VL_MDS_DSET_OPEN,
    H5VL_MDS_DSET_READ,
    H5VL_MDS_DSET_WRITE,
    H5VL_MDS_DSET_CLOSE,
    H5VL_MDS_DTYPE_COMMIT,
    H5VL_MDS_DTYPE_OPEN,
    H5VL_MDS_DTYPE_CLOSE,
    H5VL_MDS_ALLOC,
    H5VL_MDS_GET_EOA,
    H5VL_MDS_SET_EOA
} H5VL_mds_op_type_t;

H5_DLL herr_t H5VL_mds_start(void);
H5_DLL herr_t H5VL_mds_encode(H5VL_mds_op_type_t request_type, void *buf, size_t *size, ...);
H5_DLL hid_t H5VL_mds_register(H5I_type_t type, void *obj, hbool_t app_ref);

#endif /* H5_HAVE_PARALLEL */

#endif /* _H5VLmdserver_H */
