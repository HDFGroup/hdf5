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
#include "H5Aprivate.h"         /* Attributes                           */
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

/* MDS specific Dataset tansfer private properties */
#define H5VL_DSET_MDS_ID        "mds_dataset_id"

#define H5VL_MDS_LINK_ITERATE  2
#define H5VL_MDS_OBJECT_VISIT  2
#define H5VL_MDS_CHUNK_ITERATE 2

/* forward declaration of file struct */
struct H5VL_mds_file_t;

/* the object structure of the MDS VOL plugin */
typedef struct H5VL_mds_object_t {
    H5I_type_t obj_type;
    hid_t obj_id; /* the ID of the object that is created at the MDS side */
    H5F_t *raw_file; /* the raw data file that is created by the client */
    struct H5VL_mds_file_t *file; /* the file struct that this object belongs to */
} H5VL_mds_object_t;

typedef struct H5VL_mds_file_t {
    H5VL_mds_object_t common; /* common stuff, must be first */
    char *name; /* name of the container */
} H5VL_mds_file_t;

typedef struct H5VL_mds_attr_t {
    H5VL_mds_object_t common;  /* common stuff, must be first  */
    H5A_t *attr; /* the lightweight attribute struct created by the client */
} H5VL_mds_attr_t;

typedef struct H5VL_mds_dset_t {
    H5VL_mds_object_t common;  /* common stuff, must be first  */
    H5D_t *dset; /* the lightweight dataset struct created by the client */
} H5VL_mds_dset_t;

typedef struct H5VL_mds_dtype_t {
    H5VL_mds_object_t common; /* common stuff, must be first  */
    H5T_t *dtype; /* the uncommitted datatype struct for the client */
}H5VL_mds_dtype_t;

typedef struct H5VL_mds_group_t {
    H5VL_mds_object_t common;  /* common stuff, must be first  */
} H5VL_mds_group_t;

H5_DLL herr_t H5VL_mds_start(void);
H5_DLL hid_t H5VL_mds_register(H5I_type_t type, void *obj, hbool_t app_ref);

#endif /* H5_HAVE_PARALLEL */

#endif /* _H5VLmdserver_H */
