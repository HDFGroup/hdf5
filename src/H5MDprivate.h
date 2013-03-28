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
#ifndef _H5MDprivate_H
#define _H5MDprivate_H

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

#define MDS_RANK          0
#define H5MD_LISTEN_TAG   352
#define H5MD_RETURN_TAG   356
#define H5MD_CONT         2
/* MDS specific Dataset tansfer private properties */
#define H5MD_DSET_ID      "mds_dataset_id"

/* forward declaration of file struct */
struct H5MD_file_t;

/* the object structure of the MDS VOL plugin */
typedef struct H5MD_object_t {
    H5I_type_t obj_type;
    hid_t obj_id; /* the ID of the object that is created at the MDS side */
    H5F_t *raw_file; /* the raw data file that is created by the client */
    struct H5MD_file_t *file; /* the file struct that this object belongs to */
} H5MD_object_t;

typedef struct H5MD_file_t {
    H5MD_object_t common; /* common stuff, must be first */
    char *name; /* name of the container */
} H5MD_file_t;

typedef struct H5MD_attr_t {
    H5MD_object_t common;  /* common stuff, must be first  */
    H5A_t *attr; /* the lightweight attribute struct created by the client */
} H5MD_attr_t;

typedef struct H5MD_dset_t {
    H5MD_object_t common;  /* common stuff, must be first  */
    H5D_t *dset; /* the lightweight dataset struct created by the client */
} H5MD_dset_t;

typedef struct H5MD_dtype_t {
    H5MD_object_t common; /* common stuff, must be first  */
    H5T_t *dtype; /* the uncommitted datatype struct for the client */
}H5MD_dtype_t;

typedef struct H5MD_group_t {
    H5MD_object_t common;  /* common stuff, must be first  */
} H5MD_group_t;

H5_DLL herr_t H5MD_start(void);
H5_DLL int H5MD_server_terminate_cb(MPI_Comm UNUSED comm, int UNUSED comm_keyval, void UNUSED *attribute_val, void UNUSED *extra_state);

/* all the lightweigh, MDS plugin specific routines */
H5_DLL H5A_t *H5MD_attr_create(const char *name, H5T_t *type, H5S_t *space, hid_t acpl_id);
H5_DLL herr_t H5MD_attr_close(H5A_t *attr);
H5_DLL H5D_t *H5MD_dset_create(H5F_t *file, hid_t type_id, hid_t space_id, 
                               hid_t dcpl_id, hid_t dapl_id);
H5_DLL herr_t H5MD_dset_read(H5D_t *dataset, hid_t mem_type_id, const H5S_t *mem_space,
                             const H5S_t *file_space, hid_t dxpl_id, void *buf/*out*/);
H5_DLL herr_t H5MD_dset_write(H5D_t *dataset, hid_t mem_type_id, const H5S_t *mem_space,
                             const H5S_t *file_space, hid_t dxpl_id, const void *buf);

H5_DLL herr_t H5MD_dset_close(H5D_t *dataset);
H5_DLL H5F_t *H5MD_file_open(const char *name, unsigned flags, hid_t fcpl_id,
                           hid_t fapl_id, hid_t dxpl_id);
H5_DLL herr_t H5MD_file_get_obj_count(const H5F_t *f, unsigned types, hbool_t app_ref, size_t *obj_id_count_ptr);
H5_DLL herr_t H5MD_file_get_obj_ids(const H5F_t *f, unsigned types, size_t max_objs, hid_t *oid_list, hbool_t app_ref, size_t *obj_id_count_ptr);
H5_DLL herr_t H5MD_file_get_objects(const H5F_t *f, unsigned types, size_t max_index, hid_t *obj_id_list, hbool_t app_ref, size_t *obj_id_count_ptr);
H5_DLL int H5MD_file_get_objects_cb(void *obj_ptr, hid_t obj_id, void *key);

#endif /* H5_HAVE_PARALLEL */

#endif /* _H5MDprivate_H */
