/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * This file contains function prototypes for each exported function in the
 * H5FF module.
 */
#ifndef _H5FFpublic_H
#define _H5FFpublic_H

/* System headers needed by this file */

/* Public headers needed by this file */
#include "H5VLpublic.h" 	/* Public VOL header file		*/

/*****************/
/* Public Macros */
/*****************/

#ifdef __cplusplus
extern "C" {
#endif

#ifdef H5_HAVE_EFF

/*******************/
/* Public Typedefs */
/*******************/

/* An asynchronous request object */
typedef void *H5_request_t;


/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

/* API wrappers */
H5_DLL hid_t H5Fcreate_ff(const char *filename, unsigned flags, hid_t fcpl,
    hid_t fapl,
    H5_request_t *req);
H5_DLL hid_t H5Fopen_ff(const char *filename, unsigned flags, hid_t fapl_id,
    H5_request_t *req);
H5_DLL hid_t H5Gcreate_ff(hid_t loc_id, const char *name, hid_t lcpl_id,
    hid_t gcpl_id, hid_t gapl_id,
    uint64_t /* UNUSED */ trans, H5_request_t *req);
H5_DLL hid_t H5Gopen_ff(hid_t loc_id, const char *name, hid_t gapl_id,
    uint64_t /* UNUSED */ trans, H5_request_t *req);
H5_DLL hid_t H5Dcreate_ff(hid_t loc_id, const char *name, hid_t type_id,
    hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id,
    uint64_t /* UNUSED */ trans, H5_request_t *req);
H5_DLL hid_t H5Dopen_ff(hid_t loc_id, const char *name, hid_t dapl_id,
    uint64_t /* UNUSED */ trans, H5_request_t *req);
H5_DLL herr_t H5Dwrite_ff(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
    hid_t file_space_id, hid_t dxpl_id, const void *buf,
    uint64_t /* UNUSED */ trans, H5_request_t *req);
H5_DLL herr_t H5Dread_ff(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
    hid_t file_space_id, hid_t dxpl_id, void *buf/*out*/,
    uint64_t /* UNUSED */ trans, H5_request_t *req);

/* Asynchronous test & wait operations */
H5_DLL herr_t H5AOtest(H5_request_t *req, H5_status_t *status);
H5_DLL herr_t H5AOwait(H5_request_t *req, H5_status_t *status);

#endif /* H5_HAVE_EFF */

#ifdef __cplusplus
}
#endif

#endif /* _H5FFpublic_H */

