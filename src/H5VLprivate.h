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
 *              January, 2011
 */
#ifndef _H5VLprivate_H
#define _H5VLprivate_H

#include "H5VLpublic.h"

/**************************/
/* Library Private Macros */
/**************************/

/****************************/
/* Library Private Typedefs */
/****************************/

/*****************************/
/* Library Private Variables */
/*****************************/

/******************************/
/* Library Private Prototypes */
/******************************/

/* Forward declarations for prototype arguments */
struct H5P_genplist_t;
struct H5F_t;

H5_DLL int H5VL_term_interface(void);
H5_DLL H5VL_class_t *H5VL_get_class(hid_t id);
//H5_DLL hsize_t H5VL_sb_size(H5F_t *file);
H5_DLL hid_t H5VL_register(const void *cls, size_t size, hbool_t app_ref);
H5_DLL H5F_t *H5VL_open(const char *name, unsigned flags, hid_t fapl_id);
H5_DLL H5F_t *H5VL_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id);
H5_DLL herr_t H5VL_close(H5F_t *f);
H5_DLL herr_t H5VL_fapl_open(struct H5P_genplist_t *plist, hid_t vol_id, const void *vol_info);
H5_DLL herr_t H5VL_fapl_copy(hid_t vol_id, const void *fapl, void **copied_fapl);
H5_DLL herr_t H5VL_fapl_close(hid_t vol_id, void *fapl);

#endif /* !_H5VLprivate_H */
