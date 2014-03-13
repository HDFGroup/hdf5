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
 * This file contains private information about the H5X module
 */
#ifndef _H5Xprivate_H
#define _H5Xprivate_H

/* Include package's public header */
#include "H5Xpublic.h"

/* Private headers needed by this file */

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
H5_DLL herr_t H5X_init(void);

H5_DLL H5X_class_t *H5X_registered(unsigned plugin_id);
H5_DLL herr_t H5X_register(const H5X_class_t *index_plugin);
H5_DLL herr_t H5X_unregister(unsigned intex_type);

H5_DLL herr_t H5X_create(hid_t file_id, unsigned plugin_id, hid_t scope_id,
        hid_t xcpl_id);
H5_DLL herr_t H5X_create_ff(hid_t file_id, unsigned plugin_id,
        hid_t scope_id, hid_t xcpl_id, hid_t trans_id, hid_t estack_id);

H5_DLL herr_t H5X_remove(hid_t file_id, unsigned plugin_id, hid_t scope_id);
H5_DLL herr_t H5X_remove_ff(hid_t file_id, unsigned plugin_id, hid_t scope_id,
        hid_t trans_id, hid_t estack_id);

H5_DLL herr_t H5X_get_count(hid_t scope_id, hsize_t *idx_count);
H5_DLL herr_t H5X_get_count_ff(hid_t scope_id, hsize_t *idx_count, hid_t rcxt_id,
        hid_t estack_id);

#endif /* _H5Xprivate_H */
