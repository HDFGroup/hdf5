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
 * H5M module.
 */
#ifndef _H5Mpublic_H
#define _H5Mpublic_H

/* System headers needed by this file */

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/*****************/
/* Public Macros */
/*****************/

/*******************/
/* Public Typedefs */
/*******************/

/********************/
/* Public Variables */
/********************/


#ifdef __cplusplus
extern "C" {
#endif

/* Typedef for H5Miterate() callbacks */
typedef herr_t (*H5M_iterate_func_t)(const void *key, const void *value, void *context);

/*********************/
/* Public Prototypes */
/*********************/

/* API wrappers */
H5_DLL hid_t H5Mcreate_ff(hid_t loc_id, const char *name, hid_t keytype, hid_t valtype, hid_t lcpl_id, 
                          hid_t mcpl_id, hid_t mapl_id, hid_t trans_id, hid_t estack_id);
H5_DLL hid_t H5Mopen_ff(hid_t loc_id, const char *name, hid_t mapl_id, hid_t rcxt_id, hid_t estack_id);
H5_DLL herr_t H5Mset_ff(hid_t map_id, hid_t key_mem_type_id, const void *key, hid_t val_mem_type_id, 
                        const void *value, hid_t dxpl_id, hid_t trans_id, hid_t estack_id);
H5_DLL herr_t H5Mget_ff(hid_t map_id, hid_t key_mem_type_id, const void *key, hid_t val_mem_type_id, 
                        void *value, hid_t dxpl_id, hid_t rcxt_id, hid_t estack_id);
H5_DLL herr_t H5Mget_types_ff(hid_t map_id, hid_t *key_type_id, hid_t *val_type_id, 
                              hid_t rcxt_id, hid_t estack_id);
H5_DLL herr_t H5Mget_count_ff(hid_t map_id, hsize_t *count, hid_t rcxt_id, hid_t estack_id);
H5_DLL herr_t H5Mexists_ff(hid_t map_id, hid_t key_mem_type_id, const void *key, 
                           hbool_t *exists, hid_t rcxt_id, hid_t estack_id);
H5_DLL herr_t H5Miterate_ff(hid_t map_id, hid_t key_mem_type_id, hid_t value_mem_type_id, 
                            H5M_iterate_func_t callback_func, void *context, hid_t rcxt_id);
H5_DLL herr_t H5Mdelete_ff(hid_t map_id, hid_t key_mem_type_id, const void *key, 
                           hid_t trans_id, hid_t estack_id);
H5_DLL herr_t H5Mclose_ff(hid_t map_id, hid_t estack_id);

#ifdef __cplusplus
}
#endif
#endif /* _H5Mpublic_H */
