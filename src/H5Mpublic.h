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

/*
 * This file contains public declarations for the H5M module.
 *
 * NOTE:    This is an experimental API. Everything in the H5M package
 *          is subject to revision in a future release.
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

/* Macros defining operation IDs for map VOL callbacks (implemented using the
 * "optional" VOL callback) */
#define H5VL_MAP_CREATE         1
#define H5VL_MAP_OPEN           2
#define H5VL_MAP_GET_VAL        3
#define H5VL_MAP_EXISTS         4
#define H5VL_MAP_PUT            5
#define H5VL_MAP_GET            6
#define H5VL_MAP_SPECIFIC       7
#define H5VL_MAP_OPTIONAL       8
#define H5VL_MAP_CLOSE          9


/*******************/
/* Public Typedefs */
/*******************/

/* types for map GET callback */
typedef enum H5VL_map_get_t {
    H5VL_MAP_GET_MAPL,                      /* map access property list */
    H5VL_MAP_GET_MCPL,                      /* map creation property list */
    H5VL_MAP_GET_KEY_TYPE,                  /* key type                 */
    H5VL_MAP_GET_VAL_TYPE,                  /* value type               */
    H5VL_MAP_GET_COUNT                      /* key count                */
} H5VL_map_get_t;

/* types for map SPECIFIC callback */
typedef enum H5VL_map_specific_t {
    H5VL_MAP_ITER,                          /* H5Miterate               */
    H5VL_MAP_DELETE                         /* H5Mdelete                */
} H5VL_map_specific_t;

/* Callback for H5Miterate() */
typedef herr_t (*H5M_iterate_t)(hid_t map_id, const void *key, void *op_data);


/********************/
/* Public Variables */
/********************/


/*********************/
/* Public Prototypes */
/*********************/
#ifdef __cplusplus
extern "C" {
#endif

/* The map API is only built when requested since there's no support in
 * the native file format at this time. It's only supported in a few VOL
 * connectors.
 */
#ifdef H5_HAVE_MAP_API

H5_DLL hid_t H5Mcreate(hid_t loc_id, const char *name, hid_t key_type_id,
    hid_t val_type_id, hid_t lcpl_id, hid_t mcpl_id, hid_t mapl_id);
H5_DLL hid_t H5Mcreate_anon(hid_t loc_id, hid_t key_type_id, hid_t val_type_id,
    hid_t mcpl_id, hid_t mapl_id);
H5_DLL hid_t H5Mopen(hid_t loc_id, const char *name, hid_t mapl_id);
H5_DLL herr_t H5Mclose(hid_t map_id);
H5_DLL hid_t H5Mget_key_type(hid_t map_id);
H5_DLL hid_t H5Mget_val_type(hid_t map_id);
H5_DLL hid_t H5Mget_create_plist(hid_t map_id);
H5_DLL hid_t H5Mget_access_plist(hid_t map_id);
H5_DLL herr_t H5Mget_count(hid_t map_id, hsize_t *count, hid_t dxpl_id);
H5_DLL herr_t H5Mput(hid_t map_id, hid_t key_mem_type_id, const void *key,
    hid_t val_mem_type_id, const void *value, hid_t dxpl_id);
H5_DLL herr_t H5Mget(hid_t map_id, hid_t key_mem_type_id, const void *key,
    hid_t val_mem_type_id, void *value, hid_t dxpl_id);
H5_DLL herr_t H5Mexists(hid_t map_id, hid_t key_mem_type_id, const void *key,
    hbool_t *exists, hid_t dxpl_id);
H5_DLL herr_t H5Miterate(hid_t map_id, hsize_t *idx, hid_t key_mem_type_id,
    H5M_iterate_t op, void *op_data, hid_t dxpl_id);
H5_DLL herr_t H5Miterate_by_name(hid_t loc_id, const char *map_name,
    hsize_t *idx, hid_t key_mem_type_id, H5M_iterate_t op, void *op_data,
    hid_t dxpl_id, hid_t lapl_id);
H5_DLL herr_t H5Mdelete(hid_t map_id, hid_t key_mem_type_id,
    const void *key, hid_t dxpl_id);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS
#endif /* H5_NO_DEPRECATED_SYMBOLS */

#endif /*  H5_HAVE_MAP_API */

#ifdef __cplusplus
}
#endif

#endif /* _H5Mpublic_H */

