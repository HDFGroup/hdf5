/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/*
 * This file contains private information about the H5P module
 */
#ifndef _H5Pprivate_H
#define _H5Pprivate_H

#include "H5Ppublic.h"

/* Private headers needed by this file */
#include "H5private.h"

/* Forward declarations for anonymous H5P objects */
typedef struct H5P_genplist_t H5P_genplist_t;
typedef struct H5P_genclass_t H5P_genclass_t;

/* Private functions, not part of the publicly documented API */
H5_DLL herr_t H5P_init(void);

/* Internal versions of API routines */
H5_DLL herr_t H5P_close(void *_plist);
H5_DLL hid_t H5P_create_id(H5P_genclass_t *pclass);
H5_DLL hid_t H5P_copy_plist(H5P_genplist_t *old_plist);
H5_DLL herr_t H5P_get(H5P_genplist_t *plist, const char *name, void *value);
H5_DLL herr_t H5P_set(H5P_genplist_t *plist, const char *name, const void *value);
H5_DLL herr_t H5P_insert(H5P_genplist_t *plist, const char *name, size_t size,
    void *value, H5P_prp_set_func_t prp_set, H5P_prp_get_func_t prp_get,
    H5P_prp_delete_func_t prp_delete, H5P_prp_copy_func_t prp_copy, 
    H5P_prp_close_func_t prp_close);
H5_DLL herr_t H5P_remove(hid_t plist_id, H5P_genplist_t *plist, const char *name);
H5_DLL htri_t H5P_exist_plist(H5P_genplist_t *plist, const char *name);
H5_DLL char *H5P_get_class_name(H5P_genclass_t *pclass);
H5_DLL char *H5P_get_class_path(H5P_genclass_t *pclass);
H5_DLL H5P_genclass_t *H5P_open_class_path(const char *path);
H5_DLL herr_t H5P_close_class(void *_pclass);
H5_DLL herr_t H5P_get_nprops_pclass(H5P_genclass_t *pclass, size_t *nprops);
H5_DLL herr_t H5P_register(H5P_genclass_t *pclass, const char *name, size_t size,
            void *def_value, H5P_prp_create_func_t prp_create, H5P_prp_set_func_t prp_set,
            H5P_prp_get_func_t prp_get, H5P_prp_delete_func_t prp_delete,
            H5P_prp_copy_func_t prp_copy, H5P_prp_close_func_t prp_close);
H5_DLL hid_t H5P_get_driver(H5P_genplist_t *plist);
H5_DLL void * H5P_get_driver_info(H5P_genplist_t *plist);
H5_DLL herr_t H5P_set_driver(H5P_genplist_t *plist, hid_t new_driver_id,
            const void *new_driver_info);
H5_DLL herr_t H5P_set_family_offset(H5P_genplist_t *plist, hsize_t offset);
H5_DLL herr_t H5P_get_family_offset(H5P_genplist_t *plist, hsize_t *offset);
H5_DLL herr_t H5P_set_multi_type(H5P_genplist_t *plist, H5FD_mem_t type);
H5_DLL herr_t H5P_get_multi_type(H5P_genplist_t *plist, H5FD_mem_t *type);
H5_DLL herr_t H5P_set_vlen_mem_manager(H5P_genplist_t *plist,
        H5MM_allocate_t alloc_func, void *alloc_info, H5MM_free_t free_func,
        void *free_info);
H5_DLL herr_t H5P_fill_value_defined(H5P_genplist_t *plist,
        H5D_fill_value_t *status);

/* *SPECIAL* Don't make more of these! -QAK */
H5_DLL htri_t H5P_isa_class(hid_t plist_id, hid_t pclass_id);
H5_DLL void *H5P_object_verify(hid_t plist_id, hid_t pclass_id);

/* Testing functions */
#ifdef H5P_TESTING
H5_DLL char *H5P_get_class_path_test(hid_t pclass_id);
H5_DLL hid_t H5P_open_class_path_test(const char *path);
#endif /* H5P_TESTING */

/* Private functions to "peek" at properties of a certain type */
H5_DLL unsigned H5P_peek_unsigned(H5P_genplist_t *plist, const char *name);
H5_DLL hid_t H5P_peek_hid_t(H5P_genplist_t *plist, const char *name);
H5_DLL void *H5P_peek_voidp(H5P_genplist_t *plist, const char *name);
H5_DLL hsize_t H5P_peek_hsize_t(H5P_genplist_t *plist, const char *name);
H5_DLL size_t H5P_peek_size_t(H5P_genplist_t *plist, const char *name);

#endif
