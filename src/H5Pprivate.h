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
typedef struct H5P_genplist_tag H5P_genplist_t;
typedef struct H5P_genclass_tag H5P_genclass_t;

/* Private functions, not part of the publicly documented API */
__DLL__ herr_t H5P_init(void);

/* Internal versions of API routines */
__DLL__ hid_t H5P_create_id(H5P_genclass_t *pclass);
__DLL__ hid_t H5P_copy_plist(H5P_genplist_t *old_plist);
__DLL__ herr_t H5P_get(H5P_genplist_t *plist, const char *name, void *value);
__DLL__ herr_t H5P_set(H5P_genplist_t *plist, const char *name, const void *value);
__DLL__ char *H5P_get_class_name(H5P_genclass_t *pclass);
__DLL__ herr_t H5P_get_nprops_pclass(H5P_genclass_t *pclass, size_t *nprops);
__DLL__ herr_t H5P_register(H5P_genclass_t *pclass, const char *name, size_t size,
            void *def_value, H5P_prp_create_func_t prp_create, H5P_prp_set_func_t prp_set,
            H5P_prp_get_func_t prp_get, H5P_prp_delete_func_t prp_delete,
            H5P_prp_copy_func_t prp_copy, H5P_prp_close_func_t prp_close);
__DLL__ hid_t H5P_get_driver(H5P_genplist_t *plist);
__DLL__ void * H5P_get_driver_info(H5P_genplist_t *plist);
__DLL__ herr_t H5P_set_driver(H5P_genplist_t *plist, hid_t new_driver_id,
            const void *new_driver_info);
__DLL__ herr_t H5P_set_vlen_mem_manager(H5P_genplist_t *plist,
        H5MM_allocate_t alloc_func, void *alloc_info, H5MM_free_t free_func,
        void *free_info);

/* *SPECIAL* Don't make more of these! -QAK */
__DLL__ htri_t H5P_isa_class(hid_t plist_id, hid_t pclass_id);


/* Private functions to "peek" at properties of a certain type */
__DLL__ unsigned H5P_peek_unsigned(H5P_genplist_t *plist, const char *name);
__DLL__ hid_t H5P_peek_hid_t(H5P_genplist_t *plist, const char *name);
__DLL__ void *H5P_peek_voidp(H5P_genplist_t *plist, const char *name);
__DLL__ hsize_t H5P_peek_hsize_t(H5P_genplist_t *plist, const char *name);
__DLL__ size_t H5P_peek_size_t(H5P_genplist_t *plist, const char *name);

#endif
