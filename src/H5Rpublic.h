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
 * This file contains public declarations for the H5R module.
 */
#ifndef _H5Rpublic_H
#define _H5Rpublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Gpublic.h"
#include "H5Ipublic.h"

/*
 * Reference types allowed.
 */
typedef enum {
    H5R_BADTYPE     =   (-1),   /*invalid Reference Type                     */
    H5R_OBJECT,                 /*Object reference                           */
    H5R_REGION_COMPAT,          /*For backward compatibility (region)        */
    H5R_REGION,                 /*Region Reference                           */
    H5R_ATTR,                   /*Attribute Reference                        */
    H5R_EXT_OBJECT,             /*External Object reference                  */
    H5R_EXT_REGION,             /*External Region reference                  */
    H5R_EXT_ATTR,               /*External Attribute reference               */
    H5R_MAXTYPE                 /*highest type (Invalid as true type)        */
} H5R_type_t;

/* Opaque reference type */
typedef struct href *href_t;

/* NULL reference */
#define HREF_NULL ((href_t)0)

/* Publicly visible data structures */

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5R.c */
H5_DLL href_t H5Rcreate_object(hid_t loc_id, const char *name);
H5_DLL href_t H5Rcreate_region(hid_t loc_id, const char *name, hid_t space_id);
H5_DLL href_t H5Rcreate_attr(hid_t loc_id, const char *name, const char *attr_name);
H5_DLL href_t H5Rcreate_ext_object(const char *filename, const char *pathname);
H5_DLL href_t H5Rcreate_ext_region(const char *filename, const char *pathname, hid_t space_id);
H5_DLL href_t H5Rcreate_ext_attr(const char *filename, const char *pathname, const char *attr_name);
H5_DLL herr_t H5Rdestroy(href_t ref);

H5_DLL H5R_type_t H5Rget_type(href_t ref);
H5_DLL htri_t     H5Requal(href_t ref1, href_t ref2);
H5_DLL href_t     H5Rcopy(href_t ref);

H5_DLL hid_t      H5Rget_object(hid_t loc_id, hid_t oapl_id, href_t ref);
H5_DLL hid_t      H5Rget_region2(hid_t loc_id, href_t ref);
H5_DLL hid_t      H5Rget_attr(hid_t loc_id, href_t ref);

H5_DLL herr_t     H5Rget_obj_type3(hid_t loc_id, href_t ref, H5O_type_t *obj_type);
H5_DLL ssize_t    H5Rget_obj_name(hid_t loc_id, href_t ref, char *name/*out*/, size_t size);
H5_DLL ssize_t    H5Rget_attr_name(hid_t loc_id, href_t ref, char *name/*out*/, size_t size);
H5_DLL ssize_t    H5Rget_file_name(href_t ref, char *name/*out*/, size_t size);

H5_DLL herr_t H5Rencode(href_t ref, void *buf, size_t *nalloc);
H5_DLL href_t H5Rdecode(const void *buf);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */
#define H5R_DATASET_REGION H5R_REGION_COMPAT

/* Typedefs */

/* Function prototypes */
H5_DLL herr_t H5Rcreate(void *ref, hid_t loc_id, const char *name,
                        H5R_type_t ref_type, hid_t space_id);
H5_DLL hid_t H5Rdereference1(hid_t obj_id, H5R_type_t ref_type, const void *ref);
H5_DLL hid_t H5Rdereference2(hid_t obj_id, hid_t oapl_id, H5R_type_t ref_type, const void *ref);
H5_DLL hid_t H5Rget_region1(hid_t dataset, H5R_type_t ref_type, const void *ref);
H5_DLL H5G_obj_t H5Rget_obj_type1(hid_t id, H5R_type_t ref_type, const void *_ref);
H5_DLL herr_t H5Rget_obj_type2(hid_t id, H5R_type_t ref_type, const void *_ref,
    H5O_type_t *obj_type);
H5_DLL ssize_t H5Rget_name(hid_t loc_id, H5R_type_t ref_type, const void *ref,
    char *name/*out*/, size_t size);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif

#endif  /* _H5Rpublic_H */

