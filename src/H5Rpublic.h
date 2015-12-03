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
    H5R_REGION,                 /*Dataset Region Reference                   */
    H5R_ATTR,                   /*Attribute Reference                        */
    H5R_EXT_OBJECT,             /*External Object reference                  */
    H5R_EXT_REGION,             /*External Region reference                  */
    H5R_EXT_ATTR,               /*External Attribute reference               */
    H5R_MAXTYPE                 /*highest type (Invalid as true type)        */
} H5R_type_t;

/* Opaque reference type */
typedef struct href_t href_t;

/* Publicly visible data structures */

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5R.c */
H5_DLL href_t *H5Rcreate_object(hid_t loc_id, const char *name);
H5_DLL href_t *H5Rcreate_region(hid_t loc_id, const char *name, hid_t space_id);
H5_DLL href_t *H5Rcreate_attr(hid_t loc_id, const char *name, const char *attr_name);
H5_DLL href_t *H5Rcreate_ext_object(hid_t loc_id, const char *name);
H5_DLL href_t *H5Rcreate_ext_region(hid_t loc_id, const char *name, hid_t space_id);
H5_DLL href_t *H5Rcreate_ext_attr(hid_t loc_id, const char *name, const char *attr_name);
H5_DLL herr_t  H5Rdestroy(href_t *ref);

H5_DLL hid_t   H5Rdereference2(hid_t obj_id, hid_t oapl_id, const href_t *ref);
H5_DLL hid_t   H5Rget_region(hid_t loc_id, const href_t *ref);
H5_DLL herr_t  H5Rget_obj_type2(hid_t loc_id, const href_t *ref, H5O_type_t *obj_type);
H5_DLL ssize_t H5Rget_name(hid_t loc_id, const href_t *ref, char *name/*out*/, size_t size);
H5_DLL ssize_t H5Rget_filename(const href_t *ref, char *name/*out*/, size_t size);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */


/* Typedefs */


/* Function prototypes */
/* TODO should add a H5Rcreate1 that maps to other types */
H5_DLL H5G_obj_t H5Rget_obj_type1(hid_t id, H5R_type_t ref_type, const void *_ref);
H5_DLL hid_t H5Rdereference1(hid_t obj_id, H5R_type_t ref_type, const void *ref);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif

#endif  /* _H5Rpublic_H */

