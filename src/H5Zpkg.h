/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#if !(defined H5Z_FRIEND || defined H5Z_MODULE)
#error "Do not include this file outside the H5Z package!"
#endif

#ifndef H5Zpkg_H
#define H5Zpkg_H

/* Include private header file */
#include "H5Zprivate.h" /* Filter functions                */

/* Package-private version-field value identifying H5Z_class3_t, used only
 * for internal dispatch in H5Zregister()/H5Z_register(). Deliberately not
 * exposed in H5Zdevelop.h: plugin authors set the version field to the
 * literal value 2 (see H5Z_class3_t's documentation), and this constant is
 * pinned to 2 forever regardless of any future H5Z_CLASS_T_VERS_MAX growth,
 * so it must not be confused with (or replaced by) that public constant. */
#define H5Z_CLASS3_T_VERS_INTERNAL (2)

/*
 * Internal filter table entry.  H5Z_class2_t is embedded as the first member
 * so that a pointer to H5Z_entry_t can be safely cast to H5Z_class2_t * per
 * C11 §6.7.2.1p15 without relying on struct-layout coincidence.  V3-specific
 * fields are zero-initialised for filters registered via H5Z_class2_t or
 * H5Z_class1_t.
 */
typedef struct H5Z_entry_t {
    H5Z_class2_t base; /* must stay first; safe to cast to H5Z_class2_t * */
    /* --- V3 extensions (NULL for v1/v2 plugins) --- */
    H5Z_func2_t           filter2; /* Extended callback (class3); NULL for class1/class2 */
    H5Z_set_config_func_t set_config;
    H5Z_get_config_func_t get_config;
    const char           *description; /* free-form description; may be NULL */
    /* --- Reserved blob callbacks (NULL until activated by RFC-HDFG-2026-*) --- */
    void *write_blob;
    void *read_blob;
    void *close_blob;
} H5Z_entry_t;

/********************/
/* Internal filters */
/********************/

/* Shuffle filter */
H5_ATTR_VISIBILITY_HIDDEN extern const H5Z_class3_t H5Z_SHUFFLE[1];

/* Fletcher32 filter */
H5_ATTR_VISIBILITY_HIDDEN extern const H5Z_class3_t H5Z_FLETCHER32[1];

/* n-bit filter */
H5_ATTR_VISIBILITY_HIDDEN extern H5Z_class3_t H5Z_NBIT[1];

/* Scale/offset filter */
H5_ATTR_VISIBILITY_HIDDEN extern H5Z_class3_t H5Z_SCALEOFFSET[1];

/********************/
/* External filters */
/********************/

/* Deflate filter */
#ifdef H5_HAVE_FILTER_DEFLATE
H5_ATTR_VISIBILITY_HIDDEN extern const H5Z_class3_t H5Z_DEFLATE[1];
#endif /* H5_HAVE_FILTER_DEFLATE */

/* szip filter */
#ifdef H5_HAVE_FILTER_SZIP
H5_ATTR_VISIBILITY_HIDDEN extern H5Z_class3_t H5Z_SZIP[1];
#endif /* H5_HAVE_FILTER_SZIP */

/* Package internal routines */
H5_DLL herr_t H5Z__reregister_deflate(void);
H5_DLL herr_t H5Z__unregister(H5Z_filter_t filter_id);
H5_DLL herr_t H5Z__config_validate_keys(const char *params, const char *const *known_keys);
H5_DLL htri_t H5Z__config_get_int(const char *params, const char *key, int64_t *out);
H5_DLL htri_t H5Z__config_get_str(const char *params, const char *key, char *buf, size_t *buf_size);

/* Shared set_config implementation for filters that take no user parameters
 * (shuffle, fletcher32).  Sets *cd_nelmts = 0 and rejects non-empty params. */
H5_DLL herr_t H5Z__no_params_set_config(const char *params, unsigned *flags, size_t *cd_nelmts,
                                        unsigned cd_values[], size_t cd_values_size);

#endif /* H5Zpkg_H */
