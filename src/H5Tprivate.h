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
 * This file contains private information about the H5T module
 */
#ifndef _H5Tprivate_H
#define _H5Tprivate_H

/* Get package's public header */
#include "H5Tpublic.h"

/* Other public headers needed by this file */
#include "H5MMpublic.h"         /* Memory management                    */

/* Private headers needed by this file */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Gprivate.h"		/* Groups 			  	*/
#include "H5Rprivate.h"		/* References				*/

/* Forward references of package typedefs */
typedef struct H5T_t H5T_t;
typedef struct H5T_stats_t H5T_stats_t;
typedef struct H5T_path_t H5T_path_t;

/* How to copy a datatype */
typedef enum H5T_copy_t {
    H5T_COPY_TRANSIENT,
    H5T_COPY_ALL,
    H5T_COPY_REOPEN
} H5T_copy_t;

/* Location of datatype information */
typedef enum {
    H5T_LOC_BADLOC =   0,  /* invalid datatype location */
    H5T_LOC_MEMORY,        /* data stored in memory */
    H5T_LOC_DISK,          /* data stored on disk */
    H5T_LOC_MAXLOC         /* highest value (Invalid as true value) */
} H5T_loc_t;

/* VL allocation information */
typedef struct {
    H5MM_allocate_t alloc_func; /* Allocation function */
    void *alloc_info;           /* Allocation information */
    H5MM_free_t free_func;      /* Free function */
    void *free_info;            /* Free information */
} H5T_vlen_alloc_info_t;

/* Structure for conversion callback property */
typedef struct H5T_conv_cb_t {
    H5T_conv_except_func_t      func;
    void*                       user_data;
} H5T_conv_cb_t;

/* Values for the optimization of compound data reading and writing.  They indicate
 * whether the fields of the source and destination are subset of each other and
 * there is no conversion needed.  It's for the Chicago company.
 */
typedef enum {
    H5T_SUBSET_BADVALUE = -1,   /* Invalid value */
    H5T_SUBSET_FALSE = 0,       /* Source and destination aren't subset of each other */ 
    H5T_SUBSET_SRC,             /* Source is the subset of dest and no conversion is needed */
    H5T_SUBSET_DST,             /* Dest is the subset of source and no conversion is needed */
    H5T_SUBSET_CAP              /* Must be the last value */
} H5T_subset_t;

/* Forward declarations for prototype arguments */
struct H5O_t;

/* The native endianess of the platform */
H5_DLLVAR H5T_order_t H5T_native_order_g;

/* Private functions */
H5_DLL herr_t H5TN_init_interface(void);
H5_DLL herr_t H5T_init(void);
H5_DLL H5T_t *H5T_copy(const H5T_t *old_dt, H5T_copy_t method);
H5_DLL herr_t H5T_lock(H5T_t *dt, hbool_t immutable);
H5_DLL herr_t H5T_close(H5T_t *dt);
H5_DLL H5T_class_t H5T_get_class(const H5T_t *dt, htri_t internal);
H5_DLL htri_t H5T_detect_class(const H5T_t *dt, H5T_class_t cls);
H5_DLL size_t H5T_get_size(const H5T_t *dt);
H5_DLL int    H5T_cmp(const H5T_t *dt1, const H5T_t *dt2, hbool_t superset);
H5_DLL herr_t H5T_debug(const H5T_t *dt, FILE * stream);
H5_DLL struct H5O_loc_t *H5T_oloc(H5T_t *dt);
H5_DLL H5G_name_t *H5T_nameof(H5T_t *dt);
H5_DLL htri_t H5T_is_immutable(const H5T_t *dt);
H5_DLL htri_t H5T_is_named(const H5T_t *dt);
H5_DLL htri_t H5T_is_relocatable(const H5T_t *dt);
H5_DLL H5T_path_t *H5T_path_find(const H5T_t *src, const H5T_t *dst,
				  const char *name, H5T_conv_t func, hid_t dxpl_id, hbool_t is_api);
H5_DLL hbool_t H5T_path_noop(const H5T_path_t *p);
H5_DLL H5T_bkg_t H5T_path_bkg(const H5T_path_t *p);
H5_DLL herr_t H5T_convert(H5T_path_t *tpath, hid_t src_id, hid_t dst_id,
			   size_t nelmts, size_t buf_stride, size_t bkg_stride,
                           void *buf, void *bkg, hid_t dset_xfer_plist);
H5_DLL herr_t H5T_vlen_reclaim(void *elem, hid_t type_id, unsigned ndim, const hsize_t *point, void *_op_data);
H5_DLL herr_t H5T_vlen_get_alloc_info(hid_t dxpl_id, H5T_vlen_alloc_info_t **vl_alloc_info);
H5_DLL htri_t H5T_set_loc(H5T_t *dt, H5F_t *f, H5T_loc_t loc);
H5_DLL htri_t H5T_is_sensible(const H5T_t *dt);
H5_DLL uint32_t H5T_hash(H5F_t * file, const H5T_t *dt);

/* Reference specific functions */
H5_DLL H5R_type_t H5T_get_ref_type(const H5T_t *dt);

/* Operations on named datatypes */
H5_DLL H5T_t *H5T_open(const H5G_loc_t *loc, hid_t dxpl_id);
H5_DLL htri_t H5T_committed(const H5T_t *type);
H5_DLL int H5T_link(const H5T_t *type, int adjust, hid_t dxpl_id);
H5_DLL herr_t H5T_update_shared(H5T_t *type);

#endif /* _H5Tprivate_H */

