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
 * This file contains private information about the H5T module
 */
#ifndef _H5Tprivate_H
#define _H5Tprivate_H

#include "H5Tpublic.h"

/* Private headers needed by this file */
#include "H5private.h"
#include "H5Gprivate.h"         /*for H5G_entry_t                            */
#include "H5Rprivate.h"		/*for H5R_type_t			     */

#define H5T_RESERVED_ATOMS 	8
#define H5T_NAMELEN		32	/*length of debugging name buffer    */

typedef struct H5T_t H5T_t;

/* How to copy a data type */
typedef enum H5T_copy_t {
    H5T_COPY_TRANSIENT,
    H5T_COPY_ALL,
    H5T_COPY_REOPEN
} H5T_copy_t;

/* Statistics about a conversion function */
typedef struct H5T_stats_t {
    unsigned	ncalls;			/*num calls to conversion function   */
    hsize_t	nelmts;			/*total data points converted	     */
    H5_timer_t	timer;			/*total time for conversion	     */
} H5T_stats_t;

/* The data type conversion database */
typedef struct H5T_path_t {
    char	name[H5T_NAMELEN];	/*name for debugging only	     */
    H5T_t	*src;			/*source data type ID		     */
    H5T_t	*dst;			/*destination data type ID	     */
    H5T_conv_t	func;			/*data conversion function	     */
    hbool_t	is_hard;		/*is it a hard function?	     */
    H5T_stats_t	stats;			/*statistics for the conversion	     */
    H5T_cdata_t	cdata;			/*data for this function	     */
} H5T_path_t;

/*
 * VL types allowed.
 */
typedef enum {
    H5T_VLEN_BADTYPE =  -1, /* invalid VL Type */
    H5T_VLEN_SEQUENCE=0,    /* VL sequence */
    H5T_VLEN_STRING,        /* VL string */
    H5T_VLEN_MAXTYPE        /* highest type (Invalid as true type) */
} H5T_vlen_type_t;

typedef enum {
    H5T_VLEN_BADLOC =   0,  /* invalid VL Type */
    H5T_VLEN_MEMORY,        /* VL data stored in memory */
    H5T_VLEN_DISK,          /* VL data stored on disk */
    H5T_VLEN_MAXLOC         /* highest type (Invalid as true type) */
} H5T_vlen_loc_t;

/*
 * Internal data structure for passing information to H5T_vlen_get_buf_size
 */
typedef struct {
    hid_t dataset_id;   /* ID of the dataset we are working on */
    hid_t fspace_id;    /* ID of the file dataset's dataspace we are working on */
    hid_t mspace_id;    /* ID of the memory dataset's dataspace we are working on */
    void *fl_tbuf;      /* Ptr to the temporary buffer we are using for fixed-length data */
    void *vl_tbuf;      /* Ptr to the temporary buffer we are using for VL data */
    hid_t xfer_pid;     /* ID of the dataset xfer property list */
    hsize_t size;       /* Accumulated number of bytes for the selection */
} H5T_vlen_bufsize_t;

/*
 * Is the path the special no-op path? The no-op function can be set by the
 * application and there might be more than one no-op path in a
 * multi-threaded application if one thread is using the no-op path when some
 * other thread changes its definition.
 */
#define H5T_IS_NOOP(P)	((P)->is_hard && 0==H5T_cmp((P)->src, (P)->dst))

/* Private functions */
H5_DLL herr_t H5TN_init_interface(void);
H5_DLL herr_t H5T_init(void);
H5_DLL htri_t H5T_isa(H5G_entry_t *ent);
H5_DLL H5T_t *H5T_open(H5G_entry_t *loc, const char *name);
H5_DLL H5T_t *H5T_open_oid(H5G_entry_t *ent);
H5_DLL H5T_t *H5T_create(H5T_class_t type, size_t size);
H5_DLL H5T_t *H5T_copy(const H5T_t *old_dt, H5T_copy_t method);
H5_DLL herr_t H5T_commit(H5G_entry_t *loc, const char *name, H5T_t *type);
H5_DLL herr_t H5T_lock(H5T_t *dt, hbool_t immutable);
H5_DLL herr_t H5T_close(H5T_t *dt);
H5_DLL herr_t H5T_unregister(H5T_pers_t pers, const char *name, H5T_t *src,
                H5T_t *dst, H5T_conv_t func);
H5_DLL herr_t H5T_path_force_reinit(H5T_t *dt);
H5_DLL H5T_class_t H5T_get_class(const H5T_t *dt);
H5_DLL htri_t H5T_detect_class (const H5T_t *dt, H5T_class_t cls);
H5_DLL size_t H5T_get_size(const H5T_t *dt);
H5_DLL H5T_sign_t H5T_get_sign(H5T_t *dt);
H5_DLL H5T_t *H5T_get_super(H5T_t *dt);
H5_DLL char  *H5T_get_member_name(H5T_t *dt, int membno);
H5_DLL herr_t H5T_get_member_value(H5T_t *dt, int membno, void *value);
H5_DLL H5T_t *H5T_get_member_type(H5T_t *dt, int membno);
H5_DLL size_t H5T_get_member_offset(H5T_t *dt, int membno);
H5_DLL int H5T_get_nmembers(const H5T_t *dt);
H5_DLL htri_t H5T_is_variable_str(H5T_t *dt);
H5_DLL H5T_t *H5T_get_native_type(H5T_t *dt, H5T_direction_t direction, size_t *struct_align, size_t *offset, size_t *comp_size);
H5_DLL H5T_t *H5T_get_native_integer(size_t size, H5T_sign_t sign, H5T_direction_t direction, size_t *struct_align, size_t *offset, size_t *comp_size);
H5_DLL H5T_t *H5T_get_native_float(size_t size, H5T_direction_t direction, size_t *struct_align, size_t *offset, size_t *comp_size);
H5_DLL int    H5T_cmp(const H5T_t *dt1, const H5T_t *dt2);
H5_DLL htri_t H5T_is_atomic(const H5T_t *dt);
H5_DLL herr_t H5T_insert(H5T_t *parent, const char *name, size_t offset,
        const H5T_t *member);
H5_DLL H5T_t *H5T_enum_create(H5T_t *parent);
H5_DLL herr_t H5T_enum_insert(H5T_t *dt, const char *name, void *value);
H5_DLL int    H5T_get_array_ndims(H5T_t *dt);
H5_DLL herr_t H5T_get_array_dims(H5T_t *dt, hsize_t dims[], int perm[]);
H5_DLL herr_t H5T_pack(H5T_t *dt);
H5_DLL herr_t H5T_debug(const H5T_t *dt, FILE * stream);
H5_DLL H5G_entry_t *H5T_entof(H5T_t *dt);
H5_DLL htri_t H5T_is_immutable(H5T_t *dt);
H5_DLL htri_t H5T_is_named(H5T_t *dt);
H5_DLL H5T_path_t *H5T_path_find(const H5T_t *src, const H5T_t *dst,
				  const char *name, H5T_conv_t func);
H5_DLL herr_t H5T_sort_value(H5T_t *dt, int *map);
H5_DLL herr_t H5T_sort_name(H5T_t *dt, int *map);
H5_DLL herr_t H5T_convert(H5T_path_t *tpath, hid_t src_id, hid_t dst_id,
			   hsize_t nelmts, size_t buf_stride, size_t bkg_stride,
                           void *buf, void *bkg, hid_t dset_xfer_plist);
H5_DLL herr_t H5T_set_size(H5T_t *dt, size_t size);
H5_DLL herr_t H5T_set_precision(H5T_t *dt, size_t prec);
H5_DLL herr_t H5T_set_offset(H5T_t *dt, size_t offset);
H5_DLL char *H5T_enum_nameof(H5T_t *dt, void *value, char *name/*out*/,
			      size_t size);
H5_DLL herr_t H5T_enum_valueof(H5T_t *dt, const char *name,
				void *value/*out*/);
H5_DLL herr_t H5T_vlen_reclaim(void *elem, hid_t type_id, hsize_t ndim, hssize_t *point, void *_op_data);
H5_DLL htri_t H5T_vlen_mark(H5T_t *dt, H5F_t *f, H5T_vlen_loc_t loc);
H5_DLL htri_t H5T_is_sensible(const H5T_t *dt);

/* Reference specific functions */
H5_DLL H5R_type_t H5T_get_ref_type(const H5T_t *dt);

#endif
