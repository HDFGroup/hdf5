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

#include <H5Tpublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Gprivate.h>         /*for H5G_entry_t                            */

#define H5T_RESERVED_ATOMS  8
typedef struct H5T_t H5T_t;

/* Statistics about a conversion function */
typedef struct H5T_stats_t {
    uintn	ncalls;			/*num calls to conversion function   */
#ifdef H5T_DEBUG
    hsize_t	nelmts;			/*total data points converted	     */
    H5_timer_t	timer;			/*total time for conversion	     */
#endif
} H5T_stats_t;

/* How to copy a data type */
typedef enum H5T_copy_t {
    H5T_COPY_TRANSIENT,
    H5T_COPY_ALL,
    H5T_COPY_REOPEN
} H5T_copy_t;

/* Private functions */
herr_t H5T_init (void);
herr_t H5T_init_interface (void);
H5T_t *H5T_open (H5G_t *loc, const char *name);
H5T_t *H5T_create (H5T_class_t type, size_t size);
H5T_t *H5T_copy (const H5T_t *old_dt, H5T_copy_t method);
herr_t H5T_commit (H5G_t *loc, const char *name, H5T_t *type);
herr_t H5T_lock (H5T_t *dt, hbool_t immutable);
herr_t H5T_close (H5T_t *dt);
size_t H5T_get_size (const H5T_t *dt);
intn H5T_cmp (const H5T_t *dt1, const H5T_t *dt2);
hbool_t H5T_is_atomic (const H5T_t *dt);
herr_t H5T_insert (H5T_t *parent, const char *name, size_t offset,
                   const H5T_t *member);
herr_t H5T_sort_by_offset (H5T_t *dt);
herr_t H5T_pack (H5T_t *dt);
herr_t H5T_debug (H5T_t *dt, FILE * stream);
H5T_conv_t H5T_find (const H5T_t *src, const H5T_t *dst, H5T_bkg_t need_bkg,
		     H5T_cdata_t **pcdata/*out*/);
H5G_entry_t *H5T_entof (H5T_t *dt);
void H5T_timer_begin (H5_timer_t *timer, H5T_cdata_t *cdata);
void H5T_timer_end (H5_timer_t *timer, H5T_cdata_t *cdata, size_t nelmts);

/*
 * This conversion function is here so we can determine whether a conversion
 * is a no-op or not.  The other conversion functions can go in H5Tpkg.h
 */
herr_t H5T_conv_noop (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void *bkg);
#endif
