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

#include <H5Ppublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Gprivate.h>         /*for H5G_entry_t                            */
#include <H5Oprivate.h>

#define H5P_RESERVED_ATOMS  2

/* Flags to indicate special dataspace features are active */
#define H5P_VALID_MAX   0x01
#define H5P_VALID_PERM  0x02

typedef struct H5P_simple_t {
    intn        rank;                   /*number of dimensions               */
    intn        dim_flags;              /*dimension flags                    */
    size_t      *size;                  /*dimension sizes                    */
    size_t      *max;                   /*maximum dimension sizes            */
    intn        *perm;                  /*dimension permutations             */
} H5P_simple_t;

typedef struct {
    H5P_class_t         type;           /*type of dimensionality object      */
    union {
        H5P_simple_t    simple;         /*simple dimensionality information  */
    } u;
} H5P_t;

/*
 * This structure contains information about how the elements of a data space
 * are numbered.
 */
typedef struct H5P_number_t {
    int		_place_holder;		/*remove this field!*/
} H5P_number_t;

/*
 * Callbacks for data space conversion.
 */
typedef struct H5P_tconv_t {
    /* Initialize element numbering information */
    size_t (*init)(const struct H5O_layout_t *layout, const H5P_t *mem_space,
		   const H5P_t *file_space, H5P_number_t *numbering/*out*/);

    /* Gather elements from disk to type conversion buffer */
    size_t (*fgath)(H5F_t *f, const struct H5O_layout_t *layout,
		    size_t elmt_size, const H5P_t *file_space,
		    const H5P_number_t *numbering, intn start, intn nelmts,
		    void *tconv_buf/*out*/);

    /* Scatter elements from type conversion buffer to application buffer */
    herr_t (*mscat)(const void *tconv_buf, size_t elmt_size,
		    const H5P_t *mem_space, const H5P_number_t *numbering,
		    intn start, intn nelmts, void *buf/*out*/);

    /* Gather elements from app buffer to type conversion buffer */
    size_t (*mgath)(const void *buf, size_t elmt_size,
		    const H5P_t *mem_space, const H5P_number_t *numbering,
		    intn start, intn nelmts, void *tconv_buf/*out*/);

    /* Scatter elements from type conversion buffer to disk */
    herr_t (*fscat)(H5F_t *f, const struct H5O_layout_t *layout,
		    size_t elmt_size, const H5P_t *file_space,
		    const H5P_number_t *numbering, intn start, intn nelmts,
		    const void *tconv_buf);
} H5P_conv_t;

H5P_t *H5P_copy (const H5P_t *src);
herr_t H5P_close (H5P_t *ds);
size_t H5P_get_npoints (const H5P_t *ds);
intn H5P_get_ndims (const H5P_t *ds);
intn H5P_get_dims (const H5P_t *ds, size_t dims[]/*out*/);
herr_t H5P_modify (H5F_t *f, H5G_entry_t *ent, const H5P_t *space);
H5P_t *H5P_read (H5F_t *f, H5G_entry_t *ent);
intn H5P_cmp (const H5P_t *ds1, const H5P_t *ds2);
hbool_t H5P_is_simple (const H5P_t *sdim);
uintn H5P_nelem (const H5P_t *space);
const H5P_conv_t *H5P_find (const H5P_t *mem_space, const H5P_t *file_space);

/* Conversion functions for simple data spaces */
size_t H5P_simp_init (const struct H5O_layout_t *layout,
		      const H5P_t *mem_space, const H5P_t *file_space,
		      H5P_number_t *numbering/*out*/);
size_t H5P_simp_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		       size_t elmt_size, const H5P_t *file_space,
		       const H5P_number_t *numbering, intn start, intn nelmts,
		       void *tconv_buf/*out*/);
herr_t H5P_simp_mscat (const void *tconv_buf, size_t elmt_size,
		       const H5P_t *mem_space, const H5P_number_t *numbering,
		       intn start, intn nelmts, void *buf/*out*/);
size_t H5P_simp_mgath (const void *buf, size_t elmt_size,
		       const H5P_t *mem_space, const H5P_number_t *numbering,
		       intn start, intn nelmts, void *tconv_buf/*out*/);
herr_t H5P_simp_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		       size_t elmt_size, const H5P_t *file_space,
		       const H5P_number_t *numbering, intn start, intn nelmts,
		       const void *tconv_buf);

#endif
