/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

/*
 * This file contains private information about the H5S module
 */
#ifndef _H5Sprivate_H
#define _H5Sprivate_H

#include <H5Spublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Gprivate.h>		/*for H5G_entry_t			     */
#include <H5Oprivate.h>

#define H5S_RESERVED_ATOMS  2

/* Flags to indicate special dataspace features are active */
#define H5S_VALID_MAX	0x01
#define H5S_VALID_PERM	0x02

typedef struct H5S_hyperslab_t {
    hssize_t	*start;			/* Location of start of hyperslab */
    hsize_t	*count;			/* Number of elements in hyperslab */
    hsize_t	*stride;		/* Packing of values of hyperslab */
} H5S_hyperslab_t;

typedef struct H5S_simple_t {
    intn	rank;			/*number of dimensions		     */
    hsize_t	*size;			/*dimension sizes		     */
    hsize_t	*max;			/*maximum dimension sizes or NULL    */
    intn	*perm;			/*dimension permutations or NULL     */
} H5S_simple_t;

typedef struct H5S_t {
    H5S_class_t		type;		/*type of dimensionality object	     */
    union {
	H5S_simple_t	simple;		/*simple dimensionality information  */
    } u;
    uintn hslab_def;			/* Whether the hyperslab is defined */
    H5S_hyperslab_t h;			/* Hyperslab information */
} H5S_t;

/*
 * This structure contains information about how the elements of a data space
 * are numbered.
 */
typedef struct H5S_number_t {
    int _place_holder;			/*remove this field!		     */
} H5S_number_t;

/*
 * Callbacks for data space conversion.
 */
typedef struct H5S_tconv_t {
    /* Initialize element numbering information */
    size_t (*init)(const struct H5O_layout_t *layout, const H5S_t *mem_space,
		   const H5S_t *file_space, size_t desired_nelmts,
		   H5S_number_t *numbering/*out*/);

    /* Gather elements from disk to type conversion buffer */
    size_t (*fgath)(H5F_t *f, const struct H5O_layout_t *layout,
		    const struct H5O_efl_t *efl, size_t elmt_size,
		    const H5S_t *file_space, const H5S_number_t *numbering,
		    size_t start, size_t nelmts, void *tconv_buf/*out*/);

    /* Scatter elements from type conversion buffer to application buffer */
    herr_t (*mscat)(const void *tconv_buf, size_t elmt_size,
		    const H5S_t *mem_space, const H5S_number_t *numbering,
		    size_t start, size_t nelmts, void *buf/*out*/);

    /* Gather elements from app buffer to type conversion buffer */
    size_t (*mgath)(const void *buf, size_t elmt_size,
		    const H5S_t *mem_space, const H5S_number_t *numbering,
		    size_t start, size_t nelmts, void *tconv_buf/*out*/);

    /* Scatter elements from type conversion buffer to disk */
    herr_t (*fscat)(H5F_t *f, const struct H5O_layout_t *layout,
		    const struct H5O_efl_t *efl, size_t elmt_size,
		    const H5S_t *file_space, const H5S_number_t *numbering,
		    size_t start, size_t nelmts, const void *tconv_buf);

    /* Read from file to application w/o intermediate scratch buffer */
    herr_t (*read)(H5F_t *f, const struct H5O_layout_t *layout,
		   const struct H5O_efl_t *efl, size_t elmt_size,
		   const H5S_t *file_space, const H5S_t *mem_space,
		   void *buf/*out*/);

    /* Write directly from app buffer to file */
    herr_t (*write)(H5F_t *f, const struct H5O_layout_t *layout,
		    const struct H5O_efl_t *efl, size_t elmt_size,
		    const H5S_t *file_space, const H5S_t *mem_space,
		    const void *buf);
} H5S_conv_t;

H5S_t *H5S_copy (const H5S_t *src);
herr_t H5S_close (H5S_t *ds);
hsize_t H5S_get_npoints (const H5S_t *ds);
hsize_t H5S_get_npoints_max(const H5S_t *ds);
intn H5S_get_ndims (const H5S_t *ds);
intn H5S_get_dims (const H5S_t *ds, hsize_t dims[]/*out*/,
		   hsize_t max_dims[]/*out*/);
herr_t H5S_modify (H5G_entry_t *ent, const H5S_t *space);
H5S_t *H5S_read (H5F_t *f, H5G_entry_t *ent);
intn H5S_cmp (const H5S_t *ds1, const H5S_t *ds2);
hbool_t H5S_is_simple (const H5S_t *sdim);
uintn H5S_nelem (const H5S_t *space);
const H5S_conv_t *H5S_find (const H5S_t *mem_space, const H5S_t *file_space);
intn H5S_get_hyperslab (const H5S_t *ds, hssize_t offset[]/*out*/,
			hsize_t size[]/*out*/, hsize_t stride[]/*out*/);
intn H5S_extend (H5S_t *space, const hsize_t *size);

/* Conversion functions for simple data spaces */
size_t H5S_simp_init (const struct H5O_layout_t *layout,
		      const H5S_t *mem_space, const H5S_t *file_space,
		      size_t desired_nelmts, H5S_number_t *numbering/*out*/);
size_t H5S_simp_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		       const struct H5O_efl_t *efl, size_t elmt_size,
		       const H5S_t *file_space, const H5S_number_t *numbering,
		       size_t start, size_t nelmts, void *tconv_buf/*out*/);
herr_t H5S_simp_mscat (const void *tconv_buf, size_t elmt_size,
		       const H5S_t *mem_space, const H5S_number_t *numbering,
		       size_t start, size_t nelmts, void *buf/*out*/);
size_t H5S_simp_mgath (const void *buf, size_t elmt_size,
		       const H5S_t *mem_space, const H5S_number_t *numbering,
		       size_t start, size_t nelmts, void *tconv_buf/*out*/);
herr_t H5S_simp_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		       const struct H5O_efl_t *efl, size_t elmt_size,
		       const H5S_t *file_space, const H5S_number_t *numbering,
		       size_t start, size_t nelmts, const void *tconv_buf);
herr_t H5S_simp_read (H5F_t *f, const struct H5O_layout_t *layout,
		      const struct H5O_efl_t *efl, size_t elmt_size,
		      const H5S_t *file_space, const H5S_t *mem_space,
		      void *buf/*out*/);
herr_t H5S_simp_write (H5F_t *f, const struct H5O_layout_t *layout,
		       const struct H5O_efl_t *efl, size_t elmt_size,
		       const H5S_t *file_space, const H5S_t *mem_space,
		       const void *buf);
#endif
