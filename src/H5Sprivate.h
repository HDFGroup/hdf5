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

/* 
 * Dataspace extent information
 */
/* Simple extent container */
typedef struct H5S_simple_t {
    intn rank;          /* Number of dimensions */
    hsize_t *size;      /* Current size of the dimensions */
    hsize_t *max;       /* Maximum size of the dimensions */
} H5S_simple_t;

/* Extent container */
typedef struct {
    H5S_class_t	type;		    /* Type of extent */
    union {
        H5S_simple_t	simple;	/* Simple dimensionality information  */
    } u;
} H5S_extent_t;

/* 
 * Dataspace selection information
 */
/* Enumerated type for the type of selection */
typedef enum {
    H5S_SEL_NONE,           /* Nothing selected */
    H5S_SEL_POINTS,         /* Sequence of points selected */
    H5S_SEL_HYPERSLABS,     /* Hyperslab selection defined */
    H5S_SEL_ALL             /* Entire extent selected */
}H5S_sel_type;

/* Node in point selection list */
typedef struct H5S_pnt_node_tag {
    hssize_t *pnt;          /* Pointer to a selected point */
    struct H5S_pnt_node_tag *next;  /* pointer to next point in list */
} H5S_pnt_node_t;

/* Information about point selection list */
typedef struct {
    H5S_pnt_node_t *head;   /* Pointer to head of point list */
} H5S_pnt_list_t;

/* Region in dimension */
typedef struct H5S_hyper_region_tag {
    hssize_t start;    /* The low bound of a region in a dimension */
    hssize_t end;      /* The high bound of a region in a dimension */
} H5S_hyper_region_t;

/* Node in hyperslab selection list */
typedef struct H5S_hyper_node_tag {
    hssize_t *start;   /* Pointer to a corner of a hyperslab closest to the origin */
    hssize_t *end;     /* Pointer to a corner of a hyperslab furthest from the origin */
    struct H5S_hyper_node_tag *next;  /* pointer to next hyperslab in list */
} H5S_hyper_node_t;

/* Information about hyperslab boundary and pointer to hyperslab node */
typedef struct {
    hssize_t bound;         /* Location of boundary */
    H5S_hyper_node_t *node; /* Boundary's node */
} H5S_hyper_bound_t;

/* Information about hyperslab selection */
typedef struct {
    size_t count;               /* Number of nodes in list */
    H5S_hyper_node_t *head;     /* Pointer to head of hyperslab list */
    H5S_hyper_bound_t **lo_bounds;    /* Lower (closest to the origin) bound array for each dimension */
    H5S_hyper_bound_t **hi_bounds;    /* Upper (farthest from the origin) bound array for each dimension */
} H5S_hyper_list_t;

/* Selection information container */
typedef struct {
    H5S_sel_type type;  /* Type of selection (list of points or hyperslabs) */
    hsize_t *offset;    /* Offset within the extent (NULL means a 0 offset) */
    hsize_t *order;     /* Selection order.  (NULL means a specific ordering of points) */
    hsize_t num_elem;   /* Number of elements in selection */
    union {
        H5S_pnt_list_t *pnt_lst;    /* List of selected points (order is important) */
        H5S_hyper_list_t *hyper_lst;    /* List of selected hyperslabs (order is not important) */
    } sel_info;
} H5S_select_t;

/* Point selection iteration container */
typedef struct {
    hsize_t elmt_left;      /* Number of elements left to iterate over */
    H5S_pnt_node_t *curr;   /* Pointer to next node to output */
} H5S_point_iter_t;

/* Hyperslab selection iteration container */
typedef struct {
    hsize_t elmt_left;      /* Number of elements left to iterate over */
    hssize_t *pos;          /* Position to start iterating at */
} H5S_hyper_iter_t;

/* "All" selection iteration container */
typedef struct {
    hsize_t elmt_left;      /* Number of elements left to iterate over */
    hsize_t offset;         /* Next element to output */
} H5S_all_iter_t;

/* Selection iteration container */
typedef union {
    H5S_point_iter_t pnt;   /* Point selection iteration information */
    H5S_hyper_iter_t hyp;   /* Hyperslab selection iteration information */
    H5S_all_iter_t all;     /* "All" selection iteration information */
} H5S_sel_iter_t;

/* Main dataspace structure */
typedef struct H5S_t {
    H5S_extent_t extent;        /* Dataspace extent */
    H5S_select_t select;		/* Dataspace selection */
} H5S_t;

/*
 * Callbacks for data space conversion.
 */
typedef struct H5S_tconv_t {
    /* Initialize file element numbering information */
    herr_t (*finit)(const struct H5O_layout_t *layout, const H5S_t *space,
        H5S_sel_iter_t *iter);

    /* Initialize memory element numbering information */
    herr_t (*minit)(const struct H5O_layout_t *layout, const H5S_t *space,
        H5S_sel_iter_t *iter);

    /* Initialize background element numbering information */
    herr_t (*binit)(const struct H5O_layout_t *layout, const H5S_t *space,
        H5S_sel_iter_t *iter);

    /* Figure out the optimal number of elements to transfer to/from the file */
    size_t (*favail)(const H5S_t *file_space, const H5S_sel_iter_t *file_iter,
		    size_t max);

    /* Gather elements from disk to type conversion buffer */
    size_t (*fgath)(H5F_t *f, const struct H5O_layout_t *layout,
		    const struct H5O_compress_t *comp,
		    const struct H5O_efl_t *efl, size_t elmt_size,
		    const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		    size_t nelmts,
		    const H5D_transfer_t xfer_mode, void *tconv_buf/*out*/);

    /* Scatter elements from type conversion buffer to disk */
    herr_t (*fscat)(H5F_t *f, const struct H5O_layout_t *layout,
		    const struct H5O_compress_t *compress,
		    const struct H5O_efl_t *efl, size_t elmt_size,
		    const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		    size_t nelmts,
		    const H5D_transfer_t xfer_mode, const void *tconv_buf);

    /* Gather elements from app buffer to type conversion buffer */
    size_t (*mgath)(const void *buf, size_t elmt_size,
		    const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		    size_t nelmts, void *tconv_buf/*out*/);

    /* Scatter elements from type conversion buffer to application buffer */
    herr_t (*mscat)(const void *tconv_buf, size_t elmt_size,
		    const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		    size_t nelmts, void *buf/*out*/);

    /* Read from file to application w/o intermediate scratch buffer */
    herr_t (*read)(H5F_t *f, const struct H5O_layout_t *layout,
		   const struct H5O_compress_t *comp,
		   const struct H5O_efl_t *efl, size_t elmt_size,
		   const H5S_t *file_space, const H5S_t *mem_space,
		   const H5D_transfer_t xfer_mode, void *buf/*out*/);

    /* Write directly from app buffer to file */
    herr_t (*write)(H5F_t *f, const struct H5O_layout_t *layout,
		    const struct H5O_compress_t *comp,
		    const struct H5O_efl_t *efl, size_t elmt_size,
		    const H5S_t *file_space, const H5S_t *mem_space,
		    const H5D_transfer_t xfer_mode, const void *buf);
} H5S_conv_t;

H5S_t *H5S_create (H5S_class_t type);
H5S_t *H5S_copy (const H5S_t *src);
herr_t H5S_close_simple (H5S_simple_t *simple);
herr_t H5S_close (H5S_t *ds);
hsize_t H5S_extent_npoints (const H5S_t *ds);
hsize_t H5S_get_npoints_max(const H5S_t *ds);
intn H5S_get_ndims (const H5S_t *ds);
intn H5S_get_dims (const H5S_t *ds, hsize_t dims[]/*out*/,
		   hsize_t max_dims[]/*out*/);
herr_t H5S_modify (H5G_entry_t *ent, const H5S_t *space);
H5S_t *H5S_read (H5G_entry_t *ent);
intn H5S_cmp (const H5S_t *ds1, const H5S_t *ds2);
hbool_t H5S_is_simple (const H5S_t *sdim);
uintn H5S_nelem (const H5S_t *space);
herr_t H5S_find (H5S_conv_t *conv, const H5S_t *mem_space, const H5S_t *file_space);
intn H5S_get_hyperslab (const H5S_t *ds, hssize_t offset[]/*out*/,
			hsize_t size[]/*out*/, hsize_t stride[]/*out*/);
herr_t H5S_release_simple(H5S_simple_t *simple);
herr_t H5S_extent_copy(H5S_extent_t *dst, const H5S_extent_t *src);
herr_t H5S_select_copy (H5S_t *dst, const H5S_t *src);
herr_t H5S_select_release (H5S_t *space);
herr_t H5S_sel_iter_release (const H5S_t *space,H5S_sel_iter_t *sel_iter);
hsize_t H5S_select_npoints (const H5S_t *space);
intn H5S_extend (H5S_t *space, const hsize_t *size);
herr_t H5S_set_extent_simple (H5S_t *space, int rank, const hsize_t *dims,
			      const hsize_t *max);

/* Conversion functions for simple data spaces */
size_t H5S_simp_init (const struct H5O_layout_t *layout,
		      const H5S_t *mem_space, const H5S_t *file_space,
		      size_t desired_nelmts);
size_t H5S_simp_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		       const struct H5O_compress_t *comp,
		       const struct H5O_efl_t *efl, size_t elmt_size,
		       const H5S_t *file_space,
		       size_t start, size_t nelmts,
		       const H5D_transfer_t xfer_mode, void *tconv_buf/*out*/);
herr_t H5S_simp_mscat (const void *tconv_buf, size_t elmt_size,
		       const H5S_t *mem_space,
		       size_t start, size_t nelmts, void *buf/*out*/);
size_t H5S_simp_mgath (const void *buf, size_t elmt_size,
		       const H5S_t *mem_space,
		       size_t start, size_t nelmts, void *tconv_buf/*out*/);
herr_t H5S_simp_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		       const struct H5O_compress_t *comp,
		       const struct H5O_efl_t *efl, size_t elmt_size,
		       const H5S_t *file_space,
		       size_t start, size_t nelmts,
		       const H5D_transfer_t xfer_mode, const void *tconv_buf);
herr_t H5S_simp_read (H5F_t *f, const struct H5O_layout_t *layout,
		      const struct H5O_compress_t *comp,
		      const struct H5O_efl_t *efl, size_t elmt_size,
		      const H5S_t *file_space, const H5S_t *mem_space,
		      const H5D_transfer_t xfer_mode, void *buf/*out*/);
herr_t H5S_simp_write (H5F_t *f, const struct H5O_layout_t *layout,
		       const struct H5O_compress_t *comp,
		       const struct H5O_efl_t *efl, size_t elmt_size,
		       const H5S_t *file_space, const H5S_t *mem_space,
		       const H5D_transfer_t xfer_mode, const void *buf);

/* Point select functions */
herr_t H5S_point_init (const struct H5O_layout_t *layout,
		      const H5S_t *space, H5S_sel_iter_t *iter);
size_t H5S_point_favail (const H5S_t *space, const H5S_sel_iter_t *iter,
                size_t max);
size_t H5S_point_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
		size_t elmt_size, const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		size_t nelmts,
		const H5D_transfer_t xfer_mode, void *buf/*out*/);
herr_t H5S_point_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
		size_t elmt_size, const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		size_t nelmts,
		const H5D_transfer_t xfer_mode, const void *buf);
size_t H5S_point_mgath (const void *_buf, size_t elmt_size,
		const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		size_t nelmts, void *_tconv_buf/*out*/);
herr_t H5S_point_mscat (const void *_tconv_buf, size_t elmt_size,
		const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		size_t nelmts, void *_buf/*out*/);
herr_t H5S_point_add (H5S_t *space, size_t num_elemn, const hssize_t *coord[]);
herr_t H5S_point_release (H5S_t *space);
hsize_t H5S_point_npoints (const H5S_t *space);

/* "All" select functions */
herr_t H5S_all_init (const struct H5O_layout_t *layout,
		      const H5S_t *space, H5S_sel_iter_t *iter);
size_t H5S_all_favail (const H5S_t *space, const H5S_sel_iter_t *iter,
                size_t max);
size_t H5S_all_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
		size_t elmt_size, const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		size_t nelmts,
		const H5D_transfer_t xfer_mode, void *buf/*out*/);
herr_t H5S_all_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
		size_t elmt_size, const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		size_t nelmts,
		const H5D_transfer_t xfer_mode, const void *buf);
size_t H5S_all_mgath (const void *_buf, size_t elmt_size,
		const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		size_t nelmts, void *_tconv_buf/*out*/);
herr_t H5S_all_mscat (const void *_tconv_buf, size_t elmt_size,
		const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		size_t nelmts, void *_buf/*out*/);
herr_t H5S_all_release (H5S_t *space);
hsize_t H5S_all_npoints (const H5S_t *space);

/* Hyperslab selection functions */
herr_t H5S_hyper_init (const struct H5O_layout_t *layout,
		      const H5S_t *space, H5S_sel_iter_t *iter);
size_t H5S_hyper_favail (const H5S_t *space, const H5S_sel_iter_t *iter,
                size_t max);
size_t H5S_hyper_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
		size_t elmt_size, const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		size_t nelmts,
		const H5D_transfer_t xfer_mode, void *buf/*out*/);
herr_t H5S_hyper_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
		size_t elmt_size, const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		size_t nelmts,
		const H5D_transfer_t xfer_mode, const void *buf);
size_t H5S_hyper_mgath (const void *_buf, size_t elmt_size,
		const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		size_t nelmts, void *_tconv_buf/*out*/);
herr_t H5S_hyper_mscat (const void *_tconv_buf, size_t elmt_size,
		const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		size_t nelmts, void *_buf/*out*/);
herr_t H5S_hyper_add (H5S_t *space, const hssize_t *start, const hsize_t *size);
herr_t H5S_hyper_release (H5S_t *space);
herr_t H5S_hyper_sel_iter_release (H5S_sel_iter_t *sel_iter);
hsize_t H5S_hyper_npoints (const H5S_t *space);

#endif
