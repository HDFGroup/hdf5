/*
 * Copyright (C) 2000 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *		Thursday, September 28, 2000
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5S package.  Source files outside the H5S package should
 *		include H5Sprivate.h instead.
 */
#ifndef H5S_PACKAGE
#error "Do not include this file outside the H5S package!"
#endif

#ifndef _H5Spkg_H
#define _H5Spkg_H

#include <H5Sprivate.h>

/* 
 * Dataspace extent information
 */
/* Simple extent container */
typedef struct H5S_simple_t {
    unsigned rank;         /* Number of dimensions */
    hsize_t *size;      /* Current size of the dimensions */
    hsize_t *max;       /* Maximum size of the dimensions */
#ifdef LATER
    hsize_t *perm;      /* Dimension permutation array */
#endif /* LATER */
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
/* Node in point selection list (typedef'd in H5Sprivate.h) */
struct H5S_pnt_node_t {
    hssize_t *pnt;          /* Pointer to a selected point */
    struct H5S_pnt_node_t *next;  /* pointer to next point in list */
};

/* Information about point selection list */
typedef struct {
    H5S_pnt_node_t *head;   /* Pointer to head of point list */
} H5S_pnt_list_t;

/* Information about new-style hyperslab spans */

/* Information a particular hyperslab span */
struct H5S_hyper_span_t {
    hssize_t low, high;         /* Low & high bounds of span */
    hsize_t nelem;              /* Number of elements in span (only needed during I/O) */
    hsize_t pstride;            /* Pseudo-stride from start of previous span (only used during I/O) */
    struct H5S_hyper_span_info_t *down;     /* Pointer to list of spans in next dimension down */
    struct H5S_hyper_span_t *next;     /* Pointer to next span in list */
};

/* Information about a list of hyperslab spans */
struct H5S_hyper_span_info_t {
    unsigned count;                    /* Ref. count of number of spans which share this span */
    struct H5S_hyper_span_info_t *scratch;  /* Scratch pointer (used during copies & as mark during precomputes for I/O) */
    struct H5S_hyper_span_t *head;  /* Pointer to list of spans in next dimension down */
};

/* Information about one dimension in a hyperslab selection */
typedef struct {
    hssize_t start;
    hsize_t  stride;
    hsize_t  count;
    hsize_t  block;
} H5S_hyper_dim_t;

/* Information about new-style hyperslab selection */
typedef struct {
    H5S_hyper_dim_t *diminfo;    /* ->[rank] of per-dim selection info */
	/* diminfo only points to one array, which holds the information
	 * for one hyperslab selection. Perhaps this might need to be
	 * expanded into a list of arrays when the H5Sselect_hyperslab's
	 * restriction to H5S_SELECT_SET is removed. */
    H5S_hyper_dim_t *app_diminfo;/* ->[rank] of per-dim selection info */
	/* 'diminfo' points to a [potentially] optimized version of the user's
         * hyperslab information.  'app_diminfo' points to the actual parameters
         * that the application used for setting the hyperslab selection.  These
         * are only used for re-gurgitating the original values used to set the
         * hyperslab to the application when it queries the hyperslab selection
         * information. */
    H5S_hyper_span_info_t *span_lst; /* List of hyperslab span information */
} H5S_hyper_sel_t;

/* Selection information container */
typedef struct {
    H5S_sel_type type;  /* Type of selection (list of points or hyperslabs) */
    hssize_t *offset;   /* Offset within the extent (NULL means a 0 offset) */
    hsize_t *order;     /* Selection order.  (NULL means a specific ordering of points) */
    hsize_t num_elem;   /* Number of elements in selection */
    union {
        H5S_pnt_list_t *pnt_lst; /* List of selected points (order is important) */
        H5S_hyper_sel_t hslab;   /* Info about new-style hyperslab selections */
    } sel_info;
} H5S_select_t;

/* Main dataspace structure (typedef'd in H5Sprivate.h) */
struct H5S_t {
    H5S_extent_t extent;        /* Dataspace extent */
    H5S_select_t select;		/* Dataspace selection */
};

__DLL__ herr_t H5S_close_simple(H5S_simple_t *simple);
__DLL__ herr_t H5S_release_simple(H5S_simple_t *simple);
__DLL__ herr_t H5S_extent_copy(H5S_extent_t *dst, const H5S_extent_t *src);
__DLL__ herr_t H5S_register(H5S_sel_type cls, const H5S_fconv_t *fconv,
			    const H5S_mconv_t *mconv);

/* Point select functions */
__DLL__ herr_t H5S_point_add(H5S_t *space, H5S_seloper_t op, size_t num_elem,
			     const hssize_t **coord);
__DLL__ herr_t H5S_point_release(H5S_t *space);
__DLL__ hsize_t H5S_point_npoints(const H5S_t *space);
__DLL__ herr_t H5S_point_copy(H5S_t *dst, const H5S_t *src);
__DLL__ htri_t H5S_point_select_valid(const H5S_t *space);
__DLL__ hssize_t H5S_point_select_serial_size(const H5S_t *space);
__DLL__ herr_t H5S_point_select_serialize(const H5S_t *space, uint8_t *buf);
__DLL__ herr_t H5S_point_select_deserialize(H5S_t *space, const uint8_t *buf);
__DLL__ herr_t H5S_point_bounds(H5S_t *space, hsize_t *start, hsize_t *end);
__DLL__ htri_t H5S_point_select_contiguous(const H5S_t *space);
__DLL__ htri_t H5S_point_select_single(const H5S_t *space);
__DLL__ herr_t H5S_select_elements (H5S_t *space, H5S_seloper_t op,
            size_t num_elem, const hssize_t **coord);
__DLL__ herr_t H5S_point_select_iterate(void *buf, hid_t type_id, H5S_t *space,
					H5D_operator_t op,
					void *operator_data);

/* "All" select functions */
__DLL__ herr_t H5S_all_release(H5S_t *space);
__DLL__ hsize_t H5S_all_npoints(const H5S_t *space);
__DLL__ herr_t H5S_all_select_serialize(const H5S_t *space, uint8_t *buf);
__DLL__ herr_t H5S_all_select_deserialize(H5S_t *space, const uint8_t *buf);
__DLL__ herr_t H5S_all_bounds(H5S_t *space, hsize_t *start, hsize_t *end);
__DLL__ herr_t H5S_all_read(H5F_t *f, const struct H5O_layout_t *layout,
			    const struct H5O_pline_t *pline,
                            const struct H5O_fill_t *fill,
			    const struct H5O_efl_t *efl, size_t elmt_size,
			    const H5S_t *file_space, const H5S_t *mem_space,
			    hid_t dxpl_id, void *buf/*out*/,
			    hbool_t *must_convert/*out*/);
__DLL__ herr_t H5S_all_write(H5F_t *f, const struct H5O_layout_t *layout,
			    const struct H5O_pline_t *pline,
                            const struct H5O_fill_t *fill,
			    const struct H5O_efl_t *efl, size_t elmt_size,
			    const H5S_t *file_space, const H5S_t *mem_space,
			    hid_t dxpl_id, const void *buf,
			    hbool_t *must_convert/*out*/);
__DLL__ herr_t H5S_all_select_iterate(void *buf, hid_t type_id, H5S_t *space,
				      H5D_operator_t op, void *operator_data);

/* Hyperslab selection functions */
__DLL__ herr_t H5S_hyper_release(H5S_t *space);
__DLL__ herr_t H5S_hyper_sel_iter_release(H5S_sel_iter_t *sel_iter);
__DLL__ hsize_t H5S_hyper_npoints(const H5S_t *space);
__DLL__ herr_t H5S_hyper_copy(H5S_t *dst, const H5S_t *src);
__DLL__ htri_t H5S_hyper_select_valid(const H5S_t *space);
__DLL__ hssize_t H5S_hyper_select_serial_size(const H5S_t *space);
__DLL__ herr_t H5S_hyper_select_serialize(const H5S_t *space, uint8_t *buf);
__DLL__ herr_t H5S_hyper_select_deserialize(H5S_t *space, const uint8_t *buf);
__DLL__ hssize_t H5S_hyper_span_nblocks(H5S_hyper_span_info_t *spans);
__DLL__ herr_t H5S_hyper_span_blocklist(H5S_hyper_span_info_t *spans, hssize_t start[], hssize_t end[], hsize_t rank, hsize_t *startblock, hsize_t *numblocks, hsize_t **buf);
__DLL__ herr_t H5S_hyper_bounds(H5S_t *space, hsize_t *start, hsize_t *end);
__DLL__ htri_t H5S_hyper_select_contiguous(const H5S_t *space);
__DLL__ htri_t H5S_hyper_select_single(const H5S_t *space);
__DLL__ herr_t H5S_hyper_select_iterate(void *buf, hid_t type_id, H5S_t *space,
                H5D_operator_t op, void *operator_data);

/* "None" selection functions */
__DLL__ herr_t H5S_select_none(H5S_t *space);
__DLL__ herr_t H5S_none_select_serialize(const H5S_t *space, uint8_t *buf);
__DLL__ herr_t H5S_none_select_deserialize(H5S_t *space, const uint8_t *buf);
__DLL__ herr_t H5S_none_select_iterate(void *buf, hid_t type_id, H5S_t *space,
				       H5D_operator_t op, void *operator_data);

#endif
