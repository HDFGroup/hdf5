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
#include <H5Fprivate.h>
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
    H5S_SEL_ERROR	= -1, 		/* Error			*/
    H5S_SEL_NONE	= 0,           	/* Nothing selected 		*/
    H5S_SEL_POINTS	= 1,         	/* Sequence of points selected	*/
    H5S_SEL_HYPERSLABS	= 2,     	/* Hyperslab selection defined	*/
    H5S_SEL_ALL		= 3,            /* Entire extent selected	*/
    H5S_SEL_N		= 4		/*THIS MUST BE LAST		*/
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

/* Node in hyperslab selection list */
typedef struct H5S_hyper_node_tag {
    hssize_t *start;    /* Pointer to a corner of a hyperslab closest to the origin */
    hssize_t *end;      /* Pointer to a corner of a hyperslab furthest from the origin */
    struct {
        uintn cached;   /* Flag to indicate that the block is cached (during I/O only) */
        uintn size;     /* Size of cached block (in elements) */
        uintn left;     /* Elements left to access in block */
        hid_t block_id; /* Temporary buffer ID */
        uint8 *block;   /* Pointer into temporary buffer for cache */
        uint8 *pos;     /* Pointer to current location within block */
    } cinfo;
    struct H5S_hyper_node_tag *next;  /* pointer to next hyperslab in list */
} H5S_hyper_node_t;

/* Region in dimension */
typedef struct H5S_hyper_region_tag {
    hssize_t start;    /* The low bound of a region in a dimension */
    hssize_t end;      /* The high bound of a region in a dimension */
    H5S_hyper_node_t *node; /* pointer to the node the region is in */
} H5S_hyper_region_t;

/* Information about hyperslab boundary and pointer to hyperslab node */
typedef struct {
    hssize_t bound;         /* Location of boundary */
    H5S_hyper_node_t *node; /* Boundary's node */
} H5S_hyper_bound_t;

/* Information about hyperslab list */
typedef struct {
    size_t count;               /* Number of nodes in list */
    H5S_hyper_node_t *head;     /* Pointer to head of hyperslab list */
    H5S_hyper_bound_t **lo_bounds;    /* Lower (closest to the origin) bound array for each dimension */
    H5S_hyper_bound_t **hi_bounds;    /* Upper (farthest from the origin) bound array for each dimension */
} H5S_hyper_list_t;

/* Information about one dimension in a hyperslab selection */
typedef struct {
    hssize_t start;
    hsize_t  stride;
    hsize_t  count;
    hsize_t  block;
} H5S_hyper_dim_t;

/* Information about hyperslab selection */
typedef struct {
    H5S_hyper_dim_t *diminfo;    /* ->[rank] of per-dim selection info */
	/* diminfo only points to one array, which holds the information
	 * for one hyperslab selection. Perhaps this might need to be
	 * expanded into a list of arrays when the H5Sselect_hyperslab's
	 * restriction to H5S_SELECT_SET is removed. */
    H5S_hyper_list_t *hyper_lst; /* List of selected hyperslabs (order is not important) */
} H5S_hyper_sel_t;

/* Selection information container */
typedef struct {
    H5S_sel_type type;  /* Type of selection (list of points or hyperslabs) */
    hssize_t *offset;   /* Offset within the extent (NULL means a 0 offset) */
    hsize_t *order;     /* Selection order.  (NULL means a specific ordering of points) */
    hsize_t num_elem;   /* Number of elements in selection */
    union {
        H5S_pnt_list_t *pnt_lst; /* List of selected points (order is important) */
        H5S_hyper_sel_t hyper;   /* Info about hyperslab selections */
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
 * Data space conversions usually take place in two halves.  One half
 * transfers data points between memory and a data type conversion array
 * where the points are contiguous, and the other half transfers points
 * between the type conversion array and the file.
 */
typedef struct H5S_fconv_t {
    /* Identification */
    const char 		*name;
    H5S_sel_type	type;
    
    /* Initialize file element numbering information */
    herr_t (*init)(const struct H5O_layout_t *layout, const H5S_t *space,
		   H5S_sel_iter_t *iter);

    /* Determine optimal number of elements to transfer */
    size_t (*avail)(const H5S_t *file_space, const H5S_sel_iter_t *file_iter,
		    size_t max);

    /* Gather elements from disk to type conversion buffer */
    size_t (*gath)(H5F_t *f, const struct H5O_layout_t *layout,
		   const struct H5O_pline_t *pline,
		   const struct H5O_fill_t *fill,
		   const struct H5O_efl_t *efl, size_t elmt_size,
		   const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		   size_t nelmts, const struct H5D_xfer_t *xfer_parms,
		   void *tconv_buf/*out*/);

    /* Scatter elements from type conversion buffer to disk */
    herr_t (*scat)(H5F_t *f, const struct H5O_layout_t *layout,
		   const struct H5O_pline_t *pline,
		   const struct H5O_fill_t *fill,
		   const struct H5O_efl_t *efl, size_t elmt_size,
		   const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		   size_t nelmts, const struct H5D_xfer_t *xfer_parms,
		   const void *tconv_buf);
} H5S_fconv_t;

typedef struct H5S_mconv_t {
    /* Identification */
    const char		*name;
    H5S_sel_type	type;
    
    /* Initialize memory element numbering information */
    herr_t (*init)(const struct H5O_layout_t *layout, const H5S_t *space,
		   H5S_sel_iter_t *iter);

    /* Gather elements from app buffer to type conversion buffer */
    size_t (*gath)(const void *buf, size_t elmt_size,
		   const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		   size_t nelmts, void *tconv_buf/*out*/);

    /* Scatter elements from type conversion buffer to application buffer */
    herr_t (*scat)(const void *tconv_buf, size_t elmt_size,
		   const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		   size_t nelmts, void *buf/*out*/);
} H5S_mconv_t;

typedef struct H5S_conv_t {
    const H5S_fconv_t	*f;
    const H5S_mconv_t	*m;

    /*
     * If there is no data type conversion then it might be possible to
     * transfer data points between application memory and the file in one
     * step without going through the data type conversion buffer.
     *
     * rky 980918
     * If the direct read or write function determines that the transfer
     * must be done indirectly, i.e., through the conversion buffer or
     * (in the case of parallel MPI-IO) in block-by-block transfers
     * then the function returns with the value of must_convert!=0,
     * the function's return value is SUCCEED,
     * and no transfer of data is attempted.
     * Otherwise the direct read or write function returns must_convert 0,
     * with the function's return value being SUCCEED or FAIL
     * depending on whether or not the transfer of data was successful.
     */
    
    /* Read from file to application w/o intermediate scratch buffer */
    herr_t (*read)(H5F_t *f, const struct H5O_layout_t *layout,
		   const struct H5O_pline_t *pline,
		   const struct H5O_efl_t *efl, size_t elmt_size,
		   const H5S_t *file_space, const H5S_t *mem_space,
		   const H5D_transfer_t xfer_mode, void *buf/*out*/,
		   hbool_t *must_convert/*out*/ );


    /* Write directly from app buffer to file */
    herr_t (*write)(H5F_t *f, const struct H5O_layout_t *layout,
		    const struct H5O_pline_t *pline,
		    const struct H5O_efl_t *efl, size_t elmt_size,
		    const H5S_t *file_space, const H5S_t *mem_space,
		    const H5D_transfer_t xfer_mode, const void *buf,
		    hbool_t *must_convert/*out*/ );
    
#ifdef H5S_DEBUG
    struct {
	H5_timer_t	scat_timer;		/*time spent scattering	*/
	hsize_t		scat_nbytes;		/*scatter throughput	*/
	hsize_t		scat_ncalls;		/*number of calls	*/
	H5_timer_t	gath_timer;		/*time spent gathering	*/
	hsize_t		gath_nbytes;		/*gather throughput	*/
	hsize_t		gath_ncalls;		/*number of calls	*/
	H5_timer_t	bkg_timer;		/*time for background	*/
	hsize_t		bkg_nbytes;		/*background throughput	*/
	hsize_t		bkg_ncalls;		/*number of calls	*/
    } stats[2];		/* 0=output, 1=input */
#endif
} H5S_conv_t;

/* Conversion information for the various data space selection types */
extern const H5S_fconv_t	H5S_POINT_FCONV[];
extern const H5S_mconv_t	H5S_POINT_MCONV[];
extern const H5S_fconv_t	H5S_ALL_FCONV[];
extern const H5S_mconv_t	H5S_ALL_MCONV[];
extern const H5S_fconv_t	H5S_HYPER_FCONV[];
extern const H5S_mconv_t	H5S_HYPER_MCONV[];

H5S_t *H5S_create (H5S_class_t type);
H5S_t *H5S_copy (const H5S_t *src);
herr_t H5S_close_simple (H5S_simple_t *simple);
herr_t H5S_close (H5S_t *ds);
hsize_t H5S_get_simple_extent_npoints (const H5S_t *ds);
hsize_t H5S_get_npoints_max(const H5S_t *ds);
intn H5S_get_simple_extent_ndims (const H5S_t *ds);
intn H5S_get_simple_extent_dims (const H5S_t *ds, hsize_t dims[]/*out*/,
		   hsize_t max_dims[]/*out*/);
herr_t H5S_modify (H5G_entry_t *ent, const H5S_t *space);
H5S_t *H5S_read (H5G_entry_t *ent);
intn H5S_cmp (const H5S_t *ds1, const H5S_t *ds2);
hbool_t H5S_is_simple (const H5S_t *sdim);
uintn H5S_nelem (const H5S_t *space);
H5S_conv_t *H5S_find (const H5S_t *mem_space, const H5S_t *file_space);
herr_t H5S_select_hyperslab (H5S_t *space, H5S_seloper_t op,
			     const hssize_t start[],
			     const hsize_t _stride[],
			     const hsize_t count[],
			     const hsize_t _block[]);
intn H5S_get_hyperslab (const H5S_t *ds, hssize_t offset[]/*out*/,
			hsize_t size[]/*out*/, hsize_t stride[]/*out*/);
herr_t H5S_release_simple(H5S_simple_t *simple);
herr_t H5S_extent_copy(H5S_extent_t *dst, const H5S_extent_t *src);
herr_t H5S_select_copy (H5S_t *dst, const H5S_t *src);
herr_t H5S_extent_release (H5S_t *space);
herr_t H5S_select_release (H5S_t *space);
herr_t H5S_sel_iter_release (const H5S_t *space,H5S_sel_iter_t *sel_iter);
hssize_t H5S_get_select_npoints (const H5S_t *space);
intn H5S_extend (H5S_t *space, const hsize_t *size);
herr_t H5S_set_extent_simple (H5S_t *space, int rank, const hsize_t *dims,
			      const hsize_t *max);
hbool_t H5S_select_valid (const H5S_t *space);
herr_t H5S_debug(H5F_t *f, const void *_mesg, FILE *stream, intn indent,
		 intn fwidth);
herr_t H5S_register(H5S_sel_type cls, const H5S_fconv_t *fconv,
		    const H5S_mconv_t *mconv);

/* Point select functions */
herr_t H5S_point_add (H5S_t *space, size_t num_elemn, const hssize_t **coord);
herr_t H5S_point_release (H5S_t *space);
hsize_t H5S_point_npoints (const H5S_t *space);
herr_t H5S_point_copy (H5S_t *dst, const H5S_t *src);
hbool_t H5S_point_select_valid (const H5S_t *space);

/* "All" select functions */
herr_t H5S_all_release (H5S_t *space);
hsize_t H5S_all_npoints (const H5S_t *space);

/* Hyperslab selection functions */
herr_t H5S_hyper_add (H5S_t *space, const hssize_t *start, const hsize_t *end);
herr_t H5S_hyper_release (H5S_t *space);
herr_t H5S_hyper_sel_iter_release (H5S_sel_iter_t *sel_iter);
hsize_t H5S_hyper_npoints (const H5S_t *space);
int H5S_hyper_compare_regions (const void *r1, const void *r2);
int H5S_hyper_compare_bounds (const void *r1, const void *r2);
herr_t H5S_hyper_copy (H5S_t *dst, const H5S_t *src);
hbool_t H5S_hyper_select_valid (const H5S_t *space);
herr_t H5S_hyper_node_add (H5S_hyper_node_t **head, intn endflag, intn rank, const hssize_t *start, const hsize_t *size);
herr_t H5S_hyper_clip (H5S_t *space, H5S_hyper_node_t *nodes, H5S_hyper_node_t **uniq, H5S_hyper_node_t **overlap);

#ifdef HAVE_PARALLEL
    /* MPI-IO function to read directly from app buffer to file rky980813 */
    herr_t H5S_mpio_spaces_read (H5F_t *f, const struct H5O_layout_t *layout,
		   const struct H5O_pline_t *pline,
		   const struct H5O_efl_t *efl, size_t elmt_size,
		   const H5S_t *file_space, const H5S_t *mem_space,
		   const H5D_transfer_t xfer_mode, void *buf/*out*/,
                   hbool_t *must_convert /*out*/ );

    /* MPI-IO function to write directly from app buffer to file rky980813 */
    herr_t H5S_mpio_spaces_write(H5F_t *f, const struct H5O_layout_t *layout,
		    const struct H5O_pline_t *pline,
		    const struct H5O_efl_t *efl, size_t elmt_size,
		    const H5S_t *file_space, const H5S_t *mem_space,
		    const H5D_transfer_t xfer_mode, const void *buf,
                    hbool_t *must_convert /*out*/ );

#ifndef _H5S_IN_H5S_C
    /* Global var whose value comes from environment variable */
    extern hbool_t		H5_mpi_opt_types_g;
#endif /* _H5S_IN_H5S_C */

#endif

#endif
