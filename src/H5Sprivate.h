/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains private information about the H5S module
 */
#ifndef _H5Sprivate_H
#define _H5Sprivate_H

#include "H5Spublic.h"

/* Private headers needed by this file */
#include "H5private.h"
#include "H5Dpublic.h"
#include "H5Fprivate.h"
#include "H5Oprivate.h"
#include "H5Pprivate.h"

#define H5S_RESERVED_ATOMS  2

/* Flags to indicate special dataspace features are active */
#define H5S_VALID_MAX	0x01
#define H5S_VALID_PERM	0x02

/* Flags for H5S_find */
#define H5S_CONV_PAR_IO_POSSIBLE        0x0001
/* The storage options are mutually exclusive */
/* (2-bits reserved for storage type currently) */
#define H5S_CONV_STORAGE_COMPACT        0x0000  /* i.e. '0' */
#define H5S_CONV_STORAGE_CONTIGUOUS     0x0002  /* i.e. '1' */
#define H5S_CONV_STORAGE_CHUNKED        0x0004  /* i.e. '2' */
#define H5S_CONV_STORAGE_MASK           0x0006

/* Flags for "get_seq_list" methods */
#define H5S_GET_SEQ_LIST_SORTED         0x0001

/* Forward references of common typedefs */
typedef struct H5S_t H5S_t;
typedef struct H5S_pnt_node_t H5S_pnt_node_t;
typedef struct H5S_hyper_span_t H5S_hyper_span_t;
typedef struct H5S_hyper_span_info_t H5S_hyper_span_info_t;
typedef struct H5S_hyper_dim_t H5S_hyper_dim_t;

/* Point selection iteration container */
typedef struct {
    hsize_t elmt_left;      /* Number of elements left to iterate over */
    H5S_pnt_node_t *curr;   /* Pointer to next node to output */
} H5S_point_iter_t;

/* New Hyperslab selection iteration container */
typedef struct {
    /* Common fields for all hyperslab selections */
    hsize_t elmt_left;      /* Number of elements left to iterate over */
    hssize_t *off;          /* Offset in span node (used as position for regular hyperslabs) */
    unsigned iter_rank;     /* Rank of iterator information */
                            /* (This should always be the same as the dataspace
                             * rank, except for regular hyperslab selections in
                             * which there are contiguous regions in the lower
                             * dimensions which have been "flattened" out
                             */

    /* "Flattened" regular hyperslab selection fields */
    H5S_hyper_dim_t *diminfo;   /* "Flattened" regular selection information */
    hsize_t *size;          /* "Flattened" dataspace extent information */
    hssize_t *sel_off;      /* "Flattened" selection offset information */

    /* Irregular hyperslab selection fields */
    H5S_hyper_span_info_t *spans;  /* Pointer to copy of the span tree */
    H5S_hyper_span_t **span;/* Array of pointers to span nodes */
} H5S_hyper_iter_t;

/* "All" selection iteration container */
typedef struct {
    hsize_t elmt_left;      /* Number of elements left to iterate over */
    hsize_t offset;         /* Next element to output */
} H5S_all_iter_t;

/* Selection iteration container */
typedef union {
    H5S_point_iter_t pnt;   /* Point selection iteration information */
    H5S_hyper_iter_t hyp;   /* New Hyperslab selection iteration information */
    H5S_all_iter_t all;     /* "All" selection iteration information */
} H5S_sel_iter_t;

typedef struct H5S_conv_t {
    H5S_sel_type	ftype;
    H5S_sel_type	mtype;

    /*
     * If there is no data type conversion then it might be possible to
     * transfer data points between application memory and the file in one
     * step without going through the data type conversion buffer.
     */
    
    /* Read from file to application w/o intermediate scratch buffer */
    herr_t (*read)(H5F_t *f, const struct H5O_layout_t *layout,
           H5P_genplist_t *dc_plist, const H5O_efl_t *efl,
           size_t elmt_size, const H5S_t *file_space,
           const H5S_t *mem_space, hid_t dxpl_id, void *buf/*out*/);


    /* Write directly from app buffer to file */
    herr_t (*write)(H5F_t *f, struct H5O_layout_t *layout,
           H5P_genplist_t *dc_plist, const H5O_efl_t *efl,
           size_t elmt_size, const H5S_t *file_space,
           const H5S_t *mem_space, hid_t dxpl_id, const void *buf);
    
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
	H5_timer_t	read_timer;		/*time for read calls	*/
	hsize_t		read_nbytes;		/*total bytes read	*/
	hsize_t		read_ncalls;		/*number of calls	*/
	H5_timer_t	write_timer;		/*time for write calls	*/
	hsize_t		write_nbytes;		/*total bytes written	*/
	hsize_t		write_ncalls;		/*number of calls	*/
    } stats[2];		/* 0=output, 1=input */
#endif
} H5S_conv_t;

/* We get the declaration of H5G_entry_t from the H5Oprivate.h file */

H5_DLL H5S_t *H5S_create(H5S_class_t type);
H5_DLL H5S_t *H5S_copy(const H5S_t *src);
H5_DLL herr_t H5S_close(H5S_t *ds);
H5_DLL H5S_conv_t *H5S_find(const H5S_t *mem_space, const H5S_t *file_space,
                unsigned flags);
H5_DLL H5S_class_t H5S_get_simple_extent_type(const H5S_t *ds);
H5_DLL hssize_t H5S_get_simple_extent_npoints(const H5S_t *ds);
H5_DLL hsize_t H5S_get_npoints_max(const H5S_t *ds);
H5_DLL int H5S_get_simple_extent_ndims(const H5S_t *ds);
H5_DLL int H5S_get_simple_extent_dims(const H5S_t *ds, hsize_t dims[]/*out*/,
					hsize_t max_dims[]/*out*/);
H5_DLL herr_t H5S_set_extent_simple (H5S_t *space, unsigned rank, const hsize_t *dims,
		       const hsize_t *max);
H5_DLL herr_t H5S_modify(struct H5G_entry_t *ent, const H5S_t *space,
        hbool_t update_time, hid_t dxpl_id);
H5_DLL herr_t H5S_append(H5F_t *f, hid_t dxpl_id, struct H5O_t *oh, const H5S_t *ds);
H5_DLL H5S_t *H5S_read(struct H5G_entry_t *ent, hid_t dxpl_id);
H5_DLL int H5S_cmp(const H5S_t *ds1, const H5S_t *ds2);
H5_DLL htri_t H5S_is_simple(const H5S_t *sdim);
H5_DLL herr_t H5S_extent_release(H5S_t *space);
H5_DLL int H5S_extend(H5S_t *space, const hsize_t *size);
H5_DLL int H5S_set_extent(H5S_t *space, const hsize_t *size);
H5_DLL herr_t H5S_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg, FILE *stream,
			 int indent, int fwidth);

/* Operations on selections */
H5_DLL herr_t H5S_select_copy(H5S_t *dst, const H5S_t *src);
H5_DLL herr_t H5S_select_deserialize(H5S_t *space, const uint8_t *buf);
H5_DLL htri_t H5S_select_shape_same(const H5S_t *space1, const H5S_t *space2);
H5_DLL herr_t H5S_select_iterate(void *buf, hid_t type_id, H5S_t *space,
				H5D_operator_t op, void *operator_data);
H5_DLL herr_t H5S_select_fill(void *fill, size_t fill_size,
                                const H5S_t *space, void *buf);
H5_DLL herr_t H5S_select_fscat (H5F_t *f, struct H5O_layout_t *layout,
        H5P_genplist_t *dc_plist, const H5O_efl_t *efl, size_t elmt_size,
        const H5S_t *file_space, H5S_sel_iter_t *file_iter, hsize_t nelmts,
        hid_t dxpl_id, const void *_buf);
H5_DLL hsize_t H5S_select_fgath (H5F_t *f, const struct H5O_layout_t *layout,
        H5P_genplist_t *dc_plist, const H5O_efl_t *efl, size_t elmt_size,
        const H5S_t *file_space, H5S_sel_iter_t *file_iter, hsize_t nelmts,
        hid_t dxpl_id, void *buf);
H5_DLL herr_t H5S_select_mscat (const void *_tscat_buf, size_t elmt_size,
        const H5S_t *space, H5S_sel_iter_t *iter, hsize_t nelmts,
        hid_t dxpl_id, void *_buf/*out*/);
H5_DLL hsize_t H5S_select_mgath (const void *_buf, size_t elmt_size,
        const H5S_t *space, H5S_sel_iter_t *iter, hsize_t nelmts,
        hid_t dxpl_id, void *_tgath_buf/*out*/);
H5_DLL herr_t H5S_select_read(H5F_t *f, const struct H5O_layout_t *layout,
        H5P_genplist_t *dc_plist, const H5O_efl_t *efl, size_t elmt_size,
        const H5S_t *file_space, const H5S_t *mem_space, hid_t dxpl_id,
        void *buf/*out*/);
H5_DLL herr_t H5S_select_write(H5F_t *f, struct H5O_layout_t *layout,
        H5P_genplist_t *dc_plist, const H5O_efl_t *efl, size_t elmt_size,
        const H5S_t *file_space, const H5S_t *mem_space, hid_t dxpl_id,
        const void *buf/*out*/);

/* Needed for internal use of selections in H5Fistore code */
H5_DLL herr_t H5S_select_all(H5S_t *space, unsigned rel_prev);
H5_DLL herr_t H5S_select_hyperslab (H5S_t *space, H5S_seloper_t op, const hssize_t start[],
                const hsize_t *stride, const hsize_t count[],
                const hsize_t *block);

#ifdef H5_HAVE_PARALLEL
/* MPI-IO function to read directly from app buffer to file rky980813 */
H5_DLL herr_t H5S_mpio_spaces_read(H5F_t *f,
				    const struct H5O_layout_t *layout,
                                    H5P_genplist_t *dc_plist,
                                    const H5O_efl_t *efl,
				    size_t elmt_size, const H5S_t *file_space,
				    const H5S_t *mem_space, hid_t dxpl_id,
				    void *buf/*out*/);

/* MPI-IO function to write directly from app buffer to file rky980813 */
H5_DLL herr_t H5S_mpio_spaces_write(H5F_t *f,
				    struct H5O_layout_t *layout,
                                    H5P_genplist_t *dc_plist,
                                    const H5O_efl_t *efl,
				    size_t elmt_size, const H5S_t *file_space,
				    const H5S_t *mem_space, hid_t dxpl_id,
				    const void *buf);

/* MPI-IO function to check if a direct I/O transfer is possible between
 * memory and the file */
H5_DLL htri_t H5S_mpio_opt_possible(const H5S_t *mem_space,
                                     const H5S_t *file_space, const unsigned flags);

#ifndef _H5S_IN_H5S_C
/* Global vars whose value comes from environment variable */
/* (Defined in H5S.c) */
H5_DLLVAR hbool_t		H5S_mpi_opt_types_g;
H5_DLLVAR hbool_t		H5S_mpi_prefer_derived_types_g;
#endif /* _H5S_IN_H5S_C */

#endif /* H5_HAVE_PARALLEL */

#endif /* _H5Sprivate_H */
