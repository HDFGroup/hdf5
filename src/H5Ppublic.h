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
 * This file contains function prototypes for each exported function in the
 * H5P module.
 */
#ifndef _H5Ppublic_H
#define _H5Ppublic_H

/* Default Template for creation, access, etc. templates */
#define H5P_DEFAULT     (-2)

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Ipublic.h>
#include <H5Dpublic.h>
#include <H5Fpublic.h>
#include <H5Zpublic.h>

/* Property list classes */
typedef enum H5P_class_t {
    H5P_NO_CLASS         = -1,  /*error return value                         */
    H5P_FILE_CREATE      = 0,   /*file creation properties                   */
    H5P_FILE_ACCESS      = 1,   /*file access properties                     */
    H5P_DATASET_CREATE   = 2,   /*dataset creation properties                */
    H5P_DATA_XFER        = 3,   /*data transfer properties	             */
    H5P_MOUNT		 = 4,	/*file mounting properties		     */

    H5P_NCLASSES         = 5    /*this must be last!                         */
} H5P_class_t;

/* Alias for the previous H5P_DATASER_XFER property list */
/* This should eventually be publicly decommisioned - 10/6/99 - QAK */
#define H5P_DATASET_XFER H5P_DATA_XFER

#ifdef __cplusplus
extern "C" {
#endif

/* Public functions */
__DLL__ hid_t H5Pcreate(H5P_class_t type);
__DLL__ herr_t H5Pclose(hid_t plist_id);
__DLL__ hid_t H5Pcopy(hid_t plist_id);
__DLL__ H5P_class_t H5Pget_class(hid_t plist_id);
__DLL__ herr_t H5Pget_version(hid_t plist_id, int *boot/*out*/,
			      int *freelist/*out*/, int *stab/*out*/,
			      int *shhdr/*out*/);
__DLL__ herr_t H5Pset_userblock(hid_t plist_id, hsize_t size);
__DLL__ herr_t H5Pget_userblock(hid_t plist_id, hsize_t *size);
__DLL__ herr_t H5Pset_alignment(hid_t fapl_id, hsize_t threshold,
				hsize_t alignment);
__DLL__ herr_t H5Pget_alignment(hid_t fapl_id, hsize_t *threshold/*out*/,
				hsize_t *alignment/*out*/);
__DLL__ herr_t H5Pset_sizes(hid_t plist_id, size_t sizeof_addr,
			    size_t sizeof_size);
__DLL__ herr_t H5Pget_sizes(hid_t plist_id, size_t *sizeof_addr/*out*/,
			    size_t *sizeof_size/*out*/);
__DLL__ herr_t H5Pset_sym_k(hid_t plist_id, int ik, int lk);
__DLL__ herr_t H5Pget_sym_k(hid_t plist_id, int *ik/*out*/, int *lk/*out*/);
__DLL__ herr_t H5Pset_istore_k(hid_t plist_id, int ik);
__DLL__ herr_t H5Pget_istore_k(hid_t plist_id, int *ik/*out*/);
__DLL__ herr_t H5Pset_layout(hid_t plist_id, H5D_layout_t layout);
__DLL__ H5D_layout_t H5Pget_layout(hid_t plist_id);
__DLL__ herr_t H5Pset_chunk(hid_t plist_id, int ndims, const hsize_t dim[]);
__DLL__ int H5Pget_chunk(hid_t plist_id, int max_ndims, hsize_t dim[]/*out*/);
__DLL__ herr_t H5Pset_external(hid_t plist_id, const char *name, off_t offset,
			       hsize_t size);
__DLL__ int H5Pget_external_count(hid_t plist_id);
__DLL__ herr_t H5Pget_external(hid_t plist_id, int idx, size_t name_size,
			       char *name/*out*/, off_t *offset/*out*/,
			       hsize_t *size/*out*/);
__DLL__ herr_t H5Pset_driver(hid_t plist_id, hid_t driver_id,
			     const void *driver_info);
__DLL__ hid_t H5Pget_driver(hid_t plist_id);
__DLL__ void *H5Pget_driver_info(hid_t plist_id);
__DLL__ herr_t H5Pset_buffer(hid_t plist_id, size_t size, void *tconv,
			     void *bkg);
__DLL__ size_t H5Pget_buffer(hid_t plist_id, void **tconv/*out*/,
			     void **bkg/*out*/);
__DLL__ herr_t H5Pset_preserve(hid_t plist_id, hbool_t status);
__DLL__ int H5Pget_preserve(hid_t plist_id);
__DLL__ herr_t H5Pset_filter(hid_t plist_id, H5Z_filter_t filter,
			     unsigned int flags, size_t cd_nelmts,
			     const unsigned int c_values[]);
__DLL__ int H5Pget_nfilters(hid_t plist_id);
__DLL__ H5Z_filter_t H5Pget_filter(hid_t plist_id, int filter,
				   unsigned int *flags/*out*/,
				   size_t *cd_nelmts/*out*/,
				   unsigned cd_values[]/*out*/,
				   size_t namelen, char name[]);
__DLL__ herr_t H5Pset_deflate(hid_t plist_id, unsigned aggression);
__DLL__ herr_t H5Pset_cache(hid_t plist_id, int mdc_nelmts, int rdcc_nelmts,
			    size_t rdcc_nbytes, double rdcc_w0);
__DLL__ herr_t H5Pget_cache(hid_t plist_id, int *mdc_nelmts/*out*/,
			    int *rdcc_nelmts/*out*/,
			    size_t *rdcc_nbytes/*out*/, double *rdcc_w0);
__DLL__ herr_t H5Pset_hyper_cache(hid_t plist_id, unsigned cache,
				  unsigned limit);
__DLL__ herr_t H5Pget_hyper_cache(hid_t plist_id, unsigned *cache,
				  unsigned *limit);
__DLL__ herr_t H5Pset_btree_ratios(hid_t plist_id, double left, double middle,
				   double right);
__DLL__ herr_t H5Pget_btree_ratios(hid_t plist_id, double *left/*out*/,
				   double *middle/*out*/,
				   double *right/*out*/);
__DLL__ herr_t H5Pset_fill_value(hid_t plist_id, hid_t type_id,
				 const void *value);
__DLL__ herr_t H5Pget_fill_value(hid_t plist_id, hid_t type_id,
				 void *value/*out*/);
__DLL__ herr_t H5Pset_gc_references(hid_t fapl_id, unsigned gc_ref);
__DLL__ herr_t H5Pget_gc_reference(hid_t fapl_id, unsigned *gc_ref/*out*/);
__DLL__ herr_t H5Pset_vlen_mem_manager(hid_t plist_id,
        H5MM_allocate_t alloc_func, void *alloc_info, H5MM_free_t free_func,
        void *free_info);
__DLL__ herr_t H5Pget_vlen_mem_manager(hid_t plist_id,
        H5MM_allocate_t *alloc_func, void **alloc_info, H5MM_free_t *free_func,
        void **free_info);

#ifdef __cplusplus
}
#endif
#endif
