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

/* Template classes */
typedef enum H5P_class_t {
    H5P_NO_CLASS         = -1,  /*error return value                         */
    H5P_FILE_CREATE      = 0,   /*file creation template                     */
    H5P_FILE_ACCESS      = 1,   /*file access template                       */
    H5P_DATASET_CREATE   = 2,   /*dataset creation template                  */
    H5P_DATASET_XFER     = 3,   /*dataset transfer template                  */

    H5P_NCLASSES         = 4    /*this must be last!                         */
} H5P_class_t;

#ifdef __cplusplus
extern "C" {
#endif

/* Public functions */
hid_t H5Pcreate (H5P_class_t type);
herr_t H5Pclose (hid_t plist_id);
hid_t H5Pcopy (hid_t plist_id);
H5P_class_t H5Pget_class (hid_t plist_id);
herr_t H5Pget_version (hid_t plist_id, int *boot/*out*/, int *freelist/*out*/,
		       int *stab/*out*/, int *shhdr/*out*/);
herr_t H5Pset_userblock (hid_t plist_id, hsize_t size);
herr_t H5Pget_userblock (hid_t plist_id, hsize_t *size);
herr_t H5Pset_alignment (hid_t fapl_id, hsize_t threshold, hsize_t alignment);
herr_t H5Pget_alignment (hid_t fapl_id, hsize_t *threshold/*out*/,
			 hsize_t *alignment/*out*/);
herr_t H5Pset_sizes (hid_t plist_id, size_t sizeof_addr, size_t sizeof_size);
herr_t H5Pget_sizes (hid_t plist_id, size_t *sizeof_addr/*out*/,
                     size_t *sizeof_size/*out*/);
herr_t H5Pset_sym_k (hid_t plist_id, int ik, int lk);
herr_t H5Pget_sym_k (hid_t plist_id, int *ik/*out*/, int *lk/*out*/);
herr_t H5Pset_istore_k (hid_t plist_id, int ik);
herr_t H5Pget_istore_k (hid_t plist_id, int *ik/*out*/);
herr_t H5Pset_layout (hid_t plist_id, H5D_layout_t layout);
H5D_layout_t H5Pget_layout (hid_t plist_id);
herr_t H5Pset_chunk (hid_t plist_id, int ndims, const hsize_t dim[]);
int H5Pget_chunk (hid_t plist_id, int max_ndims, hsize_t dim[]/*out*/);
herr_t H5Pset_external (hid_t plist_id, const char *name, off_t offset,
			hsize_t size);
int H5Pget_external_count (hid_t plist_id);
herr_t H5Pget_external (hid_t plist_id, int idx, size_t name_size,
			char *name/*out*/, off_t *offset/*out*/,
			hsize_t *size/*out*/);
H5F_driver_t H5Pget_driver (hid_t plist_id);
herr_t H5Pset_stdio (hid_t plist_id);
herr_t H5Pget_stdio (hid_t plist_id);
herr_t H5Pset_sec2 (hid_t plist_id);
herr_t H5Pget_sec2 (hid_t plist_id);
herr_t H5Pset_core (hid_t plist_id, size_t increment);
herr_t H5Pget_core (hid_t plist_id, size_t *increment/*out*/);
herr_t H5Pset_split (hid_t plist_id, const char *meta_ext, hid_t meta_plist_id,
		     const char *raw_ext, hid_t raw_plist_id);
herr_t H5Pget_split (hid_t plist_id, size_t meta_ext_size,
		     char *meta_ext/*out*/, hid_t *meta_properties/*out*/,
		     size_t raw_ext_size, char *raw_ext/*out*/,
		     hid_t *raw_properties/*out*/);

herr_t H5Pset_family (hid_t plist_id, hsize_t memb_size, hid_t memb_plist_id);
herr_t H5Pget_family (hid_t plist_id, hsize_t *memb_size/*out*/,
		      hid_t *memb_plist_id/*out*/);
herr_t H5Pset_buffer (hid_t plist_id, size_t size, void *tconv, void *bkg);
size_t H5Pget_buffer (hid_t plist_id, void **tconv/*out*/, void **bkg/*out*/);
herr_t H5Pset_preserve (hid_t plist_id, hbool_t status);
int H5Pget_preserve (hid_t plist_id);
herr_t H5Pset_filter (hid_t plist_id, H5Z_filter_t filter, unsigned int flags,
		      size_t cd_nelmts, const unsigned int c_values[]);
int H5Pget_nfilters(hid_t plist_id);
H5Z_filter_t H5Pget_filter(hid_t plist_id, int filter,
			   unsigned int *flags/*out*/,
			   size_t *cd_nelmts/*out*/,
			   unsigned cd_values[]/*out*/,
			   size_t namelen, char name[]);
herr_t H5Pset_deflate (hid_t plist_id, unsigned aggression);
herr_t H5Pset_cache (hid_t plist_id, int mdc_nelmts, int rdcc_nelmts,
		     size_t rdcc_nbytes, double rdcc_w0);
herr_t H5Pget_cache (hid_t plist_id, int *mdc_nelmts/*out*/,
		     int *rdcc_nelmts/*out*/, size_t *rdcc_nbytes/*out*/,
		     double *rdcc_w0);
herr_t H5Pset_hyper_cache(hid_t plist_id, unsigned cache, unsigned limit);
herr_t H5Pget_hyper_cache(hid_t plist_id, unsigned *cache, unsigned *limit);
herr_t H5Pset_btree_ratios(hid_t plist_id, double left, double middle,
			   double right);
herr_t H5Pget_btree_ratios(hid_t plist_id, double *left/*out*/,
			   double *middle/*out*/, double *right/*out*/);

#ifdef HAVE_PARALLEL
herr_t H5Pset_mpi (hid_t plist_id, MPI_Comm comm, MPI_Info info);
herr_t H5Pget_mpi (hid_t plist_id, MPI_Comm *comm/*out*/,
		   MPI_Info *info/*out*/);
herr_t H5Pset_xfer (hid_t plist_id, H5D_transfer_t data_xfer_mode);
herr_t H5Pget_xfer (hid_t plist_id, H5D_transfer_t *data_xfer_mode/*out*/);
#endif

#ifdef __cplusplus
}
#endif
#endif
