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
herr_t H5Pclose (hid_t tid);
hid_t H5Pcopy (hid_t tid);
H5P_class_t H5Pget_class (hid_t tid);
herr_t H5Pget_version (hid_t tid, int *boot/*out*/, int *freelist/*out*/,
		       int *stab/*out*/, int *shhdr/*out*/);
herr_t H5Pset_userblock (hid_t tid, size_t size);
herr_t H5Pget_userblock (hid_t tid, size_t *size);
herr_t H5Pset_sizes (hid_t tid, size_t sizeof_addr, size_t sizeof_size);
herr_t H5Pget_sizes (hid_t tid, size_t *sizeof_addr/*out*/,
                     size_t *sizeof_size/*out*/);
herr_t H5Pset_sym_k (hid_t tid, int ik, int lk);
herr_t H5Pget_sym_k (hid_t tid, int *ik/*out*/, int *lk/*out*/);
herr_t H5Pset_istore_k (hid_t tid, int ik);
herr_t H5Pget_istore_k (hid_t tid, int *ik/*out*/);
herr_t H5Pset_layout (hid_t tid, H5D_layout_t layout);
H5D_layout_t H5Pget_layout (hid_t tid);
herr_t H5Pset_chunk (hid_t tid, int ndims, const size_t dim[]);
int H5Pget_chunk (hid_t tid, int max_ndims, size_t dim[]/*out*/);
herr_t H5Pset_external (hid_t plist_id, const char *name, size_t offset,
			size_t size);
int H5Pget_external_count (hid_t plist_id);
herr_t H5Pget_external (hid_t plist_id, int idx, size_t name_size,
			char *name/*out*/, size_t *offset/*out*/,
			size_t *size/*out*/);
H5F_driver_t H5Pget_driver (hid_t tid);
herr_t H5Pset_stdio (hid_t tid);
herr_t H5Pget_stdio (hid_t tid);
herr_t H5Pset_sec2 (hid_t tid);
herr_t H5Pget_sec2 (hid_t tid);
herr_t H5Pset_core (hid_t tid, size_t increment);
herr_t H5Pget_core (hid_t tid, size_t *increment/*out*/);
herr_t H5Pset_split (hid_t tid, const char *meta_ext, hid_t meta_tid,
		     const char *raw_ext, hid_t raw_tid);
herr_t H5Pget_split (hid_t tid, size_t meta_ext_size, char *meta_ext/*out*/,
		     hid_t *meta_properties/*out*/, size_t raw_ext_size,
		     char *raw_ext/*out*/, hid_t *raw_properties/*out*/);

herr_t H5Pset_family (hid_t tid, hid_t memb_tid);
herr_t H5Pget_family (hid_t tid, hid_t *memb_tid/*out*/);
herr_t H5Pset_buffer (hid_t plist_id, size_t size, void *tconv, void *bkg);
size_t H5Pget_buffer (hid_t plist_id, void **tconv/*out*/, void **bkg/*out*/);
herr_t H5Pset_preserve (hid_t plist_id, hbool_t status);
int H5Pget_preserve (hid_t plist_id);

#ifdef HAVE_PARALLEL
herr_t H5Pset_mpi (hid_t tid, MPI_Comm comm, MPI_Info info,
		   unsigned access_mode);
herr_t H5Pget_mpi (hid_t tid, MPI_Comm *comm/*out*/, MPI_Info *info/*out*/,
		   unsigned *access_mode/*out*/);
#endif

#ifdef __cplusplus
}
#endif
#endif
