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
 * This file contains public declarations for the H5D module.
 */
#ifndef _H5Dpublic_H
#define _H5Dpublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/* Values for the H5D_LAYOUT property */
typedef enum H5D_layout_t {
    H5D_LAYOUT_ERROR	= -1,

    H5D_COMPACT		= 0,	/*raw data is very small		     */
    H5D_CONTIGUOUS	= 1,	/*the default				     */
    H5D_CHUNKED		= 2,	/*slow and fancy			     */

    H5D_NLAYOUTS	= 3	/*this one must be last!		     */
} H5D_layout_t;

#if defined(WANT_H5_V1_2_COMPAT) || defined(H5_WANT_H5_V1_2_COMPAT)
/* Values for the data transfer property */
typedef enum H5D_transfer_t {
    H5D_XFER_INDEPENDENT,	/*Independent data transfer		     */
    H5D_XFER_COLLECTIVE,	/*Collective data transfer		     */
    H5D_XFER_DFLT		/*default data transfer mode		     */
} H5D_transfer_t;
#endif /* WANT_H5_V1_2_COMPAT */

/* Define the operator function pointer for H5Diterate() */
typedef herr_t (*H5D_operator_t)(void *elem, hid_t type_id, hsize_t ndim,
				 hssize_t *point, void *operator_data);

#ifdef __cplusplus
extern "C" {
#endif

__DLL__ hid_t H5Dcreate (hid_t file_id, const char *name, hid_t type_id,
			 hid_t space_id, hid_t plist_id);
__DLL__ hid_t H5Dopen (hid_t file_id, const char *name);
__DLL__ herr_t H5Dclose (hid_t dset_id);
__DLL__ hid_t H5Dget_space (hid_t dset_id);
__DLL__ hid_t H5Dget_type (hid_t dset_id);
__DLL__ hid_t H5Dget_create_plist (hid_t dset_id);
__DLL__ hsize_t H5Dget_storage_size(hid_t dset_id);
__DLL__ herr_t H5Dread (hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
			hid_t file_space_id, hid_t plist_id, void *buf/*out*/);
__DLL__ herr_t H5Dwrite (hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
			 hid_t file_space_id, hid_t plist_id, const void *buf);
__DLL__ herr_t H5Dextend (hid_t dset_id, const hsize_t *size);
__DLL__ herr_t H5Diterate(void *buf, hid_t type_id, hid_t space_id,
            H5D_operator_t op, void *operator_data);
__DLL__ herr_t H5Dvlen_reclaim(hid_t type_id, hid_t space_id, hid_t plist_id, void *buf);
__DLL__ herr_t H5Dvlen_get_buf_size(hid_t dataset_id, hid_t type_id, hid_t space_id, hsize_t *size);
__DLL__ herr_t H5Ddebug(hid_t dset_id, unsigned int flags);

#ifdef __cplusplus
}
#endif
#endif
