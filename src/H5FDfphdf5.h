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

#ifndef H5FDFPHDF5_H__
#define H5FDFPHDF5_H__

#include "H5FDpublic.h"
#include "H5Ipublic.h"

#ifdef H5_HAVE_PARALLEL
#   define H5FD_FPHDF5      (H5FD_fphdf5_init())
#else
#   define H5FD_FPHDF5      (-1)
#endif  /* H5_HAVE_PARALLEL */

/* Macros */

#define IS_H5FD_FPHDF5(f)   (H5F_get_driver_id(f) == H5FD_FPHDF5)

#ifdef H5_HAVE_PARALLEL

/* Turn on H5FDfphdf5_debug if H5F_DEBUG is on */
#ifdef H5F_DEBUG
#   ifndef H5FDfphdf5_DEBUG
#       define H5FDfphdf5_DEBUG
#   endif
#endif

/* Function prototypes */
#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */

H5_DLL hid_t H5FD_fphdf5_init(void);
H5_DLL herr_t H5Pset_fapl_fphdf5(hid_t fapl_id, MPI_Comm comm, MPI_Info info);
H5_DLL herr_t H5Pget_fapl_fphdf5(hid_t fapl_id, MPI_Comm *comm/*out*/,
                                 MPI_Info *info/*out*/);
H5_DLL MPI_Comm H5FD_fphdf5_communicator(H5FD_t *_file);
H5_DLL herr_t H5FD_fphdf5_setup(hid_t dxpl_id, MPI_Datatype btype,
                                MPI_Datatype ftype, unsigned use_view);
H5_DLL herr_t H5FD_fphdf5_teardown(hid_t dxpl_id);
H5_DLL int H5FD_fphdf5_mpi_rank(H5FD_t *_file);
H5_DLL int H5FD_fphdf5_mpi_size(H5FD_t *_file);

#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif  /* H5_HAVE_PARALLEL */

#endif  /* H5FDFPHDF5_H__ */
