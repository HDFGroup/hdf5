/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Neil Fortner <nfortne2@hdfgroup.gov>
 *              December, 2016
 *
 * Purpose:	The public header file for the RADOS VOL plugin.
 */
#ifndef H5VLrados_public_H
#define H5VLrados_public_H

#define H5_HAVE_EFF 1 /* DSMINC */

/* External headers needed by this file */
#include <rados/librados.h>

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

#define H5_VOL_RADOS   2022         /* RADOS VOL connector ID*/


#ifdef __cplusplus
extern "C" {
#endif

H5_DLL herr_t H5VLrados_init(rados_t rados_cluster, const char *rados_pool);
H5_DLL herr_t H5VLrados_term(void);
H5_DLL herr_t H5Pset_fapl_rados(hid_t fapl_id, MPI_Comm comm, MPI_Info info);

#ifdef __cplusplus
}
#endif

#endif /* H5VLrados_public_H */
