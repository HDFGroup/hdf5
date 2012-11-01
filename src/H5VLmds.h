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
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              January, 2012
 *
 * Purpose:	The public header file for the MDS VOL plugin.
 */
#ifndef H5VLmds_H
#define H5VLmds_H

#ifdef H5_HAVE_PARALLEL
#define H5VL_MDS	(H5VL_mds_init())
#else
#define H5VL_MDS	(-1)
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef H5_HAVE_PARALLEL
H5_DLL H5VL_class_t *H5VL_mds_init(void);
H5_DLL herr_t H5Pset_fapl_mds(hid_t fapl_id, char *raw_ext, char *meta_ext, MPI_Comm comm, MPI_Info info);
#endif /* H5_HAVE_PARALLEL */

#ifdef __cplusplus
}
#endif

#endif /* _H5VLmds_H */
