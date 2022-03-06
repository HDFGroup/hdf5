/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Akadio, Inc.                                                 *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * The public header file for the VFD SWMR driver.
 */
#ifndef H5FDvfd_swmr_H
#define H5FDvfd_swmr_H

#define H5FD_VFD_SWMR       (H5FDperform_init(H5FD_vfd_swmr_init))
#define H5FD_VFD_SWMR_VALUE H5_VFD_SWMR

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t  H5FD_vfd_swmr_init(void);
H5_DLL herr_t H5Pset_fapl_vfd_swmr(hid_t fapl_id);

#ifdef __cplusplus
}
#endif

#endif
