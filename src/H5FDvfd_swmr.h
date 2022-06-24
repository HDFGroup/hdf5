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

/* Semi-unique constant used to help identify structure pointers */
#define H5FD_VFD_SWMR_READER_MAGIC 0xABC123

/* ----------------------------------------------------------------------------
 * Structure:   H5FD_vfd_swmr_reader_fapl_t
 *
 * Structure for configuring the VFD SWMR reader VFD.  This structure and the
 * associated FAPL entry and get / set routines are for HDF5 library use only,
 * and should never be seen by the user.
 *
 * When a file is opened in VFD SWMR mode, the VFD SWMR reader VFD is inserted
 * at the top of the user supplied (or default) VFD stack.
 *
 * The fields of this structure are discussed indvidually below.  Note that
 * there is no version field, since this structure should not be accessible
 * to the user.  The set of fields is quite limited, as most of the necessary
 * configuration data is taken from the VFD SWMR configuration FAPL entry
 *
 * magic (int32_t)
 *      Semi-unique number, used to sanity-check that a given pointer is
 *      likely (or not) to be this structure type. MUST be first.
 *      If magic is not H5FD_VFD_SWMR_READER_MAGIC, the structure (and/or
 *      pointer to) must be considered invalid.
 *
 * fapl_id (hid_t)
 *      Library-given identification number of the FAPL containing the user
 *      supplied VFD stack.  Must be set to H5P_DEFAULT or contain a file
 *      driver entry specifying a VFD that supports VFD SWMR
 *
 * ----------------------------------------------------------------------------
 */
typedef struct H5FD_vfd_swmr_reader_fapl_t {
    int32_t magic;
    hid_t   fapl_id;
} H5FD_vfd_swmr_reader_fapl_t;

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t  H5FD_vfd_swmr_init(void);
H5_DLL herr_t H5P_pop_vfd_swmr_reader_vfd_off_fapl(hid_t fapl_id);
H5_DLL herr_t H5P_push_vfd_swmr_reader_vfd_on_fapl(hid_t fapl_id);

#ifdef __cplusplus
}
#endif

#endif
