/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef _H5DOpublic_H
#define _H5DOpublic_H

#ifdef __cplusplus
extern "C" {
#endif

/*-------------------------------------------------------------------------
 *
 * Direct chunk write function
 *
 *-------------------------------------------------------------------------
 */

H5_HLDLL herr_t H5DOwrite_chunk(hid_t dset_id, hid_t dxpl_id, uint32_t filters, const hsize_t *offset,
                                size_t data_size, const void *buf);

/*-------------------------------------------------------------------------
 *
 * Direct chunk read function
 *
 *-------------------------------------------------------------------------
 */

H5_HLDLL herr_t H5DOread_chunk(hid_t          dset_id, /*in*/
                               hid_t          dxpl_id, /*in*/
                               const hsize_t *offset,  /*in*/
                               uint32_t *     filters, /*out*/
                               void *         buf);             /*out*/

#ifdef __cplusplus
}
#endif

#endif
