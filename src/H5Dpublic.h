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

/* $Id$ */

/*
 * This file contains public declarations for the H5D module.
 */

#ifndef _H5Dpublic_H
#define _H5Dpublic_H

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Apublic.h>

/* Values for the H5D_LAYOUT property */
typedef enum H5D_layout_t {
    H5D_LAYOUT_ERROR = -1,

    H5D_COMPACT = 0,            /*raw data is very small        */
    H5D_CONTIGUOUS = 1,         /*the default                   */
    H5D_CHUNKED = 2,            /*slow and fancy                */

    H5D_NLAYOUTS = 3            /*This one must be last!        */
} H5D_layout_t;

#ifdef __cplusplus
extern                  "C" {
#endif

    hid_t                   H5Dcreate(hid_t file_id, const char *name, hid_t type_id,
                                      hid_t space_id, hid_t create_parms_id);
    hid_t                   H5Dopen(hid_t file_id, const char *name);
    herr_t                  H5Dclose(hid_t dataset_id);
    herr_t                  H5Dread(hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id,
             hid_t file_space_id, hid_t xfer_parms_id, void *buf /*out */ );
    herr_t                  H5Dwrite(hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id,
                 hid_t file_space_id, hid_t xfer_parms_id, const void *buf);

#ifdef __cplusplus
}

#endif
#endif
