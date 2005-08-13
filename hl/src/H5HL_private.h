/****************************************************************************
 * NCSA HDF                                                                 *
 * Scientific Data Technologies                                             *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/


#ifndef _H5HL_private_H
#define _H5HL_private_H

#include <hdf5.h>

#ifdef __cplusplus
extern "C" {
#endif

/*-------------------------------------------------------------------------
 *
 * Private write function used by H5TB and H5PT
 *
 *-------------------------------------------------------------------------
 */

herr_t H5TBcommon_append_records( hid_t dataset_id,
                                  hid_t mem_type_id,
                                  hsize_t nrecords,
                                  hsize_t orig_table_size,
                                  const void * data);

/*-------------------------------------------------------------------------
 *
 * Private read function used by H5TB and H5PT
 *
 *-------------------------------------------------------------------------
 */


herr_t H5TBcommon_read_records( hid_t dataset_id,
                                hid_t mem_type_id,
                                hsize_t start,
                                hsize_t nrecords,
                                hsize_t table_size,
                                void *data);

#ifdef __cplusplus
}
#endif

#endif
