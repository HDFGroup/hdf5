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
