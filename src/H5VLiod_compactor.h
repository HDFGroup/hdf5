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


#ifndef _H5VLiod_COMPACTOR_H
#define _H5VLiod_COMPACTOR_H


#include "H5VLiod_common.h"

#ifdef H5_HAVE_EFF

#include "H5VLiod_compactor_queue.h"

typedef struct {
  hsize_t offset;
  size_t len;
  hsize_t mem_offset;
  size_t mem_len;
}block_container_t;

typedef struct {
  block_container_t *blocks;
  size_t numblocks;
  size_t elementsize;
  hid_t dataset_id;
  hid_t selection_id;
  char *mem_buffer;
  size_t mem_length;
} request_list_t;

/*----------------------------------------------------------------------------------- */
H5_DLL int H5VL_iod_create_request_list (hid_t dataset_id, compactor *queue,
					 request_list_t **list, int *numentries,
					 int request_type);
H5_DLL int H5VL_iod_sort_request_list (request_list_t **list, int num_entires, int *sorted);
H5_DLL int H5VL_iod_extract_dims_info (hid_t dataspace, int *dims, hsize_t **dims_out);
H5_DLL int H5VL_iod_dataset_specific_requests (request_list_t *list, 
					       request_list_t ***dataset_list,
					       int *num_datasets);
H5_DLL int H5VL_iod_compact_requests  (request_list_t *list, int num_requests,
				       request_list_t *revised, int *num_selecoted, 
				       int **selected_requests);
H5_DLL int H5VL_iod_select_overlap (hid_t dataspace1 , hid_t dataspace2, 
				    hid_t *res_dataspace);
H5_DLL hid_t H5VL_iod_merge_selections (hid_t dataspace1, hid_t dataspace2);
/*----------------------------------------------------------------------------------------  */

#endif /* H5_HAVE_EFF*/
#endif /* _H5VL_iod_COMPACTOR_H */
