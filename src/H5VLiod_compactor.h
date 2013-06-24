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


/* TODO:  This gets replaced with H5VLiod_common.h on merging with FF code (VV) */
#include <stdio.h>
#include <stdlib.h>
#include "hdf5.h"
#include "H5VLiod_compactor_queue.h"

typedef struct {
  hsize_t offset;
  size_t len;
  char *mem_buf_pos;
}block_container_t;


typedef struct {
  block_container_t *blocks;
  size_t elementsize;
  hid_t dataset_id;
  hid_t selection_id;
  char *mem_buffer;
  size_t mem_length;
} request_list_t;

/* ----------------------------------------- */

/* Extracts the dimensions of the dataset 
   using the dataspace id */
int H5VL_create_requests_list (hid_t dataset_id, hid_t dataspace_id, 
			       compactor *queue, char *mem_buf, size_t mem_buf_len,
			       request_list_t **list, int *requests);
int H5VL_sort_request_list (request_list_t **list, int num_entires, int *sorted);
int H5VL_extract_dims_info (hid_t dataspace, int *dims, hsize_t *dims_out);
int H5VL_reconstruct_memory_buffer (hid_t dataspace, 
				    hid_t dataset, 
				    request_list_t *list);
int H5VL_check_overlap (hid_t dataspace1 , hid_t dataspace2 );
hid_t H5VL_merge_selections (hid_t dataspace1, hid_t dataset1, hid_t dataspace2, hid_t dataset2);




/*----------------------------------------------------------------------------  */


#endif /* _H5VLiod_COMPACTOR_H */
