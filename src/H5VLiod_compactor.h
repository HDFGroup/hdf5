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

/* -----------------------------------------------------------------
 * Programmer:  Vishwanath Venkatesan <vish@hdfgroup.gov>
 *              June, 2013
 *
 * Purpose:	Request Compactor server-side routine headers

 *------------------------------------------------------------------*/


#include "H5VLiod_compactor_queue.h"


compactor *curr_queue;

typedef struct {
  hsize_t offset;
  size_t len;
}block_container_t;

typedef struct {
  hid_t dataset;
  int num_requests;
  int *requests;
}dataset_container_t;

typedef struct {
  int request_id;              /* The ID of the current request */
  int merged;                  /* Was this request was used to create a merged request 
				  0 - Not Merged --> call call-back normally 
			          1 - Used for merging ---> skip this
			          2 - Merged from other requests use memory_blocks */
  block_container_t *fblocks;  /* File offset/len  */
  block_container_t *mblocks;  /* Memory offset/len  */
  size_t num_fblocks;          /* Number of File blocks */   
  size_t num_mblocks;          /* Number of Memory blocks */ 
  /*Convenience def*/ 
  size_t elementsize;          /* Size of each element in the dataset */
  hid_t dataset_id;            /* The ID of the dataset */
  hid_t selection_id;          /* The ID of the dataspace  */
  uint64_t axe_id;
  int num_peers;
  /*------------------------------------------------------------------*/
  void *mem_buf;
  size_t mem_length;
  op_data_t *op_data;          /* The datastructure that containe all the information*/
} request_list_t;



/*----------------------------------------------------------------------------------- */

H5_DLL int H5VL_iod_create_request_list (compactor *queue, request_list_t **list, 
					 int *numentries,   dataset_container_t **unique_datasets,
					 int *num_datasets, int request_type);

H5_DLL int H5VL_iod_sort_request_list (request_list_t **list, int num_entires, int *sorted);

H5_DLL int H5VL_iod_extract_dims_info (hid_t dataspace, int *dims, hsize_t **dims_out);

H5_DLL int H5VL_iod_dataset_specific_requests (request_list_t *list, 
					       request_list_t ***dataset_list,
					       int *num_datasets);

H5_DLL int H5VL_iod_compact_requests (request_list_t *req_list, int *total_requests,
				      int num_requests, int *request_list);

H5_DLL int H5VL_iod_select_overlap (hid_t dataspace1, uint64_t axe_id_1,
				    hid_t dataspace2, uint64_t axe_id_2,
				    hid_t *res_dataspace, int num_peers);

H5_DLL int H5VL_iod_get_unique_dataset_request_sets ( request_list_t *list,
						      int *u_datasets,
						      int ***dataset_request_sets );

H5_DLL int H5VL_iod_create_dataset_request_list (request_list_t *list,
						 int *u_dataset,
						 int **dataset_request_sets,
						 request_list_t **dataset_list);

H5_DLL int H5VL_iod_sort_block_container (block_container_t *io_array,
					  size_t num_entries,
					  int *sorted);

H5_DLL int H5VL_iod_construct_merged_request (request_list_t *list,
					       size_t m_element_size,
					       request_list_t *merged_req,
					       int *lselected, int num_selected,
					       int *request_list, int num_requests);

H5_DLL int H5VL_iod_reconstruct_overlapped_request (block_container_t *sf_block,
						    block_container_t *sm_block,
						    block_container_t **out_sf_block,
						    block_container_t **out_sm_block,
						    size_t *revfblks, size_t *revmblks,
						    size_t i, size_t j, 
						    int *changed, int *changed_cnt);

H5_DLL int H5VL_iod_free_memory_buffer (request_list_t *list, int num_requests);

H5_DLL int H5VL_iod_steal_writes (request_list_t *wlist, int nentries,
				  request_list_t *rlist, int nrentries);

H5_DLL int H5VL_iod_short_circuit_reads (request_list_t *wlist, int nentries,
					 request_list_t *rlist, int nrentries);


/*----------------------------------------------------------------------------------------  */

#endif /* H5_HAVE_EFF*/
#endif /*  */
