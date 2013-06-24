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


#include "H5VLiod_compator.h"


/*
  TODO:
  1. When moving this to the FF code, modify the return types to herr_t! (VV)
  2. Change the error handling to have macros!
*/

int H5VL_extract_dims_info (hid_t dataspace, int *ndims, hsize_t **dims)
{
  
  int ldims, status;
  hsize_t *dims_out = NULL;
  ldims =  H5Sget_simple_extent_ndims (dataspace);
  dims_out = (hsize_t *) malloc ( ldims * sizeof(hsize_t));
  if (NULL == dims_out){
    fprintf(stderr, "IN: %s @%d: Memory allocation error for dims", __FILE__, __LINE__);;
    /*Have to handle error here properly!*/
  }

  *ndims = ldims;
  status = H5Sget_simple_extent_dims (dataspace, dims_out, NULL);
  *dims  = dims_out;
 
  return status;
}

/* Preprocessing step, Reads each request and creates a selection list of block container..
with more information */
int H5VL_create_requests_list (hid_t dataset_id, hid_t dataspace_id, 
			       compactor *queue, char *mem_buf, size_t mem_buf_len,
			       request_list_t **list, int *numentries)
{
  
  compactor_entry t_entry;
  selection_list_t *newlist = NULL;
  int num_requests = 0, i = 0;
  hsize_t *dims_out = NULL, numelements;
  hsize_t *offset = NULL;
  size_t *len = NULL, num_requests = 0, elmnt_size = 0, chk_size = 0;
  H5S_sel_type selection_type;
  int request_id = 0;
  hid_t type_id;
  block_container_t *local_cont_ptr=NULL;
  char *current_pos = mem_buf;

  if (NULL == queue){
    fprintf (stderr, "Queue is NULL \n",__FILE__,__LINE__);
  }

  num_requests = H5VL_get_number_of_requests (queue);

  newlist = (request_list_t *) malloc ( num_requests * sizeof(request_list_t));
  if ( NULL == newlist){
    fprintf(stderr, 
	    "IN: %s @%d: Memory allocation error for request list",
	    __FILE__, __LINE__);
  }
  
  H5VL_extract_dims_info (dataspace_id, &ndims, &dims_out);  
  
  for ( request_id = 0; request_id < num_requests; request_id++){
   
    selection_type = H5Sget_select_type(dataspace);
    
    if (selection_type == H5S_SEL_NONE){
      printf ("No Selection is defined \n");
      fprintf(stderr, 
	      "IN: %s @%d: Error with no selection type!\n",
	      __FILE__, __LINE__);
    }

    else{
      type_id = H5Dget_type(dataset_id);
      elmnt_size =  H5Tget_size(type_id); 
      H5Sget_offsets(dataspace_id, elmnt_size, &offsets, &len, &entries);
      newlist[request_id].blocks = (block_container_t *) malloc 
	(entries * sizeof(block_container_t));
      if (NULL == newlist[request_id].blocks){
	fprintf(stderr, "IN: %s @%d: Memory allocation error for block_container\n");
      }
      newlist[request_id].elementsize = elmnt_size;
      newlist[request_id].dataset_id = dataset_id;
      newlist[request_id].selection_id = dataspace_id;
      newlist[request_id].mem_buffer = mem_buf;
      newlist[request_id].mem_length = mem_buf_len;
      local_cont_ptr = newlist[request_id].blocks;
      chk_size = 0;
      for ( i = 0; i < entries; i++){
	local_cont_ptr[i].offset = offset[i];
	local_cont_ptr[i].len = len[i];
	local_cont_ptr[i].mem_buffer = current_pos;
	if (!mem_buf_len < (chk_size + len[i]))
	  current_pos += len[i];
	else
	  fprintf(stderr, "FATAL: IN: %s @%d: mem_buffer size and file-lengths do not match!\n",
		  __FILE__,__LINE__);
      }
    }
  }
  
  *list = newlist;
  *numentries = num_requests;
}

hid_t H5VL_merge_selections (hid_t dataspace1, hid_t dataset1, 
			     hid_t dataspace2, hid_t dataset2){
  hid_t dataspace3;

  if (dataset1 != dataset2){
    fprintf(stderr, "FATAL IN: %s @%d: Cannot combine selections of different datasets!\n",
	    __FILE__,__LINE__);
    return  -1;
  }

  dataspace3 = H5Scombine_select(dataspace1, H5S_SELECT_OR , dataspace2); 

  return dataspace3; 
}


int H5VL_check_overlap (hid_t dataspace1 , hid_t dataspace2 ){


  hid_t dataspace3;
  hsize_t overlap;

  /*Get  the intersection*/
  dataspace_3 =  H5Scombine_select(dataspace_1, H5S_SELECT_AND, dataspace_2); 
  
  /* If there are no points, they don't overlap
     Else they overlap*/
  overlap = H5Sget_select_npoints(dataspace3);

  return (int)overlap;
}

size_t H5VL_get_memory_addr_offset (hsize_t offset, size_t len, block_container_t *blocks, char *mem_buffer

int H5VL_reconstruct_memory_buffer (hid_t dataspace, hid_t dataset, 
				    request_list_t *list, int num_requests,
				    block_container_t **merged_container){
  

  hsize_t *offsets = NULL;
  size_t *lens = NULL, num_entries, elmnt_size;
  hid_t type_id;
  int i, j, k, l;
  block_container_t *local_container = NULL;

  type_id = H5Dget_type(dataset);
  elmnt_size = H5Tget_size(type_id);
  H5Sget_offsets(dataspace, elmntsize, &offsets, &lens, &num_entries);
  
  local_container = (block_container_t *) malloc ( num_entries * sizeof (block_container_t));
  if ( NULL == local_container){
    fprintf (stderr, "IN : %s @%d: block_container_t allocation error! \n",
	     __FILE__,__LINE__);
  }

  for ( i = 0; i < num_entries; i++){
    
    
  }
  
  

  
  
}
