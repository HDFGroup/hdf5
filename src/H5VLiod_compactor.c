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



#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */
#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */


#include "H5private.h"	         	/* Generic Functions			*/
#include "H5Apublic.h"	        	/* Attributes				*/
#include "H5Dpkg.h"		        /* Dataset functions			*/
#include "H5Spublic.h"                  /* Dataspace functions                  */
#include "H5Eprivate.h"	          	/* Error handling		  	*/
#include "H5Gpkg.h"           		/* Groups		  		*/
#include "H5Iprivate.h"	        	/* IDs			  		*/
#include "H5MMprivate.h"        	/* Memory management			*/
#include "H5Oprivate.h"                 /* Object headers			*/
#include "H5Pprivate.h"  		/* Property lists			*/
#include "H5Sprivate.h" 		/* Dataspaces				*/
#include "H5Tprivate.h" 		/* Datatypes				*/
#include "H5VLprivate.h"        	/* VOL plugins				*/
#include "H5VLiod_compactor_queue.h"    /* Compactor queue datastructures       */
#include "H5VLiod_compactor.h"




#ifdef H5_HAVE_EFF

/* -----------------------------------------------------------------
 * Programmer:  Vishwanath Venkatesan <vish@hdfgroup.gov>
 *              June, 2013
 *
 * Purpose:	Request Compactor server-side routines

 *------------------------------------------------------------------*/

/*--------------Simple helper functions declaration----------------*/

static int H5VL_iod_request_exist (int current_index,
				   int *selected_indices,
				   int num_entries);

static int H5VL_iod_compare_offsets (hsize_t *offsets1,
				     size_t offset1_cnt,
				     hsize_t *offsets2,
				     size_t offset2_cnt);

static size_t H5VL_iod_get_selected_mblocks_count (int *selected_indices,
						   int num_entries,
						   request_list_t *list);

static size_t H5VL_iod_get_selected_fblocks_count (int *selected_indices,
						   int num_entries,
						   request_list_t *list);

static int H5VL_iod_sort_block_container (block_container_t *io_array,
					  size_t num_entries,
					  int *sorted);

/*---------------------------------------------------------------------*/



/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_extract_dims_info
 *
 * Purpose:	Function to extract the number of dimensions/dimensions
 *
 * Return:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_extract_dims_info (hid_t dataspace, 
				int *ndims, 
				hsize_t **dims){
  
  int ldims, ret_value = CP_SUCCESS;
  hsize_t *dims_out = NULL;

  FUNC_ENTER_NOAPI_NOINIT

  ldims =  H5Sget_simple_extent_ndims (dataspace);
  dims_out = (hsize_t *) malloc ( ldims * sizeof(hsize_t));

  if (NULL == dims){
    HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, HG_FAIL, "Cannot initialize dims array")
  }
  
  *ndims = ldims;
  ret_value = H5Sget_simple_extent_dims (dataspace, dims_out, NULL);
  *dims = dims_out;

 done:
  FUNC_LEAVE_NOAPI(ret_value);

} /*end H5VL_iod_extract_dims_info*/

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_create_request_list
 *
 * Purpose:	Function to create a request list for each type of request 
 *              retrieved from the compactor queue
 *
 * Return:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_create_request_list (compactor *queue, request_list_t **list, 
				  int *numentries,   dataset_container_t **u_datasets,
				  int *numdatasets, int request_type)
{

  compactor_entry *t_entry = NULL;
  op_data_t *op_data;
  dset_io_in_t *input;
  iod_obj_id_t iod_id;
  hg_bulk_t bulk_handle;
  hg_bulk_request_t bulk_request;
  hg_bulk_block_t bulk_block_handle;
  H5S_sel_type selection_type;
  request_list_t *newlist = NULL;
  dataset_container_t  *unique_datasets = NULL;
  hid_t space_id, src_id, dst_id, current_dset;
  hsize_t nelmts, *offsets=NULL; 
  block_container_t *local_fcont_ptr=NULL;
  block_container_t *local_mcont_ptr=NULL;
  na_addr_t source;
  void *buf = NULL;
  char *current_pos;
  size_t size, buf_size, src_size, dst_size;
  size_t *len, num_entries, chk_size;
  int ret_value = CP_SUCCESS,num_requests = 0, num_datasets = 0;
  int j = 0, request_id = 0, i, current_dset_id = 0;
  size_t ii;


  FUNC_ENTER_NOAPI(NULL)

  if (NULL == queue)
    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, HG_FAIL, "Compactor queue is NULL")
 
  /* Determine the total number of requests in the compactor queue*/
  num_requests = H5VL_iod_get_number_of_requests (queue);

  /*Create a local request list*/
  newlist = (request_list_t *) malloc ( num_requests * sizeof(request_list_t));
  if ( NULL == newlist){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, HG_FAIL,"Memory allocation error for request list")
  }

  unique_datasets = (dataset_container_t *) malloc ( num_requests * 
						     sizeof(dataset_container_t));
  if ( NULL == unique_datasets){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, HG_FAIL,"Memory allocation error for dataset list")
  }
    
  for (j = 0; j < num_requests; j++){
    unique_datasets[j].num_requests = 0;
    unique_datasets[j].requests = (int *) malloc (num_requests * sizeof(int));
    if (NULL == unique_datasets[j].requests)
      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, HG_FAIL,"Memory allocation error for dataset-req list")
    
  }
  
  /*We have to run through the queue and try to extract all request_type 
   requests and populate the request list*/
  for ( j = 0; j < num_requests; j++)
  {
    
    t_entry = (compactor_entry *) malloc (sizeof(compactor_entry));
    if (NULL == t_entry){
      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, HG_FAIL, "Memory allocation error for request list")
    }  
    t_entry->request_id = -1; 
    
    H5VL_iod_remove_request_from_compactor(queue, t_entry);
    if ( -1 == t_entry->request_id){
          HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCOPY, HG_FAIL, "Compactor queue is empty")
    }
    if (t_entry->type_request != request_type){
      free(t_entry);
      t_entry = NULL;
      continue;
    }
    else{

      /*Setting up values from the input structure*/
      op_data = (op_data_t *)t_entry->input_structure;
      input = (dset_io_in_t *)op_data->input;
      source  = HG_Handler_get_addr(op_data->hg_handle);
      iod_id = input->iod_id;
      space_id = input->space_id;
      src_id = input->mem_type_id;
      dst_id = input->dset_type_id;
      
      bulk_handle = input->bulk_handle;
      
      size = HG_Bulk_handle_get_size(bulk_handle);
      nelmts = (size_t)H5Sget_simple_extent_npoints(space_id);
      src_size = H5Tget_size(src_id); /*element size of memorytype */
      dst_size = H5Tget_size(dst_id); /*element size of filetype*/
      
      
      if (request_id == 0){
	unique_datasets[num_datasets].dataset = iod_id;
	unique_datasets[num_datasets].requests[num_requests] = request_id;
	unique_datasets[num_datasets].num_requests++;
	current_dset = iod_id;
        current_dset_id = num_datasets;  
        num_datasets++;
      } 
      else{
       if (current_dset == (hid_t)iod_id){
	  unique_datasets[current_dset_id].requests[num_requests] = request_id;
	  unique_datasets[current_dset_id].num_requests++;
	}
	else{
	  for (i = 0; i < num_datasets; i++){
	    if ((hid_t)iod_id == unique_datasets[i].dataset){
		 unique_datasets[i].requests[num_requests] = request_id;
		 unique_datasets[num_datasets - 1].num_requests++;
		 current_dset = iod_id;
		 current_dset_id = num_datasets;
	     }
	   }           
	 }
      }    

      selection_type = H5Sget_select_type(space_id);
      
      if (selection_type == H5S_SEL_NONE){
	HGOTO_ERROR(H5E_DATASPACE, H5E_BADSELECT, HG_FAIL,"There is no selection in the dataspace")
      }
      else{
	/* adjust buffer size for datatype conversion */
	if(src_size < dst_size) {
	  buf_size = dst_size * nelmts;
	}
	else{
	  buf_size = src_size * nelmts;
	  assert(buf_size == size);
	}
	
	/*Allocation buffer and retrieving the buffer associated with the selection 
	  through the function shipper*/
	
	if(NULL == (buf = malloc(buf_size)))
	  HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "can't allocate read buffer")
	
	HG_Bulk_block_handle_create(buf, size, HG_BULK_READWRITE, &bulk_block_handle);
	
	/* Write bulk data here and wait for the data to be there  */
	if(CP_SUCCESS != HG_Bulk_read_all(source, bulk_handle, bulk_block_handle, &bulk_request))
	  HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper")
	
	/* wait for it to complete */
	if(CP_SUCCESS != HG_Bulk_wait(bulk_request, HG_BULK_MAX_IDLE_TIME, HG_BULK_STATUS_IGNORE))
	  HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper")
	
	/* free the bds block handle */
	if(CP_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
	  HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle")

	/***********************************************************************************/
	/* extract offsets and lengths for this dataspace selection*/

	ret_value = H5Sget_offsets(space_id, dst_size, &offsets, &len, &num_entries);

      	newlist[request_id].fblocks = (block_container_t *) malloc 
	  (num_entries * sizeof(block_container_t));
	if (NULL == newlist[request_id].fblocks){
	  HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, HG_FAIL,"Allocation error for block container")
	}

      	newlist[request_id].mblocks = (block_container_t *) malloc 
	  (num_entries * sizeof(block_container_t));
	if (NULL == newlist[request_id].mblocks){
	  HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, HG_FAIL,"Allocation error for block container")
	}

	newlist[request_id].request_id = request_id;
	newlist[request_id].merged = 0;
	newlist[request_id].num_fblocks = num_entries;
	newlist[request_id].num_mblocks = num_entries; 
	newlist[request_id].elementsize = dst_size;
	newlist[request_id].dataset_id = iod_id;
	newlist[request_id].selection_id = space_id;
	/*Incase its not merged, to call the I/O operation 
          directly with selection and memory descriptor*/
	newlist[request_id].mem_buffer = (char *)buf; 
	newlist[request_id].mem_length = buf_size;

	local_fcont_ptr = newlist[request_id].fblocks;
	local_mcont_ptr = newlist[request_id].mblocks;

	current_pos = (char *)buf;
	chk_size = 0;
	
	for ( ii = 0; ii < num_entries; ii++){
	  local_fcont_ptr[ii].offset = offsets[ii];
	  local_fcont_ptr[ii].len = len[ii];
	  local_mcont_ptr[ii].offset = (uintptr_t)current_pos;
	  local_mcont_ptr[ii].len = len[ii];
	  if (!(buf_size < (chk_size + len[ii]))){
	    current_pos += len[ii];
	    chk_size = chk_size + len[ii];
	  }
	  else{
	    HGOTO_ERROR(H5E_HEAP, H5E_BADRANGE, FAIL,"Buffer does not match the selection offsets")
	  }
	}
      } /*end else for appropriate selection*/
      request_id++;
    } /*end else for matching request type*/
  
    if (NULL != offsets){
      free(offsets);
      offsets = NULL;
    }
    if (NULL != len){
      free(len);
      len = NULL;
    }
    if (NULL != t_entry){
      free (t_entry);
      t_entry = NULL;
    }
  }
  
  *list = newlist;
  *numentries = request_id;
  *u_datasets = unique_datasets;
  *numdatasets = num_datasets;     
 done:
  FUNC_LEAVE_NOAPI(ret_value);
} /* end  H5VL_iod_create_requests_list */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_select_overlap
 *
 * Purpose:	Function to check whether two selections overlap
 *              If the selections overlap, it returns the largest selection
 *              Else it returns the merged selection
 *
 * Return:	SUCCESS      : 0 if no overlap / > 0 therwise
 *    		FAILURE      : Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_select_overlap (hid_t dataspace_1,
			     hid_t dataspace_2,
			     hid_t *res_dataspace)
{
  
  hid_t dataspace_3;
  hsize_t overlap, np;
  hid_t ret_value;
  const H5S_t *space = NULL, *space_1 = NULL;

  FUNC_ENTER_NOAPI(NULL)
  
  /*Check validity of dataspaces before getting intersection!*/

  if(NULL == (space = (const H5S_t *)H5I_object_verify(dataspace_1, H5I_DATASPACE)))
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "dataspace1 not a dataspace")

  if(H5S_SELECT_VALID(space) != TRUE){
    /*   if(space && H5S_close(space) < 0)
      HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace")
      else*/
      HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "Not a valid dataspace")
  }      
  if(NULL == (space_1 = (const H5S_t *)H5I_object_verify(dataspace_2, H5I_DATASPACE)))
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "dataspace2 not a dataspace")
    
  if(H5S_SELECT_VALID(space_1) != TRUE){
    /*    if(space_1 && H5S_close(space_1) < 0)
      HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace")
      else*/
      HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "Not a valid dataspace")
  }
    
  /*Get the intersection*/
  dataspace_3 =  H5Scombine_select(dataspace_1, H5S_SELECT_AND, dataspace_2); 
  overlap = H5Sget_select_npoints(dataspace_3);
  ret_value = (int) overlap;
  if ( 0 == (int) overlap){
    /*There is no overlap, lets merge the selections*/
    *res_dataspace =  H5Scombine_select(dataspace_1, H5S_SELECT_OR, dataspace_2);      
  }
  else{
    /*The requests overlap*/
    np = H5S_GET_SELECT_NPOINTS(space);
    if (np < (hsize_t)H5S_GET_SELECT_NPOINTS(space_1) )
      *res_dataspace = dataspace_1;
    else
      *res_dataspace = dataspace_2;
  }

 done:
  FUNC_LEAVE_NOAPI(ret_value);
} /* end H5VL_iod_select_overlap */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_compact_request
 *
 * Purpose:	Looks at the request list and tries to compact every request
 *              with every other request. This functions assumes that all
 *              requests belong to the same dataset.
 *
 * Return:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_compact_requests (request_list_t **req_list, int num_requests)
{
  
  request_list_t *list = *req_list;
  hid_t res_dataspace, current_space;
  int ret_value = CP_SUCCESS;
  int *lselected_req = NULL;
  int num_selected = 0;
  request_list_t *merged_request = NULL;  
  hsize_t *goffsets = NULL, *moffsets = NULL;
  block_container_t *sf_block = NULL, *sm_block = NULL;
  size_t *glens = NULL, blck_cnt = 0, *mlen = NULL;
  size_t m_elmnt_size = 0, g_entries = 0, j, m_entries = 0;
  size_t fblks = 0, mblks = 0;
  int i, *sorted = NULL;
  
  FUNC_ENTER_NOAPI(NULL)

  lselected_req = (int *) malloc ( num_requests * sizeof(int));
  if ( NULL == lselected_req){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, HG_FAIL,"Memory allocation error for selected requests")
  }

  current_space = list[0].selection_id;

  if ( 0 == H5VL_iod_select_overlap ( current_space, 
				      list[1].selection_id,
				      &res_dataspace)){
    lselected_req[0] = 0;
    lselected_req[1] = 1;
    list[0].merged = 1;
    list[1].merged = 1;
    m_elmnt_size = list[0].elementsize;
    num_selected += 2;
  }
  else{
    if (current_space == res_dataspace){
      lselected_req[0] = 0;
      list[0].merged = 1;
      m_elmnt_size = list[0].elementsize;
    }
    else{
      lselected_req[0] = 1;
      list[1].merged = 1;
      m_elmnt_size = list[1].elementsize;
    }
    num_selected += 1;
  }
  
  current_space = res_dataspace;
  
  for (i = 2; i < num_requests; i++){
    if ( 0 == H5VL_iod_select_overlap ( current_space, 
					list[i].selection_id,
					&res_dataspace)){
      /*The dataspaces do not overlap*/
      lselected_req[num_selected] = i ;
      num_selected++;
    }
    else{
      if (current_space != res_dataspace){
	lselected_req[num_selected] = i;
      }
    }
    /* update the current_space to the merged space or the 
     largest space*/
    current_space = res_dataspace;
  }



  /* Now we have merged the requests, we have a merged selection */
  /* We need  to create a request list entry for the merged selection */
  merged_request = &list[num_requests];
  merged_request->request_id = list[num_requests - 1].request_id + 1;
  merged_request->merged = 2;

  /*
    Add all the selections that were added/and manually merge them
    Make sure the memory_descriptor is updated accoringly for the 
    merging */

  fblks = H5VL_iod_get_selected_fblocks_count(lselected_req,
					      num_selected,
					      list);
  sf_block = (block_container_t *) malloc ( fblks * sizeof(block_container_t));
  if (NULL == sf_block)
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, HG_FAIL,"Allocation error for sf_block")

  mblks = H5VL_iod_get_selected_mblocks_count(lselected_req,
					      num_selected,
					      list);
    
  sm_block = (block_container_t *) malloc (mblks * sizeof(block_container_t));
  if (NULL == sm_block)
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, HG_FAIL,"Allocation error for sm_block")

  assert (fblks == mblks);    

  blck_cnt = 0;
  for ( i = 0; i < num_requests; i++){
    if (CP_SUCCESS == 
	H5VL_iod_request_exist(i, lselected_req, num_selected)){

        /* At this point the mblocks and fblocks will be different only
	 for the merged selection, and we will have to handle that 
	scenario if we for some reason do two-passes for merging 
	So skipping here the case for num_fblocks != num_mblocks*/

	for (j = blck_cnt; j < list[i].num_fblocks; j++){
	  sf_block[j].offset = list[i].fblocks[j - blck_cnt].offset;
	  sf_block[j].len = list[i].fblocks[j - blck_cnt].len;
	  sm_block[j].offset = list[i].mblocks[j - blck_cnt].offset;
	  sm_block[j].len = list[i].mblocks[j - blck_cnt].len;
	}
	blck_cnt = j;
    }
  }
  /*
    Sort the file-offsets and check for contiguity.
    The contiguity check is needed to compare with the merged 
    offsets from the combine-select function. These should be
    same. 
    Once sorted use the sorted indices to get the arrangement of
    memory offests.
    The contiguity does not affect the arrangement
    of memory offsets as the file offsets can even shrink to one 
    entry, but the order of memory offsets will remain constant
  */

  assert(blck_cnt == fblks);  

  sorted = (int *) malloc (blck_cnt * sizeof(int));
  if ( NULL == sorted ){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, HG_FAIL,"Allocation error for sorted array")
  }

  H5VL_iod_sort_block_container (sf_block,
				 blck_cnt,
				 sorted);
  
  moffsets = (hsize_t *) malloc (blck_cnt * sizeof(hsize_t));
  if ( NULL == moffsets){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, HG_FAIL,"Allocation error for moffsets array")
  }
  mlen = (size_t *) malloc (blck_cnt * sizeof(size_t));
  if ( NULL == moffsets){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, HG_FAIL,"Allocation error for mlen array")
  }
  
  moffsets[0] = sf_block[0].offset;
  mlen[0] = sf_block[0].len;
  for ( j = 1; j < blck_cnt; j++){
    if (moffsets[m_entries] + mlen[m_entries] ==
	sf_block[j].offset){
      mlen[m_entries] += sf_block[j].len;
    }
    else{
      m_entries++;
      moffsets[m_entries] = sf_block[j].offset;
      mlen[m_entries] = sf_block[j].len;
    }
  }

  /* Make merged offset and len arrays for the merged selection */
  ret_value = H5Sget_offsets(current_space, m_elmnt_size,
			     &goffsets, &glens, &g_entries);
  merged_request->fblocks = (block_container_t *) malloc (m_entries * 
							  sizeof(block_container_t));
  if (NULL == merged_request->fblocks)
    HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, HG_FAIL,"Allocation error for block container")
      
  for ( j = 0; j<m_entries; j++){
    merged_request->fblocks[j].offset = goffsets[j];
    merged_request->fblocks[j].len = glens[j];
  }

  /*
    Then compare that with automatically generated offsets
    (the offsets have to match) */
  ret_value = H5VL_iod_compare_offsets (goffsets,g_entries,moffsets,m_entries);
  assert (ret_value == CP_SUCCESS);

  /*At this point all consistency checks have been completed.
    We can comfortable create the memory descriptor for the 
    merged selection*/
  merged_request->mblocks = (block_container_t *) malloc (mblks * 
							  sizeof(block_container_t));
  if (NULL == merged_request->mblocks)
    HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, HG_FAIL,"Allocation error for block container")
      
  for ( j = 0; j< mblks; j++){ 
    merged_request->mblocks[j].offset = sm_block[j].offset;
    merged_request->mblocks[j].len = sm_block[j].len;
  }

  /* We have the merged selection with its memory description.
     append this merged_request towards the end. Proactively
     more memory created than required for requests during 
     generation of dataset-specific requests.
     We can use the merged_flag to skip requests or identify merged
     request*/  

  
 done:
  FUNC_LEAVE_NOAPI(ret_value);  

}/*end H5VL_iod_compact_requests*/


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_request_exist
 *
 * Purpose:	Checks whether the current request was selected for merging
 *
 * Return:	Success:	CP_SUCCESS
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */


static int H5VL_iod_request_exist (int current_index,
				   int *selected_indices,
				   int num_entries)
{
  int i;
  int ret_value = -1;

  for (i = 0; i < num_entries; i++){
    if ( selected_indices[i] == current_index)
      ret_value = CP_SUCCESS;
  }

  return  ret_value;
}/* end H5VL_iod_request_exist */




/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_compare_offsets
 *
 * Purpose:	Compares offset arrays, To be used for sanity check after
 *              compaction
 *
 * Return:	Success:	CP_SUCCESS
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

static int H5VL_iod_compare_offsets (hsize_t *offsets1,
				     size_t offset1_cnt,
				     hsize_t *offsets2,
				     size_t offset2_cnt){
  
  int ret_value = CP_SUCCESS;
  size_t i;
  
  if (offset1_cnt != offset2_cnt){
    ret_value =  -1;
    goto done;
  }

  for (i = 0; i < offset1_cnt; i++){
    if (offsets1[i] != offsets2[i]){
      ret_value = -1;
      goto done;
    }

  }

 done:
  return ret_value;

}/*end H5VL_iod_compare_offsets */




/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_get_selected_mblocks_count
 *
 * Purpose:	Gets the number of memory off-len pair in all requests
 *              Selected
 *
 * Return:	Success:	Positive
 *		Failure:	0
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

static size_t H5VL_iod_get_selected_mblocks_count (int *selected_indices,
						   int num_entries,
						   request_list_t *list)
{

  int i;
  size_t ret_value = 0;
  for ( i = 0; i < num_entries; i++){
    ret_value += list[selected_indices[i]].num_mblocks;
  }
  
  return ret_value;
}/* end H5VL_iod_get_selected_mblocks_count*/


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_get_selected_fblocks_count
 *
 * Purpose:	Gets the number of memory off-len pair in all requests
 *              Selected
 *
 * Return:	Success:	Positive
 *		Failure:	0
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

static size_t H5VL_iod_get_selected_fblocks_count (int *selected_indices,
						   int num_entries,
						   request_list_t *list)
{
   
  int i;
  size_t ret_value = 0;
  for ( i = 0; i < num_entries; i++){
    ret_value += list[selected_indices[i]].num_fblocks;
  }
   return ret_value;
}/* end H5VL_iod_get_selected_fblocks_count*/



/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_sort_block_container
 *
 * Purpose:	Sort a given set of off-len pair in-place by providing a
 *              sorted array
 *
 * Return:	Success:	CP_SUCCESS
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */




static int H5VL_iod_sort_block_container (block_container_t *io_array,
					  size_t num_entries,
					  int *sorted)
{
  
    int i = 0, j = 0;
    size_t lm;
    int left = 0;
    int right = 0;
    int largest = 0;
    int heap_size = num_entries - 1;
    int temp = 0;
    unsigned char done = 0;
    int* temp_arr = NULL;
    int ret_value = CP_SUCCESS;

    FUNC_ENTER_NOAPI(NULL)
    
    if (NULL == sorted || NULL == io_array || num_entries == 0)
      HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, HG_FAIL,"Sorted-array/io_array/entries is NULL");

    temp_arr = (int*)malloc(num_entries*sizeof(int));
    if (NULL == temp_arr) {
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, HG_FAIL,"Allocation error for temp_arr")
    }

    temp_arr[0] = 0;
    for (lm = 1; lm < num_entries; ++lm) {
        temp_arr[lm] = lm;
    }

    /* num_entries can be a large no. so NO RECURSION */
    for (i = num_entries/2-1 ; i>=0 ; i--) {
        done = 0;
        j = i;
        largest = j;

        while (!done) {
            left = j*2+1;
            right = j*2+2;
            if ((left <= heap_size) && 
                (io_array[temp_arr[left]].offset > io_array[temp_arr[j]].offset)) {
                largest = left;
            }
            else {
                largest = j;
            }
            if ((right <= heap_size) && 
                (io_array[temp_arr[right]].offset > 
                 io_array[temp_arr[largest]].offset)) {
                largest = right;
            }
            if (largest != j) {
                temp = temp_arr[largest];
                temp_arr[largest] = temp_arr[j];
                temp_arr[j] = temp;
                j = largest;
            }
            else {
                done = 1;
            }
        }
    }

    for (i = num_entries-1; i >=1; --i) {
        temp = temp_arr[0];
        temp_arr[0] = temp_arr[i];
        temp_arr[i] = temp;            
        heap_size--;            
        done = 0;
        j = 0;
        largest = j;

        while (!done) {
            left =  j*2+1;
            right = j*2+2;
            
            if ((left <= heap_size) && 
                (io_array[temp_arr[left]].offset > 
                 io_array[temp_arr[j]].offset)) {
                largest = left;
            }
            else {
                largest = j;
            }
            if ((right <= heap_size) && 
                (io_array[temp_arr[right]].offset > 
                 io_array[temp_arr[largest]].offset)) {
                largest = right;
            }
            if (largest != j) {
                temp = temp_arr[largest];
                temp_arr[largest] = temp_arr[j];
                temp_arr[j] = temp;
                j = largest;
            }
            else {
                done = 1;
            }
        }
        sorted[i] = temp_arr[i];
    }
    sorted[0] = temp_arr[0];

    if (NULL != temp_arr) {
        free(temp_arr);
        temp_arr = NULL;
    }

 done:
  FUNC_LEAVE_NOAPI(ret_value);
}


#endif /* H5_HAVE_EFF*/
