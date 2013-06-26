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

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_extract_dims_info
 *
 * Purpose:	Function to extract the number of dimensions/dimensions
 *
 * Return:	Success:	HG_SUCCESS 
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
  
  int ldims, ret_value = HG_SUCCESS;
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
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_create_request_list (hid_t dataset_id, compactor *queue,
				  request_list_t **list, int *numentries,
				  int request_type)
{

  compactor_entry *t_entry = NULL;
  op_data_t *op_data;
  dset_io_in_t *input;
  iod_obj_id_t iod_id;
  hg_bulk_t bulk_handle;
  hg_bulk_request_t bulk_request;
  hg_bulk_block_t bulk_block_handle;
  H5S_sel_type selection_type;
  const H5S_t *space = NULL;
  request_list_t *newlist = NULL;
  hid_t space_id, src_id, dst_id;
  hsize_t nelmts, *offsets=NULL; 
  block_container_t *local_cont_ptr=NULL;
  na_addr_t source;
  void *buf = NULL;
  char *current_pos;
  size_t size, buf_size, src_size, dst_size;
  size_t *len, num_entries, npoints, chk_size;
  int ret_value = HG_SUCCESS,num_requests = 0;
  int i = 0, j = 0, request_id = 0;


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
	if(HG_SUCCESS != HG_Bulk_read_all(source, bulk_handle, bulk_block_handle, &bulk_request))
	  HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper")
	
	/* wait for it to complete */
	if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_BULK_MAX_IDLE_TIME, HG_BULK_STATUS_IGNORE))
	  HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper")
	
	/* free the bds block handle */
	if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
	  HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle")

	/***********************************************************************************/
	/* extract offsets and lengths for this dataspace selection*/

	if(NULL == (space = (const H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
	  HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

	if(H5S_SELECT_VALID(space) != TRUE)
	  HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")

        npoints = H5S_GET_SELECT_NPOINTS(space);    
	ret_value = H5S_get_offsets(space, dst_size, npoints,
				    &offsets, &len, &num_entries);
		

      	newlist[request_id].blocks = (block_container_t *) malloc 
	  (num_entries * sizeof(block_container_t));
	if (NULL == newlist[request_id].blocks){
	  HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, HG_FAIL,"Allocation error for block container")
	}
	
	newlist[request_id].elementsize = dst_size;
	newlist[request_id].dataset_id = dataset_id;
	newlist[request_id].selection_id = space_id;
	newlist[request_id].mem_buffer = (char *)buf;
	newlist[request_id].mem_length = buf_size;
	local_cont_ptr = newlist[request_id].blocks;
	current_pos = (char *)buf;
	chk_size = 0;
	
	for ( i = 0; i < num_entries; i++){
	  local_cont_ptr[i].offset = offsets[i];
	  local_cont_ptr[i].len = len[i];
	  local_cont_ptr[i].mem_buffer = current_pos;
	  if (!(buf_size < (chk_size + len[i]))){
	    current_pos += len[i];
	    chk_size = chk_size + len[i];
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

 done:
  FUNC_LEAVE_NOAPI(ret_value);
} /* end  H5VL_iod_create_requests_list */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_select_overlap
 *
 * Purpose:	Function to check whether two selections overlap
 *
 * Return:	SUCCESS      : Returns 0 which means "No Overlap"
 *		FAILURE      : Anyother value which means there is overlap
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_select_overlap (hid_t dataspace_1 , hid_t dataspace_2){

  hid_t dataspace_3;
  hsize_t overlap;
  int ret_value = HG_SUCCESS;
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

  /*O means there is no overlap, any value greater than 0, there is overlap*/
  ret_value = (int)overlap;

 done:
  FUNC_LEAVE_NOAPI(ret_value);
} /* end H5VL_iod_select_overlap */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_merge_selections
 *
 * Purpose:	Function to merge selections of the same dataset.
 *              
 * Return:	SUCCESS      : The ID for the new merged dataspace object.
 *		FAILURE      : Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

hid_t H5VL_iod_merge_selections (hid_t dataspace1, hid_t dataset1, 
				 hid_t dataspace2, hid_t dataset2){

  hid_t ret_value;

  FUNC_ENTER_NOAPI(NULL)

  if (dataset1 != dataset2){
    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Cannot merge selections of different datasets")
  }
  else{
    ret_value =  H5Scombine_select(dataspace1, H5S_SELECT_OR, dataspace2);      
  }
 
 done:
    FUNC_LEAVE_NOAPI(ret_value);
}/* end H5VL_iod_merge_selection */






#endif /* H5_HAVE_EFF*/
