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



hbool_t overlap_flag = FALSE;

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
    

static uint64_t H5VL_iod_get_difference (uint64_t axe_id1,
					 uint64_t axe_id2);


static size_t H5VL_iod_copy_desc (block_container_t *sm_block,
				  block_container_t *tmp_block,
				  size_t start,
				  size_t j);


static void H5VL_print_block_container (block_container_t *cont,
					size_t num);

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
    HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, CP_FAIL, "Cannot initialize dims array")
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
  int dataset_id;
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
  int num_peers = 0;
  int ret_value = CP_SUCCESS,num_requests = 0, num_datasets = 0;
  int j = 0, request_id = 0, i, current_dset_id = 0, lreq = 0, flag = 0;
  size_t ii;
  uint64_t axe_id;
  


  FUNC_ENTER_NOAPI(NULL)

  if (NULL == queue)
    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, CP_FAIL, "Compactor queue is NULL")
 
  /* Determine the total number of requests in the compactor queue*/
  num_requests = H5VL_iod_get_number_of_requests (queue);

#if DEBUG_COMPACTOR
  fprintf(stderr,"in %s:(%d), Number of requests :%d\n",
	  __FILE__,__LINE__,
	  num_requests);
#endif
  /*Create a local request list -- Allocate more to avoid realloc and memory corruptions*/
  newlist = (request_list_t *) malloc ( 2 * num_requests * sizeof(request_list_t));
  if ( NULL == newlist){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Memory allocation error for request list");
  }

  unique_datasets = (dataset_container_t *) malloc ( num_requests * 
						     sizeof(dataset_container_t));
  if ( NULL == unique_datasets){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Memory allocation error for dataset list");
  }
    
  for (j = 0; j < num_requests; j++){
    unique_datasets[j].num_requests = 0;
    unique_datasets[j].requests = (int *) malloc (num_requests * sizeof(int));
    if (NULL == unique_datasets[j].requests)
      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Memory allocation error for dataset-req list");
  }

  
  /*We have to run through the queue and try to extract all request_type 
   requests and populate the request list*/
  for ( j = 0; j < num_requests; j++)
  {
    
    t_entry = (compactor_entry *) malloc (sizeof(compactor_entry));
    if (NULL == t_entry){
      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL, "Memory allocation error for request list");
    }  
    t_entry->request_id = -1; 

    H5VL_iod_get_request_at (queue, t_entry, j);

    if ( -1 == t_entry->request_id){
#if DEBUG_COMPACTOR
      fprintf(stderr,"in %s:%d Compactor failure case entered \n",
	      __FILE__,__LINE__);
#endif
      HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCOPY, CP_FAIL, "Compactor queue is empty");
    }
    if (t_entry->type_request != request_type){
      free(t_entry);
      t_entry = NULL;
      continue;
    }

    else{

#if DEBUG_COMPACTOR
      fprintf(stderr,"in %s:%d Enters -- means we have requests! \n",
	      __FILE__,__LINE__);
#endif

      /*Setting up values from the input structure*/
      op_data = (op_data_t *)t_entry->input_structure;
      num_peers = t_entry->num_peers;
      input = (dset_io_in_t *)op_data->input;
      source  = HG_Handler_get_addr(op_data->hg_handle);
      iod_id = input->iod_id;
      dataset_id = input->iod_oh.cookie;
      space_id = input->space_id;
      src_id = input->mem_type_id;
      dst_id = input->dset_type_id;
      axe_id = input->axe_id;
      
      bulk_handle = input->bulk_handle;
      
      size = HG_Bulk_handle_get_size(bulk_handle);
      nelmts = (size_t)H5Sget_select_npoints(space_id);
      src_size = H5Tget_size(src_id); /*element size of memorytype */
      dst_size = H5Tget_size(dst_id); /*element size of filetype*/
      
#if DEBUG_COMPACTOR
      fprintf(stderr,"in %s(%d) op_data: %p, iod_id: %llu, dataset_id: %d space: %d, size: %zd\n",
	      __FILE__,__LINE__, (void *)op_data, iod_id, dataset_id, space_id, size);
#endif
      
      if (request_id == 0){
	unique_datasets[num_datasets].dataset = dataset_id;
#if DEBUG_COMPACTOR
	fprintf(stderr, "in %s:%d dataset: %d has the request %d at %d \n",
		__FILE__, __LINE__,
		unique_datasets[num_datasets].dataset,
		request_id,
		unique_datasets[num_datasets].num_requests);
#endif

	/* convenience def */
	lreq = unique_datasets[num_datasets].num_requests;
	unique_datasets[num_datasets].requests[lreq]= request_id;
	unique_datasets[num_datasets].num_requests += 1;
	current_dset = dataset_id;
        current_dset_id = num_datasets;  
        num_datasets++;
	
      } 
      /*from the second id!*/else{
	
	if (current_dset == (hid_t)dataset_id){
#if DEBUG_COMPACTOR
	  fprintf(stderr, "in %s:%d current dataset: %d has the request %d at %d \n",
		  __FILE__, __LINE__,
		  unique_datasets[current_dset_id].dataset,
		  request_id,
		  unique_datasets[current_dset_id].num_requests);
#endif
	  lreq = unique_datasets[current_dset_id].num_requests;
	  unique_datasets[current_dset_id].requests[lreq] = request_id;
	  unique_datasets[current_dset_id].num_requests++;
	  
	}
	else{
	  
	  flag = 0;
	  for (i = 0; i < num_datasets; i++){
	    if ((hid_t)dataset_id == unique_datasets[i].dataset){
	      lreq = unique_datasets[i].num_requests;
	      unique_datasets[i].requests[lreq] = request_id;
#if DEBUG_COMPACTOR
	      fprintf(stderr, "in %s:%d old dataset: %d has the request %d at %d \n",
		      __FILE__, __LINE__,
		      unique_datasets[i].dataset,
		      request_id,
		      unique_datasets[i].num_requests);
#endif

	      unique_datasets[i].num_requests++;
	      current_dset = dataset_id;
	      current_dset_id = i;
	      flag  = 1;
	    }
	  }
	  if (!flag){
	    /*the iod_id is new and not present in the list*/
	    
	    lreq = unique_datasets[num_datasets].num_requests;
	    unique_datasets[num_datasets].requests[lreq] = request_id;
#if DEBUG_COMPACTOR
	    fprintf(stderr, "in %s: %d new dataset: %d has the request %d at %d \n",
		    __FILE__, __LINE__,
		    unique_datasets[i].dataset,
		    request_id,
		    unique_datasets[i].num_requests);
#endif
	    unique_datasets[num_datasets].num_requests++;
	    current_dset = dataset_id;
	    current_dset_id = num_datasets;
	    num_datasets++;
	  }
	}           
      }
      selection_type = H5Sget_select_type(space_id);
      
      if (selection_type == H5S_SEL_NONE || selection_type == H5S_SEL_ALL){
#if DEBUG_COMPACTOR
	fprintf(stderr,"%s(%d) There is no selection in the dataspace\n",
		__FILE__,__LINE__);
#endif
	HGOTO_ERROR(H5E_DATASPACE, H5E_BADSELECT, CP_FAIL,"There is no selection in the dataspace");
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
	  HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, CP_FAIL, "can't allocate read buffer");

	HG_Bulk_block_handle_create(buf, size, HG_BULK_READWRITE, &bulk_block_handle);
	
	/* Write bulk data here and wait for the data to be there  */
	if(HG_SUCCESS != HG_Bulk_read_all(source, bulk_handle, bulk_block_handle, &bulk_request))
	  HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, CP_FAIL, "can't get data from function shipper");

	/* wait for it to complete */
	if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_BULK_MAX_IDLE_TIME, HG_BULK_STATUS_IGNORE))
	  HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, CP_FAIL, "can't get data from function shipper");

	/* free the bds block handle */
	if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
	  HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, CP_FAIL, "can't free bds block handle");


	/***********************************************************************************/
	/* extract offsets and lengths for this dataspace selection*/

	ret_value = H5Sget_offsets(space_id, dst_size, &offsets, &len, &num_entries);

#if DEBUG_COMPACTOR
	fprintf(stderr,"in %s:%d for request_id: %d ret_value %d, num_entries %d from get_offsets\n", 
		__FILE__,__LINE__, request_id, ret_value, num_entries);

	for (ii = 0; ii < num_entries; ii++){
	  fprintf (stderr,"offsets[%zd]: %lli, size[%zd]: %zd\n",
		   ii, offsets[ii], ii, len[ii]);
	}
#endif
      	newlist[request_id].fblocks = (block_container_t *) malloc 
	  (num_entries * sizeof(block_container_t));
	if (NULL == newlist[request_id].fblocks){
	HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, CP_FAIL,"Allocation error for block container");
	}

      	newlist[request_id].mblocks = (block_container_t *) malloc 
	  (num_entries * sizeof(block_container_t));
	if (NULL == newlist[request_id].mblocks){
	  HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, CP_FAIL,"Allocation error for block container")
	}

	newlist[request_id].request_id = request_id;
	newlist[request_id].merged = NOT_MERGED;
	newlist[request_id].num_fblocks = num_entries;

	newlist[request_id].num_mblocks = num_entries; 
	newlist[request_id].elementsize = dst_size;
	newlist[request_id].dataset_id = dataset_id;
	newlist[request_id].selection_id = space_id;
	newlist[request_id].axe_id = axe_id;
	newlist[request_id].num_peers = num_peers;

	/*Incase its not merged, to call the I/O operation 
          directly with selection and memory descriptor*/
	newlist[request_id].mem_buf = (void *)buf; 
	newlist[request_id].mem_length = buf_size;
	newlist[request_id].op_data = op_data;
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
#if DEBUG_COMPACTOR
	    fprintf(stderr, "%s(%d) Buffer does not match the selection offsets\n",
		    __FILE__,__LINE__);
#endif
	    HGOTO_ERROR(H5E_HEAP, H5E_BADRANGE, FAIL,"Buffer does not match the selection offsets");
	  }
#if DEBUG_COMPACTOR
	  fprintf(stderr,
		  "\n %zd: foffsets %lli,  flen: %zd, moffset: %lli, mlen : %zd\n",
		  ii,offsets[ii],len[ii],local_mcont_ptr[ii].offset, len[ii]);
#endif

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

#if DEBUG_COMPACTOR
  fprintf(stderr,"%s:%d N-entires : %d\n",
	  __FILE__,__LINE__, request_id);
  fprintf(stderr,"%s:%d Compactor Request List \n",__FILE__,__LINE__);
  fprintf(stderr,"Compactor-TUPLE: id -- AXE-id -- dataset -- dataspace -- fileblocks -- memblocks -- num_peers\n");
  for ( i = 0; i < request_id; i++){
    fprintf(stderr, "Compactor-Entry: %d -- %llu -- %d -- %d -- %zd -- %zd -- %d\n",
	    newlist[i].request_id,
	    newlist[i].axe_id,
	    newlist[i].dataset_id,
	    newlist[i].selection_id,
	    newlist[i].num_fblocks,
	    newlist[i].num_mblocks,
	    newlist[i].num_peers);
  }


  for ( i = 0; i < num_datasets; i++){
    fprintf(stderr, "%s:%d dataset: %d has %d requests \n",
	    __FILE__, __LINE__,
	    unique_datasets[i].dataset,
	    unique_datasets[i].num_requests);
    for (j = 0; j < unique_datasets[i].num_requests; j++){
      fprintf (stderr, "Compactor request %d\n", unique_datasets[i].requests[j]);
    }
    fprintf(stderr,"\n");

  }
#endif
  
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

int H5VL_iod_select_overlap (hid_t dataspace_1, uint64_t axe_id_1,
			     hid_t dataspace_2, uint64_t axe_id_2,
			     hid_t *res_dataspace, int num_peers)
{
  
  hid_t dataspace_3;
  hsize_t overlap, np;
  hid_t ret_value;
  uint64_t difference;


#if DEBUG_COMPACTOR
  hsize_t     *bound_start;
  hsize_t     *bound_end;
#endif

  const H5S_t *space = NULL, *space_1 = NULL;

  FUNC_ENTER_NOAPI(NULL)

  /*Check validity of dataspaces before getting intersection!*/
    
  if(NULL == (space = (const H5S_t *)H5I_object_verify(dataspace_1, H5I_DATASPACE))){

#if DEBUG_COMPACTOR
    fprintf (stderr,"dataspace: %d is not a dataspace\n",dataspace_1);
#endif

    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "dataspace1 not a dataspace");

  }
  
  fprintf(stderr,"in %s:%d num_peers: %d\n",
	  __FILE__,__LINE__,num_peers);
  
  if(H5S_SELECT_VALID(space) != TRUE){

#if DEBUG_COMPACTOR
    fprintf (stderr,"dataspace: %d is not a valid dataspace\n",dataspace_1);
#endif

    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "Not a valid dataspace")

  }      
  if(NULL == (space_1 = (const H5S_t *)H5I_object_verify(dataspace_2, H5I_DATASPACE))){

#if DEBUG_COMPACTOR
      fprintf (stderr,"dataspace: %d is not a dataspace\n",dataspace_2); 
#endif

      HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "dataspace2 not a dataspace");
  }
    
  if(H5S_SELECT_VALID(space_1) != TRUE){

#if DEBUG_COMPACTOR
    fprintf (stderr,"dataspace: %d is not a valid dataspace\n",dataspace_2);
#endif

    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "Not a valid dataspace");
  }
 

#if DEBUG_COMPACTOR
  bound_start = (hsize_t *) malloc (2 * sizeof(hsize_t));
  bound_end  =  (hsize_t *) malloc (2 * sizeof(hsize_t));

  H5Sget_select_bounds(dataspace_1,
		       bound_start,
		       bound_end);   
  fprintf(stderr, "in %s:%d Selection 1 : Start {%lli, %lli}, End {%lli, %lli}\n",
	  __FILE__,__LINE__,
	  bound_start[0], bound_start[1], bound_end[0], bound_end[1]);      

  H5Sget_select_bounds(dataspace_2, bound_start, bound_end) ;    
  fprintf(stderr, "in %s:%d Selection 2 : Start {%lli, %lli}, End {%lli, %lli}\n",
	  __FILE__, __LINE__,
	 bound_start[0], bound_start[1], bound_end[0], bound_end[1]);      
#endif

  difference = H5VL_iod_get_difference(axe_id_1, 
				       axe_id_2);

#if DEBUG_COMPACTOR
  fprintf (stderr, "in %s:%d AXE_ID_1: %llu, AXE_ID_2: %llu, difference : %llu, range: %lf\n",
	   __FILE__,__LINE__, axe_id_1, axe_id_2, difference, ((pow(2.0,64.0) - 1)/num_peers));
#endif

  if (difference >= ((pow(2.0,64.0) - 1)/num_peers) ){
    
    /*Assumption here 1 server can hold upto 10000 processes*/
#if DEBUG_COMPACTOR   
    fprintf (stderr, "in %s:%d difference : %llu, range: %lf \n", 
	     __FILE__, __LINE__,
	     difference, 
	     ((pow(2.0,64.0) - 1)/num_peers));
#endif
    /*Get the intersection*/
    dataspace_3 =  H5Scombine_select(dataspace_1, H5S_SELECT_AND, dataspace_2); 
    overlap = H5Sget_select_npoints(dataspace_3);
    ret_value = (int) overlap;
  }
  else{
    /* If they are from the same process, even if they overlap
       its temporal, so its OK*/
    overlap_flag = true;
    overlap = 0;
    ret_value = 0; 

  }

#if DEBUG_COMPACTOR
  fprintf(stderr,"in %s:%d OVERLAP VALUE is %lli\n", 
	  __FILE__, __LINE__,
	  overlap);
#endif

  if ( 0 == ret_value){
    /*There is no overlap, lets merge the selections*/
    *res_dataspace =  H5Scombine_select(dataspace_1, H5S_SELECT_OR, dataspace_2);      
  }
  else{
    /*The requests overlap*/
    np = H5Sget_select_npoints(dataspace_1);
    if (np < (hsize_t)H5Sget_select_npoints(dataspace_2) ){
      *res_dataspace = dataspace_1;

#if DEBUG_COMPACTOR
      fprintf(stderr,"in %s:%d DATASPACE 1 is bigger\n",
	      __FILE__, __LINE__);
#endif

    }
    else{
      *res_dataspace = dataspace_2;

#if DEBUG_COMPACTOR
      fprintf(stderr,"in  %s:%d DATASPACE 2 is bigger\n",
	      __FILE__, __LINE__);
#endif

    }
  }

#if DEBUG_COMPACTOR
  fprintf (stderr,"in %s:%d Res dataspace : %d\n",
	   __FILE__, __LINE__, *res_dataspace);
  H5Sget_select_bounds(*res_dataspace,
		       bound_start,
		       bound_end);   
  fprintf(stderr, "in %s:%d Result : Start {%lli, %lli}, End {%lli, %lli}\n",
	  __FILE__,__LINE__,
	  bound_start[0], bound_start[1], bound_end[0], bound_end[1]);      
#endif

  
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

int H5VL_iod_compact_requests (request_list_t *list, int *total_requests,
			       int num_requests, int *request_list)
{
  
  hid_t res_dataspace, current_space, last_merged;
  int ret_value = CP_SUCCESS;
  int *lselected_req = NULL;
  int num_selected = 0;
  size_t m_elmnt_size = 0;
  int i, merge_flag = 0;
  int original_requests = *total_requests;
  uint64_t current_axe_id;
  
  FUNC_ENTER_NOAPI(NULL)

#if DEBUG_COMPACTOR
  fprintf(stderr,"%s:%d Entering compact requests with  %d requests\n\n", 
	  __FILE__, __LINE__, num_requests);
#endif

  /* if there is only 1 request no point compacting. So we quit*/
  if (num_requests <= 1  || *total_requests <= 1){
    ret_value = CP_FAIL;
    goto done;
  }
  
  lselected_req = (int *) malloc (num_requests * 
				  sizeof(int));
  if ( NULL == lselected_req){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Memory allocation error for selected requests")
  }

#if DEBUG_COMPACTOR
  fprintf(stderr,"%s:%d requests: %d and %d  have to be checked for merging\n",
	  __FILE__,__LINE__,
	  request_list[0], request_list[1]);
  current_space = list[request_list[0]].selection_id;
  current_axe_id = list[request_list[0]].axe_id;

  fprintf (stderr,"%s:%d Current Space ID : %d\n", 
	   __FILE__,__LINE__, current_space);
  fprintf (stderr,"%s:%d Merged with : %d\n", __FILE__, __LINE__,
	   list[request_list[1]].selection_id);
#endif
  merge_flag = 0;
  m_elmnt_size = list[request_list[0]].elementsize;
  if ( 0 == H5VL_iod_select_overlap ( current_space, 
				      current_axe_id,
				      list[request_list[1]].selection_id,
				      list[request_list[1]].axe_id,
				      &res_dataspace,
				      list[request_list[1]].num_peers)){
    lselected_req[0] = 0;
    lselected_req[1] = 1;
    list[request_list[0]].merged = USED_IN_MERGING;
    list[request_list[1]].merged = USED_IN_MERGING;
    num_selected += 2;
    if (!merge_flag){
      merge_flag = 1;
    }
    last_merged = res_dataspace; /*update the last merged!*/
  }
  else{
    if (current_space == res_dataspace){
      lselected_req[0] = 0;
      list[request_list[0]].merged = USED_IN_MERGING;
    }
    else{
      lselected_req[0] = 1;
      current_axe_id = list[request_list[1]].axe_id;
      list[request_list[1]].merged = USED_IN_MERGING;
    }
    num_selected += 1;
  }

#if DEBUG_COMPACTOR
  fprintf(stderr, "%s:%d Num Selected : %d\n",__FILE__, __LINE__,
	  num_selected);
#endif

  current_space = res_dataspace;
  last_merged = res_dataspace;

  for (i = 2; i < num_requests; i++){
    if ( 0 == H5VL_iod_select_overlap ( current_space, 
					current_axe_id,
					list[request_list[i]].selection_id,
				        list[request_list[i]].axe_id,
					&res_dataspace,
					list[request_list[i]].num_peers)){
      /*The dataspaces do not overlap*/
      lselected_req[num_selected] = i ;
      num_selected++;
      if (!merge_flag){
	merge_flag = 1;
      }
      list[request_list[i]].merged = USED_IN_MERGING;
      last_merged = res_dataspace; /*update the last merged!*/
    }
    else{
      if (current_space != res_dataspace){
	lselected_req[num_selected] = i;
	current_axe_id = list[request_list[i]].axe_id;
 	if (merge_flag){
	  merge_flag = 0;
	  /* We need  to create a request list entry for the merged selection */
	  list[original_requests].request_id = list[original_requests - 1].request_id + 1;
	  list[original_requests].merged = MERGED ;
	  list[original_requests].selection_id = last_merged;
	  original_requests++;	  
	  if (CP_SUCCESS != H5VL_iod_construct_merged_request(list,
							      m_elmnt_size,
							      &list[original_requests - 1],
							      lselected_req,
							      num_selected, request_list,
							      num_requests)){
#if DEBUG_COMPACTOR
	    fprintf(stderr, "in %s:%d Error while constructing merged request",
		    __FILE__,__LINE__);
	    
#endif
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, CP_FAIL, "cannot construct merged request");
	  }
	  num_selected = 0;

	  if (lselected_req != NULL){
	    free(lselected_req);

	    lselected_req = NULL;
	    lselected_req = (int *) malloc (num_requests * 
					    sizeof(int));
	    if ( NULL == lselected_req){
	      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Memory allocation error for selected requests");
	    }
	  }
	}
      }
    }
    
    /* update the current_space to the merged space or the 
     largest space*/
    current_space = res_dataspace;
  }

 

#if DEBUG_COMPACTOR
  fprintf(stderr, "%s:%d num_selected : %d, current_dataspace : %d, res_dataspace: %d\n",
	  __FILE__, __LINE__,
	  num_selected,
	  current_space,
	  res_dataspace);
#endif

  /*All merging done 
    Are there any left if so just allocate one request item for that*/
  if(merge_flag){
    list[original_requests].request_id = list[original_requests - 1].request_id + 1;
    list[original_requests].dataset_id = list[request_list[0]].dataset_id;
    list[original_requests].merged = MERGED;
    list[original_requests].selection_id = last_merged;
    original_requests++;	  
    if (CP_SUCCESS != H5VL_iod_construct_merged_request(list,
							m_elmnt_size,
							&list[original_requests- 1],
							lselected_req,
							num_selected,
							request_list,
							num_requests)){
#if DEBUG_COMPACTOR
      fprintf(stderr, "in %s:%d Error while constructing merged request",
	      __FILE__,__LINE__);
      
#endif
      HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, CP_FAIL, "cannot construct merged request");
    }
  }
  
#if DEBUG_COMPACTOR
  fprintf (stderr, "in %s:%d Earlier %d requests now %d requests \n",
	   __FILE__,
	   __LINE__,
	   *total_requests,
	   original_requests);
  
  
#endif
  
  if(NULL != lselected_req){
    free(lselected_req);
    lselected_req = NULL;
  }
  for (i = 0; i < original_requests; i ++){
    fprintf(stderr,"%s:%d, Request :%d --> %d\n",
	    __FILE__,
	    __LINE__,
	    i,
	    list[i].merged);
  }

  
  *total_requests = original_requests;
 done:
  FUNC_LEAVE_NOAPI(ret_value);  
  
}/*end H5VL_iod_compact_requests*/

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_reconstruct_overlapped_request
 *
 * Purpose:	Whenever there is a overlapped merged request
 *              we need to construct the offsets/len with
 *              appropriate latest overwritten memory description
 *
 * Return:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_reconstruct_overlapped_request (block_container_t *sf_block,
					     block_container_t *sm_block,
					     block_container_t **out_sf_block,
					     block_container_t **out_sm_block,
					     size_t *revfblks, size_t *revmblks,
					     size_t i, size_t j, 
					     int *changed, int *changed_cnt_ret){
  
  size_t  rem_len = 0, jj;
  size_t rev_mblks, rev_fblks;
  block_container_t *rev_sm_block=NULL, *rev_sf_block=NULL;
  int  ret_value = CP_SUCCESS;
  int changed_cnt = 0, ii;
  FUNC_ENTER_NOAPI(NULL)

  rev_mblks = 0;
  rev_fblks = 0;
  
  rev_sm_block = (block_container_t *) malloc (4 * sizeof (block_container_t));
  if (NULL == rev_sm_block)
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Allocation error for rev_sm_block");
  rev_sf_block = (block_container_t *) malloc (4 * sizeof (block_container_t));
  if (NULL == rev_sf_block)
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Allocation error for rev_sf_block");
  

  fprintf (stderr, "sf[%zd].offset : %lli, sf[%zd].len: %zd, sf[%zd].offset: %lli, sf[%zd].len : %zd\n",
	   i,sf_block[i].offset,i,sf_block[i].len,j,sf_block[j].offset,j,sf_block[j].len);
 
  if(sf_block[i].offset + sf_block[i].len >
     sf_block[j].offset){
    
    if (sf_block[i].offset + sf_block[i].len <= 
	sf_block[j].offset + sf_block[j].len ){
      /*This means the latest write entry starts in-between 
	its previous and ends beyond its last offset..
	So Ideally we will have to modify the ith entry to maintain its
	memory contents till the start of the new write then later write
	takes over!*/
      if (sf_block[i].offset >= sf_block[j].offset){
	/* i and j start at same location but j write more
	   So replace i with j!*/
	rem_len  = sf_block[j].len;
	rev_sf_block[rev_mblks].offset = sf_block[j].offset;
	rev_sm_block[rev_fblks].offset = sm_block[j].offset;
	rev_sf_block[rev_mblks].len = rem_len;
	rev_sm_block[rev_fblks].len = rem_len;
	changed[changed_cnt] = i;
	changed_cnt++;
      }
      else{
	/* j's start offset is different than I but in total they 
	   write the same or j write more.. so modify i's len..
	   the offsets still remain untouched!*/
	rem_len = (size_t)(sf_block[i].offset + sf_block[i].len)
	  - sf_block[j].offset;
	rev_sf_block[rev_fblks].offset = sf_block[i].offset;
	rev_sm_block[rev_mblks].offset = sm_block[i].offset;
	rev_sf_block[rev_fblks].len = rem_len;
	rev_sm_block[rev_mblks].len = rem_len;

	changed[changed_cnt] = i;
	changed_cnt++;

	rev_mblks++; rev_fblks++;
	rev_sf_block[rev_fblks].offset = sf_block[j].offset;
	rev_sm_block[rev_mblks].offset = sm_block[j].offset;
	rev_sf_block[rev_fblks].len = sf_block[j].len; 
	rev_sm_block[rev_mblks].len = sf_block[j].len;
	changed[changed_cnt] = j;
	changed_cnt++;
      }
      rev_mblks++; rev_fblks++;
    }
    else{
      /*This means the latest starts and ends in-between 
	its previous write. So we will end up breaking the two
	writes into 3 or even 4 if required! */
      if (sf_block[i].offset == sf_block[j].offset){
	
	rev_sf_block[rev_fblks].offset = 
	  sf_block[j].offset + sf_block[j].len;
	rev_sf_block[rev_fblks].len = 
	  sf_block[i].len - (sf_block[j].offset + sf_block[j].len);
	rev_sm_block[rev_mblks].offset = 
	  sm_block[i].offset + (sf_block[j].offset + sf_block[j].len);
	rev_sm_block[rev_fblks].len = 
	  sf_block[i].len - (sf_block[j].offset + sf_block[j].len);
	
	rev_fblks++; rev_mblks++;
	changed[changed_cnt] = i;
	changed_cnt++;
	rev_sf_block[rev_fblks].offset = sf_block[j].offset;
	rev_sm_block[rev_mblks].offset = sm_block[j].offset;
	rev_sf_block[rev_fblks].len = sf_block[j].len;
	rev_sm_block[rev_mblks].len = sm_block[j].len;
	changed[changed_cnt] = j;
	changed_cnt++;
      }
      else if (sf_block[i].offset < sf_block[j].offset){
	rev_sf_block[rev_fblks].offset = sf_block[i].offset;
	rev_sm_block[rev_mblks].offset = sm_block[i].offset;
	rev_sf_block[rev_fblks].len = 
	  (size_t)(sf_block[j].offset - sf_block[i].offset);
	rev_sm_block[rev_mblks].len = 
	  (size_t)(sf_block[j].offset - sf_block[i].offset);
	rev_fblks++; rev_mblks++;
	changed[changed_cnt] = i;
	changed_cnt++;
	rem_len = (sf_block[i].offset + sf_block[i].len)- 
	  (sf_block[j].offset + sf_block[j].len);
	rev_sf_block[rev_fblks].offset =
	  sf_block[j].offset + sf_block[j].len;
	rev_sm_block[rev_mblks].offset = 
	  sm_block[i].offset + ((sf_block[j].offset +sf_block[j].len) - sf_block[i].offset);
	rev_sf_block[rev_fblks].len = rem_len;
	rev_sm_block[rev_mblks].len = rem_len;
	changed[changed_cnt] = i;
	changed_cnt++;
	rev_fblks++; rev_mblks++;
	rev_sf_block[rev_fblks].offset = sf_block[j].offset;
	rev_sm_block[rev_mblks].offset = sm_block[j].offset;
	rev_sf_block[rev_fblks].len = sf_block[j].len;
	rev_sm_block[rev_mblks].len = sm_block[j].len;
	changed[changed_cnt] = j;
	changed_cnt++;

      }
      else{
	rem_len = (sf_block[j].offset + sf_block[j].len)
	  - sf_block[i].offset;
	rev_sf_block[rev_fblks].offset = 
	  sf_block[j].offset + sf_block[j].len;
	rev_sm_block[rev_mblks].offset = sm_block[i].offset +
	  rem_len;
	rev_sf_block[rev_fblks].len = 
	  sf_block[i].len - rem_len;
	rev_sm_block[rev_mblks].len = 
	  sm_block[i].len - rem_len;

	changed[changed_cnt] = i;
	changed_cnt++;

	rev_fblks++; rev_mblks++;
	rev_sf_block[rev_fblks].offset = sf_block[j].offset;
	rev_sm_block[rev_mblks].offset = sm_block[j].offset;
	rev_sf_block[rev_fblks].len = sf_block[j].len;
	rev_sm_block[rev_mblks].len = sm_block[j].len;

	changed[changed_cnt] = j;
	changed_cnt++;
      }
      rev_fblks++; rev_mblks++;
    }
  } /*end off+len > j.offset*/
  
  *out_sf_block = rev_sf_block;
  *out_sm_block = rev_sm_block;
    
  *revfblks = rev_fblks;
  *revmblks = rev_mblks;
  *changed_cnt_ret = changed_cnt;
#if DEBUG_COMPACTOR
  for (ii = 0; ii < changed_cnt; ii++){
    fprintf (stderr,"Changed[%d]: %d\n",
	     ii, changed[ii]);
  }
  fprintf (stderr,"rev_fblks: %zd\n",
	   rev_fblks);
  for (jj = 0; jj < rev_fblks; jj++){
    fprintf (stderr, "rev_blks[%zd]: %lli, rev_blks[%zd]: %zd\n",
	     jj, rev_sf_block[jj].offset, jj, rev_sf_block[jj].len);
  }
  fflush(stderr);
#endif
  
done:
  FUNC_LEAVE_NOAPI(ret_value);  

  
}/*end difference comparison */

 


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_construct_merged_request
 *
 * Purpose:	Whenever there is a merged request, we need to construct the
 *              entry with appropriate memory description 
 *
 * Return:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */

 
int H5VL_iod_construct_merged_request (request_list_t *list,
					size_t m_elmnt_size,
					request_list_t *merged_request,
					int *lselected_req, int num_selected,
					int *request_list, int num_requests)
{

  hsize_t *goffsets = NULL, *moffsets = NULL;
  int i, *sorted = NULL;
  size_t fblks = 0, mblks = 0, rev_mblks = 0, rev_fblks = 0;
  size_t loop_blks = 0, new_blks = 0;
  size_t  g_entries = 0, j, m_entries = 0, ii;
  size_t *glens = NULL, blck_cnt = 0, *mlen = NULL;
  block_container_t *sf_block = NULL, *sm_block = NULL;
  block_container_t *f_block = NULL, *m_block = NULL;
  block_container_t *tf_block= NULL, *tm_block = NULL;
  block_container_t *tmp_block= NULL, *tmpf_block = NULL;
  uint64_t *axe_id_list = NULL, difference = 0;
  int ret_value = CP_SUCCESS, set_op_data = 0;
  int *changed = NULL, changed_cnt = 0; 
  int num_peers;

#if DEBUG_COMPACTOR
  size_t k;
#endif

  FUNC_ENTER_NOAPI(NULL)

  if (num_selected > 1){

#if DEBUG_COMPACTOR
    for (i = 0; i < num_selected; i++){
    fprintf (stderr, "in %s:%d lselected[%d]: %d\n", 
	     __FILE__,
	     __LINE__,
	     i,
	     lselected_req[i]);
    }
#endif
    


    fblks = H5VL_iod_get_selected_fblocks_count(lselected_req,
						num_selected,
						list);
    
    sf_block = (block_container_t *) malloc ( fblks * sizeof(block_container_t));
    if (NULL == sf_block)
      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Allocation error for sf_block");
    
    mblks = H5VL_iod_get_selected_mblocks_count(lselected_req,
						num_selected,
						list);
    sm_block = (block_container_t *) malloc (mblks * sizeof(block_container_t));
    if (NULL == sm_block)
      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Allocation error for sm_block");
    
    assert (fblks == mblks);    

    axe_id_list = (uint64_t *) malloc (fblks * sizeof(uint64_t));
    if (NULL == axe_id_list)
      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL, "Allocation error for axe_id_list");

    blck_cnt = 0;

    /*
      Add all the selections that were added/and manually merge them
      Make sure the memory_descriptor is updated accoringly for the 
      merging */
  
    
    for ( i = 0; i < num_requests; i++){
      
      if (CP_SUCCESS ==
	  H5VL_iod_request_exist(request_list[i], 
				 lselected_req, 
				 num_selected)){

	if (!set_op_data){

	  merged_request->op_data = list[request_list[i]].op_data;
	  num_peers = list[request_list[i]].num_peers;
	  set_op_data = 1;

	}

	for (j = 0; j < list[request_list[i]].num_fblocks; j++){
	  sf_block[blck_cnt].offset = list[request_list[i]].fblocks[j].offset;
	  sf_block[blck_cnt].len = list[request_list[i]].fblocks[j].len;
	  sm_block[blck_cnt].offset = list[request_list[i]].mblocks[j].offset;
	  sm_block[blck_cnt].len = list[request_list[i]].mblocks[j].len;
	  axe_id_list[blck_cnt] = list[request_list[i]].axe_id;
	  
#if DEBUG_COMPACTOR
	  fprintf(stderr, "in %s:%d foffset: %lli len: %zd, moffset: %lli, len: %zd\n",
		  __FILE__, __LINE__,
		  sf_block[blck_cnt].offset, sf_block[blck_cnt].len,  sm_block[blck_cnt].offset,
		  sm_block[blck_cnt].len);
#endif
	  blck_cnt++;
	}
      }
    }

    assert(blck_cnt == fblks);  
    
    loop_blks = fblks;
    /* If its the same client, we do allow overlaps*/
    /* In that case its assumed that the requests flow in temporally
       So we can replace the memory buffer with the latest request */

    if (overlap_flag){

      rev_mblks = 0;
      rev_fblks = 0;
      
      changed = (int *) malloc (4 * sizeof (int));
      if (NULL == changed)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Allocation error for changed");

      for (i = 0; i < 4; i ++){
	changed[i] = 0;
      }
 
      f_block = sf_block;
      m_block = sm_block;
      
      for (ii = 0; ii<loop_blks; ii++){
	for (j = ii+1; j< loop_blks; j++){
	  difference = H5VL_iod_get_difference(axe_id_list[ii],
						axe_id_list[j]);
	  if ( difference < ((pow(2.0,64.0) - 1)/num_peers)){
	    
	    if (CP_SUCCESS != H5VL_iod_reconstruct_overlapped_request(f_block,
								      m_block,
								      &tf_block,
								      &tm_block,
								      &rev_fblks,
								      &rev_mblks,
								      ii, j,
								      changed,
								      &changed_cnt)){
	      HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, CP_FAIL, "overlapped request construct error\n");
	    }
	    
	    if (changed_cnt == 0){
#if DEBUG_COMPACTOR
	      fprintf(stderr,
		      "Continuing no change with that combination.. the offsets remain as follows: \n");
	      H5VL_print_block_container (f_block, loop_blks);
	      H5VL_print_block_container (m_block, loop_blks);
#endif
	      continue;
	    }
	    
#if DEBUG_COMPACTOR
	    fprintf (stderr, 
		     "in %s:%d Completed reconstructing overlapped request changed: %zd fblks: %zd, mblks: %zd\n",
		     __FILE__, __LINE__, changed_cnt ,rev_fblks, rev_mblks);
	    for (k = 0; k < rev_fblks; k++){
	      fprintf (stderr, "%zd: OFFSET: %lli, LEN: %zd\n",
		       k, tf_block[k].offset, tm_block[k].len);
	    }
#endif

	    tmp_block = (block_container_t *) malloc ((fblks+(rev_fblks - 2)) * sizeof(block_container_t));
	    if (NULL == tmp_block){
	      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Allocation error for tmp_block");
	    }

	    tmpf_block = (block_container_t *) malloc ((fblks+(rev_fblks - 2)) * sizeof(block_container_t));
	    if (NULL == tmpf_block){
	      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Allocation error for tmpf_block");
	    }

	    new_blks = 0;
	    switch(changed_cnt){
	    case 1:
	      if (changed[0] == (int)ii){
#if DEBUG_COMPACTOR
		fprintf (stderr,"Enters CASE 1 with changed_cnt : %d\n",
			 changed_cnt);
#endif
		  
		H5VL_iod_copy_desc(f_block, tmpf_block, 0, ii);
		new_blks += H5VL_iod_copy_desc(m_block, tmp_block, 0,ii);

#if DEBUG_COMPACTOR
		fprintf (stderr,"***** new_blks :%zd\n", new_blks);
		H5VL_print_block_container (tmpf_block, new_blks);
		H5VL_print_block_container (tmp_block, new_blks);
#endif
		tmpf_block[new_blks].offset = tf_block[0].offset;
		tmpf_block[new_blks].len = tf_block[0].len;
		tmp_block[new_blks].offset = tm_block[0].offset;
		tmp_block[new_blks].len = tm_block[0].len;
		new_blks++;

#if DEBUG_COMPACTOR
		H5VL_print_block_container (tmpf_block, new_blks);
		H5VL_print_block_container (tmp_block, new_blks);

		fprintf (stderr, "ii+1 : %zd, j: %zd\n", ii+1, j);
#endif
		new_blks += H5VL_iod_copy_desc(f_block, tmpf_block, ii+1, j);
		H5VL_iod_copy_desc(m_block, tmp_block, ii+1, j);

		loop_blks -= 1;

		H5VL_iod_copy_desc(f_block, tmpf_block, j+1, loop_blks);
		H5VL_iod_copy_desc(m_block, tmp_block, j+1, loop_blks);
#if DEBUG_COMPACTOR
		H5VL_print_block_container (tmpf_block, loop_blks);
		H5VL_print_block_container (tmp_block, loop_blks);
#endif
		ii -= 1;
	      }
	      break;
	    case 2:
#if DEBUG_COMPACTOR
		fprintf (stderr,"Enters CASE 2 with changed_cnt : %d\n",
			 changed_cnt);
#endif

	      for (i = 0; i < 2; i++){
		if (changed[i] == (int)ii){
		  new_blks += H5VL_iod_copy_desc(f_block, tmpf_block, 0, ii);
		  H5VL_iod_copy_desc(m_block, tmp_block, 0, ii);
#if DEBUG_COMPACTOR
		  fprintf (stderr,"***** new_blks :%zd\n", new_blks);
		  H5VL_print_block_container (tmpf_block, new_blks);
		  H5VL_print_block_container (tmp_block, new_blks);
#endif
  		  tmpf_block[new_blks].offset = tf_block[i].offset;
		  tmpf_block[new_blks].len = tf_block[i].len;
		  tmp_block[new_blks].offset = tm_block[i].offset;
		  tmp_block[new_blks].len = tm_block[i].len;
		  new_blks++;
		}
		else{
 		  if (ii+1 < j){
		    new_blks += H5VL_iod_copy_desc(f_block, tmpf_block, ii+1, j);
		    H5VL_iod_copy_desc(m_block, tmp_block, ii+1, j);
		  }
		  tmpf_block[new_blks].offset = tf_block[i].offset;
		  tmpf_block[new_blks].len = tf_block[i].len;
		  tmp_block[new_blks].offset = tm_block[i].offset;
		  tmp_block[new_blks].len = tm_block[i].len;
		  new_blks++;
		  if (new_blks < loop_blks){
		    H5VL_iod_copy_desc(f_block, tmpf_block, j+1, loop_blks);
		    H5VL_iod_copy_desc(m_block, tmp_block, j+1, loop_blks);
		  }
		}
	      }
	      break;
	    case 3:
	      for (i = 0; i < 3; i ++){
		if (changed[i] == (int)ii){
#if DEBUG_COMPACTOR
		  fprintf(stderr,"in Changed CASE 3 cnt: %zd and changed[%d]: %d, loop_blck: %zd\n", 
			  changed_cnt,i,changed[i], loop_blks );

#endif
		  new_blks += H5VL_iod_copy_desc(f_block, tmpf_block, 0, ii);
		  H5VL_iod_copy_desc(m_block, tmp_block, 0, ii);
#if DEBUG_COMPACTOR
		  H5VL_print_block_container (tmpf_block, new_blks);
		  H5VL_print_block_container (tmp_block, new_blks);
#endif

		  tmpf_block[new_blks].offset = tf_block[i].offset;
		  tmpf_block[new_blks].len = tf_block[i].len;
		  tmp_block[new_blks].offset = tm_block[i].offset;
		  tmp_block[new_blks].len = tm_block[i].len;
		  new_blks++;
		  i++;
		  loop_blks += 1;
		  tmpf_block[new_blks].offset = tf_block[i].offset;
		  tmpf_block[new_blks].len = tf_block[i].len;
		  tmp_block[new_blks].offset = tm_block[i].offset;
		  tmp_block[new_blks].len = tm_block[i].len;
		  new_blks++;
#if DEBUG_COMPACTOR
		  fprintf (stderr, "*********** loop_blks: %zd, newblks: %zd\n",
			   loop_blks, new_blks);
		  H5VL_print_block_container (tmpf_block, new_blks);
		  H5VL_print_block_container (tmp_block, new_blks);
#endif

		}
		else{
#if DEBUG_COMPACTOR
		  fprintf(stderr,"in %s:%d Changed CASE 3 with Changed_cnt: %zd and changed[%d]: %d\n", 
			  __FILE__, __LINE__,changed_cnt,i,changed[i] );
#endif
		  if (ii+1 < j){
		    new_blks += H5VL_iod_copy_desc(f_block, tmpf_block, ii+1, j);
		    H5VL_iod_copy_desc(m_block, tmp_block, ii+1, j);
		  }
		  tmpf_block[new_blks].offset = tf_block[i].offset;
		  tmpf_block[new_blks].len = tf_block[i].len;
		  tmp_block[new_blks].offset = tm_block[i].offset;
		  tmp_block[new_blks].len = tm_block[i].len;
		  new_blks++;
#if DEBUG_COMPACTOR
		  fprintf (stderr, "*********** loop_blks: %zd, newblks: %zd, j: %zd\n",
			   loop_blks, new_blks, j);
		  H5VL_print_block_container (tmpf_block, new_blks);
		  H5VL_print_block_container (tmp_block, new_blks);
#endif
		  
		  if (new_blks < loop_blks){ 
		    new_blks += H5VL_iod_copy_desc(f_block, tmpf_block, j+1, loop_blks);
		    H5VL_iod_copy_desc(m_block, tmp_block, j+1, loop_blks);
		  }
		}
	      }
#if DEBUG_COMPACTOR
	      fprintf (stderr, "Total revised blocks : %zd \n", 
		       new_blks);
#endif
	      break;
	    default:
	      ret_value = CP_FAIL;
	      break;
	    }

	    if (new_blks < loop_blks){
	      H5VL_iod_copy_desc(f_block, tmpf_block, new_blks, loop_blks);
	      new_blks += H5VL_iod_copy_desc(m_block, tmp_block, new_blks, loop_blks);
	    }

	    if (NULL != m_block){
	      free(m_block);
	      m_block = NULL;
	    }
	    if (NULL != f_block){
	      free(f_block);
	      f_block = NULL;
	    }
	    if (NULL != tf_block){
	      free(tf_block);
	      tf_block = NULL;
	    }
	    if (NULL != tm_block){
	      free(tm_block);
	      tm_block = NULL;
	    }
	    f_block = tmpf_block;
	    m_block = tmp_block;
	  }
#if DEBUG_COMPACTOR
	  fprintf (stderr,"***********new_blks: %zd loop_blks: %zd**********\n", 
		   new_blks, loop_blks);
	  H5VL_print_block_container (f_block, loop_blks);
	  H5VL_print_block_container (m_block, loop_blks);
#endif
	
	}
      }
      sm_block = m_block;
      sf_block = f_block;
      blck_cnt = loop_blks;
      
    }
  
    
    /*---------------------------------------------------------------------
    Sort the file-offsets and check for contiguity.
    The contiguity check is needed to compare with the merged 
    offsets from the combine-select function. These should be
    same. 
    Once sorted use the sorted indices to get the arrangement of
    memory offests.
    The contiguity does not affect the arrangement
    of memory offsets as the file offsets can even shrink to one 
    entry, but the order of memory offsets will remain constant
    --------------------------------------------------------------------*/

    sorted = (int *) malloc (blck_cnt * sizeof(int));
    if ( NULL == sorted ){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Allocation error for sorted array");
    }


    H5VL_iod_sort_block_container (sf_block, blck_cnt, sorted);
    
     
#if DEBUG_COMPACTOR
    fprintf (stderr, "********************* AFTER Adjusting for overlap **************\n");

    for ( j = 0; j < blck_cnt; j++){
	    fprintf(stderr, "in %s:%d sorted_foffset: %lli sorted_len: %zd, sorted_moffset: %lli, sorted_mlen: %zd\n",
		    __FILE__, __LINE__,
		    sf_block[sorted[j]].offset, sf_block[sorted[j]].len,  sm_block[sorted[j]].offset,
		    sm_block[sorted[j]].len);
	    }
#endif


    moffsets = (hsize_t *) malloc (blck_cnt * sizeof(hsize_t));
    if ( NULL == moffsets){
      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Allocation error for moffsets array");
    }
    
    mlen = (size_t *) malloc (blck_cnt * sizeof(size_t));
    if ( NULL == moffsets){
      HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Allocation error for mlen array");
    }

    /*moffsets and mlen for the selected requests 
      ... manually merging from individual off-len
      pairs*/
    moffsets[0] = sf_block[sorted[0]].offset;
    mlen[0] = sf_block[sorted[0]].len;
    m_entries = 1;
    for ( j = 1; j < blck_cnt; j++){
      
      if (moffsets[m_entries - 1] + mlen[m_entries - 1] ==
	  sf_block[sorted[j]].offset){
	mlen[m_entries - 1] += sf_block[sorted[j]].len;
      }
      else{
	moffsets[m_entries] = sf_block[sorted[j]].offset;
	mlen[m_entries] = sf_block[sorted[j]].len;
	m_entries++;
      }
    }

#if DEBUG_COMPACTOR
    fprintf(stderr, "in %s:%d After merging manually\n", __FILE__, __LINE__);
    for ( j = 0; j < m_entries; j++){
      fprintf(stderr, "in %s:%d foffset: %lli len: %zd\n",
	      __FILE__, __LINE__, moffsets[j], mlen[j]);
    }
    
#endif    
    /* Make merged offset and len arrays for the merged selection */
    ret_value = H5Sget_offsets(merged_request->selection_id, m_elmnt_size,
			       &goffsets, &glens, &g_entries);
    
#if DEBUG_COMPACTOR
    for (j = 0; j < g_entries; j ++){

      fprintf(stderr,"in %s:%d merged_offset[%d]: %lli, merged_len[%d]: %zd\n",
	      __FILE__,
	      __LINE__,
	      j,goffsets[j], j,glens[j]);
    }
#endif
    merged_request->fblocks = (block_container_t *)  malloc (m_entries * 
							     sizeof(block_container_t));
    if (NULL == merged_request->fblocks)
      HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, CP_FAIL,"Allocation error for block container");

    for ( j = 0; j<m_entries; j++){
      merged_request->fblocks[j].offset = goffsets[j];
      merged_request->fblocks[j].len = glens[j];
    }
    merged_request->num_fblocks = m_entries;
    
   /*
    Then compare that with automatically generated offsets
    (the offsets have to match) */

    ret_value = H5VL_iod_compare_offsets (goffsets,g_entries,moffsets,m_entries);
    assert (ret_value == CP_SUCCESS);

    /*At this point all consistency checks have been completed.
      We can comfortable create the memory descriptor for the 
      merged selection*/
    merged_request->mblocks = (block_container_t *) malloc ( blck_cnt * 
							     sizeof(block_container_t));
    if (NULL == merged_request->mblocks)
      HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, CP_FAIL,"Allocation error for block container");
    
    for ( j = 0; j< blck_cnt; j++){ 
      merged_request->mblocks[j].offset = sm_block[sorted[j]].offset;
      merged_request->mblocks[j].len = sm_block[sorted[j]].len;
#if DEBUG_COMPACTOR
      fprintf (stderr, "in %s:%d --  %zd: MOFFSET: %lli, MLEN: %zd\n",
	       __FILE__, __LINE__, j,
	       merged_request->mblocks[j].offset,
	       merged_request->mblocks[j].len);
#endif
    }
    merged_request->num_mblocks = blck_cnt;
  }
  
 /*if num_selected > 1*/
  else{
#if DEBUG_COMPACTOR
    fprintf(stderr,"in %s:%d Should not be here with num_selected :%d \n",
	    __FILE__,
	    __LINE__,
	    num_selected);
#endif
    ret_value = CP_FAIL;
    goto done;
  }
  
 done:
  FUNC_LEAVE_NOAPI(ret_value);  
  
 }/*end  H5VL_iod_construct_merged_request*/

static void H5VL_print_block_container (block_container_t *cont,
					size_t num){
  
  size_t k;
  for (k = 0; k < num; k++){
    fprintf (stderr, "%zd: block.offset: %lli, block.len: %zd \n",
	     k,
	     cont[k].offset, cont[k].len);
  }
}



static size_t H5VL_iod_copy_desc (block_container_t *sm_block,
				 block_container_t *tmp_block,
				 size_t start,
				 size_t j){

  size_t i, cnt  = 0;
#if DEBUG_COMPACTOR
  fprintf (stderr,"start: %zd, end: %zd\n",
	   start, j);
#endif
  for (i = start; i < j; i++){
    tmp_block[i].offset = sm_block[i].offset;
    tmp_block[i].len = sm_block[i].len;
    cnt++;
  }
  return cnt;
}


static uint64_t H5VL_iod_get_difference (uint64_t axe_id_1,
					 uint64_t axe_id_2){
  
  uint64_t difference;

  if(axe_id_1 > axe_id_2)
    difference = axe_id_1 - axe_id_2;
  else
    difference = axe_id_2 - axe_id_1;
  
  return difference;

}/* end H5VL_iod_get_difference  */



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
  fprintf (stderr, "OFFSET 1 : %zd, OFFSET 2: %zd\n",
	   offset1_cnt, offset2_cnt);
  if (offset1_cnt != offset2_cnt){
    ret_value =  CP_FAIL;
    goto done;
  }

  for (i = 0; i < offset1_cnt; i++){
    if (offsets1[i] != offsets2[i]){
      ret_value = CP_FAIL;
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




int H5VL_iod_sort_block_container (block_container_t *io_array,
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
      HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, CP_FAIL,"Sorted-array/io_array/entries is NULL");

    temp_arr = (int*)malloc(num_entries*sizeof(int));
    if (NULL == temp_arr) {
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"Allocation error for temp_arr")
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
