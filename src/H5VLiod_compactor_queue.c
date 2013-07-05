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

#include "H5private.h"		/* Generic Functions			*/
#include "H5Apublic.h"		/* Attributes				*/
#include "H5Dpkg.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"         /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Sprivate.h"		/* Dataspaces				*/
#include "H5Tprivate.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/

#include "H5VLiod_compactor_queue.h"

#ifdef H5_HAVE_EFF

/* 
 * Programmer:  Vishwanath Venkatesan <vish@hdfgroup.gov>
 *              June, 2013
 *
 * Purpose:	Compactor Queue Datastructure Manipulation functions
*/

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_destroy_compactor_queue
 *
 * Purpose:	Function to deallocate the compactor queue
 *
 * Return:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
int H5VL_iod_destroy_compactor_queue(compactor *s)
{

  int ret_value = CP_SUCCESS;

  FUNC_ENTER_NOAPI(CP_FAIL)

  if (NULL == s){
    HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, CP_FAIL, "can't free NULL queue");    
  } 
  else{
    free(s);
    s = NULL;
  }
  
 done:
  FUNC_LEAVE_NOAPI(ret_value)
  
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_init_compactor_queue
 *
 * Purpose:	Function to add requests to the compactor queue
 *
 * Return:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_init_compactor_queue (compactor **s){
  
  compactor *local_queue = NULL;
  int ret_value = CP_SUCCESS;

  FUNC_ENTER_NOAPI_NOINIT

  local_queue = (compactor *) H5MM_malloc (sizeof(compactor));
  if (NULL == local_queue){
    HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, CP_FAIL, "can't allocate queue");
  }

  local_queue->head = local_queue->tail = NULL;
  *s = local_queue;

 done:
  FUNC_LEAVE_NOAPI(ret_value);

  
}

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_add_requests_to_compactor
 *
 * Purpose:	Function to add requests to the compactor queue
 *
 * Return:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_add_requests_to_compactor(compactor* s, compactor_entry request){

  int ret_value = CP_SUCCESS;
  
  FUNC_ENTER_NOAPI_NOINIT
    
  if(NULL == s){
    HGOTO_ERROR(H5E_ARGS, H5E_NOSPACE, CP_FAIL, "Compactor queue does not exists")
  }
  else if(NULL == s->head && NULL == s->tail){
    node* p = (node *)malloc(1 * sizeof(*p));
    if(NULL == p){
      HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, CP_FAIL, "Cannot initialize node p")
    }
    else{
      p->request = request;
      p->prev = p->next = NULL;
      s->head = s->tail = p;
    }
  }
  else if(NULL == s->head || NULL == s->tail){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL, "list's head/tail is null")
  }
  else{
    node* p = (node *) malloc(1 * sizeof *p);
    if(NULL == p){
      HGOTO_ERROR(H5E_SYM, H5E_CANTALLOC, CP_FAIL, "Cannot initialize node p")
    }
    else{
      p->request = request;
      p->prev = p->next = NULL;
      s->tail->next = p;
      p->prev = s->tail;
      s->tail = p;
    }
  }

 done:
  FUNC_LEAVE_NOAPI(ret_value);
} /* end H5VL_iod_add_requests_to_compactor*/
 

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_remove_request_from_compactor
 *
 * Purpose:	Function get the request at front and remove the request
 *              completely from the queue
 *
 * Return:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_remove_request_from_compactor(compactor* s, compactor_entry *entry){
  
  int ret_value = CP_SUCCESS;

  FUNC_ENTER_NOAPI_NOINIT

  if(NULL == s){
      HGOTO_ERROR(H5E_ARGS, H5E_NOSPACE, CP_FAIL, "Compactor queue does not exists")
  }
  else if(NULL == s->head && NULL == s->tail){
    /*Nothing to remove*/
    ret_value = CP_SUCCESS;
  }
  else if(NULL == s->head || NULL == s->tail){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL,"List's head/tail is null\n");
  }
  else{
    node* p = s->head;
    *entry = p->request;
    if(NULL == s->head->next && NULL == s->tail->next){
      s->head = s->tail = NULL;
    }
    else{
      s->head = s->head->next;
    }
    free(p);
    ret_value = CP_SUCCESS;
  }

 done:
  FUNC_LEAVE_NOAPI(ret_value);
} /*end  H5VL_iod_remove_request_from_compactor */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_get_request_at_front
 *
 * Purpose:	Function get the request at front / does not remove the request
 *              completely from the queue
 *
 * Ret_Valueurn:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_get_request_at_front (compactor *s, compactor_entry *entry){
  
  int ret_value = CP_SUCCESS;

  FUNC_ENTER_NOAPI_NOINIT

 if(NULL == s){
    HGOTO_ERROR(H5E_ARGS, H5E_NOSPACE, CP_FAIL, "Compactor queue does not exists")
 } 
 else if(NULL == s->head && NULL == s->tail){
   /*Nothing to remove*/
   ret_value = CP_SUCCESS;
 }
 else{
   node *p = s->head;
   *entry =  p->request;
 }
  
 done:
  FUNC_LEAVE_NOAPI(ret_value);
} /*end H5VL_iod_get_request_at_front */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_remove_element_from_queue
 *
 * Purpose:	Function to remove an element from the queue
 *
 * Ret_Valueurn:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_remove_element_from_queue(compactor* s, node* d){


  if(NULL == d->next){
    s->tail = d->prev;
  }
  else{
    d->next->prev = d->prev;
  }
  
  if(NULL == d->prev){
    s->head = d->next;
  }
  else{
    d->prev->next = d->next;
  }
  
  free(d);


} /* H5VL_iod_remove_element_from_queue */
 

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_get_number_of_requests
 *
 * Purpose:	Function get the number of requests
 *
 * ret_value :  Success:  Number of requests 
 *		Failure:  Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */



int H5VL_iod_get_number_of_requests (compactor *s){
  
  int ret_value = 0;

  FUNC_ENTER_NOAPI(NULL)
 
  if(NULL == s){
      HGOTO_ERROR(H5E_ARGS, H5E_NOSPACE, CP_FAIL, "Compactor queue does not exists")
  }
  else if(NULL == s->head && NULL == s->tail){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL, "List's Head and Tail cannot be null");
  }
  else if(NULL == s->head || NULL == s->tail){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL, "List's head/tail is null");
  }
  else{
    node* p = s->head;
    while (p){
      ret_value++;
      p = p -> next;
    }
  }

 done:
  FUNC_LEAVE_NOAPI(ret_value);
} /*end H5VL_iod_get_number_of_requests  */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_remove_request_from_compactor
 *
 * Purpose:	Function to display the current set of requests
 *
 * Ret_Valueurn:	Success:	CP_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanth Venkatesan
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */


int H5VL_iod_display_compactor_requests(compactor* s)
{

  int ret_value = CP_SUCCESS;
  node *p = NULL;
  FUNC_ENTER_NOAPI(NULL)

  if(NULL == s){
    HGOTO_ERROR(H5E_ARGS, H5E_NOSPACE, CP_FAIL, "Compactor queue does not exists");
  }
  else if(NULL == s->head && NULL == s->tail){
    printf("Empty Queue\n");
  }
  else if(NULL == s->head || NULL == s->tail){
    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, CP_FAIL, "List's head/tail is null");
  }
  else{
    p = s->head;
    while(p){
      printf("num = %d, %d\n", p->request.request_id, 
	     p->request.type_request);
      p = p->next;
    }
  }

 done:
  FUNC_LEAVE_NOAPI(ret_value);
} /*end  H5VL_iod_display_compactor_requests*/

#endif /*end H5_HAVE_EFF*/
