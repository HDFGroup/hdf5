#ifndef _H5VLiod_COMPACTOR_QUEUE_H
#define _H5VLiod_COMPACTOR_QUEUE_H


#include "H5VLiod_common.h"

#ifdef H5_HAVE_EFF

#include "H5VLiod_server.h"
 
#define READ 100
#define WRITE 150
#define SUCCESS 0
#define ERROR 1


 
typedef struct {
  op_data_t *input_structure; 
  int type_request;
  int request_id;
}compactor_entry;

struct cqueue
{
  compactor_entry request;
  struct cqueue* next;
  struct cqueue* prev;
};
 
typedef struct cqueue node;
	 
struct cqlist
{
  node* head;
  node* tail;
};
 
typedef struct cqlist compactor; 

H5_DLL int H5VL_iod_add_requests_to_compactor(compactor*, compactor_entry request);
H5_DLL int H5VL_iod_remove_request_from_compactor(compactor*, compactor_entry *); 
H5_DLL int H5VL_iod_get_request_at_front (compactor *, compactor_entry *);
H5_DLL int H5VL_iod_display_compactor_requests(compactor*);
H5_DLL int H5VL_iod_remove_element_from_queue(compactor* s, node* d);
H5_DLL int H5VL_iod_get_number_of_requests (compactor *s); 
 
#endif /*H5_HAVE_EFF*/
#endif /*H5VLiod_compactor_queue_H*/
