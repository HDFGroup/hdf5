#ifndef _H5VLiod_COMPACTOR_QUEUE_H
#define _H5VLiod_COMPACTOR_QUEUE_H


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
#define READ 100
#define WRITE 150
#define SUCCESS 0
#define ERROR 1
 
typedef struct {
  int input_structure; /* This needs to be replaced by the pointer 
			to the actual pointer*/
  int type_request;
  int request_id;
  /*may be we can add a structure for holding the contiguous buffer*/
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


int H5VL_add_requests_to_compactor(compactor*, compactor_entry request);
/*Deletes the request after retrieval*/
int H5VL_remove_request_from_compactor(compactor*, compactor_entry *); 
int H5VL_get_request_at_front (compactor *, compactor_entry *);
void H5VL_display_compactor_requests(compactor*);
void H5VL_remove_element(compactor* s, node* d);
int H5VL_get_number_of_requests (compactor *s); 
 
#endif
