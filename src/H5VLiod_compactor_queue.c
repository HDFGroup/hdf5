#include "H5VLiod_compactor_queue.h"

 
int H5VL_add_requests_to_compactor(compactor* s, compactor_entry request){

  int ret;
  
  if(NULL == s){
    fprintf(stderr, "IN: %s @ %d: Invalid Args\n", __FILE__, __LINE__);
    ret = ERROR;
  }
  
  else if(NULL == s->head && NULL == s->tail){
    node* p = malloc(1 * sizeof(*p));
    if(NULL == p){
      fprintf(stderr,"IN: %s @%d: Out of Memory\n", __FILE__, __LINE__);
      ret = ERROR;
    }
    else{
      p->request = request;
      p->prev = p->next = NULL;
      
      s->head = s->tail = p;
      ret = SUCCESS;
    }
  }
  
  else if(NULL == s->head || NULL == s->tail){
    fprintf(stderr, "IN: %s @%d: Serious error.", __FILE__, __LINE__);
    fprintf(stderr,"List one of the list's head/tail is null while other is not\n");
    ret = ERROR;
  }
  
  else{
    node* p = malloc(1 * sizeof *p);
    if(NULL == p){
      fprintf(stderr,"IN: %s @%d: Out of Memory\n", __FILE__, __LINE__);
      ret = ERROR;
    }
    else{
      p->request = request;
      p->prev = p->next = NULL;
      s->tail->next = p;
      p->prev = s->tail;
      s->tail = p;
      ret = SUCCESS;
    }
  }

  return ret;
}
 
int H5VL_remove_request_from_compactor(compactor* s, compactor_entry *entry){
  
  int ret;

  if(NULL == s){
    fprintf(stderr, "IN: %s @ %d: Invalid Args\n", __FILE__, __LINE__);
    ret = ERROR;
  }

  else if(NULL == s->head && NULL == s->tail){
    printf("Nothing to Remove_Request_From_Queue()\n");
    ret = SUCCESS;
  }

  else if(NULL == s->head || NULL == s->tail){
    fprintf(stderr, "IN: %s @%d: Serious error.", __FILE__, __LINE__);
    fprintf(stderr,"List one of the list's head/tail is null while other is not\n");
    ret = ERROR;
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
    ret = SUCCESS;
  }

  return ret;
}
 
int H5VL_get_request_at_front (compactor *s, compactor_entry *entry){
  
  int ret;

  if(NULL == s){
    fprintf(stderr, "IN: %s @ %d: Invalid Args\n", __FILE__, __LINE__);
    ret = ERROR;
  }
  
  else if(NULL == s->head && NULL == s->tail){
    printf("Nothing to Remove()\n");
    ret = SUCCESS;
  }
  else{
    node *p = s->head;
    *entry =  p->request;
  }

}

void H5VL_remove_element(compactor* s, node* d){
  
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
}
 

int H5VL_get_number_of_requests (compactor *s){
  
  int cnt = 0;
  if(NULL == s){
    fprintf(stderr, "IN: %s @ %d: Invalid Args\n", __FILE__, __LINE__);
  }
  else if(NULL == s->head && NULL == s->tail){
    printf("Empty Queue\n");
  }
  else if(NULL == s->head || NULL == s->tail){
    fprintf(stderr, "IN: %s @%d: Error.", __FILE__, __LINE__);
    fprintf(stderr,"List one of the list's head/tail is null while other is not\n");
  }
  else{
    node* p = s->head;
    while (p){
      cnt++;
      p = p -> next;
    }
  }
  return cnt;
}
 
void H5VL_display_compactor_requests(compactor* s)
{
  
  if(NULL == s){
    fprintf(stderr, "IN: %s @ %d: Invalid Args\n", __FILE__, __LINE__);
  }
  else if(NULL == s->head && NULL == s->tail){
    printf("Empty Queue\n");
  }
  else if(NULL == s->head || NULL == s->tail){
    fprintf(stderr, "IN: %s @%d: Error.", __FILE__, __LINE__);
    fprintf(stderr,"List one of the list's head/tail is null while other is not\n");
  }

  else{
    p = s->head;
    while(p){
      printf("num = %d, %d\n", p->request.request_id, p->request.type_request);
      p = p->next;
    }
  }
}
