/*
 * Copyright (C) 2000 NCSA
 *		      All rights reserved.
 *
 * Programmer: Quincey Koziol <koziol@ncsa.uiuc.edu>
 *	       Thursday, March 23, 2000
 *
 * Purpose: Manage priority queues of free-lists (of blocks of bytes).
 *      These are used in various places in the library which allocate and
 *      free differently blocks of bytes repeatedly.  Usually the same size
 *      of block is allocated and freed repeatly in a loop, while writing out
 *      chunked data for example, but the blocks may also be of different sizes
 *      from different datasets and an attempt is made to optimize access to
 *      the proper free list of blocks by using these priority queues to
 *      move frequently accessed free lists to the head of the queue.
 */

/* #define H5FL_DEBUG */

#include <H5private.h>		/*library		  */
#include <H5Eprivate.h>		/*error handling	  */
#include <H5MMprivate.h>	/*Core memory management	  */
#include <H5FLprivate.h>	/*Priority queues	  */

#define PABLO_MASK	H5FL_mask
static intn		interface_initialize_g = 0;
#define INTERFACE_INIT	NULL

/*
 * Private type definitions
 */

/* A garbage collection node for priority queues */
typedef struct H5FL_blk_gc_list_t {
    H5FL_blk_head_t *pq;                /* Pointer to the head of the PQ to garbage collect */
    struct H5FL_blk_gc_list_t *next;    /* Pointer to the next node in the list of things to garbage collect */
} H5FL_blk_gc_list_t;

/* A garbage collection node for regular free lists */
typedef struct H5FL_gc_list_t {
    H5FL_head_t *list;              /* Pointer to the head of the list to garbage collect */
    struct H5FL_gc_list_t *next;    /* Pointer to the next node in the list of things to garbage collect */
} H5FL_gc_list_t;

/* A garbage collection node for array free lists */
typedef struct H5FL_gc_arr_list_t {
    H5FL_arr_head_t *list;              /* Pointer to the head of the list to garbage collect */
    struct H5FL_gc_arr_list_t *next;    /* Pointer to the next node in the list of things to garbage collect */
} H5FL_gc_arr_list_t;

/* The head of the list of PQs to garbage collect */
static H5FL_blk_gc_list_t *H5FL_blk_gc_head=NULL;

/* The head of the list of things to garbage collect */
static H5FL_gc_list_t *H5FL_gc_head=NULL;

/* The head of the list of array things to garbage collect */
static H5FL_gc_arr_list_t *H5FL_gc_arr_head=NULL;

/* Macros for turning off free lists in the library */
#define NO_FREE_LISTS
#ifdef NO_FREE_LISTS
#define NO_REG_FREE_LISTS
#define NO_ARR_FREE_LISTS
#define NO_BLK_FREE_LISTS
#endif /* NO_FREE_LISTS */


/*-------------------------------------------------------------------------
 * Function:	H5FL_init
 *
 * Purpose:	Initialize a free list for a certain type.  Right now, this just
 *      adds the free list to the list of things to garbage collect.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, March 24, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
static herr_t
H5FL_init(H5FL_head_t *head)
{
    H5FL_gc_list_t *new_list;       /* Pointer to the node for the new list to garbage collect */
    herr_t ret_value=SUCCEED;       /* return value*/

    FUNC_ENTER (H5FL_init, FAIL);

    /* Allocate a new garbage collection node */
    if (NULL==(new_list = H5MM_malloc(sizeof(H5FL_gc_list_t))))
        HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* Initialize the new garbage collection node */
    new_list->list=head;

    /* Link in to the garbage collection list */
    new_list->next=H5FL_gc_head;
    H5FL_gc_head=new_list;

    /* Indicate that the free list is initialized */
    head->init=1;

    FUNC_LEAVE (ret_value);
}   /* end H5FL_init() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_free
 *
 * Purpose:	Release an object & put on free list
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, March 24, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
void *
H5FL_free(H5FL_head_t *head, void *obj)
{
    H5FL_node_t *temp;      /* Temp. ptr to the new free list node allocated */

    FUNC_ENTER (H5FL_free, NULL);

#ifdef NO_REG_FREE_LISTS
#ifdef H5FL_DEBUG
    HDmemset(obj,255,head->size);
#endif /* H5FL_DEBUG */
    H5MM_xfree(obj);
#else /* NO_REG_FREE_LISTS */
    /* Make certain that the free list is initialized */
    assert(head->init);

    /* Get the pointer to the info header in front of the block to free */
    temp=(H5FL_node_t *)((unsigned char *)obj-offsetof(H5FL_node_t,block));

#ifdef H5FL_DEBUG
    assert(temp->inuse);
    temp->inuse=0;
#endif /* H5FL_DEBUG */

    /* Link into the free list */
    temp->next=head->list;

    /* Point free list at the node freed */
    head->list=temp;

    /* Increment the number of blocks on free list */
    head->onlist++;
#endif /* NO_REG_FREE_LISTS */

    FUNC_LEAVE(NULL);
}   /* end H5FL_free() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_alloc
 *
 * Purpose:	Allocate a block on a free list
 *
 * Return:	Success:	Pointer to a valid object
 * 		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Friday, March 24, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
void *
H5FL_alloc(H5FL_head_t *head, uintn clear)
{
    H5FL_node_t *new_obj;   /* Pointer to the new free list node allocated */
    void *ret_value;        /* Pointer to object to return */

    FUNC_ENTER (H5FL_alloc, NULL);

#ifdef NO_REG_FREE_LISTS
    if(clear)
        ret_value=H5MM_calloc(head->size);
    else
        ret_value=H5MM_malloc(head->size);
#else /* NO_REG_FREE_LISTS */
    /* Make certain the list is initialized first */
    if(!head->init)
        H5FL_init(head);

    /* Check for nodes available on the free list first */
    if(head->list!=NULL) {
        /* Get a pointer to the block on the free list */
        ret_value=&(head->list->block);

#ifdef H5FL_DEBUG
        head->list->inuse=1;
#endif /* H5FL_DEBUG */

        /* Remove node from free list */
        head->list=head->list->next;

        /* Decrement the number of blocks on free list */
        head->onlist--;
    } /* end if */
    /* Otherwise allocate a node */
    else {
        if (NULL==(new_obj = H5MM_malloc(sizeof(H5FL_node_t)+(head->size-1))))
            HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

#ifdef H5FL_DEBUG
        new_obj->inuse=1;
#endif /* H5FL_DEBUG */

        /* Increment the number of blocks allocated in list */
        head->allocated++;

        /* Get a pointer to the new block */
        ret_value=&(new_obj->block);
    } /* end else */

    /* Clear to zeros, if asked */
    if(clear)
        HDmemset(ret_value,0,head->size);
#endif /* NO_REG_FREE_LISTS */

    FUNC_LEAVE (ret_value);
}   /* end H5FL_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_gc
 *
 * Purpose:	Garbage collect on all the object free lists
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, March 24, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
static herr_t
H5FL_gc(void)
{
    H5FL_gc_list_t *gc_list;    /* Pointer into the list of things to garbage collect */
    H5FL_head_t *list_head; /* Pointer to head of free list to garbage collect */
    H5FL_node_t *free_list; /* Pointer to nodes in free list being garbage collected */
    void *tmp;      /* Temporary node pointer */
    
    /* FUNC_ENTER_INIT() should not be called, it causes an infinite loop at library termination */
    H5_trace(FALSE, "H5FL_gc", "");

    /* Walk through all the free lists, free()'ing the nodes */
    gc_list=H5FL_gc_head;
    while(gc_list!=NULL) {
        /* Get the pointer to the list head */
        list_head=gc_list->list;

        /* For each free list being garbage collected, walk through the nodes and free them */
        free_list=list_head->list;
        while(free_list!=NULL) {
            tmp=free_list->next;

            /* Decrement the count of nodes allocated and free the node */
            list_head->allocated--;

#ifdef H5FL_DEBUG
            assert(!free_list->inuse);
#endif /* H5FL_DEBUG */

            H5MM_xfree(free_list);

            free_list=tmp;
        } /* end while */

        /* Indicate no free nodes on the free list */
        list_head->list=NULL;
        list_head->onlist=0;

        /* Go on to the next free list to garbage collect */
        gc_list=gc_list->next;
    } /* end while */

    H5_trace(TRUE, NULL, "e", SUCCEED);
    return(SUCCEED);
}   /* end H5FL_gc() */


/*--------------------------------------------------------------------------
 NAME
    H5FL_term
 PURPOSE
    Terminate various H5FL object free lists
 USAGE
    intn H5FL_term()
 RETURNS
    Success:	Positive if any action might have caused a change in some
                other interface; zero otherwise.
   	Failure:	Negative
 DESCRIPTION
    Release any resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static intn
H5FL_term(void)
{
    H5FL_gc_list_t *left;   /* pointer to garbage collection lists with work left */
    H5FL_gc_list_t *tmp;    /* Temporary pointer to a garbage collection node */
    
    /* Free the nodes on the garbage collection list, keeping nodes with allocations outstanding */
    left=NULL;
    while(H5FL_gc_head!=NULL) {
        tmp=H5FL_gc_head->next;

#ifdef H5FL_DEBUG
printf("H5FL_term: head->name=%s, head->allocated=%d\n", H5FL_gc_head->list->name,(int)H5FL_gc_head->list->allocated);
#endif /* H5FL_DEBUG */
        /* Check if the list has allocations outstanding */
        if(H5FL_gc_head->list->allocated>0) {
            /* Add free list to the list of nodes with allocations open still */
            H5FL_gc_head->next=left;
            left=H5FL_gc_head;
        } /* end if */
        /* No allocations left open for list, get rid of it */
        else {
            /* Reset the "initialized" flag, in case we restat this list somehow (I don't know how..) */
            H5FL_gc_head->list->init=0;

            /* Free the node from the garbage collection list */
            H5MM_xfree(H5FL_gc_head);
        } /* end else */

        H5FL_gc_head=tmp;
    } /* end while */

    /* Point to the list of nodes left with allocations open, if any */
    H5FL_gc_head=left;
    
    return (H5FL_gc_head!=NULL ? 1 : 0);
}


/*-------------------------------------------------------------------------
 * Function:	H5FL_blk_find_list
 *
 * Purpose:	Finds the free list for blocks of a given size.  Also moves that
 *      free list node to the head of the priority queue (if it isn't there
 *      already).  This routine does not manage the actual free list, it just
 *      works with the priority queue.
 *
 * Return:	Success:	valid pointer to the free list node
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		Thursday, March  23, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5FL_blk_node_t *
H5FL_blk_find_list(H5FL_blk_node_t **head, size_t size)
{
    H5FL_blk_node_t *temp;  /* Temp. pointer to node in the native list */
    H5FL_blk_node_t *ret_value=NULL;

    FUNC_ENTER(H5FL_blk_find_list, NULL);

    /* Find the correct free list */
    temp=*head;
    while(temp!=NULL && temp->size!=size)
        temp=temp->next;

    /* If the free list exists, move it to the front of the queue, if it's not there already */
    if(temp!=NULL && temp!=*head) {
        /* Take the node found out of it's current position */
        if(temp->next==NULL) {
            temp->prev->next=NULL;
        } /* end if */
        else {
            temp->prev->next=temp->next;
            temp->next->prev=temp->prev;
        } /* end else */

        /* Move the found node to the head of the list */
        temp->prev=NULL;
        temp->next=*head;
        (*head)->prev=temp;
        *head=temp;
    } /* end if */
    
    ret_value=temp;

    FUNC_LEAVE(ret_value);
} /* end H5FL_blk_find_list() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_blk_create_list
 *
 * Purpose:	Creates a new free list for blocks of the given size at the
 *      head of the priority queue.
 *
 * Return:	Success:	valid pointer to the free list node
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		Thursday, March  23, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5FL_blk_node_t *
H5FL_blk_create_list(H5FL_blk_node_t **head, size_t size)
{
    H5FL_blk_node_t *temp;  /* Temp. pointer to node in the list */
    H5FL_blk_node_t *ret_value=NULL;

    FUNC_ENTER(H5FL_blk_create_list, NULL);

    /* Allocate room for the new free list node */
    if(NULL==(temp=H5MM_malloc(sizeof(H5FL_blk_node_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for chunk info");
    
    /* Set the correct values for the new free list */
    temp->size=size;
    temp->list=NULL;

    /* Attach to head of priority queue */
    if(*head==NULL) {
        *head=temp;
        temp->next=temp->prev=NULL;
    } /* end if */
    else {
        temp->next=*head;
        (*head)->prev=temp;
        temp->prev=NULL;
        *head=temp;
    } /* end else */

    ret_value=temp;

done:
    FUNC_LEAVE(ret_value);
} /* end H5FL_blk_create_list() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_blk_init
 *
 * Purpose:	Initialize a priority queue of a certain type.  Right now, this just
 *      adds the PQ to the list of things to garbage collect.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, March 25, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
static herr_t
H5FL_blk_init(H5FL_blk_head_t *head)
{
    H5FL_blk_gc_list_t *new_pq;       /* Pointer to the node for the new list to garbage collect */
    herr_t ret_value=SUCCEED;       /* return value*/

    FUNC_ENTER (H5FL_blk_init, FAIL);

    /* Allocate a new garbage collection node */
    if (NULL==(new_pq = H5MM_malloc(sizeof(H5FL_blk_gc_list_t))))
        HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* Initialize the new garbage collection node */
    new_pq->pq=head;

    /* Link in to the garbage collection list */
    new_pq->next=H5FL_blk_gc_head;
    H5FL_blk_gc_head=new_pq;

    /* Indicate that the PQ is initialized */
    head->init=1;

    FUNC_LEAVE (ret_value);
}   /* end H5FL_blk_init() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_blk_alloc
 *
 * Purpose:	Allocates memory for a block.  This routine is used
 *      instead of malloc because the block can be kept on a free list so
 *      they don't thrash malloc/free as much.
 *
 * Return:	Success:	valid pointer to the block
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		Thursday, March  23, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5FL_blk_alloc(H5FL_blk_head_t *head, size_t size, uintn clear)
{
    H5FL_blk_node_t *free_list;  /* The free list of nodes of correct size */
    H5FL_blk_list_t *temp;  /* Temp. ptr to the new native list allocated */
    void *ret_value;    /* Pointer to the block to return to the user */

    FUNC_ENTER(H5FL_blk_alloc, NULL);

#ifdef NO_BLK_FREE_LISTS
    if(clear)
        ret_value=H5MM_calloc(size);
    else
        ret_value=H5MM_malloc(size);
#else /* NO_BLK_FREE_LISTS */
    /* Make certain the list is initialized first */
    if(!head->init)
        H5FL_blk_init(head);

    /* check if there is a free list for blocks of this size */
    /* and if there are any blocks available on the list */
    if((free_list=H5FL_blk_find_list(&(head->head),size))!=NULL && free_list->list!=NULL) {
        /* Remove the first node from the list and return it */
        ret_value=(void *)&(free_list->list->block);
        free_list->list=free_list->list->next;

        /* Decrement the number of blocks on free list */
        head->onlist--;
    } /* end if */
    /* No free list available, or there are no nodes on the list, allocate a new node to give to the user */
    else { 
        /* Allocate new node, with room for the page info header and the actual page data */
		if(NULL==(temp=H5MM_malloc(sizeof(H5FL_blk_list_t)+(size-1))))
		    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for chunk");

        /* Increment the number of blocks allocated */
        head->allocated++;

        /* Initialize the block allocated */
        temp->size=size;
        temp->next=NULL;

        /* Set the return value to the block itself */
        ret_value=(void *)&(temp->block);
    } /* end else */

    /* Clear the block to zeros, if requested */
    if(clear)
        HDmemset(ret_value,0,size);
#endif /* NO_BLK_FREE_LISTS */

done:
    FUNC_LEAVE(ret_value);
} /* end H5FL_blk_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_blk_free
 *
 * Purpose:	Releases memory for a block.  This routine is used
 *      instead of free because the blocks can be kept on a free list so
 *      they don't thrash malloc/free as much.
 *
 * Return:	Success:	NULL
 *
 *		Failure:	never fails
 *
 * Programmer:	Quincey Koziol
 *		Thursday, March  23, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5FL_blk_free(H5FL_blk_head_t *head, void *block)
{
    H5FL_blk_node_t *free_list;      /* The free list of nodes of correct size */
    H5FL_blk_list_t *temp;      /* Temp. ptr to the new free list node allocated */

    FUNC_ENTER(H5FL_blk_free, NULL);

#ifdef NO_BLK_FREE_LISTS
    H5MM_xfree(block);
#else /* NO_BLK_FREE_LISTS */
    /* Get the pointer to the native block info header in front of the native block to free */
    temp=(H5FL_blk_list_t *)((unsigned char *)block-offsetof(H5FL_blk_list_t,block));

    /* check if there is a free list for native blocks of this size */
    if((free_list=H5FL_blk_find_list(&(head->head),temp->size))==NULL) {
        /* No free list available, create a new list node and insert it to the queue */
        free_list=H5FL_blk_create_list(&(head->head),temp->size);
    } /* end if */

    /* Prepend the free'd native block to the front of the free list */
    if(free_list!=NULL) {
        temp->next=free_list->list;
        free_list->list=temp;
    } /* end if */

    /* Increment the number of blocks on free list */
    head->onlist++;
#endif /* NO_BLK_FREE_LISTS */

    FUNC_LEAVE(NULL);
} /* end H5FL_blk_free() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_blk_realloc
 *
 * Purpose:	Resizes a block.  This does things the straightforward, simple way,
 *      not actually using realloc.
 *
 * Return:	Success:	NULL
 *
 *		Failure:	never fails
 *
 * Programmer:	Quincey Koziol
 *		Thursday, March  23, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5FL_blk_realloc(H5FL_blk_head_t *head, void *block, size_t new_size)
{
    H5FL_blk_list_t *temp;          /* Temp. ptr to the new block node allocated */
    void *ret_value=NULL;       /* Return value */

    FUNC_ENTER(H5FL_blk_realloc, NULL);

#ifdef NO_BLK_FREE_LISTS
    ret_value=H5MM_realloc(block,new_size);
#else /* NO_BLK_FREE_LISTS */
    /* Check if we are actually re-allocating a block */
    if(block!=NULL) {
        /* Get the pointer to the chunk info header in front of the chunk to free */
        temp=(H5FL_blk_list_t *)((unsigned char *)block-offsetof(H5FL_blk_list_t,block));

        /* check if we are actually changing the size of the buffer */
        if(new_size!=temp->size) {
            ret_value=H5FL_blk_alloc(head,new_size,0);
            HDmemcpy(ret_value,block,MIN(new_size,temp->size));
            H5FL_blk_free(head,block);
        } /* end if */
        else
            ret_value=block;
    } /* end if */
    /* Not re-allocating, just allocate a fresh block */
    else
        ret_value=H5FL_blk_alloc(head,new_size,0);
#endif /* NO_BLK_FREE_LISTS */

    FUNC_LEAVE(ret_value);
} /* end H5FL_blk_realloc() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_blk_gc_list
 *
 * Purpose:	Garbage collect a priority queue
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 23, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
static herr_t
H5FL_blk_gc_list(H5FL_blk_head_t *head)
{
    H5FL_blk_list_t *list; /* The free list of native nodes of a particular size */
    void *next;     /* Temp. ptr to the free list list node */
    void *temp;     /* Temp. ptr to the free list page node */
    
    /* FUNC_ENTER_INIT() should not be called, it causes an infinite loop at library termination */
    H5_trace(FALSE, "H5FL_blk_gc_list", "");

    /* Loop through all the nodes in the block free list queue */
    while(head->head!=NULL) {
        temp=head->head->next;

        /* Loop through all the blocks in the free list, freeing them */
        list=head->head->list;
        while(list!=NULL) {
            next=list->next;

            /* Decrement the number of blocks allocated from this PQ */
            head->allocated--;

            /* Free the block */
            H5MM_xfree(list);

            list=next;
        } /* end while */

        /* Free the free list node */
        H5MM_xfree(head->head);

        /* Advance to the next free list */
        head->head=temp;
    } /* end while */

    H5_trace(TRUE, NULL, "e", SUCCEED);
    return(SUCCEED);
}   /* end H5FL_blk_gc_list() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_blk_gc
 *
 * Purpose:	Garbage collect on all the priority queues
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, March 25, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
static herr_t
H5FL_blk_gc(void)
{
    H5FL_blk_gc_list_t *gc_list;    /* Pointer into the list of things to garbage collect */
    H5FL_blk_head_t *pq_head;   /* Pointer to head of PQ to garbage collect */
    
    /* FUNC_ENTER_INIT() should not be called, it causes an infinite loop at library termination */
    H5_trace(FALSE, "H5FL_blk_gc", "");

    /* Walk through all the free lists, free()'ing the nodes */
    gc_list=H5FL_blk_gc_head;
    while(gc_list!=NULL) {
        /* Get the pointer to the list head */
        pq_head=gc_list->pq;

        /* For each free list being garbage collected, walk through the nodes and free them */
        H5FL_blk_gc_list(gc_list->pq);

        /* Indicate no free nodes on the free list */
        pq_head->head=NULL;
        pq_head->onlist=0;

        /* Go on to the next free list to garbage collect */
        gc_list=gc_list->next;
    } /* end while */

    H5_trace(TRUE, NULL, "e", SUCCEED);
    return(SUCCEED);
}   /* end H5FL_blk_gc() */


/*--------------------------------------------------------------------------
 NAME
    H5FL_blk_term
 PURPOSE
    Terminate various H5FL_blk objects
 USAGE
    void H5FL_blk_term()
 RETURNS
    Success:	Positive if any action might have caused a change in some
                other interface; zero otherwise.
   	Failure:	Negative
 DESCRIPTION
    Release any resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static intn
H5FL_blk_term(void)
{
    H5FL_blk_gc_list_t *left;   /* pointer to garbage collection lists with work left */
    H5FL_blk_gc_list_t *tmp;    /* Temporary pointer to a garbage collection node */
    
    /* Free the nodes on the garbage collection list, keeping nodes with allocations outstanding */
    left=NULL;
    while(H5FL_blk_gc_head!=NULL) {
        tmp=H5FL_blk_gc_head->next;

#ifdef H5FL_DEBUG
printf("H5FL_blk_term: head->name=%s, head->allocated=%d\n", H5FL_blk_gc_head->pq->name,(int)H5FL_blk_gc_head->pq->allocated);
#endif /* H5FL_DEBUG */
        /* Check if the list has allocations outstanding */
        if(H5FL_blk_gc_head->pq->allocated>0) {
            /* Add free list to the list of nodes with allocations open still */
            H5FL_blk_gc_head->next=left;
            left=H5FL_blk_gc_head;
        } /* end if */
        /* No allocations left open for list, get rid of it */
        else {
            /* Reset the "initialized" flag, in case we restat this list somehow (I don't know how..) */
            H5FL_blk_gc_head->pq->init=0;

            /* Free the node from the garbage collection list */
            H5MM_xfree(H5FL_blk_gc_head);
        } /* end else */

        H5FL_blk_gc_head=tmp;
    } /* end while */

    /* Point to the list of nodes left with allocations open, if any */
    H5FL_blk_gc_head=left;
    
    return (H5FL_blk_gc_head!=NULL ? 1 : 0);
}


/*-------------------------------------------------------------------------
 * Function:	H5FL_arr_init
 *
 * Purpose:	Initialize a free list for a arrays of certain type.  Right now,
 *      this just adds the free list to the list of things to garbage collect.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, March 25, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
static herr_t
H5FL_arr_init(H5FL_arr_head_t *head)
{
    H5FL_gc_arr_list_t *new_list;   /* Pointer to the node for the new list to garbage collect */
    herr_t ret_value=SUCCEED;       /* return value*/

    FUNC_ENTER (H5FL_arr_init, FAIL);

    /* Allocate a new garbage collection node */
    if (NULL==(new_list = H5MM_malloc(sizeof(H5FL_gc_arr_list_t))))
        HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* Initialize the new garbage collection node */
    new_list->list=head;

    /* Link in to the garbage collection list */
    new_list->next=H5FL_gc_arr_head;
    H5FL_gc_arr_head=new_list;

    /* Allocate room for the free lists, if the arrays have a maximum size */
    if(head->maxelem>0) {
        if (NULL==(head->u.list_arr = H5MM_calloc(head->maxelem*sizeof(H5FL_arr_node_t *))))
            HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    } /* end if */
    else {
        head->u.queue.init=0;
        head->u.queue.allocated=0;
        head->u.queue.onlist=0;
        head->u.queue.name=head->name;
        head->u.queue.head=NULL;
    } /* end else */
    
    /* Indicate that the free list is initialized */
    head->init=1;

    FUNC_LEAVE (ret_value);
}   /* end H5FL_arr_init() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_arr_free
 *
 * Purpose:	Release an array of objects & put on free list
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, March 24, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
void *
H5FL_arr_free(H5FL_arr_head_t *head, void *obj)
{
    H5FL_arr_node_t *temp;  /* Temp. ptr to the new free list node allocated */

    FUNC_ENTER (H5FL_arr_free, NULL);

#ifdef NO_ARR_FREE_LISTS
    H5MM_xfree(obj);
#else /* NO_ARR_FREE_LISTS */
    /* Make certain that the free list is initialized */
    assert(head->init);

    /* Check if there are a maximum number of elements in list */
    if(head->maxelem>0) {
        /* Get the pointer to the info header in front of the block to free */
        temp=(H5FL_arr_node_t *)((unsigned char *)obj-offsetof(H5FL_arr_node_t,arr));

        /* Double-check that there is enough room for arrays of this size */
        assert((intn)temp->nelem<=head->maxelem);

        /* Link into the free list */
        temp->next=head->u.list_arr[temp->nelem];

        /* Point free list at the node freed */
        head->u.list_arr[temp->nelem]=temp;

        /* Increment the number of blocks on free lists */
        head->onlist++;
    } /* end if */
    /* No maximum number of elements, use PQ routine */
    else {
        H5FL_blk_free(&(head->u.queue),obj);
    } /* end else */
#endif /* NO_ARR_FREE_LISTS */

    FUNC_LEAVE(NULL);
}   /* end H5FL_arr_free() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_arr_alloc
 *
 * Purpose:	Allocate an array of objects
 *
 * Return:	Success:	Pointer to a valid array object
 * 		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Saturday, March 25, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
void *
H5FL_arr_alloc(H5FL_arr_head_t *head, uintn elem, uintn clear)
{
    H5FL_arr_node_t *new_obj;   /* Pointer to the new free list node allocated */
    void *ret_value;        /* Pointer to object to return */

    FUNC_ENTER (H5FL_arr_alloc, NULL);

#ifdef NO_ARR_FREE_LISTS
    if(clear)
        ret_value=H5MM_calloc(elem*head->size);
    else
        ret_value=H5MM_malloc(elem*head->size);
#else /* NO_ARR_FREE_LISTS */
    /* Make certain the list is initialized first */
    if(!head->init)
        H5FL_arr_init(head);

    /* Check if there is a maximum number of elements in array */
    if(head->maxelem>0) {
        /* Check for nodes available on the free list first */
        if(head->u.list_arr[elem]!=NULL) {
            /* Get a pointer to the block on the free list */
            ret_value=&(head->u.list_arr[elem]->arr);

            /* Remove node from free list */
            head->u.list_arr[elem]=head->u.list_arr[elem]->next;

            /* Decrement the number of blocks on free list */
            head->onlist--;
        } /* end if */
        /* Otherwise allocate a node */
        else {
            if (NULL==(new_obj = H5MM_malloc(sizeof(H5FL_arr_node_t)+((head->size)*elem)-1)))
                HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

            /* Increment the number of blocks allocated in list */
            head->allocated++;

            /* Initialize the new object */
            new_obj->nelem=elem;
            new_obj->next=NULL;

            /* Get a pointer to the new block */
            ret_value=&(new_obj->arr);
        } /* end else */

        /* Clear to zeros, if asked */
        if(clear)
            HDmemset(ret_value,0,head->size*elem);
    } /* end if */
    /* No fixed number of elements, use PQ routine */
    else {
        ret_value=H5FL_blk_alloc(&(head->u.queue),head->size*elem,clear);
    } /* end else */
#endif /* NO_ARR_FREE_LISTS */

    FUNC_LEAVE (ret_value);
}   /* end H5FL_arr_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_arr_realloc
 *
 * Purpose:	Reallocate an array of objects
 *
 * Return:	Success:	Pointer to a valid array object
 * 		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Saturday, March 25, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
void *
H5FL_arr_realloc(H5FL_arr_head_t *head, void * obj, uintn new_elem)
{
    H5FL_arr_node_t *temp;  /* Temp. ptr to the new free list node allocated */
    void *ret_value;        /* Pointer to object to return */

    FUNC_ENTER (H5FL_arr_realloc, NULL);

#ifdef NO_ARR_FREE_LISTS
    ret_value=H5MM_realloc(obj,new_elem*head->size);
#else /* NO_ARR_FREE_LISTS */
    /* Check if we are really allocating the object */
    if(obj==NULL) {
        ret_value=H5FL_arr_alloc(head,new_elem,0);
    } /* end if */
    else {
        /* Check if there is a maximum number of elements in array */
        if(head->maxelem>0) {
            /* Get the pointer to the info header in front of the block to free */
            temp=(H5FL_arr_node_t *)((unsigned char *)obj-offsetof(H5FL_arr_node_t,arr));

            /* Check if the size is really changing */
            if(temp->nelem!=new_elem) {
                /* Get the new array of objects */
                ret_value=H5FL_arr_alloc(head,new_elem,0);

                /* Copy the appropriate amount of elements */
                HDmemcpy(ret_value,obj,head->size*MIN(temp->nelem,new_elem));

                /* Free the old block */
                H5FL_arr_free(head,obj);
            } /* end if */
            else
                ret_value=obj;
        } /* end if */
        /* No fixed number of elements, use block routine */
        else {
            ret_value=H5FL_blk_realloc(&(head->u.queue),obj,head->size*new_elem);
        } /* end else */
    } /* end else */
#endif /* NO_ARR_FREE_LISTS */

    FUNC_LEAVE (ret_value);
}   /* end H5FL_arr_realloc() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_arr_gc
 *
 * Purpose:	Garbage collect on all the array object free lists
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, March 25, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
static herr_t
H5FL_arr_gc(void)
{
    H5FL_gc_arr_list_t *gc_arr_list;    /* Pointer into the list of things to garbage collect */
    H5FL_arr_head_t *arr_list_head; /* Pointer to head of free list to garbage collect */
    H5FL_arr_node_t *arr_free_list; /* Pointer to nodes in free list being garbage collected */
    void *tmp;      /* Temporary node pointer */
    intn i;         /* Counter for array of free lists */
    
    /* FUNC_ENTER_INIT() should not be called, it causes an infinite loop at library termination */
    H5_trace(FALSE, "H5FL_arr_gc", "");

    /* Walk through all the free lists, free()'ing the nodes */
    gc_arr_list=H5FL_gc_arr_head;
    while(gc_arr_list!=NULL) {
        /* Get the pointer to the list head */
        arr_list_head=gc_arr_list->list;

        /* Check if the array has a fixed maximum number of elements */
        if(arr_list_head->maxelem>0) {
            /* Walk through the array of free lists */
            for(i=0; i<arr_list_head->maxelem; i++) {

                /* For each free list being garbage collected, walk through the nodes and free them */
                arr_free_list=arr_list_head->u.list_arr[i];
                while(arr_free_list!=NULL) {
                    tmp=arr_free_list->next;

                    /* Decrement the count of nodes allocated and free the node */
                    arr_list_head->allocated--;
                    H5MM_xfree(arr_free_list);

                    arr_free_list=tmp;
                } /* end while */

                /* Indicate no free nodes on the free list */
                arr_list_head->u.list_arr[i]=NULL;
                arr_list_head->onlist=0;
            } /* end for */
        } /* end if */
        /* No maximum number of elements, just use the PQ call to garbage collect */
        else {
            H5FL_blk_gc_list(&(arr_list_head->u.queue));
        } /* end else */

        /* Go on to the next free list to garbage collect */
        gc_arr_list=gc_arr_list->next;
    } /* end while */

    H5_trace(TRUE, NULL, "e", SUCCEED);
    return(SUCCEED);
}   /* end H5FL_arr_gc() */


/*--------------------------------------------------------------------------
 NAME
    H5FL_arr_term
 PURPOSE
    Terminate various H5FL array object free lists
 USAGE
    intn H5FL_arr_term()
 RETURNS
    Success:	Positive if any action might have caused a change in some
                other interface; zero otherwise.
   	Failure:	Negative
 DESCRIPTION
    Release any resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static intn
H5FL_arr_term(void)
{
    H5FL_gc_arr_list_t *left;   /* pointer to garbage collection lists with work left */
    H5FL_gc_arr_list_t *tmp;    /* Temporary pointer to a garbage collection node */
    
    /* Free the nodes on the garbage collection list, keeping nodes with allocations outstanding */
    left=NULL;
    while(H5FL_gc_arr_head!=NULL) {
        tmp=H5FL_gc_arr_head->next;

        /* Check if the array has a fixed maximum number of elements */
        if(H5FL_gc_arr_head->list->maxelem>0) {
            /* Check if the list has allocations outstanding */
#ifdef H5FL_DEBUG
printf("H5FL_arr_term: head->name=%s, head->allocated=%d\n", H5FL_gc_arr_head->list->name,(int)H5FL_gc_arr_head->list->allocated);
#endif /* H5FL_DEBUG */
            if(H5FL_gc_arr_head->list->allocated>0) {
                /* Add free list to the list of nodes with allocations open still */
                H5FL_gc_arr_head->next=left;
                left=H5FL_gc_arr_head;
            } /* end if */
            /* No allocations left open for list, get rid of it */
            else {
                /* Free the array of free lists */
                H5MM_xfree(H5FL_gc_arr_head->list->u.list_arr);

                /* Reset the "initialized" flag, in case we restart this list somehow (I don't know how..) */
                H5FL_gc_arr_head->list->init=0;

                /* Free the node from the garbage collection list */
                H5MM_xfree(H5FL_gc_arr_head);
            } /* end else */
        } /* end if */
        /* No maximum number of elements, use the PQ information */
        else {
#ifdef H5FL_DEBUG
printf("H5FL_arr_term: head->name=%s, head->allocated=%d\n", H5FL_gc_arr_head->list->name,(int)H5FL_gc_arr_head->list->u.queue.allocated);
#endif /* H5FL_DEBUG */
            /* Check if the list has allocations outstanding */
            if(H5FL_gc_arr_head->list->u.queue.allocated>0) {
                /* Add free list to the list of nodes with allocations open still */
                H5FL_gc_arr_head->next=left;
                left=H5FL_gc_arr_head;
            } /* end if */
            /* No allocations left open for list, get rid of it */
            else {
                /* Reset the "initialized" flag, in case we restart this list somehow (I don't know how..) */
                H5FL_gc_arr_head->list->init=0;

                /* Free the node from the garbage collection list */
                H5MM_xfree(H5FL_gc_arr_head);
            } /* end else */
        } /* end else */

        H5FL_gc_arr_head=tmp;
    } /* end while */

    /* Point to the list of nodes left with allocations open, if any */
    H5FL_gc_arr_head=left;
    
    return (H5FL_gc_arr_head!=NULL ? 1 : 0);
}   /* end H5FL_arr_term() */


/*-------------------------------------------------------------------------
 * Function:	H5FL_garbage_coll
 *
 * Purpose:	Garbage collect on all the free lists
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, March 24, 2000
 *
 * Modifications:
 * 	
 *-------------------------------------------------------------------------
 */
herr_t
H5FL_garbage_coll(void)
{
    /* FUNC_ENTER_INIT() should not be called, it causes an infinite loop at library termination */
    H5_trace(FALSE, "H5FL_garbage_coll", "");

    /* Garbage collect the free lists for regular objects */
    H5FL_gc();

    /* Garbage collect the free lists for array objects */
    H5FL_arr_gc();

    /* Garbage collect free lists for blocks */
    H5FL_blk_gc();

    H5_trace(TRUE, NULL, "e", SUCCEED);
    return(SUCCEED);
}   /* end H5FL_garbage_coll() */


/*--------------------------------------------------------------------------
 NAME
    H5FL_term_interface
 PURPOSE
    Terminate various H5FL objects
 USAGE
    void H5FL_term_interface()
 RETURNS
    Success:	Positive if any action might have caused a change in some
                other interface; zero otherwise.
   	Failure:	Negative
 DESCRIPTION
    Release any resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
intn
H5FL_term_interface(void)
{
    intn ret_value=0;
    
    /* Garbage collect any nodes on the free lists */
    H5FL_garbage_coll();

    ret_value=H5FL_term()+H5FL_arr_term()+H5FL_blk_term();

    return(ret_value);
}

