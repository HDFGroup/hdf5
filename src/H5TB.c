/****************************************************************************
* NCSA HDF								   *
* Software Development Group						   *
* National Center for Supercomputing Applications			   *
* University of Illinois at Urbana-Champaign				   *
* 605 E. Springfield, Champaign IL 61820				   *
*									   *
* For conditions of distribution and use, see the accompanying		   *
* hdf/COPYING file.							   *
*									   *
****************************************************************************/

/*
 * Created:		H5TB.c
 *			Jun 11 1998
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Temporary buffer management functions
 *
 * Library Public API Functions:
 *  H5TBget_buf     - Get an ID for a temporary buffer
 *  H5TBbuf_ptr     - Get a pointer to the temporary buffer's memory
 *  H5TBresize_buf  - Resize a temporary buffer
 *  H5TBgarbage_coll- Free all unused temporary buffers
 *  H5TBrelease_buf - Release temporary buffer
 *
 * Modifications:	
 *
 */

#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#include <H5private.h>		/* Generic Functions    */
#include <H5Iprivate.h>		/* ID Functions         */
#include <H5Eprivate.h>		/* Error handling       */
#include <H5MMprivate.h>	/* Memory Management functions  */
#include <H5TBprivate.h>	/* Temporary buffer info    */

/* Interface init/term information */
#define PABLO_MASK	H5TB_mask
#define INTERFACE_INIT	H5TB_init_interface
static intn		interface_initialize_g = FALSE;
static herr_t		H5TB_init_interface(void);
static void		H5TB_term_interface(void);

/* Local information for managing buffers */
#define H5TB_RESERVED_ATOMS         0

typedef struct tag_H5TB_t {
    hbool_t inuse;      /* Flag to indicate whether the buffer is in use or not */
    hsize_t size;       /* Current size of the buffer */
    struct tag_H5TB_t *next;    /* Pointer to next buffer in list */
    struct tag_H5TB_t *prev;    /* Pointer to previous buffer in list */
    void *buf;          /* Pointer to actual temporary buffer */
} H5TB_t;

static H5TB_t * H5TB_list_head=NULL;    /* pointer to beginning of temp. buffer list (list is in order of increasing size) */
static H5TB_t * H5TB_list_tail=NULL;    /* pointer to end of temp. buffer list */

/* Local functions */
herr_t H5TB_close(H5TB_t *tb);


/*--------------------------------------------------------------------------
NAME
   H5TB_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5TB_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5TB_init_interface(void)
{
    herr_t		    ret_value = SUCCEED;
    FUNC_ENTER(H5TB_init_interface, FAIL);

    /* Initialize the atom group for the file IDs */
    if ((ret_value = H5I_init_group(H5_TEMPBUF, H5I_TEMPBUFID_HASHSIZE,
            H5TB_RESERVED_ATOMS, NULL)) != FAIL) {
        ret_value = H5_add_exit(&H5TB_term_interface);
    }
    FUNC_LEAVE(ret_value);
}


/*--------------------------------------------------------------------------
 NAME
    H5TB_term_interface
 PURPOSE
    Terminate various H5TB objects
 USAGE
    void H5TB_term_interface()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static void
H5TB_term_interface(void)
{
    /* Destroy the atom group */
    /* Step through the list and free the buffers */
}

/*-------------------------------------------------------------------------
 * Function:	H5TB_close
 *
 * Purpose:	Releases all memory associated with a temporary buffer.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Thursday, June 11, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5TB_close(H5TB_t *tb)
{
    FUNC_ENTER(H5TB_close, FAIL);

    assert(tb);


    /* Release the main structure */
    H5MM_xfree(tb);

    FUNC_LEAVE(SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5TBget_buf
 PURPOSE
    Get an ID for a temporary buffer
 USAGE
    hid_t H5TBget_buf(size,resize)
        hsize_t size;       IN: Minimum size of buffer requested
        hbool_t resize;     IN: Whether to resize an existing buffer or get a
                                new buffer if one doesn't match the correct size
 RETURNS
    Valid buffer ID on success, negative on failure
 DESCRIPTION
    Checks for an available temporary buffer of at least the size requested and
    returns an ID for an appropriate one.  If a buffer of the minimum size
    requested is not available and the resize flag is set, the smallest buffer
    available is resized to be the correct size and returned, otherwise a new
    buffer of the correct size is allocated and returned.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t H5TBget_buf(hsize_t size, hbool_t resize)
{
    hid_t	ret_value = FAIL;
    H5TB_t *curr=H5TB_list_head,       /* pointer to current temp. buffer */
        *new;                          /* pointer to a newly created temp. buffer */

    FUNC_ENTER (H5TBget_buf, FAIL);

    while(curr!=NULL) {
        if(!curr->inuse && size<curr->size)
            break;
        curr=curr->next;
    } /* end while */

    /* Check if we found a block or not */
    if(curr!=NULL) {
        curr->inuse=TRUE;
    } else {
        if(resize) {
            curr=H5TB_list_head;       /* start at beginning again */

            /* Search for first node which isn't in use */
            while(curr!=NULL) {
                if(!curr->inuse)
                    break;
                curr=curr->next;
            } /* end while */

            /* Mark the buffer in use and resize the buffer */
            if(curr!=NULL) {
                void * old_ptr=curr->buf;

                if((curr->buf = H5MM_realloc(curr->buf, size))==NULL) {
                    curr->buf=old_ptr;  /* restore pointer if no memory available */
                    HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                          "unable to allocate space for temporary buffer");
                }
                curr->inuse=TRUE;
            }
        } /* end if */
    } /* end else */

    /* No blocks in the list are acceptable */
    /* (either too small or not able to be resized) */
    if(curr==NULL) {
        if((new=H5MM_calloc(sizeof(H5TB_t)))==NULL)
            HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                  "unable to allocate space for temporary buffer struct");
        new->inuse=TRUE;
        new->size=size;
        if((new->buf=H5MM_malloc(size))==NULL) {
            H5MM_xfree(new);    /* free structure */
            HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                  "unable to allocate space for temporary buffer");
        }

        /* Check if this is the first node in the list */
        if(H5TB_list_head==NULL) {
            H5TB_list_head=H5TB_list_tail=curr=new;
        } else {
            /* Find correct place to insert in list */
            for(curr=H5TB_list_head; curr!=NULL; curr=curr->next) {
                /* Found node to insert before */
                if(curr->size > new->size) {
                    H5TB_t *tmp=curr->prev;    /* temporary pointer */

                    /* Inserting at head of list */
                    if(tmp==NULL) {
                        H5TB_list_head=new;
                        new->next=curr;
                        curr->prev=new;
                    } else {
                        tmp->next=new;
                        new->prev=tmp;
                        curr->prev=new;
                        new->next=curr;
                    } /* end else */

                    /* set this so we can fall through to getting the ID */
                    curr=new;
                } /* end if */
            } /* end for */

            /* Add to end of list */
            if(curr==NULL) {
                curr=H5TB_list_tail;
                H5TB_list_tail=curr->next=new;
                new->prev=curr;
            } /* end if */

            /* set this so we can fall through to getting the ID */
            curr=new;
        } /* end else */
    } /* end if */

    /* Atomize */
    if ((ret_value=H5I_register (H5_TEMPBUF, curr))<0) {
        HGOTO_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register temp. buffer atom");
    }

done:
    if (ret_value < 0) {
    }
    FUNC_LEAVE(ret_value);
} /* H5TBget_buf() */

/*--------------------------------------------------------------------------
 NAME
    H5TBbuf_ptr
 PURPOSE
    Get the pointer to a temp. buffer memory
 USAGE
    void *H5TBbuf_ptr(tbid)
        hid_t tbid;       IN: Temp. buffer ID
 RETURNS
    Non-NULL pointer to buffer memory on success, NULL on failure
 DESCRIPTION
    Gets the pointer to a temp. buffer's memory.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
void *H5TBbuf_ptr(hid_t tbid)
{
    void *ret_value = NULL;
    H5TB_t *tbuf;               /* Pointer to temporary buffer */

    FUNC_ENTER (H5TBbuf_ptr, NULL);

    if (H5_TEMPBUF != H5I_group(tbid) ||
            NULL == (tbuf = H5I_object(tbid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a temp. buffer");
    }

    ret_value=tbuf->buf;

#ifdef LATER
done:
#endif
    if (ret_value == NULL) {
    }
    FUNC_LEAVE(ret_value);
} /* H5TBbuf_ptr() */

/*--------------------------------------------------------------------------
 NAME
    H5TBresize_ptr
 PURPOSE
    Resize a temp. buffer to a new size
 USAGE
    herr_t H5TBresize_ptr(tbid, size)
        hid_t tbid;       IN: Temp. buffer ID to resize
        hsize_t size;     IN: New size of temp. buffer
 RETURNS
    non-negative on success, negative on failure
 DESCRIPTION
    Resizes a temporary buffer to a new size.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5TBresize_ptr(hid_t tbid, hsize_t size)
{
    herr_t ret_value = FAIL;
    H5TB_t *tbuf,               /* Pointer to temporary buffer */
        *curr;                  /* Pointer to temp. buffer node */
    void * old_ptr;             /* Pointer to the previous buffer */

    FUNC_ENTER (H5TBresize_ptr, FAIL);

    if (H5_TEMPBUF != H5I_group(tbid) ||
            NULL == (tbuf = H5I_object(tbid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a temp. buffer");
    }

    /* Save old pointer for later */
    old_ptr=tbuf->buf;

    /* Try to resize buffer to new size */
    if((tbuf->buf = H5MM_realloc(tbuf->buf, size))==NULL) {
        tbuf->buf=old_ptr;  /* restore pointer if no memory available */
        HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
              "unable to allocate space for temporary buffer");
    }

    /* Change the size of the buffer */
    tbuf->size=size;

    /*
     * Check if we need to move the buffer in the sorted list
     */

    /* Check if this is not the last node and it need's to move */
    if(tbuf->next!=NULL && tbuf->next->size < tbuf->size) {
        /* Remove this node from the list */
        if(tbuf->prev==NULL) {  /* remove from head of list */
            H5TB_list_head=tbuf->next;
            tbuf->next->prev=NULL;
        } else {    /* remove from middle of list */
            tbuf->prev->next=tbuf->next;
            tbuf->next->prev=tbuf->prev;
        } /* end if */

        /* Find correct position in list */
        curr=H5TB_list_head;
        while(curr!=NULL) {
            if(!curr->inuse && size<curr->size)
                break;
            curr=curr->next;
        } /* end while */

        /* Insert into correct position in list */
        if(curr!=NULL) { /* can't be adding to the beginning of list, so this is in the middle somewhere */
            curr->prev->next=tbuf;
            tbuf->prev=curr->prev;
            curr->prev=tbuf;
            tbuf->next=curr;
        } else {        /* append to end of list */
            H5TB_list_tail->next=tbuf;
            tbuf->prev=H5TB_list_tail;
            tbuf->next=NULL;
            H5TB_list_tail=tbuf;
        } /* end else */
    } /* end if */

#ifdef LATER
done:
#endif
    if (ret_value == FAIL) {
    }
    FUNC_LEAVE(ret_value);
} /* H5TBresize_ptr() */

/*--------------------------------------------------------------------------
 NAME
    H5TBgarbage_coll
 PURPOSE
    Release all unused temporary buffers
 USAGE
    herr_t H5TBgarbase_coll()
 RETURNS
    non-negative on success, negative on failure
 DESCRIPTION
    Steps through the list of temporary buffers, removing unused nodes from the
    list and freeing their memory
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5TBgarbage_coll(void)
{
    herr_t ret_value = FAIL;
    H5TB_t *curr,*next;      /* Current temp. buffer node */

    FUNC_ENTER (H5TBgarbage_coll, FAIL);

    /* Step through the list, remove each unused node, repair the list and free the node */
    curr=H5TB_list_head;
    while(curr!=NULL) {
        next=curr->next;
        if(!curr->inuse) {
            /* maintain list head & tail */
            if(H5TB_list_head==curr)
                H5TB_list_head=curr->next;
            if(H5TB_list_tail==curr)
                H5TB_list_tail=curr->prev;

            /* Delete node from list */
            if(curr->prev!=NULL)
                curr->prev->next=curr->next;
            if(curr->next!=NULL)
                curr->next->prev=curr->prev;
            
            /* Free memory for node */
            if(curr->buf!=NULL)
                H5MM_xfree(curr->buf);
            H5MM_xfree(curr);
        } /* end if */
        curr=next;
    } /* end while */

    ret_value=SUCCEED;

#ifdef LATER
done:
#endif
    if (ret_value == FAIL) {
    }
    FUNC_LEAVE(ret_value);
}   /* H5TBgarbage_coll() */

/*--------------------------------------------------------------------------
 NAME
    H5TBrelease_buf
 PURPOSE
    Release a temp. buffer back to the list of unused ones.
 USAGE
    herr_t H5TBrelease_buf(tbid)
        hid_t tbid;       IN: Temp. buffer ID to release
 RETURNS
    non-negative on success, negative on failure
 DESCRIPTION
    Releases a temporary buffer.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5TBrelease_buf(hid_t tbid)
{
    herr_t ret_value = FAIL;
    H5TB_t *tbuf;               /* Pointer to temporary buffer */

    FUNC_ENTER (H5TBresize_ptr, FAIL);

    if (H5_TEMPBUF != H5I_group(tbid) ||
            NULL == (tbuf = H5I_object(tbid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a temp. buffer");
    }

    /* Release the buffer */
    tbuf->inuse=FALSE;

    ret_value=SUCCEED;

#ifdef LATER
done:
#endif
    if (ret_value == FAIL) {
    }
    FUNC_LEAVE(ret_value);
} /* H5TBresize_ptr() */

