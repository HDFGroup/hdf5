/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*LINTLIBRARY */
/*+
   FILE
       hdf5err.c
   HDF error reporting routines

   EXPORTED ROUTINES
       H5Enew_err_stack -- Create a new error stack to push values on
       H5Epush          -- Push an error value on an error stack

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
       H5E_init_interface    -- initialize the H5E interface
   + */

#define HDF5_ERR_MASTER
#include "hdf5.h"
#include "H5Eprivate.h"     /* Private error routines */
#undef HDF5_ERR_MASTER

#include "H5private.h"      /* Generic Functions */

/*--------------------- Locally scoped variables -----------------------------*/

/* Whether we've installed the library termination function yet for this interface */
static intn interface_initialize = FALSE;

/*------------------_-- Local function prototypes ----------------------------*/
static herr_t H5E_init_interface(void);

/*--------------------------------------------------------------------------
NAME
   H5E_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5E_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t H5E_init_interface(void)
{
#ifdef LATER
    CONSTR(FUNC, "H5E_init_interface");	/* For HERROR */
#endif /* LATER */
    herr_t ret_value = SUCCEED;

    /* Don't use "FUNC_ENTER" macro, to avoid potential infinite recursion */
    PABLO_TRACE_ON(H5_mask, ID_H5Iinit_interface);

    /* Don't call this routine again... */
    interface_initialize = TRUE;

    /* Initialize the atom group for the error stacks */
    ret_value=H5Ainit_group(H5_ERR,HDF5_ERRSTACK_HASHSIZE,0);

    FUNC_LEAVE(H5_mask, ID_H5E_init_interface, ret_value);
}	/* H5E_init_interface */

/*--------------------------------------------------------------------------
NAME
   H5Enew_err_stack -- Create a new error stack
USAGE
    int32 H5Enew_err_stack(initial_stack_size);
    uintn initial_stack_size;       IN: Starting size of the error stack
   
RETURNS
    The ID of the error stack created on success, FAIL on failure.

DESCRIPTION
    Dynamically creates a new error stack to push error values onto.

--------------------------------------------------------------------------*/
int32 H5Enew_err_stack(uintn initial_stack_size)
{
    CONSTR(FUNC, "H5Enew_err_stack");	/* For HERROR */
    H5E_errstack_t *new_stack=NULL;     /* Pointer to the new error stack */
    int32 ret_value = FAIL;

    FUNC_ENTER(H5E_mask, ID_H5Enew_err_stack, H5E_init_interface,FAIL);

    /* Allocate the stack header */
    if((new_stack=HDmalloc(sizeof(H5E_errstack_t)))==NULL)
        HGOTO_DONE(FAIL);

    /* Initialize the stack header */
    new_stack->stack_size=initial_stack_size;
    new_stack->stack_top=0;
    if((new_stack->err_stack=HDcalloc(initial_stack_size,sizeof(H5E_error_t)))==NULL)
      {
        HDfree(new_stack);
        HGOTO_DONE(FAIL);
      } /* end if */
    new_stack->push=H5E_store;  /* Set the default error handler */

    /* Get an atom for the error stack */
    ret_value=H5Aregister_atom(H5_ERR, new_stack);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(H5E_mask, ID_H5Enew_err_stack, ret_value);
}	/* H5Enew_err_stack */

/*--------------------------------------------------------------------------
NAME
   H5Edelete_err_stack -- Destroy an error stack
USAGE
    intn H5Edelete_err_stack(err_stack);
    int32 err_stack;       IN: Error stack to delete
   
RETURNS
    SUCCEED/FAIL

DESCRIPTION
    Destroys an error stack, releasing memory allocated, etc.

--------------------------------------------------------------------------*/
intn H5Edelete_err_stack(int32 err_stack)
{
    CONSTR(FUNC, "H5Edelete_err_stack");	/* For HERROR */
    H5E_errstack_t *old_stack=NULL;         /* Pointer to the new error stack */
    intn ret_value = SUCCEED;

    FUNC_ENTER(H5E_mask, ID_H5Edelete_err_stack, H5E_init_interface,FAIL);

    /* Clear errors and check args and all the boring stuff. */
    if (H5Aatom_group(err_stack)!=H5_ERR)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL);

    /* Get the error stack to put the error on */
    if((old_stack=H5Aremove_atom(err_stack))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    /* Clear the error descriptions and reset the stack top */
    for(; old_stack->stack_top>0; old_stack->stack_top--)
      {
        if (old_stack->err_stack[old_stack->stack_top-1].desc)
          {
              HDfree(old_stack->err_stack[old_stack->stack_top-1].desc);
              old_stack->err_stack[old_stack->stack_top-1].desc=NULL;
          } /* end if */
      } /* end if */

    HDfree(old_stack->err_stack);
    HDfree(old_stack);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(H5E_mask, ID_H5Edelete_err_stack, ret_value);
}	/* H5Edelete_err_stack */

/*--------------------------------------------------------------------------
NAME
   H5Eclear -- Clear an error stack for later error entries
USAGE
    void H5Eclear(int32 err_hand)
    int32 err_hand;         IN: The ID of the error stack to push the error onto.
   
RETURNS
   none
DESCRIPTION
    Clear an error stack to allow errors to be pushed onto it.

--------------------------------------------------------------------------*/
void H5Eclear(int32 err_hand)
{
    CONSTR(FUNC, "H5Eclear");	/* For HERROR */
    H5E_errstack_t *err_stack=NULL; /* Pointer to the error stack to put value on */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER(H5E_mask, ID_H5Eclear, H5E_init_interface,FAIL);

    /* Get the error stack for this error handler, initialized earlier in H5Enew_err_stack */
    if (H5Aatom_group(err_hand)!=H5_ERR)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL);

    /* Get the error stack to put the error on */
    if((err_stack=H5Aatom_object(err_hand))==NULL)
        HGOTO_ERROR(H5E_BADATOM, H5E_BADATOM, FAIL);

    /* Clear the error descriptions and reset the stack top */
    for(; err_stack->stack_top>0; err_stack->stack_top--)
      {
        if (err_stack->err_stack[err_stack->stack_top-1].desc)
          {
              HDfree(err_stack->err_stack[err_stack->stack_top-1].desc);
              err_stack->err_stack[err_stack->stack_top-1].desc=NULL;
          } /* end if */
      } /* end if */

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    PABLO_TRACE_OFF(H5E_mask, ID_H5Eclear);  /* ignore ret_value set */
}	/* H5Eclear */

/*--------------------------------------------------------------------------
NAME
   H5E_store -- Push an error value on an error stack
USAGE
    void H5E_store(hdf_err_code_t err, const char *function_name, const char *file_name, intn line)
    hdf_err_code_t err;     IN: The error code which occurred.
    const char *function_name;     IN: Name of the function the error occurred within.
    const char *file_name;  IN: Name of the file the error occurred within.
    intn line;              IN: Line # in the file the error occurred on.
   
RETURNS
   none
DESCRIPTION
    Pushes an error onto an error stack for this thread.  (This is the default
    action when errors occur, but can be overridden by user's code)

--------------------------------------------------------------------------*/
void H5E_store(int32 errid, hdf_maj_err_code_t maj, hdf_min_err_code_t min, const char *function_name, const char *file_name, intn line)
{
    CONSTR(FUNC, "H5E_store");	/* For HERROR */
    H5E_errstack_t *err_stack=NULL;     /* Pointer to the error stack to put value on */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER(H5E_mask, ID_H5E_store, H5E_init_interface,FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5Eclear(errid);

    /* Get the error stack to put the error on */
    if((err_stack=H5Aatom_object(errid))==NULL)
        HGOTO_ERROR(H5E_BADATOM, H5E_BADATOM, FAIL);

    /* Check if we need to expand the stack */
    if(err_stack->stack_top>=err_stack->stack_size)
      {
        H5E_error_t *old_stack=err_stack->err_stack;    /* in case realloc fails */

        /* Ask for new stack that's twice as large */
        if((err_stack->err_stack=HDrealloc(old_stack,2*err_stack->stack_size))==NULL)
          {
            err_stack->err_stack=old_stack;
            HGOTO_DONE(FAIL);
          } /* end if */
        err_stack->stack_size *= 2; /* increase the size of the stack */
      } /* end if */

    /* Push the error onto the error stack */
    err_stack->err_stack[err_stack->stack_top].maj=maj;
    err_stack->err_stack[err_stack->stack_top].min=min;
    HDstrncpy(err_stack->err_stack[err_stack->stack_top].function_name,function_name,MAX_FUNC_NAME_LEN);
    err_stack->err_stack[err_stack->stack_top].file_name=file_name;
    err_stack->err_stack[err_stack->stack_top].line=line;

    /* Increment the top of the error stack */
    err_stack->stack_top++;

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    PABLO_TRACE_OFF(H5E_mask, ID_H5E_store);  /* ignore ret_value set */
}	/* H5E_store */

/*--------------------------------------------------------------------------
NAME
   H5Epush -- Push an error value on an error stack
USAGE
    void H5Epush(hdf_err_code_t err, const char *function_name, const char *file_name, intn line)
    hdf_err_code_t err;     IN: The error code which occurred.
    const char *function_name;     IN: Name of the function the error occurred within.
    const char *file_name;  IN: Name of the file the error occurred within.
    intn line;              IN: Line # in the file the error occurred on.
   
RETURNS
   none
DESCRIPTION
    Pushes an error onto an error stack for this thread.

--------------------------------------------------------------------------*/
void H5Epush(hdf_maj_err_code_t maj, hdf_min_err_code_t min, const char *function_name, const char *file_name, intn line)
{
    CONSTR(FUNC, "H5Epush");	/* For HERROR */
    H5E_errstack_t *err_stack=NULL;     /* Pointer to the error stack to put value on */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER(H5E_mask, ID_H5Epush, H5E_init_interface,FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if (function_name==NULL || file_name==NULL || H5Aatom_group(thrderrid)!=H5_ERR)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL);

    /* Get the error stack to put the error on */
    if((err_stack=H5Aatom_object(thrderrid))==NULL)
        HGOTO_ERROR(H5E_BADATOM, H5E_BADATOM, FAIL);

    err_stack->push(thrderrid, maj, min, function_name, file_name, line);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    PABLO_TRACE_OFF(H5E_mask, ID_H5Epush);  /* ignore ret_value set */
}	/* H5Epush */

#ifdef H5_ERROR_DEBUG
/*--------------------------------------------------------------------------
NAME
   H5Eset_push -- Set the function to call when an error value is reported
USAGE
    H5E_push_func_t H5Eset_push(H5E_push_func_t func)
    H5E_push_func_t func;   IN: New function to call when an error occurs.
   
RETURNS
    The function pointer to the previous error function on success, NULL on
    failure.
DESCRIPTION
    Changes the function which is called for errors on this thread.  The thread
    ID is implicit, ie. this function must be called within each thread whose
    error function is to be changed.

--------------------------------------------------------------------------*/
H5E_push_func_t H5Eset_push(H5E_push_func_t func)
{
    CONSTR(FUNC, "H5Eset_push");	/* For HERROR */
    H5E_errstack_t *err_stack=NULL;     /* Pointer to the error stack to put value on */
    H5E_push_func_t ret_value = NULL;

    FUNC_ENTER(H5E_mask, ID_H5Eset_push, H5E_init_interface,NULL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if (func==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL);

    /* Get the error stack to put the error on */
    if((err_stack=H5Aatom_object(thrderrid))==NULL)
        HGOTO_ERROR(H5E_BADATOM, H5E_BADATOM, NULL);

    ret_value=err_stack->push;
    err_stack->push=func;

done:
  if(ret_value == NULL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(H5E_mask, ID_H5Eset_push, ret_value);
}	/* H5Eset_push */
#endif /* H5_ERROR_DEBUG */

