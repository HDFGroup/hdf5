/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Open object info algorithms.
 *
 * These are used to track the objects currently open in a file, for various
 * internal mechanisms which need to be aware of such things.
 *
 */

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */


#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access                          */
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5FOprivate.h"        /* File objects                         */
#include "H5Oprivate.h"		/* Object headers		  	*/

/* Private typedefs */

/* Information about object objects in a file */
typedef struct H5FO_open_obj_t {
    haddr_t addr;                       /* Address of object header for object */
    void *obj;                          /* Pointer to the object            */
    hbool_t deleted;                    /* Flag to indicate that the object was deleted from the file */
} H5FO_open_obj_t;

/* Declare a free list to manage the H5FO_open_obj_t struct */
H5FL_DEFINE_STATIC(H5FO_open_obj_t);


/*--------------------------------------------------------------------------
 NAME
    H5FO_create
 PURPOSE
    Create an open object info set
 USAGE
    herr_t H5FO_create(f)
        H5F_t *f;       IN/OUT: File to create opened object info set for

 RETURNS
    Returns non-negative on success, negative on failure
 DESCRIPTION
    Create a new open object info set.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5FO_create(const H5F_t *f)
{
    herr_t ret_value=SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI(H5FO_create,FAIL)

    /* Sanity check */
    assert(f);
    assert(f->shared);

    /* Create container used to store open object info */
    if((f->shared->open_objs=H5SL_create(H5SL_TYPE_HADDR,0.5,16))==NULL)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to create open object container")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FO_create() */


/*--------------------------------------------------------------------------
 NAME
    H5FO_opened
 PURPOSE
    Checks if an object at an address is already open in the file.
 USAGE
    void * H5FO_opened(f,addr)
        const H5F_t *f;         IN: File to check opened object info set
        haddr_t addr;           IN: Address of object to check

 RETURNS
    Returns a pointer to the object on success, NULL on failure
 DESCRIPTION
    Check is an object at an address (the address of the object's object header)
    is already open in the file and return the ID for that object if it is open.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
void *
H5FO_opened(const H5F_t *f, haddr_t addr)
{
    H5FO_open_obj_t *open_obj;  /* Information about open object */
    void *ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOFUNC(H5FO_opened)

    /* Sanity check */
    assert(f);
    assert(f->shared);
    assert(f->shared->open_objs);
    assert(H5F_addr_defined(addr));

    /* Get the object node from the container */
    if((open_obj=H5SL_search(f->shared->open_objs,&addr))!=NULL) {
        ret_value=open_obj->obj;
        assert(ret_value!=NULL);
    } /* end if */
    else
        ret_value=NULL;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FO_opened() */


/*--------------------------------------------------------------------------
 NAME
    H5FO_insert
 PURPOSE
    Insert a newly opened object/pointer pair into the opened object info set
 USAGE
    herr_t H5FO_insert(f,addr,obj)
        H5F_t *f;               IN/OUT: File's opened object info set
        haddr_t addr;           IN: Address of object to insert
        void *obj;              IN: Pointer to object to insert
        int type;               IN: Type of object being inserted

 RETURNS
    Returns a non-negative on success, negative on failure
 DESCRIPTION
    Insert an object/ID pair into the opened object info set.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5FO_insert(const H5F_t *f, haddr_t addr, void *obj)
{
    H5FO_open_obj_t *open_obj;  /* Information about open object */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5FO_insert,FAIL)

    /* Sanity check */
    assert(f);
    assert(f->shared);
    assert(f->shared->open_objs);
    assert(H5F_addr_defined(addr));
    assert(obj);

    /* Allocate new opened object information structure */
    if((open_obj=H5FL_MALLOC(H5FO_open_obj_t))==NULL)
        HGOTO_ERROR(H5E_CACHE,H5E_NOSPACE,FAIL,"memory allocation failed")

    /* Assign information */
    open_obj->addr=addr;
    open_obj->obj=obj;
    open_obj->deleted=0;

    /* Insert into container */
    if(H5SL_insert(f->shared->open_objs,&open_obj->addr,open_obj)<0)
        HGOTO_ERROR(H5E_CACHE,H5E_CANTINSERT,FAIL,"can't insert object into container")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FO_insert() */


/*--------------------------------------------------------------------------
 NAME
    H5FO_delete
 PURPOSE
    Remove an opened object/ID pair from the opened object info set
 USAGE
    herr_t H5FO_delete(f,addr)
        H5F_t *f;               IN/OUT: File's opened object info set
        haddr_t addr;           IN: Address of object to remove

 RETURNS
    Returns a non-negative on success, negative on failure
 DESCRIPTION
    Remove an object/ID pair from the opened object info.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5FO_delete(H5F_t *f, hid_t dxpl_id, haddr_t addr)
{
    H5FO_open_obj_t *open_obj;  /* Information about open object */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5FO_delete,FAIL)

    /* Sanity check */
    assert(f);
    assert(f->shared);
    assert(f->shared->open_objs);
    assert(H5F_addr_defined(addr));

    /* Remove from container */
    if((open_obj=H5SL_remove(f->shared->open_objs,&addr))==NULL)
        HGOTO_ERROR(H5E_CACHE,H5E_CANTRELEASE,FAIL,"can't remove object from container")

    /* Check if the object was deleted from the file */
    if(open_obj->deleted) {
        if(H5O_delete(f, dxpl_id, addr)<0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "can't delete object from file")
    } /* end if */

    /* Release the object information */
    H5FL_FREE(H5FO_open_obj_t,open_obj);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FO_delete() */


/*--------------------------------------------------------------------------
 NAME
    H5FO_mark
 PURPOSE
    Mark an object to be deleted when it is closed
 USAGE
    herr_t H5FO_mark(f,addr)
        const H5F_t *f;         IN: File opened object is in
        haddr_t addr;           IN: Address of object to delete

 RETURNS
    Returns a non-negative ID for the object on success, negative on failure
 DESCRIPTION
    Mark an opened object for deletion from the file when it is closed.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5FO_mark(const H5F_t *f, haddr_t addr, hbool_t deleted)
{
    H5FO_open_obj_t *open_obj;  /* Information about open object */
    herr_t ret_value=SUCCEED;            /* Return value */

    FUNC_ENTER_NOAPI_NOFUNC(H5FO_mark)

    /* Sanity check */
    assert(f);
    assert(f->shared);
    assert(f->shared->open_objs);
    assert(H5F_addr_defined(addr));

    /* Get the object node from the container */
    if((open_obj=H5SL_search(f->shared->open_objs,&addr))!=NULL)
        open_obj->deleted=deleted;
    else
        ret_value=FAIL;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FO_mark() */


/*--------------------------------------------------------------------------
 NAME
    H5FO_marked
 PURPOSE
    Check if an object is marked to be deleted when it is closed
 USAGE
    htri_t H5FO_mark(f,addr)
        const H5F_t *f;         IN: File opened object is in
        haddr_t addr;           IN: Address of object to delete

 RETURNS
    Returns a TRUE/FALSE on success, negative on failure
 DESCRIPTION
    Checks if the object is currently in the "opened objects" tree and
    whether its marks for deletion from the file when it is closed.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5FO_marked(const H5F_t *f, haddr_t addr)
{
    H5FO_open_obj_t *open_obj;  /* Information about open object */
    htri_t ret_value=FAIL;      /* Return value */

    FUNC_ENTER_NOAPI_NOFUNC(H5FO_marked)

    /* Sanity check */
    assert(f);
    assert(f->shared);
    assert(f->shared->open_objs);
    assert(H5F_addr_defined(addr));

    /* Get the object node from the container */
    if((open_obj=H5SL_search(f->shared->open_objs,&addr))!=NULL)
        ret_value=open_obj->deleted;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FO_marked() */


/*--------------------------------------------------------------------------
 NAME
    H5FO_dest
 PURPOSE
    Destroy an open object info set
 USAGE
    herr_t H5FO_create(f)
        H5F_t *f;               IN/OUT: File's opened object info set

 RETURNS
    Returns a non-negative on success, negative on failure
 DESCRIPTION
    Destroy an existing open object info set.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5FO_dest(const H5F_t *f)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5FO_dest,FAIL)

    /* Sanity check */
    assert(f);
    assert(f->shared);
    assert(f->shared->open_objs);

    /* Check if the object info set is empty */
    if(H5SL_count(f->shared->open_objs)!=0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTRELEASE, FAIL, "objects still in open object info set")

    /* Release the open object info set container */
    if(H5SL_close(f->shared->open_objs)<0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTCLOSEOBJ, FAIL, "can't close open object info set")

    f->shared->open_objs=NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FO_dest() */

