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
       hdf5meta.c
   HDF5 "Meta-Object" routines

   EXPORTED ROUTINES
       H5Mcreate    -- Create an object
       H5Mcopy      -- Copy an object
       H5Mrelease   -- Release access to an object

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
       H5M_init_interface    -- initialize the interface
   + */

#include "hdf5.h"
#include "H5private.h"  /* Generic functions */
#include "H5Cproto.h"   /* Template interface */
#include "H5Tproto.h"   /* Datatype interface */
#include "H5Mprivate.h" /* Meta-object interface */
#include "H5Cprivate.h"   /* Template interface */

#define PABLO_MASK	H5M_mask

/*--------------------- Locally scoped variables -----------------------------*/

/* Whether we've installed the library termination function yet for this interface */
static intn interface_initialize_g = FALSE;

/*------------------_-- Local function prototypes ----------------------------*/
static herr_t H5M_init_interface(void);

/*--------------------------------------------------------------------------
NAME
   H5M_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5M_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

MODIFICATIONS
    Robb Matzke, 4 Aug 1997
    Changed the FUNC variable value to H5M_init_interface.

--------------------------------------------------------------------------*/
static herr_t H5M_init_interface(void)
{
    herr_t ret_value = SUCCEED;
    FUNC_ENTER (H5M_init_interface, NULL, FAIL);

    FUNC_LEAVE(ret_value);
}	/* H5M_init_interface */

/*--------------------------------------------------------------------------
 NAME
    H5M_find_type
 PURPOSE
    Find the type of meta-object to issue a method call on
 USAGE
    intn H5M_find_type(type)
        hobjtype_t type;        IN: Type of object to create
 RETURNS
     Returns the index of the type in the array of methods on success, or FAIL 
        on failure.
 DESCRIPTION
        This function performs a search to find the index of the type of a 
    meta-object in the array of function pointers.
--------------------------------------------------------------------------*/
static intn H5M_find_type(hobjtype_t type)
{
    intn i;         /* local counting variable */
    intn ret_value = FAIL;

    FUNC_ENTER(H5M_find_type, H5M_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /*
     * Currently this uses a stright linear search, which can easily be changed
     * to a binary search when it becomes too slow.
     */
    for(i=0; i<(sizeof(meta_func_arr)/sizeof(meta_func_t)); i++)
        if(type==meta_func_arr[i].type)
            HGOTO_DONE(i);

done:
    if(ret_value == FAIL)   
      { /* Error condition cleanup */

      } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5M_find_type() */

/*--------------------------------------------------------------------------
 NAME
    H5Mcreate
 PURPOSE
    Create a new HDF5 object.
 USAGE
    hatom_t H5Mcreate(owner_id, type, name)
        hatom_t owner_id;       IN: Group/file which owns this object
        hobjtype_t type;        IN: Type of object to create
        const char *name;       IN: Name of the object
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function re-directs the object's creation into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
hatom_t H5Mcreate(hatom_t owner_id, hobjtype_t type, const char *name)
{
    intn i;         /* local counting variable */
    hatom_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Mcreate, H5M_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(type<=BADGROUP || type>=MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

    i=H5M_find_type(type);
    if(meta_func_arr[i].create==NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);
    ret_value=meta_func_arr[i].create(owner_id,type,name);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Mcreate() */


/*--------------------------------------------------------------------------
 NAME
    H5Mcopy
 PURPOSE
    Copy an HDF5 object.
 USAGE
    hatom_t H5Mcopy(oid)
        hatom_t oid;       IN: Object to copy
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function re-directs the object's copy into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
hatom_t H5Mcopy(hatom_t oid)
{
    group_t group=H5Aatom_group(oid);   /* Atom group for incoming object */
    intn i;         /* local counting variable */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Mcopy, H5M_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(group<=BADGROUP || group>=MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

    i=H5M_find_type(group);
    if(meta_func_arr[i].copy==NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);
    ret_value=meta_func_arr[i].copy(oid);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Mcopy() */

/*--------------------------------------------------------------------------
 NAME
    H5Mrelease
 PURPOSE
    Release access to an HDF5 object.
 USAGE
    herr_t H5Mrelease(oid)
        hatom_t oid;       IN: Object to release access to
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function re-directs the object's release into the appropriate
    interface, as defined by the function pointers in hdf5fptr.h
--------------------------------------------------------------------------*/
herr_t H5Mrelease(hatom_t oid)
{
    group_t group=H5Aatom_group(oid);   /* Atom group for incoming object */
    intn i;         /* local counting variable */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Mrelease, H5M_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(group<=BADGROUP || group>=MAXGROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

    i=H5M_find_type(group);
    if(meta_func_arr[i].release==NULL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);
    ret_value=meta_func_arr[i].release(oid);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Mrelease() */

