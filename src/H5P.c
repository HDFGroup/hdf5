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
       H5P.c
   HDF5 Data-space routines

   EXPORTED ROUTINES
       H5Pcreate    -- Create a data-space
       H5Prelease   -- Release access to a data-space

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
       H5P_init_interface    -- initialize the interface
   + */

#include "hdf5.h"
#include "H5private.h"  /* Generic Functions */
#include "H5Pprivate.h" /* Data-space functions */

/*--------------------- Locally scoped variables -----------------------------*/

/* Whether we've installed the library termination function yet for this interface */
static intn interface_initialize = FALSE;

/*--------------------------------------------------------------------------
NAME
   H5P_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5P_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t H5P_init_interface(void)
{
#ifdef LATER
    CONSTR(FUNC, "H5P_init_interface");	/* For HERROR */
#endif /* LATER */
    herr_t ret_value = SUCCEED;

    /* Don't use "FUNC_ENTER" macro, to avoid potential infinite recursion */
    PABLO_TRACE_ON(H5P_mask, ID_H5P_init_interface);

    /* Don't call this routine again... */
    interface_initialize = TRUE;

    /* Initialize the atom group for the file IDs */
    ret_value=H5Ainit_group(H5_DATASPACE,HDF5_DATASPACEID_HASHSIZE,H5P_RESERVED_ATOMS);

    FUNC_LEAVE(H5P_mask, ID_H5P_init_interface, ret_value);
}	/* H5P_init_interface */

/*--------------------------------------------------------------------------
 NAME
    H5P_create
 PURPOSE
    Create a new HDF5 dimensionality object
 USAGE
    hatom_t H5P_create(owner_id, type, name)
        hatom_t owner_id;       IN: Group/file which owns this object
        hobjtype_t type;        IN: Type of object to create
        const char *name;       IN: Name of the object
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function actually creates the dimensionality object.
--------------------------------------------------------------------------*/
hatom_t H5P_create(hatom_t owner_id, hobjtype_t type, const char *name)
{
    CONSTR(FUNC, "H5P_create");     /* for HERROR */
    H5P_dim_t *new_dim;               /* new dimensionality object to create */
    hatom_t ret_value = SUCCEED;

    FUNC_ENTER(H5P_mask, ID_H5P_create, H5P_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Allocate space for the new data-type */
    if((new_dim=HDmalloc(sizeof(H5P_dim_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
    
    /* Initialize the dimensionality object */
    new_dim->rank=0;
    new_dim->dims=NULL;

    /* Register the new datatype and get an ID for it */
    if((ret_value=H5Aregister_atom(H5_DATASPACE, (const VOIDP)new_dim))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(H5P_mask, ID_H5P_create, ret_value);
} /* end H5P_create() */

/*--------------------------------------------------------------------------
 NAME
    H5Pnelem
 PURPOSE
    Return the number of elements in a dataspace
 USAGE
    uintn H5Pnelem(sid)
        hatom_t sid;            IN: Dataspace object to query
 RETURNS
    The number of elements in a dataspace on success, UFAIL on failure
 DESCRIPTION
        This function determines the number of dataset elements in a 
    dataspace.  For example, a simple 3-dimensional dataspace with dimensions
    2, 3 and 4 would have 24 elements.
    UFAIL is returned on an error, otherwise the number of elements is returned.
--------------------------------------------------------------------------*/
uintn H5Pnelem(hatom_t sid)
{
    CONSTR(FUNC, "H5Pnelem");    /* for HERROR */
    uintn        ret_value = UFAIL;

    FUNC_ENTER(H5P_mask, ID_H5Pnelem, H5P_init_interface, UFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

#ifdef FINISH_THIS
#else /* FINISH_THIS */
    if(sid==H5P_SCALAR)
        ret_value=1;
#endif /* FINISH_THIS */

done:
  if(ret_value == UFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(H5P_mask, ID_H5Pnelem, ret_value);
} /* end H5Pnelem() */

/*--------------------------------------------------------------------------
 NAME
    H5P_release
 PURPOSE
    Release access to an HDF5 dimensionality object.
 USAGE
    herr_t H5P_release(oid)
        hatom_t oid;       IN: Object to release access to
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function releases a dimensionality from active use by a user.
--------------------------------------------------------------------------*/
herr_t H5P_release(hatom_t oid)
{
    CONSTR(FUNC, "H5P_release");    /* for HERROR */
    H5P_dim_t *dim;         /* dimensionality object to release */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5P_mask, ID_H5Prelease, H5P_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Chuck the object! :-) */
    if((dim=H5Aremove_atom(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    if(dim->rank>0)
        HDfree(dim->dims);
    HDfree(dim);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(H5P_mask, ID_H5P_release, ret_value);
} /* end H5P_release() */

