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

#ifdef NOT_YET
/*--------------------------------------------------------------------------
 NAME
    H5Tcreate
 PURPOSE
    Create a new HDF5 data-type object
 USAGE
    hatom_t H5Tcreate(owner_id, type, name)
        hatom_t owner_id;       IN: Group/file which owns this object
        hobjtype_t type;        IN: Type of object to create
        const char *name;       IN: Name of the object
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function actually creates the data-type object.
--------------------------------------------------------------------------*/
hatom_t H5T_create(hatom_t owner_id, hobjtype_t type, const char *name)
{
    CONSTR(FUNC, "H5T_create");     /* for HERROR */
    h5_datatype_t *new_dt;            /* new data-type object to create */
    hatom_t ret_value = SUCCEED;

    FUNC_ENTER(H5T_mask, ID_H5Tcreate, H5T_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Allocate space for the new data-type */
    if((new_dt=HDmalloc(sizeof(h5_datatype_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
    
    /* Initialize the datatype */
    new_dt->dt.base=0;           /* No Default datatype */
    new_dt->name=HDstrdup(name); /* Make a copy of the datatype's name */
    new_dt->ci=NULL;             /* Set the complex information to NULL */

    /* Register the new datatype and get an ID for it */
    if((ret_value=H5Aregister_atom(H5_DATATYPE, (const VOIDP)new_dt))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(H5T_mask, ID_H5Tcreate, ret_value);
} /* end H5T_create() */
#endif /* NOT_YET */

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

#ifdef NOT_YET
/*--------------------------------------------------------------------------
 NAME
    H5Trelease
 PURPOSE
    Release access to an HDF5 datatype object.
 USAGE
    herr_t H5Trelease(oid)
        hatom_t oid;       IN: Object to release access to
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function releases a datatype from active use by a user.
--------------------------------------------------------------------------*/
herr_t H5T_release(hatom_t oid)
{
    CONSTR(FUNC, "H5T_release");    /* for HERROR */
    h5_datatype_t *dt;         /* new data-type object to create */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5T_mask, ID_H5Trelease, H5T_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Chuck the object! :-) */
    if((dt=H5Aremove_atom(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    if(dt->name!=NULL)
        HDfree(dt->name);
    if(dt->ci!=NULL)
      {
      } /* end if */
    HDfree(dt);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(H5T_mask, ID_H5Trelease, ret_value);
} /* end H5T_release() */
#endif /* NOT_YET */

