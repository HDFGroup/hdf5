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

#include <H5private.h>  /* Generic Functions */
#include <H5Aprivate.h> /* Atom Functions */
#include <H5Eprivate.h> /* Error handling */
#include <H5MMprivate.h> /* Memory Management functions */
#include <H5Pprivate.h> /* Data-space functions */

#define PABLO_MASK	H5P_mask

/*--------------------- Locally scoped variables -----------------------------*/

/* Whether we've installed the library termination function yet for this interface */
static intn interface_initialize_g = FALSE;

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
    herr_t ret_value = SUCCEED;
    FUNC_ENTER (H5P_init_interface, NULL, FAIL);

    /* Initialize the atom group for the file IDs */
    if((ret_value=H5Ainit_group(H5_DATASPACE,H5A_DATASPACEID_HASHSIZE,H5P_RESERVED_ATOMS,H5P_destroy))!=FAIL)
        ret_value=H5_add_exit(&H5P_term_interface);

    FUNC_LEAVE(ret_value);
}	/* H5P_init_interface */

/*--------------------------------------------------------------------------
NAME
   H5P_init -- Make certain that the interface has been initialized
USAGE
    herr_t H5P_init()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Library public routine to make certain the H5P interface has been properly
    initialized.

--------------------------------------------------------------------------*/
herr_t H5P_init(void)
{
    herr_t ret_value = SUCCEED;
    FUNC_ENTER (H5P_init, H5P_init_interface, FAIL);

    /* Actual work is done in the FUNC_ENTER macro */

    FUNC_LEAVE(ret_value);
}	/* H5P_init */

/*--------------------------------------------------------------------------
 NAME
    H5P_term_interface
 PURPOSE
    Terminate various H5P objects
 USAGE
    void H5P_term_interface()
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
void H5P_term_interface (void)
{
    H5Adestroy_group(H5_DATASPACE);
} /* end H5P_term_interface() */

/*--------------------------------------------------------------------------
 NAME
    H5P_create
 PURPOSE
    Create a new HDF5 dimensionality object
 USAGE
    hid_t H5P_create(owner_id, type, name)
        hid_t owner_id;       IN: Group/file which owns this object
        hobjtype_t type;        IN: Type of object to create
        const char *name;       IN: Name of the object
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function actually creates the dimensionality object.
--------------------------------------------------------------------------*/
hid_t H5P_create(hid_t owner_id, hobjtype_t type, const char *name)
{
    H5P_dim_t *new_dim;               /* new dimensionality object to create */
    hid_t ret_value = SUCCEED;

    FUNC_ENTER(H5P_create, H5P_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Allocate space for the new data-type */
    if((new_dim=HDcalloc(1,sizeof(H5P_dim_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
    
    /* Initialize the dimensionality object */
    new_dim->type=H5P_TYPE_UNKNOWN;
    new_dim->s=NULL;

    /* Register the new datatype and get an ID for it */
    if((ret_value=H5Aregister_atom(H5_DATASPACE, (const VOIDP)new_dim))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5P_create() */

/*--------------------------------------------------------------------------
 NAME
    H5P_get_lrank
 PURPOSE
    Return the logical rank of a dataspace (internal)
 USAGE
    uint32 H5P_get_lrank(sdim)
        H5P_sdim_t *sdim;            IN: Pointer to dataspace object to query
 RETURNS
    The logical rank of a dataspace on success, UFAIL on failure
 DESCRIPTION
        This function determines the number of logical dimensions in a 
    dataspace.  The logical rank is the actual number of dimensions of the
    dataspace, not the dimensionality of the space its embedded in.
    UFAIL is returned on an error, otherwise the rank is returned.
--------------------------------------------------------------------------*/
uint32 H5P_get_lrank(H5P_sdim_t *sdim)
{
    uint32 ret_value = UFAIL;

    FUNC_ENTER(H5P_get_lrank, H5P_init_interface, UFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    assert(sdim);
    ret_value=sdim->rank;

#ifdef LATER
done:
#endif /* LATER */
  if(ret_value == UFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5P_get_lrank() */

/*--------------------------------------------------------------------------
 NAME
    H5P_is_simple
 PURPOSE
    Check if a dataspace is simple (internal)
 USAGE
    hbool_t H5P_is_simple(sdim)
        H5P_sdim_t *sdim;            IN: Pointer to dataspace object to query
 RETURNS
    BTRUE/BFALSE/BFAIL
 DESCRIPTION
        This function determines the if a dataspace is "simple". ie. if it
    has orthogonal, evenly spaced dimensions.
--------------------------------------------------------------------------*/
hbool_t H5P_is_simple(H5P_dim_t *sdim)
{
    hbool_t ret_value = BFAIL;

    FUNC_ENTER(H5P_is_simple, H5P_init_interface, UFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    assert(sdim);
    if(sdim->type==H5P_TYPE_UNKNOWN)
      {
        HGOTO_ERROR (H5E_INTERNAL, H5E_UNINITIALIZED, FAIL);
      }
    else 
        ret_value=sdim->type==H5P_TYPE_SIMPLE ? BTRUE : BFALSE; /* Currently all dataspaces are simple, but check anyway */

done:
  if(ret_value == BFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5P_is_simple() */

/*--------------------------------------------------------------------------
 NAME
    H5Pis_simple
 PURPOSE
    Check if a dataspace is simple
 USAGE
    hbool_t H5Pis_simple(sid)
        hid_t sid;            IN: ID of dataspace object to query
 RETURNS
    BTRUE/BFALSE/BFAIL
 DESCRIPTION
        This function determines the if a dataspace is "simple". ie. if it
    has orthogonal, evenly spaced dimensions.
--------------------------------------------------------------------------*/
hbool_t H5Pis_simple(hid_t sid)
{
    H5P_dim_t *space=NULL;      /* dataspace to modify */
    hbool_t        ret_value = BFAIL;

    FUNC_ENTER(H5Pis_simple, H5P_init_interface, BFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    if((space=H5Aatom_object(sid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    ret_value=H5P_is_simple(space);

done:
  if(ret_value == BFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Pis_simple() */

/*--------------------------------------------------------------------------
 NAME
    H5Pset_space
 PURPOSE
    Determine the size of a dataspace
 USAGE
    herr_t H5Pset_space(sid, rank, dims)
        hid_t sid;            IN: Dataspace object to query
        uint32 rank;            IN: # of dimensions for the dataspace
        uint32 *dims;           IN: Size of each dimension for the dataspace
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function sets the number and size of each dimension in the
    dataspace.  Setting RANK to a value of zero allows scalar objects to be
    created.  Dimensions are specified from slowest to fastest changing in the
    DIMS array (i.e. 'C' order).  Setting the size of a dimension to zero
    indicates that the dimension is of unlimited size and should be allowed to
    expand.  Currently, only the first dimension in the array (the slowest) may
    be unlimited in size.
--------------------------------------------------------------------------*/
herr_t H5Pset_space(hid_t sid, uint32 rank, uint32 *dims)
{
    H5P_dim_t *space=NULL;      /* dataspace to modify */
    uintn u;                    /* local counting variable */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Pset_space, H5P_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Get the object */
    if((space=H5Aatom_object(sid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    if(rank>0 && dims==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL);

    /* shift out of the previous state to a "simple" dataspace */
    switch(space->type)
      {
        case H5P_TYPE_UNKNOWN:
            if((space->s=HDcalloc(sizeof(H5P_sdim_t),1))==NULL)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
            break;

        case H5P_TYPE_SIMPLE:
            /* do nothing */
            break;

        case H5P_TYPE_COMPLEX:
            /* eventually this will destroy whatever "complex" dataspace info is retained, right now it's an error */
            /* Fall through to report error */

        default:
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL);
      } /* end switch */
    space->type=H5P_TYPE_SIMPLE;

    if(rank==0)
      { /* scalar variable */
        space->s->rank=0;      /* set to scalar rank */
        space->s->dim_flags=0; /* no maximum dimensions or dimension permutations */
        if(space->s->size!=NULL)
            space->s->size=H5MM_xfree(space->s->size);
        if(space->s->max!=NULL)
            space->s->max=H5MM_xfree(space->s->max);
        if(space->s->perm!=NULL)
            space->s->max=H5MM_xfree(space->s->perm);
      } /* end if */
    else 
      {
        /* Reset the dataspace flags */
        space->s->dim_flags=0;

        /* Free the old space for now */
        if(space->s->size!=NULL)
            space->s->size=H5MM_xfree(space->s->size);
        if(space->s->max!=NULL)
            space->s->max=H5MM_xfree(space->s->max);
        if(space->s->perm!=NULL)
            space->s->perm=H5MM_xfree(space->s->perm);

        /* Set the rank and copy the dims */
        space->s->rank=rank;
        if((space->s->size=HDcalloc(sizeof(uint32),rank))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
        HDmemcpy(space->s->size,dims,sizeof(uint32)*rank);

        /* check if there are unlimited dimensions and create the maximum dims array */
        for(u=0; u<(uintn)rank; u++)
            if(dims[u]==0)
              {
                if(u>0) /* sanity check for unlimited dimensions not in the lowest dimensionality */
                    HGOTO_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL);
                if((space->s->max=HDcalloc(sizeof(uint32),rank))==NULL)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
                HDmemcpy(space->s->max,dims,sizeof(uint32)*rank);
                space->s->dim_flags|=H5P_VALID_MAX;
                break;
              } /* end if */
      } /* end else */

done:
  if(ret_value == FAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Pset_space() */

/*--------------------------------------------------------------------------
 NAME
    H5P_nelem
 PURPOSE
    Return the number of elements in a dataspace (internal)
 USAGE
    uintn H5P_nelem(space)
        H5P_dim_t *space;            IN: Pointer to the dataspace object to query
 RETURNS
    The number of elements in a dataspace on success, UFAIL on failure
 DESCRIPTION
        This function determines the number of dataset elements in a 
    dataspace.  For example, a simple 3-dimensional dataspace with dimensions
    2, 3 and 4 would have 24 elements.
    UFAIL is returned on an error, otherwise the number of elements is returned.
--------------------------------------------------------------------------*/
uintn H5P_nelem(H5P_dim_t *space)
{
    uintn u;                    /* local counting variable */
    uintn        ret_value = UFAIL;

    FUNC_ENTER(H5P_nelem, H5P_init_interface, UFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    assert(space);

    /* Check for anything but simple dataspaces for now */
    if(space->type!=H5P_TYPE_SIMPLE)
        HGOTO_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL);
    
    /* Check for other form of scalar dataspaces */
    if(space->s->rank==0)
        ret_value=1;
    else
      {
        for(ret_value=1, u=0; u<(uintn)space->s->rank; u++)
            ret_value*=space->s->size[u];
      } /* end else */

done:
  if(ret_value == UFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5P_nelem() */

/*--------------------------------------------------------------------------
 NAME
    H5Pnelem
 PURPOSE
    Return the number of elements in a dataspace
 USAGE
    uintn H5Pnelem(sid)
        hid_t sid;            IN: Dataspace object to query
 RETURNS
    The number of elements in a dataspace on success, UFAIL on failure
 DESCRIPTION
        This function determines the number of dataset elements in a 
    dataspace.  For example, a simple 3-dimensional dataspace with dimensions
    2, 3 and 4 would have 24 elements.
    UFAIL is returned on an error, otherwise the number of elements is returned.
--------------------------------------------------------------------------*/
uintn H5Pnelem(hid_t sid)
{
    H5P_dim_t *space=NULL;      /* dataspace to modify */
    uintn        ret_value = UFAIL;

    FUNC_ENTER(H5Pnelem, H5P_init_interface, UFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    if(sid==H5P_SCALAR)
        ret_value=1;
    else
     {
        if((space=H5Aatom_object(sid))==NULL)
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

        ret_value=H5P_nelem(space);
     } /* end else */

done:
  if(ret_value == UFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Pnelem() */

/*--------------------------------------------------------------------------
 NAME
    H5P_destroy
 PURPOSE
    Private function to destroy dataspace objects.
 USAGE
    void H5P_destroy(dataspace)
        void *dataspace;       IN: Pointer to dataspace object to destroy
 RETURNS
    none
 DESCRIPTION
    This function releases whatever memory is used by a dataspace object.
    It should only be called from the atom manager when the reference count
    for a dataspace drops to zero.
--------------------------------------------------------------------------*/
void H5P_destroy(void *dataspace)
{
    H5P_dim_t *dim=(H5P_dim_t *)dataspace;  /* dimensionality object to release */

    /* Don't call standard init/leave code, this is a private void function */
    /* FUNC_ENTER(H5T_destroy, H5T_init_interface, FAIL); */

    if(dim->type==H5P_TYPE_SIMPLE)
      {
        if(dim->s!=NULL)
          {
            if(dim->s->size!=NULL)
                HDfree(dim->s->size);
            if(dim->s->max!=NULL)
                HDfree(dim->s->max);
            if(dim->s->perm!=NULL)
                HDfree(dim->s->perm);
            HDfree(dim->s);
          } /* end if */
      } /* end if */
    HDfree(dim);

#ifdef LATER
done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
#endif /* LATER */

} /* H5P_destroy */

/*--------------------------------------------------------------------------
 NAME
    H5P_release
 PURPOSE
    Release access to an HDF5 dimensionality object.
 USAGE
    herr_t H5P_release(oid)
        hid_t oid;       IN: Object to release access to
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function releases a dimensionality from active use by a user.
--------------------------------------------------------------------------*/
herr_t H5P_release(hid_t oid)
{
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5P_release, H5P_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Chuck the object! :-) */
    if(H5Adec_ref(oid)==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5P_release() */

