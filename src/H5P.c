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


#include <H5private.h>  	/* Generic Functions			*/
#include <H5Aprivate.h> 	/* Atom Functions			*/
#include <H5Eprivate.h> 	/* Error handling			*/
#include <H5MMprivate.h> 	/* Memory Management functions		*/
#include <H5Oprivate.h>		/*object headers			*/
#include <H5Pprivate.h> 	/* Data-space functions			*/

#define PABLO_MASK	H5P_mask

/* Interface initialization */
static intn interface_initialize_g = FALSE;
#define INTERFACE_INIT	H5P_init_interface
static herr_t H5P_init_interface (void);
static void H5P_term_interface (void);

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
static herr_t
H5P_init_interface (void)
{
   herr_t ret_value = SUCCEED;
   FUNC_ENTER (H5P_init_interface, FAIL);

   /* Initialize the atom group for the file IDs */
   if ((ret_value=H5Ainit_group (H5_DATASPACE, H5A_DATASPACEID_HASHSIZE,
				 H5P_RESERVED_ATOMS,
				 (herr_t (*)(void*))H5P_close))!=FAIL) {
      ret_value=H5_add_exit(&H5P_term_interface);
   }

   FUNC_LEAVE (ret_value);
}

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
static void
H5P_term_interface (void)
{
   H5Adestroy_group (H5_DATASPACE);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pcreate
 *
 * Purpose:	Creates a new data space object and opens it for access.
 *
 * Return:	Success:	The ID for the new data space object.
 *
 *		Failure:	FAIL
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Pcreate (H5P_class_t type)
{
   H5P_t	*ds = NULL;
   hid_t	ret_value = FAIL;
   
   FUNC_ENTER (H5Pcreate, FAIL);
   H5ECLEAR;

   ds = H5MM_xcalloc (1, sizeof(H5P_t));
   ds->type = type;
   
   switch (type) {
   case H5P_SCALAR:
      /*void*/
      break;
      
   case H5P_SIMPLE:
      ds->u.simple.rank = 0;
      break;

   case H5P_COMPLEX:
      /* Complex types are not supported yet */
      HGOTO_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, FAIL);

   default:
      /* Unknown data space type */
      HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL);
   }

   /* Register the new data space and get an ID for it */
   if ((ret_value = H5Aregister_atom (H5_DATASPACE, ds))<0) {
      HGOTO_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL);
   }

 done:
   if (ret_value<0) {
      H5MM_xfree (ds);
   }

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pclose
 *
 * Purpose:	Release access to a data space object.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pclose (hid_t space_id)
{
   FUNC_ENTER (H5Pclose, FAIL);
   H5ECLEAR;

   /* check args */
   if (H5_DATASPACE!=H5Aatom_group (space_id) ||
       NULL==H5Aatom_object (space_id)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL); /*not a data space*/
   }
   
   /* When the reference count reaches zero the resources are freed */
   if (H5A_dec_ref (space_id)<0) {
      HRETURN_ERROR (H5E_ATOM, H5E_BADATOM, FAIL); /*problem freeing id*/
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_close
 *
 * Purpose:	Releases all memory associated with a data space.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_close (H5P_t *ds)
{
   FUNC_ENTER (H5P_close, FAIL);

   assert (ds);

   switch (ds->type) {
   case H5P_SCALAR:
      /*void*/
      break;
      
   case H5P_SIMPLE:
      H5MM_xfree (ds->u.simple.size);
      H5MM_xfree (ds->u.simple.max);
      H5MM_xfree (ds->u.simple.perm);
      break;

   case H5P_COMPLEX:
      /* nothing */
      break;

   default:
      assert ("unknown data space type" && 0);
      break;
   }
   H5MM_xfree (ds);
   
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_copy
 *
 * Purpose:	Copies a data space.
 *
 * Return:	Success:	A pointer to a new copy of SRC
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5P_t *
H5P_copy (const H5P_t *src)
{
   H5P_t	*dst = NULL;
   int		i;
   
   FUNC_ENTER (H5P_copy, NULL);

   dst = H5MM_xmalloc (sizeof(H5P_t));
   *dst = *src;

   switch (dst->type) {
   case H5P_SCALAR:
      /*void*/
      break;
      
   case H5P_SIMPLE:
      if (dst->u.simple.size) {
	 dst->u.simple.size = H5MM_xmalloc (dst->u.simple.rank * sizeof(intn));
	 for (i=0; i<dst->u.simple.rank; i++) {
	    dst->u.simple.size[i] = src->u.simple.size[i];
	 }
      }
      if (dst->u.simple.max) {
	 dst->u.simple.max = H5MM_xmalloc (dst->u.simple.rank * sizeof(intn));
	 for (i=0; i<dst->u.simple.rank; i++) {
	    dst->u.simple.max[i] = src->u.simple.max[i];
	 }
      }
      if (dst->u.simple.perm) {
	 dst->u.simple.perm = H5MM_xmalloc (dst->u.simple.rank * sizeof(intn));
	 for (i=0; i<dst->u.simple.rank; i++) {
	    dst->u.simple.perm[i] = src->u.simple.perm[i];
	 }
      }
      break;

   case H5P_COMPLEX:
      /*void*/
      break;

   default:
      assert ("unknown data space type" && 0);
      break;
   }

   FUNC_LEAVE (dst);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_npoints
 *
 * Purpose:	Determines how many data points a data set has.
 *
 * Return:	Success:	Number of data points in the data set.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Pget_npoints (hid_t space_id)
{
   H5P_t	*ds = NULL;
   size_t	ret_value = 0;

   FUNC_ENTER(H5Pget_npoints, 0);
   H5ECLEAR;

   /* check args */
   if (H5_DATASPACE!=H5Aatom_group (space_id) ||
       NULL==(ds=H5Aatom_object (space_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, 0); /*not a data space*/
   }

   ret_value = H5P_get_npoints (ds);

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_get_npoints
 *
 * Purpose:	Determines how many data points a data set has.
 *
 * Return:	Success:	Number of data points in the data set.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5P_get_npoints (const H5P_t *ds)
{
   size_t	ret_value = 0;
   intn		i;
   
   FUNC_ENTER(H5P_get_npoints, 0);

   /* check args */
   assert (ds);

   switch (ds->type) {
   case H5P_SCALAR:
      ret_value = 1;
      break;

   case H5P_SIMPLE:
      for (ret_value=1, i=0; i<ds->u.simple.rank; i++) {
	 ret_value *= ds->u.simple.size[i];
      }
      break;

   case H5P_COMPLEX:
      /* complex data spaces are not supported yet */
      HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, 0);

   default:
      assert ("unknown data space class" && 0);
      HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, 0);
   }

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_ndims
 *
 * Purpose:	Determines the dimensionality of a data space.
 *
 * Return:	Success:	The number of dimensions in a data space.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5Pget_ndims (hid_t space_id)
{
   H5P_t	*ds = NULL;
   size_t	ret_value = 0;

   FUNC_ENTER(H5Pget_ndims, FAIL);
   H5ECLEAR;

   /* check args */
   if (H5_DATASPACE!=H5Aatom_group (space_id) ||
       NULL==(ds=H5Aatom_object (space_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL); /*not a data space*/
   }

   ret_value = H5P_get_ndims (ds);

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_get_ndims
 *
 * Purpose:	Returns the number of dimensions in a data space.
 *
 * Return:	Success:	Non-negative number of dimensions.  Zero
 *				implies a scalar.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5P_get_ndims (const H5P_t *ds)
{
   intn		ret_value = FAIL;
   
   FUNC_ENTER (H5P_get_ndims, FAIL);

   /* check args */
   assert (ds);

   switch (ds->type) {
   case H5P_SCALAR:
      ret_value = 0;
      break;

   case H5P_SIMPLE:
      ret_value = ds->u.simple.rank;
      break;

   case H5P_COMPLEX:
      /* complex data spaces are not supported yet */
      HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, FAIL);

   default:
      assert ("unknown data space class" && 0);
      HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, FAIL);
   }

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dims
 *
 * Purpose:	Returns the size in each dimension of a data space DS through
 *		the DIMS argument.
 *
 * Return:	Success:	Number of dimensions, the same value as
 *				returned by H5Pget_ndims().
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5Pget_dims (hid_t space_id, size_t dims[]/*out*/)
{
   
   H5P_t	*ds = NULL;
   size_t	ret_value = 0;

   FUNC_ENTER(H5Pget_dims, FAIL);
   H5ECLEAR;

   /* check args */
   if (H5_DATASPACE!=H5Aatom_group (space_id) ||
       NULL==(ds=H5Aatom_object (space_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL); /*not a data space*/
   }
   if (!dims) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL); /*no output buffer*/
   }
   

   ret_value = H5P_get_dims (ds, dims);

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_get_dims
 *
 * Purpose:	Returns the size in each dimension of a data space.  This
 *		function may not be meaningful for all types of data spaces.
 *
 * Return:	Success:	Number of dimensions.  Zero implies scalar.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5P_get_dims (const H5P_t *ds, size_t dims[])
{
   intn		ret_value = FAIL;
   intn		i;
   
   FUNC_ENTER (H5P_get_dims, FAIL);

   /* check args */
   assert (ds);
   assert (dims);

   switch (ds->type) {
   case H5P_SCALAR:
      ret_value = 0;
      break;

   case H5P_SIMPLE:
      ret_value = ds->u.simple.rank;
      for (i=0; i<ret_value; i++) {
	 dims[i] = ds->u.simple.size[i];
      }
      break;

   case H5P_COMPLEX:
      /* complex data spaces are not supported yet */
      HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, FAIL);

   default:
      assert ("unknown data space class" && 0);
      HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, FAIL);
   }

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_modify
 *
 * Purpose:	Updates a data space by writing a message to an object
 *		header.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_modify (H5F_t *f, H5G_entry_t *ent, const H5P_t *ds)
{
   FUNC_ENTER (H5O_modify, FAIL);

   assert (f);
   assert (ent);
   assert (ds);

   switch (ds->type) {
   case H5P_SCALAR:
      /* Scalar data spaces are not implemented yet */
      HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, FAIL);

   case H5P_SIMPLE:
      if (H5O_modify (f, NO_ADDR, ent, H5O_SDSPACE, 0, &(ds->u.simple))<0) {
	 /* Can't update simple data space message */
	 HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL);
      }
      break;

   case H5P_COMPLEX:
      /* Complex data spaces are not implemented yet */
      HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, FAIL);

   default:
      assert ("unknown data space class" && 0);
      break;
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_read
 *
 * Purpose:	Reads the data space from an object header.
 *
 * Return:	Success:	Pointer to a new data space.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5P_t *
H5P_read (H5F_t *f, H5G_entry_t *ent)
{
   H5P_t	*ds = NULL;
   
   FUNC_ENTER (H5P_read, NULL);

   /* check args */
   assert (f);
   assert (ent);

   ds = H5MM_xcalloc (1, sizeof(H5P_t));

   if (H5O_read (f, NO_ADDR, ent, H5O_SDSPACE, 0, &(ds->u.simple))) {
      ds->type = H5P_SIMPLE;
      
   } else {
      ds->type = H5P_SCALAR;
   }

   FUNC_LEAVE (ds);
}



/*-------------------------------------------------------------------------
 * Function:	H5P_cmp
 *
 * Purpose:	Compares two data spaces.
 *
 * Return:	Success:	0 if DS1 and DS2 are the same.
 *				<0 if DS1 is less than DS2.
 * 				>0 if DS1 is greater than DS2.
 *
 *		Failure:	0, never fails
 *
 * Programmer:	Robb Matzke
 *              Wednesday, December 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5P_cmp (const H5P_t *ds1, const H5P_t *ds2)
{
   intn		i;
   
   FUNC_ENTER (H5P_cmp, 0);

   /* check args */
   assert (ds1);
   assert (ds2);

   /* compare */
   if (ds1->type < ds2->type) HRETURN (-1);
   if (ds1->type > ds2->type) HRETURN (1);

   switch (ds1->type) {
   case H5P_SIMPLE:
      if (ds1->u.simple.rank < ds2->u.simple.rank) HRETURN (-1);
      if (ds1->u.simple.rank > ds2->u.simple.rank) HRETURN (1);

      /* don't compare flags */

      for (i=0; i<ds1->u.simple.rank; i++) {
	 if (ds1->u.simple.size[i] < ds2->u.simple.size[i]) HRETURN (-1);
	 if (ds1->u.simple.size[i] > ds2->u.simple.size[i]) HRETURN (1);
      }

      /* don't compare max dimensions */
      
      for (i=0; i<ds1->u.simple.rank; i++) {
	 if ((ds1->u.simple.perm?ds1->u.simple.perm[i]:i) <
	     (ds2->u.simple.perm?ds2->u.simple.perm[i]:i)) HRETURN (-1);
	 if ((ds1->u.simple.perm?ds2->u.simple.perm[i]:i) >
	     (ds2->u.simple.perm?ds2->u.simple.perm[i]:i)) HRETURN (1);
      }

      break;

   default:
      assert ("not implemented yet" && 0);
   }

   FUNC_LEAVE (0);
}


/*--------------------------------------------------------------------------
 NAME
    H5P_is_simple
 PURPOSE
    Check if a dataspace is simple (internal)
 USAGE
    hbool_t H5P_is_simple(sdim)
        H5P_t *sdim;            IN: Pointer to dataspace object to query
 RETURNS
    BTRUE/BFALSE/BFAIL
 DESCRIPTION
        This function determines the if a dataspace is "simple". ie. if it
    has orthogonal, evenly spaced dimensions.
--------------------------------------------------------------------------*/
hbool_t
H5P_is_simple (const H5P_t *sdim)
{
    hbool_t ret_value = BFAIL;

    FUNC_ENTER(H5P_is_simple, UFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    assert(sdim);
    ret_value=sdim->type==H5P_SIMPLE ? BTRUE : BFALSE; /* Currently all dataspaces are simple, but check anyway */


    FUNC_LEAVE(ret_value);
}

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
    H5P_t *space=NULL;      /* dataspace to modify */
    hbool_t        ret_value = BFAIL;

    FUNC_ENTER(H5Pis_simple, BFAIL);

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
}

/*--------------------------------------------------------------------------
 NAME
    H5Pset_space
 PURPOSE
    Determine the size of a dataspace
 USAGE
    herr_t H5Pset_space(sid, rank, dims)
        hid_t sid;            IN: Dataspace object to query
        intn rank;            IN: # of dimensions for the dataspace
        const intn *dims;     IN: Size of each dimension for the dataspace
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
herr_t H5Pset_space(hid_t sid, intn rank, const intn *dims)
{
    H5P_t *space=NULL;      /* dataspace to modify */
    intn u;                    /* local counting variable */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Pset_space, FAIL);

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
        case H5P_SIMPLE:
            /* do nothing */
            break;

        case H5P_COMPLEX:
            /*
	     * eventually this will destroy whatever "complex" dataspace info
	     * is retained, right now it's an error
	     */
            /* Fall through to report error */

        default:
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL);
      } /* end switch */
    space->type=H5P_SIMPLE;

    if(rank==0)
      { /* scalar variable */
        space->u.simple.rank=0;      /* set to scalar rank */
        space->u.simple.dim_flags=0; /* no maximum dimensions or dimension permutations */
        if(space->u.simple.size!=NULL)
            space->u.simple.size=H5MM_xfree(space->u.simple.size);
        if(space->u.simple.max!=NULL)
            space->u.simple.max=H5MM_xfree(space->u.simple.max);
        if(space->u.simple.perm!=NULL)
            space->u.simple.max=H5MM_xfree(space->u.simple.perm);
      } /* end if */
    else 
      {
        /* Reset the dataspace flags */
        space->u.simple.dim_flags=0;

        /* Free the old space for now */
        if(space->u.simple.size!=NULL)
            space->u.simple.size=H5MM_xfree(space->u.simple.size);
        if(space->u.simple.max!=NULL)
            space->u.simple.max=H5MM_xfree(space->u.simple.max);
        if(space->u.simple.perm!=NULL)
            space->u.simple.perm=H5MM_xfree(space->u.simple.perm);

        /* Set the rank and copy the dims */
        space->u.simple.rank=rank;
        if((space->u.simple.size=HDcalloc(sizeof(intn),rank))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
        HDmemcpy(space->u.simple.size,dims,sizeof(intn)*rank);

        /* check if there are unlimited dimensions and create the maximum dims array */
        for(u=0; u<rank; u++)
            if(dims[u]==0)
              {
                if(u>0) /* sanity check for unlimited dimensions not in the lowest dimensionality */
                    HGOTO_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL);
                if((space->u.simple.max=HDcalloc(sizeof(intn),rank))==NULL)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
                HDmemcpy(space->u.simple.max,dims,sizeof(intn)*rank);
                space->u.simple.dim_flags|=H5P_VALID_MAX;
                break;
              } /* end if */
      } /* end else */

done:
  if(ret_value == FAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
}
