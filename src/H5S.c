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

#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#include <H5private.h>		/* Generic Functions			  */
#include <H5Iprivate.h>		/* ID Functions		  */
#include <H5Eprivate.h>		/* Error handling		  */
#include <H5MMprivate.h>	/* Memory Management functions		  */
#include <H5Oprivate.h>		/*object headers		  */
#include <H5Sprivate.h>		/* Data-space functions			  */

/* Interface initialization */
#define PABLO_MASK	H5S_mask
#define INTERFACE_INIT	H5S_init_interface
static intn		interface_initialize_g = FALSE;
static herr_t		H5S_init_interface(void);
static void		H5S_term_interface(void);


/*--------------------------------------------------------------------------
NAME
   H5S_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5S_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5S_init_interface(void)
{
    herr_t		    ret_value = SUCCEED;
    FUNC_ENTER(H5S_init_interface, FAIL);

    /* Initialize the atom group for the file IDs */
    if ((ret_value = H5I_init_group(H5_DATASPACE, H5I_DATASPACEID_HASHSIZE,
				    H5S_RESERVED_ATOMS,
				    (herr_t (*)(void *)) H5S_close)) != FAIL) {
	ret_value = H5_add_exit(&H5S_term_interface);
    }
    FUNC_LEAVE(ret_value);
}


/*--------------------------------------------------------------------------
 NAME
    H5S_term_interface
 PURPOSE
    Terminate various H5S objects
 USAGE
    void H5S_term_interface()
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
H5S_term_interface(void)
{
    H5I_destroy_group(H5_DATASPACE);
}

/*-------------------------------------------------------------------------
 * Function:	H5Screate_simple
 *
 * Purpose:	Creates a new simple data space object and opens it for
 *		access. The DIMS argument is the size of the simple dataset
 *		and the MAXDIMS argument is the upper limit on the size of
 *		the dataset.  MAXDIMS may be the null pointer in which case
 *		the upper limit is the same as DIMS.  If an element of
 *		MAXDIMS is H5S_UNLIMITED then the corresponding dimension is
 *		unlimited, otherwise no element of MAXDIMS should be smaller
 *		than the corresponding element of DIMS.
 *
 * Return:	Success:	The ID for the new simple data space object.
 *
 *		Failure:	FAIL
 *
 * Errors:
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, January  27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Screate_simple(int rank, const hsize_t *dims, const hsize_t *maxdims)
{
    H5S_t	*ds = NULL;
    hid_t	ret_value = FAIL;
    int		i;

    FUNC_ENTER(H5Screate, FAIL);

    /* Check arguments */
    if (rank<0) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "dimensionality cannot be negative");
    }
    if (!dims) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "no dimensions specified");
    }
    if (maxdims) {
	for (i=0; i<rank; i++) {
	    if (H5S_UNLIMITED!=maxdims[i] && maxdims[i]<dims[i]) {
		HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			       "maxdims is smaller than dims");
	    }
	}
    }

    /* Create a new data space */
    ds = H5MM_xcalloc(1, sizeof(H5S_t));
#ifdef LATER /* QAK */
    if(rank>0)	/* for creating simple dataspace */
      {
#endif /* LATER */
	ds->type = H5S_SIMPLE;
	ds->hslab_def = FALSE;	  /* no hyperslab defined currently */

	/* Initialize rank and dimensions */
	ds->u.simple.rank = rank;

	ds->u.simple.size = H5MM_xcalloc(1, rank*sizeof(hsize_t));
	HDmemcpy(ds->u.simple.size, dims, rank*sizeof(hsize_t));

	if (maxdims) {
	    ds->u.simple.max = H5MM_xcalloc(1, rank*sizeof(hsize_t));
	    HDmemcpy (ds->u.simple.max, maxdims, rank*sizeof(hsize_t));
	}
#ifdef LATER /* QAK */
      } /* end if */
    else /* rank==0, for scalar data space */
      {
	ds->type = H5S_SCALAR;
      } /* end else */
#endif /* LATER */
    
    /* Register the new data space and get an ID for it */
    if ((ret_value = H5I_register(H5_DATASPACE, ds)) < 0) {
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
		    "unable to register data space for ID");
    }

 done:
    if (ret_value < 0) {
	H5MM_xfree(ds);
    }
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Sclose
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
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Sclose(hid_t space_id)
{
    FUNC_ENTER(H5Sclose, FAIL);

    /* Check args */
    if (H5_DATASPACE != H5I_group(space_id) ||
	NULL == H5I_object(space_id)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    /* When the reference count reaches zero the resources are freed */
    if (H5I_dec_ref(space_id) < 0) {
	HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "problem freeing id");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_close
 *
 * Purpose:	Releases all memory associated with a data space.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_close(H5S_t *ds)
{
    FUNC_ENTER(H5S_close, FAIL);

    assert(ds);

    switch (ds->type) {
    case H5S_SCALAR:
	/*void */
	break;

    case H5S_SIMPLE:
    H5S_close_simple(&(ds->u.simple));
	break;

    case H5S_COMPLEX:
	/* nothing */
	break;

    default:
	assert("unknown data space type" && 0);
	break;
    }
    if(ds->hslab_def==TRUE) {
	H5MM_xfree(ds->h.start);
	H5MM_xfree(ds->h.count);
	H5MM_xfree(ds->h.stride);
    } /* end if */
    H5MM_xfree(ds);

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_close_simple
 *
 * Purpose:	Releases all memory associated with a simple data space.
 *          (but doesn't free the simple space itself)
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Friday, April  17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_close_simple(H5S_simple_t *simple)
{
    FUNC_ENTER(H5S_close_simple, FAIL);

    assert(simple);

	H5MM_xfree(simple->size);
	H5MM_xfree(simple->max);
	H5MM_xfree(simple->perm);

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Scopy
 *
 * Purpose:	Copies a dataspace.
 *
 * Return:	Success:	ID of the new dataspace
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Friday, January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Scopy (hid_t space_id)
{
    H5S_t	*src = NULL;
    H5S_t	*dst = NULL;
    hid_t	ret_value = FAIL;
    
    FUNC_ENTER (H5Scopy, FAIL);

    /* Check args */
    if (H5_DATASPACE!=H5I_group (space_id) ||
	NULL==(src=H5I_object (space_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }

    /* Copy */
    if (NULL==(dst=H5S_copy (src))) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to copy data space");
    }

    /* Atomize */
    if ((ret_value=H5I_register (H5_DATASPACE, dst))<0) {
	HRETURN_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL,
		       "unable to register data space atom");
    }

    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_copy
 *
 * Purpose:	Copies a data space.
 *
 * Return:	Success:	A pointer to a new copy of SRC
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5S_t *
H5S_copy(const H5S_t *src)
{
    H5S_t		   *dst = NULL;
    int			    i;

    FUNC_ENTER(H5S_copy, NULL);

    dst = H5MM_xmalloc(sizeof(H5S_t));
    *dst = *src;

    switch (dst->type) {
    case H5S_SCALAR:
	/*void */
	break;

    case H5S_SIMPLE:
	if (dst->u.simple.size) {
	    dst->u.simple.size = H5MM_xmalloc(dst->u.simple.rank *
					      sizeof(dst->u.simple.size[0]));
	    for (i = 0; i < dst->u.simple.rank; i++) {
		dst->u.simple.size[i] = src->u.simple.size[i];
	    }
	}
	if (dst->u.simple.max) {
	    dst->u.simple.max = H5MM_xmalloc(dst->u.simple.rank *
					     sizeof(dst->u.simple.max[0]));
	    for (i = 0; i < dst->u.simple.rank; i++) {
		dst->u.simple.max[i] = src->u.simple.max[i];
	    }
	}
	if (dst->u.simple.perm) {
	    dst->u.simple.perm = H5MM_xmalloc(dst->u.simple.rank *
					      sizeof(dst->u.simple.perm[0]));
	    for (i = 0; i < dst->u.simple.rank; i++) {
		dst->u.simple.perm[i] = src->u.simple.perm[i];
	    }
	}
	break;

    case H5S_COMPLEX:
	/*void */
	break;

    default:
	assert("unknown data space type" && 0);
	break;
    }

    FUNC_LEAVE(dst);
}

/*-------------------------------------------------------------------------
 * Function:	H5Sget_npoints
 *
 * Purpose:	Determines how many data points a data set has.
 *
 * Return:	Success:	Number of data points in the data set.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5Sget_npoints(hid_t space_id)
{
    H5S_t		   *ds = NULL;
    hsize_t		    ret_value = 0;

    FUNC_ENTER(H5Sget_npoints, 0);

    /* Check args */
    if (H5_DATASPACE != H5I_group(space_id) ||
	NULL == (ds = H5I_object(space_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a data space");
    }
    ret_value = H5S_get_npoints(ds);

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_get_npoints
 *
 * Purpose:	Determines how many data points a data set has.
 *
 * Return:	Success:	Number of data points in the data set.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5S_get_npoints(const H5S_t *ds)
{
    hsize_t		    ret_value = 0;
    intn		    i;

    FUNC_ENTER(H5S_get_npoints, 0);

    /* check args */
    assert(ds);

    switch (ds->type) {
    case H5S_SCALAR:
	ret_value = 1;
	break;

    case H5S_SIMPLE:
	/*
	 * Count the elements selected by the hypeslab if there is one,
	 * otherwise count all the elements.
	 */
	if (ds->hslab_def) {
	    for (ret_value=1, i=0; i<ds->u.simple.rank; i++) {
		ret_value *= ds->h.count[i];
	    }
	} else {
	    for (ret_value=1, i=0; i<ds->u.simple.rank; i++) {
		ret_value *= ds->u.simple.size[i];
	    }
	}
	break;

    case H5S_COMPLEX:
	HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, 0,
		      "complex data spaces are not supported yet");

    default:
	assert("unknown data space class" && 0);
	HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, 0,
		      "internal error (unknown data space class)");
    }

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_get_npoints_max
 *
 * Purpose:	Determines the maximum number of data points a data space may
 *		have.  If the `max' array is null then the maximum number of
 *		data points is the same as the current number of data points
 *		without regard to the hyperslab.  If any element of the `max'
 *		array is zero then the maximum possible size is returned.
 *
 * Return:	Success:	Maximum number of data points the data space
 *				may have.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5S_get_npoints_max(const H5S_t *ds)
{
    hsize_t		    ret_value = 0;
    intn		    i;

    FUNC_ENTER(H5S_get_npoints_max, 0);

    /* check args */
    assert(ds);

    switch (ds->type) {
    case H5S_SCALAR:
	ret_value = 1;
	break;

    case H5S_SIMPLE:
	if (ds->u.simple.max) {
	    for (ret_value=1, i=0; i<ds->u.simple.rank; i++) {
		if (H5S_UNLIMITED==ds->u.simple.max[i]) {
		    ret_value = MAX_HSIZET;
		    break;
		} else {
		    ret_value *= ds->u.simple.max[i];
		}
	    }
	} else {
	    for (ret_value=1, i=0; i<ds->u.simple.rank; i++) {
		ret_value *= ds->u.simple.size[i];
	    }
	}
	break;

    case H5S_COMPLEX:
	HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, 0,
		      "complex data spaces are not supported yet");

    default:
	assert("unknown data space class" && 0);
	HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, 0,
		      "internal error (unknown data space class)");
    }

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Sget_ndims
 *
 * Purpose:	Determines the dimensionality of a data space.
 *
 * Return:	Success:	The number of dimensions in a data space.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Sget_ndims(hid_t space_id)
{
    H5S_t		   *ds = NULL;
    intn		   ret_value = 0;

    FUNC_ENTER(H5Sget_ndims, FAIL);

    /* Check args */
    if (H5_DATASPACE != H5I_group(space_id) ||
	NULL == (ds = H5I_object(space_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    ret_value = H5S_get_ndims(ds);

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_get_ndims
 *
 * Purpose:	Returns the number of dimensions in a data space.
 *
 * Return:	Success:	Non-negative number of dimensions.  Zero
 *				implies a scalar.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5S_get_ndims(const H5S_t *ds)
{
    intn		    ret_value = FAIL;

    FUNC_ENTER(H5S_get_ndims, FAIL);

    /* check args */
    assert(ds);

    switch (ds->type) {
    case H5S_SCALAR:
	ret_value = 0;
	break;

    case H5S_SIMPLE:
	ret_value = ds->u.simple.rank;
	break;

    case H5S_COMPLEX:
	HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
		      "complex data spaces are not supported yet");

    default:
	assert("unknown data space class" && 0);
	HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
		      "internal error (unknown data space class)");
    }

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Sget_dims
 *
 * Purpose:	Returns the size in each dimension of a data space DS through
 *		the DIMS argument.
 *
 * Return:	Success:	Number of dimensions, the same value as
 *				returned by H5Sget_ndims().
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Sget_dims(hid_t space_id, hsize_t dims[]/*out*/)
{
    H5S_t		   *ds = NULL;
    intn		   ret_value = 0;

    FUNC_ENTER(H5Sget_dims, FAIL);

    /* Check args */
    if (H5_DATASPACE != H5I_group(space_id) ||
	NULL == (ds = H5I_object(space_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if (!dims) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");
    }
    ret_value = H5S_get_dims(ds, dims, NULL);

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_get_dims
 *
 * Purpose:	Returns the size in each dimension of a data space.  This
 *		function may not be meaningful for all types of data spaces.
 *
 * Return:	Success:	Number of dimensions.  Zero implies scalar.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5S_get_dims(const H5S_t *ds, hsize_t dims[], hsize_t max_dims[])
{
    intn	ret_value = FAIL;
    intn	i;

    FUNC_ENTER(H5S_get_dims, FAIL);

    /* check args */
    assert(ds);
    assert(dims);

    switch (ds->type) {
    case H5S_SCALAR:
	ret_value = 0;
	break;

    case H5S_SIMPLE:
	ret_value = ds->u.simple.rank;
	for (i=0; i<ret_value; i++) {
	    if (dims) dims[i] = ds->u.simple.size[i];
	    if (max_dims) {
		if (ds->u.simple.max) {
		    max_dims[i] = ds->u.simple.max[i];
		} else {
		    max_dims[i] = ds->u.simple.size[i];
		}
	    }
	}
	break;

    case H5S_COMPLEX:
	HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
		      "complex data spaces are not supported yet");

    default:
	assert("unknown data space class" && 0);
	HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
		      "internal error (unknown data space class)");
    }

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_modify
 *
 * Purpose:	Updates a data space by writing a message to an object
 *		header.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_modify(H5G_entry_t *ent, const H5S_t *ds)
{
    FUNC_ENTER(H5S_modify, FAIL);

    assert(ent);
    assert(ds);

    switch (ds->type) {
    case H5S_SCALAR:
	HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
		      "scalar data spaces are not implemented yet");

    case H5S_SIMPLE:
	if (H5O_modify(ent, H5O_SDSPACE, 0, 0, &(ds->u.simple)) < 0) {
	    HRETURN_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL,
			  "can't update simple data space message");
	}
	break;

    case H5S_COMPLEX:
	HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
		      "complex data spaces are not implemented yet");

    default:
	assert("unknown data space class" && 0);
	break;
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_read
 *
 * Purpose:	Reads the data space from an object header.
 *
 * Return:	Success:	Pointer to a new data space.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5S_t *
H5S_read(H5F_t __unused__ *f, H5G_entry_t *ent)
{
    H5S_t		   *ds = NULL;

    FUNC_ENTER(H5S_read, NULL);

    /* check args */
    assert(f);
    assert(ent);

    ds = H5MM_xcalloc(1, sizeof(H5S_t));

    if (H5O_read(ent, H5O_SDSPACE, 0, &(ds->u.simple))) {
	ds->type = H5S_SIMPLE;

    } else {
	ds->type = H5S_SCALAR;
    }

    FUNC_LEAVE(ds);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_cmp
 *
 * Purpose:	Compares two data spaces.
 *
 * Return:	Success:	0 if DS1 and DS2 are the same.
 *				<0 if DS1 is less than DS2.
 *				>0 if DS1 is greater than DS2.
 *
 *		Failure:	0, never fails
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5S_cmp(const H5S_t *ds1, const H5S_t *ds2)
{
    intn		    i;

    FUNC_ENTER(H5S_cmp, 0);

    /* check args */
    assert(ds1);
    assert(ds2);

    /* compare */
    if (ds1->type < ds2->type)
	HRETURN(-1);
    if (ds1->type > ds2->type)
	HRETURN(1);

    switch (ds1->type) {
    case H5S_SIMPLE:
	if (ds1->u.simple.rank < ds2->u.simple.rank)
	    HRETURN(-1);
	if (ds1->u.simple.rank > ds2->u.simple.rank)
	    HRETURN(1);

	for (i = 0; i < ds1->u.simple.rank; i++) {
	    if (ds1->u.simple.size[i] < ds2->u.simple.size[i])
		HRETURN(-1);
	    if (ds1->u.simple.size[i] > ds2->u.simple.size[i])
		HRETURN(1);
	}

	/* don't compare max dimensions */

	for (i = 0; i < ds1->u.simple.rank; i++) {
	    if ((ds1->u.simple.perm ? ds1->u.simple.perm[i] : i) <
		(ds2->u.simple.perm ? ds2->u.simple.perm[i] : i))
		HRETURN(-1);
	    if ((ds1->u.simple.perm ? ds2->u.simple.perm[i] : i) >
		(ds2->u.simple.perm ? ds2->u.simple.perm[i] : i))
		HRETURN(1);
	}

	/* Check if we should compare hyperslab definitions */
	if(ds1->hslab_def==TRUE && ds2->hslab_def==TRUE) {
	    for (i = 0; i < ds1->u.simple.rank; i++) {
		if (ds1->h.start[i] < ds2->h.start[i])
		    HRETURN(-1);
		if (ds1->h.start[i] > ds2->h.start[i])
		    HRETURN(1);
		if (ds1->h.count[i] < ds2->h.count[i])
		    HRETURN(-1);
		if (ds1->h.count[i] > ds2->h.count[i])
		    HRETURN(1);
		if (ds1->h.stride[i] < ds2->h.stride[i])
		    HRETURN(-1);
		if (ds1->h.stride[i] > ds2->h.stride[i])
		    HRETURN(1);
	    }
	} else {
	    if(ds1->hslab_def!=ds2->hslab_def)
		HRETURN(ds1->hslab_def==TRUE ? 1 : -1);
	}

	break;

    default:
	assert("not implemented yet" && 0);
    }

    FUNC_LEAVE(0);
}


/*--------------------------------------------------------------------------
 NAME
    H5S_is_simple
 PURPOSE
    Check if a dataspace is simple (internal)
 USAGE
    hbool_t H5S_is_simple(sdim)
	H5S_t *sdim;		IN: Pointer to dataspace object to query
 RETURNS
    TRUE/FALSE/FAIL
 DESCRIPTION
	This function determines the if a dataspace is "simple". ie. if it
    has orthogonal, evenly spaced dimensions.
--------------------------------------------------------------------------*/
hbool_t
H5S_is_simple(const H5S_t *sdim)
{
    hbool_t		    ret_value = FAIL;

    FUNC_ENTER(H5S_is_simple, FAIL);

    /* Check args and all the boring stuff. */
    assert(sdim);
    ret_value = sdim->type == H5S_SIMPLE ? TRUE : FALSE;      /* Currently all dataspaces are simple, but check anyway */

    FUNC_LEAVE(ret_value);
}


/*--------------------------------------------------------------------------
 NAME
    H5Sis_simple
 PURPOSE
    Check if a dataspace is simple
 USAGE
    hbool_t H5Sis_simple(sid)
	hid_t sid;	      IN: ID of dataspace object to query
 RETURNS
    TRUE/FALSE/FAIL
 DESCRIPTION
	This function determines the if a dataspace is "simple". ie. if it
    has orthogonal, evenly spaced dimensions.
--------------------------------------------------------------------------*/
hbool_t 
H5Sis_simple(hid_t sid)
{
    H5S_t		   *space = NULL;	/* dataspace to modify */
    hbool_t		    ret_value = FAIL;

    FUNC_ENTER(H5Sis_simple, FAIL);

    /* Check args and all the boring stuff. */
    if ((space = H5I_object(sid)) == NULL)
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "not a data space");

    ret_value = H5S_is_simple(space);

  done:
    if (ret_value == FAIL) {   /* Error condition cleanup */

    }				/* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}


/*--------------------------------------------------------------------------
 NAME
    H5Sset_space
 PURPOSE
    Determine the size of a dataspace
 USAGE
    herr_t H5Sset_space(sid, rank, dims)
	hid_t sid;	      IN: Dataspace object to query
	intn rank;	      IN: # of dimensions for the dataspace
	const size_t *dims;   IN: Size of each dimension for the dataspace
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
	This function sets the number and size of each dimension in the
    dataspace.	Setting RANK to a value of zero allows scalar objects to be
    created.  Dimensions are specified from slowest to fastest changing in the
    DIMS array (i.e. 'C' order).  Setting the size of a dimension to zero
    indicates that the dimension is of unlimited size and should be allowed to
    expand.  Currently, only the first dimension in the array (the slowest) may
    be unlimited in size.
--------------------------------------------------------------------------*/
herr_t
H5Sset_space(hid_t sid, int rank, const hsize_t *dims)
{
    H5S_t		   *space = NULL;	/* dataspace to modify */
    intn		    u;	/* local counting variable */
    herr_t		    ret_value = SUCCEED;

    FUNC_ENTER(H5Sset_space, FAIL);

    /* Check args */
    if ((space = H5I_object(sid)) == NULL) {
	HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "not a data space");
    }
    if (rank > 0 && dims == NULL) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no dimensions specified");
    }
    if (rank<0) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid rank");
    }
    if (dims) {
	for (u=0; u<rank; u++) {
	    if (dims[u]<=0) {
		HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			       "invalid dimension size");
	    }
	}
    }

    /* shift out of the previous state to a "simple" dataspace */
    switch (space->type) {
    case H5S_SCALAR:
    case H5S_SIMPLE:
	/* do nothing */
	break;

    case H5S_COMPLEX:
	/*
	 * eventually this will destroy whatever "complex" dataspace info
	 * is retained, right now it's an error
	 */
	/* Fall through to report error */

    default:
	HRETURN_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL,
		      "unknown data space class");
    }
    space->type = H5S_SIMPLE;

    /* Reset hyperslab definition, if one is defined */
    if(space->hslab_def==TRUE) {
	H5MM_xfree(space->h.start);
	H5MM_xfree(space->h.count);
	H5MM_xfree(space->h.stride);
	space->hslab_def=FALSE;
    }

    if (rank == 0) {		/* scalar variable */
	space->type = H5S_SCALAR;
	space->u.simple.rank = 0;	/* set to scalar rank */
	if (space->u.simple.size != NULL)
	    space->u.simple.size = H5MM_xfree(space->u.simple.size);
	if (space->u.simple.max != NULL)
	    space->u.simple.max = H5MM_xfree(space->u.simple.max);
	if (space->u.simple.perm != NULL)
	    space->u.simple.max = H5MM_xfree(space->u.simple.perm);
    } else {
	/* Free the old space for now */
	if (space->u.simple.size != NULL)
	    space->u.simple.size = H5MM_xfree(space->u.simple.size);
	if (space->u.simple.max != NULL)
	    space->u.simple.max = H5MM_xfree(space->u.simple.max);
	if (space->u.simple.perm != NULL)
	    space->u.simple.perm = H5MM_xfree(space->u.simple.perm);

	/* Set the rank and copy the dims */
	space->u.simple.rank = rank;
	space->u.simple.size = H5MM_xcalloc(rank, sizeof(hsize_t));
	HDmemcpy(space->u.simple.size, dims, sizeof(hsize_t) * rank);

    }
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5Sset_hyperslab
 PURPOSE
    Select a hyperslab from a simple dataspace
 USAGE
    herr_t H5Sset_hyperslab(sid, start, count, stride)
	hid_t sid;	      IN: Dataspace object to select hyperslab from
	const int *start;  IN: Starting location for hyperslab to select
	const size_t *count;  IN: Number of elements in hyperslab
	const size_t *stride; IN: Packing of elements in hyperslab
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
	This function selects a hyperslab from a simple dataspace.  The stride
    array may be used to sub-sample the hyperslab chosen, a value of 1 in each
    position of the stride array selects contiguous elements in the array,
    a value of 2 selects every other element, etc.  If the stride parameter is
    set to NULL, a contiguous hyperslab is chosen.  The values in the start and
    count arrays may be negative, to allow for selecting hyperslabs in chunked
    datasets which extend in arbitrary directions.
--------------------------------------------------------------------------*/
herr_t
H5Sset_hyperslab(hid_t sid, const hssize_t *start, const hsize_t *count,
		 const hsize_t *stride)
{
    H5S_t	*space = NULL;		/* dataspace to modify */
    hsize_t	*tmp_stride=NULL;	/* temp. copy of stride */
    intn	u;			/* local counting variable */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER(H5Sset_hyperslab, FAIL);

    /* Get the object */
    if (H5_DATASPACE != H5I_group(sid) || (space = H5I_object(sid)) == NULL) {
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "not a data space");
    }
    if (start == NULL || count==NULL) {
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		    "invalid hyperslab selected");
    }

    /* We can't modify other types of dataspaces currently, so error out */
    if (space->type!=H5S_SIMPLE) {
	HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL,
		    "unknown dataspace type");
    }

    /* Set up stride values for later use */
    tmp_stride= H5MM_xmalloc(space->u.simple.rank*sizeof(tmp_stride[0]));
    for (u=0; u<space->u.simple.rank; u++) {
	tmp_stride[u] = stride ? stride[u] : 1;
    }

    /* Range check arguments */
    for (u=0; u<space->u.simple.rank; u++) {
	if (start[u]<0 || (hsize_t)(start[u])>=space->u.simple.size[u]) {
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL,
			"hyperslab bounds out of range");
	}
	if (start[u]<0 ||
	    start[u]+(count[u]*tmp_stride[u])>space->u.simple.size[u]) {
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL,
			"hyperslab bounds out of range");
	}
    }

    /* Allocate space for the hyperslab information */
    if (NULL==space->h.start) {
	space->h.start= H5MM_xcalloc(space->u.simple.rank, sizeof(hsize_t));
	space->h.count= H5MM_xcalloc(space->u.simple.rank, sizeof(hsize_t));
	space->h.stride= H5MM_xcalloc(space->u.simple.rank, sizeof(hsize_t));
    }

    /* Build hyperslab */
    for(u=0; u<space->u.simple.rank; u++) {
	space->h.start[u] = start[u];
	space->h.count[u] = count[u];
	space->h.stride[u] = tmp_stride[u];
    }
    space->hslab_def=TRUE;

done:
    if (ret_value == FAIL) {	/* Error condition cleanup */

    }				/* end if */

    /* Normal function cleanup */
    H5MM_xfree(tmp_stride);
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Sget_hyperslab
 *
 * Purpose:	Retrieves information about the hyperslab from a simple data
 *		space.	If no hyperslab has been defined then the hyperslab
 *		is the same as the entire array.
 *
 * Return:	Success:	Hyperslab dimensionality.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Sget_hyperslab (hid_t sid, hssize_t offset[]/*out*/, hsize_t size[]/*out*/,
		  hsize_t stride[]/*out*/)
{
    const H5S_t	*ds = NULL;
    intn	ret_value = FAIL;
    
    FUNC_ENTER (H5Sget_hyperslab, FAIL);

    /* Check args */
    if (H5_DATASPACE!=H5I_group (sid) || NULL==(ds=H5I_object (sid))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }

    /* Get hyperslab info */
    if ((ret_value=H5S_get_hyperslab (ds, offset, size, stride))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab information");
    }

    FUNC_LEAVE (ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_get_hyperslab
 *
 * Purpose:	Retrieves information about the hyperslab from a simple data
 *		space.	If no hyperslab has been defined then the hyperslab
 *		is the same as the entire array.
 *
 * Return:	Success:	Hyperslab dimensionality.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5S_get_hyperslab (const H5S_t *ds, hssize_t offset[]/*out*/,
		   hsize_t size[]/*out*/, hsize_t stride[]/*out*/)
{
    intn	i;
    intn	ret_value = FAIL;
    
    FUNC_ENTER (H5S_get_hyperslab, FAIL);

    /* Check args */
    assert (ds);
    switch (ds->type) {
    case H5S_SCALAR:
	break;

    case H5S_SIMPLE:
	if (ds->hslab_def) {
	    for (i=0; i<ds->u.simple.rank; i++) {
		if (offset) offset[i] = ds->h.start[i];
		if (size) size[i] = ds->h.count[i];
		if (stride) stride[i] = ds->h.stride[i];
	    }
	} else {
	    for (i=0; i<ds->u.simple.rank; i++) {
		if (offset) offset[i] = 0;
		if (size) size[i] = ds->u.simple.size[i];
		if (stride) stride[i] = 1;
	    }
	}
	ret_value = ds->u.simple.rank;
	break;

    case H5S_COMPLEX:	/*fall through*/
    default:
	HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
		       "hyperslabs not supported for this type of space");
    }

    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_find
 *
 * Purpose:	Given two data spaces (MEM_SPACE and FILE_SPACE) this
 *		function locates the data space conversion functions and
 *		initializes CONV to point to them.  The CONV contains
 *		function pointers for converting in either direction.
 *
 * Return:	Success:	Pointer to a data space conversion callback
 *				list.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
const H5S_conv_t *
H5S_find (const H5S_t *mem_space, const H5S_t *file_space)
{
    static H5S_conv_t		_conv;
    static const H5S_conv_t	*conv = NULL;
    
    FUNC_ENTER (H5S_find, NULL);

    /* Check args */
    assert (mem_space && H5S_SIMPLE==mem_space->type);
    assert (file_space && H5S_SIMPLE==file_space->type);

    /*
     * We can't do conversion if the source and destination select a
     * different number of data points.
     */
    if (H5S_get_npoints (mem_space) != H5S_get_npoints (file_space)) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_BADRANGE, NULL,
		       "memory and file data spaces are different sizes");
    }

    /*
     * Initialize pointers.  This will eventually be a table lookup based
     * on the source and destination data spaces, similar to H5T_find(), but
     * for now we only support simple data spaces.
     */
    if (!conv) {
	_conv.init = H5S_simp_init;
	_conv.fgath = H5S_simp_fgath;
	_conv.mscat = H5S_simp_mscat;
	_conv.mgath = H5S_simp_mgath;
	_conv.fscat = H5S_simp_fscat;
	_conv.read = H5S_simp_read;
	_conv.write = H5S_simp_write;
	conv = &_conv;
    }
    
    FUNC_LEAVE (conv);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_extend
 *
 * Purpose:	Extend the dimensions of a data space.
 *
 * Return:	Success:	Number of dimensions whose size increased.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Friday, January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5S_extend (H5S_t *space, const hsize_t *size)
{
    intn		i, ret_value=0;
    
    FUNC_ENTER (H5S_extend, FAIL);

    /* Check args */
    assert (space && H5S_SIMPLE==space->type);
    assert (size);

    for (i=0; i<space->u.simple.rank; i++) {
	if (space->u.simple.size[i]<size[i]) {
	    if (space->u.simple.max &&
		H5S_UNLIMITED!=space->u.simple.max[i] &&
		space->u.simple.max[i]<size[i]) {
		HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			       "dimension cannot be increased");
	    }
	    ret_value++;
	}
    }

    /* Update */
    if (ret_value) {
	for (i=0; i<space->u.simple.rank; i++) {
	    if (space->u.simple.size[i]<size[i]) {
		space->u.simple.size[i] = size[i];
	    }
	}
    }

    FUNC_LEAVE (ret_value);
}
		
