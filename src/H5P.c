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
static char             RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#include <H5private.h>          /* Generic Functions                      */
#include <H5Aprivate.h>         /* Atom Functions                 */
#include <H5Eprivate.h>         /* Error handling                 */
#include <H5MMprivate.h>        /* Memory Management functions            */
#include <H5Oprivate.h>         /*object headers                  */
#include <H5Pprivate.h>         /* Data-space functions                   */


/* Interface initialization */
#define PABLO_MASK      H5P_mask
#define INTERFACE_INIT  H5P_init_interface
static intn             interface_initialize_g = FALSE;
static herr_t           H5P_init_interface(void);
static void             H5P_term_interface(void);


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
H5P_init_interface(void)
{
    herr_t                  ret_value = SUCCEED;
    FUNC_ENTER(H5P_init_interface, FAIL);

    /* Initialize the atom group for the file IDs */
    if ((ret_value = H5A_init_group(H5_DATASPACE, H5A_DATASPACEID_HASHSIZE,
				    H5P_RESERVED_ATOMS,
				    (herr_t (*)(void *)) H5P_close)) != FAIL) {
        ret_value = H5_add_exit(&H5P_term_interface);
    }
    FUNC_LEAVE(ret_value);
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
H5P_term_interface(void)
{
    H5A_destroy_group(H5_DATASPACE);
}

/*-------------------------------------------------------------------------
 * Function:    H5Pcreate_simple
 *
 * Purpose:     Creates a new simple data space object and opens it for access.
 *
 * Return:      Success:        The ID for the new simple data space object.
 *
 *              Failure:        FAIL
 *
 * Errors:
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, January  27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Pcreate_simple(intn rank, size_t dims[])
{
    H5P_t                  *ds = NULL;
    hid_t                   ret_value = FAIL;

    FUNC_ENTER(H5Pcreate, FAIL);
    H5ECLEAR;

    ds = H5MM_xcalloc(1, sizeof(H5P_t));
    ds->type = H5P_SIMPLE;
    ds->hslab_def=FALSE;    /* no hyperslab defined currently */

    /* Initialize rank and dimensions */
    ds->u.simple.rank = rank;
    ds->u.simple.dim_flags = 0; /* max & perm information is not valid/useful */
    ds->u.simple.size = H5MM_xcalloc(1, rank*sizeof(size_t));
    HDmemcpy(ds->u.simple.size, dims, rank*sizeof(size_t));
    ds->u.simple.max = H5MM_xcalloc(1, rank*sizeof(size_t));
    ds->u.simple.perm = H5MM_xcalloc(1, rank*sizeof(intn));

    /* Register the new data space and get an ID for it */
    if ((ret_value = H5A_register(H5_DATASPACE, ds)) < 0) {
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
                    "unable to register data space for ID");
    }
  done:
    if (ret_value < 0) {
        H5MM_xfree(ds);
    }
    FUNC_LEAVE(ret_value);
}

#ifdef OLD_WAY
/*-------------------------------------------------------------------------
 * Function:    H5Pcreate
 *
 * Purpose:     Creates a new data space object and opens it for access.
 *
 * Return:      Success:        The ID for the new data space object.
 *
 *              Failure:        FAIL
 *
 * Errors:
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Pcreate(H5P_class_t type)
{
    H5P_t                  *ds = NULL;
    hid_t                   ret_value = FAIL;

    FUNC_ENTER(H5Pcreate, FAIL);
    H5ECLEAR;

    ds = H5MM_xcalloc(1, sizeof(H5P_t));
    ds->type = type;

    switch (type) {
    case H5P_SCALAR:
        /*void */
        break;

    case H5P_SIMPLE:
        ds->u.simple.rank = 0;
        break;

    case H5P_COMPLEX:
        HGOTO_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
                    "complex types are not supported yet");

    default:
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                    "unknown data space type");
    }

    /* Register the new data space and get an ID for it */
    if ((ret_value = H5A_register(H5_DATASPACE, ds)) < 0) {
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
                    "unable to register data space for ID");
    }
  done:
    if (ret_value < 0) {
        H5MM_xfree(ds);
    }
    FUNC_LEAVE(ret_value);
}
#endif /* OLD_WAY */

/*-------------------------------------------------------------------------
 * Function:    H5Pclose
 *
 * Purpose:     Release access to a data space object.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Errors:
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pclose(hid_t space_id)
{
    FUNC_ENTER(H5Pclose, FAIL);
    H5ECLEAR;

    /* check args */
    if (H5_DATASPACE != H5A_group(space_id) ||
        NULL == H5A_object(space_id)) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    /* When the reference count reaches zero the resources are freed */
    if (H5A_dec_ref(space_id) < 0) {
        HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "problem freeing id");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5P_close
 *
 * Purpose:     Releases all memory associated with a data space.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_close(H5P_t *ds)
{
    FUNC_ENTER(H5P_close, FAIL);

    assert(ds);

    switch (ds->type) {
    case H5P_SCALAR:
        /*void */
        break;

    case H5P_SIMPLE:
        H5MM_xfree(ds->u.simple.size);
        H5MM_xfree(ds->u.simple.max);
        H5MM_xfree(ds->u.simple.perm);
        break;

    case H5P_COMPLEX:
        /* nothing */
        break;

    default:
        assert("unknown data space type" && 0);
        break;
    }
    if(ds->hslab_def==TRUE)
      {
        H5MM_xfree(ds->h.start);
        H5MM_xfree(ds->h.count);
        H5MM_xfree(ds->h.stride);
      } /* end if */
    H5MM_xfree(ds);

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5P_copy
 *
 * Purpose:     Copies a data space.
 *
 * Return:      Success:        A pointer to a new copy of SRC
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5P_t                  *
H5P_copy(const H5P_t *src)
{
    H5P_t                  *dst = NULL;
    int                     i;

    FUNC_ENTER(H5P_copy, NULL);

    dst = H5MM_xmalloc(sizeof(H5P_t));
    *dst = *src;

    switch (dst->type) {
    case H5P_SCALAR:
        /*void */
        break;

    case H5P_SIMPLE:
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

    case H5P_COMPLEX:
        /*void */
        break;

    default:
        assert("unknown data space type" && 0);
        break;
    }

    FUNC_LEAVE(dst);
}

/*-------------------------------------------------------------------------
 * Function:    H5Pget_npoints
 *
 * Purpose:     Determines how many data points a data set has.
 *
 * Return:      Success:        Number of data points in the data set.
 *
 *              Failure:        0
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Pget_npoints(hid_t space_id)
{
    H5P_t                  *ds = NULL;
    size_t                  ret_value = 0;

    FUNC_ENTER(H5Pget_npoints, 0);
    H5ECLEAR;

    /* check args */
    if (H5_DATASPACE != H5A_group(space_id) ||
        NULL == (ds = H5A_object(space_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a data space");
    }
    ret_value = H5P_get_npoints(ds);

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5P_get_npoints
 *
 * Purpose:     Determines how many data points a data set has.
 *
 * Return:      Success:        Number of data points in the data set.
 *
 *              Failure:        0
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5P_get_npoints(const H5P_t *ds)
{
    size_t                  ret_value = 0;
    intn                    i;

    FUNC_ENTER(H5P_get_npoints, 0);

    /* check args */
    assert(ds);

    switch (ds->type) {
    case H5P_SCALAR:
        ret_value = 1;
        break;

    case H5P_SIMPLE:
        for (ret_value = 1, i = 0; i < ds->u.simple.rank; i++) {
            ret_value *= ds->u.simple.size[i];
        }
        break;

    case H5P_COMPLEX:
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
 * Function:    H5Pget_ndims
 *
 * Purpose:     Determines the dimensionality of a data space.
 *
 * Return:      Success:        The number of dimensions in a data space.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5Pget_ndims(hid_t space_id)
{
    H5P_t                  *ds = NULL;
    size_t                  ret_value = 0;

    FUNC_ENTER(H5Pget_ndims, FAIL);
    H5ECLEAR;

    /* check args */
    if (H5_DATASPACE != H5A_group(space_id) ||
        NULL == (ds = H5A_object(space_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    ret_value = H5P_get_ndims(ds);

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5P_get_ndims
 *
 * Purpose:     Returns the number of dimensions in a data space.
 *
 * Return:      Success:        Non-negative number of dimensions.  Zero
 *                              implies a scalar.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5P_get_ndims(const H5P_t *ds)
{
    intn                    ret_value = FAIL;

    FUNC_ENTER(H5P_get_ndims, FAIL);

    /* check args */
    assert(ds);

    switch (ds->type) {
    case H5P_SCALAR:
        ret_value = 0;
        break;

    case H5P_SIMPLE:
        ret_value = ds->u.simple.rank;
        break;

    case H5P_COMPLEX:
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
 * Function:    H5Pget_dims
 *
 * Purpose:     Returns the size in each dimension of a data space DS through
 *              the DIMS argument.
 *
 * Return:      Success:        Number of dimensions, the same value as
 *                              returned by H5Pget_ndims().
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5Pget_dims(hid_t space_id, size_t dims[] /*out */ )
{

    H5P_t                  *ds = NULL;
    size_t                  ret_value = 0;

    FUNC_ENTER(H5Pget_dims, FAIL);
    H5ECLEAR;

    /* check args */
    if (H5_DATASPACE != H5A_group(space_id) ||
        NULL == (ds = H5A_object(space_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if (!dims) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");
    }
    ret_value = H5P_get_dims(ds, dims);

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5P_get_dims
 *
 * Purpose:     Returns the size in each dimension of a data space.  This
 *              function may not be meaningful for all types of data spaces.
 *
 * Return:      Success:        Number of dimensions.  Zero implies scalar.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5P_get_dims(const H5P_t *ds, size_t dims[])
{
    intn                    ret_value = FAIL;
    intn                    i;

    FUNC_ENTER(H5P_get_dims, FAIL);

    /* check args */
    assert(ds);
    assert(dims);

    switch (ds->type) {
    case H5P_SCALAR:
        ret_value = 0;
        break;

    case H5P_SIMPLE:
        ret_value = ds->u.simple.rank;
        for (i = 0; i < ret_value; i++) {
            dims[i] = ds->u.simple.size[i];
        }
        break;

    case H5P_COMPLEX:
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
 * Function:    H5P_modify
 *
 * Purpose:     Updates a data space by writing a message to an object
 *              header.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_modify(H5F_t *f, H5G_entry_t *ent, const H5P_t *ds)
{
    FUNC_ENTER(H5O_modify, FAIL);

    assert(f);
    assert(ent);
    assert(ds);

    switch (ds->type) {
    case H5P_SCALAR:
        HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
                      "scalar data spaces are not implemented yet");

    case H5P_SIMPLE:
        if (H5O_modify(ent, H5O_SDSPACE, 0, 0, &(ds->u.simple)) < 0) {
            HRETURN_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL,
                          "can't update simple data space message");
        }
        break;

    case H5P_COMPLEX:
        HRETURN_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
                      "complex data spaces are not implemented yet");

    default:
        assert("unknown data space class" && 0);
        break;
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5P_read
 *
 * Purpose:     Reads the data space from an object header.
 *
 * Return:      Success:        Pointer to a new data space.
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5P_t                  *
H5P_read(H5F_t *f, H5G_entry_t *ent)
{
    H5P_t                  *ds = NULL;

    FUNC_ENTER(H5P_read, NULL);

    /* check args */
    assert(f);
    assert(ent);

    ds = H5MM_xcalloc(1, sizeof(H5P_t));

    if (H5O_read(ent, H5O_SDSPACE, 0, &(ds->u.simple))) {
        ds->type = H5P_SIMPLE;

    } else {
        ds->type = H5P_SCALAR;
    }

    FUNC_LEAVE(ds);
}

/*-------------------------------------------------------------------------
 * Function:    H5P_cmp
 *
 * Purpose:     Compares two data spaces.
 *
 * Return:      Success:        0 if DS1 and DS2 are the same.
 *                              <0 if DS1 is less than DS2.
 *                              >0 if DS1 is greater than DS2.
 *
 *              Failure:        0, never fails
 *
 * Programmer:  Robb Matzke
 *              Wednesday, December 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5P_cmp(const H5P_t *ds1, const H5P_t *ds2)
{
    intn                    i;

    FUNC_ENTER(H5P_cmp, 0);

    /* check args */
    assert(ds1);
    assert(ds2);

    /* compare */
    if (ds1->type < ds2->type)
        HRETURN(-1);
    if (ds1->type > ds2->type)
        HRETURN(1);

    switch (ds1->type) {
    case H5P_SIMPLE:
        if (ds1->u.simple.rank < ds2->u.simple.rank)
            HRETURN(-1);
        if (ds1->u.simple.rank > ds2->u.simple.rank)
            HRETURN(1);

        /* don't compare flags */

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

        break;

    default:
        assert("not implemented yet" && 0);
    }

    FUNC_LEAVE(0);
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
H5P_is_simple(const H5P_t *sdim)
{
    hbool_t                 ret_value = BFAIL;

    FUNC_ENTER(H5P_is_simple, UFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    assert(sdim);
    ret_value = sdim->type == H5P_SIMPLE ? BTRUE : BFALSE;      /* Currently all dataspaces are simple, but check anyway */

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
hbool_t 
H5Pis_simple(hid_t sid)
{
    H5P_t                  *space = NULL;       /* dataspace to modify */
    hbool_t                 ret_value = BFAIL;

    FUNC_ENTER(H5Pis_simple, BFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    if ((space = H5A_object(sid)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "not a data space");

    ret_value = H5P_is_simple(space);

  done:
    if (ret_value == BFAIL) {   /* Error condition cleanup */

    }                           /* end if */
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
        const size_t *dims;   IN: Size of each dimension for the dataspace
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
herr_t
H5Pset_space(hid_t sid, intn rank, const size_t *dims)
{
    H5P_t                  *space = NULL;       /* dataspace to modify */
    intn                    u;  /* local counting variable */
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5Pset_space, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Get the object */
    if ((space = H5A_object(sid)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "not a data space");
    if (rank > 0 && dims == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid rank");

    /* shift out of the previous state to a "simple" dataspace */
    switch (space->type) {
    case H5P_SCALAR:
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
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL,
                    "unknown data space class");
    }                           /* end switch */
    space->type = H5P_SIMPLE;

    if (rank == 0) {            /* scalar variable */
        space->type = H5P_SCALAR;
        space->u.simple.rank = 0;       /* set to scalar rank */
        space->u.simple.dim_flags = 0;  /* no maximum dimensions or dimension permutations */
        if (space->u.simple.size != NULL)
            space->u.simple.size = H5MM_xfree(space->u.simple.size);
        if (space->u.simple.max != NULL)
            space->u.simple.max = H5MM_xfree(space->u.simple.max);
        if (space->u.simple.perm != NULL)
            space->u.simple.max = H5MM_xfree(space->u.simple.perm);
    }
    /* end if */ 
    else {
        /* Reset the dataspace flags */
        space->u.simple.dim_flags = 0;

        /* Free the old space for now */
        if (space->u.simple.size != NULL)
            space->u.simple.size = H5MM_xfree(space->u.simple.size);
        if (space->u.simple.max != NULL)
            space->u.simple.max = H5MM_xfree(space->u.simple.max);
        if (space->u.simple.perm != NULL)
            space->u.simple.perm = H5MM_xfree(space->u.simple.perm);

        /* Set the rank and copy the dims */
        space->u.simple.rank = rank;
        space->u.simple.size = H5MM_xcalloc(rank, sizeof(size_t));
        HDmemcpy(space->u.simple.size, dims, sizeof(size_t) * rank);

        /* check if there are unlimited dimensions and create the maximum dims array */
        for (u = 0; u < rank; u++)
            if (dims[u] == 0) {
                if (u > 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
                                "unlimited dimensions not in the lowest "
                                "dimensionality");
                space->u.simple.max = H5MM_xcalloc(rank, sizeof(size_t));
                HDmemcpy(space->u.simple.max, dims, sizeof(size_t) * rank);
                space->u.simple.dim_flags |= H5P_VALID_MAX;
                break;
            }                   /* end if */
    }                           /* end else */

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5Pset_hyperslab
 PURPOSE
    Select a hyperslab from a simple dataspace
 USAGE
    herr_t H5Pset_hyperslab(sid, start, count, stride)
        hid_t sid;            IN: Dataspace object to select hyperslab from
        const size_t *start;  IN: Starting location for hyperslab to select
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
H5Pset_hyperslab(hid_t sid, const size_t *start, const size_t *count, const size_t *stride)
{
    H5P_t                  *space = NULL;       /* dataspace to modify */
    size_t                 *tmp_stride=NULL;    /* temp. copy of stride */
    intn                    u;  /* local counting variable */
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5Pset_hyperslab, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Get the object */
    if (H5_DATASPACE != H5A_group(sid) || (space = H5A_object(sid)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "not a data space");
    if (start == NULL || count==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid hyperslab selected");

    /* We can't modify other types of dataspaces currently, so error out */
    if (space->type!=H5P_SIMPLE)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL,"unknown dataspace type");

    /* Set up stride values for later use */
    tmp_stride= H5MM_xmalloc(space->u.simple.rank*sizeof(size_t));
    for (u=0; u<space->u.simple.rank; u++) {
	tmp_stride[u] = stride ? stride[u] : 1;
    }

    /* Allocate space for the hyperslab information */
    if (NULL==space->h.start) {
	space->h.start= H5MM_xcalloc(space->u.simple.rank,sizeof(size_t));
	space->h.count= H5MM_xcalloc(space->u.simple.rank,sizeof(size_t));
	space->h.stride= H5MM_xcalloc(space->u.simple.rank,sizeof(size_t));
    }

    /* Build hyperslab */
    for(u=0; u<space->u.simple.rank; u++)
      {
        /* Range checking arguments */
        if(start[u]+(count[u]*tmp_stride[u])<=0 || start[u]+(count[u]*tmp_stride[u])>=space->u.simple.size[u])
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL,"hyperslab bounds out of range");
        
        /* copy "normalized" (i.e. strictly increasing) values for hyperslab parameters */
        space->h.start[u]=MIN(start[u],start[u]+((ABS(count[u])-1)*tmp_stride[u]));
        space->h.count[u]=ABS(count[u]);
        space->h.stride[u]=ABS(tmp_stride[u]);
      } /* end for */
      space->hslab_def=TRUE;

done:
    if (ret_value == FAIL) {    /* Error condition cleanup */
        /* Free hyperslab arrays if we encounter an error */
	H5MM_xfree(space->h.start);
	H5MM_xfree(space->h.count);
	H5MM_xfree(space->h.stride);
	space->hslab_def = FALSE;
    }                           /* end if */

    /* Normal function cleanup */
    H5MM_xfree(tmp_stride);
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pget_hyperslab
 *
 * Purpose:	Retrieves information about the hyperslab from a simple data
 *		space.  If no hyperslab has been defined then the hyperslab
 *		is the same as the entire array.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_hyperslab (hid_t sid, size_t offset[]/*out*/, size_t size[]/*out*/,
		  size_t stride[]/*out*/)
{
    H5P_t	*ds = NULL;
    
    FUNC_ENTER (H5Pget_hyperslab, FAIL);
    H5ECLEAR;

    /* Check args */
    if (H5_DATASPACE!=H5A_group (sid) || NULL==(ds=H5A_object (sid))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }

    /* Get hyperslab info */
    if (H5P_get_hyperslab (ds, offset, size, stride)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab information");
    }

    FUNC_LEAVE (SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5P_get_hyperslab
 *
 * Purpose:	Retrieves information about the hyperslab from a simple data
 *		space.  If no hyperslab has been defined then the hyperslab
 *		is the same as the entire array.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_get_hyperslab (H5P_t *ds, size_t offset[]/*out*/, size_t size[]/*out*/,
		   size_t stride[]/*out*/)
{
    intn		i;
    
    FUNC_ENTER (H5P_get_hyperslab, FAIL);

    /* Check args */
    assert (ds);
    switch (ds->type) {
    case H5P_SCALAR:
	break;

    case H5P_SIMPLE:
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
	break;

    case H5P_COMPLEX:	/*fall through*/
    default:
	HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
		       "hyperslabs not supported for this type of space");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_find
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
 *              Wednesday, January 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
const H5P_conv_t *
H5P_find (const H5P_t *mem_space, const H5P_t *file_space)
{
    static H5P_conv_t		_conv;
    static const H5P_conv_t 	*conv = NULL;
    
    FUNC_ENTER (H5P_find, NULL);

    /* Check args */
    assert (mem_space && H5P_SIMPLE==mem_space->type);
    assert (file_space && H5P_SIMPLE==file_space->type);

    /*
     * We can't do conversion if the source and destination select a
     * different number of data points.
     */
    if (H5P_get_npoints (mem_space) != H5P_get_npoints (file_space)) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_BADRANGE, NULL,
		       "memory and file data spaces are different sizes");
    }

    /*
     * Initialize pointers.  This will eventually be a table lookup based
     * on the source and destination data spaces, similar to H5T_find(), but
     * for now we only support simple data spaces.
     */
    if (!conv) {
	_conv.init = H5P_simp_init;
	_conv.fgath = H5P_simp_fgath;
	_conv.mscat = H5P_simp_mscat;
	_conv.mgath = H5P_simp_mgath;
	_conv.fscat = H5P_simp_fscat;
	conv = &_conv;
    }
    
    FUNC_LEAVE (conv);
}
