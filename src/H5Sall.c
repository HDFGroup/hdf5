/*
 * Copyright (C) 1998-2001 NCSA
 *                         All rights reserved.
 *
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, June 16, 1998
 *
 * Purpose:	"All" selection data space I/O functions.
 */

#define H5S_PACKAGE		/*suppress error about including H5Spkg	  */

#include "H5private.h"		/* Generic Functions			  */
#include "H5Eprivate.h"		/* Error handling		  */
#include "H5Iprivate.h"		/* ID Functions		  */
#include "H5Spkg.h"		/* Dataspace functions			  */
#include "H5Vprivate.h"         /* Vector functions */

/* Interface initialization */
#define PABLO_MASK      H5Sall_mask
#define INTERFACE_INIT  NULL
static int             interface_initialize_g = 0;


/*-------------------------------------------------------------------------
 * Function:	H5S_all_init
 *
 * Purpose:	Initializes iteration information for all selection.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_all_init (const H5S_t *space, size_t UNUSED elmt_size, H5S_sel_iter_t *sel_iter)
{
    FUNC_ENTER_NOAPI(H5S_all_init, FAIL);

    /* Check args */
    assert (space && H5S_SEL_ALL==space->select.type);
    assert (sel_iter);

    /* Initialize the number of elements to iterate over */
    sel_iter->all.elmt_left=H5S_get_simple_extent_npoints(space);

    /* Start at the upper left location */
    sel_iter->all.offset=0;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_all_favail
 *
 * Purpose:	Figure out the optimal number of elements to transfer to/from
 *		the file.
 *
 * Return:	non-negative number of elements on success, zero on
 *		failure.
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5S_all_favail (const H5S_t * UNUSED space, const H5S_sel_iter_t *sel_iter, hsize_t max)
{
    FUNC_ENTER_NOAPI(H5S_all_favail, 0);

    /* Check args */
    assert (space && H5S_SEL_ALL==space->select.type);
    assert (sel_iter);

    FUNC_LEAVE (MIN(sel_iter->all.elmt_left,max));
}   /* H5S_all_favail() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_release
 PURPOSE
    Release all selection information for a dataspace
 USAGE
    herr_t H5S_all_release(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Releases "all" selection information for a dataspace
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_release (H5S_t * UNUSED space)
{
    FUNC_ENTER_NOAPI(H5S_all_release, FAIL);

    /* Check args */
    assert (space);

    FUNC_LEAVE (SUCCEED);
}   /* H5S_all_release() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_npoints
 PURPOSE
    Compute number of elements in current selection
 USAGE
    hsize_t H5S_all_npoints(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    The number of elements in selection on success, 0 on failure
 DESCRIPTION
    Compute number of elements in current selection.  For "all" selections,
    this is the same as the number of points in the extent.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hsize_t
H5S_all_npoints (const H5S_t *space)
{
    unsigned u;     /* Counters */
    hsize_t ret_value;

    FUNC_ENTER_NOAPI(H5S_all_npoints, 0);

    /* Check args */
    assert (space);

    for(u=0, ret_value=1; u<space->extent.u.simple.rank; u++)
        ret_value*=space->extent.u.simple.size[u];
    
    FUNC_LEAVE (ret_value);
}   /* H5S_all_npoints() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_select_serialize
 PURPOSE
    Serialize the current selection into a user-provided buffer.
 USAGE
    herr_t H5S_all_select_serialize(space, buf)
        H5S_t *space;           IN: Dataspace pointer of selection to serialize
        uint8 *buf;             OUT: Buffer to put serialized selection into
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Serializes the current element selection into a buffer.  (Primarily for
    storing on disk).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_select_serialize (const H5S_t *space, uint8_t *buf)
{
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_all_select_serialize, FAIL);

    assert(space);

    /* Store the preamble information */
    UINT32ENCODE(buf, (uint32_t)space->select.type);  /* Store the type of selection */
    UINT32ENCODE(buf, (uint32_t)1);  /* Store the version number */
    UINT32ENCODE(buf, (uint32_t)0);  /* Store the un-used padding */
    UINT32ENCODE(buf, (uint32_t)0);  /* Store the additional information length */

    /* Set success */
    ret_value=SUCCEED;

    FUNC_LEAVE (ret_value);
}   /* H5S_all_select_serialize() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_select_deserialize
 PURPOSE
    Deserialize the current selection from a user-provided buffer.
 USAGE
    herr_t H5S_all_select_deserialize(space, buf)
        H5S_t *space;           IN/OUT: Dataspace pointer to place selection into
        uint8 *buf;             IN: Buffer to retrieve serialized selection from
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Deserializes the current selection into a buffer.  (Primarily for retrieving
    from disk).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_select_deserialize (H5S_t *space, const uint8_t UNUSED *buf)
{
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_all_select_deserialize, FAIL);

    assert(space);

    /* Change to "all" selection */
    if((ret_value=H5S_select_all(space))<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection");
    } /* end if */

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_all_select_deserialize() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_bounds
 PURPOSE
    Gets the bounding box containing the selection.
 USAGE
    herr_t H5S_all_bounds(space, hsize_t *start, hsize_t *end)
        H5S_t *space;           IN: Dataspace pointer of selection to query
        hsize_t *start;         OUT: Starting coordinate of bounding box
        hsize_t *end;           OUT: Opposite coordinate of bounding box
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Retrieves the bounding box containing the current selection and places
    it into the user's buffers.  The start and end buffers must be large
    enough to hold the dataspace rank number of coordinates.  The bounding box
    exactly contains the selection, ie. if a 2-D element selection is currently
    defined with the following points: (4,5), (6,8) (10,7), the bounding box
    with be (4, 5), (10, 8).  Calling this function on a "none" selection
    returns fail.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_bounds(H5S_t *space, hsize_t *start, hsize_t *end)
{
    int rank;                  /* Dataspace rank */
    int i;                     /* index variable */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_NOAPI(H5S_all_bounds, FAIL);

    assert(space);
    assert(start);
    assert(end);

    /* Get the dataspace extent rank */
    rank=space->extent.u.simple.rank;

    /* Just copy over the complete extent */
    for(i=0; i<rank; i++) {
        start[i]=0;
        end[i]=space->extent.u.simple.size[i]-1;
    } /* end for */

    FUNC_LEAVE (ret_value);
}   /* H5Sget_all_bounds() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_all
 PURPOSE
    Specify the the entire extent is selected
 USAGE
    herr_t H5S_select_all(dsid)
        hid_t dsid;             IN: Dataspace ID of selection to modify
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function selects the entire extent for a dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_select_all (H5S_t *space)
{
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER_NOAPI(H5S_select_all, FAIL);

    /* Check args */
    assert(space);

    /* Remove current selection first */
    if(H5S_select_release(space)<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't release selection");
    } /* end if */

    /* Set selection type */
    space->select.type=H5S_SEL_ALL;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_select_all() */


/*--------------------------------------------------------------------------
 NAME
    H5Sselect_all
 PURPOSE
    Specify the the entire extent is selected
 USAGE
    herr_t H5Sselect_all(dsid)
        hid_t dsid;             IN: Dataspace ID of selection to modify
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function selects the entire extent for a dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Sselect_all (hid_t spaceid)
{
    H5S_t	*space = NULL;  /* Dataspace to modify selection of */
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER_API(H5Sselect_all, FAIL);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(spaceid) || NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }

    /* Remove current selection first */
    if((ret_value=H5S_select_all(space))<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection");
    } /* end if */

done:
    FUNC_LEAVE (ret_value);
}   /* H5Sselect_all() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_select_get_seq_list
 PURPOSE
    Create a list of offsets & lengths for a selection
 USAGE
    herr_t H5S_all_select_get_seq_list(flags,space,iter,elem_size,maxseq,maxbytes,nseq,nbytes,off,len)
        unsigned flags;         IN: Flags for extra information about operation
        H5S_t *space;           IN: Dataspace containing selection to use.
        H5S_sel_iter_t *iter;   IN/OUT: Selection iterator describing last
                                    position of interest in selection.
        size_t elem_size;       IN: Size of an element
        size_t maxseq;          IN: Maximum number of sequences to generate
        size_t maxbytes;        IN: Maximum number of bytes to include in the
                                    generated sequences
        size_t *nseq;           OUT: Actual number of sequences generated
        size_t *nbytes;         OUT: Actual number of bytes in sequences generated
        hsize_t *off;           OUT: Array of offsets
        size_t *len;            OUT: Array of lengths
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Use the selection in the dataspace to generate a list of byte offsets and
    lengths for the region(s) selected.  Start/Restart from the position in the
    ITER parameter.  The number of sequences generated is limited by the MAXSEQ
    parameter and the number of sequences actually generated is stored in the
    NSEQ parameter.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_select_get_seq_list(unsigned UNUSED flags, const H5S_t *space,H5S_sel_iter_t *iter,
    size_t elem_size, size_t maxseq, size_t maxbytes, size_t *nseq, size_t *nbytes,
    hsize_t *off, size_t *len)
{
    hsize_t bytes_left;         /* The number of bytes left in the selection */
    hsize_t elem_used;          /* The number of bytes used */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_NOAPI (H5S_all_select_get_seq_list, FAIL);

    /* Check args */
    assert(space);
    assert(iter);
    assert(elem_size>0);
    assert(maxseq>0);
    assert(maxbytes>0);
    assert(nseq);
    assert(nbytes);
    assert(off);
    assert(len);

    /* Calculate the number of bytes left in the selection */
    bytes_left=iter->all.elmt_left*elem_size;

    /* "round" off the maxbytes allowed to a multiple of the element size */
    maxbytes=(maxbytes/elem_size)*elem_size;

    /* Compute the offset in the dataset */
    off[0]=iter->all.offset*elem_size;
    len[0]=MIN(maxbytes,bytes_left);

    /* Should only need one sequence for 'all' selections */
    *nseq=1;

    /* Set the number of bytes used */
    *nbytes=len[0];

    /* Update the iterator */
    elem_used=len[0]/elem_size;
    iter->all.elmt_left-=elem_used;
    iter->all.offset+=elem_used;

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
} /* end H5S_all_select_get_seq_list() */
