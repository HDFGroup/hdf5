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
 * Function:	H5S_all_iter_init
 *
 * Purpose:	Initializes iteration information for "all" selection.
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
H5S_all_iter_init (const H5S_t *space, size_t UNUSED elmt_size, H5S_sel_iter_t *sel_iter)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5S_all_iter_init, FAIL);

    /* Check args */
    assert (space && H5S_SEL_ALL==space->select.type);
    assert (sel_iter);

    /* Initialize the number of elements to iterate over */
    sel_iter->all.elmt_left=H5S_get_simple_extent_npoints(space);

    /* Start at the upper left location */
    sel_iter->all.offset=0;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_all_iter_init() */


/*-------------------------------------------------------------------------
 * Function:	H5S_all_iter_nelmts
 *
 * Purpose:	Return number of elements left to process in iterator
 *
 * Return:	non-negative number of elements on success, zero on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5S_all_iter_nelmts (const H5S_sel_iter_t *sel_iter)
{
    hsize_t ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5S_all_iter_nelmts, 0);

    /* Check args */
    assert (sel_iter);

    /* Set return value */
    ret_value=sel_iter->all.elmt_left;

done:
    FUNC_LEAVE(ret_value);
}   /* H5S_all_iter_nelmts() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_iter_release
 PURPOSE
    Release "all" selection iterator information for a dataspace
 USAGE
    herr_t H5S_all_iter_release(sel_iter)
        H5S_sel_iter_t *sel_iter;       IN: Pointer to selection iterator
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Releases all information for a dataspace "all" selection iterator
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_iter_release (H5S_sel_iter_t * UNUSED sel_iter)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5S_all_iter_release, FAIL);

    /* Check args */
    assert (sel_iter);

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_all_iter_release() */


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
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5S_all_release, FAIL);

    /* Check args */
    assert (space);

done:
    FUNC_LEAVE (ret_value);
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
    hsize_t ret_value;

    FUNC_ENTER_NOAPI(H5S_all_npoints, 0);

    /* Check args */
    assert (space);

    ret_value=(hsize_t)H5S_get_simple_extent_npoints(space);
    
done:
    FUNC_LEAVE (ret_value);
}   /* H5S_all_npoints() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_is_valid
 PURPOSE
    Check whether the selection fits within the extent, with the current
    offset defined.
 USAGE
    htri_t H5S_all_is_valid(space);
        H5S_t *space;             IN: Dataspace pointer to query
 RETURNS
    TRUE if the selection fits within the extent, FALSE if it does not and
        Negative on an error.
 DESCRIPTION
    Determines if the current selection at the current offet fits within the
    extent for the dataspace.  Offset is irrelevant for this type of selection.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5S_all_is_valid (const H5S_t UNUSED *space)
{
    htri_t ret_value=TRUE;      /* Return value */

    FUNC_ENTER_NOAPI(H5S_all_is_valid, FAIL);

    assert(space);

done:
    FUNC_LEAVE (ret_value);
} /* end H5S_all_is_valid() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_serial_size
 PURPOSE
    Determine the number of bytes needed to store the serialized "all"
        selection information.
 USAGE
    hssize_t H5S_all_serial_size(space)
        H5S_t *space;             IN: Dataspace pointer to query
 RETURNS
    The number of bytes required on success, negative on an error.
 DESCRIPTION
    Determines the number of bytes required to serialize an "all"
    selection for storage on disk.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hssize_t
H5S_all_serial_size (const H5S_t UNUSED *space)
{
    hssize_t ret_value=FAIL;    /* return value */

    FUNC_ENTER_NOAPI(H5S_all_serial_size, FAIL);

    assert(space);

    /* Basic number of bytes required to serialize point selection:
     *  <type (4 bytes)> + <version (4 bytes)> + <padding (4 bytes)> + 
     *      <length (4 bytes)> = 16 bytes
     */
    ret_value=16;

done:
    FUNC_LEAVE (ret_value);
} /* end H5S_all_serial_size() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_serialize
 PURPOSE
    Serialize the current selection into a user-provided buffer.
 USAGE
    herr_t H5S_all_serialize(space, buf)
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
H5S_all_serialize (const H5S_t *space, uint8_t *buf)
{
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_all_serialize, FAIL);

    assert(space);

    /* Store the preamble information */
    UINT32ENCODE(buf, (uint32_t)space->select.type);  /* Store the type of selection */
    UINT32ENCODE(buf, (uint32_t)1);  /* Store the version number */
    UINT32ENCODE(buf, (uint32_t)0);  /* Store the un-used padding */
    UINT32ENCODE(buf, (uint32_t)0);  /* Store the additional information length */

    /* Set success */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_all_serialize() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_deserialize
 PURPOSE
    Deserialize the current selection from a user-provided buffer.
 USAGE
    herr_t H5S_all_deserialize(space, buf)
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
H5S_all_deserialize (H5S_t *space, const uint8_t UNUSED *buf)
{
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_all_deserialize, FAIL);

    assert(space);

    /* Change to "all" selection */
    if((ret_value=H5S_select_all(space,1))<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection");

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_all_deserialize() */


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
H5S_all_bounds(const H5S_t *space, hsize_t *start, hsize_t *end)
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

done:
    FUNC_LEAVE (ret_value);
}   /* H5Sget_all_bounds() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_is_contiguous
 PURPOSE
    Check if a "all" selection is contiguous within the dataspace extent.
 USAGE
    htri_t H5S_all_is_contiguous(space)
        H5S_t *space;           IN: Dataspace pointer to check
 RETURNS
    TRUE/FALSE/FAIL
 DESCRIPTION
    Checks to see if the current selection in the dataspace is contiguous.
    This is primarily used for reading the entire selection in one swoop.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5S_all_is_contiguous(const H5S_t UNUSED *space)
{
    htri_t ret_value=TRUE;   /* Return value */

    FUNC_ENTER_NOAPI(H5S_all_is_contiguous, FAIL);

    assert(space);

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_all_is_contiguous() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_is_single
 PURPOSE
    Check if a "all" selection is a single block within the dataspace extent.
 USAGE
    htri_t H5S_all_is_single(space)
        H5S_t *space;           IN: Dataspace pointer to check
 RETURNS
    TRUE/FALSE/FAIL
 DESCRIPTION
    Checks to see if the current selection in the dataspace is a single block.
    This is primarily used for reading the entire selection in one swoop.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5S_all_is_single(const H5S_t UNUSED *space)
{
    htri_t ret_value=TRUE;   /* Return value */

    FUNC_ENTER_NOAPI(H5S_all_is_single, FAIL);

    assert(space);

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_all_is_single() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_is_regular
 PURPOSE
    Check if a "all" selection is "regular"
 USAGE
    htri_t H5S_all_is_regular(space)
        const H5S_t *space;     IN: Dataspace pointer to check
 RETURNS
    TRUE/FALSE/FAIL
 DESCRIPTION
    Checks to see if the current selection in a dataspace is the a regular
    pattern.
    This is primarily used for reading the entire selection in one swoop.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5S_all_is_regular(const H5S_t UNUSED *space)
{
    htri_t ret_value=TRUE;   /* Return value */

    FUNC_ENTER_NOAPI(H5S_all_is_regular, FAIL);

    /* Check args */
    assert(space);

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_all_is_regular() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_all
 PURPOSE
    Specify the the entire extent is selected
 USAGE
    herr_t H5S_select_all(dsid)
        hid_t dsid;             IN: Dataspace ID of selection to modify
        unsigned rel_prev;      IN: Flag whether to release previous selection or not
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
H5S_select_all (H5S_t *space, unsigned rel_prev)
{
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER_NOAPI(H5S_select_all, FAIL);

    /* Check args */
    assert(space);

    /* Remove current selection first */
    if(rel_prev)
        if((*space->select.release)(space)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't release selection");

    /* Set selection type */
    space->select.type=H5S_SEL_ALL;

    /* Set selection methods */
    space->select.get_seq_list=H5S_all_get_seq_list;
    space->select.get_npoints=H5S_all_npoints;
    space->select.release=H5S_all_release;
    space->select.iter_init=H5S_all_iter_init;
    space->select.iter_nelmts=H5S_all_iter_nelmts;
    space->select.iter_release=H5S_all_iter_release;
    space->select.is_valid=H5S_all_is_valid;
    space->select.serial_size=H5S_all_serial_size;
    space->select.serialize=H5S_all_serialize;
    space->select.bounds=H5S_all_bounds;
    space->select.is_contiguous=H5S_all_is_contiguous;
    space->select.is_single=H5S_all_is_single;
    space->select.is_regular=H5S_all_is_regular;

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
    if (NULL == (space=H5I_object_verify(spaceid, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

    /* Call internal routine to do the work */
    if((ret_value=H5S_select_all(space,1))<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Sselect_all() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_get_seq_list
 PURPOSE
    Create a list of offsets & lengths for a selection
 USAGE
    herr_t H5S_all_get_seq_list(space,flags,iter,elem_size,maxseq,maxbytes,nseq,nbytes,off,len)
        H5S_t *space;           IN: Dataspace containing selection to use.
        unsigned flags;         IN: Flags for extra information about operation
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
H5S_all_get_seq_list(const H5S_t UNUSED *space, unsigned UNUSED flags, H5S_sel_iter_t *iter,
    size_t elem_size, size_t UNUSED maxseq, size_t maxbytes, size_t *nseq, size_t *nbytes,
    hsize_t *off, size_t *len)
{
    hsize_t bytes_left;         /* The number of bytes left in the selection */
    hsize_t elem_used;          /* The number of bytes used */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_NOAPI (H5S_all_get_seq_list, FAIL);

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

done:
    FUNC_LEAVE (ret_value);
} /* end H5S_all_get_seq_list() */

