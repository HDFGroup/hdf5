/*
 * Copyright (C) 1998-2001 NCSA
 *                         All rights reserved.
 *
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.ued>
 *              Friday, May 29, 1998
 *
 * Purpose:	Dataspace functions.
 */

#define H5S_PACKAGE		/*suppress error about including H5Spkg	  */

#include "H5private.h"		/* Generic Functions			  */
#include "H5Dprivate.h"         /* Datasets (for their properties) */
#include "H5Eprivate.h"		/* Error handling		  */
#include "H5FLprivate.h"	/* Free Lists	  */
#include "H5Iprivate.h"		/* ID Functions		  */
#include "H5Spkg.h"		/* Dataspace functions			  */
#include "H5Vprivate.h"         /* Vector functions */

/* Interface initialization */
#define PABLO_MASK      H5Sselect_mask
#define INTERFACE_INIT  NULL
static int             interface_initialize_g = 0;

static hssize_t H5S_get_select_hyper_nblocks(H5S_t *space);
static hssize_t H5S_get_select_elem_npoints(H5S_t *space);
static herr_t H5S_get_select_hyper_blocklist(H5S_t *space, hsize_t startblock, hsize_t numblocks, hsize_t *buf);
static herr_t H5S_get_select_elem_pointlist(H5S_t *space, hsize_t startpoint, hsize_t numpoints, hsize_t *buf);
static herr_t H5S_get_select_bounds(H5S_t *space, hsize_t *start, hsize_t *end);

/* Declare external the free list for hssize_t arrays */
H5FL_ARR_EXTERN(hssize_t);

/* Declare a free list to manage arrays of size_t */
H5FL_ARR_DEFINE_STATIC(size_t,-1);

/* Declare a free list to manage arrays of hsize_t */
H5FL_ARR_DEFINE_STATIC(hsize_t,-1);

/* Declare a free list to manage the H5S_sel_iter_t struct */
H5FL_DEFINE_STATIC(H5S_sel_iter_t);

/* Declare a free list to manage blocks of single datatype element data */
H5FL_BLK_EXTERN(type_elem);


/*--------------------------------------------------------------------------
 NAME
    H5S_select_copy
 PURPOSE
    Copy a selection from one dataspace to another
 USAGE
    herr_t H5S_select_copy(dst, src)
        H5S_t *dst;  OUT: Pointer to the destination dataspace
        H5S_t *src;  IN: Pointer to the source dataspace
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Copies all the selection information (include offset) from the source
    dataspace to the destination dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_select_copy (H5S_t *dst, const H5S_t *src)
{
    herr_t ret_value=SUCCEED;     /* return value */

    FUNC_ENTER_NOAPI(H5S_select_copy, FAIL);

    /* Check args */
    assert(dst);
    assert(src);

    /* Copy regular fields */
    dst->select=src->select;

/* Need to copy order information still */

    /* Copy offset information */
    if (NULL==(dst->select.offset = H5FL_ARR_ALLOC(hssize_t,src->extent.u.simple.rank,1))) {
        HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
		       "memory allocation failed");
    }
    if(src->select.offset!=NULL)
        HDmemcpy(dst->select.offset,src->select.offset,(src->extent.u.simple.rank*sizeof(hssize_t)));

    /* Perform correct type of copy based on the type of selection */
    switch (src->extent.type) {
        case H5S_SCALAR:
            /*nothing needed */
            break;

        case H5S_SIMPLE:
            /* Deep copy extra stuff */
            switch(src->select.type) {
                case H5S_SEL_NONE:
                case H5S_SEL_ALL:
                    /*nothing needed */
                    break;

                case H5S_SEL_POINTS:
                    ret_value=H5S_point_copy(dst,src);
                    break;

                case H5S_SEL_HYPERSLABS:
                    ret_value=H5S_hyper_copy(dst,src);
                    break;

                default:
                    assert("unknown selection type" && 0);
                    break;
            } /* end switch */
            break;

        case H5S_COMPLEX:
            /*void */
            break;

        default:
            assert("unknown data space type" && 0);
            break;
    }

    FUNC_LEAVE (ret_value);
}   /* H5S_select_copy() */

/*--------------------------------------------------------------------------
 NAME
    H5S_select_release
 PURPOSE
    Release selection information for a dataspace
 USAGE
    herr_t H5S_select_release(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Releases all selection information for a dataspace
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_select_release (H5S_t *space)
{
    herr_t ret_value=SUCCEED;     /* return value */

    FUNC_ENTER_NOAPI(H5S_select_release, FAIL);

    /* Check args */
    assert (space);

    switch(space->select.type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
            ret_value=H5S_point_release(space);
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_release(space);
            break;

        case H5S_SEL_ALL:            /* Entire extent selected */
            ret_value=H5S_all_release(space);
            break;

        case H5S_SEL_NONE:           /* Nothing selected */
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    }

    /* Reset type of selection to "all" */
    space->select.type=H5S_SEL_ALL;

    FUNC_LEAVE (ret_value);
}   /* H5S_select_release() */


/*--------------------------------------------------------------------------
 NAME
    H5Sget_select_npoints
 PURPOSE
    Get the number of elements in current selection
 USAGE
    hssize_t H5Sget_select_npoints(dsid)
        hid_t dsid;             IN: Dataspace ID of selection to query
 RETURNS
    The number of elements in selection on success, 0 on failure
 DESCRIPTION
    Returns the number of elements in current selection for dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hssize_t
H5Sget_select_npoints(hid_t spaceid)
{
    H5S_t	*space = NULL;      /* Dataspace to modify selection of */
    hssize_t ret_value=FAIL;        /* return value */

    FUNC_ENTER_API(H5Sget_select_npoints, 0);
    H5TRACE1("Hs","i",spaceid);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a data space");
    }

    ret_value = H5S_get_select_npoints(space);

    FUNC_LEAVE (ret_value);
}   /* H5Sget_select_npoints() */


/*--------------------------------------------------------------------------
 NAME
    H5S_get_select_npoints
 PURPOSE
    Get the number of elements in current selection
 USAGE
    herr_t H5S_get_select_npoints(ds)
        H5S_t *ds;             IN: Dataspace pointer
 RETURNS
    The number of elements in selection on success, 0 on failure
 DESCRIPTION
    Returns the number of elements in current selection for dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hssize_t
H5S_get_select_npoints (const H5S_t *space)
{
    hssize_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_get_select_npoints, FAIL);

    assert(space);

    switch(space->select.type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
            ret_value=H5S_point_npoints(space);
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_npoints(space);
            break;

        case H5S_SEL_ALL:            /* Entire extent selected */
            ret_value=H5S_all_npoints(space);
            break;

        case H5S_SEL_NONE:           /* Nothing selected */
            ret_value=0;
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    }

    FUNC_LEAVE (ret_value);
}   /* H5S_get_select_npoints() */


/*--------------------------------------------------------------------------
 NAME
    H5S_sel_iter_release
 PURPOSE
    Release selection iterator information for a dataspace
 USAGE
    herr_t H5S_sel_iter_release(sel_iter)
        const H5S_t *space;             IN: Pointer to dataspace iterator is for
        H5S_sel_iter_t *sel_iter;       IN: Pointer to selection iterator
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Releases all information for a dataspace selection iterator
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_sel_iter_release (const H5S_t *space, H5S_sel_iter_t *sel_iter)
{
    herr_t ret_value=SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(H5S_sel_iter_release, FAIL);

    /* Check args */
    assert (sel_iter);

    switch(space->select.type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
        case H5S_SEL_ALL:            /* Entire extent selected */
            /* no action needed */
            ret_value=SUCCEED;
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_sel_iter_release(sel_iter);
            break;

        case H5S_SEL_NONE:           /* Nothing selected */
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    }

    FUNC_LEAVE (ret_value);
}   /* H5S_sel_iter_release() */

/*--------------------------------------------------------------------------
 NAME
    H5Sselect_valid
 PURPOSE
    Check whether the selection fits within the extent, with the current
    offset defined.
 USAGE
    htri_t H5Sselect_void(dsid)
        hid_t dsid;             IN: Dataspace ID to query
 RETURNS
    TRUE if the selection fits within the extent, FALSE if it does not and
        Negative on an error.
 DESCRIPTION
    Determines if the current selection at the current offet fits within the
    extent for the dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5Sselect_valid(hid_t spaceid)
{
    H5S_t	*space = NULL;      /* Dataspace to modify selection of */
    htri_t ret_value=FAIL;     /* return value */

    FUNC_ENTER_API(H5Sselect_valid, 0);
    H5TRACE1("b","i",spaceid);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a data space");
    }

    ret_value = H5S_select_valid(space);

    FUNC_LEAVE (ret_value);
}   /* H5Sselect_valid() */

/*--------------------------------------------------------------------------
 NAME
    H5S_select_valid
 PURPOSE
    Check whether the selection fits within the extent, with the current
    offset defined.
 USAGE
    htri_t H5Sselect_void(space)
        H5S_t *space;             IN: Dataspace pointer to query
 RETURNS
    TRUE if the selection fits within the extent, FALSE if it does not and
        Negative on an error.
 DESCRIPTION
    Determines if the current selection at the current offet fits within the
    extent for the dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5S_select_valid (const H5S_t *space)
{
    htri_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_select_valid, FAIL);

    assert(space);

    switch(space->select.type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
            ret_value=H5S_point_select_valid(space);
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_select_valid(space);
            break;

        case H5S_SEL_ALL:            /* Entire extent selected */
        case H5S_SEL_NONE:           /* Nothing selected */
            ret_value=TRUE;
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    }

    FUNC_LEAVE (ret_value);
}   /* H5S_select_valid() */

/*--------------------------------------------------------------------------
 NAME
    H5S_select_serial_size
 PURPOSE
    Determine the number of bytes needed to serialize the current selection
    offset defined.
 USAGE
    hssize_t H5S_select_serial_size(space)
        H5S_t *space;             IN: Dataspace pointer to query
 RETURNS
    The number of bytes required on success, negative on an error.
 DESCRIPTION
    Determines the number of bytes required to serialize the current selection
    information for storage on disk.  This routine just hands off to the
    appropriate routine for each type of selection.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hssize_t
H5S_select_serial_size (const H5S_t *space)
{
    hssize_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_select_serial_size, FAIL);

    assert(space);

    switch(space->select.type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
            ret_value=H5S_point_select_serial_size(space);
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_select_serial_size(space);
            break;

        case H5S_SEL_ALL:            /* Entire extent selected */
        case H5S_SEL_NONE:           /* Nothing selected */
            ret_value=16;            /* replace with real function call at some point */
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    }

    FUNC_LEAVE (ret_value);
}   /* H5S_select_serial_size() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_serialize
 PURPOSE
    Serialize the current selection into a user-provided buffer.
 USAGE
    herr_t H5S_select_serialize(space, buf)
        H5S_t *space;           IN: Dataspace pointer of selection to serialize
        uint8 *buf;             OUT: Buffer to put serialized selection into
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Serializes the current selection into a buffer.  (Primarily for storing
    on disk).  This routine just hands off to the appropriate routine for each
    type of selection.
    The serialized information for all types of selections follows this format:
        <type of selection> = uint32
        <version #>         = uint32
        <padding, not-used> = 4 bytes
        <length of selection specific information> = uint32
        <selection specific information> = ? bytes (depends on selection type)
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_select_serialize (const H5S_t *space, uint8_t *buf)
{
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_select_serialize, FAIL);

    assert(space);

    switch(space->select.type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
            ret_value=H5S_point_select_serialize(space,buf);
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_select_serialize(space,buf);
            break;

        case H5S_SEL_ALL:            /* Entire extent selected */
            ret_value=H5S_all_select_serialize(space,buf);
            break;

        case H5S_SEL_NONE:           /* Nothing selected */
            ret_value=H5S_none_select_serialize(space,buf);
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    }

    FUNC_LEAVE (ret_value);
}   /* H5S_select_serialize() */

/*--------------------------------------------------------------------------
 NAME
    H5S_select_deserialize
 PURPOSE
    Deserialize the current selection from a user-provided buffer into a real
        selection in the dataspace.
 USAGE
    herr_t H5S_select_deserialize(space, buf)
        H5S_t *space;           IN/OUT: Dataspace pointer to place selection into
        uint8 *buf;             IN: Buffer to retrieve serialized selection from
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Deserializes the current selection into a buffer.  (Primarily for retrieving
    from disk).  This routine just hands off to the appropriate routine for each
    type of selection.  The format of the serialized information is shown in
    the H5S_select_serialize() header.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_select_deserialize (H5S_t *space, const uint8_t *buf)
{
    const uint8_t *tbuf;    /* Temporary pointer to the selection type */
    uint32_t sel_type;       /* Pointer to the selection type */
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_select_deserialize, FAIL);

    assert(space);

    tbuf=buf;
    UINT32DECODE(tbuf, sel_type);
    switch(sel_type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
            ret_value=H5S_point_select_deserialize(space,buf);
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_select_deserialize(space,buf);
            break;

        case H5S_SEL_ALL:            /* Entire extent selected */
            ret_value=H5S_all_select_deserialize(space,buf);
            break;

        case H5S_SEL_NONE:           /* Nothing selected */
            ret_value=H5S_none_select_deserialize(space,buf);
            break;

        default:
            break;
    }

    FUNC_LEAVE (ret_value);
}   /* H5S_select_deserialize() */


/*--------------------------------------------------------------------------
 NAME
    H5S_get_select_hyper_nblocks
 PURPOSE
    Get the number of hyperslab blocks in current hyperslab selection
 USAGE
    hssize_t H5S_get_select_hyper_nblocks(space)
        H5S_t *space;             IN: Dataspace ptr of selection to query
 RETURNS
    The number of hyperslab blocks in selection on success, negative on failure
 DESCRIPTION
    Returns the number of hyperslab blocks in current selection for dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static hssize_t
H5S_get_select_hyper_nblocks(H5S_t *space)
{
    hssize_t ret_value=FAIL;        /* return value */
    unsigned u;                    /* Counter */

    FUNC_ENTER_NOINIT(H5S_get_select_hyper_nblocks);

    assert(space);

    /* Check for a "regular" hyperslab selection */
    if(space->select.sel_info.hslab.diminfo != NULL) {
        /* Check each dimension */
        for(ret_value=1,u=0; u<space->extent.u.simple.rank; u++)
            ret_value*=space->select.sel_info.hslab.app_diminfo[u].count;
    } /* end if */
    else
        ret_value = H5S_hyper_span_nblocks(space->select.sel_info.hslab.span_lst);

    FUNC_LEAVE (ret_value);
}   /* H5S_get_select_hyper_nblocks() */


/*--------------------------------------------------------------------------
 NAME
    H5Sget_select_hyper_nblocks
 PURPOSE
    Get the number of hyperslab blocks in current hyperslab selection
 USAGE
    hssize_t H5Sget_select_hyper_nblocks(dsid)
        hid_t dsid;             IN: Dataspace ID of selection to query
 RETURNS
    The number of hyperslab blocks in selection on success, negative on failure
 DESCRIPTION
    Returns the number of hyperslab blocks in current selection for dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hssize_t
H5Sget_select_hyper_nblocks(hid_t spaceid)
{
    H5S_t	*space = NULL;      /* Dataspace to modify selection of */
    hssize_t ret_value=FAIL;        /* return value */

    FUNC_ENTER_API(H5Sget_select_hyper_nblocks, FAIL);
    H5TRACE1("Hs","i",spaceid);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(space->select.type!=H5S_SEL_HYPERSLABS)
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a hyperslab selection");

    ret_value = H5S_get_select_hyper_nblocks(space);

    FUNC_LEAVE (ret_value);
}   /* H5Sget_select_hyper_nblocks() */


/*--------------------------------------------------------------------------
 NAME
    H5S_get_select_elem_npoints
 PURPOSE
    Get the number of points in current element selection
 USAGE
    hssize_t H5S_get_select_elem_npoints(space)
        H5S_t *space;             IN: Dataspace ptr of selection to query
 RETURNS
    The number of element points in selection on success, negative on failure
 DESCRIPTION
    Returns the number of element points in current selection for dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static hssize_t
H5S_get_select_elem_npoints(H5S_t *space)
{
    hssize_t ret_value=FAIL;        /* return value */

    FUNC_ENTER_NOINIT(H5S_get_select_elem_npoints);

    assert(space);

    ret_value = space->select.num_elem;

    FUNC_LEAVE (ret_value);
}   /* H5Sget_select_elem_npoints() */


/*--------------------------------------------------------------------------
 NAME
    H5Sget_select_elem_npoints
 PURPOSE
    Get the number of points in current element selection
 USAGE
    hssize_t H5Sget_select_elem_npoints(dsid)
        hid_t dsid;             IN: Dataspace ID of selection to query
 RETURNS
    The number of element points in selection on success, negative on failure
 DESCRIPTION
    Returns the number of element points in current selection for dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hssize_t
H5Sget_select_elem_npoints(hid_t spaceid)
{
    H5S_t	*space = NULL;      /* Dataspace to modify selection of */
    hssize_t ret_value=FAIL;        /* return value */

    FUNC_ENTER_API(H5Sget_select_elem_npoints, FAIL);
    H5TRACE1("Hs","i",spaceid);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(space->select.type!=H5S_SEL_POINTS)
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an element selection");

    ret_value = H5S_get_select_elem_npoints(space);

    FUNC_LEAVE (ret_value);
}   /* H5Sget_select_elem_npoints() */


/*--------------------------------------------------------------------------
 NAME
    H5S_get_select_hyper_blocklist
 PURPOSE
    Get the list of hyperslab blocks currently selected
 USAGE
    herr_t H5S_get_select_hyper_blocklist(space, startblock, numblocks, buf)
        H5S_t *space;           IN: Dataspace pointer of selection to query
        hsize_t startblock;     IN: Hyperslab block to start with
        hsize_t numblocks;      IN: Number of hyperslab blocks to get
        hsize_t *buf;           OUT: List of hyperslab blocks selected
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
        Puts a list of the hyperslab blocks into the user's buffer.  The blocks
    start with the 'startblock'th block in the list of blocks and put
    'numblocks' number of blocks into the user's buffer (or until the end of
    the list of blocks, whichever happens first)
        The block coordinates have the same dimensionality (rank) as the
    dataspace they are located within.  The list of blocks is formatted as
    follows: <"start" coordinate> immediately followed by <"opposite" corner
    coordinate>, followed by the next "start" and "opposite" coordinate, etc.
    until all the block information requested has been put into the user's
    buffer.
        No guarantee of any order of the blocks is implied.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_get_select_hyper_blocklist(H5S_t *space, hsize_t startblock, hsize_t numblocks, hsize_t *buf)
{
    H5S_hyper_dim_t *diminfo;               /* Alias for dataspace's diminfo information */
    hsize_t tmp_count[H5O_LAYOUT_NDIMS];    /* Temporary hyperslab counts */
    hssize_t offset[H5O_LAYOUT_NDIMS];      /* Offset of element in dataspace */
    hssize_t start[H5O_LAYOUT_NDIMS];   /* Location of start of hyperslab */
    hssize_t end[H5O_LAYOUT_NDIMS];     /* Location of end of hyperslab */
    hssize_t temp_off;            /* Offset in a given dimension */
    int i;                     /* Counter */
    int fast_dim;      /* Rank of the fastest changing dimension for the dataspace */
    int temp_dim;      /* Temporary rank holder */
    int ndims;         /* Rank of the dataspace */
    int done;          /* Whether we are done with the iteration */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_NOINIT(H5S_get_select_hyper_blocklist);

    assert(space);
    assert(buf);

    /* Check for a "regular" hyperslab selection */
    if(space->select.sel_info.hslab.diminfo != NULL) {
        /* Set some convienence values */
        ndims=space->extent.u.simple.rank;
        fast_dim=ndims-1;
        /*
         * Use the "application dimension information" to pass back to the user
         * the blocks they set, not the optimized, internal information.
         */
        diminfo=space->select.sel_info.hslab.app_diminfo;

        /* Build the tables of count sizes as well as the initial offset */
        for(i=0; i<ndims; i++) {
            tmp_count[i]=diminfo[i].count;
            offset[i]=diminfo[i].start;
        } /* end for */

        /* We're not done with the iteration */
        done=0;

        /* Go iterate over the hyperslabs */
        while(done==0 && numblocks>0) {
            /* Iterate over the blocks in the fastest dimension */
            while(tmp_count[fast_dim]>0 && numblocks>0) {

                /* Check if we should copy this block information */
                if(startblock==0) {
                    /* Copy the starting location */
                    HDmemcpy(buf,offset,sizeof(hsize_t)*ndims);
                    buf+=ndims;

                    /* Compute the ending location */
                    HDmemcpy(buf,offset,sizeof(hsize_t)*ndims);
                    for(i=0; i<ndims; i++)
                        buf[i]+=(diminfo[i].block-1);
                    buf+=ndims;

                    /* Decrement the number of blocks to retrieve */
                    numblocks--;
                } /* end if */
                else
                    startblock--;

                /* Move the offset to the next sequence to start */
                offset[fast_dim]+=diminfo[fast_dim].stride;

                /* Decrement the block count */
                tmp_count[fast_dim]--;
            } /* end while */

            /* Work on other dimensions if necessary */
            if(fast_dim>0 && numblocks>0) {
                /* Reset the block counts */
                tmp_count[fast_dim]=diminfo[fast_dim].count;

                /* Bubble up the decrement to the slower changing dimensions */
                temp_dim=fast_dim-1;
                while(temp_dim>=0 && done==0) {
                    /* Decrement the block count */
                    tmp_count[temp_dim]--;

                    /* Check if we have more blocks left */
                    if(tmp_count[temp_dim]>0)
                        break;

                    /* Check for getting out of iterator */
                    if(temp_dim==0)
                        done=1;

                    /* Reset the block count in this dimension */
                    tmp_count[temp_dim]=diminfo[temp_dim].count;
                
                    /* Wrapped a dimension, go up to next dimension */
                    temp_dim--;
                } /* end while */
            } /* end if */

            /* Re-compute offset array */
            for(i=0; i<ndims; i++) {
                temp_off=diminfo[i].start+diminfo[i].stride*(diminfo[i].count-tmp_count[i]);
                offset[i]=temp_off;
            } /* end for */
        } /* end while */
    } /* end if */
    else {
        ret_value=H5S_hyper_span_blocklist(space->select.sel_info.hslab.span_lst,start,end,(hsize_t)0,&startblock,&numblocks,&buf);
    } /* end else */

    FUNC_LEAVE (ret_value);
}   /* H5S_get_select_hyper_blocklist() */


/*--------------------------------------------------------------------------
 NAME
    H5Sget_select_hyper_blocklist
 PURPOSE
    Get the list of hyperslab blocks currently selected
 USAGE
    herr_t H5Sget_select_hyper_blocklist(dsid, startblock, numblocks, buf)
        hid_t dsid;             IN: Dataspace ID of selection to query
        hsize_t startblock;     IN: Hyperslab block to start with
        hsize_t numblocks;      IN: Number of hyperslab blocks to get
        hsize_t *buf;           OUT: List of hyperslab blocks selected
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
        Puts a list of the hyperslab blocks into the user's buffer.  The blocks
    start with the 'startblock'th block in the list of blocks and put
    'numblocks' number of blocks into the user's buffer (or until the end of
    the list of blocks, whichever happen first)
        The block coordinates have the same dimensionality (rank) as the
    dataspace they are located within.  The list of blocks is formatted as
    follows: <"start" coordinate> immediately followed by <"opposite" corner
    coordinate>, followed by the next "start" and "opposite" coordinate, etc.
    until all the block information requested has been put into the user's
    buffer.
        No guarantee of any order of the blocks is implied.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Sget_select_hyper_blocklist(hid_t spaceid, hsize_t startblock, hsize_t numblocks, hsize_t *buf)
{
    H5S_t	*space = NULL;      /* Dataspace to modify selection of */
    herr_t ret_value=FAIL;        /* return value */

    FUNC_ENTER_API(H5Sget_select_hyper_blocklist, FAIL);
    H5TRACE4("e","ihh*h",spaceid,startblock,numblocks,buf);

    /* Check args */
    if(buf==NULL)
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid pointer");
    if (H5I_DATASPACE != H5I_get_type(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(space->select.type!=H5S_SEL_HYPERSLABS)
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a hyperslab selection");

    /* Go get the correct number of blocks */
    if(numblocks>0)
        ret_value = H5S_get_select_hyper_blocklist(space,startblock,numblocks,buf);
    else
        ret_value=SUCCEED;      /* Successfully got 0 blocks... */

    FUNC_LEAVE (ret_value);
}   /* H5Sget_select_hyper_blocklist() */


/*--------------------------------------------------------------------------
 NAME
    H5S_get_select_elem_pointlist
 PURPOSE
    Get the list of element points currently selected
 USAGE
    herr_t H5S_get_select_elem_pointlist(space, hsize_t *buf)
        H5S_t *space;           IN: Dataspace pointer of selection to query
        hsize_t startpoint;     IN: Element point to start with
        hsize_t numpoints;      IN: Number of element points to get
        hsize_t *buf;           OUT: List of element points selected
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
        Puts a list of the element points into the user's buffer.  The points
    start with the 'startpoint'th block in the list of points and put
    'numpoints' number of points into the user's buffer (or until the end of
    the list of points, whichever happen first)
        The point coordinates have the same dimensionality (rank) as the
    dataspace they are located within.  The list of points is formatted as
    follows: <coordinate> followed by the next coordinate, etc. until all the
    point information in the selection have been put into the user's buffer.
        The points are returned in the order they will be interated through
    when a selection is read/written from/to disk.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_get_select_elem_pointlist(H5S_t *space, hsize_t startpoint, hsize_t numpoints, hsize_t *buf)
{
    H5S_pnt_node_t *node;       /* Point node */
    int rank;                  /* Dataspace rank */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_NOINIT(H5S_get_select_elem_pointlist);

    assert(space);
    assert(buf);

    /* Get the dataspace extent rank */
    rank=space->extent.u.simple.rank;

    /* Get the head of the point list */
    node=space->select.sel_info.pnt_lst->head;

    /* Iterate to the first point to return */
    while(node!=NULL && startpoint>0) {
        startpoint--;
        node=node->next;
      } /* end while */

    /* Iterate through the node, copying each hyperslab's information */
    while(node!=NULL && numpoints>0) {
        HDmemcpy(buf,node->pnt,sizeof(hsize_t)*rank);
        buf+=rank;
        numpoints--;
        node=node->next;
      } /* end while */

    FUNC_LEAVE (ret_value);
}   /* H5Sget_select_elem_pointlist() */


/*--------------------------------------------------------------------------
 NAME
    H5Sget_select_elem_pointlist
 PURPOSE
    Get the list of element points currently selected
 USAGE
    herr_t H5Sget_select_elem_pointlist(dsid, hsize_t *buf)
        hid_t dsid;             IN: Dataspace ID of selection to query
        hsize_t startpoint;     IN: Element point to start with
        hsize_t numpoints;      IN: Number of element points to get
        hsize_t *buf;           OUT: List of element points selected
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
        Puts a list of the element points into the user's buffer.  The points
    start with the 'startpoint'th block in the list of points and put
    'numpoints' number of points into the user's buffer (or until the end of
    the list of points, whichever happen first)
        The point coordinates have the same dimensionality (rank) as the
    dataspace they are located within.  The list of points is formatted as
    follows: <coordinate> followed by the next coordinate, etc. until all the
    point information in the selection have been put into the user's buffer.
        The points are returned in the order they will be interated through
    when a selection is read/written from/to disk.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Sget_select_elem_pointlist(hid_t spaceid, hsize_t startpoint, hsize_t numpoints, hsize_t *buf)
{
    H5S_t	*space = NULL;      /* Dataspace to modify selection of */
    herr_t ret_value=FAIL;        /* return value */

    FUNC_ENTER_API(H5Sget_select_elem_pointlist, FAIL);
    H5TRACE4("e","ihh*h",spaceid,startpoint,numpoints,buf);

    /* Check args */
    if(buf==NULL)
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid pointer");
    if (H5I_DATASPACE != H5I_get_type(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(space->select.type!=H5S_SEL_POINTS)
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a point selection");

    ret_value = H5S_get_select_elem_pointlist(space,startpoint,numpoints,buf);

    FUNC_LEAVE (ret_value);
}   /* H5Sget_select_elem_pointlist() */


/*--------------------------------------------------------------------------
 NAME
    H5S_get_select_bounds
 PURPOSE
    Gets the bounding box containing the selection.
 USAGE
    herr_t H5S_get_select_bounds(space, hsize_t *start, hsize_t *end)
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
        The bounding box calculations _does_ include the current offset of the
    selection within the dataspace extent.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_get_select_bounds(H5S_t *space, hsize_t *start, hsize_t *end)
{
    int rank;                  /* Dataspace rank */
    int i;                     /* index variable */
    herr_t ret_value=FAIL;   /* return value */

    FUNC_ENTER_NOINIT(H5S_get_select_bounds);

    assert(space);
    assert(start);
    assert(end);

    /* Set the start and end arrays up */
    rank=space->extent.u.simple.rank;
    for(i=0; i<rank; i++) {
        start[i]=UINT_MAX;
        end[i]=0;
    } /* end for */

    switch(space->select.type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
            ret_value=H5S_point_bounds(space,start,end);
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_bounds(space,start,end);
            break;

        case H5S_SEL_ALL:            /* Entire extent selected */
            ret_value=H5S_all_bounds(space,start,end);
            break;

        case H5S_SEL_NONE:           /* Nothing selected */
        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    }

    FUNC_LEAVE (ret_value);
}   /* H5S_get_select_bounds() */


/*--------------------------------------------------------------------------
 NAME
    H5Sget_select_bounds
 PURPOSE
    Gets the bounding box containing the selection.
 USAGE
    herr_t H5S_get_select_bounds(space, start, end)
        hid_t dsid;             IN: Dataspace ID of selection to query
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
        The bounding box calculations _does_ include the current offset of the
    selection within the dataspace extent.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Sget_select_bounds(hid_t spaceid, hsize_t *start, hsize_t *end)
{
    H5S_t	*space = NULL;      /* Dataspace to modify selection of */
    herr_t ret_value=FAIL;        /* return value */

    FUNC_ENTER_API(H5Sget_select_bounds, FAIL);
    H5TRACE3("e","i*h*h",spaceid,start,end);

    /* Check args */
    if(start==NULL || end==NULL)
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid pointer");
    if (H5I_DATASPACE != H5I_get_type(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }

    ret_value = H5S_get_select_bounds(space,start,end);

    FUNC_LEAVE (ret_value);
}   /* H5Sget_select_bounds() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_contiguous
 PURPOSE
    Check if the selection is contiguous within the dataspace extent.
 USAGE
    htri_t H5S_select_contiguous(space)
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
H5S_select_contiguous(const H5S_t *space)
{
    htri_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_select_contiguous, FAIL);

    assert(space);

    switch(space->select.type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
            ret_value=H5S_point_select_contiguous(space);
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_select_contiguous(space);
            break;

        case H5S_SEL_ALL:            /* Entire extent selected */
            ret_value=TRUE;
            break;

        case H5S_SEL_NONE:           /* Nothing selected */
            ret_value=FALSE;
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    }

    FUNC_LEAVE (ret_value);
}   /* H5S_select_contiguous() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_iter_init
 PURPOSE
    Construct an iterator for a dataspace & selection
 USAGE
    herr_t H5S_select_iter_init(space, elmt_size, iter)
        H5S_t *space;   IN: Dataspace object containing selection to iterate over
        size_t elmt_size;   IN: Size of element in dataspace
        H5S_sel_iter_t *iter;   OUT: Iterator to initialize
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Genericly initializes an iterator, based on the type of selection in the
    dataspace.
--------------------------------------------------------------------------*/
herr_t
H5S_select_iter_init(const H5S_t *space, size_t elmt_size, H5S_sel_iter_t *iter)
{
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOINIT(H5S_select_iter_init);

    assert(space);
    assert(iter);

    /* Initialize iterator */
    switch(space->select.type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
            ret_value=H5S_point_init(space,elmt_size,iter);
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_init(space,elmt_size,iter);
            break;

        case H5S_SEL_ALL:            /* Entire extent selected */
            ret_value=H5S_all_init(space,elmt_size,iter);
            break;

        case H5S_SEL_NONE:           /* Nothing selected */
            ret_value=FALSE;
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    }

    FUNC_LEAVE (ret_value);
} /* end H5S_select_iter_init() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_iterate
 PURPOSE
    Iterate over the selected elements in a memory buffer.
 USAGE
    herr_t H5S_select_iterate(buf, type_id, space, operator, operator_data)
        void *buf;      IN/OUT: Buffer containing elements to iterate over
        hid_t type_id;  IN: Datatype ID of BUF array.
        H5S_t *space;   IN: Dataspace object containing selection to iterate over
        H5D_operator_t op; IN: Function pointer to the routine to be
                                called for each element in BUF iterated over.
        void *operator_data;    IN/OUT: Pointer to any user-defined data
                                associated with the operation.
 RETURNS
    Returns the return value of the last operator if it was non-zero, or zero
    if all elements were processed. Otherwise returns a negative value.
 DESCRIPTION
    Iterates over the selected elements in a memory buffer, calling the user's
    callback function for each element.  The selection in the dataspace is
    modified so that any elements already iterated over are removed from the
    selection if the iteration is interrupted (by the H5D_operator_t function
    returning non-zero) in the "middle" of the iteration and may be re-started
    by the user where it left off.

    NOTE: Until "subtracting" elements from a selection is implemented,
        the selection is not modified.
--------------------------------------------------------------------------*/
herr_t
H5S_select_iterate(void *buf, hid_t type_id, H5S_t *space, H5D_operator_t op,
        void *operator_data)
{
    H5T_t *dt;                  /* Datatype structure */
    H5P_genplist_t *dx_plist;   /* Dataset transfer property list */
    H5S_sel_iter_t *iter=NULL;  /* Selection iteration info */
    uint8_t *loc;               /* Current element location in buffer */
    hssize_t coords[H5O_LAYOUT_NDIMS];  /* Coordinates of element in dataspace */
    hssize_t nelmts;            /* Number of elements in selection */
    hsize_t space_size[H5O_LAYOUT_NDIMS]; /* Dataspace size */
    hsize_t *off=NULL;          /* Array to store sequence offsets */
    hsize_t curr_off;           /* Current offset within sequence */
    hsize_t tmp_off;            /* Temporary offset within sequence */
    size_t *len=NULL;           /* Array to store sequence lengths */
    size_t curr_len;            /* Length of bytes left to process in sequence */
    size_t vector_size;         /* Value for vector size */
    size_t nseq;                /* Number of sequences generated */
    size_t curr_seq;            /* Current sequnce being worked on */
    size_t nbytes;              /* Number of bytes used in sequences */
    size_t max_bytes;           /* Maximum number of bytes allowed in sequences */
    size_t elmt_size;           /* Datatype size */
    int ndims;                  /* Number of dimensions in dataspace */
    int	i;			/* Local Index variable */
    herr_t user_ret=0;          /* User's return value */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5S_select_iterate, FAIL);

    /* Check args */
    assert(buf);
    assert(H5I_DATATYPE == H5I_get_type(type_id));
    assert(space);
    assert(op);

    /* Get the hyperslab vector size */
    /* (from the default data transfer property list, for now) */
    dx_plist = H5I_object(H5P_DATASET_XFER_DEFAULT);
    assert(dx_plist);
    if (H5P_get(dx_plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((len = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O length vector array");
    if((off = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O offset vector array");

    /* Get the datatype size */
    if (NULL==(dt=H5I_object(type_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an valid base datatype");
    if((elmt_size=H5T_get_size(dt))==0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, FAIL, "datatype size invalid");

    /* Allocate iterator */
    if((iter = H5FL_ALLOC(H5S_sel_iter_t,1))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate selection iterator");

    /* Initialize iterator */
    if (H5S_select_iter_init(space, elmt_size, iter)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator");

    /* Get the number of elements in selection */
    if((nelmts = H5S_get_select_npoints(space))<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't get number of elements selected");

    /* Get the rank of the dataspace */
    ndims=space->extent.u.simple.rank;

    /* Copy the size of the space */
    assert(space->extent.u.simple.size);
    assert(ndims>0);
    HDmemcpy(space_size, space->extent.u.simple.size, ndims*sizeof(hsize_t));
    space_size[ndims]=elmt_size;

    /* Compute the maximum number of bytes required */
    H5_ASSIGN_OVERFLOW(max_bytes,nelmts*elmt_size,hsize_t,size_t);

    /* Loop, while elements left in selection */
    while(max_bytes>0 && user_ret==0) {
        /* Get the sequences of bytes */
        if(H5S_select_get_seq_list(0,space,iter,elmt_size,vector_size,max_bytes,&nseq,&nbytes,off,len)<0)
            HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed");

        /* Loop, while sequences left to process */
        for(curr_seq=0; curr_seq<nseq && user_ret==0; curr_seq++) {
            /* Get the current offset */
            curr_off=off[curr_seq];

            /* Get the number of bytes in sequence */
            curr_len=len[curr_seq];

            /* Loop, while bytes left in sequence */
            while(curr_len>0 && user_ret==0) {
                /* Compute the coordinate from the offset */
                for(i=ndims, tmp_off=curr_off; i>=0; i--) {
                    coords[i]=tmp_off%space_size[i];
                    tmp_off/=space_size[i];
                } /* end for */

                /* Get the location within the user's buffer */
                loc=(unsigned char *)buf+curr_off;

                /* Call user's callback routine */
                user_ret=(*op)(loc,type_id,(hsize_t)ndims,coords,operator_data);

                /* Increment offset in dataspace */
                curr_off+=elmt_size;

                /* Decrement number of bytes left in sequence */
                curr_len-=elmt_size;
            } /* end while */
        } /* end for */

        /* Decrement number of elements left to process */
        assert((nbytes%elmt_size)==0);
        max_bytes-=nbytes;
    } /* end while */

    /* Set return value */
    ret_value=user_ret;

done:
    /* Release selection iterator */
    if(iter!=NULL) {
        if (H5S_sel_iter_release(space, iter)<0)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator");
	H5FL_FREE(H5S_sel_iter_t,iter);
    } /* end if */

    /* Release length & offset vectors */
    if(len!=NULL)
        H5FL_ARR_FREE(size_t,len);
    if(off!=NULL)
        H5FL_ARR_FREE(hsize_t,off);

    FUNC_LEAVE(ret_value);
}   /* end H5S_select_iterate() */


/*--------------------------------------------------------------------------
 NAME
    H5Sget_select_type
 PURPOSE
    Retrieve the type of selection in a dataspace
 USAGE
    H5S_sel_type H5Sget_select_type(space_id)
        hid_t space_id;	        IN: Dataspace object to reset
 RETURNS
    Non-negative on success/Negative on failure.  Return value is from the
    set of values in the H5S_sel_type enumerated type.
 DESCRIPTION
	This function retrieves the type of selection currently defined for
    a dataspace.
--------------------------------------------------------------------------*/
H5S_sel_type
H5Sget_select_type(hid_t space_id)
{
    H5S_t		   *space = NULL;	/* dataspace to modify */

    FUNC_ENTER_API(H5Sget_select_type, H5S_SEL_ERROR);
    H5TRACE1("St","i",space_id);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(space_id) || NULL == (space = H5I_object(space_id)))
        HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, H5S_SEL_ERROR, "not a data space");

    FUNC_LEAVE(space->select.type);
}   /* end H5Sget_select_type() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_single
 PURPOSE
    Check if the selection is a single block within the dataspace extent.
 USAGE
    htri_t H5S_select_single(space)
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
H5S_select_single(const H5S_t *space)
{
    htri_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_select_single, FAIL);

    /* Check args */
    assert(space);

    switch(space->select.type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
            ret_value=H5S_point_select_single(space);
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_select_single(space);
            break;

        case H5S_SEL_ALL:            /* Entire extent selected */
            ret_value=TRUE;
            break;

        case H5S_SEL_NONE:           /* Nothing selected */
            ret_value=FALSE;
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    }

    FUNC_LEAVE (ret_value);
}   /* H5S_select_single() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_shape_same
 PURPOSE
    Check if two selections are the same shape
 USAGE
    htri_t H5S_select_shape_same(space1, space2)
        const H5S_t *space1;         IN: 1st Dataspace pointer to compare
        const H5S_t *space2;         IN: 2nd Dataspace pointer to compare
 RETURNS
    TRUE/FALSE/FAIL
 DESCRIPTION
    Checks to see if the current selection in the dataspaces are the same
    dimensionality and shape.
    This is primarily used for reading the entire selection in one swoop.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Assumes that there is only a single "block" for hyperslab selections.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5S_select_shape_same(const H5S_t *space1, const H5S_t *space2)
{
    H5S_hyper_span_t *span1=NULL,*span2=NULL;   /* Hyperslab span node */
    hsize_t	elmts1,elmts2;                  /* Number of elements in each dimension of selection */
    unsigned	u;                              /* Index variable */
    htri_t ret_value=TRUE;  /* return value */

    FUNC_ENTER_NOAPI(H5S_select_shape_same, FAIL);

    /* Check args */
    assert(space1);
    assert(space2);

    if (space1->extent.u.simple.rank!=space2->extent.u.simple.rank)
        HGOTO_DONE(FALSE);

    /* Get information about memory and file */
    for (u=0; u<space1->extent.u.simple.rank; u++) {
        switch(space1->select.type) {
            case H5S_SEL_HYPERSLABS:
                /* Check size hyperslab selection in this dimension */
                if(space1->select.sel_info.hslab.diminfo != NULL) {
                    elmts1=space1->select.sel_info.hslab.diminfo[u].block;
                } /* end if */
                else {
                    /* Check for the first dimension */
                    if(span1==NULL)
                        span1=space1->select.sel_info.hslab.span_lst->head;

                    /* Get the number of elements in the span */
                    elmts1=(span1->high-span1->low)+1;

                    /* Advance to the next dimension */
                    span1=span1->down->head;
                } /* end else */
                break;

            case H5S_SEL_ALL:
                elmts1=space1->extent.u.simple.size[u];
                break;

            case H5S_SEL_POINTS:
                elmts1=1;
                break;

            default:
                assert(0 && "Invalid selection type!");
        } /* end switch */

        switch(space2->select.type) {
            case H5S_SEL_HYPERSLABS:
                /* Check size hyperslab selection in this dimension */
                if(space2->select.sel_info.hslab.diminfo != NULL) {
                    elmts2=space2->select.sel_info.hslab.diminfo[u].block;
                } /* end if */
                else {
                    /* Check for the first dimension */
                    if(span2==NULL)
                        span2=space2->select.sel_info.hslab.span_lst->head;

                    /* Get the number of elements in the span */
                    elmts2=(span2->high-span2->low)+1;

                    /* Advance to the next dimension */
                    span2=span2->down->head;
                } /* end else */
                break;

            case H5S_SEL_ALL:
                elmts2=space2->extent.u.simple.size[u];
                break;

            case H5S_SEL_POINTS:
                elmts2=1;
                break;

            default:
                assert(0 && "Invalid selection type!");
        } /* end switch */

        /* Make certaint the selections have the same number of elements in this dimension */
        if (elmts1!=elmts2)
            HGOTO_DONE(FALSE);
    } /* end for */

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_select_shape_same() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_regular
 PURPOSE
    Check if a selection is "regular"
 USAGE
    htri_t H5S_select_regular(space)
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
H5S_select_regular(const H5S_t *space)
{
    htri_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_select_regular, FAIL);

    /* Check args */
    assert(space);

    /* Check for a "regular" selection */
    /* [Defer (mostly) to the selection routines] */
    switch(space->select.type) {
        case H5S_SEL_POINTS:         /* Sequence of points selected */
            ret_value=H5S_point_select_regular(space);
            break;

        case H5S_SEL_HYPERSLABS:     /* Hyperslab selection defined */
            ret_value=H5S_hyper_select_regular(space);
            break;

        case H5S_SEL_ALL:            /* Entire extent selected */
            ret_value=TRUE;
            break;

        case H5S_SEL_NONE:           /* Nothing selected */
            ret_value=FALSE;
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    } /* end switch */

    FUNC_LEAVE (ret_value);
}   /* H5S_select_regular() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_fill
 PURPOSE
    Fill a selection in memory with a value
 USAGE
    herr_t H5S_select_fill(fill,fill_size,space,buf)
        const void *fill;       IN: Pointer to fill value to use
        size_t fill_size;       IN: Size of elements in memory buffer & size of
                                    fill value
        H5S_t *space;           IN: Dataspace describing memory buffer &
                                    containing selection to use.
        void *buf;              IN/OUT: Memory buffer to fill selection in
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Use the selection in the dataspace to fill elements in a memory buffer.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    The memory buffer elements are assumed to have the same datatype as the
    fill value being placed into them.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_select_fill(void *_fill, size_t fill_size, const H5S_t *space, void *_buf)
{
    H5P_genplist_t *dx_plist;   /* Dataset transfer property list */
    H5S_sel_iter_t *iter=NULL;  /* Selection iteration info */
    uint8_t *buf;               /* Current location in buffer */
    void *fill=_fill;           /* Alias for fill-value buffer */
    hssize_t nelmts;            /* Number of elements in selection */
    hsize_t *off=NULL;          /* Array to store sequence offsets */
    size_t *len=NULL;           /* Array to store sequence lengths */
    size_t vector_size;         /* Value for vector size */
    size_t nseq;                /* Number of sequences generated */
    size_t curr_seq;            /* Current sequnce being worked on */
    size_t nbytes;              /* Number of bytes used in sequences */
    size_t max_bytes;           /* Total number of bytes in selection */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_NOAPI(H5S_select_fill, FAIL);

    /* Check args */
    assert(fill_size>0);
    assert(space);
    assert(_buf);

    /* Check if we need a temporary fill value buffer */
    if(fill==NULL) {
        if (NULL==(fill = H5FL_BLK_ALLOC(type_elem,fill_size,1)))
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "fill value buffer allocation failed");
    } /* end if */

    /* Get the hyperslab vector size */
    /* (from the default data transfer property list, for now) */
    dx_plist = H5I_object(H5P_DATASET_XFER_DEFAULT);
    assert(dx_plist);
    if (H5P_get(dx_plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((len = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O length vector array");
    if((off = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O offset vector array");

    /* Allocate iterator */
    if((iter = H5FL_ALLOC(H5S_sel_iter_t,1))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate selection iterator");

    /* Initialize iterator */
    if (H5S_select_iter_init(space, fill_size, iter)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator");

    /* Get the number of elements in selection */
    if((nelmts = H5S_get_select_npoints(space))<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't get number of elements selected");

    /* Compute the number of bytes to process */
    H5_CHECK_OVERFLOW(nelmts,hssize_t,size_t);
    max_bytes=(size_t)nelmts*fill_size;

    /* Loop, while elements left in selection */
    while(max_bytes>0) {
        /* Get the sequences of bytes */
        if(H5S_select_get_seq_list(0,space,iter,fill_size,vector_size,max_bytes,&nseq,&nbytes,off,len)<0)
            HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed");

        /* Loop over sequences */
        for(curr_seq=0; curr_seq<nseq; curr_seq++) {
            /* Get offset in memory buffer */
            buf=(uint8_t *)_buf+off[curr_seq];

            /* Fill each sequence in memory with fill value */
            assert((len[curr_seq]%fill_size)==0);
            H5V_array_fill(buf, fill, fill_size, (len[curr_seq]/fill_size));
        } /* end for */

        /* Decrement number of bytes left to process */
        max_bytes-=nbytes;
    } /* end while */

done:
    /* Release selection iterator */
    if(iter!=NULL) {
        if (H5S_sel_iter_release(space, iter)<0)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator");
	H5FL_FREE(H5S_sel_iter_t,iter);
    } /* end if */

    /* Release length & offset vectors */
    if(len!=NULL)
        H5FL_ARR_FREE(size_t,len);
    if(off!=NULL)
        H5FL_ARR_FREE(hsize_t,off);

    /* Release fill value, if allocated */
    if(_fill==NULL && fill)
        H5FL_BLK_FREE(type_elem,fill);

    FUNC_LEAVE (ret_value);
}   /* H5S_select_fill() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_get_seq_list
 PURPOSE
    Create a list of offsets & lengths for a selection
 USAGE
    herr_t H5S_select_get_file_list(flags,space,iter,flag,elem_size,maxseq,maxbytes,nseq,off,len)
        unsigned flags;         IN: Flags for extra information about operation
        H5S_t *space;           IN: Dataspace containing selection to use.
        H5S_sel_iter_t *iter;   IN/OUT: Selection iterator describing last
                                    position of interest in selection.
        unsigned flag;          IN: Flag to indicate whether to update the
                                    iterator or not.
        size_t elem_size;       IN: Size of an element
        size_t maxseq;          IN: Maximum number of sequences to generate
        size_t maxbytes;        IN: Maximum number of bytes to include in the
                                    generated sequences
        size_t *nseq;           OUT: Actual number of sequences generated
        hsize_t *off;           OUT: Array of offsets
        hsize_t *len;           OUT: Array of lengths
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Use the selection in the dataspace to generate a list of byte offsets and
    lengths for the region(s) selected.  Start/Restart from the position in the
    ITER parameter.  Updating the iterator is controlled with the FLAG
    parameter.  The number of sequences generated is limited by the MAXSEQ
    parameter and the number of sequences actually generated is stored in the
    NSEQ parameter.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_select_get_seq_list(unsigned flags, const H5S_t *space,H5S_sel_iter_t *iter,
    size_t elem_size, size_t maxseq, size_t maxbytes, size_t *nseq, size_t *nbytes,
    hsize_t *off, size_t *len)
{
    herr_t ret_value=SUCCEED;      /* return value */

    FUNC_ENTER_NOAPI (H5S_select_get_seq_list, FAIL);

    /* Check args */
    assert(space);
    assert(iter);
    assert(elem_size>0);
    assert(maxseq>0);
    assert(maxbytes>0);
    assert(nseq);
    assert(off);
    assert(len);

    /* Get the list of sequences for each type selection */
    /* [Defer (mostly) to the selection routines] */
    switch(space->select.type) {
        case H5S_SEL_POINTS:        /* Sequence of points selected */
            ret_value=H5S_point_select_get_seq_list(flags,space,iter,elem_size,maxseq,maxbytes,nseq,nbytes,off,len);
            break;

        case H5S_SEL_HYPERSLABS:    /* Hyperslab selection defined */
            ret_value=H5S_hyper_select_get_seq_list(flags,space,iter,elem_size,maxseq,maxbytes,nseq,nbytes,off,len);
            break;

        case H5S_SEL_ALL:           /* Entire extent selected */
            ret_value=H5S_all_select_get_seq_list(flags,space,iter,elem_size,maxseq,maxbytes,nseq,nbytes,off,len);
            break;

        case H5S_SEL_NONE:          /* Nothing selected */
            *nseq=0;                /* Set the number of sequences generated */
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            assert(0 && "Invalid selection type!");
            break;
    } /* end switch */

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5S_select_get_seq_list() */


/*-------------------------------------------------------------------------
 * Function:	H5S_select_favail
 *
 * Purpose:	Figure out the optimal number of elements to transfer to/from
 *		the file.
 *
 * Return:	non-negative number of elements on success, zero on
 *		failure.
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, July 24, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5S_select_favail(const H5S_t *space, const H5S_sel_iter_t *iter, hsize_t max)
{
    hsize_t ret_value=0;      /* Return value */

    FUNC_ENTER_NOAPI (H5S_select_favail, 0);

    /* Check args */
    assert(space);
    assert(iter);
    assert(max>0);

    /* Get the number of elements to transfer for each type of selection */
    /* [Defer (mostly) to the selection routines] */
    switch(space->select.type) {
        case H5S_SEL_POINTS:        /* Sequence of points selected */
            ret_value=H5S_point_favail(space,iter,max);
            break;

        case H5S_SEL_HYPERSLABS:    /* Hyperslab selection defined */
            ret_value=H5S_hyper_favail(space,iter,max);
            break;

        case H5S_SEL_ALL:           /* Entire extent selected */
            ret_value=H5S_all_favail(space,iter,max);
            break;

        case H5S_SEL_NONE:          /* Nothing selected */
            ret_value=0;            /* Set the number of elements to transfer */
            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            assert(0 && "Invalid selection type!");
            break;
    } /* end switch */

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5S_select_favail() */


/*-------------------------------------------------------------------------
 * Function:	H5S_select_fscat
 *
 * Purpose:	Scatters dataset elements from the type conversion buffer BUF
 *		to the file F where the data points are arranged according to
 *		the file data space FILE_SPACE and stored according to
 *		LAYOUT and EFL. Each element is ELMT_SIZE bytes.
 *		The caller is requesting that NELMTS elements are copied.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, June 20, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_select_fscat (H5F_t *f, const struct H5O_layout_t *layout,
                 H5P_genplist_t *dc_plist, size_t elmt_size,
		 const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		 hsize_t nelmts, hid_t dxpl_id, const void *_buf)
{
    const uint8_t *buf=_buf;       /* Alias for pointer arithmetic */
    hsize_t *off=NULL;             /* Array to store sequence offsets */
    size_t *len=NULL;              /* Array to store sequence lengths */
    size_t vector_size;            /* Value for vector size */
    size_t maxbytes;               /* Number of bytes in the buffer */
    size_t  nseq;                  /* Number of sequences generated */
    size_t  nbytes;                /* Number of bytes used in sequences */
    H5P_genplist_t *dx_plist;      /* Dataset transfer property list */
    herr_t  ret_value=SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(H5S_select_fscat, FAIL);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (_buf);

    /* Get the hyperslab vector size */
    if(TRUE!=H5P_isa_class(dxpl_id,H5P_DATASET_XFER) || NULL == (dx_plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
    if (H5P_get(dx_plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((len = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O length vector array");
    if((off = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O offset vector array");

    /* Compute the number of bytes available in buffer */
    H5_ASSIGN_OVERFLOW(maxbytes,nelmts*elmt_size,hsize_t,size_t);

    /* Loop until all elements are written */
    while(maxbytes>0) {
        /* Get list of sequences for selection to write */
        if(H5S_select_get_seq_list(H5S_GET_SEQ_LIST_SORTED,file_space,file_iter,elmt_size,vector_size,maxbytes,&nseq,&nbytes,off,len)<0)
            HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed");

        /* Write sequence list out */
        if (H5F_seq_writev(f, dxpl_id, layout, dc_plist, file_space, elmt_size, nseq, len, off, buf)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error");

        /* Update buffer */
        buf += nbytes;

        /* Decrement number of elements left to process */
        assert(nbytes%elmt_size==0);
        maxbytes -= nbytes;
    } /* end while */

done:
    if(len!=NULL)
        H5FL_ARR_FREE(size_t,len);
    if(off!=NULL)
        H5FL_ARR_FREE(hsize_t,off);
    FUNC_LEAVE (ret_value);
} /* H5S_select_fscat() */


/*-------------------------------------------------------------------------
 * Function:	H5S_select_fgath
 *
 * Purpose:	Gathers data points from file F and accumulates them in the
 *		type conversion buffer BUF.  The LAYOUT argument describes
 *		how the data is stored on disk and EFL describes how the data
 *		is organized in external files.  ELMT_SIZE is the size in
 *		bytes of a datum which this function treats as opaque.
 *		FILE_SPACE describes the data space of the dataset on disk
 *		and the elements that have been selected for reading (via
 *		hyperslab, etc).  This function will copy at most NELMTS
 *		elements.
 *
 * Return:	Success:	Number of elements copied.
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Monday, June 24, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5S_select_fgath (H5F_t *f, const struct H5O_layout_t *layout,
               H5P_genplist_t *dc_plist,
	       size_t elmt_size, const H5S_t *file_space,
	       H5S_sel_iter_t *file_iter, hsize_t nelmts, hid_t dxpl_id,
	       void *_buf/*out*/)
{
    uint8_t *buf=_buf;          /* Alias for pointer arithmetic */
    hsize_t *off=NULL;          /* Array to store sequence offsets */
    size_t *len=NULL;           /* Array to store sequence lengths */
    size_t vector_size;         /* Value for vector size */
    size_t maxbytes;            /* Number of bytes in the buffer */
    size_t nseq;                /* Number of sequences generated */
    size_t nbytes;              /* Number of bytes used in sequences */
    H5P_genplist_t *dx_plist;   /* Dataset transfer property list */
    hsize_t ret_value=nelmts;   /* Return value */

    FUNC_ENTER_NOAPI(H5S_select_fgath, 0);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (_buf);

    /* Get the hyperslab vector size */
    if(TRUE!=H5P_isa_class(dxpl_id,H5P_DATASET_XFER) || NULL == (dx_plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a file access property list");
    if (H5P_get(dx_plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, 0, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((len = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate I/O length vector array");
    if((off = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate I/O offset vector array");

    /* Compute the number of bytes available in buffer */
    H5_ASSIGN_OVERFLOW(maxbytes,nelmts*elmt_size,hsize_t,size_t);

    /* Loop until all elements are written */
    while(maxbytes>0) {
        /* Get list of sequences for selection to write */
        if(H5S_select_get_seq_list(H5S_GET_SEQ_LIST_SORTED,file_space,file_iter,elmt_size,vector_size,maxbytes,&nseq,&nbytes,off,len)<0)
            HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, 0, "sequence length generation failed");

        /* Read sequence list in */
        if (H5F_seq_readv(f, dxpl_id, layout, dc_plist, file_space, elmt_size, nseq, len, off, buf)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, 0, "read error");

        /* Update buffer */
        buf += nbytes;

        /* Decrement number of elements left to process */
        assert(nbytes%elmt_size==0);
        maxbytes -= nbytes;
    } /* end while */

done:
    if(len!=NULL)
        H5FL_ARR_FREE(size_t,len);
    if(off!=NULL)
        H5FL_ARR_FREE(hsize_t,off);
    FUNC_LEAVE (ret_value);
} /* H5S_select_fgath() */


/*-------------------------------------------------------------------------
 * Function:	H5S_select_mscat
 *
 * Purpose:	Scatters NELMTS data points from the scatter buffer
 *		TSCAT_BUF to the application buffer BUF.  Each element is
 *		ELMT_SIZE bytes and they are organized in application memory
 *		according to SPACE.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 8, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_select_mscat (const void *_tscat_buf, size_t elmt_size, const H5S_t *space,
    H5S_sel_iter_t *iter, hsize_t nelmts, hid_t dxpl_id, void *_buf/*out*/)
{
    H5P_genplist_t *dx_plist;   /* Dataset transfer property list */
    uint8_t *buf=(uint8_t *)_buf;   /* Get local copies for address arithmetic */
    const uint8_t *tscat_buf=(const uint8_t *)_tscat_buf;
    hsize_t *off=NULL;          /* Array to store sequence offsets */
    size_t vector_size;         /* Value for vector size */
    size_t *len=NULL;           /* Array to store sequence lengths */
    size_t curr_len;            /* Length of bytes left to process in sequence */
    size_t maxbytes;            /* Number of bytes in the buffer */
    size_t nseq;                /* Number of sequences generated */
    size_t curr_seq;            /* Current sequence being processed */
    size_t nbytes;              /* Number of bytes used in sequences */
    herr_t ret_value=SUCCEED;   /* Number of elements scattered */

    FUNC_ENTER_NOAPI(H5S_select_mscat, FAIL);

    /* Check args */
    assert (tscat_buf);
    assert (elmt_size>0);
    assert (space);
    assert (iter);
    assert (nelmts>0);
    assert (buf);

    /* Get the hyperslab vector size */
    if(TRUE!=H5P_isa_class(dxpl_id,H5P_DATASET_XFER) || NULL == (dx_plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");
    if (H5P_get(dx_plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((len = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O length vector array");
    if((off = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O offset vector array");

    /* Compute the number of bytes available in buffer */
    H5_ASSIGN_OVERFLOW(maxbytes,nelmts*elmt_size,hsize_t,size_t);

    /* Loop until all elements are written */
    while(maxbytes>0) {
        /* Get list of sequences for selection to write */
        if(H5S_select_get_seq_list(0,space,iter,elmt_size,vector_size,maxbytes,&nseq,&nbytes,off,len)<0)
            HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, 0, "sequence length generation failed");

        /* Loop, while sequences left to process */
        for(curr_seq=0; curr_seq<nseq; curr_seq++) {
            /* Get the number of bytes in sequence */
            curr_len=len[curr_seq];

            HDmemcpy(buf+off[curr_seq],tscat_buf,curr_len);

            /* Advance offset in destination buffer */
            tscat_buf+=curr_len;
        } /* end for */

        /* Decrement number of elements left to process */
        assert(nbytes%elmt_size==0);
        maxbytes -= nbytes;
    } /* end while */

done:
    if(len!=NULL)
        H5FL_ARR_FREE(size_t,len);
    if(off!=NULL)
        H5FL_ARR_FREE(hsize_t,off);
    FUNC_LEAVE(ret_value);
}   /* H5S_select_mscat() */


/*-------------------------------------------------------------------------
 * Function:	H5S_select_mgath
 *
 * Purpose:	Gathers dataset elements from application memory BUF and
 *		copies them into the gather buffer TGATH_BUF.
 *		Each element is ELMT_SIZE bytes and arranged in application
 *		memory according to SPACE.  
 *		The caller is requesting that at most NELMTS be gathered.
 *
 * Return:	Success:	Number of elements copied.
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Monday, June 24, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5S_select_mgath (const void *_buf, size_t elmt_size, const H5S_t *space,
    H5S_sel_iter_t *iter, hsize_t nelmts, hid_t dxpl_id, void *_tgath_buf/*out*/)
{
    H5P_genplist_t *dx_plist;   /* Dataset transfer property list */
    const uint8_t *buf=(const uint8_t *)_buf;   /* Get local copies for address arithmetic */
    uint8_t *tgath_buf=(uint8_t *)_tgath_buf;
    hsize_t *off=NULL;          /* Array to store sequence offsets */
    size_t vector_size;         /* Value for vector size */
    size_t *len=NULL;           /* Array to store sequence lengths */
    size_t curr_len;            /* Length of bytes left to process in sequence */
    size_t maxbytes;            /* Number of bytes in the buffer */
    size_t nseq;                /* Number of sequences generated */
    size_t curr_seq;            /* Current sequence being processed */
    size_t nbytes;              /* Number of bytes used in sequences */
    hsize_t ret_value=nelmts;   /* Number of elements gathered */

    FUNC_ENTER_NOAPI(H5S_select_mgath, 0);

    /* Check args */
    assert (buf);
    assert (elmt_size>0);
    assert (space);
    assert (iter);
    assert (nelmts>0);
    assert (tgath_buf);

    /* Get the hyperslab vector size */
    if(TRUE!=H5P_isa_class(dxpl_id,H5P_DATASET_XFER) || NULL == (dx_plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a dataset transfer property list");
    if (H5P_get(dx_plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, 0, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((len = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate I/O length vector array");
    if((off = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate I/O offset vector array");

    /* Compute the number of bytes available in buffer */
    H5_ASSIGN_OVERFLOW(maxbytes,nelmts*elmt_size,hsize_t,size_t);

    /* Loop until all elements are written */
    while(maxbytes>0) {
        /* Get list of sequences for selection to write */
        if(H5S_select_get_seq_list(0,space,iter,elmt_size,vector_size,maxbytes,&nseq,&nbytes,off,len)<0)
            HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, 0, "sequence length generation failed");

        /* Loop, while sequences left to process */
        for(curr_seq=0; curr_seq<nseq; curr_seq++) {
            /* Get the number of bytes in sequence */
            curr_len=len[curr_seq];

            HDmemcpy(tgath_buf,buf+off[curr_seq],curr_len);

            /* Advance offset in gather buffer */
            tgath_buf+=curr_len;
        } /* end for */

        /* Decrement number of elements left to process */
        assert(nbytes%elmt_size==0);
        maxbytes -= nbytes;
    } /* end while */

done:
    if(len!=NULL)
        H5FL_ARR_FREE(size_t,len);
    if(off!=NULL)
        H5FL_ARR_FREE(hsize_t,off);
    FUNC_LEAVE(ret_value);
}   /* H5S_select_mgath() */


/*-------------------------------------------------------------------------
 * Function:	H5S_select_read
 *
 * Purpose:	Reads directly from file into application memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 23, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_select_read(H5F_t *f, const H5O_layout_t *layout, H5P_genplist_t *dc_plist,
            size_t elmt_size, const H5S_t *file_space,
	    const H5S_t *mem_space, hid_t dxpl_id, void *_buf/*out*/)
{
    H5P_genplist_t *dx_plist;   /* Dataset transfer property list */
    H5S_sel_iter_t *mem_iter=NULL;   /* Memory selection iteration info */
    H5S_sel_iter_t *file_iter=NULL;  /* File selection iteration info */
    uint8_t *buf;               /* Local buffer pointer, for address arithmetic */
    hsize_t *mem_off=NULL;      /* Array to store sequence offsets in memory */
    hsize_t *file_off=NULL;     /* Array to store sequence offsets in the file */
    size_t vector_size;         /* Value for vector size */
    size_t *mem_len=NULL;       /* Array to store sequence lengths in memory */
    size_t *file_len=NULL;      /* Array to store sequence lengths in the file */
    size_t maxbytes;            /* Number of bytes in selection */
    size_t mem_nseq;            /* Number of sequences generated in the file */
    size_t file_nseq;           /* Number of sequences generated in memory */
    size_t mem_nbytes;          /* Number of bytes used in memory sequences */
    size_t file_nbytes;         /* Number of bytes used in file sequences */
    size_t curr_mem_seq;        /* Current memory sequence to operate on */
    size_t curr_file_seq;       /* Current file sequence to operate on */
    size_t tmp_file_len;        /* Temporary number of bytes in file sequence */
    unsigned partial_file;      /* Whether a partial file sequence was accessed */
    size_t orig_file_len;       /* Original file sequence length for partial file access */
    size_t orig_file_seq;       /* Original file sequence to operate on */
    size_t tot_file_seq;        /* Number of file sequences to access */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5S_select_read, FAIL);

    /* Get the hyperslab vector size */
    if(TRUE!=H5P_isa_class(dxpl_id,H5P_DATASET_XFER) || NULL == (dx_plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");
    if (H5P_get(dx_plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((mem_len = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O length vector array");
    if((mem_off = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O offset vector array");
    if((file_len = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O length vector array");
    if((file_off = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O offset vector array");

    /* Allocate file iterator */
    if((file_iter = H5FL_ALLOC(H5S_sel_iter_t,1))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate selection iterator");

    /* Initialize file iterator */
    if (H5S_select_iter_init(file_space, elmt_size, file_iter)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator");

    /* Allocate memory iterator */
    if((mem_iter = H5FL_ALLOC(H5S_sel_iter_t,1))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate selection iterator");

    /* Initialize memory iterator */
    if (H5S_select_iter_init(mem_space, elmt_size, mem_iter)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator");

    /* Get number of bytes in selection */
    maxbytes=H5S_get_select_npoints(file_space)*elmt_size;

    /* Initialize sequence counts */
    curr_mem_seq=curr_file_seq=0;
    mem_nseq=file_nseq=0;

    /* Loop, until all bytes are processed */
    while(maxbytes>0) {
        /* Check if more file sequences are needed */
        if(curr_file_seq>=file_nseq) {
            /* Get sequences for file selection */
            if(H5S_select_get_seq_list(H5S_GET_SEQ_LIST_SORTED,file_space,file_iter,elmt_size,vector_size,maxbytes,&file_nseq,&file_nbytes,file_off,file_len)<0)
                HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed");

            /* Start at the beginning of the sequences again */
            curr_file_seq=0;
        } /* end if */

        /* Check if more memory sequences are needed */
        if(curr_mem_seq>=mem_nseq) {
            /* Get sequences for memory selection */
            if(H5S_select_get_seq_list(0,mem_space,mem_iter,elmt_size,vector_size,maxbytes,&mem_nseq,&mem_nbytes,mem_off,mem_len)<0)
                HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed");

            /* Start at the beginning of the sequences again */
            curr_mem_seq=0;

            /* Set the buffer pointer using the first sequence */
            H5_CHECK_OVERFLOW(mem_off[0],hsize_t,size_t);
            buf=(uint8_t *)_buf+(size_t)mem_off[0];
        } /* end if */

        /* Check if current file sequence will fit into current memory sequence */
        if(mem_len[curr_mem_seq]>=file_len[curr_file_seq]) {
            /* Save the current number file sequence */
            orig_file_seq=curr_file_seq;

            /* Determine how many file sequences will fit into current memory sequence */
            tmp_file_len=0;
            tot_file_seq=0;
            while((tmp_file_len+file_len[curr_file_seq])<=mem_len[curr_mem_seq] && curr_file_seq<file_nseq) {
                tmp_file_len+=file_len[curr_file_seq];
                curr_file_seq++;
                tot_file_seq++;
            } /* end while */

            /* Check for partial file sequence */
            if(tmp_file_len<mem_len[curr_mem_seq] && curr_file_seq<file_nseq) {
                /* Get the original file sequence length */
                orig_file_len=file_len[curr_file_seq];

                /* Make the last file sequence a partial access */
                file_len[curr_file_seq]=mem_len[curr_mem_seq]-tmp_file_len;

                /* Increase the number of bytes to access */
                tmp_file_len=mem_len[curr_mem_seq];

                /* Indicate that there is an extra sequence to include in the file access */
                tot_file_seq++;

                /* Indicate a partial file sequence */
                partial_file=1;
            } /* end if */
            else
                partial_file=0;

            /* Read file sequences into current memory sequence */
            if (H5F_seq_readv(f, dxpl_id, layout, dc_plist, file_space, elmt_size, tot_file_seq, &file_len[orig_file_seq], &file_off[orig_file_seq], buf)<0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, FAIL, "read error");

            /* Update last file sequence, if it was partially accessed */
            if(partial_file) {
                file_off[curr_file_seq]+=orig_file_len-file_len[curr_file_seq];
                file_len[curr_file_seq]=orig_file_len-file_len[curr_file_seq];
            } /* end if */

            /* Check if the current memory sequence was only partially accessed */
            if(tmp_file_len<mem_len[curr_mem_seq]) {
                /* Adjust current memory sequence */
                mem_off[curr_mem_seq]+=tmp_file_len;
                mem_len[curr_mem_seq]-=tmp_file_len;

                /* Adjust memory buffer pointer */
                buf+=tmp_file_len;
            } /* end if */
            else {
                /* Must have used entire memory sequence, advance to next one */
                curr_mem_seq++;

                /* Check if it is valid to adjust buffer pointer */
                if(curr_mem_seq<mem_nseq) {
                    H5_CHECK_OVERFLOW(mem_off[curr_mem_seq],hsize_t,size_t);
                    buf=(uint8_t *)_buf+(size_t)mem_off[curr_mem_seq];
                } /* end if */
            } /* end else */

            /* Decrement number of bytes left to process */
            maxbytes-=tmp_file_len;
        } /* end if */
        else {
            /* Save number of bytes to access */
            tmp_file_len=mem_len[curr_mem_seq];

            /* Read part of current file sequence into current memory sequence */
            if (H5F_seq_read(f, dxpl_id, layout, dc_plist, file_space, elmt_size, tmp_file_len, file_off[curr_file_seq], buf)<0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, FAIL, "read error");

            /* Update current file sequence information */
            file_off[curr_file_seq]+=tmp_file_len;
            file_len[curr_file_seq]-=tmp_file_len;

            /* Increment memory sequence */
            curr_mem_seq++;

            /* Check if it is valid to adjust buffer pointer */
            if(curr_mem_seq<mem_nseq) {
                H5_CHECK_OVERFLOW(mem_off[curr_mem_seq],hsize_t,size_t);
                buf=(uint8_t *)_buf+(size_t)mem_off[curr_mem_seq];
            } /* end if */

            /* Decrement number of bytes left to process */
            maxbytes-=tmp_file_len;
        } /* end else */
    } /* end while */

done:
    /* Release file selection iterator */
    if(file_iter!=NULL) {
        if (H5S_sel_iter_release(file_space, file_iter)<0)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator");
	H5FL_FREE(H5S_sel_iter_t,file_iter);
    } /* end if */

    /* Release memory selection iterator */
    if(mem_iter!=NULL) {
        if (H5S_sel_iter_release(mem_space, mem_iter)<0)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator");
	H5FL_FREE(H5S_sel_iter_t,mem_iter);
    } /* end if */

    /* Free vector arrays */
    if(file_len!=NULL)
        H5FL_ARR_FREE(size_t,file_len);
    if(file_off!=NULL)
        H5FL_ARR_FREE(hsize_t,file_off);
    if(mem_len!=NULL)
        H5FL_ARR_FREE(size_t,mem_len);
    if(mem_off!=NULL)
        H5FL_ARR_FREE(hsize_t,mem_off);
    FUNC_LEAVE(ret_value);
} /* end H5S_select_read() */


/*-------------------------------------------------------------------------
 * Function:	H5S_select_write
 *
 * Purpose:	Writes directly from application memory into a file
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 23, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_select_write(H5F_t *f, const H5O_layout_t *layout, H5P_genplist_t *dc_plist,
            size_t elmt_size, const H5S_t *file_space,
	    const H5S_t *mem_space, hid_t dxpl_id, const void *_buf/*out*/)
{
    H5P_genplist_t *dx_plist;   /* Dataset transfer property list */
    H5S_sel_iter_t *mem_iter=NULL;   /* Memory selection iteration info */
    H5S_sel_iter_t *file_iter=NULL;  /* File selection iteration info */
    const uint8_t *buf;         /* Local buffer pointer, for address arithmetic */
    hsize_t *mem_off=NULL;      /* Array to store sequence offsets in memory */
    hsize_t *file_off=NULL;     /* Array to store sequence offsets in the file */
    size_t vector_size;         /* Value for vector size */
    size_t *mem_len=NULL;       /* Array to store sequence lengths in memory */
    size_t *file_len=NULL;      /* Array to store sequence lengths in the file */
    size_t maxbytes;            /* Number of bytes in selection */
    size_t mem_nseq;            /* Number of sequences generated in the file */
    size_t file_nseq;           /* Number of sequences generated in memory */
    size_t mem_nbytes;          /* Number of bytes used in memory sequences */
    size_t file_nbytes;         /* Number of bytes used in file sequences */
    size_t curr_mem_seq;        /* Current memory sequence to operate on */
    size_t curr_file_seq;       /* Current file sequence to operate on */
    size_t tmp_file_len;        /* Temporary number of bytes in file sequence */
    unsigned partial_file;      /* Whether a partial file sequence was accessed */
    size_t orig_file_len;       /* Original file sequence length for partial file access */
    size_t orig_file_seq;       /* Original file sequence to operate on */
    size_t tot_file_seq;        /* Number of file sequences to access */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5S_select_write, FAIL);

    /* Get the hyperslab vector size */
    if(TRUE!=H5P_isa_class(dxpl_id,H5P_DATASET_XFER) || NULL == (dx_plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");
    if (H5P_get(dx_plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((mem_len = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O length vector array");
    if((mem_off = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O offset vector array");
    if((file_len = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O length vector array");
    if((file_off = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O offset vector array");

    /* Allocate file iterator */
    if((file_iter = H5FL_ALLOC(H5S_sel_iter_t,1))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate selection iterator");

    /* Initialize file iterator */
    if (H5S_select_iter_init(file_space, elmt_size, file_iter)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator");

    /* Allocate memory iterator */
    if((mem_iter = H5FL_ALLOC(H5S_sel_iter_t,1))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate selection iterator");

    /* Initialize memory iterator */
    if (H5S_select_iter_init(mem_space, elmt_size, mem_iter)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator");

    /* Get number of bytes in selection */
    maxbytes=H5S_get_select_npoints(file_space)*elmt_size;

    /* Initialize sequence counts */
    curr_mem_seq=curr_file_seq=0;
    mem_nseq=file_nseq=0;

    /* Loop, until all bytes are processed */
    while(maxbytes>0) {
        /* Check if more file sequences are needed */
        if(curr_file_seq>=file_nseq) {
            /* Get sequences for file selection */
            if(H5S_select_get_seq_list(H5S_GET_SEQ_LIST_SORTED,file_space,file_iter,elmt_size,vector_size,maxbytes,&file_nseq,&file_nbytes,file_off,file_len)<0)
                HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed");

            /* Start at the beginning of the sequences again */
            curr_file_seq=0;
        } /* end if */

        /* Check if more memory sequences are needed */
        if(curr_mem_seq>=mem_nseq) {
            /* Get sequences for memory selection */
            if(H5S_select_get_seq_list(0,mem_space,mem_iter,elmt_size,vector_size,maxbytes,&mem_nseq,&mem_nbytes,mem_off,mem_len)<0)
                HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed");

            /* Start at the beginning of the sequences again */
            curr_mem_seq=0;

            /* Set the buffer pointer using the first sequence */
            H5_CHECK_OVERFLOW(mem_off[0],hsize_t,size_t);
            buf=(const uint8_t *)_buf+(size_t)mem_off[0];
        } /* end if */

        /* Check if current file sequence will fit into current memory sequence */
        if(mem_len[curr_mem_seq]>=file_len[curr_file_seq]) {
            /* Save the current number file sequence */
            orig_file_seq=curr_file_seq;

            /* Determine how many file sequences will fit into current memory sequence */
            tmp_file_len=0;
            tot_file_seq=0;
            while((tmp_file_len+file_len[curr_file_seq])<=mem_len[curr_mem_seq] && curr_file_seq<file_nseq) {
                tmp_file_len+=file_len[curr_file_seq];
                curr_file_seq++;
                tot_file_seq++;
            } /* end while */

            /* Check for partial file sequence */
            if(tmp_file_len<mem_len[curr_mem_seq] && curr_file_seq<file_nseq) {
                /* Get the original file sequence length */
                orig_file_len=file_len[curr_file_seq];

                /* Make the last file sequence a partial access */
                file_len[curr_file_seq]=mem_len[curr_mem_seq]-tmp_file_len;

                /* Increase the number of bytes to access */
                tmp_file_len=mem_len[curr_mem_seq];

                /* Indicate that there is an extra sequence to include in the file access */
                tot_file_seq++;

                /* Indicate a partial file sequence */
                partial_file=1;
            } /* end if */
            else
                partial_file=0;

            /* Write current memory sequence into file sequences */
            if (H5F_seq_writev(f, dxpl_id, layout, dc_plist, file_space, elmt_size, tot_file_seq, &file_len[orig_file_seq], &file_off[orig_file_seq], buf)<0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error");

            /* Update last file sequence, if it was partially accessed */
            if(partial_file) {
                file_off[curr_file_seq]+=orig_file_len-file_len[curr_file_seq];
                file_len[curr_file_seq]=orig_file_len-file_len[curr_file_seq];
            } /* end if */

            /* Check if the current memory sequence was only partially accessed */
            if(tmp_file_len<mem_len[curr_mem_seq]) {
                /* Adjust current memory sequence */
                mem_off[curr_mem_seq]+=tmp_file_len;
                mem_len[curr_mem_seq]-=tmp_file_len;

                /* Adjust memory buffer pointer */
                buf+=tmp_file_len;
            } /* end if */
            else {
                /* Must have used entire memory sequence, advance to next one */
                curr_mem_seq++;

                /* Check if it is valid to adjust buffer pointer */
                if(curr_mem_seq<mem_nseq) {
                    H5_CHECK_OVERFLOW(mem_off[curr_mem_seq],hsize_t,size_t);
                    buf=(const uint8_t *)_buf+(size_t)mem_off[curr_mem_seq];
                } /* end if */
            } /* end else */

            /* Decrement number of bytes left to process */
            maxbytes-=tmp_file_len;
        } /* end if */
        else {
            /* Save number of bytes to access */
            tmp_file_len=mem_len[curr_mem_seq];

            /* Write part of current memory sequence to current file sequence */
            if (H5F_seq_write(f, dxpl_id, layout, dc_plist, file_space, elmt_size, tmp_file_len, file_off[curr_file_seq], buf)<0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error");

            /* Update current file sequence information */
            file_off[curr_file_seq]+=tmp_file_len;
            file_len[curr_file_seq]-=tmp_file_len;

            /* Increment memory sequence */
            curr_mem_seq++;

            /* Check if it is valid to adjust buffer pointer */
            if(curr_mem_seq<mem_nseq) {
                H5_CHECK_OVERFLOW(mem_off[curr_mem_seq],hsize_t,size_t);
                buf=(const uint8_t *)_buf+(size_t)mem_off[curr_mem_seq];
            } /* end if */

            /* Decrement number of bytes left to process */
            maxbytes-=tmp_file_len;
        } /* end else */
    } /* end while */

done:
    /* Release file selection iterator */
    if(file_iter!=NULL) {
        if (H5S_sel_iter_release(file_space, file_iter)<0)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator");
	H5FL_FREE(H5S_sel_iter_t,file_iter);
    } /* end if */

    /* Release memory selection iterator */
    if(mem_iter!=NULL) {
        if (H5S_sel_iter_release(mem_space, mem_iter)<0)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator");
	H5FL_FREE(H5S_sel_iter_t,mem_iter);
    } /* end if */

    /* Free vector arrays */
    if(file_len!=NULL)
        H5FL_ARR_FREE(size_t,file_len);
    if(file_off!=NULL)
        H5FL_ARR_FREE(hsize_t,file_off);
    if(mem_len!=NULL)
        H5FL_ARR_FREE(size_t,mem_len);
    if(mem_off!=NULL)
        H5FL_ARR_FREE(hsize_t,mem_off);
    FUNC_LEAVE(ret_value);
} /* end H5S_select_write() */

