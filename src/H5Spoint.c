/*
 * Copyright (C) 1998-2001 NCSA
 *                         All rights reserved.
 *
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, June 16, 1998
 *
 * Purpose:	Point selection data space I/O functions.
 */

#define H5S_PACKAGE		/*suppress error about including H5Spkg	  */

#include "H5private.h"		/* Generic Functions			  */
#include "H5Eprivate.h"		/* Error handling		  */
#include "H5FLprivate.h"	/* Free Lists	  */
#include "H5Iprivate.h"		/* ID Functions		  */
#include "H5MMprivate.h"	/* Memory Management functions		  */
#include "H5Spkg.h"		/* Dataspace functions			  */
#include "H5Vprivate.h"         /* Vector functions */

/* Interface initialization */
#define PABLO_MASK      H5Spoint_mask
#define INTERFACE_INIT  NULL
static int             interface_initialize_g = 0;

/* Declare a free list to manage the H5S_pnt_node_t struct */
H5FL_DEFINE_STATIC(H5S_pnt_node_t);

/* Declare a free list to manage the H5S_pnt_list_t struct */
H5FL_DEFINE_STATIC(H5S_pnt_list_t);


/*-------------------------------------------------------------------------
 * Function:	H5S_point_init
 *
 * Purpose:	Initializes iteration information for point selection.
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
H5S_point_init (const H5S_t *space, size_t UNUSED elmt_size, H5S_sel_iter_t *sel_iter)
{
    FUNC_ENTER_NOAPI(H5S_point_init, FAIL);

    /* Check args */
    assert (space && H5S_SEL_POINTS==space->select.type);
    assert (sel_iter);

    /* Initialize the number of points to iterate over */
    sel_iter->pnt.elmt_left=space->select.num_elem;

    /* Start at the head of the list of points */
    sel_iter->pnt.curr=space->select.sel_info.pnt_lst->head;
    
    FUNC_LEAVE (SUCCEED);
}


/*--------------------------------------------------------------------------
 NAME
    H5S_point_add
 PURPOSE
    Add a series of elements to a point selection
 USAGE
    herr_t H5S_point_add(space, num_elem, coord)
        H5S_t *space;           IN: Dataspace of selection to modify
        size_t num_elem;        IN: Number of elements in COORD array.
        const hssize_t *coord[];    IN: The location of each element selected
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function adds elements to the current point selection for a dataspace
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5S_point_add (H5S_t *space, H5S_seloper_t op, size_t num_elem, const hssize_t **_coord)
{
    H5S_pnt_node_t *top, *curr, *new_node; /* Point selection nodes */
    const hssize_t *coord=(const hssize_t *)_coord;     /* Pointer to the actual coordinates */
    unsigned i;                 /* Counter */
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_point_add, FAIL);

    assert(space);
    assert(num_elem>0);
    assert(coord);
    assert(op==H5S_SELECT_SET || op==H5S_SELECT_APPEND || op==H5S_SELECT_PREPEND);

    top=curr=NULL;
    for(i=0; i<num_elem; i++) {
        /* Allocate space for the new node */
        if((new_node = H5FL_ALLOC(H5S_pnt_node_t,0))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate point node");

        if((new_node->pnt = H5MM_malloc(space->extent.u.simple.rank*sizeof(hssize_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate coordinate information");

        /* Copy over the coordinates */
        HDmemcpy(new_node->pnt,coord+(i*space->extent.u.simple.rank),(space->extent.u.simple.rank*sizeof(hssize_t)));

        /* Link into list */
        new_node->next=NULL;
        if(top==NULL)
            top=new_node;
        else
            curr->next=new_node;
        curr=new_node;
    } /* end for */

    /* Insert the list of points selected in the proper place */
    if(op==H5S_SELECT_SET || op==H5S_SELECT_PREPEND) {
        /* Append current list, if there is one */
        if(space->select.sel_info.pnt_lst->head!=NULL)
            curr->next=space->select.sel_info.pnt_lst->head;

        /* Put new list in point selection */
        space->select.sel_info.pnt_lst->head=top;
    } /* end if */
    else {  /* op==H5S_SELECT_APPEND */
        new_node=space->select.sel_info.pnt_lst->head;
        if(new_node!=NULL) {
            while(new_node->next!=NULL)
                new_node=new_node->next;

            /* Append new list to point selection */
            new_node->next=top;
        } /* end if */
        else 
            space->select.sel_info.pnt_lst->head=top;
    } /* end else */

    /* Add the number of elements in the new selection */
    space->select.num_elem+=num_elem;

    ret_value=SUCCEED;
    
done:
    FUNC_LEAVE (ret_value);
}   /* H5S_point_add() */

/*-------------------------------------------------------------------------
 * Function:	H5S_point_favail
 *
 * Purpose:	Figure out the optimal number of elements to transfer to/from the file
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
H5S_point_favail (const H5S_t * UNUSED space,
		  const H5S_sel_iter_t *sel_iter, hsize_t max)
{
    FUNC_ENTER_NOAPI(H5S_point_favail, 0);

    /* Check args */
    assert (space && H5S_SEL_POINTS==space->select.type);
    assert (sel_iter);

    FUNC_LEAVE (MIN(sel_iter->pnt.elmt_left,max));
}   /* H5S_point_favail() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_release
 PURPOSE
    Release point selection information for a dataspace
 USAGE
    herr_t H5S_point_release(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Releases all point selection information for a dataspace
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_point_release (H5S_t *space)
{
    H5S_pnt_node_t *curr, *next;        /* Point selection nodes */

    FUNC_ENTER_NOAPI(H5S_point_release, FAIL);

    /* Check args */
    assert (space);

    /* Delete all the nodes from the list */
    curr=space->select.sel_info.pnt_lst->head;
    while(curr!=NULL) {
        next=curr->next;
        H5MM_xfree(curr->pnt);
        H5FL_FREE(H5S_pnt_node_t,curr);
        curr=next;
    } /* end while */
    
    /* Free & reset the point list header */
    H5FL_FREE(H5S_pnt_list_t,space->select.sel_info.pnt_lst);
    space->select.sel_info.pnt_lst=NULL;

    /* Reset the number of elements in the selection */
    space->select.num_elem=0;
    
    FUNC_LEAVE (SUCCEED);
}   /* H5S_point_release() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_npoints
 PURPOSE
    Compute number of elements in current selection
 USAGE
    hsize_t H5S_point_npoints(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    The number of elements in selection on success, 0 on failure
 DESCRIPTION
    Compute number of elements in current selection.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hsize_t
H5S_point_npoints (const H5S_t *space)
{
    FUNC_ENTER_NOAPI(H5S_point_npoints, 0);

    /* Check args */
    assert (space);

    FUNC_LEAVE (space->select.num_elem);
}   /* H5S_point_npoints() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_copy
 PURPOSE
    Copy a selection from one dataspace to another
 USAGE
    herr_t H5S_point_copy(dst, src)
        H5S_t *dst;  OUT: Pointer to the destination dataspace
        H5S_t *src;  IN: Pointer to the source dataspace
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Copies all the point selection information from the source
    dataspace to the destination dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_point_copy (H5S_t *dst, const H5S_t *src)
{
    H5S_pnt_node_t *curr, *new_node, *new_head;    /* Point information nodes */
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER_NOAPI(H5S_point_copy, FAIL);

    assert(src);
    assert(dst);

    /* Allocate room for the head of the point list */
    if((dst->select.sel_info.pnt_lst=H5FL_ALLOC(H5S_pnt_list_t,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
            "can't allocate point node");

    curr=src->select.sel_info.pnt_lst->head;
    new_head=NULL;
    while(curr!=NULL) {
        /* Create each point */
        if((new_node=H5FL_ALLOC(H5S_pnt_node_t,0))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate point node");
        if((new_node->pnt = H5MM_malloc(src->extent.u.simple.rank*sizeof(hssize_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate coordinate information");
        HDmemcpy(new_node->pnt,curr->pnt,(src->extent.u.simple.rank*sizeof(hssize_t)));
        new_node->next=NULL;

        /* Keep the order the same when copying */
        if(new_head==NULL)
            new_head=dst->select.sel_info.pnt_lst->head=new_node;
        else {
            new_head->next=new_node;
            new_head=new_node;
        } /* end else */

        curr=curr->next;
    } /* end while */

done:
    FUNC_LEAVE (ret_value);
} /* end H5S_point_copy() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_select_valid
 PURPOSE
    Check whether the selection fits within the extent, with the current
    offset defined.
 USAGE
    htri_t H5S_point_select_valid(space);
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
H5S_point_select_valid (const H5S_t *space)
{
    H5S_pnt_node_t *curr;      /* Point information nodes */
    unsigned u;                   /* Counter */
    htri_t ret_value=TRUE;     /* return value */

    FUNC_ENTER_NOAPI(H5S_point_select_valid, FAIL);

    assert(space);

    /* Check each point to determine whether selection+offset is within extent */
    curr=space->select.sel_info.pnt_lst->head;
    while(curr!=NULL) {
        /* Check each dimension */
        for(u=0; u<space->extent.u.simple.rank; u++) {
            /* Check if an offset has been defined */
            /* Bounds check the selected point + offset against the extent */
            if(((curr->pnt[u]+space->select.offset[u])>(hssize_t)space->extent.u.simple.size[u])
                    || ((curr->pnt[u]+space->select.offset[u])<0)) {
                ret_value=FALSE;
                break;
            } /* end if */
        } /* end for */

        curr=curr->next;
    } /* end while */

    FUNC_LEAVE (ret_value);
} /* end H5S_point_select_valid() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_select_serial_size
 PURPOSE
    Determine the number of bytes needed to store the serialized point selection
    information.
 USAGE
    hssize_t H5S_point_select_serial_size(space)
        H5S_t *space;             IN: Dataspace pointer to query
 RETURNS
    The number of bytes required on success, negative on an error.
 DESCRIPTION
    Determines the number of bytes required to serialize the current point
    selection information for storage on disk.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hssize_t
H5S_point_select_serial_size (const H5S_t *space)
{
    H5S_pnt_node_t *curr;       /* Point information nodes */
    hssize_t ret_value=FAIL;    /* return value */

    FUNC_ENTER_NOAPI(H5S_point_select_serial_size, FAIL);

    assert(space);

    /* Basic number of bytes required to serialize point selection:
     *  <type (4 bytes)> + <version (4 bytes)> + <padding (4 bytes)> + 
     *      <length (4 bytes)> + <rank (4 bytes)> + <# of points (4 bytes)> = 24 bytes
     */
    ret_value=24;

    /* Count points in selection */
    curr=space->select.sel_info.pnt_lst->head;
    while(curr!=NULL) {
        /* Add 4 bytes times the rank for each element selected */
        ret_value+=4*space->extent.u.simple.rank;
        curr=curr->next;
    } /* end while */

    FUNC_LEAVE (ret_value);
} /* end H5S_point_select_serial_size() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_select_serialize
 PURPOSE
    Serialize the current selection into a user-provided buffer.
 USAGE
    herr_t H5S_point_select_serialize(space, buf)
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
H5S_point_select_serialize (const H5S_t *space, uint8_t *buf)
{
    H5S_pnt_node_t *curr;   /* Point information nodes */
    uint8_t *lenp;          /* pointer to length location for later storage */
    uint32_t len=0;         /* number of bytes used */
    unsigned u;                /* local counting variable */
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_point_select_serialize, FAIL);

    assert(space);

    /* Store the preamble information */
    UINT32ENCODE(buf, (uint32_t)space->select.type);  /* Store the type of selection */
    UINT32ENCODE(buf, (uint32_t)1);  /* Store the version number */
    UINT32ENCODE(buf, (uint32_t)0);  /* Store the un-used padding */
    lenp=buf;           /* keep the pointer to the length location for later */
    buf+=4;             /* skip over space for length */

    /* Encode number of dimensions */
    UINT32ENCODE(buf, (uint32_t)space->extent.u.simple.rank);
    len+=4;

    /* Encode number of elements */
    UINT32ENCODE(buf, (uint32_t)space->select.num_elem);
    len+=4;

    /* Encode each point in selection */
    curr=space->select.sel_info.pnt_lst->head;
    while(curr!=NULL) {
        /* Add 4 bytes times the rank for each element selected */
        len+=4*space->extent.u.simple.rank;

        /* Encode each point */
        for(u=0; u<space->extent.u.simple.rank; u++)
            UINT32ENCODE(buf, (uint32_t)curr->pnt[u]);

        curr=curr->next;
    } /* end while */

    /* Encode length */
    UINT32ENCODE(lenp, (uint32_t)len);  /* Store the length of the extra information */
    
    /* Set success */
    ret_value=SUCCEED;

    FUNC_LEAVE (ret_value);
}   /* H5S_point_select_serialize() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_select_deserialize
 PURPOSE
    Deserialize the current selection from a user-provided buffer.
 USAGE
    herr_t H5S_point_select_deserialize(space, buf)
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
H5S_point_select_deserialize (H5S_t *space, const uint8_t *buf)
{
    H5S_seloper_t op=H5S_SELECT_SET;    /* Selection operation */
    uint32_t rank;           /* Rank of points */
    size_t num_elem=0;      /* Number of elements in selection */
    hssize_t *coord=NULL, *tcoord;   /* Pointer to array of elements */
    unsigned i,j;              /* local counting variables */
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_point_select_deserialize, FAIL);

    /* Check args */
    assert(space);
    assert(buf);

    /* Deserialize points to select */
    buf+=16;    /* Skip over selection header */
    UINT32DECODE(buf,rank);  /* decode the rank of the point selection */
    if(rank!=space->extent.u.simple.rank)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "rank of pointer does not match dataspace");
    UINT32DECODE(buf,num_elem);  /* decode the number of points */

    /* Allocate space for the coordinates */
    if((coord = H5MM_malloc(num_elem*rank*sizeof(hssize_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate coordinate information");
    
    /* Retrieve the coordinates from the buffer */
    for(tcoord=coord,i=0; i<num_elem; i++)
        for(j=0; j<(unsigned)rank; j++,tcoord++)
            UINT32DECODE(buf, *tcoord);

    /* Select points */
    if((ret_value=H5S_select_elements(space,op,num_elem,(const hssize_t **)coord))<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection");
    } /* end if */

done:
    /* Free the coordinate array if necessary */
    if(coord!=NULL)
        H5MM_xfree(coord);

    FUNC_LEAVE (ret_value);
}   /* H5S_point_select_deserialize() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_bounds
 PURPOSE
    Gets the bounding box containing the selection.
 USAGE
    herr_t H5S_point_bounds(space, hsize_t *start, hsize_t *end)
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
    with be (4, 5), (10, 8).
        The bounding box calculations _does_ include the current offset of the
    selection within the dataspace extent.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_point_bounds(H5S_t *space, hsize_t *start, hsize_t *end)
{
    H5S_pnt_node_t *node;       /* Point node */
    int rank;                  /* Dataspace rank */
    int i;                     /* index variable */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_NOAPI(H5S_point_bounds, FAIL);

    assert(space);
    assert(start);
    assert(end);

    /* Get the dataspace extent rank */
    rank=space->extent.u.simple.rank;

    /* Iterate through the node, checking the bounds on each element */
    node=space->select.sel_info.pnt_lst->head;
    while(node!=NULL) {
        for(i=0; i<rank; i++) {
            if(start[i]>(hsize_t)(node->pnt[i]+space->select.offset[i]))
                start[i]=node->pnt[i]+space->select.offset[i];
            if(end[i]<(hsize_t)(node->pnt[i]+space->select.offset[i]))
                end[i]=node->pnt[i]+space->select.offset[i];
        } /* end for */
        node=node->next;
      } /* end while */

    FUNC_LEAVE (ret_value);
}   /* H5Sget_point_bounds() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_select_contiguous
 PURPOSE
    Check if a point selection is contiguous within the dataspace extent.
 USAGE
    htri_t H5S_point_select_contiguous(space)
        H5S_t *space;           IN: Dataspace pointer to check
 RETURNS
    TRUE/FALSE/FAIL
 DESCRIPTION
    Checks to see if the current selection in the dataspace is contiguous.
    This is primarily used for reading the entire selection in one swoop.
    This code currently doesn't properly check for contiguousness when there is
    more than one point, as that would take a lot of extra coding that we
    don't need now.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5S_point_select_contiguous(const H5S_t *space)
{
    htri_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_point_select_contiguous, FAIL);

    assert(space);

    /* One point is definitely contiguous */
    if(space->select.num_elem==1)
    	ret_value=TRUE;
    else	/* More than one point might be contiguous, but it's complex to check and we don't need it right now */
    	ret_value=FALSE;

    FUNC_LEAVE (ret_value);
}   /* H5S_point_select_contiguous() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_select_single
 PURPOSE
    Check if a point selection is single within the dataspace extent.
 USAGE
    htri_t H5S_point_select_contiguous(space)
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
H5S_point_select_single(const H5S_t *space)
{
    htri_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_point_select_single, FAIL);

    assert(space);

    /* One point is definitely contiguous */
    if(space->select.num_elem==1)
    	ret_value=TRUE;
    else
    	ret_value=FALSE;

    FUNC_LEAVE (ret_value);
}   /* H5S_point_select_single() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_select_regular
 PURPOSE
    Check if a point selection is "regular"
 USAGE
    htri_t H5S_point_select_regular(space)
        const H5S_t *space;     IN: Dataspace pointer to check
 RETURNS
    TRUE/FALSE/FAIL
 DESCRIPTION
    Checks to see if the current selection in a dataspace is the a regular
    pattern.
    This is primarily used for reading the entire selection in one swoop.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Doesn't check for points selected to be next to one another in a regular
    pattern yet.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5S_point_select_regular(const H5S_t *space)
{
    htri_t ret_value=FAIL;  /* return value */

    FUNC_ENTER_NOAPI(H5S_point_select_regular, FAIL);

    /* Check args */
    assert(space);

    /* Only simple check for regular points for now... */
    if(space->select.num_elem==1)
    	ret_value=TRUE;
    else
    	ret_value=FALSE;

    FUNC_LEAVE (ret_value);
}   /* H5S_point_select_regular() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_elements
 PURPOSE
    Specify a series of elements in the dataspace to select
 USAGE
    herr_t H5S_select_elements(dsid, op, num_elem, coord)
        hid_t dsid;             IN: Dataspace ID of selection to modify
        H5S_seloper_t op;       IN: Operation to perform on current selection
        size_t num_elem;        IN: Number of elements in COORD array.
        const hssize_t **coord; IN: The location of each element selected
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function selects array elements to be included in the selection for
    the dataspace.  The COORD array is a 2-D array of size <dataspace rank>
    by NUM_ELEM (ie. a list of coordinates in the dataspace).  The order of
    the element coordinates in the COORD array specifies the order that the
    array elements are iterated through when I/O is performed.  Duplicate
    coordinates are not checked for.  The selection operator, OP, determines
    how the new selection is to be combined with the existing selection for
    the dataspace.  Currently, only H5S_SELECT_SET is supported, which replaces
    the existing selection with the one defined in this call.  When operators
    other than H5S_SELECT_SET are used to combine a new selection with an
    existing selection, the selection ordering is reset to 'C' array ordering.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5S_select_elements (H5S_t *space, H5S_seloper_t op, size_t num_elem,
    const hssize_t **coord)
{
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER_NOAPI(H5S_select_elements, FAIL);

    /* Check args */
    assert(space);
    assert(num_elem);
    assert(coord);
    assert(op==H5S_SELECT_SET || op==H5S_SELECT_APPEND || op==H5S_SELECT_PREPEND);

    /* If we are setting a new selection, remove current selection first */
    if(op==H5S_SELECT_SET) {
        if(H5S_select_release(space)<0) {
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL,
                "can't release point selection");
        } /* end if */
    } /* end if */

    /* Allocate space for the point selection information if necessary */
    if(space->select.type!=H5S_SEL_POINTS || space->select.sel_info.pnt_lst==NULL) {
        if((space->select.sel_info.pnt_lst = H5FL_ALLOC(H5S_pnt_list_t,1))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate element information");
    } /* end if */

    /* Add points to selection */
    if(H5S_point_add(space,op,num_elem,coord)<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL,
            "can't insert elements");
    }

    /* Set selection type */
    space->select.type=H5S_SEL_POINTS;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_select_elements() */


/*--------------------------------------------------------------------------
 NAME
    H5Sselect_elements
 PURPOSE
    Specify a series of elements in the dataspace to select
 USAGE
    herr_t H5Sselect_elements(dsid, op, num_elem, coord)
        hid_t dsid;             IN: Dataspace ID of selection to modify
        H5S_seloper_t op;       IN: Operation to perform on current selection
        size_t num_elem;        IN: Number of elements in COORD array.
        const hssize_t **coord; IN: The location of each element selected
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function selects array elements to be included in the selection for
    the dataspace.  The COORD array is a 2-D array of size <dataspace rank>
    by NUM_ELEM (ie. a list of coordinates in the dataspace).  The order of
    the element coordinates in the COORD array specifies the order that the
    array elements are iterated through when I/O is performed.  Duplicate
    coordinates are not checked for.  The selection operator, OP, determines
    how the new selection is to be combined with the existing selection for
    the dataspace.  Currently, only H5S_SELECT_SET is supported, which replaces
    the existing selection with the one defined in this call.  When operators
    other than H5S_SELECT_SET are used to combine a new selection with an
    existing selection, the selection ordering is reset to 'C' array ordering.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Sselect_elements (hid_t spaceid, H5S_seloper_t op, size_t num_elem,
    const hssize_t **coord)
{
    H5S_t	*space = NULL;  /* Dataspace to modify selection of */
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER_API(H5Sselect_elements, FAIL);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(coord==NULL || num_elem==0) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "elements not specified");
    } /* end if */
    if(!(op==H5S_SELECT_SET || op==H5S_SELECT_APPEND || op==H5S_SELECT_PREPEND)) {
        HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL,
            "operations other than H5S_SELECT_SET not supported currently");
    } /* end if */

    /* Call the real element selection routine */
    if((ret_value=H5S_select_elements(space,op,num_elem,coord))<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't select elements");
    } /* end if */

done:
    FUNC_LEAVE (ret_value);
}   /* H5Sselect_elements() */


/*--------------------------------------------------------------------------
 NAME
    H5S_point_select_get_seq_list
 PURPOSE
    Create a list of offsets & lengths for a selection
 USAGE
    herr_t H5S_point_select_get_seq_list(flags,space,iter,elem_size,maxseq,maxbytes,nseq,nbytes,off,len)
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
H5S_point_select_get_seq_list(unsigned flags, const H5S_t *space,H5S_sel_iter_t *iter,
    size_t elem_size, size_t maxseq, size_t maxbytes, size_t *nseq, size_t *nbytes,
    hsize_t *off, size_t *len)
{
    hsize_t bytes_left;         /* The number of bytes left in the selection */
    hsize_t start_bytes_left;   /* The initial number of bytes left in the selection */
    H5S_pnt_node_t *node;       /* Point node */
    hsize_t dims[H5O_LAYOUT_NDIMS];     /* Total size of memory buf */
    int	ndims;                  /* Dimensionality of space*/
    hsize_t	acc;            /* Coordinate accumulator */
    hsize_t	loc;            /* Coordinate offset */
    size_t      curr_seq;       /* Current sequence being operated on */
    int         i;              /* Local index variable */
    herr_t ret_value=SUCCEED;      /* return value */

    FUNC_ENTER_NOAPI (H5S_point_select_get_seq_list, FAIL);

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

    /* "round" off the maxbytes allowed to a multiple of the element size */
    maxbytes=(maxbytes/elem_size)*elem_size;

    /* Choose the minimum number of bytes to sequence through */
    start_bytes_left=bytes_left=MIN(iter->pnt.elmt_left*elem_size,maxbytes);

    /* Get the dataspace dimensions */
    if ((ndims=H5S_get_simple_extent_dims (space, dims, NULL))<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to retrieve data space dimensions");

    /* Walk through the points in the selection, starting at the current */
    /*  location in the iterator */
    node=iter->pnt.curr;
    curr_seq=0;
    while(node!=NULL) {
        /* Compute the offset of each selected point in the buffer */
        for(i=ndims-1,acc=elem_size,loc=0; i>=0; i--) {
            loc+=(node->pnt[i]+space->select.offset[i])*acc;
            acc*=dims[i];
        } /* end for */

        /* Check if this is a later point in the selection */
        if(curr_seq>0) {
            /* If a sorted sequence is requested, make certain we don't go backwards in the offset */
            if((flags&H5S_GET_SEQ_LIST_SORTED) && loc<off[curr_seq-1])
                break;

            /* Check if this point extends the previous sequence */
            /* (Unlikely, but possible) */
            if(loc==(off[curr_seq-1]+len[curr_seq-1])) {
                /* Extend the previous sequence */
                len[curr_seq-1]+=elem_size;
            } /* end if */
            else {
                /* Add a new sequence */
                off[curr_seq]=loc;
                len[curr_seq]=elem_size;

                /* Increment sequence count */
                curr_seq++;
            } /* end else */
        } /* end if */
        else {
            /* Add a new sequence */
            off[curr_seq]=loc;
            len[curr_seq]=elem_size;

            /* Increment sequence count */
            curr_seq++;
        } /* end else */

        /* Decrement number of bytes left to process */
        bytes_left-=elem_size;

        /* Move the iterator */
        iter->pnt.curr=node->next;
        iter->pnt.elmt_left--;

        /* Check if we're finished with all sequences */
        if(curr_seq==maxseq)
            break;

        /* Check if we're finished with all the bytes available */
        if(bytes_left==0)
            break;

        /* Advance to the next point */
        node=node->next;
      } /* end while */

    /* Set the number of sequences generated */
    *nseq=curr_seq;

    /* Set the number of bytes used */
    *nbytes=(start_bytes_left-bytes_left);

done:
    FUNC_LEAVE (ret_value);
} /* end H5S_point_select_get_seq_list() */
