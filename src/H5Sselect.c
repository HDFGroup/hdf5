/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.ued>
 *              Friday, May 29, 1998
 *
 * Purpose:	Dataspace functions.
 */

#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Iprivate.h>
#include <H5MMprivate.h>
#include <H5Sprivate.h>
#include <H5Vprivate.h>

/* Interface initialization */
#define PABLO_MASK      H5S_select_mask
#define INTERFACE_INIT  H5S_select_init
static intn             interface_initialize_g = FALSE;
static herr_t		    H5S_select_init(void);
static void		        H5S_select_term(void);


/*--------------------------------------------------------------------------
 NAME
    H5S_select_init
 PURPOSE
    Initialize selection interface
 USAGE
   herr_t  H5S_select_init(void)
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5S_select_init (void)
{
    herr_t		    ret_value = SUCCEED;
    FUNC_ENTER (H5S_select_init, FAIL);

    /* Register the atexit function for this (sub)interface */
    ret_value = H5_add_exit(&H5S_select_term);
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5S_select_term
 PURPOSE
    Terminate various H5S selection objects and free lists
 USAGE
    void H5S_select_term()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Release the selection resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static void
H5S_select_term(void)
{
}

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
    SUCCEED/FAIL
 DESCRIPTION
    Copies all the selection information (include offset) from the source
    dataspace to the destination dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5S_select_copy (H5S_t *dst, const H5S_t *src)
{
    FUNC_ENTER (H5S_select_copy, FAIL);

    /* Check args */
    assert(dst);
    assert(src);


    /* Perform correct type of copy based on the type of selection */

    FUNC_LEAVE (SUCCEED);
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
    SUCCEED/FAIL
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
    herr_t ret_value;     /* Counters */

    FUNC_ENTER (H5S_select_release, FAIL);

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
    } /* end switch() */

    FUNC_LEAVE (SUCCEED);
}   /* H5S_select_release() */

/*--------------------------------------------------------------------------
 NAME
    H5Sselect_hyperslab
 PURPOSE
    Specify a hyperslab to combine with the current hyperslab selection
 USAGE
    herr_t H5Sselect_hyperslab(dsid, op, start, stride, count, block)
        hid_t dsid;             IN: Dataspace ID of selection to modify
        H5S_seloper_t op;       IN: Operation to perform on current selection
        hssize_t *start;        IN: Offset of start of hyperslab
        hssize_t *stride;       IN: Hyperslab stride
        hssize_t *count;        IN: Number of blocks included in hyperslab
        hssize_t *block;        IN: Size of block in hyperslab
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Combines a hyperslab selection with the current selection for a dataspace.
    If the current selection is not a hyperslab, it is freed and the hyperslab
    parameters passed in are combined with the H5S_SEL_ALL hyperslab (ie. a
    selection composing the entire current extent).  Currently, only the
    H5S_SELECT_SET operation is supported.  If STRIDE or BLOCK is NULL, they
    are assumed to be set to all '1'.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Sselect_hyperslab (hid_t spaceid, H5S_seloper_t op, hssize_t *start,
    hssize_t *_stride, hssize_t *count, hssize_t *_block)
{
    H5S_t	*space = NULL;  /* Dataspace to modify selection of */
    hssize_t *stride,       /* Stride array */
        *block;             /* Block size array */
    uintn contig;           /* whether selection is contiguous or not */
    int i;                  /* Counters */
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER (H5Sselect_hyperslab, FAIL);

    /* Check args */
    if (H5_DATASPACE != H5I_group(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(start==NULL || count==NULL) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "hyperslab not specified");
    } /* end if */
    if(op!=H5S_SELECT_SET) {
        HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL,
            "operations other than H5S_SELECT_SET not supported currently");
    } /* end if */

    /* Fill in the correct stride values */
    if(_stride==NULL) {
        hssize_t fill=1;

        if((stride = H5MM_malloc(sizeof(hssize_t)*space->extent.u.simple.rank))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate stride vector");
        H5V_array_fill(stride,&fill,sizeof(hssize_t),space->extent.u.simple.rank);
    } else {
        stride=_stride;
    } /* end else */

    /* Fill in the correct block values */
    if(_block==NULL) {
        hssize_t fill=1;

        if((block = H5MM_malloc(sizeof(hssize_t)*space->extent.u.simple.rank))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate stride vector");
        H5V_array_fill(block,&fill,sizeof(hssize_t),space->extent.u.simple.rank);
    } else {
        block=_block;
    } /* end else */

/* Check for overlapping blocks (remove when real block-merging algorithm is in place) */
if(op==H5S_SELECT_SET && _block!=NULL) {
    for(i=0; i<space->extent.u.simple.rank; i++) {
        if(stride[i]<block[i]) {
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                "hyperslab blocks overlap");
        } /* end if */
    } /* end for */
} /* end if */

    /* Determine if selection is contiguous */
    contig=1;       /* assume hyperslab is contiguous, until proven otherwise */
    for(i=0; i<space->extent.u.simple.rank; i++) {
        /* contiguous hyperslabs have the block size equal to the stride */
        if(stride[i]!=block[i]) {
            contig=0;   /* hyperslab isn't contiguous */
            break;      /* no use looking further */
        } /* end if */
    } /* end for */

#ifdef QAK
printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    /* If we are setting a new selection, remove current selection first */
    if(op==H5S_SELECT_SET) {
        if(H5S_select_release(space)<0) {
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL,
                "can't release hyperslab");
        } /* end if */
    } /* end if */

#ifdef QAK
printf("%s: check 2.0\n",FUNC);
#endif /* QAK */
    /* Allocate space for the hyperslab selection information if necessary */
    if(space->select.type!=H5S_SEL_HYPERSLABS || space->select.sel_info.hyper_lst==NULL) {
        if((space->select.sel_info.hyper_lst = H5MM_calloc(sizeof(H5S_hyper_list_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate hyperslab information");
        if((space->select.sel_info.hyper_lst->lo_bounds = H5MM_calloc(space->extent.u.simple.rank*sizeof(H5S_hyper_bound_t *)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate hyperslab lo bound information");
        if((space->select.sel_info.hyper_lst->hi_bounds = H5MM_calloc(space->extent.u.simple.rank*sizeof(H5S_hyper_bound_t *)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate hyperslab lo bound information");
    } /* end if */

#ifdef QAK
printf("%s: check 3.0\n",FUNC);
#endif /* QAK */
    /* Add hyperslab to selection */
    if(contig) { /* Check for trivial case */
        if(H5S_hyper_add(space,start,count)<0) {
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL,
                "can't insert hyperslab");
        }
    } else {
/* Generate list of blocks to add/remove based on selection operation */
/* Add/Remove blocks to/from selection */
        assert("complex hyperslabs not supported yet" && 0);
    } /* end if */

    /* Set selection type */
    space->select.type=H5S_SEL_HYPERSLABS;
    ret_value=SUCCEED;
#ifdef QAK
printf("%s: check 4.0\n",FUNC);
#endif /* QAK */

done:
    if(_stride==NULL && stride!=NULL)
        H5MM_xfree(stride);
    if(_block==NULL && block!=NULL)
        H5MM_xfree(block);

    FUNC_LEAVE (ret_value);
}   /* H5Sselect_hyperslab() */

/*--------------------------------------------------------------------------
 NAME
    H5Sselect_npoints
 PURPOSE
    Get the number of elements in current selection
 USAGE
    hsize_t H5Sselect_npoints(dsid)
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
hsize_t H5Sselect_npoints (hid_t spaceid)
{
    H5S_t	*space = NULL;      /* Dataspace to modify selection of */
    hsize_t ret_value=0;        /* return value */

    FUNC_ENTER (H5Sselect_npoints, 0);

    /* Check args */
    if (H5_DATASPACE != H5I_group(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a data space");
    }

    ret_value = H5S_select_npoints(space);

    FUNC_LEAVE (ret_value);
}   /* H5Sselect_npoints() */

/*--------------------------------------------------------------------------
 NAME
    H5S_select_npoints
 PURPOSE
    Get the number of elements in current selection
 USAGE
    herr_t H5Sselect_hyperslab(ds)
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
hsize_t H5S_select_npoints (const H5S_t *space)
{
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER (H5Sselect_npoints, FAIL);

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
    } /* end switch */

    FUNC_LEAVE (ret_value);
}   /* H5Sselect_npoints() */
