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
#include <H5TBprivate.h>

/* Interface initialization */
#define PABLO_MASK      H5S_select_mask
#define INTERFACE_INIT  NULL
static intn             interface_initialize_g = 0;

static hssize_t H5S_get_select_hyper_nblocks(H5S_t *space);
static hssize_t H5S_get_select_elem_npoints(H5S_t *space);
static herr_t H5S_get_select_hyper_blocklist(H5S_t *space, hsize_t startblock, hsize_t numblocks, hsize_t *buf);
static herr_t H5S_get_select_elem_pointlist(H5S_t *space, hsize_t startpoint, hsize_t numpoints, hsize_t *buf);
static herr_t H5S_get_select_bounds(H5S_t *space, hsize_t *start, hsize_t *end);


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

    FUNC_ENTER (H5S_select_copy, FAIL);

    /* Check args */
    assert(dst);
    assert(src);

    /* Copy regular fields */
    dst->select=src->select;

/* Need to copy order information still */

    /* Copy offset information */
    if (NULL==(dst->select.offset = H5MM_calloc(src->extent.u.simple.rank*sizeof(hssize_t)))) {
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

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
            break;
    }

    /* Reset type of selection to "all" */
    space->select.type=H5S_SEL_ALL;

    FUNC_LEAVE (ret_value);
}   /* H5S_select_release() */

/*-------------------------------------------------------------------------
 * Function:	H5S_select_hyperslab
 *
 * Purpose:	Internal version of H5Sselect_hyperslab().
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke (split from HSselect_hyperslab()).
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_select_hyperslab (H5S_t *space, H5S_seloper_t op,
		      const hssize_t start[/*space_id*/],
		      const hsize_t stride[/*space_id*/],
		      const hsize_t count[/*space_id*/],
		      const hsize_t block[/*space_id*/])
{
    hid_t stride_id=FAIL,block_id=FAIL; /* Stride & block temp. buffer IDs */
    hsize_t *_stride=NULL;        /* Stride array */
    hsize_t *_block=NULL;        /* Block size array */
    hssize_t slab[H5O_LAYOUT_NDIMS]; /* Location of the block to add for strided selections */
    size_t slice[H5O_LAYOUT_NDIMS];	 /* Size of preceding dimension's slice */
    H5S_hyper_node_t *add=NULL, /* List of hyperslab nodes to add */
        *uniq=NULL,         /* List of unique hyperslab nodes */
        *tmp;               /* Temporary hyperslab node */
    uintn acc;                /* Accumulator for building slices */
    uintn contig;             /* whether selection is contiguous or not */
    int i,j;                  /* Counters */
    H5S_hyper_dim_t *diminfo; /* per-dimension info for the selection */
    herr_t ret_value=FAIL;    /* return value */

    FUNC_ENTER (H5S_select_hyperslab, FAIL);

    /* Check args */
    assert(space);
    assert(start);
    assert(count);
    assert(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID);
    
    /* Fill in the correct stride values */
    if(stride==NULL) {
        hssize_t fill=1;

        /* Allocate temporary buffer */
        if((stride_id = H5TB_get_buf(sizeof(hssize_t)*space->extent.u.simple.rank,0,(void **)&_stride))<0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate stride vector");
        H5V_array_fill(_stride,&fill,sizeof(hssize_t),space->extent.u.simple.rank);
        stride = _stride;
    }

    /* Fill in the correct block values */
    if(block==NULL) {
        hssize_t fill=1;

        if((block_id = H5TB_get_buf(sizeof(hssize_t)*space->extent.u.simple.rank,0,(void **)&_block))<0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate block vector");
        H5V_array_fill(_block,&fill,sizeof(hssize_t),space->extent.u.simple.rank);
        block = _block;
    }

    /*
     * Check for overlapping hyperslab blocks in new selection (remove when
     *  real block-merging algorithm is in place? -QAK).
     */
    if(op==H5S_SELECT_SET && block!=NULL) {
        for(i=0; i<space->extent.u.simple.rank; i++) {
            if(stride[i]<block[i]) {
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "hyperslab blocks overlap");
            } /* end if */
        } /* end for */
    } /* end if */

    /* Determine if selection is contiguous */
    /* assume hyperslab is contiguous, until proven otherwise */
    contig=1;
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
    if(space->select.type!=H5S_SEL_HYPERSLABS || space->select.sel_info.hslab.hyper_lst==NULL) {
        if((space->select.sel_info.hslab.hyper_lst = H5MM_calloc(sizeof(H5S_hyper_list_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab information");
        if((space->select.sel_info.hslab.hyper_lst->lo_bounds = H5MM_calloc(space->extent.u.simple.rank* sizeof(H5S_hyper_bound_t *)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab lo bound information");
        if((space->select.sel_info.hslab.hyper_lst->hi_bounds = H5MM_calloc(space->extent.u.simple.rank* sizeof(H5S_hyper_bound_t *)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab lo bound information");
    } /* end if */

    /* Generate list of blocks to add/remove based on selection operation */
    switch(op) {
        case H5S_SELECT_SET:
        case H5S_SELECT_OR:
            /* Generate list of blocks to add to selection */
            if(contig) { /* Check for trivial case */

                /* Account for strides & blocks being equal, but larger than one */
                /* (Why someone would torture us this way, I don't know... -QAK :-) */
                for(i=0; i<space->extent.u.simple.rank; i++)
                    slab[i]=count[i]*stride[i];

                /* Add the contiguous hyperslab to the selection */
                if(H5S_hyper_node_add(&add,0,space->extent.u.simple.rank,start,(const hsize_t *)slab)<0) {
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslab");
                }
            } else {
                /* Build the slice sizes for each dimension */
                for(i=0, acc=1; i<space->extent.u.simple.rank; i++) {
                    slice[i]=acc;
                    acc*=count[i];
                } /* end for */

                /* Step through all the blocks to add */
                /* (reuse the count in ACC above) */
                for(i=0; i<(int)acc; i++) {
                    /* Build the location of the block */
                    for(j=0; j<space->extent.u.simple.rank; j++)
                        slab[j]=start[j]+((i/slice[j])%count[j])*stride[j];
                    
                    /* Add the block to the list of hyperslab selections */
                    if(H5S_hyper_node_add(&add,0,space->extent.u.simple.rank,(const hssize_t *)slab, (const hsize_t *)block)<0) {
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslab");
                    } /* end if */
                } /* end for */
            } /* end else */

            /* Clip list of new blocks to add against current selection */
            if(op==H5S_SELECT_OR) {
                H5S_hyper_clip(space,add,&uniq,NULL);
                add=uniq;
            } /* end if */
            else {
                /* Copy all the per-dimension selection info into the space descriptor */
                if((diminfo = H5MM_malloc(sizeof(H5S_hyper_dim_t)*space->extent.u.simple.rank))==NULL) {
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate per-dimension vector");
                } /* end if */
                for(i=0; i<space->extent.u.simple.rank; i++) {
                    diminfo[i].start = start[i];
                    diminfo[i].stride = stride[i];
                    diminfo[i].count = count[i];
                    diminfo[i].block = block[i];
                } /* end for */
                space->select.sel_info.hslab.diminfo = diminfo;
            } /* end else */
            break;

        default:
            HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");
    } /* end switch */

    /* Add new blocks to current selection */
    while(add!=NULL) {
        tmp=add->next;

        /* Add new block */
        if(H5S_hyper_add(space,(const hssize_t *)add->start, (const hsize_t *)add->end)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslab");
        
        /* Free nodes in list */
        H5MM_xfree(add->start);
        H5MM_xfree(add->end);
        H5MM_xfree(add);

        /* Go to next node */
        add=tmp;
    } /* end while */

    /* Merge blocks for better I/O performance */
    /* Regenerate lo/hi bounds arrays? */

#ifdef QAK
    printf("%s: check 3.0\n",FUNC);
#endif /* QAK */

    /* Set selection type */
    space->select.type=H5S_SEL_HYPERSLABS;
    ret_value=SUCCEED;
#ifdef QAK
    printf("%s: check 4.0\n",FUNC);
#endif /* QAK */

done:
    if(_stride!=NULL) H5TB_release_buf(stride_id);
    if(_block!=NULL) H5TB_release_buf(block_id);
    FUNC_LEAVE (ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5Sselect_hyperslab
 PURPOSE
    Specify a hyperslab to combine with the current hyperslab selection
 USAGE
    herr_t H5Sselect_hyperslab(dsid, op, start, stride, count, block)
        hid_t dsid;             IN: Dataspace ID of selection to modify
        H5S_seloper_t op;       IN: Operation to perform on current selection
        const hssize_t *start;        IN: Offset of start of hyperslab
        const hssize_t *stride;       IN: Hyperslab stride
        const hssize_t *count;        IN: Number of blocks included in hyperslab
        const hssize_t *block;        IN: Size of block in hyperslab
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Combines a hyperslab selection with the current selection for a dataspace.
    If the current selection is not a hyperslab, it is freed and the hyperslab
    parameters passed in are combined with the H5S_SEL_ALL hyperslab (ie. a
    selection composing the entire current extent).  Currently, only the
    H5S_SELECT_SET & H5S_SELECT_OR operations are supported.  If STRIDE or
    BLOCK is NULL, they are assumed to be set to all '1'.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Sselect_hyperslab(hid_t space_id, H5S_seloper_t op,
		     const hssize_t start[/*space_id*/],
		     const hsize_t _stride[/*space_id*/],
		     const hsize_t count[/*space_id*/],
		     const hsize_t _block[/*space_id*/])
{
    H5S_t	*space = NULL;  /* Dataspace to modify selection of */

    FUNC_ENTER (H5Sselect_hyperslab, FAIL);
    H5TRACE6("e","iSs*[a0]Hs*[a0]h*[a0]h*[a0]h",space_id,op,start,_stride,
             count,_block);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(space_id) ||
            NULL == (space=H5I_object(space_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(start==NULL || count==NULL) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "hyperslab not specified");
    } /* end if */

    if(!(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID)) {
        HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");
    } /* end if */

    if (H5S_select_hyperslab(space, op, start, _stride, count, _block)<0) {
        HRETURN_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL,
		      "unable to set hyperslab selection");
    }

    FUNC_LEAVE (SUCCEED);
}

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

    FUNC_ENTER (H5S_select_elements, FAIL);

    /* Check args */
    assert(space);
    assert(num_elem);
    assert(coord);
    assert(op==H5S_SELECT_SET);

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
    /* Allocate space for the point selection information if necessary */
    if(space->select.type!=H5S_SEL_POINTS || space->select.sel_info.pnt_lst==NULL) {
        if((space->select.sel_info.pnt_lst = H5MM_calloc(sizeof(H5S_pnt_list_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate element information");
    } /* end if */

#ifdef QAK
    printf("%s: check 3.0\n",FUNC);
#endif /* QAK */
    /* Add points to selection */
    if(H5S_point_add(space,num_elem,coord)<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL,
            "can't insert elements");
    }

    /* Set selection type */
    space->select.type=H5S_SEL_POINTS;
#ifdef QAK
    printf("%s: check 4.0\n",FUNC);
#endif /* QAK */

done:
    FUNC_LEAVE (ret_value);
}   /* H5Sselect_elements() */

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

    FUNC_ENTER (H5Sselect_elements, FAIL);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(coord==NULL || num_elem==0) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "elements not specified");
    } /* end if */
    if(op!=H5S_SELECT_SET) {
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
herr_t H5S_select_all (H5S_t *space)
{
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER (H5S_select_all, FAIL);

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

    FUNC_ENTER (H5Sselect_all, FAIL);

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
    H5S_select_none
 PURPOSE
    Specify that nothing is selected in the extent
 USAGE
    herr_t H5S_select_none(dsid)
        hid_t dsid;             IN: Dataspace ID of selection to modify
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function de-selects the entire extent for a dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5S_select_none (H5S_t *space)
{
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER (H5S_select_none, FAIL);

    /* Check args */
    assert(space);

    /* Remove current selection first */
    if(H5S_select_release(space)<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL,
            "can't release hyperslab");
    } /* end if */

    /* Set selection type */
    space->select.type=H5S_SEL_NONE;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_select_none() */

/*--------------------------------------------------------------------------
 NAME
    H5Sselect_none
 PURPOSE
    Specify that nothing is selected in the extent
 USAGE
    herr_t H5Sselect_none(dsid)
        hid_t dsid;             IN: Dataspace ID of selection to modify
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function de-selects the entire extent for a dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Sselect_none (hid_t spaceid)
{
    H5S_t	*space = NULL;  /* Dataspace to modify selection of */
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER (H5Sselect_none, FAIL);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }

    /* Change to "none" selection */
    if((ret_value=H5S_select_none(space))<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection");
    } /* end if */

done:
    FUNC_LEAVE (ret_value);
}   /* H5Sselect_none() */

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

    FUNC_ENTER (H5Sget_select_npoints, 0);
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
hssize_t
H5S_get_select_npoints (const H5S_t *space)
{
    hssize_t ret_value=FAIL;  /* return value */

    FUNC_ENTER (H5S_get_select_npoints, FAIL);

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

    FUNC_ENTER (H5S_sel_iter_release, FAIL);

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

    FUNC_ENTER (H5Sselect_valid, 0);
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

    FUNC_ENTER (H5S_select_valid, FAIL);

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

    FUNC_ENTER (H5S_select_serial_size, FAIL);

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

    FUNC_ENTER (H5S_select_serialize, FAIL);

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
    const uint32_t *sel_type;       /* Pointer to the selection type */
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER (H5S_select_deserialize, FAIL);

    assert(space);

    sel_type=(const uint32_t *)buf;
    switch(*sel_type) {
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

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
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

    FUNC_ENTER (H5S_get_select_hyper_nblocks, FAIL);

    assert(space);

    ret_value = space->select.sel_info.hslab.hyper_lst->count;

    FUNC_LEAVE (ret_value);
}   /* H5Sget_select_hyper_nblocks() */

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

    FUNC_ENTER (H5Sget_select_hyper_nblocks, FAIL);
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

    FUNC_ENTER (H5S_get_select_elem_npoints, FAIL);

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

    FUNC_ENTER (H5Sget_select_elem_npoints, FAIL);
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
    herr_t H5S_get_select_hyper_blocklist(space, hsize_t *buf)
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
static herr_t
H5S_get_select_hyper_blocklist(H5S_t *space, hsize_t startblock, hsize_t numblocks, hsize_t *buf)
{
    H5S_hyper_node_t *node;     /* Hyperslab node */
    intn rank;                  /* Dataspace rank */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5S_get_select_hyper_blocklist, FAIL);

    assert(space);
    assert(buf);

    /* Get the dataspace extent rank */
    rank=space->extent.u.simple.rank;

    /* Get the head of the hyperslab list */
    node=space->select.sel_info.hslab.hyper_lst->head;

    /* Get to the correct first node to give back to the user */
    while(node!=NULL && startblock>0) {
        startblock--;
        node=node->next;
      } /* end while */
    
    /* Iterate through the node, copying each hyperslab's information */
    while(node!=NULL && numblocks>0) {
        HDmemcpy(buf,node->start,sizeof(hsize_t)*rank);
        buf+=rank;
        HDmemcpy(buf,node->end,sizeof(hsize_t)*rank);
        buf+=rank;
        numblocks--;
        node=node->next;
      } /* end while */

    FUNC_LEAVE (ret_value);
}   /* H5Sget_select_hyper_blocklist() */

/*--------------------------------------------------------------------------
 NAME
    H5Sget_select_hyper_blocklist
 PURPOSE
    Get the list of hyperslab blocks currently selected
 USAGE
    herr_t H5Sget_select_hyper_blocklist(dsid, hsize_t *buf)
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

    FUNC_ENTER (H5Sget_select_hyper_blocklist, FAIL);
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

    ret_value = H5S_get_select_hyper_blocklist(space,startblock,numblocks,buf);

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
    intn rank;                  /* Dataspace rank */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5S_get_select_elem_pointlist, FAIL);

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
    node=space->select.sel_info.pnt_lst->head;
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

    FUNC_ENTER (H5Sget_select_elem_pointlist, FAIL);
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
    intn rank;                  /* Dataspace rank */
    intn i;                     /* index variable */
    herr_t ret_value=FAIL;   /* return value */

    FUNC_ENTER (H5S_get_select_bounds, FAIL);

    assert(space);
    assert(start);
    assert(end);

    /* Set all the start and end arrays up */
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

    FUNC_ENTER (H5Sget_select_bounds, FAIL);
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
