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
#define INTERFACE_INIT  H5S_select_init
static intn             interface_initialize_g = FALSE;
static herr_t		H5S_select_init(void);
static void		H5S_select_term(void);


/*--------------------------------------------------------------------------
 NAME
    H5S_select_init
 PURPOSE
    Initialize selection interface
 USAGE
   herr_t  H5S_select_init(void)
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_select_init (void)
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
    Non-negative on success/Negative on failure
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
     * Check for overlapping blocks (remove when real block-merging algorithm
     * is in place?).
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

#ifndef OLD_WAY
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
            } /* end if */

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
            break;
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
#else /* OLD_WAY */
    /* Add hyperslab to selection */
    if(contig) { /* Check for trivial case */

        /* Account for strides & blocks being equal, but larger than one */
        /* (Why someone would torture us this way, I don't know... -QAK :-) */
        for(i=0; i<space->extent.u.simple.rank; i++)
            slab[i]=count[i]*stride[i];

        /* Add the contiguous hyperslab to the selection */
        if(H5S_hyper_add(space,start,(const hsize_t *)slab)<0) {
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
            if(H5S_hyper_add(space,(const hssize_t *)slab, (const hsize_t *)block)<0) {
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslab");
            } /* end if */
        } /* end for */
    } /* end if */
#endif /* OLD_WAY */

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
    herr_t ret_value=FAIL;  /* return value */

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
    ret_value=SUCCEED;
#ifdef QAK
    printf("%s: check 4.0\n",FUNC);
#endif /* QAK */

done:
    FUNC_LEAVE (ret_value);
}   /* H5Sselect_elements() */

/*--------------------------------------------------------------------------
 NAME
    H5Sselect_all
 PURPOSE
    Specify the the entire extent is selected
 USAGE
    herr_t H5Sselect_elements(dsid)
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
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER (H5Sselect_all, FAIL);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(spaceid) ||
            NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }

    /* Remove current selection first */
    if(H5S_select_release(space)<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL,
            "can't release hyperslab");
    } /* end if */

    /* Set selection type */
    space->select.type=H5S_SEL_ALL;
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5Sselect_all() */

/*--------------------------------------------------------------------------
 NAME
    H5Sselect_none
 PURPOSE
    Specify that nothing is selected in the extent
 USAGE
    herr_t H5Sselect_elements(dsid)
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

    /* Remove current selection first */
    if(H5S_select_release(space)<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL,
            "can't release hyperslab");
    } /* end if */

    /* Set selection type */
    space->select.type=H5S_SEL_NONE;
    ret_value=SUCCEED;

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
