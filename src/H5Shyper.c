/*
 * Copyright (C) 1998-2001 NCSA
 *                         All rights reserved.
 *
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Thursday, June 18, 1998
 *
 * Purpose:	Hyperslab selection data space I/O functions.
 */

#define H5S_PACKAGE		/*suppress error about including H5Spkg	  */

#include "H5private.h"
#include "H5Dprivate.h"
#include "H5Eprivate.h"
#include "H5Fprivate.h"
#include "H5FLprivate.h"	/*Free Lists	  */
#include "H5Iprivate.h"
#include "H5MMprivate.h"
#include "H5Pprivate.h"         /* Property Lists */
#include "H5Spkg.h"
#include "H5Tprivate.h"         /* Datatypes */
#include "H5Vprivate.h"

/* Interface initialization */
#define PABLO_MASK      H5Shyper_mask
#define INTERFACE_INIT  NULL
static int             interface_initialize_g = 0;

/* Local datatypes */
/* Parameter block for H5S_hyper_select_iter_mem */
typedef struct {
    hid_t dt;
    size_t elem_size;
    const H5S_t *space;
    H5S_sel_iter_t *iter;
    void *src;
    H5D_operator_t op;
    void * op_data;
} H5S_hyper_iter_info_t;

/* Static function prototypes */
static herr_t H5S_hyper_init (const H5S_t *space, size_t elmt_size, H5S_sel_iter_t *iter);
static hsize_t H5S_hyper_favail (const H5S_t UNUSED *space,
		  const H5S_sel_iter_t *sel_iter, hsize_t max);
static hsize_t H5S_hyper_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill,
		 const struct H5O_efl_t *efl, size_t elmt_size,
		 const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		 hsize_t nelmts, hid_t dxpl_id, void *_buf/*out*/);
static herr_t H5S_hyper_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill,
		 const struct H5O_efl_t *efl, size_t elmt_size,
		 const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		 hsize_t nelmts, hid_t dxpl_id, const void *_buf);
static hsize_t H5S_hyper_mgath (const void *_buf, size_t elmt_size,
		 const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		 hsize_t nelmts, void *_tconv_buf/*out*/);
static herr_t H5S_hyper_mscat (const void *_tconv_buf, size_t elmt_size,
		 const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		 hsize_t nelmts, void *_buf/*out*/);
static herr_t H5S_hyper_free_span_info (H5S_hyper_span_info_t *span_info);
static herr_t H5S_hyper_free_span (H5S_hyper_span_t *span);
static H5S_hyper_span_info_t *H5S_hyper_copy_span (H5S_hyper_span_info_t *spans);
static herr_t H5S_hyper_span_scratch (H5S_hyper_span_info_t *spans, void *scr_value);
static herr_t H5S_hyper_span_precompute (H5S_hyper_span_info_t *spans, size_t elmt_size);
static herr_t H5S_select_hyperslab (H5S_t *space, H5S_seloper_t op, const hssize_t start[],
                const hsize_t stride[], const hsize_t count[],
                const hsize_t block[]);

const H5S_fconv_t	H5S_HYPER_FCONV[1] = {{
    "hslab",	 				/*name			*/
    H5S_SEL_HYPERSLABS,				/*selection type	*/
    H5S_hyper_init,				/*initialize		*/
    H5S_hyper_favail,				/*available		*/
    H5S_hyper_fgath,				/*gather		*/
    H5S_hyper_fscat,				/*scatter		*/
}};

const H5S_mconv_t	H5S_HYPER_MCONV[1] = {{
    "hslab",					/*name			*/
    H5S_SEL_HYPERSLABS,				/*selection type	*/
    H5S_hyper_init, 				/*initialize		*/
    H5S_hyper_mgath,				/*gather		*/
    H5S_hyper_mscat,				/*scatter		*/
}};

/* Declare a free list to manage the H5S_hyper_span_t struct */
H5FL_DEFINE_STATIC(H5S_hyper_span_t);

/* Declare a free list to manage arrays of H5S_hyper_span_t */
H5FL_ARR_DEFINE_STATIC(H5S_hyper_span_t,H5S_MAX_RANK);

/* Declare a free list to manage the H5S_hyper_span_info_t struct */
H5FL_DEFINE_STATIC(H5S_hyper_span_info_t);

/* Declare a free list to manage arrays of size_t */
H5FL_ARR_DEFINE_STATIC(size_t,-1);

/* Declare a free list to manage arrays of hsize_t */
H5FL_ARR_DEFINE_STATIC(hsize_t,-1);

/* Declare a free list to manage arrays of H5S_hyper_dim_t */
H5FL_ARR_DEFINE_STATIC(H5S_hyper_dim_t,H5S_MAX_RANK);

#ifdef QAK
static herr_t
H5S_hyper_print_spans_helper(struct H5S_hyper_span_t *span,unsigned depth)
{
    struct H5S_hyper_span_t *tmp_span;

    FUNC_ENTER(H5S_hyper_print_spans_helper, FAIL);

    tmp_span=span;
    while(tmp_span) {
        printf("%s: depth=%u, span=%p, (%d, %d), nelem=%u, pstride=%u\n",FUNC,depth,tmp_span,(int)tmp_span->low,(int)tmp_span->high,(unsigned)tmp_span->nelem,(unsigned)tmp_span->pstride);
        if(tmp_span->down && tmp_span->down->head) {
            printf("%s: spans=%p, count=%u, scratch=%p, head=%p\n",FUNC,tmp_span->down,tmp_span->down->count,tmp_span->down->scratch,tmp_span->down->head);
            H5S_hyper_print_spans_helper(tmp_span->down->head,depth+1);
        } /* end if */
        tmp_span=tmp_span->next;
    } /* end while */

    FUNC_LEAVE(SUCCEED);
}

static herr_t
H5S_hyper_print_spans(const struct H5S_hyper_span_info_t *span_lst)
{
    FUNC_ENTER(H5S_hyper_print_spans, FAIL);

    if(span_lst!=NULL) {
        printf("%s: spans=%p, count=%u, scratch=%p, head=%p\n",FUNC,span_lst,span_lst->count,span_lst->scratch,span_lst->head);
        H5S_hyper_print_spans_helper(span_lst->head,0);
    } /* end if */

    FUNC_LEAVE(SUCCEED);
}
#endif /* QAK */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_init
 *
 * Purpose:	Initializes iteration information for hyperslab span tree selection.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Programmer:	Quincey Koziol
 *              Saturday, February 24, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_hyper_init (const H5S_t *space, size_t elmt_size, H5S_sel_iter_t *sel_iter)
{
    H5S_hyper_span_info_t *spans;   /* Pointer to hyperslab span info node */
    unsigned u;                    /* Index variable */

    FUNC_ENTER (H5S_hyper_init, FAIL);

    /* Check args */
    assert(space && H5S_SEL_HYPERSLABS==space->select.type);
    assert(sel_iter);
    assert(space->select.sel_info.hslab.span_lst);

    /* Initialize the number of points to iterate over */
    sel_iter->hyp.elmt_left=space->select.num_elem;

/* Initialize the information needed for non-regular hyperslab I/O */
    /* Make a copy of the span tree to iterate over */
    sel_iter->hyp.spans=H5S_hyper_copy_span(space->select.sel_info.hslab.span_lst);

    /* Set the nelem & pstride values according to the element size */
    H5S_hyper_span_precompute(sel_iter->hyp.spans,elmt_size);

    /* Allocate the span tree pointers, span pointers and positions */
    sel_iter->hyp.span = H5FL_ARR_ALLOC(H5S_hyper_span_t,space->extent.u.simple.rank,0);
    sel_iter->hyp.off = H5FL_ARR_ALLOC(hsize_t,space->extent.u.simple.rank,0);

    /* Initialize the starting span_info's and spans */
    spans=sel_iter->hyp.spans;
    for(u=0; u<space->extent.u.simple.rank; u++) {
        /* Set the pointers to the initial span in each dimension */
        assert(spans);
        assert(spans->head);

        /* Set the pointer to the first span in the list for this node */
        sel_iter->hyp.span[u] = spans->head;

        /* Set the initial offset to low bound of span */
        sel_iter->hyp.off[u]=sel_iter->hyp.span[u]->low;

        /* Get the pointer to the next level down */
        spans=spans->head->down;
    } /* end for */

/* Initialize the information needed for regular hyperslab I/O */
    /* Allocate the position & initialize to invalid location */
    sel_iter->hyp.pos = H5FL_ARR_ALLOC(hsize_t,space->extent.u.simple.rank,0);
    sel_iter->hyp.pos[0]=(-1);
    H5V_array_fill(sel_iter->hyp.pos, sel_iter->hyp.pos, sizeof(hssize_t),
		   space->extent.u.simple.rank);

    FUNC_LEAVE (SUCCEED);
}   /* H5S_hyper_init() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_sel_iter_release
 PURPOSE
    Release "new" hyperslab selection iterator information for a dataspace
 USAGE
    herr_t H5S_hyper_sel_iter_release(sel_iter)
        H5S_sel_iter_t *sel_iter;       IN: Pointer to selection iterator
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Releases all information for a dataspace "new" hyperslab selection iterator
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_hyper_sel_iter_release (H5S_sel_iter_t *sel_iter)
{
    FUNC_ENTER (H5S_hyper_sel_iter_release, FAIL);

    /* Check args */
    assert (sel_iter);

/* Release the information needed for non-regular hyperslab I/O */
    /* Free the copy of the selections span tree */
    if(sel_iter->hyp.spans!=NULL)
        H5S_hyper_free_span_info(sel_iter->hyp.spans);

    /* Release the array of pointers to span nodes */
    if(sel_iter->hyp.span!=NULL)
        H5FL_ARR_FREE(H5S_hyper_span_t,sel_iter->hyp.span);

    /* Release the array of offsets */
    if(sel_iter->hyp.off!=NULL)
        H5FL_ARR_FREE(hsize_t,sel_iter->hyp.off);

/* Release the information needed for regular hyperslab I/O */
    /* Release the hyperslab position */
    if(sel_iter->hyp.pos!=NULL)
        H5FL_ARR_FREE(hsize_t,sel_iter->hyp.pos);

    FUNC_LEAVE (SUCCEED);
}   /* H5S_hyper_sel_iter_release() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_favail
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
static hsize_t
H5S_hyper_favail (const H5S_t * UNUSED space,
		  const H5S_sel_iter_t *sel_iter, hsize_t max)
{
    FUNC_ENTER (H5S_hyper_favail, 0);

    /* Check args */
    assert (space && H5S_SEL_HYPERSLABS==space->select.type);
    assert (sel_iter);

#ifdef QAK
    printf("%s: sel_iter->hyp.elmt_left=%u, max=%u\n",FUNC,(unsigned)sel_iter->hyp.elmt_left,(unsigned)max);
#endif /* QAK */
    FUNC_LEAVE (MIN(sel_iter->hyp.elmt_left,max));
}   /* H5S_hyper_favail() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_iter_next
 *
 * Purpose:	Moves a hyperslab iterator to the beginning of the next sequence
 *      of elements to read.  Handles walking off the end in all dimensions.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, September 8, 2000
 *
 * Notes:
 *      Only used for the optimized hyperslab I/O routines
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5S_hyper_iter_next (const H5S_t *file_space, H5S_sel_iter_t *file_iter)
{
    hsize_t iter_offset[H5O_LAYOUT_NDIMS];
    hsize_t iter_count[H5O_LAYOUT_NDIMS];
    int fast_dim;  /* Rank of the fastest changing dimension for the dataspace */
    int temp_dim;  /* Temporary rank holder */
    unsigned i;        /* Counters */
    unsigned ndims;    /* Number of dimensions of dataset */

    FUNC_ENTER (H5S_hyper_iter_next, FAIL);

    /* Set some useful rank information */
    fast_dim=file_space->extent.u.simple.rank-1;
    ndims=file_space->extent.u.simple.rank;

    /* Calculate the offset and block count for each dimension */
    for(i=0; i<ndims; i++) {
        iter_offset[i]=(file_iter->hyp.pos[i]-file_space->select.sel_info.hslab.diminfo[i].start)%file_space->select.sel_info.hslab.diminfo[i].stride;
        iter_count[i]=(file_iter->hyp.pos[i]-file_space->select.sel_info.hslab.diminfo[i].start)/file_space->select.sel_info.hslab.diminfo[i].stride;
    } /* end for */

    /* Start with the fastest changing dimension */
    temp_dim=fast_dim;
    while(temp_dim>=0) {
        if(temp_dim==fast_dim) {
            /* Move to the next block in the current dimension */
            iter_offset[temp_dim]=0;    /* reset the offset in the fastest dimension */
            iter_count[temp_dim]++;

            /* If this block is still in the range of blocks to output for the dimension, break out of loop */
            if(iter_count[temp_dim]<file_space->select.sel_info.hslab.diminfo[temp_dim].count)
                break;
            else
                iter_count[temp_dim]=0; /* reset back to the beginning of the line */
        } /* end if */
        else {
            /* Move to the next row in the curent dimension */
            iter_offset[temp_dim]++;

            /* If this block is still in the range of blocks to output for the dimension, break out of loop */
            if(iter_offset[temp_dim]<file_space->select.sel_info.hslab.diminfo[temp_dim].block)
                break;
            else {
                /* Move to the next block in the current dimension */
                iter_offset[temp_dim]=0;
                iter_count[temp_dim]++;

                /* If this block is still in the range of blocks to output for the dimension, break out of loop */
                if(iter_count[temp_dim]<file_space->select.sel_info.hslab.diminfo[temp_dim].count)
                    break;
                else
                    iter_count[temp_dim]=0; /* reset back to the beginning of the line */
            } /* end else */
        } /* end else */

        /* Decrement dimension count */
        temp_dim--;
    } /* end while */

    /* Translate current iter_offset and iter_count into iterator position */
    for(i=0; i<ndims; i++)
        file_iter->hyp.pos[i]=file_space->select.sel_info.hslab.diminfo[i].start+(file_space->select.sel_info.hslab.diminfo[i].stride*iter_count[i])+iter_offset[i];

    FUNC_LEAVE (SUCCEED);
} /* H5S_hyper_iter_next() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_fread
 *
 * Purpose:	Performs an optimized gather from the file, based on a hyperslab
 *      span tree.
 *
 * Return:	Success:	Number of elements copied.
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Friday, September 8, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5S_hyper_fread (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill,
		 const struct H5O_efl_t *efl, size_t elmt_size,
		 const H5S_t *space, H5S_sel_iter_t *iter,
		 hsize_t nelem, hid_t dxpl_id, void *_buf/*out*/)
{
    uint8_t	*dst=(uint8_t *)_buf;   /* Alias for pointer arithmetic */
    H5S_hyper_span_t **ispan;       /* Iterator's hyperslab span nodes */
    H5S_hyper_span_t *curr_span;    /* Current hyperslab span node */
    hsize_t slab[H5O_LAYOUT_NDIMS]; /* Cumulative size of each dimension in bytes */
    hsize_t acc;        /* Accumulator for computing cumulative sizes */
    hssize_t *abs_arr;  /* Absolute hyperslab span position */
    hssize_t *off_arr;  /* Offset within the dataspace extent */
    int fast_dim;      /* Rank of the fastest changing dimension for the dataspace */
    int curr_dim;      /* Current dimension being operated on */
    int ndims;         /* Number of dimensions of dataset */
    hsize_t loc_off;    /* Element offset in the dataspace */
    size_t span_size;   /* Number of bytes in current span to actually process */
    size_t io_bytes_left;   /* Number of bytes left to process */
    int i;             /* Index variable */
    size_t *seq_len_arr=NULL;   /* Array of sequence lengths */
    hsize_t *buf_off_arr=NULL;  /* Array of dataset offsets */
    size_t last_io_bytes_left=0;    /* Last I/O bytes left before readv() called */
    size_t nseq=0;              /* Number of sequence/offsets stored in the arrays */
    size_t vector_size;         /* Value for vector size */
    H5P_genplist_t *plist;      /* Property list */
    hssize_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_fread, 0);
#ifdef QAK
printf("%s: Called!\n",FUNC);
#endif /* QAK */

    /* Check args */
    assert(f);
    assert(layout);
    assert(pline);
    assert(fill);
    assert(efl);
    assert(elmt_size>0);
    assert(space);
    assert(iter);
    assert(nelem>0);
    assert(dst);

    /* Set the rank of the fastest changing dimension */
    ndims=space->extent.u.simple.rank;
    fast_dim=(ndims-1);

    /* Get the pointers to the current span info and span nodes */
    curr_span=iter->hyp.span[fast_dim];
    abs_arr=iter->hyp.off;
    off_arr=space->select.offset;
    ispan=iter->hyp.span;

    /* Set the amount of elements to perform I/O on, etc. */
    H5_ASSIGN_OVERFLOW(io_bytes_left,(nelem*elmt_size),hsize_t,size_t);

    /* Compute the cumulative size of dataspace dimensions */
    for(i=fast_dim, acc=elmt_size; i>=0; i--) {
        slab[i]=acc;
        acc*=space->extent.u.simple.size[i];
    } /* end for */

    /* Set the offset of the first element iterated on */
    for(i=0, loc_off=0; i<ndims; i++)
        /* Compute the sequential element offset */
        loc_off+=(abs_arr[i]+space->select.offset[i])*slab[i];

    /* Get the hyperslab vector size */
    if(TRUE!=H5P_isa_class(dxpl_id,H5P_DATASET_XFER) || NULL == (plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
    if (H5P_get(plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, 0, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((seq_len_arr = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate vector I/O array");
    if((buf_off_arr = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate vector I/O array");

    /* Range check against number of elements left in selection */
    assert(io_bytes_left<=(iter->hyp.elmt_left*elmt_size));

    /* Take care of any partial spans leftover from previous I/Os */
    if(abs_arr[fast_dim]!=curr_span->low) {

        /* Finish the span in the fastest changing dimension */

        /* Compute the number of bytes to attempt in this span */
        H5_ASSIGN_OVERFLOW(span_size,((curr_span->high-abs_arr[fast_dim])+1)*elmt_size,hsize_t,size_t);

        /* Check number of bytes against upper bounds allowed */
        if(span_size>io_bytes_left)
            span_size=io_bytes_left;

        if (H5F_seq_read(f, dxpl_id, layout, pline, fill, efl, space,
                elmt_size, span_size, loc_off, dst/*out*/)<0) {
            HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, 0, "read error");
        }

        /* Increment offset in destination */
        dst+=span_size;

        /* Decrement I/O left to perform */
        io_bytes_left-=span_size;

        /* Check if we are done */
        if(io_bytes_left>0) {
            /* Move to next span in fastest changing dimension */
            curr_span=curr_span->next;

            if(curr_span!=NULL) {
                /* Move location offset of destination */
	        loc_off+=(curr_span->low-abs_arr[fast_dim])*elmt_size;

                /* Move iterator for fastest changing dimension */
                abs_arr[fast_dim]=curr_span->low;
            } /* end if */
        } /* end if */
        else {
            abs_arr[fast_dim]+=span_size/elmt_size;

            /* Check if we are still within the span */
            if(abs_arr[fast_dim]<=curr_span->high) {
                iter->hyp.span[fast_dim]=curr_span;

                goto partial_done;      /* finished with partial span */
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[fast_dim]=curr_span->low;
                    iter->hyp.span[fast_dim]=curr_span;

                    goto partial_done;      /* finished with partial span */
                } /* end if */
            } /* end else */
        } /* end else */

        /* Adjust iterator pointers */

        if(curr_span==NULL) {
/* Same as code in main loop */
            /* Start at the next fastest dim */
            curr_dim=fast_dim-1;

            /* Work back up through the dimensions */
            while(curr_dim>=0) {
                /* Reset the current span */
                curr_span=iter->hyp.span[curr_dim];

                /* Increment absolute position */
                abs_arr[curr_dim]++;

                /* Check if we are still within the span */
                if(abs_arr[curr_dim]<=curr_span->high) {
                    break;
                } /* end if */
                /* If we walked off that span, advance to the next span */
                else {
                    /* Advance span in this dimension */
                    curr_span=curr_span->next;

                    /* Check if we have a valid span in this dimension still */
                    if(curr_span!=NULL) {
                        /* Reset absolute position */
                        abs_arr[curr_dim]=curr_span->low;

                        break;
                    } /* end if */
                    else {
                        /* If we finished the span list in this dimension, decrement the dimension worked on and loop again */
                        curr_dim--;
                    } /* end else */
                } /* end else */
            } /* end while */

            /* Check if we are finished with the spans in the tree */
            if(curr_dim<0) {
                /* We had better be done with I/O or bad things are going to happen... */
                assert(io_bytes_left==0);

                goto partial_done;      /* finished with partial span */
            } /* end if */
            else {
                /* Reset the span in the current dimension */
                ispan[curr_dim]=curr_span;

                /* Walk back down the iterator positions, reseting them */
                while(curr_dim<fast_dim) {
                    assert(curr_span);
                    assert(curr_span->down);
                    assert(curr_span->down->head);

                    /* Set the new span_info & span for this dimension */
                    iter->hyp.span[curr_dim+1]=curr_span->down->head;

                    /* Advance span down the tree */
                    curr_span=curr_span->down->head;

                    /* Reset the absolute offset for the dim */
                    abs_arr[curr_dim+1]=curr_span->low;

                    /* Increment current dimension */
                    curr_dim++;
                } /* end while */

                /* Verify that the curr_span points to the fastest dim */
                assert(curr_span==iter->hyp.span[fast_dim]);
            } /* end else */

            /* Reset the buffer offset */
            for(i=0, loc_off=0; i<ndims; i++)
                loc_off+=(abs_arr[i]+off_arr[i])*slab[i];
        } /* end if */
    } /* end if */

partial_done:   /* Yes, goto's are evil, so sue me... :-) */
    /* Get the number of bytes left to process currently */
    last_io_bytes_left=io_bytes_left;

    /* Perform the I/O on the elements, based on the position of the iterator */
    while(io_bytes_left>0) {
        /* Adjust location offset of destination to compensate for initial increment below */
        loc_off-=curr_span->pstride;

        /* Loop over all the spans in the fastest changing dimension */
        while(curr_span!=NULL) {
            /* Move location offset of destination */
            loc_off+=curr_span->pstride;

            /* Compute the number of elements to attempt in this span */
            H5_ASSIGN_OVERFLOW(span_size,curr_span->nelem,hsize_t,size_t);

            /* Check number of elements against upper bounds allowed */
            if(span_size>=io_bytes_left) {
                /* Trim the number of bytes to output */
                span_size=io_bytes_left;
                io_bytes_left=0;

/* COMMON */
                /* Store the I/O information for the span */
                seq_len_arr[nseq]=span_size;
                buf_off_arr[nseq]=loc_off;
            
                /* Increment the number of sequences in arrays */
                nseq++;

                /* If the sequence & offset arrays are full, read them in */
                if(nseq>=vector_size) {
                    /* Read in the sequences */
                    if (H5F_seq_readv(f, dxpl_id, layout, pline, fill, efl, space,
                        elmt_size, nseq, seq_len_arr, buf_off_arr, dst/*out*/)<0) {
                        HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, 0, "read error");
                    } /* end if */

                    /* Increment the offset of the destination buffer */
                    dst+=(last_io_bytes_left-io_bytes_left);

                    /* Keep around the current number of I/O bytes left */
                    last_io_bytes_left=io_bytes_left;
                    nseq=0;
                } /* end else */
/* end COMMON */

                /* Break out now, we are finished with I/O */
                break;
            } /* end if */
            else {
                /* Decrement I/O left to perform */
                io_bytes_left-=span_size;

/* COMMON */
                /* Store the I/O information for the span */
                seq_len_arr[nseq]=span_size;
                buf_off_arr[nseq]=loc_off;
            
                /* Increment the number of sequences in arrays */
                nseq++;

                /* If the sequence & offset arrays are full, read them in */
                if(nseq>=vector_size) {
                    /* Read in the sequences */
                    if (H5F_seq_readv(f, dxpl_id, layout, pline, fill, efl, space,
                        elmt_size, nseq, seq_len_arr, buf_off_arr, dst/*out*/)<0) {
                        HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, 0, "read error");
                    } /* end if */

                    /* Increment the offset of the destination buffer */
                    dst+=(last_io_bytes_left-io_bytes_left);

                    /* Keep around the current number of I/O bytes left */
                    last_io_bytes_left=io_bytes_left;
                    nseq=0;
                } /* end else */
/* end COMMON */
            } /* end else */

	    /* Move to next span in fastest changing dimension */
	    curr_span=curr_span->next;
        } /* end while */

        /* Check if we are done */
        if(io_bytes_left==0) {
            abs_arr[fast_dim]=curr_span->low+(span_size/elmt_size);

            /* Check if we are still within the span */
            if(abs_arr[fast_dim]<=curr_span->high) {
                iter->hyp.span[fast_dim]=curr_span;
                break;
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[fast_dim]=curr_span->low;
                    iter->hyp.span[fast_dim]=curr_span;
                    break;
                } /* end if */
            } /* end else */
        } /* end if */

        /* Adjust iterator pointers */

        /* Start at the next fastest dim */
        curr_dim=fast_dim-1;

        /* Work back up through the dimensions */
        while(curr_dim>=0) {
            /* Reset the current span */
	    curr_span=iter->hyp.span[curr_dim];

            /* Increment absolute position */
            abs_arr[curr_dim]++;

            /* Check if we are still within the span */
            if(abs_arr[curr_dim]<=curr_span->high) {
                break;
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[curr_dim]=curr_span->low;

                    break;
                } /* end if */
                else {
                    /* If we finished the span list in this dimension, decrement the dimension worked on and loop again */
                    curr_dim--;
                } /* end else */
            } /* end else */
        } /* end while */

        /* Check if we are finished with the spans in the tree */
        if(curr_dim<0) {
            /* We had better be done with I/O or bad things are going to happen... */
            assert(io_bytes_left==0);
            break;
        } /* end if */
        else {
            /* Reset the span in the current dimension */
            ispan[curr_dim]=curr_span;

            /* Walk back down the iterator positions, reseting them */
            while(curr_dim<fast_dim) {
                assert(curr_span);
                assert(curr_span->down);
                assert(curr_span->down->head);

                /* Set the new span for the next dimension down */
                iter->hyp.span[curr_dim+1]=curr_span->down->head;

                /* Advance span down the tree */
                curr_span=curr_span->down->head;

                /* Reset the absolute offset for the dim */
                abs_arr[curr_dim+1]=curr_span->low;

                /* Increment current dimension */
                curr_dim++;
            } /* end while */

            /* Verify that the curr_span points to the fastest dim */
            assert(curr_span==iter->hyp.span[fast_dim]);
        } /* end else */

        /* Reset the buffer offset */
        for(i=0, loc_off=0; i<ndims; i++)
            loc_off+=(abs_arr[i]+off_arr[i])*slab[i];
    } /* end while */
        
    /* Check for any stored sequences which need to be flushed */
    if(nseq>0) {
        /* Read in the sequence */
        if (H5F_seq_readv(f, dxpl_id, layout, pline, fill, efl, space,
            elmt_size, nseq, seq_len_arr, buf_off_arr, dst/*out*/)<0) {
            HRETURN_ERROR(H5E_DATASPACE, H5E_READERROR, 0, "read error");
        } /* end if */
    } /* end if */

    /* Increment amount of I/O performed */
    iter->hyp.elmt_left-=nelem;

    /* Success! */
    ret_value=nelem;

done:
    if(seq_len_arr!=NULL)
        H5FL_ARR_FREE(size_t,seq_len_arr);
    if(buf_off_arr!=NULL)
        H5FL_ARR_FREE(hsize_t,buf_off_arr);

    FUNC_LEAVE (ret_value);
} /* H5S_hyper_fread() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_fread_opt
 *
 * Purpose:	Performs an optimized gather from the file, based on a regular
 *      hyperslab (i.e. one which was generated from just one call to
 *      H5Sselect_hyperslab).
 *
 * Return:	Success:	Number of elements copied.
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Friday, September 8, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5S_hyper_fread_opt (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill,
		 const struct H5O_efl_t *efl, size_t elmt_size,
		 const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		 hsize_t nelmts, hid_t dxpl_id, void *_buf/*out*/)
{
    size_t *seq_len_arr=NULL;   /* Array of sequence lengths */
    hsize_t *buf_off_arr=NULL;  /* Array of dataset offsets */
    size_t nseq=0;              /* Number of sequence/offsets stored in the arrays */
    size_t tot_buf_size=0;      /* Total number of bytes in buffer */

    hssize_t offset[H5O_LAYOUT_NDIMS];      /* Offset on disk */
    hsize_t	slab[H5O_LAYOUT_NDIMS];         /* Hyperslab size */
    hssize_t	wrap[H5O_LAYOUT_NDIMS];         /* Bytes to wrap around at the end of a row */
    hsize_t	skip[H5O_LAYOUT_NDIMS];         /* Bytes to skip between blocks */
    hsize_t	tmp_count[H5O_LAYOUT_NDIMS];    /* Temporary block count */
    hsize_t	tmp_block[H5O_LAYOUT_NDIMS];    /* Temporary block offset */
    uint8_t	*buf=(uint8_t *)_buf;   /* Alias for pointer arithmetic */
    const H5S_hyper_dim_t *tdiminfo;    /* Local pointer to diminfo information */
    hssize_t fast_dim_start,            /* Local copies of fastest changing dimension info */
        fast_dim_offset;
    hsize_t fast_dim_stride,            /* Local copies of fastest changing dimension info */
        fast_dim_block,
        fast_dim_buf_off;
    size_t fast_dim_count;
    size_t tot_blk_count;      /* Total number of blocks left to output */
    size_t act_blk_count;      /* Actual number of blocks to output */
    int fast_dim;  /* Rank of the fastest changing dimension for the dataspace */
    int temp_dim;  /* Temporary rank holder */
    hsize_t	acc;	/* Accumulator */
    hsize_t	buf_off;        /* Current buffer offset for copying memory */
    int i;         /* Counters */
    unsigned u;        /* Counters */
    int   	ndims;      /* Number of dimensions of dataset */
    size_t actual_read;     /* The actual number of elements to read in */
    size_t actual_bytes;    /* The actual number of bytes to copy */
    size_t io_left;         /* The number of elements left in I/O operation */
    size_t tot_seq;         /* The number of sequences filled */
    hsize_t *buf_off_arr_p;     /* Pointer into the buffer offset array */
    size_t seq_count;           /* Temporary count of sequences left to process */
#ifndef NO_DUFFS_DEVICE
    size_t duffs_index;         /* Counting index for Duff's device */
#endif /* NO_DUFFS_DEVICE */
    size_t vector_size;         /* Value for vector size */
    H5P_genplist_t *plist;      /* Property list */
    hsize_t ret_value=0;        /* Return value */

    FUNC_ENTER (H5S_hyper_fread_opt, 0);

#ifdef QAK
printf("%s: Called!\n",FUNC);
#endif /* QAK */
    /* Check if this is the first element read in from the hyperslab */
    if(file_iter->hyp.pos[0]==(-1)) {
        for(u=0; u<file_space->extent.u.simple.rank; u++)
            file_iter->hyp.pos[u]=file_space->select.sel_info.hslab.diminfo[u].start;
    } /* end if */

#ifdef QAK
for(i=0; i<file_space->extent.u.simple.rank; i++)
    printf("%s: file_file->hyp.pos[%d]=%d\n",FUNC,(int)i,(int)file_iter->hyp.pos[i]);
#endif /* QAK */

    /* Get the hyperslab vector size */
    if(TRUE!=H5P_isa_class(dxpl_id,H5P_DATASET_XFER) || NULL == (plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a file access property list");
    if (H5P_get(plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, 0, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((seq_len_arr = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate vector I/O array");
    if((buf_off_arr = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate vector I/O array");

    /* Set the rank of the fastest changing dimension */
    fast_dim=file_space->extent.u.simple.rank-1;
    ndims=file_space->extent.u.simple.rank;

    /* initialize row sizes for each dimension */
    for(i=(ndims-1),acc=1; i>=0; i--) {
        slab[i]=acc*elmt_size;
        acc*=file_space->extent.u.simple.size[i];
    } /* end for */

    /* Set the number of elements left for I/O */
    H5_ASSIGN_OVERFLOW(io_left,nelmts,hsize_t,size_t);

#ifdef QAK
    printf("%s: fast_dim=%d\n",FUNC,(int)fast_dim);
    printf("%s: file_space->select.sel_info.hslab.diminfo[%d].start=%d\n",FUNC,(int)fast_dim,(int)file_space->select.sel_info.hslab.diminfo[fast_dim].start);
    printf("%s: file_space->select.sel_info.hslab.diminfo[%d].stride=%d\n",FUNC,(int)fast_dim,(int)file_space->select.sel_info.hslab.diminfo[fast_dim].stride);
#endif /* QAK */
    /* Check if we stopped in the middle of a sequence of elements */
    if((file_iter->hyp.pos[fast_dim]-file_space->select.sel_info.hslab.diminfo[fast_dim].start)%file_space->select.sel_info.hslab.diminfo[fast_dim].stride!=0 ||
        ((file_iter->hyp.pos[fast_dim]!=file_space->select.sel_info.hslab.diminfo[fast_dim].start) && file_space->select.sel_info.hslab.diminfo[fast_dim].stride==1)) {
        hsize_t leftover;  /* The number of elements left over from the last sequence */

#ifdef QAK
printf("%s: Check 1.0\n",FUNC);
#endif /* QAK */
        /* Calculate the number of elements left in the sequence */
        if(file_space->select.sel_info.hslab.diminfo[fast_dim].stride==1)
            leftover=file_space->select.sel_info.hslab.diminfo[fast_dim].block-(file_iter->hyp.pos[fast_dim]-file_space->select.sel_info.hslab.diminfo[fast_dim].start);
        else
            leftover=file_space->select.sel_info.hslab.diminfo[fast_dim].block-((file_iter->hyp.pos[fast_dim]-file_space->select.sel_info.hslab.diminfo[fast_dim].start)%file_space->select.sel_info.hslab.diminfo[fast_dim].stride);

        /* Make certain that we don't read too many */
        actual_read=MIN(leftover,io_left);
        actual_bytes=actual_read*elmt_size;

        /* Copy the location of the point to get */
        HDmemcpy(offset, file_iter->hyp.pos,ndims*sizeof(hssize_t));
        offset[ndims] = 0;

        /* Add in the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] += file_space->select.offset[i];
#ifdef QAK
for(i=0; i<ndims+1; i++)
    printf("%s: offset[%d]=%d\n",FUNC,(int)i,(int)offset[i]);
#endif /* QAK */

        /* Compute the initial buffer offset */
        for(i=0,buf_off=0; i<ndims; i++)
            buf_off+=offset[i]*slab[i];
#ifdef QAK
printf("%s: buf_off=%ld, actual_read=%d, actual_bytes=%d\n",FUNC,(long)buf_off,(int)actual_read,(int)actual_bytes);
#endif /* QAK */

        /* Read in the rest of the sequence */
        if (H5F_seq_read(f, dxpl_id, layout, pline, fill, efl, file_space,
            elmt_size, actual_bytes, buf_off, buf/*out*/)<0) {
            HRETURN_ERROR(H5E_DATASPACE, H5E_READERROR, 0, "read error");
        }

        /* Increment the offset of the buffer */
        buf+=actual_bytes;

        /* Decrement the amount left to read */
        io_left-=actual_read;

        /* Advance the point iterator */
        /* If we had enough buffer space to read in the rest of the sequence
         * in the fastest changing dimension, move the iterator offset to
         * the beginning of the next block to read.  Otherwise, just advance
         * the iterator in the fastest changing dimension.
         */
        if(actual_read==leftover) {
            /* Move iterator offset to beginning of next sequence in the fastest changing dimension */
            H5S_hyper_iter_next(file_space,file_iter);
        } /* end if */
        else {
            file_iter->hyp.pos[fast_dim]+=actual_read; /* whole sequence not read in, just advance fastest dimension offset */
        } /* end if */
    } /* end if */

    /* Now that we've cleared the "remainder" of the previous fastest dimension
     * sequence, we must be at the beginning of a sequence, so use the fancy
     * algorithm to compute the offsets and run through as many as possible,
     * until the buffer fills up.
     */
    if(io_left>0) { /* Just in case the "remainder" above filled the buffer */
#ifdef QAK
printf("%s: Check 2.0, ndims=%d, io_left=%d, nelmts=%d\n",FUNC,(int)ndims,(int)io_left,(int)nelmts);
#endif /* QAK */
        /* Compute the arrays to perform I/O on */
        /* Copy the location of the point to get */
        HDmemcpy(offset, file_iter->hyp.pos,ndims*sizeof(hssize_t));
        offset[ndims] = 0;
#ifdef QAK
for(i=0; i<ndims+1; i++)
    printf("%s: offset[%d]=%d\n",FUNC,(int)i,(int)offset[i]);
#endif /* QAK */

        /* Add in the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] += file_space->select.offset[i];

        /* Compute the current "counts" for this location */
        for(i=0; i<ndims; i++) {
            tmp_count[i] = (file_iter->hyp.pos[i]-file_space->select.sel_info.hslab.diminfo[i].start)%file_space->select.sel_info.hslab.diminfo[i].stride;
            tmp_block[i] = (file_iter->hyp.pos[i]-file_space->select.sel_info.hslab.diminfo[i].start)/file_space->select.sel_info.hslab.diminfo[i].stride;
        } /* end for */
#ifdef QAK
for(i=0; i<ndims; i++) {
    printf("%s: tmp_count[%d]=%d, tmp_block[%d]=%d\n",FUNC,(int)i,(int)tmp_count[i],(int)i,(int)tmp_block[i]);
    printf("%s: slab[%d]=%d\n",FUNC,(int)i,(int)slab[i]);
}
#endif /* QAK */

        /* Compute the initial buffer offset */
        for(i=0,buf_off=0; i<ndims; i++)
            buf_off+=offset[i]*slab[i];

        /* Set the number of elements to read each time */
        H5_ASSIGN_OVERFLOW(actual_read,file_space->select.sel_info.hslab.diminfo[fast_dim].block,hsize_t,size_t);

        /* Set the number of actual bytes */
        actual_bytes=actual_read*elmt_size;
#ifdef QAK
printf("%s: buf_off=%ld, actual_read=%d, actual_bytes=%d\n",FUNC,(long)buf_off,(int)actual_read,(int)actual_bytes);
#endif /* QAK */

#ifdef QAK
for(i=0; i<file_space->extent.u.simple.rank; i++)
    printf("%s: diminfo: start[%d]=%d, stride[%d]=%d, block[%d]=%d, count[%d]=%d\n",FUNC,
        (int)i,(int)file_space->select.sel_info.hslab.diminfo[i].start,
        (int)i,(int)file_space->select.sel_info.hslab.diminfo[i].stride,
        (int)i,(int)file_space->select.sel_info.hslab.diminfo[i].block,
        (int)i,(int)file_space->select.sel_info.hslab.diminfo[i].count);
#endif /* QAK */

        /* Set the local copy of the diminfo pointer */
        tdiminfo=file_space->select.sel_info.hslab.diminfo;

        /* Set local copies of information for the fastest changing dimension */
        fast_dim_start=tdiminfo[fast_dim].start;
        fast_dim_stride=tdiminfo[fast_dim].stride;
        fast_dim_block=tdiminfo[fast_dim].block;
        fast_dim_buf_off=slab[fast_dim]*fast_dim_stride;
        fast_dim_offset=fast_dim_start+file_space->select.offset[fast_dim];

        /* Compute the number of blocks which would fit into the buffer */
        tot_blk_count=io_left/fast_dim_block;

        /* Compute the amount to wrap at the end of each row */
        for(i=0; i<ndims; i++)
            wrap[i]=(file_space->extent.u.simple.size[i]-(tdiminfo[i].stride*tdiminfo[i].count))*slab[i];

        /* Compute the amount to skip between blocks */
        for(i=0; i<ndims; i++)
            skip[i]=(tdiminfo[i].stride-tdiminfo[i].block)*slab[i];

        /* Fill the sequence length array (since they will all be the same for optimized hyperslabs) */
        for(u=0; u<vector_size; u++)
            seq_len_arr[u]=actual_bytes;

        /* Read in data until an entire sequence can't be read in any longer */
        while(io_left>0) {
            /* Reset copy of number of blocks in fastest dimension */
            H5_ASSIGN_OVERFLOW(fast_dim_count,tdiminfo[fast_dim].count-tmp_count[fast_dim],hsize_t,size_t);

            /* Check if this entire row will fit into buffer */
            if(fast_dim_count<=tot_blk_count) {

                /* Entire row of blocks fits into buffer */
                act_blk_count=fast_dim_count;

                /* Loop over all the blocks in the fastest changing dimension */
                while(fast_dim_count>0) {
                    /* Gather the sequence */

                    /* Compute the number of sequences to fill */
                    tot_seq=MIN(vector_size-nseq,fast_dim_count);

                    /* Get a copy of the number of sequences to fill */
                    seq_count=tot_seq;

                    /* Set the pointer to the correct starting array element */
                    buf_off_arr_p=&buf_off_arr[nseq];

#ifdef NO_DUFFS_DEVICE
                    /* Fill up the buffer, or finish up the blocks in this dimension */
                    while(seq_count>0) {
                        /* Store of length & offset */
                        /* seq_len_arr[nseq] already has the correct value */
                        *buf_off_arr_p++=buf_off;

                        /* Increment the source offset */
                        buf_off+=fast_dim_buf_off;

                        seq_count--;
                    } /* end while */
#else /* NO_DUFFS_DEVICE */
                    duffs_index = (seq_count + 7) / 8;
                    switch (seq_count % 8) {
                        case 0:
                            do
                              {
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 7:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 6:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 5:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 4:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 3:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 2:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 1:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                          } while (--duffs_index > 0);
                    } /* end switch */
#endif /* NO_DUFFS_DEVICE */

                    /* Increment number of array elements used */
                    nseq+=tot_seq;

                    /* Increment the total number of bytes contained in arrays */
                    tot_buf_size += tot_seq*actual_bytes;

                    /* Decrement number of blocks left */
                    fast_dim_count -= tot_seq;

                    /* If the sequence & offset arrays are full, read them in */
                    if(nseq>=vector_size) {
                        /* Read in the sequences */
                        if (H5F_seq_readv(f, dxpl_id, layout, pline, fill, efl, file_space,
                            elmt_size, nseq, seq_len_arr, buf_off_arr, buf/*out*/)<0) {
                            HRETURN_ERROR(H5E_DATASPACE, H5E_READERROR, 0, "read error");
                        } /* end if */

                        /* Increment the offset of the destination buffer */
                        buf+=tot_buf_size;

                        /* Reset the number of bytes & sequences */
                        tot_buf_size=0;
                        nseq=0;
                    } /* end else */
                } /* end while */

                /* Decrement number of elements left */
                io_left -= actual_read*act_blk_count;

                /* Decrement number of blocks left */
                tot_blk_count -= act_blk_count;

                /* Increment information to reflect block just processed */
                offset[fast_dim]=fast_dim_offset;    /* reset the offset in the fastest dimension */
                tmp_count[fast_dim]=0;

                /* Increment offset in destination buffer */
                buf_off += wrap[fast_dim];
            } /* end if */
            else {

                /* Entire row of blocks doesn't fit into buffer */
                act_blk_count=tot_blk_count;

                /* Reduce number of blocks to output */
                fast_dim_count=tot_blk_count;

                /* Loop over all the blocks in the fastest changing dimension */
                while(fast_dim_count>0) {
                    /* Gather the sequence */

                    /* Compute the number of sequences to fill */
                    tot_seq=MIN(vector_size-nseq,fast_dim_count);

                    /* Get a copy of the number of sequences to fill */
                    seq_count=tot_seq;

                    /* Set the pointer to the correct starting array element */
                    buf_off_arr_p=&buf_off_arr[nseq];

                    /* Fill up the buffer, or finish up the blocks in this dimension */
                    while(seq_count>0) {
                        /* Store of length & offset */
                        /* seq_len_arr[nseq] already has the correct value */
                        *buf_off_arr_p++=buf_off;

                        /* Increment the source offset */
                        buf_off+=fast_dim_buf_off;

                        seq_count--;
                    } /* end while */

                    /* Increment number of array elements used */
                    nseq+=tot_seq;

                    /* Increment the total number of bytes contained in arrays */
                    tot_buf_size += tot_seq*actual_bytes;

                    /* Decrement number of blocks left */
                    fast_dim_count -= tot_seq;

                    /* If the sequence & offset arrays are full, read them in */
                    if(nseq>=vector_size) {
                        /* Read in the sequences */
                        if (H5F_seq_readv(f, dxpl_id, layout, pline, fill, efl, file_space,
                            elmt_size, nseq, seq_len_arr, buf_off_arr, buf/*out*/)<0) {
                            HRETURN_ERROR(H5E_DATASPACE, H5E_READERROR, 0, "read error");
                        } /* end if */

                        /* Increment the offset of the destination buffer */
                        buf+=tot_buf_size;

                        /* Reset the number of bytes & sequences */
                        tot_buf_size=0;
                        nseq=0;
                    } /* end else */
                } /* end while */

                /* Decrement number of elements left */
                io_left -= actual_read*act_blk_count;

                /* Decrement number of blocks left */
                tot_blk_count -= act_blk_count;

                /* Increment information to reflect block just processed */
                offset[fast_dim]+=(fast_dim_stride*act_blk_count);    /* reset the offset in the fastest dimension */
                tmp_count[fast_dim]+=act_blk_count;

                /* Handle any leftover, partial blocks in this row */
                if(io_left>0) {
                    actual_read=io_left;
                    actual_bytes=actual_read*elmt_size;

                    /* Gather the sequence */

                    /* Store of length & offset */
                    seq_len_arr[nseq]=actual_bytes;
                    buf_off_arr[nseq]=buf_off;

                    /* Increment the total number of bytes contained in arrays */
                    tot_buf_size += actual_bytes;

                    /* Increment the number of sequences in arrays */
                    nseq++;

                    /* If the sequence & offset arrays are full, read them in */
                    if(nseq>=vector_size) {
                        /* Read in the sequences */
                        if (H5F_seq_readv(f, dxpl_id, layout, pline, fill, efl, file_space,
                            elmt_size, nseq, seq_len_arr, buf_off_arr, buf/*out*/)<0) {
                            HRETURN_ERROR(H5E_DATASPACE, H5E_READERROR, 0, "read error");
                        } /* end if */

                        /* Increment the offset of the destination buffer */
                        buf+=tot_buf_size;

                        /* Reset the number of bytes & sequences */
                        tot_buf_size=0;
                        nseq=0;
                    } /* end else */

                    /* Increment the source offset */
                    buf_off+=fast_dim_buf_off;

                    /* Decrement the number of elements left */
                    io_left -= actual_read;

                    /* Increment buffer correctly */
                    offset[fast_dim]+=actual_read;
                } /* end if */

                /* don't bother checking slower dimensions */
                assert(tot_blk_count==0);
                assert(io_left==0);
                break;
            } /* end else */

            /* Increment the offset and count for the other dimensions */
            temp_dim=fast_dim-1;
            while(temp_dim>=0) {
                /* Move to the next row in the curent dimension */
                offset[temp_dim]++;
                tmp_block[temp_dim]++;

                /* If this block is still in the range of blocks to output for the dimension, break out of loop */
                if(tmp_block[temp_dim]<tdiminfo[temp_dim].block)
                    break;
                else {
                    /* Move to the next block in the current dimension */
                    offset[temp_dim]+=(tdiminfo[temp_dim].stride-tdiminfo[temp_dim].block);
                    buf_off += skip[temp_dim];
                    tmp_block[temp_dim]=0;
                    tmp_count[temp_dim]++;

                    /* If this block is still in the range of blocks to output for the dimension, break out of loop */
                    if(tmp_count[temp_dim]<tdiminfo[temp_dim].count)
                        break;
                    else {
                        offset[temp_dim]=tdiminfo[temp_dim].start+file_space->select.offset[temp_dim];
                        buf_off += wrap[temp_dim];
                        tmp_count[temp_dim]=0; /* reset back to the beginning of the line */
                        tmp_block[temp_dim]=0;
                    } /* end else */
                } /* end else */

                /* Decrement dimension count */
                temp_dim--;
            } /* end while */
        } /* end while */

        /* Check for any stored sequences which need to be flushed */
        if(nseq>0) {
            /* Read in the sequence */
            if (H5F_seq_readv(f, dxpl_id, layout, pline, fill, efl, file_space,
                elmt_size, nseq, seq_len_arr, buf_off_arr, buf/*out*/)<0) {
                HRETURN_ERROR(H5E_DATASPACE, H5E_READERROR, 0, "read error");
            } /* end if */
        } /* end if */

        /* Subtract out the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] -= file_space->select.offset[i];

        /* Update the iterator with the location we stopped */
        HDmemcpy(file_iter->hyp.pos, offset, ndims*sizeof(hssize_t));
    } /* end if */

    /* Decrement the number of elements left in selection */
    file_iter->hyp.elmt_left -= (nelmts-io_left);

    /* Set the return value */
    ret_value= (nelmts-io_left);

done:
    if(seq_len_arr!=NULL)
        H5FL_ARR_FREE(size_t,seq_len_arr);
    if(buf_off_arr!=NULL)
        H5FL_ARR_FREE(hsize_t,buf_off_arr);

    FUNC_LEAVE (ret_value);
} /* H5S_hyper_fread_opt() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_fgath
 *
 * Purpose:	Gathers data points from file F and accumulates them in the
 *		type conversion buffer BUF.  The LAYOUT argument describes
 *		how the data is stored on disk and EFL describes how the data
 *		is organized in external files.  ELMT_SIZE is the size in
 *		bytes of a datum which this function treats as opaque.
 *		FILE_SPACE describes the data space of the dataset on disk
 *		and the elements that have been selected for reading (via
 *		hyperslab, etc).  This function will copy at most NELMTS elements.
 *
 * Return:	Success:	Number of elements copied.
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-08-03
 *		The data transfer properties are passed by ID since that's
 *		what the virtual file layer needs.
 *-------------------------------------------------------------------------
 */
static hsize_t
H5S_hyper_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill,
		 const struct H5O_efl_t *efl, size_t elmt_size,
		 const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		 hsize_t nelmts, hid_t dxpl_id, void *_buf/*out*/)
{
    hsize_t  num_read=0;       /* number of elements read into buffer */
    herr_t  ret_value=SUCCEED;

    FUNC_ENTER (H5S_hyper_fgath, 0);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (_buf);

    /* Check for the special case of just one H5Sselect_hyperslab call made */
    if(file_space->select.sel_info.hslab.diminfo!=NULL) {
        /* Use optimized call to read in regular hyperslab */
        num_read=H5S_hyper_fread_opt(f,layout,pline,fill,efl,elmt_size,file_space,file_iter,nelmts,dxpl_id,_buf);
    } /* end if */
    else {
        /* Perform generic hyperslab operation */
        num_read=H5S_hyper_fread(f,layout,pline,fill,efl,elmt_size,file_space,file_iter,nelmts,dxpl_id,_buf);
    } /* end else */

    FUNC_LEAVE (ret_value==SUCCEED ? num_read : 0);
} /* H5S_hyper_fgath() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_fwrite
 *
 * Purpose:	Performs an optimized scatter to the file, based on a hyperslab
 *      span selection.
 *
 * Return:	Success:	Number of elements copied.
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 12, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5S_hyper_fwrite (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill,
		 const struct H5O_efl_t *efl, size_t elmt_size,
		 const H5S_t *space, H5S_sel_iter_t *iter,
		 hsize_t nelem, hid_t dxpl_id, const void *_buf)
{
    const uint8_t	*src=(const uint8_t *)_buf;   /* Alias for pointer arithmetic */
    H5S_hyper_span_t **ispan;       /* Iterator's hyperslab span nodes */
    H5S_hyper_span_t *curr_span;    /* Current hyperslab span node */
    hsize_t slab[H5O_LAYOUT_NDIMS]; /* Cumulative size of each dimension in bytes */
    hsize_t acc;        /* Accumulator for computing cumulative sizes */
    hssize_t *abs_arr;  /* Absolute hyperslab span position */
    hssize_t *off_arr;  /* Offset within the dataspace extent */
    int fast_dim;      /* Rank of the fastest changing dimension for the dataspace */
    int curr_dim;      /* Current dimension being operated on */
    int ndims;         /* Number of dimensions of dataset */
    hsize_t loc_off;    /* Element offset in the dataspace */
    size_t span_size;  /* Number of bytes in current span to actually process */
    size_t io_bytes_left;   /* Number of bytes left to process */
    int i;             /* Index variable */
    size_t *seq_len_arr=NULL;   /* Array of sequence lengths */
    hsize_t *buf_off_arr=NULL;  /* Array of dataset offsets */
    size_t last_io_bytes_left=0;    /* Last I/O bytes left before readv() called */
    size_t nseq=0;              /* Number of sequence/offsets stored in the arrays */
    size_t vector_size;         /* Value for vector size */
    H5P_genplist_t *plist;      /* Property list */
    hssize_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_fwrite, 0);
#ifdef QAK
printf("%s: Called!\n",FUNC);
#endif /* QAK */

    /* Check args */
    assert(f);
    assert(layout);
    assert(pline);
    assert(fill);
    assert(efl);
    assert(elmt_size>0);
    assert(space);
    assert(iter);
    assert(nelem>0);
    assert(src);

    /* Set the rank of the fastest changing dimension */
    ndims=space->extent.u.simple.rank;
    fast_dim=(ndims-1);

    /* Get the pointers to the current span info and span nodes */
    curr_span=iter->hyp.span[fast_dim];
    abs_arr=iter->hyp.off;
    off_arr=space->select.offset;
    ispan=iter->hyp.span;

    /* Set the amount of elements to perform I/O on, etc. */
    H5_ASSIGN_OVERFLOW(io_bytes_left,(nelem*elmt_size),hsize_t,size_t);

    /* Compute the cumulative size of dataspace dimensions */
    for(i=fast_dim, acc=elmt_size; i>=0; i--) {
        slab[i]=acc;
        acc*=space->extent.u.simple.size[i];
    } /* end for */

    /* Set the offset of the first element iterated on */
    for(i=0, loc_off=0; i<ndims; i++)
        /* Compute the sequential element offset */
        loc_off+=(abs_arr[i]+space->select.offset[i])*slab[i];

    /* Range check against number of elements left in selection */
    assert(io_bytes_left<=(iter->hyp.elmt_left*elmt_size));

    /* Get the hyperslab vector size */
    if(TRUE!=H5P_isa_class(dxpl_id,H5P_DATASET_XFER) || NULL == (plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a file access property list");
    if (H5P_get(plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, 0, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((seq_len_arr = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate vector I/O array");
    if((buf_off_arr = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate vector I/O array");

    /* Take care of any partial spans leftover from previous I/Os */
    if(abs_arr[fast_dim]!=curr_span->low) {

        /* Finish the span in the fastest changing dimension */

        /* Compute the number of bytes to attempt in this span */
        H5_ASSIGN_OVERFLOW(span_size,((curr_span->high-abs_arr[fast_dim])+1)*elmt_size,hsize_t,size_t);

        /* Check number of bytes against upper bounds allowed */
        if(span_size>io_bytes_left)
            span_size=io_bytes_left;

        if (H5F_seq_write(f, dxpl_id, layout, pline, fill, efl, space,
            elmt_size, span_size, loc_off, src)<0) {
            HRETURN_ERROR(H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
        } /* end if */

        /* Increment offset in destination */
        src+=span_size;

        /* Decrement I/O left to perform */
        io_bytes_left-=span_size;

        /* Check if we are done */
        if(io_bytes_left>0) {
            /* Move to next span in fastest changing dimension */
            curr_span=curr_span->next;

            if(curr_span!=NULL) {
                /* Move location offset of destination */
	        loc_off+=(curr_span->low-abs_arr[fast_dim])*elmt_size;

                /* Move iterator for fastest changing dimension */
                abs_arr[fast_dim]=curr_span->low;
            } /* end if */
        } /* end if */
        else {
            abs_arr[fast_dim]+=span_size/elmt_size;

            /* Check if we are still within the span */
            if(abs_arr[fast_dim]<=curr_span->high) {
                iter->hyp.span[fast_dim]=curr_span;

                goto partial_done;      /* finished with partial span */
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[fast_dim]=curr_span->low;
                    iter->hyp.span[fast_dim]=curr_span;

                    goto partial_done;      /* finished with partial span */
                } /* end if */
            } /* end else */
        } /* end else */

        /* Adjust iterator pointers */

        if(curr_span==NULL) {
/* Same as code in main loop */
            /* Start at the next fastest dim */
            curr_dim=fast_dim-1;

            /* Work back up through the dimensions */
            while(curr_dim>=0) {
                /* Reset the current span */
                curr_span=iter->hyp.span[curr_dim];

                /* Increment absolute position */
                abs_arr[curr_dim]++;

                /* Check if we are still within the span */
                if(abs_arr[curr_dim]<=curr_span->high) {
                    break;
                } /* end if */
                /* If we walked off that span, advance to the next span */
                else {
                    /* Advance span in this dimension */
                    curr_span=curr_span->next;

                    /* Check if we have a valid span in this dimension still */
                    if(curr_span!=NULL) {
                        /* Reset absolute position */
                        abs_arr[curr_dim]=curr_span->low;

                        break;
                    } /* end if */
                    else {
                        /* If we finished the span list in this dimension, decrement the dimension worked on and loop again */
                        curr_dim--;
                    } /* end else */
                } /* end else */
            } /* end while */

            /* Check if we are finished with the spans in the tree */
            if(curr_dim<0) {
                /* We had better be done with I/O or bad things are going to happen... */
                assert(io_bytes_left==0);

                goto partial_done;      /* finished with partial span */
            } /* end if */
            else {
                /* Reset the span in the current dimension */
                ispan[curr_dim]=curr_span;

                /* Walk back down the iterator positions, reseting them */
                while(curr_dim<fast_dim) {
                    assert(curr_span);
                    assert(curr_span->down);
                    assert(curr_span->down->head);

                    /* Set the new span for this dimension */
                    iter->hyp.span[curr_dim+1]=curr_span->down->head;

                    /* Advance span down the tree */
                    curr_span=curr_span->down->head;

                    /* Reset the absolute offset for the dim */
                    abs_arr[curr_dim+1]=curr_span->low;

                    /* Increment current dimension */
                    curr_dim++;
                } /* end while */

                /* Verify that the curr_span points to the fastest dim */
                assert(curr_span==iter->hyp.span[fast_dim]);
            } /* end else */

            /* Reset the buffer offset */
            for(i=0, loc_off=0; i<ndims; i++)
                loc_off+=(abs_arr[i]+off_arr[i])*slab[i];
        } /* end if */
    } /* end if */

partial_done:   /* Yes, goto's are evil, so sue me... :-) */
    /* Get the number of bytes left to process currently */
    last_io_bytes_left=io_bytes_left;

    /* Perform the I/O on the elements, based on the position of the iterator */
    while(io_bytes_left>0) {
        /* Adjust location offset of destination to compensate for initial increment below */
        loc_off-=curr_span->pstride;

        /* Loop over all the spans in the fastest changing dimension */
        while(curr_span!=NULL) {
            /* Move location offset of destination */
            loc_off+=curr_span->pstride;

            /* Compute the number of elements to attempt in this span */
            H5_ASSIGN_OVERFLOW(span_size,curr_span->nelem,hsize_t,size_t);

            /* Check number of elements against upper bounds allowed */
            if(span_size>=io_bytes_left) {
                /* Trim the number of bytes to output */
                span_size=io_bytes_left;
                io_bytes_left=0;

/* COMMON */
                /* Store the I/O information for the span */
                seq_len_arr[nseq]=span_size;
                buf_off_arr[nseq]=loc_off;
            
                /* Increment the number of sequences in arrays */
                nseq++;

                /* If the sequence & offset arrays are full, read them in */
                if(nseq>=vector_size) {
                    /* Write out the sequences */
                    if (H5F_seq_writev(f, dxpl_id, layout, pline, fill, efl, space,
                        elmt_size, nseq, seq_len_arr, buf_off_arr, src)<0) {
                        HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
                    } /* end if */

                    /* Increment the offset of the destination buffer */
                    src+=(last_io_bytes_left-io_bytes_left);

                    /* Keep around the current number of I/O bytes left */
                    last_io_bytes_left=io_bytes_left;
                    nseq=0;
                } /* end else */
/* end COMMON */

                /* Break out now, we are finished with I/O */
                break;
            } /* end if */
            else {
                /* Decrement I/O left to perform */
                io_bytes_left-=span_size;

/* COMMON */
                /* Store the I/O information for the span */
                seq_len_arr[nseq]=span_size;
                buf_off_arr[nseq]=loc_off;
            
                /* Increment the number of sequences in arrays */
                nseq++;

                /* If the sequence & offset arrays are full, read them in */
                if(nseq>=vector_size) {
                    /* Write out the sequences */
                    if (H5F_seq_writev(f, dxpl_id, layout, pline, fill, efl, space,
                        elmt_size, nseq, seq_len_arr, buf_off_arr, src)<0) {
                        HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
                    } /* end if */

                    /* Increment the offset of the destination buffer */
                    src+=(last_io_bytes_left-io_bytes_left);

                    /* Keep around the current number of I/O bytes left */
                    last_io_bytes_left=io_bytes_left;
                    nseq=0;
                } /* end else */
/* end COMMON */
            } /* end else */

	    /* Move to next span in fastest changing dimension */
	    curr_span=curr_span->next;
        } /* end while */

        /* Check if we are done */
        if(io_bytes_left==0) {
            abs_arr[fast_dim]=curr_span->low+(span_size/elmt_size);

            /* Check if we are still within the span */
            if(abs_arr[fast_dim]<=curr_span->high) {
                iter->hyp.span[fast_dim]=curr_span;
                break;
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[fast_dim]=curr_span->low;
                    iter->hyp.span[fast_dim]=curr_span;
                    break;
                } /* end if */
            } /* end else */
        } /* end if */

        /* Adjust iterator pointers */

        /* Start at the next fastest dim */
        curr_dim=fast_dim-1;

        /* Work back up through the dimensions */
        while(curr_dim>=0) {
            /* Reset the current span */
	    curr_span=iter->hyp.span[curr_dim];

            /* Increment absolute position */
            abs_arr[curr_dim]++;

            /* Check if we are still within the span */
            if(abs_arr[curr_dim]<=curr_span->high) {
                break;
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[curr_dim]=curr_span->low;

                    break;
                } /* end if */
                else {
                    /* If we finished the span list in this dimension, decrement the dimension worked on and loop again */
                    curr_dim--;
                } /* end else */
            } /* end else */
        } /* end while */

        /* Check if we are finished with the spans in the tree */
        if(curr_dim<0) {
            /* We had better be done with I/O or bad things are going to happen... */
            assert(io_bytes_left==0);
            break;
        } /* end if */
        else {
            /* Reset the span in the current dimension */
            ispan[curr_dim]=curr_span;

            /* Walk back down the iterator positions, reseting them */
            while(curr_dim<fast_dim) {
                assert(curr_span);
                assert(curr_span->down);
                assert(curr_span->down->head);

                /* Set the new span for the next dimension down */
                iter->hyp.span[curr_dim+1]=curr_span->down->head;

                /* Advance span down the tree */
                curr_span=curr_span->down->head;

                /* Reset the absolute offset for the dim */
                abs_arr[curr_dim+1]=curr_span->low;

                /* Increment current dimension */
                curr_dim++;
            } /* end while */

            /* Verify that the curr_span points to the fastest dim */
            assert(curr_span==iter->hyp.span[fast_dim]);
        } /* end else */

        /* Reset the buffer offset */
        for(i=0, loc_off=0; i<ndims; i++)
            loc_off+=(abs_arr[i]+off_arr[i])*slab[i];
    } /* end while */

    /* Check for any stored sequences which need to be flushed */
    if(nseq>0) {
        /* Write out the sequence */
        if (H5F_seq_writev(f, dxpl_id, layout, pline, fill, efl, space,
            elmt_size, nseq, seq_len_arr, buf_off_arr, src)<0) {
            HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
        } /* end if */
    } /* end if */

    /* Increment amount of I/O performed */
    iter->hyp.elmt_left-=nelem;

    /* Success! */
    ret_value=nelem;

done:
    if(seq_len_arr!=NULL)
        H5FL_ARR_FREE(size_t,seq_len_arr);
    if(buf_off_arr!=NULL)
        H5FL_ARR_FREE(hsize_t,buf_off_arr);

    FUNC_LEAVE (ret_value);
} /* H5S_hyper_fwrite() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_fwrite_opt
 *
 * Purpose:	Performs an optimized scatter to the file, based on a regular
 *      hyperslab (i.e. one which was generated from just one call to
 *      H5Sselect_hyperslab).
 *
 * Return:	Success:	Number of elements copied.
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Friday, July 6, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5S_hyper_fwrite_opt (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill,
		 const struct H5O_efl_t *efl, size_t elmt_size,
		 const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		 hsize_t nelmts, hid_t dxpl_id, const void *_buf)
{
    size_t *seq_len_arr=NULL;      /* Array of sequence lengths */
    hsize_t *buf_off_arr=NULL;     /* Array of dataset offsets */
    size_t nseq=0;              /* Number of sequence/offsets stored in the arrays */
    size_t tot_buf_size=0;      /* Total number of bytes in buffer */

    hssize_t offset[H5O_LAYOUT_NDIMS];      /* Offset on disk */
    hsize_t	slab[H5O_LAYOUT_NDIMS];         /* Hyperslab size */
    hssize_t	wrap[H5O_LAYOUT_NDIMS];         /* Bytes to wrap around at the end of a row */
    hsize_t	skip[H5O_LAYOUT_NDIMS];         /* Bytes to skip between blocks */
    hsize_t	tmp_count[H5O_LAYOUT_NDIMS];    /* Temporary block count */
    hsize_t	tmp_block[H5O_LAYOUT_NDIMS];    /* Temporary block offset */
    const uint8_t	*buf=_buf;      /* Alias for pointer arithmetic */
    const H5S_hyper_dim_t *tdiminfo;    /* Local pointer to diminfo information */
    hssize_t fast_dim_start,            /* Local copies of fastest changing dimension info */
        fast_dim_offset;
    hsize_t fast_dim_stride,            /* Local copies of fastest changing dimension info */
        fast_dim_block,
        fast_dim_buf_off;
    size_t fast_dim_count;
    size_t tot_blk_count;      /* Total number of blocks left to output */
    size_t act_blk_count;      /* Actual number of blocks to output */
    int fast_dim;  /* Rank of the fastest changing dimension for the dataspace */
    int temp_dim;  /* Temporary rank holder */
    hsize_t	acc;	/* Accumulator */
    hsize_t	buf_off;        /* Current buffer offset for copying memory */
    int i;         /* Counters */
    unsigned u;        /* Counters */
    int   	ndims;      /* Number of dimensions of dataset */
    size_t actual_write;     /* The actual number of elements to write out */
    size_t actual_bytes;    /* The actual number of bytes to copy */
    size_t io_left;         /* The number of elements left in I/O operation */
    size_t tot_seq;         /* The number of sequences filled */
    hsize_t *buf_off_arr_p;     /* Pointer into the buffer offset array */
    size_t seq_count;           /* Temporary count of sequences left to process */
#ifndef NO_DUFFS_DEVICE
    size_t duffs_index;         /* Counting index for Duff's device */
#endif /* NO_DUFFS_DEVICE */
    size_t vector_size;         /* Value for vector size */
    H5P_genplist_t *plist;      /* Property list */
    hsize_t ret_value=0;        /* Return value */

    FUNC_ENTER (H5S_hyper_fwrite_opt, 0);

#ifdef QAK
printf("%s: Called!, file_iter->hyp.pos[0]==%d\n",FUNC,(int)file_iter->hyp.pos[0]);
#endif /* QAK */
    /* Check if this is the first element written from the hyperslab */
    if(file_iter->hyp.pos[0]==(-1)) {
        for(u=0; u<file_space->extent.u.simple.rank; u++)
            file_iter->hyp.pos[u]=file_space->select.sel_info.hslab.diminfo[u].start;
    } /* end if */

#ifdef QAK
for(i=0; i<file_space->extent.u.simple.rank; i++)
    printf("%s: file_file->hyp.pos[%d]=%d\n",FUNC,(int)i,(int)file_iter->hyp.pos[i]);
#endif /* QAK */

    /* Get the hyperslab vector size */
    if(TRUE!=H5P_isa_class(dxpl_id,H5P_DATASET_XFER) || NULL == (plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a file access property list");
    if (H5P_get(plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, 0, "unable to get value");

    /* Allocate the vector I/O arrays */
    if((seq_len_arr = H5FL_ARR_ALLOC(size_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate vector I/O array");
    if((buf_off_arr = H5FL_ARR_ALLOC(hsize_t,vector_size,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "can't allocate vector I/O array");

    /* Set the rank of the fastest changing dimension */
    fast_dim=file_space->extent.u.simple.rank-1;
    ndims=file_space->extent.u.simple.rank;

    /* initialize row sizes for each dimension */
    for(i=(ndims-1),acc=1; i>=0; i--) {
        slab[i]=acc*elmt_size;
        acc*=file_space->extent.u.simple.size[i];
    } /* end for */

    /* Set the number of elements left for I/O */
    H5_ASSIGN_OVERFLOW(io_left,nelmts,hsize_t,size_t);

#ifdef QAK
    printf("%s: fast_dim=%d\n",FUNC,(int)fast_dim);
    printf("%s: file_space->select.sel_info.hslab.diminfo[%d].start=%d\n",FUNC,(int)fast_dim,(int)file_space->select.sel_info.hslab.diminfo[fast_dim].start);
    printf("%s: file_space->select.sel_info.hslab.diminfo[%d].stride=%d\n",FUNC,(int)fast_dim,(int)file_space->select.sel_info.hslab.diminfo[fast_dim].stride);
#endif /* QAK */
    /* Check if we stopped in the middle of a sequence of elements */
    if((file_iter->hyp.pos[fast_dim]-file_space->select.sel_info.hslab.diminfo[fast_dim].start)%file_space->select.sel_info.hslab.diminfo[fast_dim].stride!=0 ||
        ((file_iter->hyp.pos[fast_dim]!=file_space->select.sel_info.hslab.diminfo[fast_dim].start) && file_space->select.sel_info.hslab.diminfo[fast_dim].stride==1)) {
        hsize_t leftover;  /* The number of elements left over from the last sequence */

#ifdef QAK
printf("%s: Check 1.0\n",FUNC);
#endif /* QAK */
        /* Calculate the number of elements left in the sequence */
        if(file_space->select.sel_info.hslab.diminfo[fast_dim].stride==1)
            leftover=file_space->select.sel_info.hslab.diminfo[fast_dim].block-(file_iter->hyp.pos[fast_dim]-file_space->select.sel_info.hslab.diminfo[fast_dim].start);
        else
            leftover=file_space->select.sel_info.hslab.diminfo[fast_dim].block-((file_iter->hyp.pos[fast_dim]-file_space->select.sel_info.hslab.diminfo[fast_dim].start)%file_space->select.sel_info.hslab.diminfo[fast_dim].stride);

        /* Make certain that we don't write too many */
        actual_write=MIN(leftover,io_left);
        actual_bytes=actual_write*elmt_size;

        /* Copy the location of the point to get */
        HDmemcpy(offset, file_iter->hyp.pos,ndims*sizeof(hssize_t));
        offset[ndims] = 0;

        /* Add in the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] += file_space->select.offset[i];
#ifdef QAK
for(i=0; i<ndims+1; i++)
    printf("%s: offset[%d]=%d\n",FUNC,(int)i,(int)offset[i]);
#endif /* QAK */

        /* Compute the initial buffer offset */
        for(i=0,buf_off=0; i<ndims; i++)
            buf_off+=offset[i]*slab[i];
#ifdef QAK
printf("%s: buf_off=%ld, actual_write=%d, actual_bytes=%d\n",FUNC,(long)buf_off,(int)actual_write,(int)actual_bytes);
#endif /* QAK */

        /* Write out the rest of the sequence */
        if (H5F_seq_write(f, dxpl_id, layout, pline, fill, efl, file_space,
            elmt_size, actual_bytes, buf_off, buf)<0) {
            HRETURN_ERROR(H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
        }

        /* Increment the offset of the buffer */
        buf+=actual_bytes;

        /* Decrement the amount left to write */
        io_left-=actual_write;

        /* Advance the point iterator */
        /* If we had enough buffer space to write out the rest of the sequence
         * in the fastest changing dimension, move the iterator offset to
         * the beginning of the next block to write.  Otherwise, just advance
         * the iterator in the fastest changing dimension.
         */
        if(actual_write==leftover) {
            /* Move iterator offset to beginning of next sequence in the fastest changing dimension */
            H5S_hyper_iter_next(file_space,file_iter);
        } /* end if */
        else {
            file_iter->hyp.pos[fast_dim]+=actual_write; /* whole sequence not written out, just advance fastest dimension offset */
        } /* end if */
    } /* end if */

    /* Now that we've cleared the "remainder" of the previous fastest dimension
     * sequence, we must be at the beginning of a sequence, so use the fancy
     * algorithm to compute the offsets and run through as many as possible,
     * until the buffer runs dry.
     */
    if(io_left>0) { /* Just in case the "remainder" above emptied the buffer */
#ifdef QAK
printf("%s: Check 2.0, ndims=%d, io_left=%d, nelmts=%d\n",FUNC,(int)ndims,(int)io_left,(int)nelmts);
#endif /* QAK */
        /* Compute the arrays to perform I/O on */
        /* Copy the location of the point to get */
        HDmemcpy(offset, file_iter->hyp.pos,ndims*sizeof(hssize_t));
        offset[ndims] = 0;
#ifdef QAK
for(i=0; i<ndims+1; i++)
    printf("%s: offset[%d]=%d\n",FUNC,(int)i,(int)offset[i]);
#endif /* QAK */

        /* Add in the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] += file_space->select.offset[i];

        /* Compute the current "counts" for this location */
        for(i=0; i<ndims; i++) {
            tmp_count[i] = (file_iter->hyp.pos[i]-file_space->select.sel_info.hslab.diminfo[i].start)%file_space->select.sel_info.hslab.diminfo[i].stride;
            tmp_block[i] = (file_iter->hyp.pos[i]-file_space->select.sel_info.hslab.diminfo[i].start)/file_space->select.sel_info.hslab.diminfo[i].stride;
        } /* end for */
#ifdef QAK
for(i=0; i<ndims; i++) {
    printf("%s: tmp_count[%d]=%d, tmp_block[%d]=%d\n",FUNC,(int)i,(int)tmp_count[i],(int)i,(int)tmp_block[i]);
    printf("%s: slab[%d]=%d\n",FUNC,(int)i,(int)slab[i]);
}
#endif /* QAK */

        /* Compute the initial buffer offset */
        for(i=0,buf_off=0; i<ndims; i++)
            buf_off+=offset[i]*slab[i];

        /* Set the number of elements to write each time */
        H5_ASSIGN_OVERFLOW(actual_write,file_space->select.sel_info.hslab.diminfo[fast_dim].block,hsize_t,size_t);

        /* Set the number of actual bytes */
        actual_bytes=actual_write*elmt_size;
#ifdef QAK
printf("%s: buf_off=%ld, actual_write=%d, actual_bytes=%d\n",FUNC,(long)buf_off,(int)actual_write,(int)actual_bytes);
#endif /* QAK */

#ifdef QAK
for(i=0; i<file_space->extent.u.simple.rank; i++)
    printf("%s: diminfo: start[%d]=%d, stride[%d]=%d, block[%d]=%d, count[%d]=%d\n",FUNC,
        (int)i,(int)file_space->select.sel_info.hslab.diminfo[i].start,
        (int)i,(int)file_space->select.sel_info.hslab.diminfo[i].stride,
        (int)i,(int)file_space->select.sel_info.hslab.diminfo[i].block,
        (int)i,(int)file_space->select.sel_info.hslab.diminfo[i].count);
#endif /* QAK */

        /* Set the local copy of the diminfo pointer */
        tdiminfo=file_space->select.sel_info.hslab.diminfo;

        /* Set local copies of information for the fastest changing dimension */
        fast_dim_start=tdiminfo[fast_dim].start;
        fast_dim_stride=tdiminfo[fast_dim].stride;
        fast_dim_block=tdiminfo[fast_dim].block;
        fast_dim_buf_off=slab[fast_dim]*fast_dim_stride;
        fast_dim_offset=fast_dim_start+file_space->select.offset[fast_dim];

        /* Compute the number of blocks which would fit into the buffer */
        tot_blk_count=io_left/fast_dim_block;

        /* Compute the amount to wrap at the end of each row */
        for(i=0; i<ndims; i++)
            wrap[i]=(file_space->extent.u.simple.size[i]-(tdiminfo[i].stride*tdiminfo[i].count))*slab[i];

        /* Compute the amount to skip between blocks */
        for(i=0; i<ndims; i++)
            skip[i]=(tdiminfo[i].stride-tdiminfo[i].block)*slab[i];

        /* Fill the sequence length array (since they will all be the same for optimized hyperslabs) */
        for(u=0; u<vector_size; u++)
            seq_len_arr[u]=actual_bytes;

        /* Write out data until an entire sequence can't be written any longer */
        while(io_left>0) {
            /* Reset copy of number of blocks in fastest dimension */
            H5_ASSIGN_OVERFLOW(fast_dim_count,tdiminfo[fast_dim].count-tmp_count[fast_dim],hsize_t,size_t);

            /* Check if this entire row will fit into buffer */
            if(fast_dim_count<=tot_blk_count) {

                /* Entire row of blocks fits into buffer */
                act_blk_count=fast_dim_count;

                /* Loop over all the blocks in the fastest changing dimension */
                while(fast_dim_count>0) {
                    /* Gather the sequence */

                    /* Compute the number of sequences to fill */
                    tot_seq=MIN(vector_size-nseq,fast_dim_count);

                    /* Get a copy of the number of sequences to fill */
                    seq_count=tot_seq;

                    /* Set the pointer to the correct starting array element */
                    buf_off_arr_p=&buf_off_arr[nseq];

#ifdef NO_DUFFS_DEVICE
                    /* Fill up the buffer, or finish up the blocks in this dimension */
                    while(seq_count>0) {
                        /* Store of length & offset */
                        /* seq_len_arr[nseq] already has the correct value */
                        *buf_off_arr_p++=buf_off;

                        /* Increment the source offset */
                        buf_off+=fast_dim_buf_off;

                        seq_count--;
                    } /* end while */
#else /* NO_DUFFS_DEVICE */
                    duffs_index = (seq_count + 7) / 8;
                    switch (seq_count % 8) {
                        case 0:
                            do
                              {
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 7:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 6:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 5:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 4:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 3:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 2:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                        case 1:
                                /* Store of length & offset */
                                /* seq_len_arr[nseq] already has the correct value */
                                *buf_off_arr_p++=buf_off;

                                /* Increment the source offset */
                                buf_off+=fast_dim_buf_off;

                          } while (--duffs_index > 0);
                    } /* end switch */
#endif /* NO_DUFFS_DEVICE */

                    /* Increment number of array elements used */
                    nseq+=tot_seq;

                    /* Increment the total number of bytes contained in arrays */
                    tot_buf_size += tot_seq*actual_bytes;

                    /* Decrement number of blocks left */
                    fast_dim_count -= tot_seq;

                    /* If the sequence & offset arrays are full, write them out */
                    if(nseq>=vector_size) {
                        /* Write out the sequences */
                        if (H5F_seq_writev(f, dxpl_id, layout, pline, fill, efl, file_space,
                            elmt_size, nseq, seq_len_arr, buf_off_arr, buf)<0) {
                                HRETURN_ERROR(H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
                        } /* end if */

                        /* Increment the offset of the destination buffer */
                        buf+=tot_buf_size;

                        /* Reset the number of bytes & sequences */
                        tot_buf_size=0;
                        nseq=0;
                    } /* end else */
                } /* end while */

                /* Decrement number of elements left */
                io_left -= actual_write*act_blk_count;

                /* Decrement number of blocks left */
                tot_blk_count -= act_blk_count;

                /* Increment information to reflect block just processed */
                offset[fast_dim]=fast_dim_offset;    /* reset the offset in the fastest dimension */
                tmp_count[fast_dim]=0;

                /* Increment offset in destination buffer */
                buf_off += wrap[fast_dim];
            } /* end if */
            else {

                /* Entire row of blocks doesn't fit into buffer */
                act_blk_count=tot_blk_count;

                /* Reduce number of blocks to output */
                fast_dim_count=tot_blk_count;

                /* Loop over all the blocks in the fastest changing dimension */
                while(fast_dim_count>0) {
                    /* Gather the sequence */

                    /* Compute the number of sequences to fill */
                    tot_seq=MIN(vector_size-nseq,fast_dim_count);

                    /* Get a copy of the number of sequences to fill */
                    seq_count=tot_seq;

                    /* Set the pointer to the correct starting array element */
                    buf_off_arr_p=&buf_off_arr[nseq];

                    /* Fill up the buffer, or finish up the blocks in this dimension */
                    while(seq_count>0) {
                        /* Store of length & offset */
                        /* seq_len_arr[nseq] already has the correct value */
                        *buf_off_arr_p++=buf_off;

                        /* Increment the source offset */
                        buf_off+=fast_dim_buf_off;

                        seq_count--;
                    } /* end while */

                    /* Increment number of array elements used */
                    nseq+=tot_seq;

                    /* Increment the total number of bytes contained in arrays */
                    tot_buf_size += tot_seq*actual_bytes;

                    /* Decrement number of blocks left */
                    fast_dim_count -= tot_seq;

                    /* If the sequence & offset arrays are full, write them out */
                    if(nseq>=vector_size) {
                        /* Write out the sequences */
                        if (H5F_seq_writev(f, dxpl_id, layout, pline, fill, efl, file_space,
                            elmt_size, nseq, seq_len_arr, buf_off_arr, buf)<0) {
                            HRETURN_ERROR(H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
                        } /* end if */

                        /* Increment the offset of the destination buffer */
                        buf+=tot_buf_size;

                        /* Reset the number of bytes & sequences */
                        tot_buf_size=0;
                        nseq=0;
                    } /* end else */
                } /* end while */

                /* Decrement number of elements left */
                io_left -= actual_write*act_blk_count;

                /* Decrement number of blocks left */
                tot_blk_count -= act_blk_count;

                /* Increment information to reflect block just processed */
                offset[fast_dim]+=(fast_dim_stride*act_blk_count);    /* reset the offset in the fastest dimension */
                tmp_count[fast_dim]+=act_blk_count;

                /* Handle any leftover, partial blocks in this row */
                if(io_left>0) {
                    actual_write=io_left;
                    actual_bytes=actual_write*elmt_size;

                    /* Gather the sequence */

                    /* Store of length & offset */
                    seq_len_arr[nseq]=actual_bytes;
                    buf_off_arr[nseq]=buf_off;

                    /* Increment the total number of bytes contained in arrays */
                    tot_buf_size += actual_bytes;

                    /* Increment the number of sequences in arrays */
                    nseq++;

                    /* If the sequence & offset arrays are full, write them out */
                    if(nseq>=vector_size) {
                        /* Write out the sequences */
                        if (H5F_seq_writev(f, dxpl_id, layout, pline, fill, efl, file_space,
                            elmt_size, nseq, seq_len_arr, buf_off_arr, buf)<0) {
                            HRETURN_ERROR(H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
                        } /* end if */

                        /* Increment the offset of the destination buffer */
                        buf+=tot_buf_size;

                        /* Reset the number of bytes & sequences */
                        tot_buf_size=0;
                        nseq=0;
                    } /* end else */

                    /* Increment the source offset */
                    buf_off+=fast_dim_buf_off;

                    /* Decrement the number of elements left */
                    io_left -= actual_write;

                    /* Increment buffer correctly */
                    offset[fast_dim]+=actual_write;
                } /* end if */

                /* don't bother checking slower dimensions */
                assert(tot_blk_count==0);
                assert(io_left==0);
                break;
            } /* end else */

            /* Increment the offset and count for the other dimensions */
            temp_dim=fast_dim-1;
            while(temp_dim>=0) {
                /* Move to the next row in the curent dimension */
                offset[temp_dim]++;
                tmp_block[temp_dim]++;

                /* If this block is still in the range of blocks to output for the dimension, break out of loop */
                if(tmp_block[temp_dim]<tdiminfo[temp_dim].block)
                    break;
                else {
                    /* Move to the next block in the current dimension */
                    offset[temp_dim]+=(tdiminfo[temp_dim].stride-tdiminfo[temp_dim].block);
                    buf_off += skip[temp_dim];
                    tmp_block[temp_dim]=0;
                    tmp_count[temp_dim]++;

                    /* If this block is still in the range of blocks to output for the dimension, break out of loop */
                    if(tmp_count[temp_dim]<tdiminfo[temp_dim].count)
                        break;
                    else {
                        offset[temp_dim]=tdiminfo[temp_dim].start+file_space->select.offset[temp_dim];
                        buf_off += wrap[temp_dim];
                        tmp_count[temp_dim]=0; /* reset back to the beginning of the line */
                        tmp_block[temp_dim]=0;
                    } /* end else */
                } /* end else */

                /* Decrement dimension count */
                temp_dim--;
            } /* end while */
        } /* end while */

        /* Check for any stored sequences which need to be flushed */
        if(nseq>0) {
            /* Write out the sequence */
            if (H5F_seq_writev(f, dxpl_id, layout, pline, fill, efl, file_space,
                elmt_size, nseq, seq_len_arr, buf_off_arr, buf)<0) {
                HRETURN_ERROR(H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
            } /* end if */
        } /* end if */

        /* Subtract out the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] -= file_space->select.offset[i];

        /* Update the iterator with the location we stopped */
        HDmemcpy(file_iter->hyp.pos, offset, ndims*sizeof(hssize_t));
    } /* end if */

    /* Decrement the number of elements left in selection */
    file_iter->hyp.elmt_left -= (nelmts-io_left);

    ret_value= (nelmts-io_left);

done:
    if(seq_len_arr!=NULL)
        H5FL_ARR_FREE(size_t,seq_len_arr);
    if(buf_off_arr!=NULL)
        H5FL_ARR_FREE(hsize_t,buf_off_arr);

    FUNC_LEAVE (ret_value);
} /* H5S_hyper_fwrite_opt() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_fscat
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
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-08-03
 *		The data transfer properties are passed by ID since that's
 *		what the virtual file layer needs.
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_hyper_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill,
		 const struct H5O_efl_t *efl, size_t elmt_size,
		 const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		 hsize_t nelmts, hid_t dxpl_id, const void *_buf)
{
    hsize_t  num_written=0;       /* number of elements read into buffer */
    herr_t  ret_value=SUCCEED;

    FUNC_ENTER (H5S_hyper_fscat, 0);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (_buf);

    /* Check for the special case of just one H5Sselect_hyperslab call made */
    if(file_space->select.sel_info.hslab.diminfo!=NULL) {
        /* Use optimized call to write out regular hyperslab */
        num_written=H5S_hyper_fwrite_opt(f,layout,pline,fill,efl,elmt_size,file_space,file_iter,nelmts,dxpl_id,_buf);
    } /* end if */
    else {
        /* Perform generic hyperslab operation */
        num_written=H5S_hyper_fwrite(f,layout,pline,fill,efl,elmt_size,file_space,file_iter,nelmts,dxpl_id,_buf);
    } /* end else */

    FUNC_LEAVE (ret_value==FAIL ? ret_value : (num_written >0) ? SUCCEED : FAIL);
} /* H5S_hyper_fscat() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_mread
 *
 * Purpose:	Performs an optimized gather from a memory buffer, based on a
 *      hyperslab span selection.
 *
 * Return:	Success:	Number of elements copied.
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 12, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
H5S_hyper_mread (const void *_buf, size_t elmt_size, const H5S_t *space,
    H5S_sel_iter_t *iter, hsize_t nelem, void *_tconv_buf/*out*/)
{
    const uint8_t	*src=(const uint8_t *)_buf;   /* Alias for pointer arithmetic */
    const uint8_t	*tmp_src;   /* Alias for pointer arithmetic */
    uint8_t	*dst=(uint8_t *)_tconv_buf;   /* Alias for pointer arithmetic */
    H5S_hyper_span_t **ispan;       /* Iterator's hyperslab span nodes */
    H5S_hyper_span_t *curr_span;    /* Current hyperslab span node */
    hsize_t slab[H5O_LAYOUT_NDIMS]; /* Cumulative size of each dimension in bytes */
    hsize_t acc;        /* Accumulator for computing cumulative sizes */
    hssize_t *abs_arr;  /* Absolute hyperslab span position */
    hssize_t *off_arr;  /* Offset within the dataspace extent */
    int fast_dim;      /* Rank of the fastest changing dimension for the dataspace */
    int curr_dim;      /* Current dimension being operated on */
    int ndims;         /* Number of dimensions of dataset */
    size_t span_size;   /* Number of bytes in current span to actually process */
    size_t io_bytes_left;   /* Number of bytes left to process */
    int i;             /* Index variable */
    hssize_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_mread, FAIL);
#ifdef QAK
printf("%s: Called!\n",FUNC);
#endif /* QAK */

    /* Check args */
    assert(src);
    assert(elmt_size>0);
    assert(space);
    assert(iter);
    assert(nelem>0);
    assert(dst);

    /* Set the rank of the fastest changing dimension */
    ndims=space->extent.u.simple.rank;
    fast_dim=(ndims-1);

    /* Get the pointers to the current span info and span nodes */
    curr_span=iter->hyp.span[fast_dim];
    abs_arr=iter->hyp.off;
    off_arr=space->select.offset;
    ispan=iter->hyp.span;

    /* Set the amount of elements to perform I/O on, etc. */
    H5_ASSIGN_OVERFLOW(io_bytes_left,nelem*elmt_size,hsize_t,size_t);

    /* Compute the cumulative size of dataspace dimensions */
    for(i=fast_dim, acc=elmt_size; i>=0; i--) {
        slab[i]=acc;
        acc*=space->extent.u.simple.size[i];
    } /* end for */

    /* Set the offset of the first element iterated on */
    for(i=0, tmp_src=src; i<ndims; i++)
        /* Compute the sequential element offset */
        tmp_src+=(abs_arr[i]+space->select.offset[i])*slab[i];

    /* Range check against number of elements left in selection */
    assert(io_bytes_left<=(iter->hyp.elmt_left*elmt_size));

    /* Take care of any partial spans leftover from previous I/Os */
    if(abs_arr[fast_dim]!=curr_span->low) {

        /* Finish the span in the fastest changing dimension */

        /* Compute the number of bytes to attempt in this span */
        H5_ASSIGN_OVERFLOW(span_size,((curr_span->high-abs_arr[fast_dim])+1)*elmt_size,hsize_t,size_t);

        /* Check number of elements against upper bounds allowed */
        if(span_size>io_bytes_left)
            span_size=io_bytes_left;

        HDmemcpy(dst,tmp_src,span_size);

        /* Increment offset in destination */
        dst+=span_size;

        /* Decrement I/O left to perform */
        io_bytes_left-=span_size;

        /* Check if we are done */
        if(io_bytes_left>0) {
            /* Move to next span in fastest changing dimension */
            curr_span=curr_span->next;

            if(curr_span!=NULL) {
                /* Move location offset of destination */
	        tmp_src+=(curr_span->low-abs_arr[fast_dim])*elmt_size;

                /* Move iterator for fastest changing dimension */
                abs_arr[fast_dim]=curr_span->low;
            } /* end if */
        } /* end if */
        else {
            abs_arr[fast_dim]+=span_size/elmt_size;

            /* Check if we are still within the span */
            if(abs_arr[fast_dim]<=curr_span->high) {
                iter->hyp.span[fast_dim]=curr_span;

                goto partial_done;      /* finished with partial span */
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[fast_dim]=curr_span->low;
                    iter->hyp.span[fast_dim]=curr_span;

                    goto partial_done;      /* finished with partial span */
                } /* end if */
            } /* end else */
        } /* end else */

        /* Adjust iterator pointers */

        if(curr_span==NULL) {
/* Same as code in main loop */
            /* Start at the next fastest dim */
            curr_dim=fast_dim-1;

            /* Work back up through the dimensions */
            while(curr_dim>=0) {
                /* Reset the current span */
                curr_span=iter->hyp.span[curr_dim];

                /* Increment absolute position */
                abs_arr[curr_dim]++;

                /* Check if we are still within the span */
                if(abs_arr[curr_dim]<=curr_span->high) {
                    break;
                } /* end if */
                /* If we walked off that span, advance to the next span */
                else {
                    /* Advance span in this dimension */
                    curr_span=curr_span->next;

                    /* Check if we have a valid span in this dimension still */
                    if(curr_span!=NULL) {
                        /* Reset absolute position */
                        abs_arr[curr_dim]=curr_span->low;

                        break;
                    } /* end if */
                    else {
                        /* If we finished the span list in this dimension, decrement the dimension worked on and loop again */
                        curr_dim--;
                    } /* end else */
                } /* end else */
            } /* end while */

            /* Check if we are finished with the spans in the tree */
            if(curr_dim<0) {
                /* We had better be done with I/O or bad things are going to happen... */
                assert(io_bytes_left==0);

                goto partial_done;      /* finished with partial span */
            } /* end if */
            else {
                /* Reset the span in the current dimension */
                ispan[curr_dim]=curr_span;

                /* Walk back down the iterator positions, reseting them */
                while(curr_dim<fast_dim) {
                    assert(curr_span);
                    assert(curr_span->down);
                    assert(curr_span->down->head);

                    /* Set the new span for this dimension */
                    iter->hyp.span[curr_dim+1]=curr_span->down->head;

                    /* Advance span down the tree */
                    curr_span=curr_span->down->head;

                    /* Reset the absolute offset for the dim */
                    abs_arr[curr_dim+1]=curr_span->low;

                    /* Increment current dimension */
                    curr_dim++;
                } /* end while */

                /* Verify that the curr_span points to the fastest dim */
                assert(curr_span==iter->hyp.span[fast_dim]);
            } /* end else */

            /* Reset the buffer offset */
            for(i=0, tmp_src=src; i<ndims; i++)
                tmp_src+=(abs_arr[i]+off_arr[i])*slab[i];
        } /* end if */
    } /* end if */

partial_done:   /* Yes, goto's are evil, so sue me... :-) */

    /* Perform the I/O on the elements, based on the position of the iterator */
    while(io_bytes_left>0) {
        /* Adjust buffer offset of source to compensate for initial increment below */
        tmp_src-=curr_span->pstride;

        /* Loop over all the spans in the fastest changing dimension */
        while(curr_span!=NULL) {
            /* Move buffer offset of source */
            tmp_src+=curr_span->pstride;

            /* Compute the number of elements to attempt in this span */
            H5_ASSIGN_OVERFLOW(span_size,curr_span->nelem,hsize_t,size_t);

            /* Check number of elements against upper bounds allowed */
            if(span_size>=io_bytes_left) {
                /* Trim the number of bytes to output */
                span_size=io_bytes_left;
                io_bytes_left=0;

/* COMMON */
                /* "Read" the data from the source buffer */
                HDmemcpy(dst,tmp_src,span_size);

                /* Increment offset in destination */
                dst+=span_size;
/* end COMMON */

                /* Break out now, we are finished with I/O */
                break;
            } /* end if */
            else {
                /* Decrement I/O left to perform */
                io_bytes_left-=span_size;

/* COMMON */
                /* "Read" the data from the source buffer */
                HDmemcpy(dst,tmp_src,span_size);

                /* Increment offset in destination */
                dst+=span_size;
/* end COMMON */
            } /* end else */

	    /* Move to next span in fastest changing dimension */
	    curr_span=curr_span->next;
        } /* end while */

        /* Check if we are done */
        if(io_bytes_left==0) {
            abs_arr[fast_dim]=curr_span->low+(span_size/elmt_size);

            /* Check if we are still within the span */
            if(abs_arr[fast_dim]<=curr_span->high) {
                iter->hyp.span[fast_dim]=curr_span;
                break;
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[fast_dim]=curr_span->low;
                    iter->hyp.span[fast_dim]=curr_span;
                    break;
                } /* end if */
            } /* end else */
        } /* end if */

        /* Adjust iterator pointers */

        /* Start at the next fastest dim */
        curr_dim=fast_dim-1;

        /* Work back up through the dimensions */
        while(curr_dim>=0) {
            /* Reset the current span */
	    curr_span=iter->hyp.span[curr_dim];

            /* Increment absolute position */
            abs_arr[curr_dim]++;

            /* Check if we are still within the span */
            if(abs_arr[curr_dim]<=curr_span->high) {
                break;
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[curr_dim]=curr_span->low;

                    break;
                } /* end if */
                else {
                    /* If we finished the span list in this dimension, decrement the dimension worked on and loop again */
                    curr_dim--;
                } /* end else */
            } /* end else */
        } /* end while */

        /* Check if we are finished with the spans in the tree */
        if(curr_dim<0) {
            /* We had better be done with I/O or bad things are going to happen... */
            assert(io_bytes_left==0);
            break;
        } /* end if */
        else {
            /* Reset the span in the current dimension */
            ispan[curr_dim]=curr_span;

            /* Walk back down the iterator positions, reseting them */
            while(curr_dim<fast_dim) {
                assert(curr_span);
                assert(curr_span->down);
                assert(curr_span->down->head);

                /* Set the new span for the next dimension down */
                iter->hyp.span[curr_dim+1]=curr_span->down->head;

                /* Advance span down the tree */
                curr_span=curr_span->down->head;

                /* Reset the absolute offset for the dim */
                abs_arr[curr_dim+1]=curr_span->low;

                /* Increment current dimension */
                curr_dim++;
            } /* end while */

            /* Verify that the curr_span points to the fastest dim */
            assert(curr_span==iter->hyp.span[fast_dim]);
        } /* end else */

        /* Reset the buffer offset */
        for(i=0, tmp_src=src; i<ndims; i++)
            tmp_src+=(abs_arr[i]+off_arr[i])*slab[i];
    } /* end while */

    /* Increment amount of I/O performed */
    iter->hyp.elmt_left-=nelem;

    /* Success! */
    ret_value=nelem;

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_mread() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_mread_opt
 *
 * Purpose:	Performs an optimized gather from a memory buffer, based on a
 *      regular hyperslab (i.e. one which was generated from just one call to
 *      H5Sselect_hyperslab).
 *
 * Return:	Success:	Number of elements copied.
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 12, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5S_hyper_mread_opt (const void *_buf, size_t elmt_size,
		 const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		 hsize_t nelmts, void *_tconv_buf/*out*/)
{
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];     /* Size of the source buffer */
    hsize_t	slab[H5O_LAYOUT_NDIMS];         /* Hyperslab size */
    hssize_t	wrap[H5O_LAYOUT_NDIMS];         /* Bytes to wrap around at the end of a row */
    hsize_t	skip[H5O_LAYOUT_NDIMS];         /* Bytes to skip between blocks */
    hssize_t offset[H5O_LAYOUT_NDIMS];      /* Offset on disk */
    hsize_t	tmp_count[H5O_LAYOUT_NDIMS];    /* Temporary block count */
    hsize_t	tmp_block[H5O_LAYOUT_NDIMS];    /* Temporary block offset */
    const uint8_t	*src=(const uint8_t *)_buf;   /* Alias for pointer arithmetic */
    uint8_t	*dst=(uint8_t *)_tconv_buf;   /* Alias for pointer arithmetic */
    const H5S_hyper_dim_t *tdiminfo;      /* Temporary pointer to diminfo information */
    hssize_t fast_dim_start,            /* Local copies of fastest changing dimension info */
        fast_dim_offset;
    hsize_t fast_dim_stride,            /* Local copies of fastest changing dimension info */
        fast_dim_block;
    size_t fast_dim_count;
    size_t tot_blk_count;               /* Total number of blocks left to output */
    size_t act_blk_count;               /* Actual number of blocks to output */
    size_t fast_dim_buf_off;            /* Local copy of amount to move fastest dimension buffer offset */
    int fast_dim;  /* Rank of the fastest changing dimension for the dataspace */
    int temp_dim;  /* Temporary rank holder */
    hsize_t	acc;	/* Accumulator */
    int i;         /* Counters */
    unsigned u;         /* Counters */
    int   	ndims;      /* Number of dimensions of dataset */
    size_t actual_read;     /* The actual number of elements to read in */
    size_t actual_bytes;    /* The actual number of bytes to copy */
    size_t io_left;         /* The number of elements left in I/O operation */
#ifndef NO_DUFFS_DEVICE
    size_t duffs_index;     /* Counting index for Duff's device */
#endif /* NO_DUFFS_DEVICE */

    FUNC_ENTER (H5S_hyper_mread_opt, 0);

#ifdef QAK
printf("%s: Called!, nelmts=%lu, elmt_size=%d\n",FUNC,(unsigned long)nelmts,(int)elmt_size);
#endif /* QAK */
    /* Check if this is the first element read in from the hyperslab */
    if(mem_iter->hyp.pos[0]==(-1)) {
        for(u=0; u<mem_space->extent.u.simple.rank; u++)
            mem_iter->hyp.pos[u]=mem_space->select.sel_info.hslab.diminfo[u].start;
    } /* end if */

#ifdef QAK
for(u=0; u<mem_space->extent.u.simple.rank; u++)
    printf("%s: mem_file->hyp.pos[%u]=%d\n",FUNC,(unsigned)u,(int)mem_iter->hyp.pos[u]);
#endif /* QAK */

    /* Set the aliases for a few important dimension ranks */
    fast_dim=mem_space->extent.u.simple.rank-1;
    ndims=mem_space->extent.u.simple.rank;

    /* Set up the size of the memory space */
    HDmemcpy(mem_size, mem_space->extent.u.simple.size,mem_space->extent.u.simple.rank*sizeof(hsize_t));
    mem_size[mem_space->extent.u.simple.rank]=elmt_size;

    /* initialize row sizes for each dimension */
    for(i=(ndims-1),acc=1; i>=0; i--) {
        slab[i]=acc*elmt_size;
        acc*=mem_size[i];
    } /* end for */
#ifdef QAK
for(i=0; i<ndims; i++)
    printf("%s: mem_size[%d]=%d, slab[%d]=%d\n",FUNC,(int)i,(int)mem_size[i],(int)i,(int)slab[i]);
#endif /* QAK */

    /* Set the number of elements left for I/O */
    H5_ASSIGN_OVERFLOW(io_left,nelmts,hsize_t,size_t);

    /* Check if we stopped in the middle of a sequence of elements */
    if((mem_iter->hyp.pos[fast_dim]-mem_space->select.sel_info.hslab.diminfo[fast_dim].start)%mem_space->select.sel_info.hslab.diminfo[fast_dim].stride!=0 ||
            ((mem_iter->hyp.pos[fast_dim]!=mem_space->select.sel_info.hslab.diminfo[fast_dim].start) && mem_space->select.sel_info.hslab.diminfo[fast_dim].stride==1)) {
        size_t leftover;  /* The number of elements left over from the last sequence */

#ifdef QAK
printf("%s: Check 1.0, io_left=%lu\n",FUNC,(unsigned long)io_left);
#endif /* QAK */
        /* Calculate the number of elements left in the sequence */
        if(mem_space->select.sel_info.hslab.diminfo[fast_dim].stride==1)
            leftover=mem_space->select.sel_info.hslab.diminfo[fast_dim].block-(mem_iter->hyp.pos[fast_dim]-mem_space->select.sel_info.hslab.diminfo[fast_dim].start);
        else
            leftover=mem_space->select.sel_info.hslab.diminfo[fast_dim].block-((mem_iter->hyp.pos[fast_dim]-mem_space->select.sel_info.hslab.diminfo[fast_dim].start)%mem_space->select.sel_info.hslab.diminfo[fast_dim].stride);

        /* Make certain that we don't write too many */
        actual_read=MIN(leftover,io_left);
        actual_bytes=actual_read*elmt_size;

        /* Copy the location of the point to get */
        HDmemcpy(offset, mem_iter->hyp.pos,ndims*sizeof(hssize_t));
        offset[ndims] = 0;

        /* Add in the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] += mem_space->select.offset[i];

        /* Compute the initial buffer offset */
        for(i=0,src=_buf; i<ndims; i++)
            src+=offset[i]*slab[i];

        /* Scatter out the rest of the sequence */
        HDmemcpy(dst,src,actual_bytes);

        /* Increment the offset of the buffer */
        dst+=actual_bytes;

        /* Decrement the number of elements written out */
        io_left -= actual_read;

        /* Advance the point iterator */
        /* If we had enough buffer space to read in the rest of the sequence
         * in the fastest changing dimension, move the iterator offset to
         * the beginning of the next block to read.  Otherwise, just advance
         * the iterator in the fastest changing dimension.
         */
        if(actual_read==leftover) {
            /* Move iterator offset to beginning of next sequence in the fastest changing dimension */
            H5S_hyper_iter_next(mem_space,mem_iter);
        } /* end if */
        else {
            mem_iter->hyp.pos[fast_dim]+=actual_read; /* whole sequence not written out, just advance fastest dimension offset */
        } /* end if */
    } /* end if */

    /* Now that we've cleared the "remainder" of the previous fastest dimension
     * sequence, we must be at the beginning of a sequence, so use the fancy
     * algorithm to compute the offsets and run through as many as possible,
     * until the buffer fills up.
     */
    if(io_left>0) { /* Just in case the "remainder" above filled the buffer */
#ifdef QAK
printf("%s: Check 2.0, io_left=%lu\n",FUNC,(unsigned long)io_left);
#endif /* QAK */
        /* Compute the arrays to perform I/O on */
        /* Copy the location of the point to get */
        HDmemcpy(offset, mem_iter->hyp.pos,ndims*sizeof(hssize_t));
        offset[ndims] = 0;

        /* Add in the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] += mem_space->select.offset[i];

        /* Compute the current "counts" for this location */
        for(i=0; i<ndims; i++) {
            tmp_count[i] = (mem_iter->hyp.pos[i]-mem_space->select.sel_info.hslab.diminfo[i].start)%mem_space->select.sel_info.hslab.diminfo[i].stride;
            tmp_block[i] = (mem_iter->hyp.pos[i]-mem_space->select.sel_info.hslab.diminfo[i].start)/mem_space->select.sel_info.hslab.diminfo[i].stride;
        } /* end for */

        /* Compute the initial buffer offset */
        for(i=0,src=_buf; i<ndims; i++)
            src+=offset[i]*slab[i];

        /* Set the number of elements to write each time */
        H5_ASSIGN_OVERFLOW(actual_read,mem_space->select.sel_info.hslab.diminfo[fast_dim].block,hsize_t,size_t);

        /* Set the number of actual bytes */
        actual_bytes=actual_read*elmt_size;
#ifdef QAK
printf("%s: src=%p, actual_bytes=%u\n",FUNC,src,(int)actual_bytes);
#endif /* QAK */

#ifdef QAK
printf("%s: actual_read=%d\n",FUNC,(int)actual_read);
for(i=0; i<ndims; i++)
    printf("%s: diminfo: start[%d]=%d, stride[%d]=%d, block[%d]=%d, count[%d]=%d\n",FUNC,
        (int)i,(int)mem_space->select.sel_info.hslab.diminfo[i].start,
        (int)i,(int)mem_space->select.sel_info.hslab.diminfo[i].stride,
        (int)i,(int)mem_space->select.sel_info.hslab.diminfo[i].block,
        (int)i,(int)mem_space->select.sel_info.hslab.diminfo[i].count);
#endif /* QAK */

        /* Set the local copy of the diminfo pointer */
        tdiminfo=mem_space->select.sel_info.hslab.diminfo;

        /* Set local copies of information for the fastest changing dimension */
        fast_dim_start=tdiminfo[fast_dim].start;
        fast_dim_stride=tdiminfo[fast_dim].stride;
        fast_dim_block=tdiminfo[fast_dim].block;
        H5_ASSIGN_OVERFLOW(fast_dim_buf_off,(slab[fast_dim]*fast_dim_stride),hsize_t,size_t);
        fast_dim_offset=fast_dim_start+mem_space->select.offset[fast_dim];

        /* Compute the number of blocks which would fit into the buffer */
        tot_blk_count=io_left/fast_dim_block;

        /* Compute the amount to wrap at the end of each row */
        for(i=0; i<ndims; i++)
            wrap[i]=(mem_size[i]-(tdiminfo[i].stride*tdiminfo[i].count))*slab[i];

        /* Compute the amount to skip between blocks */
        for(i=0; i<ndims; i++)
            skip[i]=(tdiminfo[i].stride-tdiminfo[i].block)*slab[i];

        /* Read in data until an entire sequence can't be written out any longer */
        while(io_left>0) {
            /* Reset copy of number of blocks in fastest dimension */
            H5_ASSIGN_OVERFLOW(fast_dim_count,tdiminfo[fast_dim].count-tmp_count[fast_dim],hsize_t,size_t);

            /* Check if this entire row will fit into buffer */
            if(fast_dim_count<=tot_blk_count) {

                /* Entire row of blocks fits into buffer */
                act_blk_count=fast_dim_count;

#ifdef NO_DUFFS_DEVICE
                /* Loop over all the blocks in the fastest changing dimension */
                while(fast_dim_count>0) {
                    /* Scatter out the sequence */
                    HDmemcpy(dst,src,actual_bytes);

                    /* Increment the offset of the buffer */
                    dst+=actual_bytes;

                    /* Increment information to reflect block just processed */
                    src+=fast_dim_buf_off;

                    /* Decrement number of blocks */
                    fast_dim_count--;
                } /* end while */
#else /* NO_DUFFS_DEVICE */
                duffs_index = (fast_dim_count + 7) / 8;
                /* The following size_t cast is required on HPUX 10.20 in
                 * order to make the system compuiler happy.  It can be
                 * removed when we are no longer supporting that platform. -QAK
                 */
                switch (fast_dim_count % 8) {
                    case 0:
                        do
                          {
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            dst+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            src+=fast_dim_buf_off;

                    case 7:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            dst+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            src+=fast_dim_buf_off;

                    case 6:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            dst+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            src+=fast_dim_buf_off;

                    case 5:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            dst+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            src+=fast_dim_buf_off;

                    case 4:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            dst+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            src+=fast_dim_buf_off;

                    case 3:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            dst+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            src+=fast_dim_buf_off;

                    case 2:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            dst+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            src+=fast_dim_buf_off;

                    case 1:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            dst+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            src+=fast_dim_buf_off;

                      } while (--duffs_index > 0);
                } /* end switch */
#endif /* NO_DUFFS_DEVICE */

                /* Decrement number of elements left */
                io_left -= actual_read*act_blk_count;

                /* Decrement number of blocks left */
                tot_blk_count -= act_blk_count;

                /* Increment information to reflect block just processed */
                offset[fast_dim]=fast_dim_offset;    /* reset the offset in the fastest dimension */
                tmp_count[fast_dim]=0;

                /* Increment offset in destination buffer */
                src += wrap[fast_dim];
            } /* end if */
            else {

                /* Entire row of blocks doesn't fit into buffer */
                act_blk_count=tot_blk_count;

                /* Reduce number of blocks to output */
                fast_dim_count=tot_blk_count;

                /* Loop over all the blocks in the fastest changing dimension */
                while(fast_dim_count>0) {
                    /* Scatter out the sequence */
                    HDmemcpy(dst,src,actual_bytes);

                    /* Increment the offset of the buffer */
                    dst+=actual_bytes;

                    /* Increment information to reflect block just processed */
                    src+=fast_dim_buf_off;

                    /* Decrement number of blocks */
                    fast_dim_count--;
                } /* end while */

                /* Decrement number of elements left */
                io_left -= actual_read*act_blk_count;

                /* Decrement number of blocks left */
                tot_blk_count -= act_blk_count;

                /* Increment information to reflect block just processed */
                offset[fast_dim]+=(fast_dim_stride*act_blk_count);    /* reset the offset in the fastest dimension */
                tmp_count[fast_dim]+=act_blk_count;

                /* Handle any leftover, partial blocks in this row */
                if(io_left>0) {
                    actual_read=io_left;
                    actual_bytes=actual_read*elmt_size;

                    /* Scatter out the rest of the sequence */
                    HDmemcpy(dst,src,actual_bytes);

                    /* Increment the offset of the buffer */
                    dst+=actual_bytes;

                    /* Decrement the number of elements left */
                    io_left -= actual_read;

                    /* Increment buffer correctly */
                    offset[fast_dim]+=actual_read;
                } /* end if */

                /* don't bother checking slower dimensions */
                assert(tot_blk_count==0);
                assert(io_left==0);
                break;
            } /* end else */

            /* Increment the offset and count for the other dimensions */
            temp_dim=fast_dim-1;
            while(temp_dim>=0) {
                /* Move to the next row in the curent dimension */
                offset[temp_dim]++;
                tmp_block[temp_dim]++;

                /* If this block is still in the range of blocks to output for the dimension, break out of loop */
                if(tmp_block[temp_dim]<tdiminfo[temp_dim].block)
                    break;
                else {
                    /* Move to the next block in the current dimension */
                    offset[temp_dim]+=(tdiminfo[temp_dim].stride-tdiminfo[temp_dim].block);
                    src += skip[temp_dim];
                    tmp_block[temp_dim]=0;
                    tmp_count[temp_dim]++;

                    /* If this block is still in the range of blocks to output for the dimension, break out of loop */
                    if(tmp_count[temp_dim]<tdiminfo[temp_dim].count)
                        break;
                    else {
                        offset[temp_dim]=tdiminfo[temp_dim].start+mem_space->select.offset[temp_dim];
                        src += wrap[temp_dim];
                        tmp_count[temp_dim]=0; /* reset back to the beginning of the line */
                        tmp_block[temp_dim]=0;
                    } /* end else */
                } /* end else */

                /* Decrement dimension count */
                temp_dim--;
            } /* end while */
        } /* end while */

        /* Subtract out the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] -= mem_space->select.offset[i];

        /* Update the iterator with the location we stopped */
        HDmemcpy(mem_iter->hyp.pos, offset, ndims*sizeof(hssize_t));
    } /* end if */

    /* Decrement the number of elements left in selection */
    mem_iter->hyp.elmt_left-=(nelmts-io_left);

    FUNC_LEAVE (nelmts-io_left);
} /* end H5S_hyper_mread_opt() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_mgath
 *
 * Purpose:	Gathers dataset elements from application memory BUF and
 *		copies them into the data type conversion buffer TCONV_BUF.
 *		Each element is ELMT_SIZE bytes and arranged in application
 *		memory according to MEM_SPACE.  
 *		The caller is requesting that at most NELMTS be gathered.
 *
 * Return:	Success:	Number of elements copied.
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5S_hyper_mgath (const void *_buf, size_t elmt_size,
		 const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		 hsize_t nelmts, void *_tconv_buf/*out*/)
{
    hsize_t  num_read;       /* number of elements read into buffer */

    FUNC_ENTER (H5S_hyper_mgath, 0);

    /* Check args */
    assert (elmt_size>0);
    assert (mem_space);
    assert (mem_iter);
    assert (nelmts>0);
    assert (_buf);
    assert (_tconv_buf);

    /* Check for the special case of just one H5Sselect_hyperslab call made */
    if(mem_space->select.sel_info.hslab.diminfo!=NULL) {
        /* Use optimized call to read in regular hyperslab */
        num_read=H5S_hyper_mread_opt(_buf,elmt_size,mem_space,mem_iter,nelmts,_tconv_buf);
    } /* end if */
    else {
        /* Perform generic hyperslab operation */
        num_read=H5S_hyper_mread(_buf,elmt_size,mem_space,mem_iter,nelmts,_tconv_buf);
    } /* end else */

    FUNC_LEAVE (num_read);
}   /* H5S_hyper_mgath() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_mwrite
 *
 * Purpose:	Performs an optimized gather from a memory buffer, based on a
 *      hyperslab span selection.
 *
 * Return:	Success:	Number of elements copied.
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 12, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
H5S_hyper_mwrite (const void *_tconv_buf, size_t elmt_size, const H5S_t *space,
    H5S_sel_iter_t *iter, hsize_t nelem, void *_buf/*out*/)
{
    const uint8_t	*src=(const uint8_t *)_tconv_buf;   /* Alias for pointer arithmetic */
    uint8_t	*dst=(uint8_t *)_buf;   /* Alias for pointer arithmetic */
    uint8_t	*tmp_dst;   		/* Alias for pointer arithmetic */
    H5S_hyper_span_t **ispan;       /* Iterator's hyperslab span nodes */
    H5S_hyper_span_t *curr_span;    /* Current hyperslab span node */
    hsize_t slab[H5O_LAYOUT_NDIMS]; /* Cumulative size of each dimension in bytes */
    hsize_t acc;        /* Accumulator for computing cumulative sizes */
    hssize_t *abs_arr;  /* Absolute hyperslab span position */
    hssize_t *off_arr;  /* Offset within the dataspace extent */
    int fast_dim;      /* Rank of the fastest changing dimension for the dataspace */
    int curr_dim;      /* Current dimension being operated on */
    int ndims;         /* Number of dimensions of dataset */
    size_t span_size;   /* Number of bytes in current span to actually process */
    size_t io_bytes_left;   /* Number of bytes left to process */
    int i;             /* Index variable */
    hssize_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_mwrite, FAIL);
#ifdef QAK
printf("%s: Called!\n",FUNC);
#endif /* QAK */

    /* Check args */
    assert(src);
    assert(elmt_size>0);
    assert(space);
    assert(iter);
    assert(nelem>0);
    assert(dst);

    /* Set the rank of the fastest changing dimension */
    ndims=space->extent.u.simple.rank;
    fast_dim=(ndims-1);

    /* Get the pointers to the current span info and span nodes */
    curr_span=iter->hyp.span[fast_dim];
    abs_arr=iter->hyp.off;
    off_arr=space->select.offset;
    ispan=iter->hyp.span;

    /* Set the amount of elements to perform I/O on, etc. */
    H5_ASSIGN_OVERFLOW(io_bytes_left,nelem*elmt_size,hsize_t,size_t);

    /* Compute the cumulative size of dataspace dimensions */
    for(i=fast_dim, acc=elmt_size; i>=0; i--) {
        slab[i]=acc;
        acc*=space->extent.u.simple.size[i];
    } /* end for */

    /* Set the offset of the first element iterated on */
    for(i=0, tmp_dst=dst; i<ndims; i++)
        /* Compute the sequential element offset */
        tmp_dst+=(abs_arr[i]+space->select.offset[i])*slab[i];

    /* Range check against number of elements left in selection */
    assert(io_bytes_left<=(iter->hyp.elmt_left*elmt_size));

    /* Take care of any partial spans leftover from previous I/Os */
    if(abs_arr[fast_dim]!=curr_span->low) {

        /* Finish the span in the fastest changing dimension */

        /* Compute the number of bytes to attempt in this span */
        H5_ASSIGN_OVERFLOW(span_size,((curr_span->high-abs_arr[fast_dim])+1)*elmt_size,hsize_t,size_t);

        /* Check number of elements against upper bounds allowed */
        if(span_size>io_bytes_left)
            span_size=io_bytes_left;

        HDmemcpy(tmp_dst,src,span_size);

        /* Increment offset in destination */
        src+=span_size;

        /* Decrement I/O left to perform */
        io_bytes_left-=span_size;

        /* Check if we are done */
        if(io_bytes_left>0) {
            /* Move to next span in fastest changing dimension */
            curr_span=curr_span->next;

            if(curr_span!=NULL) {
                /* Move location offset of destination */
	        tmp_dst+=(curr_span->low-abs_arr[fast_dim])*elmt_size;

                /* Move iterator for fastest changing dimension */
                abs_arr[fast_dim]=curr_span->low;
            } /* end if */
        } /* end if */
        else {
            abs_arr[fast_dim]+=span_size/elmt_size;

            /* Check if we are still within the span */
            if(abs_arr[fast_dim]<=curr_span->high) {
                iter->hyp.span[fast_dim]=curr_span;

                goto partial_done;      /* finished with partial span */
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[fast_dim]=curr_span->low;
                    iter->hyp.span[fast_dim]=curr_span;

                    goto partial_done;      /* finished with partial span */
                } /* end if */
            } /* end else */
        } /* end else */

        /* Adjust iterator pointers */

        if(curr_span==NULL) {
/* Same as code in main loop */
            /* Start at the next fastest dim */
            curr_dim=fast_dim-1;

            /* Work back up through the dimensions */
            while(curr_dim>=0) {
                /* Reset the current span */
                curr_span=iter->hyp.span[curr_dim];

                /* Increment absolute position */
                abs_arr[curr_dim]++;

                /* Check if we are still within the span */
                if(abs_arr[curr_dim]<=curr_span->high) {
                    break;
                } /* end if */
                /* If we walked off that span, advance to the next span */
                else {
                    /* Advance span in this dimension */
                    curr_span=curr_span->next;

                    /* Check if we have a valid span in this dimension still */
                    if(curr_span!=NULL) {
                        /* Reset absolute position */
                        abs_arr[curr_dim]=curr_span->low;

                        break;
                    } /* end if */
                    else {
                        /* If we finished the span list in this dimension, decrement the dimension worked on and loop again */
                        curr_dim--;
                    } /* end else */
                } /* end else */
            } /* end while */

            /* Check if we are finished with the spans in the tree */
            if(curr_dim<0) {
                /* We had better be done with I/O or bad things are going to happen... */
                assert(io_bytes_left==0);

                goto partial_done;      /* finished with partial span */
            } /* end if */
            else {
                /* Reset the span in the current dimension */
                ispan[curr_dim]=curr_span;

                /* Walk back down the iterator positions, reseting them */
                while(curr_dim<fast_dim) {
                    assert(curr_span);
                    assert(curr_span->down);
                    assert(curr_span->down->head);

                    /* Set the new span_info & span for this dimension */
                    iter->hyp.span[curr_dim+1]=curr_span->down->head;

                    /* Advance span down the tree */
                    curr_span=curr_span->down->head;

                    /* Reset the absolute offset for the dim */
                    abs_arr[curr_dim+1]=curr_span->low;

                    /* Increment current dimension */
                    curr_dim++;
                } /* end while */

                /* Verify that the curr_span points to the fastest dim */
                assert(curr_span==iter->hyp.span[fast_dim]);
            } /* end else */

            /* Reset the buffer offset */
            for(i=0, tmp_dst=dst; i<ndims; i++)
                tmp_dst+=(abs_arr[i]+off_arr[i])*slab[i];
        } /* end if */
    } /* end if */

partial_done:   /* Yes, goto's are evil, so sue me... :-) */

    /* Perform the I/O on the elements, based on the position of the iterator */
    while(io_bytes_left>0) {
        /* Adjust buffer offset of destination to compensate for initial increment below */
        tmp_dst-=(size_t)curr_span->pstride;

        /* Loop over all the spans in the fastest changing dimension */
        while(curr_span!=NULL) {
            /* Move buffer offset of destination */
            tmp_dst+=(size_t)curr_span->pstride;

            /* Compute the number of elements to attempt in this span */
            H5_ASSIGN_OVERFLOW(span_size,curr_span->nelem,hsize_t,size_t);

            /* Check number of elements against upper bounds allowed */
            if(span_size>=io_bytes_left) {
                /* Trim the number of bytes to output */
                span_size=io_bytes_left;
                io_bytes_left=0;

/* COMMON */
                /* "Write" the data into the destination buffer */
                HDmemcpy(tmp_dst,src,span_size);

                /* Increment offset in destination */
                src+=span_size;
/* end COMMON */

                /* Break out now, we are finished with I/O */
                break;
            } /* end if */
            else {
                /* Decrement I/O left to perform */
                io_bytes_left-=span_size;

/* COMMON */
                /* "Write" the data into the destination buffer */
                HDmemcpy(tmp_dst,src,span_size);

                /* Increment offset in destination */
                src+=span_size;
/* end COMMON */
            } /* end else */

	    /* Move to next span in fastest changing dimension */
	    curr_span=curr_span->next;
        } /* end while */

        /* Check if we are done */
        if(io_bytes_left==0) {
            abs_arr[fast_dim]=curr_span->low+(span_size/elmt_size);

            /* Check if we are still within the span */
            if(abs_arr[fast_dim]<=curr_span->high) {
                iter->hyp.span[fast_dim]=curr_span;
                break;
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[fast_dim]=curr_span->low;
                    iter->hyp.span[fast_dim]=curr_span;
                    break;
                } /* end if */
            } /* end else */
        } /* end if */

        /* Adjust iterator pointers */

        /* Start at the next fastest dim */
        curr_dim=fast_dim-1;

        /* Work back up through the dimensions */
        while(curr_dim>=0) {
            /* Reset the current span */
	    curr_span=iter->hyp.span[curr_dim];

            /* Increment absolute position */
            abs_arr[curr_dim]++;

            /* Check if we are still within the span */
            if(abs_arr[curr_dim]<=curr_span->high) {
                break;
            } /* end if */
            /* If we walked off that span, advance to the next span */
            else {
                /* Advance span in this dimension */
                curr_span=curr_span->next;

                /* Check if we have a valid span in this dimension still */
                if(curr_span!=NULL) {
                    /* Reset absolute position */
                    abs_arr[curr_dim]=curr_span->low;

                    break;
                } /* end if */
                else {
                    /* If we finished the span list in this dimension, decrement the dimension worked on and loop again */
                    curr_dim--;
                } /* end else */
            } /* end else */
        } /* end while */

        /* Check if we are finished with the spans in the tree */
        if(curr_dim<0) {
            /* We had better be done with I/O or bad things are going to happen... */
            assert(io_bytes_left==0);
            break;
        } /* end if */
        else {
            /* Reset the span in the current dimension */
            ispan[curr_dim]=curr_span;

            /* Walk back down the iterator positions, reseting them */
            while(curr_dim<fast_dim) {
                assert(curr_span);
                assert(curr_span->down);
                assert(curr_span->down->head);

                /* Set the new span_info & span for the next dimension down */
                iter->hyp.span[curr_dim+1]=curr_span->down->head;

                /* Advance span down the tree */
                curr_span=curr_span->down->head;

                /* Reset the absolute offset for the dim */
                abs_arr[curr_dim+1]=curr_span->low;

                /* Increment current dimension */
                curr_dim++;
            } /* end while */

            /* Verify that the curr_span points to the fastest dim */
            assert(curr_span==iter->hyp.span[fast_dim]);
        } /* end else */

        /* Reset the buffer offset */
        for(i=0, tmp_dst=dst; i<ndims; i++)
            tmp_dst+=(abs_arr[i]+off_arr[i])*slab[i];
    } /* end while */

    /* Increment amount of I/O performed */
    iter->hyp.elmt_left-=nelem;

    /* Success! */
    ret_value=nelem;

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_mwrite() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_mwrite_opt
 *
 * Purpose:	Performs an optimized scatter to a memory buffer, based on a
 *      regular hyperslab (i.e. one which was generated from just one call to
 *      H5Sselect_hyperslab).
 *
 * Return:	Success:	Number of elements copied.
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 12, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5S_hyper_mwrite_opt (const void *_tconv_buf, size_t elmt_size,
		 const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		 hsize_t nelmts, void *_buf/*out*/)
{
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];     /* Size of the source buffer */
    hsize_t	slab[H5O_LAYOUT_NDIMS];         /* Hyperslab size */
    hssize_t	wrap[H5O_LAYOUT_NDIMS];         /* Bytes to wrap around at the end of a row */
    hsize_t	skip[H5O_LAYOUT_NDIMS];         /* Bytes to skip between blocks */
    hssize_t    offset[H5O_LAYOUT_NDIMS];       /* Offset on disk */
    hsize_t	tmp_count[H5O_LAYOUT_NDIMS];    /* Temporary block count */
    hsize_t	tmp_block[H5O_LAYOUT_NDIMS];    /* Temporary block offset */
    const uint8_t	*src=(const uint8_t *)_tconv_buf;   /* Alias for pointer arithmetic */
    uint8_t	*dst=(uint8_t *)_buf;   /* Alias for pointer arithmetic */
    const H5S_hyper_dim_t *tdiminfo;      /* Temporary pointer to diminfo information */
    hssize_t fast_dim_start,            /* Local copies of fastest changing dimension info */
        fast_dim_offset;
    hsize_t fast_dim_stride,            /* Local copies of fastest changing dimension info */
        fast_dim_block;
    size_t fast_dim_count;
    size_t tot_blk_count;              /* Total number of blocks left to output */
    size_t act_blk_count;              /* Actual number of blocks to output */
    size_t fast_dim_buf_off;            /* Local copy of amount to move fastest dimension buffer offset */
    int fast_dim;  /* Rank of the fastest changing dimension for the dataspace */
    int temp_dim;  /* Temporary rank holder */
    hsize_t	acc;	/* Accumulator */
    int i;         /* Counters */
    unsigned u;         /* Counters */
    int   	ndims;      /* Number of dimensions of dataset */
    size_t actual_write;       /* The actual number of elements to read in */
    size_t actual_bytes;     /* The actual number of bytes to copy */
    size_t io_left;             /* The number of elements left in I/O operation */
#ifndef NO_DUFFS_DEVICE
    size_t duffs_index;         /* Counting index for Duff's device */
#endif /* NO_DUFFS_DEVICE */

    FUNC_ENTER (H5S_hyper_mwrite_opt, 0);

#ifdef QAK
printf("%s: Called!, nelmts=%lu, elmt_size=%d\n",FUNC,(unsigned long)nelmts,(int)elmt_size);
#endif /* QAK */
    /* Check if this is the first element read in from the hyperslab */
    if(mem_iter->hyp.pos[0]==(-1)) {
        for(u=0; u<mem_space->extent.u.simple.rank; u++)
            mem_iter->hyp.pos[u]=mem_space->select.sel_info.hslab.diminfo[u].start;
    } /* end if */

#ifdef QAK
for(u=0; u<mem_space->extent.u.simple.rank; u++)
    printf("%s: mem_file->hyp.pos[%u]=%d\n",FUNC,(unsigned)u,(int)mem_iter->hyp.pos[u]);
#endif /* QAK */

    /* Set the aliases for a few important dimension ranks */
    fast_dim=mem_space->extent.u.simple.rank-1;
    ndims=mem_space->extent.u.simple.rank;

    /* Set up the size of the memory space */
    HDmemcpy(mem_size, mem_space->extent.u.simple.size,mem_space->extent.u.simple.rank*sizeof(hsize_t));
    mem_size[mem_space->extent.u.simple.rank]=elmt_size;

    /* initialize row sizes for each dimension */
    for(i=(ndims-1),acc=1; i>=0; i--) {
        slab[i]=acc*elmt_size;
        acc*=mem_size[i];
    } /* end for */
#ifdef QAK
for(i=0; i<ndims; i++)
    printf("%s: mem_size[%d]=%d, slab[%d]=%d\n",FUNC,(int)i,(int)mem_size[i],(int)i,(int)slab[i]);
#endif /* QAK */

    /* Set the number of elements left for I/O */
    H5_ASSIGN_OVERFLOW(io_left,nelmts,hsize_t,size_t);

    /* Check if we stopped in the middle of a sequence of elements */
    if((mem_iter->hyp.pos[fast_dim]-mem_space->select.sel_info.hslab.diminfo[fast_dim].start)%mem_space->select.sel_info.hslab.diminfo[fast_dim].stride!=0 ||
            ((mem_iter->hyp.pos[fast_dim]!=mem_space->select.sel_info.hslab.diminfo[fast_dim].start) && mem_space->select.sel_info.hslab.diminfo[fast_dim].stride==1)) {
        size_t leftover;  /* The number of elements left over from the last sequence */

#ifdef QAK
printf("%s: Check 1.0, io_left=%lu\n",FUNC,(unsigned long)io_left);
#endif /* QAK */
        /* Calculate the number of elements left in the sequence */
        if(mem_space->select.sel_info.hslab.diminfo[fast_dim].stride==1)
            leftover=mem_space->select.sel_info.hslab.diminfo[fast_dim].block-(mem_iter->hyp.pos[fast_dim]-mem_space->select.sel_info.hslab.diminfo[fast_dim].start);
        else
            leftover=mem_space->select.sel_info.hslab.diminfo[fast_dim].block-((mem_iter->hyp.pos[fast_dim]-mem_space->select.sel_info.hslab.diminfo[fast_dim].start)%mem_space->select.sel_info.hslab.diminfo[fast_dim].stride);

        /* Make certain that we don't write too many */
        actual_write=MIN(leftover,io_left);
        actual_bytes=actual_write*elmt_size;

        /* Copy the location of the point to get */
        HDmemcpy(offset, mem_iter->hyp.pos,ndims*sizeof(hssize_t));
        offset[ndims] = 0;

        /* Add in the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] += mem_space->select.offset[i];

        /* Compute the initial buffer offset */
        for(i=0,dst=(unsigned char *)_buf; i<ndims; i++)
            dst+=offset[i]*slab[i];

        /* Scatter out the rest of the sequence */
        HDmemcpy(dst,src,actual_bytes);

        /* Increment the offset of the buffer */
        src+=actual_bytes;

        /* Decrement the number of elements written out */
        io_left -= actual_write;

        /* Advance the point iterator */
        /* If we had enough buffer space to write out the rest of the sequence
         * in the fastest changing dimension, move the iterator offset to
         * the beginning of the next block to write.  Otherwise, just advance
         * the iterator in the fastest changing dimension.
         */
        if(actual_write==leftover) {
            /* Move iterator offset to beginning of next sequence in the fastest changing dimension */
            H5S_hyper_iter_next(mem_space,mem_iter);
        } /* end if */
        else {
            mem_iter->hyp.pos[fast_dim]+=actual_write; /* whole sequence not written out, just advance fastest dimension offset */
        } /* end if */
    } /* end if */

    /* Now that we've cleared the "remainder" of the previous fastest dimension
     * sequence, we must be at the beginning of a sequence, so use the fancy
     * algorithm to compute the offsets and run through as many as possible,
     * until the buffer fills up.
     */
    if(io_left>0) { /* Just in case the "remainder" above filled the buffer */
#ifdef QAK
printf("%s: Check 2.0, io_left=%lu\n",FUNC,(unsigned long)io_left);
#endif /* QAK */
        /* Compute the arrays to perform I/O on */
        /* Copy the location of the point to get */
        HDmemcpy(offset, mem_iter->hyp.pos,ndims*sizeof(hssize_t));
        offset[ndims] = 0;

        /* Add in the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] += mem_space->select.offset[i];

        /* Compute the current "counts" for this location */
        for(i=0; i<ndims; i++) {
            tmp_count[i] = (mem_iter->hyp.pos[i]-mem_space->select.sel_info.hslab.diminfo[i].start)%mem_space->select.sel_info.hslab.diminfo[i].stride;
            tmp_block[i] = (mem_iter->hyp.pos[i]-mem_space->select.sel_info.hslab.diminfo[i].start)/mem_space->select.sel_info.hslab.diminfo[i].stride;
        } /* end for */

        /* Compute the initial buffer offset */
        for(i=0,dst=(unsigned char *)_buf; i<ndims; i++)
            dst+=offset[i]*slab[i];

        /* Set the number of elements to write each time */
        H5_ASSIGN_OVERFLOW(actual_write,mem_space->select.sel_info.hslab.diminfo[fast_dim].block,hsize_t,size_t);

        /* Set the number of actual bytes */
        actual_bytes=actual_write*elmt_size;
#ifdef QAK
printf("%s: dst=%p, actual_bytes=%u\n",FUNC,dst,(int)actual_bytes);
#endif /* QAK */

#ifdef QAK
printf("%s: actual_write=%d\n",FUNC,(int)actual_write);
for(i=0; i<ndims; i++)
    printf("%s: diminfo: start[%d]=%d, stride[%d]=%d, block[%d]=%d, count[%d]=%d\n",FUNC,
        (int)i,(int)mem_space->select.sel_info.hslab.diminfo[i].start,
        (int)i,(int)mem_space->select.sel_info.hslab.diminfo[i].stride,
        (int)i,(int)mem_space->select.sel_info.hslab.diminfo[i].block,
        (int)i,(int)mem_space->select.sel_info.hslab.diminfo[i].count);
#endif /* QAK */

        /* Set the local copy of the diminfo pointer */
        tdiminfo=mem_space->select.sel_info.hslab.diminfo;

        /* Set local copies of information for the fastest changing dimension */
        fast_dim_start=tdiminfo[fast_dim].start;
        fast_dim_stride=tdiminfo[fast_dim].stride;
        fast_dim_block=tdiminfo[fast_dim].block;
        H5_ASSIGN_OVERFLOW(fast_dim_buf_off,slab[fast_dim]*fast_dim_stride,hsize_t,size_t);
        fast_dim_offset=fast_dim_start+mem_space->select.offset[fast_dim];

        /* Compute the number of blocks which would fit into the buffer */
        tot_blk_count=io_left/fast_dim_block;

        /* Compute the amount to wrap at the end of each row */
        for(i=0; i<ndims; i++)
            wrap[i]=(mem_size[i]-(tdiminfo[i].stride*tdiminfo[i].count))*slab[i];

        /* Compute the amount to skip between blocks */
        for(i=0; i<ndims; i++)
            skip[i]=(tdiminfo[i].stride-tdiminfo[i].block)*slab[i];

        /* Read in data until an entire sequence can't be written out any longer */
        while(io_left>0) {
            /* Reset copy of number of blocks in fastest dimension */
            H5_ASSIGN_OVERFLOW(fast_dim_count,tdiminfo[fast_dim].count-tmp_count[fast_dim],hsize_t,size_t);

            /* Check if this entire row will fit into buffer */
            if(fast_dim_count<=tot_blk_count) {

                /* Entire row of blocks fits into buffer */
                act_blk_count=fast_dim_count;

#ifdef NO_DUFFS_DEVICE
                /* Loop over all the blocks in the fastest changing dimension */
                while(fast_dim_count>0) {
                    /* Scatter out the sequence */
                    HDmemcpy(dst,src,actual_bytes);

                    /* Increment the offset of the buffer */
                    src+=actual_bytes;

                    /* Increment information to reflect block just processed */
                    dst+=fast_dim_buf_off;

                    /* Decrement number of blocks */
                    fast_dim_count--;
                } /* end while */
#else /* NO_DUFFS_DEVICE */
                duffs_index = (fast_dim_count + 7) / 8;
                /* The following size_t cast is required on HPUX 10.20 in
                 * order to make the system compuiler happy.  It can be
                 * removed when we are no longer supporting that platform. -QAK
                 */
                switch (fast_dim_count % 8) {
                    case 0:
                        do
                          {
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            src+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            dst+=fast_dim_buf_off;

                    case 7:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            src+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            dst+=fast_dim_buf_off;

                    case 6:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            src+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            dst+=fast_dim_buf_off;

                    case 5:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            src+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            dst+=fast_dim_buf_off;

                    case 4:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            src+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            dst+=fast_dim_buf_off;

                    case 3:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            src+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            dst+=fast_dim_buf_off;

                    case 2:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            src+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            dst+=fast_dim_buf_off;

                    case 1:
                            /* Scatter out the sequence */
                            HDmemcpy(dst,src,actual_bytes);

                            /* Increment the offset of the buffer */
                            src+=actual_bytes;

                            /* Increment information to reflect block just processed */
                            dst+=fast_dim_buf_off;

                      } while (--duffs_index > 0);
                } /* end switch */
#endif /* NO_DUFFS_DEVICE */

                /* Decrement number of elements left */
                io_left -= actual_write*act_blk_count;

                /* Decrement number of blocks left */
                tot_blk_count -= act_blk_count;

                /* Increment information to reflect block just processed */
                offset[fast_dim]=fast_dim_offset;    /* reset the offset in the fastest dimension */
                tmp_count[fast_dim]=0;

                /* Increment offset in destination buffer */
                dst += wrap[fast_dim];
            } /* end if */
            else {

                /* Entire row of blocks doesn't fit into buffer */
                act_blk_count=tot_blk_count;

                /* Reduce number of blocks to output */
                fast_dim_count=tot_blk_count;

                /* Loop over all the blocks in the fastest changing dimension */
                while(fast_dim_count>0) {
                    /* Scatter out the sequence */
                    HDmemcpy(dst,src,actual_bytes);

                    /* Increment the offset of the buffer */
                    src+=actual_bytes;

                    /* Increment information to reflect block just processed */
                    dst+=fast_dim_buf_off;

                    /* Decrement number of blocks */
                    fast_dim_count--;
                } /* end while */

                /* Decrement number of elements left */
                io_left -= actual_write*act_blk_count;

                /* Decrement number of blocks left */
                tot_blk_count -= act_blk_count;

                /* Increment information to reflect block just processed */
                offset[fast_dim]+=(fast_dim_stride*act_blk_count);    /* reset the offset in the fastest dimension */
                tmp_count[fast_dim]+=act_blk_count;

                /* Handle any leftover, partial blocks in this row */
                if(io_left>0) {
                    actual_write=io_left;
                    actual_bytes=actual_write*elmt_size;

                    /* Scatter out the rest of the sequence */
                    HDmemcpy(dst,src,actual_bytes);

                    /* Increment the offset of the buffer */
                    src+=actual_bytes;

                    /* Decrement the number of elements left */
                    io_left -= actual_write;

                    /* Increment buffer correctly */
                    offset[fast_dim]+=actual_write;
                } /* end if */

                /* don't bother checking slower dimensions */
                assert(tot_blk_count==0);
                assert(io_left==0);
                break;
            } /* end else */

            /* Increment the offset and count for the other dimensions */
            temp_dim=fast_dim-1;
            while(temp_dim>=0) {
                /* Move to the next row in the curent dimension */
                offset[temp_dim]++;
                tmp_block[temp_dim]++;

                /* If this block is still in the range of blocks to output for the dimension, break out of loop */
                if(tmp_block[temp_dim]<tdiminfo[temp_dim].block)
                    break;
                else {
                    /* Move to the next block in the current dimension */
                    offset[temp_dim]+=(tdiminfo[temp_dim].stride-tdiminfo[temp_dim].block);
                    dst += skip[temp_dim];
                    tmp_block[temp_dim]=0;
                    tmp_count[temp_dim]++;

                    /* If this block is still in the range of blocks to output for the dimension, break out of loop */
                    if(tmp_count[temp_dim]<tdiminfo[temp_dim].count)
                        break;
                    else {
                        offset[temp_dim]=tdiminfo[temp_dim].start+mem_space->select.offset[temp_dim];
                        dst += wrap[temp_dim];
                        tmp_count[temp_dim]=0; /* reset back to the beginning of the line */
                        tmp_block[temp_dim]=0;
                    } /* end else */
                } /* end else */

                /* Decrement dimension count */
                temp_dim--;
            } /* end while */
        } /* end while */

        /* Subtract out the selection offset */
        for(i=0; i<ndims; i++)
            offset[i] -= mem_space->select.offset[i];

        /* Update the iterator with the location we stopped */
        HDmemcpy(mem_iter->hyp.pos, offset, ndims*sizeof(hssize_t));
    } /* end if */

    /* Decrement the number of elements left in selection */
    mem_iter->hyp.elmt_left-=(nelmts-io_left);

    FUNC_LEAVE (nelmts-io_left);
} /* end H5S_hyper_mwrite_opt() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_mscat
 *
 * Purpose:	Scatters NELMTS data points from the type conversion buffer
 *		TCONV_BUF to the application buffer BUF.  Each element is
 *		ELMT_SIZE bytes and they are organized in application memory
 *		according to MEM_SPACE.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, June 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_hyper_mscat (const void *_tconv_buf, size_t elmt_size,
		 const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		 hsize_t nelmts, void *_buf/*out*/)
{
    hsize_t  num_written;            /* number of elements written into buffer */

    FUNC_ENTER (H5S_hyper_mscat, 0);

    /* Check args */
    assert (elmt_size>0);
    assert (mem_space);
    assert (mem_iter);
    assert (nelmts>0);
    assert (_buf);
    assert (_tconv_buf);

    /* Check for the special case of just one H5Sselect_hyperslab call made */
    if(mem_space->select.sel_info.hslab.diminfo!=NULL) {
        /* Use optimized call to write out regular hyperslab */
        num_written=H5S_hyper_mwrite_opt(_tconv_buf,elmt_size,mem_space,mem_iter,nelmts,_buf);
    } /* end if */
    else {
        /* Perform generic hyperslab operation */
        num_written=H5S_hyper_mwrite(_tconv_buf,elmt_size,mem_space,mem_iter,nelmts,_buf);
    } /* end else */

    FUNC_LEAVE (num_written>0 ? SUCCEED : FAIL);
}   /* H5S_hyper_mscat() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_npoints
 PURPOSE
    Compute number of elements in current selection
 USAGE
    hsize_t H5S_hyper_npoints(space)
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
H5S_hyper_npoints (const H5S_t *space)
{
    FUNC_ENTER (H5S_hyper_npoints, 0);

    /* Check args */
    assert (space);

    FUNC_LEAVE (space->select.num_elem);
}   /* H5S_hyper_npoints() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_new_span
 PURPOSE
    Make a new hyperslab span node
 USAGE
    H5S_hyper_span_t *H5S_hyper_new_span(low, high, down, next)
        hssize_t low, high;         IN: Low and high bounds for new span node
        H5S_hyper_span_info_t *down;     IN: Down span tree for new node
        H5S_hyper_span_t *next;     IN: Next span for new node
 RETURNS
    Pointer to next span node on success, NULL on failure
 DESCRIPTION
    Allocate and initialize a new hyperslab span node, filling in the low &
    high bounds, the down span and next span pointers also.  Increment the
    reference count of the 'down span' if applicable.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5S_hyper_span_t *
H5S_hyper_new_span (hssize_t low, hssize_t high, H5S_hyper_span_info_t *down, H5S_hyper_span_t *next)
{
    H5S_hyper_span_t *ret_value=NULL;

    FUNC_ENTER (H5S_hyper_new_span, NULL);

    /* Allocate a new span node */
    if((ret_value = H5FL_ALLOC(H5S_hyper_span_t,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");
       
    /* Copy the span's basic information */
    ret_value->low=low;
    ret_value->high=high;
    ret_value->nelem=(high-low)+1;
    ret_value->pstride=0;
    ret_value->down=down;
    ret_value->next=next;

    /* Increment the reference count of the 'down span' if there is one */
    if(ret_value->down!=NULL)
        ret_value->down->count++;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_new_span() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_span_precompute_helper
 PURPOSE
    Helper routine to precompute the nelem and pstrides in bytes.
 USAGE
    herr_t H5S_hyper_span_precompute_helper(span_info, elmt_size)
        H5S_hyper_span_info_t *span_info;      IN/OUT: Span tree to work on
        size_t elmt_size;                      IN: element size to work with
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Change the nelem and pstride values in the span tree from elements to
    bytes using the elmt_size parameter.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_span_precompute_helper (H5S_hyper_span_info_t *spans, size_t elmt_size)
{
    H5S_hyper_span_t *span;     /* Hyperslab span */
    herr_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_span_precompute, FAIL);

    assert(spans);

    /* Check if we've already set this down span tree */
    if(spans->scratch!=(void *)~((size_t)NULL)) {
        /* Set the tree's scratch pointer */
        spans->scratch=(void *)~((size_t)NULL);

        /* Set the scratch pointers in all the nodes */
        span=spans->head;

        /* Loop over all the spans for this down span tree */
        while(span!=NULL) {
            /* If there are down spans, set their scratch value also */
            if(span->down!=NULL) {
                if(H5S_hyper_span_precompute_helper(span->down,elmt_size)==FAIL)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't reset hyperslab scratch pointer");
            } /* end if */

            /* Change the nelem & pstride values into bytes */
            span->nelem *= elmt_size;
            span->pstride *= elmt_size;

            /* Advance to next span */
            span=span->next;
        } /* end while */
    } /* end if */

    /* Success! */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_span_precompute_helper() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_span_precompute
 PURPOSE
    Precompute the nelem and pstrides in bytes.
 USAGE
    herr_t H5S_hyper_span_precompute(span_info, elmt_size)
        H5S_hyper_span_info_t *span_info;      IN/OUT: Span tree to work on
        size_t elmt_size;                      IN: element size to work with
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Change the nelem and pstride values in the span tree from elements to
    bytes using the elmt_size parameter.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_span_precompute (H5S_hyper_span_info_t *spans, size_t elmt_size)
{
    herr_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_span_precompute, FAIL);

    assert(spans);

    /* Call the helper routine to actually do the work */
    if(H5S_hyper_span_precompute_helper(spans,elmt_size)==FAIL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't precompute span info");

    /* Reset the scratch pointers for the next routine which needs them */
    if(H5S_hyper_span_scratch(spans,NULL)==FAIL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't reset hyperslab scratch pointer");

    /* Success! */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_span_precompute() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_span_scratch
 PURPOSE
    Set the scratch pointers on hyperslab span trees
 USAGE
    herr_t H5S_hyper_span_scratch(span_info)
        H5S_hyper_span_info_t *span_info;      IN: Span tree to reset
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Set the scratch pointers on a hyperslab span tree.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_span_scratch (H5S_hyper_span_info_t *spans, void *scr_value)
{
    H5S_hyper_span_t *span;     /* Hyperslab span */
    herr_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_span_scratch, FAIL);

    assert(spans);

    /* Check if we've already set this down span tree */
    if(spans->scratch!=scr_value) {
        /* Set the tree's scratch pointer */
        spans->scratch=scr_value;

        /* Set the scratch pointers in all the nodes */
        span=spans->head;
        while(span!=NULL) {
            /* If there are down spans, set their scratch value also */
            if(span->down!=NULL) {
                if(H5S_hyper_span_scratch(span->down,scr_value)==FAIL)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't reset hyperslab scratch pointer");
            } /* end if */

            /* Advance to next span */
            span=span->next;
        } /* end while */
    } /* end if */

    /* Success! */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_span_scratch() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_copy_span_helper
 PURPOSE
    Helper routine to copy a hyperslab span tree
 USAGE
    H5S_hyper_span_info_t * H5S_hyper_copy_span_helper(spans)
        H5S_hyper_span_info_t *spans;      IN: Span tree to copy
 RETURNS
    Pointer to the copied span tree on success, NULL on failure
 DESCRIPTION
    Copy a hyperslab span tree, using reference counting as appropriate.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5S_hyper_span_info_t *
H5S_hyper_copy_span_helper (H5S_hyper_span_info_t *spans)
{
    H5S_hyper_span_t *span;         /* Hyperslab span */
    H5S_hyper_span_t *new_span;     /* Temporary hyperslab span */
    H5S_hyper_span_t *prev_span;    /* Previous hyperslab span */
    H5S_hyper_span_info_t *new_down;    /* New down span tree */
    H5S_hyper_span_info_t *ret_value=NULL;

    FUNC_ENTER (H5S_hyper_copy_span_helper, NULL);

    assert(spans);

    /* Check if the span tree was already copied */
    if(spans->scratch!=NULL && spans->scratch!=(void *)~((size_t)NULL)) {
        /* Just return the value of the already copied span tree */
        ret_value=spans->scratch;

        /* Increment the reference count of the span tree */
        ret_value->count++;
    } /* end if */
    else {
        /* Allocate a new span_info node */
        if((ret_value = H5FL_ALLOC(H5S_hyper_span_info_t,0))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");
           
        /* Copy the span_info information */
        ret_value->count=1;
        ret_value->scratch=NULL;
        ret_value->head=NULL;

        /* Set the scratch pointer in the node being copied to the newly allocated node */
        spans->scratch=ret_value;

        /* Copy over the nodes in the span list */
        span=spans->head;
        prev_span=NULL;
        while(span!=NULL) {
            /* Allocate a new node */
            if((new_span = H5S_hyper_new_span(span->low,span->high,NULL,NULL))==NULL)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

            /* Append to list of spans */
            if(prev_span==NULL)
                ret_value->head=new_span;
            else
                prev_span->next=new_span;

            /* Copy the pstride */
            new_span->pstride=span->pstride;

            /* Recurse to copy the 'down' spans, if there are any */
            if(span->down!=NULL) {
                if((new_down = H5S_hyper_copy_span_helper(span->down))==NULL)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");
                new_span->down=new_down;
            } /* end if */

            /* Update the previous (new) span */
            prev_span=new_span;

            /* Advance to next span */
            span=span->next;
        } /* end while */
    } /* end else */

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_copy_span_helper() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_copy_span
 PURPOSE
    Copy a hyperslab span tree
 USAGE
    H5S_hyper_span_info_t * H5S_hyper_copy_span(span_info)
        H5S_hyper_span_info_t *span_info;      IN: Span tree to copy
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Copy a hyperslab span tree, using reference counting as appropriate.
    (Which means that just the nodes in the top span tree are duplicated and
    the reference counts of their 'down spans' are just incremented)
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5S_hyper_span_info_t *
H5S_hyper_copy_span (H5S_hyper_span_info_t *spans)
{
    H5S_hyper_span_info_t *ret_value=NULL;

    FUNC_ENTER (H5S_hyper_copy_span, NULL);

    assert(spans);

    /* Copy the hyperslab span tree */
    ret_value=H5S_hyper_copy_span_helper(spans);

    /* Reset the scratch pointers for the next routine which needs them */
    if(H5S_hyper_span_scratch(spans,NULL)==FAIL)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, NULL, "can't reset span tree scratch pointers");

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_copy_span() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_cmp_spans
 PURPOSE
    Check if two hyperslab slabs are the same
 USAGE
    htri_d H5S_hyper_cmp_spans(span1, span2)
        H5S_hyper_span_t *span1;    IN: First span tree to compare
        H5S_hyper_span_t *span2;    IN: Second span tree to compare
 RETURNS
    TRUE (1) or FALSE (0) on success, negative on failure
 DESCRIPTION
    Compare two hyperslab slabs to determine if they refer to the same
    selection.  If span1 & span2 are both NULL, that counts as equal
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static htri_t
H5S_hyper_cmp_spans (H5S_hyper_span_info_t *span_info1, H5S_hyper_span_info_t *span_info2)
{
    H5S_hyper_span_t *span1;
    H5S_hyper_span_t *span2;
    htri_t nest=FAIL;
    htri_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_cmp_spans, FAIL);

    /* Check for redundant comparison */
    if(span_info1==span_info2) 
        ret_value=TRUE;
    else {
        /* Check for both spans being NULL */
        if(span_info1==NULL && span_info2==NULL)
            ret_value=TRUE;
        else {
            /* Check for one span being NULL */
            if(span_info1==NULL || span_info2==NULL)
                ret_value=FALSE;
            else {
                /* Get the pointers to the actual lists of spans */
                span1=span_info1->head;
                span2=span_info2->head;

                /* Sanity checking */
                assert(span1);
                assert(span2);

                /* infinite loop which must be broken out of */
                while (1) {
                    /* Check for both spans being NULL */
                    if(span1==NULL && span2==NULL) {
                        ret_value=TRUE;
                        break;
                    } /* end if */
                    else {
                        /* Check for one span being NULL */
                        if(span1==NULL || span2==NULL) {
                            ret_value=FALSE;
                            break;
                        } /* end if */
                        else {
                            /* Check if the actual low & high span information is the same */
                            if(span1->low!=span2->low || span1->high!=span2->high) {
                                ret_value=FALSE;
                                break;
                            } /* end if */
                            else {
                                if(span1->down!=NULL || span2!=NULL) {
                                    if((nest=H5S_hyper_cmp_spans(span1->down,span2->down))==FAIL) {
                                        ret_value=FAIL;
                                        break;
                                    } /* end if */
                                    else {
                                        if(nest==FALSE) {
                                            ret_value=FALSE;
                                            break;
                                        } /* end if */
                                        else {
                                            /* Keep going... */
                                        } /* end else */
                                    } /* end else */
                                } /* end if */
                                else {
                                    /* Keep going... */
                                } /* end else */
                            } /* end else */
                        } /* end else */
                    } /* end else */

                    /* Advance to the next nodes in the span list */
                    span1=span1->next;
                    span2=span2->next;
                } /* end while */
            } /* end else */
        } /* end else */
    } /* end else */

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_cmp_spans() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_free_span_info
 PURPOSE
    Free a hyperslab span info node
 USAGE
    herr_t H5S_hyper_free_span_info(span_info)
        H5S_hyper_span_info_t *span_info;      IN: Span info node to free
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Free a hyperslab span info node, along with all the span nodes and the
    'down spans' from the nodes, if reducing their reference count to zero
    indicates it is appropriate to do so.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_free_span_info (H5S_hyper_span_info_t *span_info)
{
    H5S_hyper_span_t *span, *next_span;
    herr_t ret_value=SUCCEED;

    FUNC_ENTER (H5S_hyper_free_span_info, FAIL);

    assert(span_info);

    /* Decrement the span tree's reference count */
    span_info->count--;

    /* Free the span tree if the reference count drops to zero */
    if(span_info->count==0) {

        /* Work through the list of spans pointed to by this 'info' node */
        span=span_info->head;
        while(span!=NULL) {
            next_span=span->next;
            if(H5S_hyper_free_span(span)<0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "failed to release hyperslab span");
            span=next_span;
        } /* end while */

        /* Free this span info */
        H5FL_FREE(H5S_hyper_span_info_t,span_info);
    } /* end if */

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_free_span_info() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_free_span
 PURPOSE
    Free a hyperslab span node
 USAGE
    herr_t H5S_hyper_free_span(span)
        H5S_hyper_span_t *span;      IN: Span node to free
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Free a hyperslab span node, along with the 'down spans' from the node,
    if reducing their reference count to zero indicates it is appropriate to
    do so.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_free_span (H5S_hyper_span_t *span)
{
    herr_t ret_value=SUCCEED;

    FUNC_ENTER (H5S_hyper_free_span, FAIL);

    assert(span);

    /* Decrement the reference count of the 'down spans', freeing them if appropriate */
    if(span->down!=NULL) {
        if(H5S_hyper_free_span_info(span->down)<0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "failed to release hyperslab span tree");
    } /* end if */

    /* Free this span */
    H5FL_FREE(H5S_hyper_span_t,span);

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_free_span() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_copy
 PURPOSE
    Copy a selection from one dataspace to another
 USAGE
    herr_t H5S_hyper_copy(dst, src)
        H5S_t *dst;  OUT: Pointer to the destination dataspace
        H5S_t *src;  IN: Pointer to the source dataspace
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Copies all the hyperslab selection information from the source
    dataspace to the destination dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_hyper_copy (H5S_t *dst, const H5S_t *src)
{
    H5S_hyper_dim_t *new_diminfo=NULL;	/* New per-dimension info array[rank] */
    unsigned u;                    /* Counters */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5S_hyper_copy, FAIL);

    assert(src);
    assert(dst);

    /* Check if there is regular hyperslab information to copy */
    if(src->select.sel_info.hslab.diminfo!=NULL) {
        /* Create the per-dimension selection info */
        if((new_diminfo = H5FL_ARR_ALLOC(H5S_hyper_dim_t,src->extent.u.simple.rank,0))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate per-dimension array");

        /* Copy the per-dimension selection info */
        for(u=0; u<src->extent.u.simple.rank; u++) {
            new_diminfo[u].start = src->select.sel_info.hslab.diminfo[u].start;
            new_diminfo[u].stride = src->select.sel_info.hslab.diminfo[u].stride;
            new_diminfo[u].count = src->select.sel_info.hslab.diminfo[u].count;
            new_diminfo[u].block = src->select.sel_info.hslab.diminfo[u].block;
        } /* end for */
        dst->select.sel_info.hslab.diminfo = new_diminfo;

        /* Create the per-dimension selection info */
        if((new_diminfo = H5FL_ARR_ALLOC(H5S_hyper_dim_t,src->extent.u.simple.rank,0))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate per-dimension array");

        /* Copy the per-dimension selection info */
        for(u=0; u<src->extent.u.simple.rank; u++) {
            new_diminfo[u].start = src->select.sel_info.hslab.app_diminfo[u].start;
            new_diminfo[u].stride = src->select.sel_info.hslab.app_diminfo[u].stride;
            new_diminfo[u].count = src->select.sel_info.hslab.app_diminfo[u].count;
            new_diminfo[u].block = src->select.sel_info.hslab.app_diminfo[u].block;
        } /* end for */
        dst->select.sel_info.hslab.app_diminfo = new_diminfo;
    } /* end if */
    else {
        dst->select.sel_info.hslab.diminfo = new_diminfo;
        dst->select.sel_info.hslab.app_diminfo = new_diminfo;
    } /* end else */

    /* Check if there is hyperslab span information to copy */
    if(src->select.sel_info.hslab.span_lst!=NULL) {
        /* Copy the hyperslab span information */
        dst->select.sel_info.hslab.span_lst=H5S_hyper_copy_span(src->select.sel_info.hslab.span_lst);
    } /* end if */

done:
    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_copy() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_select_valid_helper
 PURPOSE
    Check whether the selection fits within the extent, with the current
    offset defined.
 USAGE
    htri_t H5S_hyper_select_valid_helper(spans, offset, rank);
        const H5S_hyper_span_info_t *spans; IN: Pointer to current hyperslab span tree
        const hssize_t *offset;             IN: Pointer to offset array
        const hsize_t *size;                IN: Pointer to size array
        hsize_t rank;                       IN: Current rank looking at
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
static htri_t
H5S_hyper_select_valid_helper (const H5S_hyper_span_info_t *spans, const hssize_t *offset, const hsize_t *size, hsize_t rank)
{
    H5S_hyper_span_t *curr;     /* Hyperslab information nodes */
    htri_t tmp;                 /* temporary return value */
    htri_t ret_value=TRUE;      /* return value */

    FUNC_ENTER (H5S_hyper_select_valid_helper, FAIL);

    assert(spans);
    assert(offset);
    assert(size);
    assert(rank<H5O_LAYOUT_NDIMS);

    /* Check each point to determine whether selection+offset is within extent */
    curr=spans->head;
    while(curr!=NULL && ret_value==TRUE) {
        /* Check if an offset has been defined */
        /* Bounds check the selected point + offset against the extent */
        if(((curr->low+offset[rank])>=(hssize_t)size[rank])
                || ((curr->low+offset[rank])<0)
                || ((curr->high+offset[rank])>=(hssize_t)size[rank])
                || ((curr->high+offset[rank])<0)) {
            ret_value=FALSE;
            break;
        } /* end if */

        /* Recurse if this node has down spans */
        if(curr->down!=NULL) {
            if((tmp=H5S_hyper_select_valid_helper(curr->down,offset,size,rank+1))!=TRUE) {
                ret_value=tmp;
                break;
            } /* end if */
        } /* end if */

        /* Advance to next node */
        curr=curr->next;
    } /* end while */

    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_select_valid_helper() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_select_valid
 PURPOSE
    Check whether the selection fits within the extent, with the current
    offset defined.
 USAGE
    htri_t H5S_hyper_select_valid(space);
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
H5S_hyper_select_valid (const H5S_t *space)
{
    unsigned u;                    /* Counter */
    htri_t ret_value=TRUE;      /* return value */

    FUNC_ENTER (H5S_hyper_select_valid, FAIL);

    assert(space);

    /* Check for a "regular" hyperslab selection */
    if(space->select.sel_info.hslab.diminfo != NULL) {
        const H5S_hyper_dim_t *diminfo=space->select.sel_info.hslab.diminfo; /* local alias for diminfo */
        hssize_t end;      /* The high bound of a region in a dimension */

        /* Check each dimension */
        for(u=0; u<space->extent.u.simple.rank; u++) {
            /* if block or count is zero, then can skip the test since */
            /* no data point is chosen */
            if (diminfo[u].count*diminfo[u].block != 0) {
                /* Bounds check the start point in this dimension */
                if((diminfo[u].start+space->select.offset[u])<0 ||
                    (diminfo[u].start+space->select.offset[u])>=(hssize_t)space->extent.u.simple.size[u]) {
                    ret_value=FALSE;
                    break;
                } /* end if */

                /* Compute the largest location in this dimension */
                end=diminfo[u].start+diminfo[u].stride*(diminfo[u].count-1)+(diminfo[u].block-1)+space->select.offset[u];

                /* Bounds check the end point in this dimension */
                if(end<0 || end>=(hssize_t)space->extent.u.simple.size[u]) {
                    ret_value=FALSE;
                    break;
                } /* end if */
            }
        } /* end for */
    } /* end if */
    else {
        /* Call the recursive routine to validate the span tree */
        ret_value=H5S_hyper_select_valid_helper(space->select.sel_info.hslab.span_lst,space->select.offset,space->extent.u.simple.size,(hsize_t)0);
    } /* end else */

    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_select_valid() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_span_nblocks
 PURPOSE
    Count the number of blocks in a span tree
 USAGE
    hssize_t H5S_hyper_span_nblocks(spans)
        const H5S_hyper_span_info_t *spans; IN: Hyperslan span tree to count elements of
 RETURNS
    Number of blocks in span tree on success; negative on failure
 DESCRIPTION
    Counts the number of blocks described by the spans in a span tree.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hssize_t
H5S_hyper_span_nblocks (H5S_hyper_span_info_t *spans)
{
    H5S_hyper_span_t *span;     /* Hyperslab span */
    hssize_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_span_nblocks, FAIL);

    /* Count the number of elements in the span tree */
    if(spans==NULL)
        ret_value=0;
    else {
        span=spans->head;
        ret_value=0;
        while(span!=NULL) {
            /* If there are down spans, add the total down span blocks */
            if(span->down!=NULL)
                ret_value+=H5S_hyper_span_nblocks(span->down);
            /* If there are no down spans, just count the block in this span */
            else
                ret_value++;
            
            /* Advance to next span */
            span=span->next;
        } /* end while */
    } /* end else */

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_span_nblocks() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_select_serial_size
 PURPOSE
    Determine the number of bytes needed to store the serialized hyperslab
        selection information.
 USAGE
    hssize_t H5S_hyper_select_serial_size(space)
        H5S_t *space;             IN: Dataspace pointer to query
 RETURNS
    The number of bytes required on success, negative on an error.
 DESCRIPTION
    Determines the number of bytes required to serialize the current hyperslab
    selection information for storage on disk.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hssize_t
H5S_hyper_select_serial_size (const H5S_t *space)
{
    unsigned u;                    /* Counter */
    hssize_t block_count;       /* block counter for regular hyperslabs */
    hssize_t ret_value=FAIL;    /* return value */

    FUNC_ENTER (H5S_hyper_select_serial_size, FAIL);

    assert(space);

    /* Basic number of bytes required to serialize point selection:
     *  <type (4 bytes)> + <version (4 bytes)> + <padding (4 bytes)> + 
     *      <length (4 bytes)> + <rank (4 bytes)> + <# of blocks (4 bytes)> = 24 bytes
     */
    ret_value=24;

    /* Check for a "regular" hyperslab selection */
    if(space->select.sel_info.hslab.diminfo != NULL) {
        /* Check each dimension */
        for(block_count=1,u=0; u<space->extent.u.simple.rank; u++)
            block_count*=space->select.sel_info.hslab.diminfo[u].count;
        ret_value+=8*block_count*space->extent.u.simple.rank;
    } /* end if */
    else {
        /* Spin through hyperslab spans, adding 8 * rank bytes for each block */
        block_count=H5S_hyper_span_nblocks(space->select.sel_info.hslab.span_lst);
        ret_value+=8*space->extent.u.simple.rank*block_count;
    } /* end else */

    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_select_serial_size() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_select_serialize_helper
 PURPOSE
    Serialize the current selection into a user-provided buffer.
 USAGE
    herr_t H5S_hyper_select_serialize_helper(spans, start, end, rank, buf)
        H5S_hyper_span_info_t *spans;   IN: Hyperslab span tree to serialize
        hssize_t start[];       IN/OUT: Accumulated start points
        hssize_t end[];         IN/OUT: Accumulated end points
        hsize_t rank;           IN: Current rank looking at
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
static herr_t
H5S_hyper_select_serialize_helper (const H5S_hyper_span_info_t *spans, hssize_t *start, hssize_t *end, hsize_t rank, uint8_t **buf)
{
    H5S_hyper_span_t *curr;     /* Pointer to current hyperslab span */
    hsize_t u;                  /* Index variable */
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER (H5S_hyper_select_serialize_helper, FAIL);

    /* Sanity checks */
    assert(spans);
    assert(start);
    assert(end);
    assert(rank<H5O_LAYOUT_NDIMS);
    assert(buf && *buf);

    /* Walk through the list of spans, recursing or outputing them */
    curr=spans->head;
    while(curr!=NULL) {
        /* Recurse if this node has down spans */
        if(curr->down!=NULL) {
            /* Add the starting and ending points for this span to the list */
            start[rank]=curr->low;
            end[rank]=curr->high;

            /* Recurse down to the next dimension */
            if(H5S_hyper_select_serialize_helper(curr->down,start,end,rank+1,buf)<0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "failed to release hyperslab spans");
        } /* end if */
        else {
            /* Encode all the previous dimensions starting & ending points */

            /* Encode previous starting points */
            for(u=0; u<rank; u++)
                UINT32ENCODE(*buf, (uint32_t)start[u]);

            /* Encode starting point for this span */
            UINT32ENCODE(*buf, (uint32_t)curr->low);

            /* Encode previous ending points */
            for(u=0; u<rank; u++)
                UINT32ENCODE(*buf, (uint32_t)end[u]);

            /* Encode starting point for this span */
            UINT32ENCODE(*buf, (uint32_t)curr->high);
        } /* end else */

        /* Advance to next node */
        curr=curr->next;
    } /* end while */

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_select_serialize_helper() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_select_serialize
 PURPOSE
    Serialize the current selection into a user-provided buffer.
 USAGE
    herr_t H5S_hyper_select_serialize(space, buf)
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
H5S_hyper_select_serialize (const H5S_t *space, uint8_t *buf)
{
    H5S_hyper_dim_t *diminfo;               /* Alias for dataspace's diminfo information */
    hsize_t tmp_count[H5O_LAYOUT_NDIMS];    /* Temporary hyperslab counts */
    hssize_t offset[H5O_LAYOUT_NDIMS];      /* Offset of element in dataspace */
    hssize_t start[H5O_LAYOUT_NDIMS];   /* Location of start of hyperslab */
    hssize_t end[H5O_LAYOUT_NDIMS];     /* Location of end of hyperslab */
    hssize_t temp_off;            /* Offset in a given dimension */
    uint8_t *lenp;          /* pointer to length location for later storage */
    uint32_t len=0;         /* number of bytes used */
    int i;                 /* local counting variable */
    hssize_t block_count;       /* block counter for regular hyperslabs */
    int fast_dim;      /* Rank of the fastest changing dimension for the dataspace */
    int temp_dim;      /* Temporary rank holder */
    int ndims;         /* Rank of the dataspace */
    int done;          /* Whether we are done with the iteration */
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER (H5S_hyper_select_serialize, FAIL);

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

    /* Check for a "regular" hyperslab selection */
    if(space->select.sel_info.hslab.diminfo != NULL) {
        /* Set some convienence values */
        ndims=space->extent.u.simple.rank;
        fast_dim=ndims-1;
        diminfo=space->select.sel_info.hslab.diminfo;

        /* Check each dimension */
        for(block_count=1,i=0; i<ndims; i++)
            block_count*=diminfo[i].count;

        /* Encode number of hyperslabs */
        UINT32ENCODE(buf, (uint32_t)block_count);
        len+=4;

        /* Now serialize the information for the regular hyperslab */

        /* Build the tables of count sizes as well as the initial offset */
        for(i=0; i<ndims; i++) {
            tmp_count[i]=diminfo[i].count;
            offset[i]=diminfo[i].start;
        } /* end for */

        /* We're not done with the iteration */
        done=0;

        /* Go iterate over the hyperslabs */
        while(done==0) {
            /* Iterate over the blocks in the fastest dimension */
            while(tmp_count[fast_dim]>0) {
                /* Add 8 bytes times the rank for each hyperslab selected */
                len+=8*ndims;

                /* Encode hyperslab starting location */
                for(i=0; i<ndims; i++)
                    UINT32ENCODE(buf, (uint32_t)offset[i]);

                /* Encode hyperslab ending location */
                for(i=0; i<ndims; i++)
                    UINT32ENCODE(buf, (uint32_t)(offset[i]+(diminfo[i].block-1)));

                /* Move the offset to the next sequence to start */
                offset[fast_dim]+=diminfo[fast_dim].stride;

                /* Decrement the block count */
                tmp_count[fast_dim]--;
            } /* end while */

            /* Work on other dimensions if necessary */
            if(fast_dim>0) {
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
                temp_off=diminfo[i].start
                    +diminfo[i].stride*(diminfo[i].count-tmp_count[i]);
                offset[i]=temp_off;
            } /* end for */
        } /* end while */
    } /* end if */
    else {
        /* Encode number of hyperslabs */
        block_count=H5S_hyper_span_nblocks(space->select.sel_info.hslab.span_lst);
        UINT32ENCODE(buf, (uint32_t)block_count);
        len+=4;

        /* Add 8 bytes times the rank for each hyperslab selected */
        len+=8*space->extent.u.simple.rank*block_count;

        /* Encode each hyperslab in selection */
        H5S_hyper_select_serialize_helper(space->select.sel_info.hslab.span_lst,start,end,(hsize_t)0,&buf);
    } /* end else */

    /* Encode length */
    UINT32ENCODE(lenp, (uint32_t)len);  /* Store the length of the extra information */
    
    /* Set success */
    ret_value=SUCCEED;

    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_select_serialize() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_select_deserialize
 PURPOSE
    Deserialize the current selection from a user-provided buffer.
 USAGE
    herr_t H5S_hyper_select_deserialize(space, buf)
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
H5S_hyper_select_deserialize (H5S_t *space, const uint8_t *buf)
{
    uint32_t rank;           	/* rank of points */
    size_t num_elem=0;      	/* number of elements in selection */
    hssize_t *start=NULL;	/* hyperslab start information */
    hssize_t *end=NULL;	    /* hyperslab end information */
    hsize_t *count=NULL;    	/* hyperslab count information */
    hsize_t *block=NULL;    	/* hyperslab block information */
    hssize_t *tstart=NULL;	/* temporary hyperslab pointers */
    hssize_t *tend=NULL;	/* temporary hyperslab pointers */
    hsize_t *tcount=NULL;	/* temporary hyperslab pointers */
    hsize_t *tblock=NULL;	/* temporary hyperslab pointers */
    unsigned i,j;              	/* local counting variables */
    herr_t ret_value=FAIL;  	/* return value */

    FUNC_ENTER (H5S_hyper_select_deserialize, FAIL);

    /* Check args */
    assert(space);
    assert(buf);

    /* Deserialize slabs to select */
    buf+=16;    /* Skip over selection header */
    UINT32DECODE(buf,rank);  /* decode the rank of the point selection */
    if(rank!=space->extent.u.simple.rank)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "rank of pointer does not match dataspace");
    UINT32DECODE(buf,num_elem);  /* decode the number of points */

    /* Allocate space for the coordinates */
    if((start = H5FL_ARR_ALLOC(hsize_t,rank,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab information");
    if((end = H5FL_ARR_ALLOC(hsize_t,rank,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab information");
    if((block = H5FL_ARR_ALLOC(hsize_t,rank,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab information");
    if((count = H5FL_ARR_ALLOC(hsize_t,rank,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab information");
    
    /* Set the count for all blocks */
    for(tcount=count,j=0; j<rank; j++,tcount++)
        *tcount=1;

    /* Retrieve the coordinates from the buffer */
    for(i=0; i<num_elem; i++) {
        /* Decode the starting points */
        for(tstart=start,j=0; j<rank; j++,tstart++)
            UINT32DECODE(buf, *tstart);

        /* Decode the ending points */
        for(tend=end,j=0; j<rank; j++,tend++)
            UINT32DECODE(buf, *tend);

        /* Change the ending points into blocks */
        for(tblock=block,tstart=start,tend=end,j=0; j<(unsigned)rank; j++,tstart++,tend++,tblock++)
            *tblock=(*tend-*tstart)+1;

        /* Select or add the hyperslab to the current selection */
        if((ret_value=H5S_select_hyperslab(space,(i==0 ? H5S_SELECT_SET : H5S_SELECT_OR),start,NULL,count,block))<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection");
    } /* end for */

    /* Free temporary buffers */
    H5FL_ARR_FREE(hsize_t,start);
    H5FL_ARR_FREE(hsize_t,end);
    H5FL_ARR_FREE(hsize_t,count);
    H5FL_ARR_FREE(hsize_t,block);

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_select_deserialize() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_span_blocklist
 PURPOSE
    Get a list of hyperslab blocks currently selected
 USAGE
    herr_t H5S_hyper_span_blocklist(spans, start, end, rank, startblock, numblocks, buf)
        H5S_hyper_span_info_t *spans;   IN: Dataspace pointer of selection to query
        hssize_t start[];       IN/OUT: Accumulated start points
        hssize_t end[];         IN/OUT: Accumulated end points
        hsize_t rank;           IN: Rank of dataspace
        hsize_t *startblock;    IN/OUT: Hyperslab block to start with
        hsize_t *numblocks;     IN/OUT: Number of hyperslab blocks to get
        hsize_t **buf;          OUT: List of hyperslab blocks selected
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
        Puts a list of the hyperslab blocks into the user's buffer.  The blocks
    start with the '*startblock'th block in the list of blocks and put
    '*numblocks' number of blocks into the user's buffer (or until the end of
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
herr_t
H5S_hyper_span_blocklist(H5S_hyper_span_info_t *spans, hssize_t start[], hssize_t end[], hsize_t rank, hsize_t *startblock, hsize_t *numblocks, hsize_t **buf)
{
    H5S_hyper_span_t *curr;     /* Pointer to current hyperslab span */
    hsize_t u;                  /* Index variable */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5S_hyper_span_blocklist, FAIL);

    /* Sanity checks */
    assert(spans);
    assert(rank<H5O_LAYOUT_NDIMS);
    assert(start);
    assert(end);
    assert(startblock);
    assert(numblocks && *numblocks>0);
    assert(buf && *buf);

    /* Walk through the list of spans, recursing or outputing them */
    curr=spans->head;
    while(curr!=NULL && *numblocks>0) {
        /* Recurse if this node has down spans */
        if(curr->down!=NULL) {
            /* Add the starting and ending points for this span to the list */
            start[rank]=curr->low;
            end[rank]=curr->high;

            /* Recurse down to the next dimension */
            if(H5S_hyper_span_blocklist(curr->down,start,end,rank+1,startblock,numblocks,buf)<0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "failed to release hyperslab spans");
        } /* end if */
        else {
            /* Skip this block if we haven't skipped all the startblocks yet */
            if(*startblock>0) {
                /* Decrement the starting block */
                (*startblock)--;
            }
            /* Process this block */
            else {
                /* Encode all the previous dimensions starting & ending points */

                /* Copy previous starting points */
                for(u=0; u<rank; u++, (*buf)++)
                    HDmemcpy(*buf, &start[u], sizeof(hsize_t));

                /* Copy starting point for this span */
                HDmemcpy(*buf, &curr->low, sizeof(hsize_t));
                (*buf)++;

                /* Copy previous ending points */
                for(u=0; u<rank; u++, (*buf)++)
                    HDmemcpy(*buf, &end[u], sizeof(hsize_t));

                /* Copy starting point for this span */
                HDmemcpy(*buf, &curr->high, sizeof(hsize_t));
                (*buf)++;

                /* Decrement the number of blocks processed */
                (*numblocks)--;
            } /* end else */
        } /* end else */

        /* Advance to next node */
        curr=curr->next;
    } /* end while */

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_span_blocklist() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_bounds_helper
 PURPOSE
    Gets the bounding box containing the selection.
 USAGE
    htri_t H5S_hyper_bounds_helper(spans, offset, rank);
        const H5S_hyper_span_info_t *spans; IN: Pointer to current hyperslab span tree
        const hssize_t *offset;         IN: Pointer to offset array
        hsize_t rank;                   IN: Current rank looking at
        hsize_t *start;                 OUT: Start array bounds
        hsize_t *end;                   OUT: End array bounds
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
static herr_t
H5S_hyper_bounds_helper (const H5S_hyper_span_info_t *spans, const hssize_t *offset, hsize_t rank, hsize_t *start, hsize_t *end)
{
    H5S_hyper_span_t *curr;     /* Hyperslab information nodes */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5S_hyper_bounds_helper, FAIL);

    assert(spans);
    assert(offset);
    assert(rank<H5O_LAYOUT_NDIMS);
    assert(start);
    assert(end);

    /* Check each point to determine whether selection+offset is within extent */
    curr=spans->head;
    while(curr!=NULL) {
        /* Check if the current span extends the bounding box */
        if((curr->low+offset[rank])<(hssize_t)start[rank])
            start[rank]=curr->low+offset[rank];
        if((curr->high+offset[rank])>(hssize_t)end[rank])
            end[rank]=curr->high+offset[rank];

        /* Recurse if this node has down spans */
        if(curr->down!=NULL) {
            if(H5S_hyper_bounds_helper(curr->down,offset,rank+1,start,end)<0) {
                ret_value=FAIL;
                break;
            } /* end if */
        } /* end if */

        /* Advance to next node */
        curr=curr->next;
    } /* end while */

    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_bounds_helper() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_bounds
 PURPOSE
    Gets the bounding box containing the selection.
 USAGE
    herr_t H5S_hyper_bounds(space, hsize_t *start, hsize_t *end)
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
H5S_hyper_bounds(H5S_t *space, hsize_t *start, hsize_t *end)
{
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5S_hyper_bounds, FAIL);

    assert(space);
    assert(start);
    assert(end);

    /* Check for a "regular" hyperslab selection */
    if(space->select.sel_info.hslab.diminfo!=NULL) {
        const H5S_hyper_dim_t *diminfo=space->select.sel_info.hslab.diminfo; /* local alias for diminfo */
        int rank;              /* Dataspace rank */
        int i;                 /* index variable */

        /* Get the dataspace extent rank */
        rank=space->extent.u.simple.rank;

        /* Check each dimension */
        for(i=0; i<rank; i++) {
            /* Compute the smallest location in this dimension */
            start[i]=diminfo[i].start+space->select.offset[i];

            /* Compute the largest location in this dimension */
            end[i]=diminfo[i].start+diminfo[i].stride*(diminfo[i].count-1)+(diminfo[i].block-1)+space->select.offset[i];
        } /* end for */
    } /* end if */
    else {
        /* Call the recursive routine to get the bounds for the span tree */
        ret_value=H5S_hyper_bounds_helper(space->select.sel_info.hslab.span_lst,space->select.offset,(hsize_t)0,start,end);
    } /* end if */

    FUNC_LEAVE (ret_value);
}   /* H5Sget_hyper_bounds() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_select_contiguous
 PURPOSE
    Check if a hyperslab selection is contiguous within the dataspace extent.
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
H5S_hyper_select_contiguous(const H5S_t *space)
{
    htri_t ret_value=FAIL;  /* return value */
    H5S_hyper_span_info_t *spans;   /* Hyperslab span info node */
    H5S_hyper_span_t *span;     /* Hyperslab span node */
    unsigned u;                    /* index variable */

    FUNC_ENTER (H5S_hyper_select_contiguous, FAIL);

    assert(space);

    /* Quicker check for a "regular" hyperslab selection */
    if(space->select.sel_info.hslab.diminfo != NULL) {
        /*
         * For a regular hyperslab to be contiguous, it must have only one
         * block (i.e. count==1 in all dimensions) and the block size must be
         * the same as the dataspace extent's in all but the slowest changing
         * dimension.
         */
        ret_value=TRUE;	/* assume true and reset if the dimensions don't match */
        for(u=1; u<space->extent.u.simple.rank; u++) {
            if(space->select.sel_info.hslab.diminfo[u].count>1 || space->select.sel_info.hslab.diminfo[u].block!=space->extent.u.simple.size[u]) {
                ret_value=FALSE;
                break;
            } /* end if */
        } /* end for */
    } /* end if */
    else {
        /*
         * For a hyperslab to be contiguous, it's size must be the same as the
         * dataspace extent's in all but the slowest changing dimension
         */
        ret_value=TRUE;	/* assume true and reset if the dimensions don't match */

        /* Get information for slowest changing information */
        spans=space->select.sel_info.hslab.span_lst;
        span=spans->head;

        /* If there are multiple spans in the slowest changing dimension, the selection isn't contiguous */
        if(span->next!=NULL)
            ret_value=FALSE;
        else {
            /* Now check the rest of the dimensions */
            if(span->down!=NULL) {
                u=1;    /* Current dimension working on */

                /* Get the span information for the next fastest dimension */
                spans=span->down;

                /* Cycle down the spans until we run out of down spans or find a non-contiguous span */
                while(spans!=NULL) {
                    span=spans->head;

                    /* Check that this is the only span and it spans the entire dimension */
                    if(span->next!=NULL) {
                        ret_value=FALSE;
                        break;
                    } /* end if */
                    else {
                        /* If this span doesn't cover the entire dimension, then this selection isn't contiguous */
                        if(((span->high-span->low)+1)!=(hssize_t)space->extent.u.simple.size[u]) {
                            ret_value=FALSE;
                            break;
                        } /* end if */
                        else {
                            /* Walk down to the next span */
                            spans=span->down;

                            /* Increment dimension */
                            u++;
                        } /* end else */
                    } /* end else */
                } /* end while */
            } /* end if */
        } /* end else */
    } /* end else */

    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_select_contiguous() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_select_iterate_helper
 PURPOSE
    Internal routine to iterate over the elements of a span tree hyperslab selection
 USAGE
    herr_t H5S_iterate_hyperslab_io(iter_info)
        H5S_hyper_iter_info_t *iter_info;   IN/OUT: Block of iteration parameters to pass into recursive calls
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Iterates over the elements in a hyperslab span tree selection, calling a
    user's callback routine for each element.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_select_iterate_helper(H5S_hyper_iter_info_t *iter_info)
{
    const H5S_t *space; /* Dataspace operating with */
    H5S_sel_iter_t *iter;   /* Selection iterator */
    H5S_hyper_span_t *curr_span;    /* Current hyperslab span node */
    hsize_t slab[H5O_LAYOUT_NDIMS]; /* Cumulative size of each dimension in bytes */
    hsize_t acc;        /* Accumulator for computing cumulative sizes */
    hssize_t off_arr[H5O_LAYOUT_NDIMS];  /* Current hyperslab span position */
    int fast_dim;      /* Rank of the fastest changing dimension for the dataspace */
    int curr_dim;      /* Current dimension being operated on */
    int ndims;         /* Number of dimensions of dataset */
    hsize_t span_io;    /* Number of elements in current span to actually process */
    herr_t user_ret=0;  /* User's return value */
    uint8_t *loc;       /* Current element location pointer */
    hsize_t loc_off;    /* Element offset in the dataspace */
    int i;             /* Index variable */
    unsigned u;            /* Index variable */
    herr_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_select_iterate_helper, FAIL);

    /* Check args */
    assert(iter_info);

    /* Retrieve some information from the interation info */
    space=iter_info->space;
    iter=iter_info->iter;

    /* Set the rank of the fastest changing dimension */
    ndims=space->extent.u.simple.rank;
    fast_dim=(ndims-1);

    /* Get the pointers to the current span info and span nodes */
    curr_span=iter->hyp.span[fast_dim];

    /* Compute the cumulative size of dataspace dimensions */
    for(i=fast_dim, acc=iter_info->elem_size; i>=0; i--) {
        slab[i]=acc;
        acc*=space->extent.u.simple.size[i];
    } /* end for */

    /* Set the offset of the first element iterated on */
    for(i=0, loc_off=0; i<ndims; i++) {
        /* Set the location */
        off_arr[i]=iter->hyp.span[i]->low;

        /* Compute the sequential element offset */
        loc_off+=off_arr[i]*slab[i];
    } /* end for */

    /* Perform the I/O on the elements, based on the position of the iterator */
    user_ret=0;
    while(curr_span!=NULL && user_ret==0) {
        /* Compute the number of elements to attempt in this span */
        span_io=(curr_span->high-curr_span->low)+1;

        /* Iterate through all the span elements */
        for(u=0, loc=(uint8_t *)iter_info->src+loc_off; u<span_io && user_ret==0; u++) {
            /* Call the user's callback routine */
            user_ret=(*(iter_info->op))(loc,iter_info->dt,(hsize_t)ndims,off_arr,iter_info->op_data);

            /* Increment the element location */
            off_arr[fast_dim]++;

            /* Increment the buffer offset */
            loc+=slab[fast_dim];
        } /* end for */

        /* Get out now for user return values not zero */
        if(user_ret!=0)
            break;

        /* Adjust iterator pointers */

        /* Advance span in fastest dimension */
        curr_span=curr_span->next;

        /* See if we are still in the fastest changing dimension */
        if(curr_span!=NULL) {
            /* Move the buffer offset */
            loc_off+=(span_io+(curr_span->low-off_arr[fast_dim]))*iter_info->elem_size;

            /* Move the element location */
            off_arr[fast_dim]=curr_span->low;
        } /* end if */
        /* We walked off the spans for the fastest dimension, work our way back up */
        else {
            /* Start at the fastest dim */
            curr_dim=fast_dim-1;

            /* Get the pointer to the correct dimension */
            curr_span=iter->hyp.span[curr_dim];

            /* Work back up through the dimensions */
            while(curr_dim>=0) {
                /* Increment position in span */
                off_arr[curr_dim]++;

                /* Check if we are still within the span */
                if(off_arr[curr_dim]<=curr_span->high) {
                    break;
                } /* end if */
                /* If we walked off that span, advance to the next span */
                else {
                    /* Advance span in this dimension */
                    curr_span=curr_span->next;

                    /* Check if we have a valid span in this dimension still */
                    if(curr_span!=NULL) {
                        /* Reset the offset for the dim */
                        off_arr[curr_dim]=curr_span->low;

                        break;
                    } /* end if */
                    else {
                        /* If we finished the span list in this dimension, decrement the dimension worked on and loop again */
                        curr_dim--;

                        /* Reset the curr_span to the next dim */
                        if(curr_dim>=0)
                            curr_span=iter->hyp.span[curr_dim];
                    } /* end else */
                } /* end else */
            } /* end while */

            /* Check if we are finished with the spans in the tree */
            if(curr_dim<0) {
                /* We had better be done with I/O or bad things are going to happen... */
                break;
            } /* end if */
            else {
                /* Reset the span in the current dimension */
                iter->hyp.span[curr_dim]=curr_span;

                /* Walk back down the iterator positions, reseting them */
                while(curr_dim<fast_dim) {
                    assert(curr_span);
                    assert(curr_span->down);
                    assert(curr_span->down->head);

                    /* Set the new span for this dimension */
                    iter->hyp.span[curr_dim+1]=curr_span->down->head;

                    /* Advance span down the tree */
                    curr_span=curr_span->down->head;

                    /* Reset the offset for the dim */
                    off_arr[curr_dim+1]=curr_span->low;

                    /* Increment current dimension */
                    curr_dim++;
                } /* end while */

                /* Verify that the curr_span points to the fastest dim */
                assert(curr_span==iter->hyp.span[fast_dim]);

                /* Verify that the offset is correct for the fastest dim */
                assert(off_arr[fast_dim]==curr_span->low);
            } /* end else */

            /* Reset the buffer offset */
            for(i=0, loc_off=0; i<ndims; i++)
                loc_off+=off_arr[i]*slab[i];
        } /* end else */
    } /* end while */

    /* Success! */
    ret_value=(user_ret==0 ? SUCCEED : user_ret);

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_select_iterate_helper() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_select_iterate_mem_opt
 PURPOSE
    Iterate over the data points in a regular hyperslab selection, calling a
    user's function for each element.
 USAGE
    herr_t H5S_hyper_select_iterate_mem_opt(buf, type_id, space, op, operator_data)
        H5S_sel_iter_t *iter;   IN/OUT: Selection iterator
        void *buf;      IN/OUT: Buffer containing elements to iterate over
        hid_t type_id;  IN: Datatype ID of BUF array.
        H5S_t *space;   IN: Dataspace object containing selection to iterate over
        H5D_operator_t op; IN: Function pointer to the routine to be
                                called for each element in BUF iterated over.
        void *op_data;  IN/OUT: Pointer to any user-defined data associated
                                with the operation.
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
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_select_iterate_mem_opt(H5S_sel_iter_t * UNUSED iter, void *buf, hid_t type_id, H5S_t *space, H5D_operator_t op,
        void *op_data)
{
    H5S_hyper_dim_t *diminfo;               /* Alias for dataspace's diminfo information */
    hsize_t tmp_count[H5O_LAYOUT_NDIMS];    /* Temporary hyperslab counts */
    hsize_t tmp_block[H5O_LAYOUT_NDIMS];    /* Temporary hyperslab blocks */
    hssize_t offset[H5O_LAYOUT_NDIMS];      /* Offset of element in dataspace */
    hsize_t slab[H5O_LAYOUT_NDIMS];         /* Size of objects in buffer */
    size_t elem_size;           /* Size of data element in buffer */
    hssize_t temp_off;            /* Offset in a given dimension */
    uint8_t *loc;               /* Current element location */
    int i;                     /* Counter */
    unsigned u;                    /* Counter */
    int fast_dim;      /* Rank of the fastest changing dimension for the dataspace */
    int temp_dim;      /* Temporary rank holder */
    unsigned ndims;        /* Rank of the dataspace */
    H5T_t *dt;                  /* Datatype structure */
    herr_t user_ret=0;          /* User's return value */

    FUNC_ENTER (H5S_hyper_select_iterate_mem_opt, FAIL);

    /* Set some convienence values */
    ndims=space->extent.u.simple.rank;
    fast_dim=ndims-1;
    diminfo=space->select.sel_info.hslab.diminfo;

    /* Get the data element size */
    if (NULL==(dt=H5I_object(type_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an valid base datatype");
    elem_size=H5T_get_size(dt);

    /* Elements in the fastest dimension are 'elem_size' */
    slab[ndims-1]=elem_size;

    /* If we have two or more dimensions, build the other dimension's element sizes */
    if(ndims>=2) {
        /* Build the table of next-dimension down 'element' sizes */
        for(i=ndims-2; i>=0; i--)
            slab[i]=slab[i+1]*space->extent.u.simple.size[i+1];
    } /* end if */

    /* Build the tables of count & block sizes as well as the initial offset */
    for(u=0; u<ndims; u++) {
        tmp_count[u]=diminfo[u].count;
        tmp_block[u]=diminfo[u].block;
        offset[u]=diminfo[u].start;
    } /* end for */

    /* Initialize the starting location */
    for(loc=buf,u=0; u<ndims; u++)
        loc+=diminfo[u].start*slab[u];

    /* Go iterate over the hyperslabs */
    while(user_ret==0) {
        /* Iterate over the blocks in the fastest dimension */
        while(tmp_count[fast_dim]>0 && user_ret==0) {

            /* Iterate over the elements in the fastest dimension */
            while(tmp_block[fast_dim]>0 && user_ret==0) {
                user_ret=(*op)(loc,type_id,(hsize_t)ndims,offset,op_data);

                /* Increment the buffer location */
                loc+=slab[fast_dim];

                /* Increment the offset in the dataspace */
                offset[fast_dim]++;

                /* Decrement the sequence count */
                tmp_block[fast_dim]--;
            } /* end while */

            /* Reset the sequence count */
            tmp_block[fast_dim]=diminfo[fast_dim].block;

            /* Move the location to the next sequence to start */
            loc+=(diminfo[fast_dim].stride-diminfo[fast_dim].block)*slab[fast_dim];
             
            /* Move the offset to the next sequence to start */
            offset[fast_dim]+=(diminfo[fast_dim].stride-diminfo[fast_dim].block);

            /* Decrement the block count */
            tmp_count[fast_dim]--;
        } /* end while */

        /* Check for getting out of iterator, we're done in the 1-D case */
        if(ndims==1)
            goto done; /* Yes, an evil goto.. :-) -QAK */

        /* Work on other dimensions if necessary */
        if(fast_dim>0 && user_ret==0) {
            /* Reset the sequence and block counts */
            tmp_block[fast_dim]=diminfo[fast_dim].block;
            tmp_count[fast_dim]=diminfo[fast_dim].count;

            /* Bubble up the decrement to the slower changing dimensions */
            temp_dim=fast_dim-1;
            while(temp_dim>=0) {
                /* Decrement the sequence count in this dimension */
                tmp_block[temp_dim]--;

                /* Check if we are still in the sequence */
                if(tmp_block[temp_dim]>0)
                    break;

                /* Reset the sequence count in this dimension */
                tmp_block[temp_dim]=diminfo[temp_dim].block;

                /* Decrement the block count */
                tmp_count[temp_dim]--;

                /* Check if we have more blocks left */
                if(tmp_count[temp_dim]>0)
                    break;

                /* Check for getting out of iterator */
                if(temp_dim==0)
                    goto done; /* Yes, an evil goto.. :-) -QAK */

                /* Reset the block count in this dimension */
                tmp_count[temp_dim]=diminfo[temp_dim].count;
            
                /* Wrapped a dimension, go up to next dimension */
                temp_dim--;
            } /* end while */
        } /* end if */

        /* Re-compute buffer location & offset array */
        for(loc=buf,u=0; u<ndims; u++) {
            temp_off=diminfo[u].start
                +diminfo[u].stride*(diminfo[u].count-tmp_count[u])
                    +(diminfo[u].block-tmp_block[u]);
            loc+=temp_off*slab[u];
            offset[u]=temp_off;
        } /* end for */
    } /* end while */

done:
    FUNC_LEAVE (user_ret);
} /* end H5S_hyper_select_iterate_mem_opt() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_select_iterate
 PURPOSE
    Iterate over a hyperslab selection, calling a user's function for each
        element.
 USAGE
    herr_t H5S_hyper_select_iterate(buf, type_id, space, op, operator_data)
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
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_hyper_select_iterate(void *buf, hid_t type_id, H5S_t *space, H5D_operator_t op,
        void *operator_data)
{
    H5S_hyper_iter_info_t iter_info;  /* Block of parameters to pass into recursive calls */
    H5S_sel_iter_t	iter;   /* selection iteration info*/
    size_t elmt_size;           /* Datatype size */
    H5T_t *dt;                  /* Datatype structure */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5S_hyper_select_iterate, FAIL);

    assert(buf);
    assert(space);
    assert(op);
    assert(H5I_DATATYPE == H5I_get_type(type_id));

    /* Initialize iterator */
    HDmemset(&iter,0,sizeof(H5S_sel_iter_t));

    /* Get the datatype size */
    if (NULL==(dt=H5I_object(type_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an valid base datatype");
    elmt_size=H5T_get_size(dt);

    /* Construct iterator for hyperslab selection */
    if (H5S_hyper_init(space, elmt_size, &iter)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection information");

    /* Check for the special case of just one H5Sselect_hyperslab call made */
    if(space->select.sel_info.hslab.diminfo!=NULL) {
        /* Use optimized call to iterate over regular hyperslab */
        ret_value=H5S_hyper_select_iterate_mem_opt(&iter,buf,type_id,space,op,operator_data);
    }
    else {
        /* Initialize parameter block for recursive calls */
        iter_info.dt=type_id;
        iter_info.elem_size=elmt_size;
        iter_info.space=space;
        iter_info.iter=&iter;
        iter_info.src=buf;

        /* Copy the location of the region in the file */
        iter_info.op=op;
        iter_info.op_data=operator_data;

        /* Call the recursive iterator routine */
        ret_value=H5S_hyper_select_iterate_helper(&iter_info);
    } /* end else */

    /* Release selection iterator */
    H5S_sel_iter_release(space,&iter);

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_select_iterate() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_release
 PURPOSE
    Release hyperslab selection information for a dataspace
 USAGE
    herr_t H5S_hyper_release(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Releases all hyperslab selection information for a dataspace
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
 * 	Robb Matzke, 1998-08-25
 *	The fields which are freed are set to NULL to prevent them from being
 *	freed again later.  This fixes some allocation problems where
 *	changing the hyperslab selection of one data space causes a core dump
 *	when closing some other data space.
--------------------------------------------------------------------------*/
herr_t
H5S_hyper_release (H5S_t *space)
{
    herr_t ret_value=SUCCEED;

    FUNC_ENTER (H5S_hyper_release, FAIL);

    /* Check args */
    assert (space && H5S_SEL_HYPERSLABS==space->select.type);

    /* Reset the number of points selected */
    space->select.num_elem=0;

    /* Release the regular selection info */
    if(space->select.sel_info.hslab.diminfo!=NULL) {
        H5FL_ARR_FREE(H5S_hyper_dim_t,space->select.sel_info.hslab.diminfo);
        space->select.sel_info.hslab.diminfo = NULL;
        H5FL_ARR_FREE(H5S_hyper_dim_t,space->select.sel_info.hslab.app_diminfo);
        space->select.sel_info.hslab.app_diminfo = NULL;
    } /* end if */

    /* Release irregular hyperslab information */
    if(space->select.sel_info.hslab.span_lst!=NULL) {
        if(H5S_hyper_free_span_info(space->select.sel_info.hslab.span_lst)<0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "failed to release hyperslab spans");
        space->select.sel_info.hslab.span_lst=NULL;
    } /* end if */

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_release() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_recover_span
 PURPOSE
    Recover a generated span, if appropriate
 USAGE
    herr_t H5S_hyper_recover_span(recover, curr_span, next_span)
        unsigned *recover;                 IN/OUT: Pointer recover flag
        H5S_hyper_span_t **curr_span;   IN/OUT: Pointer to current span in list
        H5S_hyper_span_t *next_span;    IN: Pointer to next span
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Check if the current span needs to be recovered and free it if so.
    Set the current span to the next span in any case.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_recover_span (unsigned *recover, H5S_hyper_span_t **curr_span, H5S_hyper_span_t *next_span)
{
    herr_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_recover_span, FAIL);

    assert(recover);
    assert(curr_span);

    /* Check if the span should be recovered */
    if(*recover) {
        H5S_hyper_free_span(*curr_span);
        *recover=0;
    } /* end if */

    /* Set the current span to next span */
    *curr_span=next_span;

    /* Success! */
    ret_value=SUCCEED;

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_recover_span() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_append_span
 PURPOSE
    Create a new span and append to span list
 USAGE
    herr_t H5S_hyper_append_span(prev_span, span_tree, low, high, down, next)
        H5S_hyper_span_t **prev_span;    IN/OUT: Pointer to previous span in list
        H5S_hyper_span_info_t **span_tree;  IN/OUT: Pointer to span tree to append to
        hssize_t low, high;         IN: Low and high bounds for new span node
        H5S_hyper_span_info_t *down;     IN: Down span tree for new node
        H5S_hyper_span_t *next;     IN: Next span for new node
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Create a new span node and append to a span list.  Update the previous
    span in the list also.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_append_span (H5S_hyper_span_t **prev_span, H5S_hyper_span_info_t ** span_tree, hssize_t low, hssize_t high, H5S_hyper_span_info_t *down, H5S_hyper_span_t *next)
{
    H5S_hyper_span_t *new_span;
    herr_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_append_span, FAIL);

    assert(prev_span);
    assert(span_tree);

    /* Check for adding first node to merged spans */
    if(*prev_span==NULL) {
        /* Allocate new span node to append to list */
        if((new_span = H5S_hyper_new_span(low,high,down,next))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");
           
        /* Make first node in span list */

        /* Check that we haven't already allocated a span tree */
        assert(*span_tree==NULL);

        /* Allocate a new span_info node */
        if((*span_tree = H5FL_ALLOC(H5S_hyper_span_info_t,0))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");
           
        /* Set the span tree's basic information */
        (*span_tree)->count=1;
        (*span_tree)->scratch=NULL;
        (*span_tree)->head=new_span;

        /* Update previous merged span */
        *prev_span=new_span;
    } /* end if */
    /* Merge or append to existing merged spans list */
    else {
        /* Check if span can just extend the previous merged span */
        if((((*prev_span)->high+1)==low) && 
                H5S_hyper_cmp_spans(down,(*prev_span)->down)==TRUE) {
            /* Extend previous merged span to include new high bound */
            (*prev_span)->high=high;
            (*prev_span)->nelem+=(high-low)+1;
        } /* end if */
        else {
            /* Allocate new span node to append to list */
            if((new_span = H5S_hyper_new_span(low,high,down,next))==NULL)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");
           
            /* Check if there is actually a down span */
            if(new_span->down) {
                /* Check if the down spans for the new span node are the same as the previous span node */
                if(H5S_hyper_cmp_spans(new_span->down,(*prev_span)->down)==TRUE) {
                    /* Release the down span for the new node */
                    H5S_hyper_free_span_info(new_span->down);

                    /* Point the new node's down span at the previous node's down span */
                    new_span->down=(*prev_span)->down;

                    /* Increment the reference count to the shared down span */
                    new_span->down->count++;
                } /* end if */
            } /* end if */

            /* Indicate elements to previous span */
            new_span->pstride=low-(*prev_span)->low;

            /* Append to end of merged spans list */
            (*prev_span)->next=new_span;
            *prev_span=new_span;
        } /* end else */
    } /* end else */

    /* Success! */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_append_span() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_clip_spans
 PURPOSE
    Clip a new span tree against the current spans in the hyperslab selection
 USAGE
    herr_t H5S_hyper_clip_spans(span_a, span_b, a_not_b, a_and_b, b_not_a)
        H5S_hyper_span_t *a_spans;    IN: Span tree 'a' to clip with.
        H5S_hyper_span_t *b_spans;    IN: Span tree 'b' to clip with.
        H5S_hyper_span_t **a_not_b;  OUT: Span tree of 'a' hyperslab spans which
                                            doesn't overlap with 'b' hyperslab
                                            spans.
        H5S_hyper_span_t **a_and_b;  OUT: Span tree of 'a' hyperslab spans which
                                            overlaps with 'b' hyperslab spans.
        H5S_hyper_span_t **b_not_a;  OUT: Span tree of 'b' hyperslab spans which
                                            doesn't overlap with 'a' hyperslab
                                            spans.
 RETURNS
    non-negative on success, negative on failure
 DESCRIPTION
    Clip one span tree ('a') against another span tree ('b').  Creates span
    trees for the area defined by the 'a' span tree which does not overlap the
    'b' span tree, the area defined by the overlap of the 'a' hyperslab span
    tree and the 'b' span tree, and the area defined by the 'b' hyperslab span
    tree which does not overlap the 'a' span tree.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_clip_spans (H5S_hyper_span_info_t *a_spans, H5S_hyper_span_info_t *b_spans,
    H5S_hyper_span_info_t **a_not_b, H5S_hyper_span_info_t **a_and_b,
    H5S_hyper_span_info_t **b_not_a)
{
    H5S_hyper_span_t *span_a;   /* Pointer to a node in span tree 'a' */
    H5S_hyper_span_t *span_b;   /* Pointer to a node in span tree 'b' */
    H5S_hyper_span_t *tmp_span; /* Temporary pointer to new span */
    H5S_hyper_span_t *last_a_not_b;   /* Pointer to previous node in span tree 'a_not_b' */
    H5S_hyper_span_t *last_a_and_b;   /* Pointer to previous node in span tree 'a_and_b' */
    H5S_hyper_span_t *last_b_not_a;   /* Pointer to previous node in span tree 'b_not_a' */
    H5S_hyper_span_info_t *down_a_not_b; /* Temporary pointer to a_not_b span tree of down spans for overlapping nodes */
    H5S_hyper_span_info_t *down_a_and_b; /* Temporary pointer to a_and_b span tree of down spans for overlapping nodes */
    H5S_hyper_span_info_t *down_b_not_a; /* Temporary pointer to b_and_a span tree of down spans for overlapping nodes */
    unsigned recover_a, recover_b;         /* Flags to indicate when to recover temporary spans */
    herr_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_clip_spans, FAIL);

    /* Check args */
    assert (a_spans);
    assert (b_spans);
    assert (a_not_b);
    assert (a_and_b);
    assert (b_not_a);

#ifdef QAK
printf("%s: a_spans=%p, b_spans=%p\n",FUNC,a_spans,b_spans);
#endif /* QAK */
    /* Check if both span trees are not defined */
    if(a_spans==NULL && b_spans==NULL) {
#ifdef QAK
printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
        *a_not_b=NULL;
        *a_and_b=NULL;
        *b_not_a=NULL;
    } /* end if */
    /* If span 'a' is not defined, but 'b' is, copy 'b' and set the other return span trees to empty */
    else if(a_spans==NULL) {
#ifdef QAK
printf("%s: check 2.0\n",FUNC);
#endif /* QAK */
        *a_not_b=NULL;
        *a_and_b=NULL;
        if((*b_not_a=H5S_hyper_copy_span(b_spans))==NULL)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, FAIL, "can't copy hyperslab span tree");
    } /* end if */
    /* If span 'b' is not defined, but 'a' is, copy 'a' and set the other return span trees to empty */
    else if(b_spans==NULL) {
#ifdef QAK
printf("%s: check 3.0\n",FUNC);
#endif /* QAK */
        if((*a_not_b=H5S_hyper_copy_span(a_spans))==NULL)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, FAIL, "can't copy hyperslab span tree");
        *a_and_b=NULL;
        *b_not_a=NULL;
    } /* end if */
    /* If span 'a' and 'b' are both defined, calculate the proper span trees */
    else {
#ifdef QAK
printf("%s: check 4.0\n",FUNC);
#endif /* QAK */
        /* Check if both span trees completely overlap */
        if(H5S_hyper_cmp_spans(a_spans,b_spans)==TRUE) {
#ifdef QAK
printf("%s: check 4.1\n",FUNC);
#endif /* QAK */
            *a_not_b=NULL;
            if((*a_and_b=H5S_hyper_copy_span(a_spans))==NULL)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, FAIL, "can't copy hyperslab span tree");
            *b_not_a=NULL;
        } /* end if */
        else {
#ifdef QAK
printf("%s: check 4.2\n",FUNC);
#endif /* QAK */
            /* Get the pointers to the new and old span lists */
            span_a=a_spans->head;
            span_b=b_spans->head;

            /* Set the pointer to the previous spans */
            last_a_not_b=NULL;
            last_a_and_b=NULL;
            last_b_not_a=NULL;

            /* No spans to recover yet */
            recover_a=recover_b=0;

            /* Work through the list of spans in the new list */
            while(span_a!=NULL && span_b!=NULL) {
#ifdef QAK
printf("%s: check 4.3, span_a=%p, span_b=%p\n",FUNC,span_a,span_b);
#endif /* QAK */
                /* Check if span 'a' is completely before span 'b' */
                /*    AAAAAAA                            */
                /* <-----------------------------------> */
                /*             BBBBBBBBBB                */
                if(span_a->high<span_b->low) {
#ifdef QAK
printf("%s: check 4.3.1, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                    /* Copy span 'a' and add to a_not_b list */

                    /* Merge/add span 'a' with/to a_not_b list */
                    if(H5S_hyper_append_span(&last_a_not_b,a_not_b,span_a->low,span_a->high,span_a->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                    /* Advance span 'a', leave span 'b' */
                    H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);
                } /* end if */
                /* Check if span 'a' overlaps only the lower bound */
                /*  of span 'b' , up to the upper bound of span 'b' */
                /*    AAAAAAAAAAAA                       */
                /* <-----------------------------------> */
                /*             BBBBBBBBBB                */
                else if(span_a->low<span_b->low && (span_a->high>=span_b->low && span_a->high<=span_b->high)) {
#ifdef QAK
printf("%s: check 4.3.2, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                    /* Split span 'a' into two parts at the low bound of span 'b' */

                    /* Merge/add lower part of span 'a' with/to a_not_b list */
                    if(H5S_hyper_append_span(&last_a_not_b,a_not_b,span_a->low,span_b->low-1,span_a->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                    /* Check for overlaps between upper part of span 'a' and lower part of span 'b' */

                    /* Make certain both spans either have a down span or both don't have one */
                    assert((span_a->down!=NULL && span_b->down!=NULL) || (span_a->down==NULL && span_b->down==NULL));

                    /* If there are no down spans, just add the overlapping area to the a_and_b list */
                    if(span_a->down==NULL) {
                        /* Merge/add overlapped part with/to a_and_b list */
                        if(H5S_hyper_append_span(&last_a_and_b,a_and_b,span_b->low,span_a->high,NULL,NULL)==FAIL)
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");
                    } /* end if */
                    /* If there are down spans, check for the overlap in them and add to each appropriate list */
                    else {
                        /* NULL out the temporary pointers to clipped areas in down spans */
                        down_a_not_b=NULL;
                        down_a_and_b=NULL;
                        down_b_not_a=NULL;

                        /* Check for overlaps in the 'down spans' of span 'a' & 'b' */
                        if(H5S_hyper_clip_spans(span_a->down,span_b->down,&down_a_not_b,&down_a_and_b,&down_b_not_a)<0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLIP, FAIL, "can't clip hyperslab information");
                        
                        /* Check for additions to the a_not_b list */
                        if(down_a_not_b!=NULL) {
                            /* Merge/add overlapped part with/to a_not_b list */
                            if(H5S_hyper_append_span(&last_a_not_b,a_not_b,span_b->low,span_a->high,down_a_not_b,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_a_not_b);
                        } /* end if */

                        /* Check for additions to the a_and_b list */
                        if(down_a_and_b!=NULL) {
                            /* Merge/add overlapped part with/to a_and_b list */
                            if(H5S_hyper_append_span(&last_a_and_b,a_and_b,span_b->low,span_a->high,down_a_and_b,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_a_and_b);
                        } /* end if */

                        /* Check for additions to the b_not_a list */
                        if(down_b_not_a!=NULL) {
                            /* Merge/add overlapped part with/to b_not_a list */
                            if(H5S_hyper_append_span(&last_b_not_a,b_not_a,span_b->low,span_a->high,down_b_not_a,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_b_not_a);
                        } /* end if */
                    } /* end else */

                    /* Split off upper part of span 'b' at upper span of span 'a' */

                    /* Check if there is actually an upper part of span 'b' to split off */
                    if(span_a->high<span_b->high) {
                        /* Allocate new span node for upper part of span 'b' */
                        if((tmp_span = H5S_hyper_new_span(span_a->high+1,span_b->high,span_b->down,span_b->next))==NULL)
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                        /* Advance span 'a' */
                        H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);

                        /* Make upper part of span 'b' into new span 'b' */
                        H5S_hyper_recover_span(&recover_b,&span_b,tmp_span);
                        recover_b=1;
                    } /* end if */
                    /* No upper part of span 'b' to split */
                    else {
                        /* Advance both 'a' and 'b' */
                        H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);
                        H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
                    } /* end else */
                } /* end if */
                /* Check if span 'a' overlaps the lower & upper bound */
                /*  of span 'b' */
                /*    AAAAAAAAAAAAAAAAAAAAA              */
                /* <-----------------------------------> */
                /*             BBBBBBBBBB                */
                else if(span_a->low<span_b->low && span_a->high>span_b->high) {
#ifdef QAK
printf("%s: check 4.3.3, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                    /* Split off lower part of span 'a' at lower span of span 'b' */

                    /* Merge/add lower part of span 'a' with/to a_not_b list */
                    if(H5S_hyper_append_span(&last_a_not_b,a_not_b,span_a->low,span_b->low-1,span_a->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                    /* Check for overlaps between middle part of span 'a' and span 'b' */

                    /* Make certain both spans either have a down span or both don't have one */
                    assert((span_a->down!=NULL && span_b->down!=NULL) || (span_a->down==NULL && span_b->down==NULL));

                    /* If there are no down spans, just add the overlapping area to the a_and_b list */
                    if(span_a->down==NULL) {
                        /* Merge/add overlapped part with/to a_and_b list */
                        if(H5S_hyper_append_span(&last_a_and_b,a_and_b,span_b->low,span_b->high,NULL,NULL)==FAIL)
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");
                    } /* end if */
                    /* If there are down spans, check for the overlap in them and add to each appropriate list */
                    else {
                        /* NULL out the temporary pointers to clipped areas in down spans */
                        down_a_not_b=NULL;
                        down_a_and_b=NULL;
                        down_b_not_a=NULL;

                        /* Check for overlaps in the 'down spans' of span 'a' & 'b' */
                        if(H5S_hyper_clip_spans(span_a->down,span_b->down,&down_a_not_b,&down_a_and_b,&down_b_not_a)<0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLIP, FAIL, "can't clip hyperslab information");
                        
                        /* Check for additions to the a_not_b list */
                        if(down_a_not_b!=NULL) {
                            /* Merge/add overlapped part with/to a_not_b list */
                            if(H5S_hyper_append_span(&last_a_not_b,a_not_b,span_b->low,span_b->high,down_a_not_b,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_a_not_b);
                        } /* end if */

                        /* Check for additions to the a_and_b list */
                        if(down_a_and_b!=NULL) {
                            /* Merge/add overlapped part with/to a_and_b list */
                            if(H5S_hyper_append_span(&last_a_and_b,a_and_b,span_b->low,span_b->high,down_a_and_b,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_a_and_b);
                        } /* end if */

                        /* Check for additions to the b_not_a list */
                        if(down_b_not_a!=NULL) {
                            /* Merge/add overlapped part with/to b_not_a list */
                            if(H5S_hyper_append_span(&last_b_not_a,b_not_a,span_b->low,span_b->high,down_b_not_a,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_b_not_a);
                        } /* end if */
                    } /* end else */

                    /* Split off upper part of span 'a' at upper span of span 'b' */

                    /* Allocate new span node for upper part of span 'a' */
                    if((tmp_span = H5S_hyper_new_span(span_b->high+1,span_a->high,span_a->down,span_a->next))==NULL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                    /* Make upper part of span 'a' the new span 'a' */
                    H5S_hyper_recover_span(&recover_a,&span_a,tmp_span);
                    recover_a=1;

                    /* Advance span 'b' */
                    H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
                } /* end if */
                /* Check if span 'a' is entirely within span 'b' */
                /*                AAAAA                  */
                /* <-----------------------------------> */
                /*             BBBBBBBBBB                */
                else if(span_a->low>=span_b->low && span_a->high<=span_b->high) {
#ifdef QAK
printf("%s: check 4.3.4, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                    /* Split off lower part of span 'b' at lower span of span 'a' */

                    /* Check if there is actually a lower part of span 'b' to split off */
                    if(span_a->low>span_b->low) {
                        /* Merge/add lower part of span 'b' with/to b_not_a list */
                        if(H5S_hyper_append_span(&last_b_not_a,b_not_a,span_b->low,span_a->low-1,span_b->down,NULL)==FAIL)
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");
                    } /* end if */
                    else {
                        /* Keep going, nothing to split off */
                    } /* end else */

                    /* Check for overlaps between span 'a' and midle of span 'b' */

                    /* Make certain both spans either have a down span or both don't have one */
                    assert((span_a->down!=NULL && span_b->down!=NULL) || (span_a->down==NULL && span_b->down==NULL));

                    /* If there are no down spans, just add the overlapping area to the a_and_b list */
                    if(span_a->down==NULL) {
                        /* Merge/add overlapped part with/to a_and_b list */
                        if(H5S_hyper_append_span(&last_a_and_b,a_and_b,span_a->low,span_a->high,NULL,NULL)==FAIL)
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");
                    } /* end if */
                    /* If there are down spans, check for the overlap in them and add to each appropriate list */
                    else {
                        /* NULL out the temporary pointers to clipped areas in down spans */
                        down_a_not_b=NULL;
                        down_a_and_b=NULL;
                        down_b_not_a=NULL;

                        /* Check for overlaps in the 'down spans' of span 'a' & 'b' */
                        if(H5S_hyper_clip_spans(span_a->down,span_b->down,&down_a_not_b,&down_a_and_b,&down_b_not_a)<0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLIP, FAIL, "can't clip hyperslab information");
                        
                        /* Check for additions to the a_not_b list */
                        if(down_a_not_b!=NULL) {
                            /* Merge/add overlapped part with/to a_not_b list */
                            if(H5S_hyper_append_span(&last_a_not_b,a_not_b,span_a->low,span_a->high,down_a_not_b,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_a_not_b);
                        } /* end if */

                        /* Check for additions to the a_and_b list */
                        if(down_a_and_b!=NULL) {
                            /* Merge/add overlapped part with/to a_and_b list */
                            if(H5S_hyper_append_span(&last_a_and_b,a_and_b,span_a->low,span_a->high,down_a_and_b,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_a_and_b);
                        } /* end if */

                        /* Check for additions to the b_not_a list */
                        if(down_b_not_a!=NULL) {
                            /* Merge/add overlapped part with/to b_not_a list */
                            if(H5S_hyper_append_span(&last_b_not_a,b_not_a,span_a->low,span_a->high,down_b_not_a,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_b_not_a);
                        } /* end if */
                    } /* end else */

                    /* Check if there is actually an upper part of span 'b' to split off */
                    if(span_a->high<span_b->high) {
                        /* Split off upper part of span 'b' at upper span of span 'a' */

                        /* Allocate new span node for upper part of spans 'a' */
                        if((tmp_span = H5S_hyper_new_span(span_a->high+1,span_b->high,span_b->down,span_b->next))==NULL)
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                        /* And advance span 'a' */
                        H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);

                        /* Make upper part of span 'b' the new span 'b' */
                        H5S_hyper_recover_span(&recover_b,&span_b,tmp_span);
                        recover_b=1;
                    } /* end if */
                    else {
                        /* Advance both span 'a' & span 'b' */
                        H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);
                        H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
                    } /* end else */
                } /* end if */
                /* Check if span 'a' overlaps only the upper bound */
                /*  of span 'b' */
                /*                AAAAAAAAAA             */
                /* <-----------------------------------> */
                /*             BBBBBBBBBB                */
                else if((span_a->low>=span_b->low && span_a->low<=span_b->high) && span_a->high>span_b->high) {
#ifdef QAK
printf("%s: check 4.3.5, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                    /* Check if there is actually a lower part of span 'b' to split off */
                    if(span_a->low>span_b->low) {
                        /* Split off lower part of span 'b' at lower span of span 'a' */

                        /* Merge/add lower part of span 'b' with/to b_not_a list */
                        if(H5S_hyper_append_span(&last_b_not_a,b_not_a,span_b->low,span_a->low-1,span_b->down,NULL)==FAIL)
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");
                    } /* end if */
                    else {
                        /* Keep going, nothing to split off */
                    } /* end else */

                    /* Check for overlaps between lower part of span 'a' and upper part of span 'b' */

                    /* Make certain both spans either have a down span or both don't have one */
                    assert((span_a->down!=NULL && span_b->down!=NULL) || (span_a->down==NULL && span_b->down==NULL));

                    /* If there are no down spans, just add the overlapping area to the a_and_b list */
                    if(span_a->down==NULL) {
                        /* Merge/add overlapped part with/to a_and_b list */
                        if(H5S_hyper_append_span(&last_a_and_b,a_and_b,span_a->low,span_b->high,NULL,NULL)==FAIL)
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");
                    } /* end if */
                    /* If there are down spans, check for the overlap in them and add to each appropriate list */
                    else {
                        /* NULL out the temporary pointers to clipped areas in down spans */
                        down_a_not_b=NULL;
                        down_a_and_b=NULL;
                        down_b_not_a=NULL;

                        /* Check for overlaps in the 'down spans' of span 'a' & 'b' */
                        if(H5S_hyper_clip_spans(span_a->down,span_b->down,&down_a_not_b,&down_a_and_b,&down_b_not_a)<0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLIP, FAIL, "can't clip hyperslab information");
                        
                        /* Check for additions to the a_not_b list */
                        if(down_a_not_b!=NULL) {
                            /* Merge/add overlapped part with/to a_not_b list */
                            if(H5S_hyper_append_span(&last_a_not_b,a_not_b,span_a->low,span_b->high,down_a_not_b,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_a_not_b);
                        } /* end if */

                        /* Check for additions to the a_and_b list */
                        if(down_a_and_b!=NULL) {
                            /* Merge/add overlapped part with/to a_and_b list */
                            if(H5S_hyper_append_span(&last_a_and_b,a_and_b,span_a->low,span_b->high,down_a_and_b,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_a_and_b);
                        } /* end if */

                        /* Check for additions to the b_not_a list */
                        if(down_b_not_a!=NULL) {
                            /* Merge/add overlapped part with/to b_not_a list */
                            if(H5S_hyper_append_span(&last_b_not_a,b_not_a,span_a->low,span_b->high,down_b_not_a,NULL)==FAIL)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                            /* Release the down span tree generated */
                            H5S_hyper_free_span_info(down_b_not_a);
                        } /* end if */
                    } /* end else */

                    /* Split off upper part of span 'a' at upper span of span 'b' */

                    /* Allocate new span node for upper part of span 'a' */
                    if((tmp_span = H5S_hyper_new_span(span_b->high+1,span_a->high,span_a->down,span_a->next))==NULL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                    /* Make upper part of span 'a' into new span 'a' */
                    H5S_hyper_recover_span(&recover_a,&span_a,tmp_span);
                    recover_a=1;

                    /* Advance span 'b' */
                    H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
                } /* end if */
                /* span 'a' must be entirely above span 'b' */
                /*                         AAAAA         */
                /* <-----------------------------------> */
                /*             BBBBBBBBBB                */
                else {
#ifdef QAK
printf("%s: check 4.3.6, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                    /* Copy span 'b' and add to b_not_a list */

                    /* Merge/add span 'b' with/to b_not_a list */
                    if(H5S_hyper_append_span(&last_b_not_a,b_not_a,span_b->low,span_b->high,span_b->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                    /* Advance span 'b', leave span 'a' */
                    H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
                } /* end else */
            } /* end while */
#ifdef QAK
printf("%s: check 5.0, span_a=%p, span_b=%p\n",FUNC,span_a,span_b);
#endif /* QAK */

            /* Clean up 'a' spans which haven't been covered yet */
            if(span_a!=NULL && span_b==NULL) {
#ifdef QAK
printf("%s: check 6.0, span_a=%p, span_b=%p\n",FUNC,span_a,span_b);
#endif /* QAK */
                while(span_a!=NULL) {
                    /* Copy span 'a' and add to a_not_b list */

                    /* Merge/add span 'a' with/to a_not_b list */
                    if(H5S_hyper_append_span(&last_a_not_b,a_not_b,span_a->low,span_a->high,span_a->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                    /* Advance to the next 'a' span */
                    H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);
                } /* end while */
            } /* end if */
            /* Clean up 'b' spans which haven't been covered yet */
            else if(span_a==NULL && span_b!=NULL) {
#ifdef QAK
printf("%s: check 7.0, span_a=%p, span_b=%p\n",FUNC,span_a,span_b);
#endif /* QAK */
                while(span_b!=NULL) {
                    /* Copy span 'b' and add to b_not_a list */

                    /* Merge/add span 'b' with/to b_not_a list */
                    if(H5S_hyper_append_span(&last_b_not_a,b_not_a,span_b->low,span_b->high,span_b->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab span");

                    /* Advance to the next 'b' span */
                    H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
                } /* end while */
            } /* end if */
        } /* end else */
    } /* end else */

    /* Success! */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_clip_spans() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_merge_spans_helper
 PURPOSE
    Merge two hyperslab span tree together
 USAGE
    H5S_hyper_span_info_t *H5S_hyper_merge_spans_helper(a_spans, b_spans)
        H5S_hyper_span_info_t *a_spans; IN: First hyperslab spans to merge
                                                together
        H5S_hyper_span_info_t *b_spans; IN: Second hyperslab spans to merge
                                                together
 RETURNS
    Pointer to span tree containing the merged spans on success, NULL on failure
 DESCRIPTION
    Merge two sets of hyperslab spans together and return the span tree from
    the merged set.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5S_hyper_span_info_t *
H5S_hyper_merge_spans_helper (H5S_hyper_span_info_t *a_spans, H5S_hyper_span_info_t *b_spans)
{
    H5S_hyper_span_info_t *merged_spans=NULL; /* Pointer to the merged span tree */
    H5S_hyper_span_info_t *tmp_spans;   /* Pointer to temporary new span tree */
    H5S_hyper_span_t *tmp_span;         /* Pointer to temporary new span */
    H5S_hyper_span_t *span_a;           /* Pointer to current span 'a' working on */
    H5S_hyper_span_t *span_b;           /* Pointer to current span 'b' working on */
    H5S_hyper_span_t *prev_span_merge;  /* Pointer to previous merged span */
    unsigned recover_a, recover_b;         /* Flags to indicate when to recover temporary spans */
    H5S_hyper_span_info_t *ret_value=NULL;

    FUNC_ENTER (H5S_hyper_merge_spans_helper, NULL);

#ifdef QAK
printf("%s: a_spans=%p, b_spans=%p\n",FUNC,a_spans,b_spans);
#endif /* QAK */

    /* Make certain both 'a' & 'b' spans have down span trees or neither does */
    assert((a_spans!=NULL && b_spans!=NULL) || (a_spans==NULL && b_spans==NULL));

    /* Check if the span trees for the 'a' span and the 'b' span are the same */
    if(H5S_hyper_cmp_spans(a_spans,b_spans)==TRUE) {
#ifdef QAK
printf("%s: check 0.5\n",FUNC);
#endif /* QAK */
        if(a_spans==NULL)
            merged_spans=NULL;
        else {
            /* Copy one of the span trees to return */
            if((merged_spans=H5S_hyper_copy_span(a_spans))==NULL)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "can't copy hyperslab span tree");
        } /* end else */
    } /* end if */
    else {
#ifdef QAK
printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
        /* Get the pointers to the 'a' and 'b' span lists */
        span_a=a_spans->head;
        span_b=b_spans->head;

        /* Set the pointer to the previous spans */
        prev_span_merge=NULL;

        /* No spans to recover yet */
        recover_a=recover_b=0;

        /* Work through the list of spans in the new list */
        while(span_a!=NULL && span_b!=NULL) {
#ifdef QAK
printf("%s: check 3.0, span_a=%p, span_b=%p\n",FUNC,span_a,span_b);
#endif /* QAK */
            /* Check if the 'a' span is completely before 'b' span */
            /*    AAAAAAA                            */
            /* <-----------------------------------> */
            /*             BBBBBBBBBB                */
            if(span_a->high<span_b->low) {
#ifdef QAK
printf("%s: check 3.1, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                /* Merge/add span 'a' with/to the merged spans */
                if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_a->low,span_a->high,span_a->down,NULL)==FAIL)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                /* Advance span 'a' */
                H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);
            } /* end if */
            /* Check if span 'a' overlaps only the lower bound */
            /*  of span 'b', up to the upper bound of span 'b' */
            /*    AAAAAAAAAAAA                       */
            /* <-----------------------------------> */
            /*             BBBBBBBBBB                */
            else if(span_a->low<span_b->low && (span_a->high>=span_b->low && span_a->high<=span_b->high)) {
#ifdef QAK
printf("%s: check 3.2, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                /* Check if span 'a' and span 'b' down spans are equal */
                if(H5S_hyper_cmp_spans(span_a->down,span_b->down)==TRUE) {
                    /* Merge/add copy of span 'a' with/to merged spans */
                    if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_a->low,span_a->high,span_a->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");
                } /* end if */
                else {
                    /* Merge/add lower part of span 'a' with/to merged spans */
                    if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_a->low,span_b->low-1,span_a->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                    /* Get merged span tree for overlapped section */
                    tmp_spans=H5S_hyper_merge_spans_helper(span_a->down,span_b->down);

                    /* Merge/add overlapped section to merged spans */
                    if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_b->low,span_a->high,tmp_spans,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                    /* Release merged span tree for overlapped section */
                    H5S_hyper_free_span_info(tmp_spans);
                } /* end else */

                /* Check if there is an upper part of span 'b' */
                if(span_a->high<span_b->high) {
                    /* Copy upper part of span 'b' as new span 'b' */

                    /* Allocate new span node to append to list */
                    if((tmp_span = H5S_hyper_new_span(span_a->high+1,span_b->high,span_b->down,span_b->next))==NULL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                    /* Advance span 'a' */
                    H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);

                    /* Set new span 'b' to tmp_span */
                    H5S_hyper_recover_span(&recover_b,&span_b,tmp_span);
                    recover_b=1;
                } /* end if */
                else {
                    /* Advance both span 'a' & 'b' */
                    H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);
                    H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
                } /* end else */
            } /* end if */
            /* Check if span 'a' overlaps the lower & upper bound */
            /*  of span 'b' */
            /*    AAAAAAAAAAAAAAAAAAAAA              */
            /* <-----------------------------------> */
            /*             BBBBBBBBBB                */
            else if(span_a->low<span_b->low && span_a->high>span_b->high) {
#ifdef QAK
printf("%s: check 3.3, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                /* Check if span 'a' and span 'b' down spans are equal */
                if(H5S_hyper_cmp_spans(span_a->down,span_b->down)==TRUE) {
                    /* Merge/add copy of lower & middle parts of span 'a' to merged spans */
                    if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_a->low,span_b->high,span_a->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");
                } /* end if */
                else {
                    /* Merge/add lower part of span 'a' to merged spans */
                    if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_a->low,span_b->low-1,span_a->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                    /* Get merged span tree for overlapped section */
                    tmp_spans=H5S_hyper_merge_spans_helper(span_a->down,span_b->down);

                    /* Merge/add overlapped section to merged spans */
                    if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_b->low,span_b->high,tmp_spans,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                    /* Release merged span tree for overlapped section */
                    H5S_hyper_free_span_info(tmp_spans);
                } /* end else */

                /* Copy upper part of span 'a' as new span 'a' (remember to free) */

                /* Allocate new span node to append to list */
                if((tmp_span = H5S_hyper_new_span(span_b->high+1,span_a->high,span_a->down,span_a->next))==NULL)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                /* Set new span 'a' to tmp_span */
                H5S_hyper_recover_span(&recover_a,&span_a,tmp_span);
                recover_a=1;

                /* Advance span 'b' */
                H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
            } /* end if */
            /* Check if span 'a' is entirely within span 'b' */
            /*                AAAAA                  */
            /* <-----------------------------------> */
            /*             BBBBBBBBBB                */
            else if(span_a->low>=span_b->low && span_a->high<=span_b->high) {
#ifdef QAK
printf("%s: check 3.4, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                /* Check if span 'a' and span 'b' down spans are equal */
                if(H5S_hyper_cmp_spans(span_a->down,span_b->down)==TRUE) {
                    /* Merge/add copy of lower & middle parts of span 'b' to merged spans */
                    if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_b->low,span_a->high,span_a->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");
                } /* end if */
                else {
                    /* Check if there is a lower part of span 'b' */
                    if(span_a->low>span_b->low) {
                        /* Merge/add lower part of span 'b' to merged spans */
                        if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_b->low,span_a->low-1,span_b->down,NULL)==FAIL)
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");
                    } /* end if */
                    else {
                        /* No lower part of span 'b' , keep going... */
                    } /* end else */

                    /* Get merged span tree for overlapped section */
                    tmp_spans=H5S_hyper_merge_spans_helper(span_a->down,span_b->down);

                    /* Merge/add overlapped section to merged spans */
                    if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_a->low,span_a->high,tmp_spans,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                    /* Release merged span tree for overlapped section */
                    H5S_hyper_free_span_info(tmp_spans);
                } /* end else */

                /* Check if there is an upper part of span 'b' */
                if(span_a->high<span_b->high) {
                    /* Copy upper part of span 'b' as new span 'b' (remember to free) */

                    /* Allocate new span node to append to list */
                    if((tmp_span = H5S_hyper_new_span(span_a->high+1,span_b->high,span_b->down,span_b->next))==NULL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                    /* Advance span 'a' */
                    H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);

                    /* Set new span 'b' to tmp_span */
                    H5S_hyper_recover_span(&recover_b,&span_b,tmp_span);
                    recover_b=1;
                } /* end if */
                else {
                    /* Advance both spans */
                    H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);
                    H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
                } /* end else */
            } /* end if */
            /* Check if span 'a' overlaps only the upper bound */
            /*  of span 'b' */
            /*                AAAAAAAAAA             */
            /* <-----------------------------------> */
            /*             BBBBBBBBBB                */
            else if((span_a->low>=span_b->low && span_a->low<=span_b->high) && span_a->high>span_b->high) {
#ifdef QAK
printf("%s: check 3.5, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                /* Check if span 'a' and span 'b' down spans are equal */
                if(H5S_hyper_cmp_spans(span_a->down,span_b->down)==TRUE) {
                    /* Merge/add copy of span 'b' to merged spans if so */
                    if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_b->low,span_b->high,span_b->down,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");
                } /* end if */
                else {
                    /* Check if there is a lower part of span 'b' */
                    if(span_a->low>span_b->low) {
                        /* Merge/add lower part of span 'b' to merged spans */
                        if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_b->low,span_a->low-1,span_b->down,NULL)==FAIL)
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");
                    } /* end if */
                    else {
                        /* No lower part of span 'b' , keep going... */
                    } /* end else */

                    /* Get merged span tree for overlapped section */
                    tmp_spans=H5S_hyper_merge_spans_helper(span_a->down,span_b->down);

                    /* Merge/add overlapped section to merged spans */
                    if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_a->low,span_b->high,tmp_spans,NULL)==FAIL)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                    /* Release merged span tree for overlapped section */
                    H5S_hyper_free_span_info(tmp_spans);
                } /* end else */

                /* Copy upper part of span 'a' as new span 'a' */

                /* Allocate new span node to append to list */
                if((tmp_span = H5S_hyper_new_span(span_b->high+1,span_a->high,span_a->down,span_a->next))==NULL)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                /* Set new span 'a' to tmp_span */
                H5S_hyper_recover_span(&recover_a,&span_a,tmp_span);
                recover_a=1;

                /* Advance span 'b' */
                H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
            } /* end if */
            /* Span 'a' must be entirely above span 'b' */
            /*                         AAAAA         */
            /* <-----------------------------------> */
            /*             BBBBBBBBBB                */
            else {
#ifdef QAK
printf("%s: check 3.6, span_a->(low, high)=(%ld, %ld), span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                /* Merge/add span 'b' with the merged spans */
                if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_b->low,span_b->high,span_b->down,NULL)==FAIL)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                /* Advance span 'b' */
                H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
            } /* end else */
        } /* end while */
#ifdef QAK
printf("%s: check 4.0, span_a=%p, span_b=%p\n",FUNC,span_a,span_b);
#endif /* QAK */

        /* Clean up 'a' spans which haven't been added to the list of merged spans */
        if(span_a!=NULL && span_b==NULL) {
            while(span_a!=NULL) {
#ifdef QAK
printf("%s: check 5.0, span_a->(low, high)=(%ld, %ld)\n",FUNC,(long)span_a->low,(long)span_a->high);
#endif /* QAK */
                /* Merge/add all 'a' spans into the merged spans */
                if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_a->low,span_a->high,span_a->down,NULL)==FAIL)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                /* Advance to next 'a' span, until all processed */
                H5S_hyper_recover_span(&recover_a,&span_a,span_a->next);
            } /* end while */
        } /* end if */
        
        /* Clean up 'b' spans which haven't been added to the list of merged spans */
        if(span_a==NULL && span_b!=NULL) {
            while(span_b!=NULL) {
#ifdef QAK
printf("%s: check 6.0, span_b->(low, high)=(%ld, %ld)\n",FUNC,(long)span_b->low,(long)span_b->high);
#endif /* QAK */
                /* Merge/add all 'b' spans into the merged spans */
                if(H5S_hyper_append_span(&prev_span_merge,&merged_spans,span_b->low,span_b->high,span_b->down,NULL)==FAIL)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

                /* Advance to next 'b' span, until all processed */
                H5S_hyper_recover_span(&recover_b,&span_b,span_b->next);
            } /* end while */
        } /* end if */
#ifdef QAK
printf("%s: check 7.0, span_a=%p, span_b=%p\n",FUNC,span_a,span_b);
#endif /* QAK */
    } /* end else */

    /* Success!  */
    ret_value=merged_spans;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_merge_spans_helper() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_merge_spans
 PURPOSE
    Merge new hyperslab spans to existing hyperslab selection
 USAGE
    herr_t H5S_hyper_merge_spans(space, new_spans)
        H5S_t *space;             IN: Dataspace to add new spans to hyperslab
                                        selection.
        H5S_hyper_span_t *new_spans;    IN: Span tree of new spans to add to
                                            hyperslab selection
 RETURNS
    non-negative on success, negative on failure
 DESCRIPTION
    Add a set of hyperslab spans to an existing hyperslab selection.  The
    new spans are required to be non-overlapping with the existing spans in
    the dataspace's current hyperslab selection.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_merge_spans (H5S_t *space, H5S_hyper_span_info_t *new_spans)
{
    herr_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_merge_spans, FAIL);

    /* Check args */
    assert (space);
    assert (new_spans);

#ifdef QAK
printf("%s: space->select.sel_info.hslab.span_lst=%p, new_spans=%p\n",FUNC,space->select.sel_info.hslab.span_lst,new_spans);
#endif /* QAK */
    /* If this is the first span tree in the hyperslab selection, just use it */
    if(space->select.sel_info.hslab.span_lst==NULL) {
        space->select.sel_info.hslab.span_lst=H5S_hyper_copy_span(new_spans);
    } /* end if */
    else {
        H5S_hyper_span_info_t *merged_spans;

        /* Get the merged spans */
        merged_spans=H5S_hyper_merge_spans_helper(space->select.sel_info.hslab.span_lst, new_spans);

        /* Sanity checking since we started with some spans, we should still have some after the merge */
        assert(merged_spans);

        /* Free the previous spans */
        H5S_hyper_free_span_info(space->select.sel_info.hslab.span_lst);

        /* Point to the new merged spans */
        space->select.sel_info.hslab.span_lst=merged_spans;
    } /* end else */

    /* Success!  */
    ret_value=SUCCEED;

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_merge_spans() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_spans_nelem
 PURPOSE
    Count the number of elements in a span tree
 USAGE
    hssize_t H5S_hyper_spans_nelem(spans)
        const H5S_hyper_span_info_t *spans; IN: Hyperslan span tree to count elements of
 RETURNS
    Number of elements in span tree on success; negative on failure
 DESCRIPTION
    Counts the number of elements described by the spans in a span tree.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static hssize_t
H5S_hyper_spans_nelem (H5S_hyper_span_info_t *spans)
{
    H5S_hyper_span_t *span;     /* Hyperslab span */
    hssize_t ret_value=FAIL;

    FUNC_ENTER (H5S_hyper_spans_nelem, FAIL);

    /* Count the number of elements in the span tree */
    if(spans==NULL)
        ret_value=0;
    else {
        span=spans->head;
        ret_value=0;
        while(span!=NULL) {
            /* If there are down spans, multiply the size of this span by the total down span elements */
            if(span->down!=NULL)
                ret_value+=span->nelem*H5S_hyper_spans_nelem(span->down);
            /* If there are no down spans, just count the elements in this span */
            else
                ret_value+=span->nelem;
            
            /* Advance to next span */
            span=span->next;
        } /* end while */
    } /* end else */

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_spans_nelem() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_make_spans
 PURPOSE
    Create a span tree
 USAGE
    H5S_hyper_span_t *H5S_hyper_make_spans(rank, start, stride, count, block)
        unsigned rank;               IN: # of dimensions of the space
        const hssize_t *start;    IN: Starting location of the hyperslabs
        const hsize_t *stride;    IN: Stride from the beginning of one block to
                                        the next
        const hsize_t *count;     IN: Number of blocks
        const hsize_t *block;     IN: Size of hyperslab block
 RETURNS
    Pointer to new span tree on success, NULL on failure
 DESCRIPTION
    Generates a new span tree for the hyperslab parameters specified.
    Each span tree has a list of the elements spanned in each dimension, with
    each span node containing a pointer to the list of spans in the next
    dimension down.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5S_hyper_span_info_t *
H5S_hyper_make_spans (unsigned rank, const hssize_t *start, const hsize_t *stride,
    const hsize_t *count, const hsize_t *block)
{
    H5S_hyper_span_info_t *down;/* Pointer to spans in next dimension down */
    H5S_hyper_span_t *span;     /* New hyperslab span */
    H5S_hyper_span_t *last_span;/* Current position in hyperslab span list */
    H5S_hyper_span_t *head;     /* Head of new hyperslab span list */
    int i;                     /* Counters */
    unsigned u;                    /* Counters */
    H5S_hyper_span_info_t *ret_value=NULL;

    FUNC_ENTER (H5S_hyper_make_spans, NULL);

    /* Check args */
    assert (rank>0);
    assert (start);
    assert (stride);
    assert (count);
    assert (block);

    /* Start creating spans in fastest changing dimension */
    down=NULL;
    for(i=(rank-1); i>=0; i--) {

        /* Start a new list in this dimension */
        head=last_span=NULL;

        /* Generate all the spans segments for this dimension */
        for(u=0; u<count[i]; u++) {
            /* Allocate a span node */
            if((span = H5FL_ALLOC(H5S_hyper_span_t,0))==NULL)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

            /* Set the span's basic information */
            span->low=start[i]+(stride[i]*u);
            span->high=span->low+(block[i]-1);
            span->nelem=block[i];
            span->pstride=stride[i];
            span->next=NULL;

            /* Append to the list of spans in this dimension */
            if(head==NULL)
                head=span;
            else
                last_span->next=span;
            
            /* Move current pointer */
            last_span=span;

            /* Set the information for the next dimension down's spans, if appropriate */
            if(down!=NULL) {
                span->down=down;
                down->count++;  /* Increment reference count for shared span */
            } /* end if */
            else {
                span->down=NULL;
            } /* end else */
        } /* end for */

        /* Allocate a span info node */
        if((down = H5FL_ALLOC(H5S_hyper_span_info_t,0))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate hyperslab span");

        /* Set the reference count */
        down->count=0;

        /* Reset the scratch pad space */
        down->scratch=0;

        /* Keep the pointer to the next dimension down's completed list */
        down->head=head;
    } /* end for */

    /* Indicate that there is a pointer to this tree */
    down->count=1;

    /* Success!  Return the head of the list in the slowest changing dimension */
    ret_value=down;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_make_spans() */

#ifndef NEW_HYPERSLAB_API

/*-------------------------------------------------------------------------
 * Function:	H5S_generate_hyperlab
 *
 * Purpose:	Generate hyperslab information from H5S_select_hyperslab()
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol (split from HS_select_hyperslab()).
 *              Tuesday, September 12, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_generate_hyperslab (H5S_t *space, H5S_seloper_t op,
		      const hssize_t start[],
		      const hsize_t _stride[],
		      const hsize_t _count[],
		      const hsize_t _block[])
{
    hsize_t stride[H5O_LAYOUT_NDIMS];   /* Optimized stride information */
    hsize_t count[H5O_LAYOUT_NDIMS];    /* Optimized count information */
    hsize_t block[H5O_LAYOUT_NDIMS];    /* Optimized block information */
    H5S_hyper_span_info_t *new_spans;   /* Span tree for new hyperslab */
    H5S_hyper_span_info_t *a_not_b=NULL;    /* Span tree for hyperslab spans in old span tree and not in new span tree */
    H5S_hyper_span_info_t *a_and_b=NULL;    /* Span tree for hyperslab spans in both old and new span trees */
    H5S_hyper_span_info_t *b_not_a=NULL;    /* Span tree for hyperslab spans in new span tree and not in old span tree */
    hssize_t nelem;             /* Number of elements in hyperslab span tree */
    unsigned u;                    /* Counters */
    herr_t ret_value=FAIL;    /* return value */

    FUNC_ENTER (H5S_generate_hyperslab, FAIL);

    /* Check args */
    assert(space);
    assert(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID);
    assert(start);
    assert(_stride);
    assert(_count);
    assert(_block);
    
#ifdef QAK
printf("%s: space=%p\n",FUNC,space);
#endif /* QAK */
    /* Optimize hyperslab selection to merge contiguous blocks */
    for(u=0; u<space->extent.u.simple.rank; u++) {
        /* contiguous hyperslabs have the block size equal to the stride */
        if(_stride[u]==_block[u]) {
            count[u]=1;
            stride[u]=1;
            block[u]=_block[u]*_count[u];
        }
        else {
            stride[u]=_stride[u];
            count[u]=_count[u];
            block[u]=_block[u];
        } /* end if */
    } /* end for */

    /* Generate span tree for new hyperslab information */
    if((new_spans=H5S_hyper_make_spans(space->extent.u.simple.rank,start,stride,count,block))==NULL)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't create hyperslab information");
#ifdef QAK
printf("%s: new_spans=%p\n",FUNC,new_spans);
#endif /* QAK */

    /* Generate list of blocks to add/remove based on selection operation */
    if(op==H5S_SELECT_SET) {
        /* Add new spans to current selection */
        if(H5S_hyper_merge_spans(space,new_spans)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

        /* Set the number of elements in current selection */
        if((nelem=H5S_hyper_spans_nelem(new_spans))<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
        space->select.num_elem=nelem;
    } /* end if */
    else {
        /* Generate lists of spans which overlap and don't overlap */
        if(H5S_hyper_clip_spans(space->select.sel_info.hslab.span_lst,new_spans,&a_not_b,&a_and_b,&b_not_a)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLIP, FAIL, "can't clip hyperslab information");

        switch(op) {
            case H5S_SELECT_OR:
                /* Add any new spans from b_not_a to current selection */
                if(b_not_a!=NULL) {
                    if(H5S_hyper_merge_spans(space,b_not_a)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(b_not_a))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    space->select.num_elem+=nelem;
                } /* end if */
                break;

            case H5S_SELECT_AND:
                /* Free the current selection */
                if(H5S_hyper_free_span_info(space->select.sel_info.hslab.span_lst)<0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "failed to release hyperslab spans");
                space->select.sel_info.hslab.span_lst=NULL;

                /* Reset the number of items in selection */
                space->select.num_elem=0;

                /* Check if there are any overlapped selections */
                if(a_and_b!=NULL) {
                    if(H5S_hyper_merge_spans(space,a_and_b)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(a_and_b))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    space->select.num_elem=nelem;
                } /* end if */
                break;

            case H5S_SELECT_XOR:
                /* Free the current selection */
                if(H5S_hyper_free_span_info(space->select.sel_info.hslab.span_lst)<0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "failed to release hyperslab spans");
                space->select.sel_info.hslab.span_lst=NULL;

                /* Reset the number of items in selection */
                space->select.num_elem=0;

                /* Check if there are any non-overlapped selections */
                if(a_not_b!=NULL) {
                    if(H5S_hyper_merge_spans(space,a_not_b)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(a_not_b))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    space->select.num_elem=nelem;
                } /* end if */
                if(b_not_a!=NULL) {
                    if(H5S_hyper_merge_spans(space,b_not_a)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(b_not_a))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    space->select.num_elem+=nelem;
                } /* end if */
                break;

            case H5S_SELECT_NOTB:
                /* Free the current selection */
                if(H5S_hyper_free_span_info(space->select.sel_info.hslab.span_lst)<0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "failed to release hyperslab spans");
                space->select.sel_info.hslab.span_lst=NULL;

                /* Reset the number of items in selection */
                space->select.num_elem=0;

                /* Check if there are any non-overlapped selections */
                if(a_not_b!=NULL) {
                    if(H5S_hyper_merge_spans(space,a_not_b)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(a_not_b))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    space->select.num_elem=nelem;
                } /* end if */
                break;

            case H5S_SELECT_NOTA:
                /* Free the current selection */
                if(H5S_hyper_free_span_info(space->select.sel_info.hslab.span_lst)<0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "failed to release hyperslab spans");
                space->select.sel_info.hslab.span_lst=NULL;

                /* Reset the number of items in selection */
                space->select.num_elem=0;

                /* Check if there are any non-overlapped selections */
                if(b_not_a!=NULL) {
                    if(H5S_hyper_merge_spans(space,b_not_a)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(b_not_a))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    space->select.num_elem=nelem;
                } /* end if */
                break;

            default:
                HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");
        } /* end switch */
#ifdef QAK
printf("%s: a_not_b=%p, a_and_b=%p, b_not_a=%p\n",FUNC,a_not_b,a_and_b,b_not_a);
#endif /* QAK */
        /* Free the hyperslab trees generated from the clipping algorithm */
        if(a_not_b)
            H5S_hyper_free_span_info(a_not_b);
        if(a_and_b)
            H5S_hyper_free_span_info(a_and_b);
        if(b_not_a)
            H5S_hyper_free_span_info(b_not_a);
    } /* end else */

    /* Free the new spans */
    if(H5S_hyper_free_span_info(new_spans)<0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "failed to release temporary hyperslab spans");

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
} /* end H5S_generate_hyperslab() */


/*-------------------------------------------------------------------------
 * Function:	H5S_select_hyperslab
 *
 * Purpose:	Internal version of H5Sselect_hyperslab().
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, January 10, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_select_hyperslab (H5S_t *space, H5S_seloper_t op,
		      const hssize_t start[],
		      const hsize_t stride[],
		      const hsize_t count[],
		      const hsize_t block[])
{
    hsize_t *_stride=NULL;      /* Stride array */
    hsize_t *_block=NULL;       /* Block size array */
    unsigned u;                    /* Counters */
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
        if ((_stride=H5FL_ARR_ALLOC(hsize_t,space->extent.u.simple.rank,0))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for stride buffer");
        H5V_array_fill(_stride,&fill,sizeof(hssize_t),space->extent.u.simple.rank);
        stride = _stride;
    }

    /* Fill in the correct block values */
    if(block==NULL) {
        hssize_t fill=1;

        /* Allocate temporary buffer */
        if ((_block=H5FL_ARR_ALLOC(hsize_t,space->extent.u.simple.rank,0))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for stride buffer");
        H5V_array_fill(_block,&fill,sizeof(hssize_t),space->extent.u.simple.rank);
        block = _block;
    }

    /* Fixup operation if selection is 'none' and operation is an OR */
    /* (Allows for 'or'ing a sequence of hyperslab into a 'none' selection to */
    /* have same affect as setting the first hyperslab in the sequence to have */
    /* the 'set' operation and the rest of the hyperslab sequence to be 'or'ed */
    /* after that */
    if(space->select.type==H5S_SEL_NONE && op==H5S_SELECT_OR)
        op=H5S_SELECT_SET;

    if(op==H5S_SELECT_SET) {
        /*
         * Check for overlapping hyperslab blocks in new selection.
         */
        for(u=0; u<space->extent.u.simple.rank; u++) {
            if(count[u]>1 && stride[u]<block[u])
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "hyperslab blocks overlap");
        } /* end for */

        /* If we are setting a new selection, remove current selection first */
        if(H5S_select_release(space)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't release hyperslab");

        /* Copy all the application per-dimension selection info into the space descriptor */
        if((diminfo = H5FL_ARR_ALLOC(H5S_hyper_dim_t,space->extent.u.simple.rank,0))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate per-dimension vector");
        for(u=0; u<space->extent.u.simple.rank; u++) {
            diminfo[u].start = start[u];
            diminfo[u].stride = stride[u];
            diminfo[u].count = count[u];
            diminfo[u].block = block[u];
        } /* end for */
        space->select.sel_info.hslab.app_diminfo = diminfo;

        /* Allocate room for the optimized per-dimension selection info */
        if((diminfo = H5FL_ARR_ALLOC(H5S_hyper_dim_t,space->extent.u.simple.rank,0))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate per-dimension vector");

        /* Optimize the hyperslab selection to detect contiguously selected block/stride information */
        /* Modify the stride, block & count for contiguous hyperslab selections */
        for(u=0; u<space->extent.u.simple.rank; u++) {
            /* Starting location doesn't get optimized */
            diminfo[u].start = start[u];

            /* contiguous hyperslabs have the block size equal to the stride */
            if(stride[u]==block[u]) {
                diminfo[u].stride=1;
                diminfo[u].count=1;
                diminfo[u].block=count[u]*block[u];
            } /* end if */
            else {
                diminfo[u].stride=stride[u];
                diminfo[u].count=count[u];
                diminfo[u].block=block[u];
            } /* end else */
        } /* end for */
        space->select.sel_info.hslab.diminfo = diminfo;

        /* Build the hyperslab information also */
        if(H5S_generate_hyperslab (space, H5S_SELECT_SET, start, stride, count, block)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't generate hyperslabs");
    } /* end if */
    else if(op>=H5S_SELECT_OR && op<=H5S_SELECT_NOTA) {
        switch(space->select.type) {
            case H5S_SEL_ALL:
                /* break out now, 'or'ing with an all selection leaves the all selection */
                HGOTO_DONE(SUCCEED);

            case H5S_SEL_HYPERSLABS:
                /* Is this the first 'or' operation? */
                if(space->select.sel_info.hslab.diminfo != NULL) {
                    /* Remove the 'diminfo' information, since we're adding to it */
                    H5FL_ARR_FREE(H5S_hyper_dim_t,space->select.sel_info.hslab.diminfo);
                    space->select.sel_info.hslab.diminfo = NULL;

                    /* Remove the 'app_diminfo' information also, since we're adding to it */
                    H5FL_ARR_FREE(H5S_hyper_dim_t,space->select.sel_info.hslab.app_diminfo);
                    space->select.sel_info.hslab.app_diminfo = NULL;
                } /* end if */

                /* Add in the new hyperslab information */
                if(H5S_generate_hyperslab (space, op, start, stride, count, block)<0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't generate hyperslabs");
                break;

            default:
                HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");
        } /* end switch() */
    } /* end if */
    else
        HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");

    /* Set selection type */
    space->select.type=H5S_SEL_HYPERSLABS;

    ret_value=SUCCEED;

done:
    if(_stride!=NULL)
        H5FL_ARR_FREE(hsize_t,_stride);
    if(_block!=NULL)
        H5FL_ARR_FREE(hsize_t,_block);
    FUNC_LEAVE (ret_value);
}   /* end H5S_select_hyperslab() */


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
    selection composing the entire current extent).  If STRIDE or BLOCK is
    NULL, they are assumed to be set to all '1'.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Sselect_hyperslab(hid_t space_id, H5S_seloper_t op, const hssize_t start[],
         const hsize_t stride[], const hsize_t count[], const hsize_t block[])
{
    H5S_t	*space = NULL;  /* Dataspace to modify selection of */

    FUNC_ENTER (H5Sselect_hyperslab, FAIL);
    H5TRACE6("e","iSs*Hs*h*h*h",space_id,op,start,stride,count,block);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(space_id) ||
            NULL == (space=H5I_object(space_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(start==NULL || count==NULL)
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "hyperslab not specified");

    if(!(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID))
        HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");

    if (H5S_select_hyperslab(space, op, start, stride, count, block)<0)
        HRETURN_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to set hyperslab selection");

    FUNC_LEAVE (SUCCEED);
} /* end H5Sselect_hyperslab() */
#else /* OLD_WAY */ /* Works */

/*-------------------------------------------------------------------------
 * Function:	H5S_operate_hyperslab
 *
 * Purpose:	Combines two hyperslabs with an operation, putting the
 *              result into a third hyperslab selection
 *
 * Return:	non-negative on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, October 30, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_operate_hyperslab (H5S_t *result, H5S_hyper_span_info_t *spans1, H5S_seloper_t op, H5S_hyper_span_info_t *spans2)
{
    H5S_hyper_span_info_t *a_not_b=NULL;    /* Span tree for hyperslab spans in old span tree and not in new span tree */
    H5S_hyper_span_info_t *a_and_b=NULL;    /* Span tree for hyperslab spans in both old and new span trees */
    H5S_hyper_span_info_t *b_not_a=NULL;    /* Span tree for hyperslab spans in new span tree and not in old span tree */
    hssize_t nelem;             /* Number of elements in hyperslab span tree */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5S_operate_hyperslab, NULL);

    /* Check args */
    assert(result);
    assert(spans2);
    assert(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID);
    
    /* Free the current selection for the result space */
    if(H5S_select_release(result)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't release result selection");

    /* Just copy the selection from spans2 if we are setting the selection */
    /* ('space1' to 'result' aliasing happens at the next layer up) */
    if(op==H5S_SELECT_SET) {
        if(H5S_hyper_merge_spans(result,spans2)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

        /* Update the number of elements in current selection */
        if((nelem=H5S_hyper_spans_nelem(spans2))<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
        result->select.num_elem=nelem;
    } /* end if */
    else {
        assert(spans1);

        /* Generate lists of spans which overlap and don't overlap */
        if(H5S_hyper_clip_spans(spans1,spans2,&a_not_b,&a_and_b,&b_not_a)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLIP, FAIL, "can't clip hyperslab information");

        /* Switch on the operation */
        switch(op) {
            case H5S_SELECT_OR:
                /* Copy spans from spans1 to current selection */
                if(spans1!=NULL) {
                    if(H5S_hyper_merge_spans(result,spans1)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(spans1))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    result->select.num_elem=nelem;
                } /* end if */

                /* Add any new spans from spans2 to current selection */
                if(b_not_a!=NULL) {
                    if(H5S_hyper_merge_spans(result,b_not_a)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(b_not_a))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    result->select.num_elem+=nelem;
                } /* end if */
                break;

            case H5S_SELECT_AND:
                /* Check if there are any overlapped selections */
                if(a_and_b!=NULL) {
                    if(H5S_hyper_merge_spans(result,a_and_b)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(a_and_b))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    result->select.num_elem=nelem;
                } /* end if */
                break;

            case H5S_SELECT_XOR:
                /* Check if there are any non-overlapped selections */
                if(a_not_b!=NULL) {
                    if(H5S_hyper_merge_spans(result,a_not_b)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(a_not_b))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    result->select.num_elem=nelem;
                } /* end if */
                if(b_not_a!=NULL) {
                    if(H5S_hyper_merge_spans(result,b_not_a)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(b_not_a))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    result->select.num_elem+=nelem;
                } /* end if */
                break;

            case H5S_SELECT_NOTB:
                /* Check if there are any non-overlapped selections */
                if(a_not_b!=NULL) {
                    if(H5S_hyper_merge_spans(result,a_not_b)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(a_not_b))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    result->select.num_elem=nelem;
                } /* end if */
                break;

            case H5S_SELECT_NOTA:
                /* Check if there are any non-overlapped selections */
                if(b_not_a!=NULL) {
                    if(H5S_hyper_merge_spans(result,b_not_a)<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslabs");

                    /* Update the number of elements in current selection */
                    if((nelem=H5S_hyper_spans_nelem(b_not_a))<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't count hyperslab span elements");
                    result->select.num_elem=nelem;
                } /* end if */
                break;

            default:
                HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");
        } /* end switch */

        /* Free the hyperslab trees generated from the clipping algorithm */
        if(a_not_b)
            H5S_hyper_free_span_info(a_not_b);
        if(a_and_b)
            H5S_hyper_free_span_info(a_and_b);
        if(b_not_a)
            H5S_hyper_free_span_info(b_not_a);
    } /* end else */

    /* Set selection type */
    result->select.type=H5S_SEL_HYPERSLABS;

    /* Set the return value */
    ret_value=SUCCEED;

done:

    FUNC_LEAVE (ret_value);
}   /* end H5S_operate_hyperslab() */


/*-------------------------------------------------------------------------
 * Function:	H5S_generate_hyperlab
 *
 * Purpose:	Generate hyperslab information from H5S_select_hyperslab()
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol (split from HS_select_hyperslab()).
 *              Tuesday, September 12, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_generate_hyperslab (H5S_t *space, H5S_seloper_t op,
		      const hssize_t start[],
		      const hsize_t _stride[],
		      const hsize_t _count[],
		      const hsize_t _block[])
{
    hsize_t stride[H5O_LAYOUT_NDIMS];   /* Optimized stride information */
    hsize_t count[H5O_LAYOUT_NDIMS];    /* Optimized count information */
    hsize_t block[H5O_LAYOUT_NDIMS];    /* Optimized block information */
    H5S_hyper_span_info_t *new_spans=NULL;   /* Span tree for new hyperslab */
    H5S_hyper_span_info_t *tmp_spans=NULL;   /* Temporary copy of selection */
    unsigned u;                 /* Counters */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5S_generate_hyperslab, FAIL);

    /* Check args */
    assert(space);
    assert(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID);
    assert(start);
    assert(_stride);
    assert(_count);
    assert(_block);
    
    /* Optimize hyperslab selection to merge contiguous blocks */
    for(u=0; u<space->extent.u.simple.rank; u++) {
        /* contiguous hyperslabs have the block size equal to the stride */
        if(_stride[u]==_block[u]) {
            count[u]=1;
            stride[u]=1;
            block[u]=_block[u]*_count[u];
        }
        else {
            stride[u]=_stride[u];
            count[u]=_count[u];
            block[u]=_block[u];
        } /* end if */
    } /* end for */

    /* Generate span tree for new hyperslab information */
    if((new_spans=H5S_hyper_make_spans(space->extent.u.simple.rank,start,stride,count,block))==NULL)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't create hyperslab information");

    /* Copy the original dataspace */
    if(space->select.sel_info.hslab.span_lst!=NULL)
        if (NULL==(tmp_spans=H5S_hyper_copy_span(space->select.sel_info.hslab.span_lst)))
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy data space");

    /* Combine tmp_space (really space) & new_space, with the result in space */
    if(H5S_operate_hyperslab(space,tmp_spans,op,new_spans)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLIP, FAIL, "can't clip hyperslab information");

    /* Set return value */
    ret_value=SUCCEED;

done:
    /* Free temporary data structures */
    if(tmp_spans!=NULL)
        H5S_hyper_free_span_info(tmp_spans);
    if(new_spans!=NULL)
        H5S_hyper_free_span_info(new_spans);

    FUNC_LEAVE (ret_value);
} /* end H5S_generate_hyperslab() */


/*-------------------------------------------------------------------------
 * Function:	H5S_select_hyperslab
 *
 * Purpose:	Internal version of H5Sselect_hyperslab().
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, January 10, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_select_hyperslab (H5S_t *space, H5S_seloper_t op,
		      const hssize_t start[],
		      const hsize_t stride[],
		      const hsize_t count[],
		      const hsize_t block[])
{
    hsize_t *_stride=NULL;      /* Stride array */
    hsize_t *_block=NULL;       /* Block size array */
    unsigned u;                    /* Counters */
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
        if ((_stride=H5FL_ARR_ALLOC(hsize_t,space->extent.u.simple.rank,0))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for stride buffer");
        H5V_array_fill(_stride,&fill,sizeof(hssize_t),space->extent.u.simple.rank);
        stride = _stride;
    }

    /* Fill in the correct block values */
    if(block==NULL) {
        hssize_t fill=1;

        /* Allocate temporary buffer */
        if ((_block=H5FL_ARR_ALLOC(hsize_t,space->extent.u.simple.rank,0))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for stride buffer");
        H5V_array_fill(_block,&fill,sizeof(hssize_t),space->extent.u.simple.rank);
        block = _block;
    }

    /* Fixup operation if selection is 'none' and operation is an OR */
    /* (Allows for 'or'ing a sequence of hyperslab into a 'none' selection to */
    /* have same affect as setting the first hyperslab in the sequence to have */
    /* the 'set' operation and the rest of the hyperslab sequence to be 'or'ed */
    /* after that */
    if(space->select.type==H5S_SEL_NONE && op==H5S_SELECT_OR)
        op=H5S_SELECT_SET;

    if(op==H5S_SELECT_SET) {
        /*
         * Check for overlapping hyperslab blocks in new selection.
         */
        for(u=0; u<space->extent.u.simple.rank; u++) {
            if(count[u]>1 && stride[u]<block[u])
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "hyperslab blocks overlap");
        } /* end for */

        /* If we are setting a new selection, remove current selection first */
        if(H5S_select_release(space)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't release hyperslab");

        /* Copy all the application per-dimension selection info into the space descriptor */
        if((diminfo = H5FL_ARR_ALLOC(H5S_hyper_dim_t,space->extent.u.simple.rank,0))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate per-dimension vector");
        for(u=0; u<space->extent.u.simple.rank; u++) {
            diminfo[u].start = start[u];
            diminfo[u].stride = stride[u];
            diminfo[u].count = count[u];
            diminfo[u].block = block[u];
        } /* end for */
        space->select.sel_info.hslab.app_diminfo = diminfo;

        /* Allocate room for the optimized per-dimension selection info */
        if((diminfo = H5FL_ARR_ALLOC(H5S_hyper_dim_t,space->extent.u.simple.rank,0))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate per-dimension vector");

        /* Optimize the hyperslab selection to detect contiguously selected block/stride information */
        /* Modify the stride, block & count for contiguous hyperslab selections */
        for(u=0; u<space->extent.u.simple.rank; u++) {
            /* Starting location doesn't get optimized */
            diminfo[u].start = start[u];

            /* contiguous hyperslabs have the block size equal to the stride */
            if(stride[u]==block[u]) {
                diminfo[u].stride=1;
                diminfo[u].count=1;
                diminfo[u].block=count[u]*block[u];
            } /* end if */
            else {
                diminfo[u].stride=stride[u];
                diminfo[u].count=count[u];
                diminfo[u].block=block[u];
            } /* end else */
        } /* end for */
        space->select.sel_info.hslab.diminfo = diminfo;

        /* Build the hyperslab information also */
        if(H5S_generate_hyperslab (space, H5S_SELECT_SET, start, stride, count, block)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't generate hyperslabs");
    } /* end if */
    else if(op>=H5S_SELECT_OR && op<=H5S_SELECT_NOTA) {
        switch(space->select.type) {
            case H5S_SEL_HYPERSLABS:
                /* Is this the first 'or' operation? */
                if(space->select.sel_info.hslab.diminfo != NULL) {
                    /* Remove the 'diminfo' information, since we're adding to it */
                    H5FL_ARR_FREE(H5S_hyper_dim_t,space->select.sel_info.hslab.diminfo);
                    space->select.sel_info.hslab.diminfo = NULL;

                    /* Remove the 'app_diminfo' information also, since we're adding to it */
                    H5FL_ARR_FREE(H5S_hyper_dim_t,space->select.sel_info.hslab.app_diminfo);
                    space->select.sel_info.hslab.app_diminfo = NULL;
                } /* end if */

                /* Add in the new hyperslab information */
                if(H5S_generate_hyperslab (space, op, start, stride, count, block)<0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't generate hyperslabs");
                break;

            default:
                HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");
        } /* end switch() */
    } /* end if */
    else
        HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");

    ret_value=SUCCEED;

done:
    if(_stride!=NULL)
        H5FL_ARR_FREE(hsize_t,_stride);
    if(_block!=NULL)
        H5FL_ARR_FREE(hsize_t,_block);
    FUNC_LEAVE (ret_value);
}   /* end H5S_select_hyperslab() */


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
    selection composing the entire current extent).  If STRIDE or BLOCK is
    NULL, they are assumed to be set to all '1'.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Sselect_hyperslab(hid_t space_id, H5S_seloper_t op, const hssize_t start[],
         const hsize_t stride[], const hsize_t count[], const hsize_t block[])
{
    H5S_t	*space = NULL;  /* Dataspace to modify selection of */

    FUNC_ENTER (H5Sselect_hyperslab, FAIL);
    H5TRACE6("e","iSs*Hs*h*h*h",space_id,op,start,stride,count,block);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(space_id) ||
            NULL == (space=H5I_object(space_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(start==NULL || count==NULL)
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "hyperslab not specified");

    if(!(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID))
        HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");

    if (H5S_select_hyperslab(space, op, start, stride, count, block)<0)
        HRETURN_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to set hyperslab selection");

    FUNC_LEAVE (SUCCEED);
} /* end H5Sselect_hyperslab() */


/*--------------------------------------------------------------------------
 NAME
    H5Scombine_hyperslab
 PURPOSE
    Specify a hyperslab to combine with the current hyperslab selection and
    return a new dataspace with the combined selection as the selection in the
    new dataspace.
 USAGE
    hid_t H5Srefine_hyperslab(dsid, op, start, stride, count, block)
        hid_t dsid;             IN: Dataspace ID of selection to use
        H5S_seloper_t op;       IN: Operation to perform on current selection
        const hssize_t *start;        IN: Offset of start of hyperslab
        const hssize_t *stride;       IN: Hyperslab stride
        const hssize_t *count;        IN: Number of blocks included in hyperslab
        const hssize_t *block;        IN: Size of block in hyperslab
 RETURNS
    Dataspace ID on success/Negative on failure
 DESCRIPTION
    Combines a hyperslab selection with the current selection for a dataspace,
    creating a new dataspace to return the generated selection.
    If the current selection is not a hyperslab, it is freed and the hyperslab
    parameters passed in are combined with the H5S_SEL_ALL hyperslab (ie. a
    selection composing the entire current extent).  If STRIDE or BLOCK is
    NULL, they are assumed to be set to all '1'.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t
H5Scombine_hyperslab(hid_t space_id, H5S_seloper_t op, const hssize_t start[],
         const hsize_t stride[], const hsize_t count[], const hsize_t block[])
{
    H5S_t	*space = NULL;  /* Dataspace to modify selection of */
    H5S_t	*new_space = NULL;  /* New dataspace created */
    hid_t	ret_value = FAIL;

    FUNC_ENTER (H5Scombine_hyperslab, FAIL);
    H5TRACE6("i","iSs*Hs*h*h*h",space_id,op,start,stride,count,block);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(space_id) ||
            NULL == (space=H5I_object(space_id))) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(start==NULL || count==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "hyperslab not specified");

    if(!(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID))
        HGOTO_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");

    /* Copy the first dataspace */
    if (NULL==(new_space=H5S_copy (space)))
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, NULL, "unable to copy data space");

    /* Go modify the selection in the new dataspace */
    if (H5S_select_hyperslab(new_space, op, start, stride, count, block)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to set hyperslab selection");

    /* Atomize */
    if ((ret_value=H5I_register (H5I_DATASPACE, new_space))<0)
        HGOTO_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom");

done:
    if (ret_value<0 && new_space)
        H5S_close(new_space);

    FUNC_LEAVE (ret_value);
} /* end H5Scombine_hyperslab() */


/*-------------------------------------------------------------------------
 * Function:	H5S_combine_select
 *
 * Purpose:	Internal version of H5Scombine_select().
 *
 * Return:	New dataspace on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, October 30, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5S_t *
H5S_combine_select (H5S_t *space1, H5S_seloper_t op, H5S_t *space2)
{
    H5S_t *new_space=NULL;    /* New dataspace generated */
    H5S_t *ret_value=NULL;    /* return value */

    FUNC_ENTER (H5S_combine_select, NULL);

    /* Check args */
    assert(space1);
    assert(space2);
    assert(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID);
    
    /* Copy the first dataspace */
    if (NULL==(new_space=H5S_copy (space1)))
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, NULL, "unable to copy data space");

    /* Combine space1 & space2, with the result in new_space */
    if(H5S_operate_hyperslab(new_space,space1->select.sel_info.hslab.span_lst,op,space2->select.sel_info.hslab.span_lst)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLIP, NULL, "can't clip hyperslab information");

    /* Set the return value */
    ret_value=new_space;

done:
    if(ret_value==NULL && new_space!=NULL)
        H5S_close(new_space);

    FUNC_LEAVE (ret_value);
}   /* end H5S_combine_select() */


/*--------------------------------------------------------------------------
 NAME
    H5Scombine_select
 PURPOSE
    Combine two hyperslab selections with an operation, returning a dataspace
    with the resulting selection.
 USAGE
    hid_t H5Scombine_select(space1, op, space2)
        hid_t space1;           IN: First Dataspace ID 
        H5S_seloper_t op;       IN: Selection operation
        hid_t space2;           IN: Second Dataspace ID
 RETURNS
    Dataspace ID on success/Negative on failure
 DESCRIPTION
    Combine two existing hyperslab selections with an operation, returning
    a new dataspace with the resulting selection.  The dataspace extent from
    space1 is copied for the dataspace extent of the newly created dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t
H5Scombine_select(hid_t space1_id, H5S_seloper_t op, hid_t space2_id)
{
    H5S_t	*space1;                /* First Dataspace */
    H5S_t	*space2;                /* Second Dataspace */
    H5S_t	*new_space = NULL;      /* New Dataspace */
    hid_t	ret_value = FAIL;

    FUNC_ENTER (H5Scombine_select, FAIL);
    H5TRACE3("i","iSsi",space1_id,op,space2_id);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(space1_id) ||
            NULL == (space1=H5I_object(space1_id))) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if (H5I_DATASPACE != H5I_get_type(space2_id) ||
            NULL == (space2=H5I_object(space2_id))) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(!(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID))
        HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");

    /* Check that both dataspaces have the same rank */
    if(space1->extent.u.simple.rank!=space2->extent.u.simple.rank)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "dataspaces not same rank");

    /* Check that both dataspaces have hyperslab selections */
    if(space1->select.type!=H5S_SEL_HYPERSLABS || space2->select.type!=H5S_SEL_HYPERSLABS)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "dataspaces don't have hyperslab selections");

    /* Go combine the dataspaces */
    if ((new_space=H5S_combine_select(space1, op, space2))==NULL)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to create hyperslab selection");

    /* Atomize */
    if ((ret_value=H5I_register (H5I_DATASPACE, new_space))<0)
        HGOTO_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom");

done:
    if (ret_value<0 && new_space)
        H5S_close(new_space);

    FUNC_LEAVE (ret_value);
} /* end H5Scombine_select() */


/*-------------------------------------------------------------------------
 * Function:	H5S_select_select
 *
 * Purpose:	Internal version of H5Sselect_select().
 *
 * Return:	New dataspace on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, October 30, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_select_select (H5S_t *space1, H5S_seloper_t op, H5S_t *space2)
{
    H5S_hyper_span_info_t *tmp_spans=NULL;   /* Temporary copy of selection */
    herr_t ret_value=FAIL;    /* return value */

    FUNC_ENTER (H5S_select_select, FAIL);

    /* Check args */
    assert(space1);
    assert(space2);
    assert(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID);
    
    /* Copy the first dataspace */
    if (NULL==(tmp_spans=H5S_hyper_copy_span(space1->select.sel_info.hslab.span_lst)))
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy data space");

    /* Combine tmp_spans (from space1) & spans from space2, with the result in space1 */
    if(H5S_operate_hyperslab(space1,tmp_spans,op,space2->select.sel_info.hslab.span_lst)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLIP, FAIL, "can't clip hyperslab information");

    /* Set the return value */
    ret_value=SUCCEED;

done:
    if(tmp_spans!=NULL)
        H5S_hyper_free_span_info(tmp_spans);

    FUNC_LEAVE (ret_value);
}   /* end H5S_select_select() */


/*--------------------------------------------------------------------------
 NAME
    H5Sselect_select
 PURPOSE
    Refine a hyperslab selection with an operation using a second hyperslab
    to modify it.
 USAGE
    herr_t H5Sselect_select(space1, op, space2)
        hid_t space1;           IN/OUT: First Dataspace ID 
        H5S_seloper_t op;       IN: Selection operation
        hid_t space2;           IN: Second Dataspace ID
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Refine an existing hyperslab selection with an operation, using a second
    hyperslab.  The first selection is modified to contain the result of
    space1 operated on by space2.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Sselect_select(hid_t space1_id, H5S_seloper_t op, hid_t space2_id)
{
    H5S_t	*space1;                /* First Dataspace */
    H5S_t	*space2;                /* Second Dataspace */
    hid_t	ret_value = FAIL;

    FUNC_ENTER (H5Sselect_select, FAIL);
    H5TRACE3("e","iSsi",space1_id,op,space2_id);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(space1_id) ||
            NULL == (space1=H5I_object(space1_id))) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if (H5I_DATASPACE != H5I_get_type(space2_id) ||
            NULL == (space2=H5I_object(space2_id))) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if(!(op>H5S_SELECT_NOOP && op<H5S_SELECT_INVALID))
        HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "invalid selection operation");

    /* Check that both dataspaces have the same rank */
    if(space1->extent.u.simple.rank!=space2->extent.u.simple.rank)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "dataspaces not same rank");

    /* Check that both dataspaces have hyperslab selections */
    if(space1->select.type!=H5S_SEL_HYPERSLABS || space2->select.type!=H5S_SEL_HYPERSLABS)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "dataspaces don't have hyperslab selections");

    /* Go refine the first selection */
    if (H5S_select_select(space1, op, space2)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to modify hyperslab selection");

    /* Set the return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
} /* end H5Sselect_select() */
#endif /* OLD_WAY */ /* Works */
