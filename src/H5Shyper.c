/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Thursday, June 18, 1998
 *
 * Purpose:	Hyperslab selection data space I/O functions.
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Iprivate.h>
#include <H5Sprivate.h>
#include <H5Vprivate.h>
#include <H5MMprivate.h>
#include <H5TBprivate.h>
#include <H5Dprivate.h>

/* Interface initialization */
#define PABLO_MASK      H5Shyper_mask
#define INTERFACE_INIT  NULL
static intn             interface_initialize_g = 0;

/* Local datatypes */
/* Parameter block for H5S_hyper_fread, H5S_hyper_fwrite, H5S_hyper_mread & H5S_hyper_mwrite */
typedef struct {
    H5F_t *f;
    const struct H5O_layout_t *layout;
    const struct H5O_pline_t *pline;
    const struct H5O_fill_t *fill;
    const struct H5O_efl_t *efl;
    size_t elmt_size;
    const H5S_t *space;
    H5S_sel_iter_t *iter;
	size_t nelmts;
    const H5F_xfer_t *xfer_parms;
    const void *src;
    void *dst;
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];
    hssize_t offset[H5O_LAYOUT_NDIMS];
    hsize_t	hsize[H5O_LAYOUT_NDIMS];
    H5S_hyper_bound_t **lo_bounds;
    H5S_hyper_bound_t **hi_bounds;
} H5S_hyper_io_info_t;

/* Parameter block for H5S_hyper_select_iter_mem */
typedef struct {
    hid_t dt;
    size_t elem_size;
    const H5S_t *space;
    H5S_sel_iter_t *iter;
    void *src;
    H5S_hyper_bound_t **lo_bounds;
    H5S_hyper_bound_t **hi_bounds;
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];
    hssize_t mem_offset[H5O_LAYOUT_NDIMS];
    H5D_operator_t op;
    void * op_data;
} H5S_hyper_iter_info_t;

/* Static function prototypes */
static intn H5S_hyper_bsearch(hssize_t size, H5S_hyper_bound_t *barr,
			      size_t count);
static hid_t
H5S_hyper_get_regions (size_t *num_regions, intn dim, size_t bound_count,
		       H5S_hyper_bound_t **lo_bounds,
		       H5S_hyper_bound_t **hi_bounds, hssize_t *pos,
		       hssize_t *offset);
static size_t H5S_hyper_fread (intn dim, H5S_hyper_io_info_t *io_info);
static size_t H5S_hyper_fwrite (intn dim,
				H5S_hyper_io_info_t *io_info);
static herr_t H5S_hyper_init (const struct H5O_layout_t *layout,
			      const H5S_t *space, H5S_sel_iter_t *iter, size_t *min_elem_out);
static size_t H5S_hyper_favail (const H5S_t *space, const H5S_sel_iter_t *iter,
				size_t max);
static size_t H5S_hyper_fgath (H5F_t *f, const struct H5O_layout_t *layout,
			       const struct H5O_pline_t *pline,
			       const struct H5O_fill_t *fill,
			       const struct H5O_efl_t *efl, size_t elmt_size,
			       const H5S_t *file_space,
			       H5S_sel_iter_t *file_iter, size_t nelmts,
			       const H5F_xfer_t *xfer_parms, void *buf/*out*/);
static herr_t H5S_hyper_fscat (H5F_t *f, const struct H5O_layout_t *layout,
			       const struct H5O_pline_t *pline,
			       const struct H5O_fill_t *fill,
			       const struct H5O_efl_t *efl, size_t elmt_size,
			       const H5S_t *file_space,
			       H5S_sel_iter_t *file_iter, size_t nelmts,
			       const H5F_xfer_t *xfer_parms, const void *buf);
static size_t H5S_hyper_mgath (const void *_buf, size_t elmt_size,
			       const H5S_t *mem_space,
			       H5S_sel_iter_t *mem_iter, size_t nelmts,
			       void *_tconv_buf/*out*/);
static herr_t H5S_hyper_mscat (const void *_tconv_buf, size_t elmt_size,
			       const H5S_t *mem_space,
			       H5S_sel_iter_t *mem_iter, size_t nelmts,
			       void *_buf/*out*/);

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

/* Array for use with I/O algorithms which frequently need array of zeros */
static const hssize_t	zero[H5O_LAYOUT_NDIMS]={0};		/* Array of zeros */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_init
 *
 * Purpose:	Initializes iteration information for hyperslab selection.
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
static herr_t
H5S_hyper_init (const struct H5O_layout_t UNUSED *layout,
	       const H5S_t *space, H5S_sel_iter_t *sel_iter,
		size_t UNUSED *min_elem_out)
{
    FUNC_ENTER (H5S_hyper_init, FAIL);

    /* Check args */
    assert (space && H5S_SEL_HYPERSLABS==space->select.type);
    assert (sel_iter);

    /* Initialize the number of points to iterate over */
    sel_iter->hyp.elmt_left=space->select.num_elem;

    /* Allocate the position & initialize to invalid location */
    sel_iter->hyp.pos = H5MM_malloc(space->extent.u.simple.rank * sizeof(hssize_t));
    sel_iter->hyp.pos[0]=(-1);
    H5V_array_fill(sel_iter->hyp.pos, sel_iter->hyp.pos, sizeof(hssize_t),
		   space->extent.u.simple.rank);
    
    FUNC_LEAVE (SUCCEED);
}   /* H5S_hyper_init() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_favail
 *
 * Purpose:	Figure out the optimal number of elements to transfer to/from the file
 *
 * Return:	non-negative number of elements on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5S_hyper_favail (const H5S_t UNUSED *space,
		  const H5S_sel_iter_t *sel_iter, size_t max)
{
    FUNC_ENTER (H5S_hyper_favail, FAIL);

    /* Check args */
    assert (space && H5S_SEL_HYPERSLABS==space->select.type);
    assert (sel_iter);

#ifdef QAK
    printf("%s: max=%u\n",FUNC,(unsigned)max);
#endif /* QAK */
    FUNC_LEAVE (MIN(sel_iter->hyp.elmt_left,max));
}   /* H5S_hyper_favail() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_compare_regions
 *
 * Purpose:	Compares two regions for equality (regions must not overlap!)
 *
 * Return:	an integer less than, equal to, or greater than zero if the
 *		first region is considered to be respectively less than,
 *		equal to, or greater than the second
 *
 * Programmer:	Quincey Koziol
 *              Friday, July 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5S_hyper_compare_regions (const void *r1, const void *r2)
{
    if (((const H5S_hyper_region_t *)r1)->start < ((const H5S_hyper_region_t *)r2)->start)
        return(-1);
    else if (((const H5S_hyper_region_t *)r1)->start > ((const H5S_hyper_region_t *)r2)->start)
        return(1);
    else
        return(0);
}   /* end H5S_hyper_compare_regions */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_get_regions
 *
 * Purpose:	Builds a sorted array of the overlaps in a dimension
 *
 * Return:	Success:	Pointer to valid array (num_regions parameter
 *				set to array size)
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Monday, June 29, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5S_hyper_get_regions (size_t *num_regions, intn dim, size_t bound_count,
   H5S_hyper_bound_t **lo_bounds, H5S_hyper_bound_t **hi_bounds, hssize_t *pos,
   hssize_t *offset)
{
    hid_t ret_value=FAIL;	            /* Id of temporary buffer to return */
    H5S_hyper_region_t *reg=NULL;	    /* Pointer to array of regions */
    H5S_hyper_node_t *node;             /* Region node for a given boundary */
    size_t num_reg=0;                   /* Number of regions in array */
    size_t curr_reg=0;                  /* The current region we are working with */
    size_t uniq_reg;                    /* The number of unique regions */
    intn next_dim;                      /* Next fastest dimension */
    intn temp_dim;                      /* Temporary dim. holder */
    size_t i;                           /* Counters */

    FUNC_ENTER (H5S_hyper_get_regions, FAIL);
    
    assert(num_regions);
    assert(lo_bounds);
    assert(hi_bounds);
    assert(pos);

#ifdef QAK
    printf("%s: check 1.0, dim=%d\n",FUNC,dim);
    for(i=0; i<3; i++)
        printf("%s: %d - pos=%d, offset=%d\n",FUNC,i,(int)pos[i],offset!=NULL ? (int)offset[i] : 0);
#endif /* QAK */

    /* Check if we need to generate a list of regions for the 0th dim. */
    if(dim<0) {
#ifdef QAK
	printf("%s: check 1.1, bound_count=%d\n",FUNC,bound_count);
#endif /* QAK */
        for(i=0; i<bound_count; i++) {
#ifdef QAK
	printf("%s: check 1.2, lo_bounds[0][%d].bound=%d, hi_bounds[0][%d].bound=%d\n",FUNC,(int)i,(int)lo_bounds[0][i].bound,(int)i,(int)hi_bounds[0][i].bound);
#endif /* QAK */
            /* Skip past already iterated regions */
            if(pos[0]==(-1) || ((pos[0]+offset[0])>=lo_bounds[0][i].bound && (pos[0]+offset[0]) <= hi_bounds[0][i].bound)) {
                /* Check if we've allocated the array yet */
                if(num_reg==0) {
                    /* Allocate temporary buffer */
                    ret_value=H5TB_get_buf(sizeof(H5S_hyper_region_t),0,(void **)&reg);

                    /* Initialize with first region */
                    reg[0].start=MAX(lo_bounds[0][i].bound,pos[0])+offset[0];
                    reg[0].end=hi_bounds[0][i].bound+offset[0];
                    reg[0].node=hi_bounds[0][i].node;

                    /* Increment the number of regions */
                    num_reg++;
                } else {
                    /*
                     * Check if we should merge this region into the current
                     * region.
                     */
                    if(lo_bounds[0][i].bound<reg[curr_reg].end) {
                        reg[curr_reg].end=MAX(hi_bounds[0][i].bound,
						    reg[curr_reg].end)+(offset!=NULL ? offset[0] : 0 );
                    } else { /* no overlap with previous region, add new region */
                        /* Check if this is actually a different region */
                        if(lo_bounds[0][i].bound!=reg[curr_reg].start &&
                            hi_bounds[0][i].bound!=reg[curr_reg].end) {

                            /* Enlarge array */
                            H5TB_resize_buf(ret_value,(sizeof(H5S_hyper_region_t)*(num_reg+1)),(void **)&reg);

                            /* Initialize with new region */
                            reg[num_reg].start=lo_bounds[0][i].bound+offset[0];
                            reg[num_reg].end=hi_bounds[0][i].bound+offset[0];
                            reg[num_reg].node=hi_bounds[0][i].node;

                            /*
                             * Increment the number of regions & the current
                             * region.
                             */
                            num_reg++;
                            curr_reg++;
                        } /* end if */
                    } /* end else */
                } /* end else */
            } /* end if */
        } /* end for */
    } else {
	/* Generate list of regions based on the current position */
#ifdef QAK
	printf("%s: check 2.0, bound_count=%d\n",FUNC,bound_count);
	printf("%s: check 2.0, pos[%d]=%d, offset[%d]=%d, hi_bounds[%d][%d].bound=%d\n",FUNC,(int)dim,(int)pos[dim],(int)dim,(int)offset[dim],
        (int)dim,(int)0,(int)hi_bounds[dim][0].bound);
#endif /* QAK */
        next_dim=dim+1;
        /* Skip past bounds which don't overlap */
        i=0;
        while(pos[dim]>(hi_bounds[dim][i].bound+offset[dim]) && i<bound_count)
            i++;
#ifdef QAK
	printf("%s: check 2.0.5, lo_bounds[%d][%d].bound=%d\n",FUNC,
        (int)dim,(int)i,(int)lo_bounds[dim][i].bound);
#endif /* QAK */

        for (/*void*/;
             i<bound_count && pos[dim]>=lo_bounds[dim][i].bound+offset[dim];
	     i++) {
#ifdef QAK
	    printf("%s: check 2.1, i=%d, num_reg=%d, pos[%d]=%d\n",
		   FUNC,i,(int)num_reg,dim,(int)pos[dim]);
	    {
		intn j;
		node=lo_bounds[dim][i].node;
		for(j=next_dim; j>=0; j--)
		    printf("%s: lo_bound[%d]=%d, hi_bound[%d]=%d\n",
			   FUNC,j,(int)node->start[j],j,(int)node->end[j]);
	    }
#endif /* QAK */
            /* Check if each boundary overlaps in the higher dimensions */
            node=lo_bounds[dim][i].node;
            temp_dim=dim;
            while(temp_dim>=0 && pos[temp_dim]>=(node->start[temp_dim]+offset[temp_dim]) && pos[temp_dim]<=(node->end[temp_dim]+offset[temp_dim]))
                temp_dim--;

            /* Yes, all previous positions match, this is a valid region */
            if(temp_dim<0) {
#ifdef QAK
		printf("%s: check 3.0\n",FUNC);
#endif /* QAK */
                /* Check if we've allocated the array yet */
                if(num_reg==0) {
#ifdef QAK
		    printf("%s: check 3.1\n", FUNC);
#endif /* QAK */
                    /* Allocate temporary buffer */
                    ret_value=H5TB_get_buf(sizeof(H5S_hyper_region_t),0,(void **)&reg);

                    /* Initialize with first region */
                    reg[0].start=MAX(node->start[next_dim],pos[next_dim])+offset[next_dim];
                    reg[0].end=node->end[next_dim]+offset[next_dim];
                    reg[0].node=node;
#ifdef QAK
		    printf("%s: check 3.2, lo_bounds=%d, start=%d, "
			   "hi_bounds=%d, end=%d\n",
			   FUNC, (int)node->start[next_dim],
			   (int)reg[curr_reg].start,
			   (int)node->end[next_dim],
			   (int)reg[curr_reg].end);
#endif /* QAK */

                    /* Increment the number of regions */
                    num_reg++;
                } else {
#ifdef QAK
		    printf("%s: check 4.0, lo_bounds=%d, start=%d, "
			   "hi_bounds=%d, end=%d\n",
			   FUNC, (int)node->start[next_dim],
			   (int)reg[curr_reg].start,
			   (int)node->end[next_dim],
			   (int)reg[curr_reg].end);
#endif /* QAK */
                    /* Enlarge array */
                    H5TB_resize_buf(ret_value,(sizeof(H5S_hyper_region_t)*(num_reg+1)),(void **)&reg);

                    /* Initialize with new region */
                    reg[num_reg].start=node->start[next_dim]+offset[next_dim];
                    reg[num_reg].end=node->end[next_dim]+offset[next_dim];
                    reg[num_reg].node=node;

                    /* Increment the number of regions & the current region */
                    num_reg++;
                    curr_reg++;
                } /* end else */
            } /* end if */
        } /* end for */

        /* Sort region list and eliminate duplicates if necessary */
        if(num_reg>1) {
            HDqsort(reg,num_reg,sizeof(H5S_hyper_region_t),H5S_hyper_compare_regions);
            for(i=1,curr_reg=0,uniq_reg=1; i<num_reg; i++) {
                if(reg[curr_reg].start!=reg[i].start &&
                        reg[curr_reg].end!=reg[i].end) {
                    uniq_reg++;
                    curr_reg++;
                    reg[curr_reg].start=reg[i].start;
                    reg[curr_reg].end=reg[i].end;
                    reg[curr_reg].node=reg[i].node;
                } /* end if */
            } /* end for */
            num_reg=uniq_reg;
        } /* end if */
    } /* end else */

    /* Save the number of regions we generated */
    *num_regions=num_reg;

#ifdef QAK
    printf("%s: check 10.0, reg=%p, num_reg=%d\n",
	   FUNC,reg,num_reg);
    for(i=0; i<num_reg; i++)
        printf("%s: start[%d]=%d, end[%d]=%d\n",
	       FUNC,i,(int)reg[i].start,i,(int)reg[i].end);
#endif /* QAK */

    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_get_regions() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_block_cache
 *
 * Purpose:	Cache a hyperslab block for reading or writing.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_hyper_block_cache (H5S_hyper_node_t *node,
		       H5S_hyper_io_info_t *io_info, uintn block_read)
{
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of slab in file*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    intn i;                   /* Counters */

    FUNC_ENTER (H5S_hyper_block_cache, SUCCEED);

    assert(node);
    assert(io_info);

    /* Allocate temporary buffer of proper size */
    if((node->cinfo.block_id=H5TB_get_buf(node->cinfo.size*io_info->elmt_size,1,(void **)&(node->cinfo.block)))<0)
        HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
            "can't allocate hyperslab cache block");

    /* Read in block, if we are read caching */
    if(block_read) {
        /* Copy the location of the region in the file */
        HDmemcpy(file_offset,node->start,(io_info->space->extent.u.simple.rank * sizeof(hssize_t)));
        file_offset[io_info->space->extent.u.simple.rank]=0;

        /* Set the hyperslab size to read */
        for(i=0; i<io_info->space->extent.u.simple.rank; i++)
            hsize[i]=(node->end[i]-node->start[i])+1;
        hsize[io_info->space->extent.u.simple.rank]=io_info->elmt_size;

        if (H5F_arr_read (io_info->f, io_info->xfer_parms,
			  io_info->layout, io_info->pline,
			  io_info->fill, io_info->efl, hsize, hsize,
			  zero, file_offset, node->cinfo.block/*out*/)<0)
            HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, FAIL, "read error");
    } /* end if */
    else {
/* keep information for writing block later? */
    } /* end else */
    
    /* Set up parameters for accessing block (starting the read and write information at the same point) */
    node->cinfo.wleft=node->cinfo.rleft=node->cinfo.size;
    node->cinfo.wpos=node->cinfo.rpos=node->cinfo.block;

    /* Set cached flag */
    node->cinfo.cached=1;

    FUNC_LEAVE (SUCCEED);
}   /* H5S_hyper_block_cache() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_block_read
 *
 * Purpose:	Read in data from a cached hyperslab block
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_hyper_block_read (H5S_hyper_node_t *node, H5S_hyper_io_info_t *io_info, hsize_t region_size)
{
    FUNC_ENTER (H5S_hyper_block_read, SUCCEED);

    assert(node && node->cinfo.cached);
    assert(io_info);

    /* Copy the elements into the user's buffer */
    /*
        !! NOTE !! This will need to be changed for different dimension
            permutations from the standard 'C' ordering!
    */
    HDmemcpy(io_info->dst, node->cinfo.rpos, (size_t)(region_size*io_info->elmt_size));

    /*
     * Decrement the number of elements left in block to read & move the
     * offset
     */
    node->cinfo.rpos+=region_size*io_info->elmt_size;
    node->cinfo.rleft-=region_size;

    /* If we've read in all the elements from the block, throw it away */
    if(node->cinfo.rleft==0 && (node->cinfo.wleft==0 || node->cinfo.wleft==node->cinfo.size)) {
        /* Release the temporary buffer */
        H5TB_release_buf(node->cinfo.block_id);

        /* Reset the caching flag for next time */
        node->cinfo.cached=0;
    } /* end if */

    FUNC_LEAVE (SUCCEED);
}   /* H5S_hyper_block_read() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_block_write
 *
 * Purpose:	Write out data to a cached hyperslab block
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_hyper_block_write (H5S_hyper_node_t *node,
		       H5S_hyper_io_info_t *io_info,
		       hsize_t region_size)
{
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of slab in file*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    intn i;                   /* Counters */

    FUNC_ENTER (H5S_hyper_block_write, SUCCEED);

    assert(node && node->cinfo.cached);
    assert(io_info);

    /* Copy the elements into the user's buffer */
    /*
        !! NOTE !! This will need to be changed for different dimension
            permutations from the standard 'C' ordering!
    */
    HDmemcpy(node->cinfo.wpos, io_info->src, (size_t)(region_size*io_info->elmt_size));

    /*
     * Decrement the number of elements left in block to read & move the
     * offset
     */
    node->cinfo.wpos+=region_size*io_info->elmt_size;
    node->cinfo.wleft-=region_size;

    /* If we've read in all the elements from the block, throw it away */
    if(node->cinfo.wleft==0 && (node->cinfo.rleft==0 || node->cinfo.rleft==node->cinfo.size)) {
        /* Copy the location of the region in the file */
        HDmemcpy(file_offset, node->start, (io_info->space->extent.u.simple.rank * sizeof(hssize_t)));
        file_offset[io_info->space->extent.u.simple.rank]=0;

        /* Set the hyperslab size to write */
        for(i=0; i<io_info->space->extent.u.simple.rank; i++)
            hsize[i]=(node->end[i]-node->start[i])+1;
        hsize[io_info->space->extent.u.simple.rank]=io_info->elmt_size;

        if (H5F_arr_write (io_info->f, io_info->xfer_parms,
			   io_info->layout, io_info->pline,
			   io_info->fill, io_info->efl, hsize, hsize,
			   zero, file_offset, node->cinfo.block/*out*/)<0)
            HRETURN_ERROR (H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error");

        /* Release the temporary buffer */
        H5TB_release_buf(node->cinfo.block_id);

        /* Reset the caching flag for next time */
        node->cinfo.cached=0;
    } /* end if */

    FUNC_LEAVE (SUCCEED);
}   /* H5S_hyper_block_write() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_fread
 *
 * Purpose:	Recursively gathers data points from a file using the
 *		parameters passed to H5S_hyper_fgath.
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
static size_t
H5S_hyper_fread (intn dim, H5S_hyper_io_info_t *io_info)
{
    hsize_t region_size;                /* Size of lowest region */
    uintn parm_init=0;          /* Whether one-shot parameters set up */
    hid_t reg_id;               /* ID of temporary region buffer */
    H5S_hyper_region_t *regions;  /* Pointer to array of hyperslab nodes overlapped */
    size_t num_regions;         /* number of regions overlapped */
    size_t i;                   /* Counters */
    intn j;
    size_t num_read=0;          /* Number of elements read */

    FUNC_ENTER (H5S_hyper_fread, 0);

    assert(io_info);

#ifdef QAK
    printf("%s: check 1.0, dim=%d\n",FUNC,dim);
#endif /* QAK */

    /* Get a sorted list (in the next dimension down) of the regions which */
    /*  overlap the current index in this dim */
    if((reg_id=H5S_hyper_get_regions(&num_regions,dim,
            io_info->space->select.sel_info.hslab.hyper_lst->count,
            io_info->lo_bounds, io_info->hi_bounds,
            io_info->iter->hyp.pos,io_info->space->select.offset))>=0) {

        /* Get the pointer to the actual regions array */
        regions=H5TB_buf_ptr(reg_id);

        /*
	 * Check if this is the second to last dimension in dataset (Which
	 * means that we've got a list of the regions in the fastest changing
	 * dimension and should input those regions).
	 */
#ifdef QAK
	printf("%s: check 2.0, rank=%d, cache_hyper=%d\n",
	       FUNC,(int)io_info->space->extent.u.simple.rank,
           (int)io_info->xfer_parms->cache_hyper);
	for(i=0; i<num_regions; i++)
	    printf("%s: check 2.1, region #%d: start=%d, end=%d\n",
		   FUNC,i,(int)regions[i].start,(int)regions[i].end);
#endif /* QAK */
        if((dim+2)==io_info->space->extent.u.simple.rank) {
            /* perform I/O on data from regions */
            for(i=0; i<num_regions && io_info->nelmts>0; i++) {
                /* Compute the size of the region to read */
                region_size=MIN(io_info->nelmts,
				(regions[i].end-regions[i].start)+1);

                /* Check if this hyperslab block is cached or could be cached */
                if(!regions[i].node->cinfo.cached &&
		   (io_info->xfer_parms->cache_hyper &&
		    (io_info->xfer_parms->block_limit==0 ||
		     io_info->xfer_parms->block_limit>=(regions[i].node->cinfo.size*io_info->elmt_size)))) {
                    /* if we aren't cached, attempt to cache the block */
                    H5S_hyper_block_cache(regions[i].node,io_info,1);
                } /* end if */

                /* Read information from the cached block */
                if(regions[i].node->cinfo.cached) {
                    if(H5S_hyper_block_read(regions[i].node,io_info,region_size)<0)
                        HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, 0, "read error");
                }
                else {
                    /* Set up hyperslab I/O parameters which apply to all regions */
                    if(!parm_init) {
                        /* Copy the location of the region in the file */
                        HDmemcpy(io_info->offset,io_info->iter->hyp.pos,(io_info->space->extent.u.simple.rank * sizeof(hssize_t)));
                        io_info->offset[io_info->space->extent.u.simple.rank]=0;

                        /* Set flag */
                        parm_init=1;
                    } /* end if */

#ifdef QAK
    printf("%s: check 2.2, i=%d, region_size=%d\n",FUNC,(int)i,(int)region_size);
#endif /* QAK */
                    /* Fill in the region specific parts of the I/O request */
                    io_info->hsize[io_info->space->extent.u.simple.rank-1]=region_size;
                    io_info->offset[io_info->space->extent.u.simple.rank-1]=regions[i].start;

                    /*
                     * Gather from file.
                     */
                    if (H5F_arr_read (io_info->f, io_info->xfer_parms,
				      io_info->layout, io_info->pline,
				      io_info->fill, io_info->efl,
				      io_info->hsize, io_info->hsize, zero, io_info->offset,
				      io_info->dst/*out*/)<0) {
                        HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, 0,
				       "read error");
                    }
#ifdef QAK
    printf("%s: check 2.3, region #%d\n",FUNC,(int)i);
    for(j=0; j<io_info->space->extent.u.simple.rank; j++)
    printf("%s: %d - pos=%d\n", FUNC,j,(int)io_info->iter->hyp.pos[j]);
#endif /* QAK */
                } /* end else */

                /* Advance the pointer in the buffer */
                io_info->dst = ((uint8_t *)io_info->dst) +
				   region_size*io_info->elmt_size;

                /* Increment the number of elements read */
                num_read+=region_size;

                /* Decrement the buffer left */
                io_info->nelmts-=region_size;

                /* Set the next position to start at */
                if(region_size==(hsize_t)((regions[i].end-regions[i].start)+1))
                    io_info->iter->hyp.pos[dim+1]=(-1);
                else
                    io_info->iter->hyp.pos[dim+1] = regions[i].start + region_size;

                /* Decrement the iterator count */
                io_info->iter->hyp.elmt_left-=region_size;
            } /* end for */
        } else { /* recurse on each region to next dimension down */
#ifdef QAK
    printf("%s: check 3.0, num_regions=%d\n",FUNC,(int)num_regions);
    for(i=0; i<num_regions; i++)
        printf("%s: region %d={%d, %d}\n", FUNC,i,(int)regions[i].start,(int)regions[i].end);
#endif /* QAK */

            /* Increment the dimension we are working with */
            dim++;

            /* Step through each region in this dimension */
            for(i=0; i<num_regions && io_info->nelmts>0; i++) {
                /* Step through each location in each region */
                for(j=regions[i].start; j<=regions[i].end && io_info->nelmts>0; j++) {
#ifdef QAK
    printf("%s: check 4.0, dim=%d, location=%d\n",FUNC,dim,j);
#endif /* QAK */

                    /* Set the correct position we are working on */
                    io_info->iter->hyp.pos[dim]=j;

                    /* Go get the regions in the next lower dimension */
                    num_read+=H5S_hyper_fread(dim, io_info);

                    /* Advance to the next row if we got the whole region */
                    if(io_info->iter->hyp.pos[dim+1]==(-1))
                        io_info->iter->hyp.pos[dim]=j+1;
                } /* end for */
                if(j>regions[i].end && io_info->iter->hyp.pos[dim+1]==(-1))
                    io_info->iter->hyp.pos[dim]=(-1);
            } /* end for */
        } /* end else */

        /* Release the temporary buffer */
        H5TB_release_buf(reg_id);
    } /* end if */

    FUNC_LEAVE (num_read);
}   /* H5S_hyper_fread() */

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
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5S_hyper_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill,
		 const struct H5O_efl_t *efl, size_t elmt_size,
		 const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		 size_t nelmts, const H5F_xfer_t *xfer_parms,
		 void *_buf/*out*/)
{
    H5S_hyper_bound_t **lo_bounds;    /* Lower (closest to the origin) bound array for each dimension */
    H5S_hyper_bound_t **hi_bounds;    /* Upper (farthest from the origin) bound array for each dimension */
    H5S_hyper_io_info_t io_info;  /* Block of parameters to pass into recursive calls */
    intn	i;				/*counters		*/
    size_t  num_read;       /* number of elements read into buffer */

    FUNC_ENTER (H5S_hyper_fgath, 0);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (_buf);

#ifdef QAK
    printf("%s: check 1.0\n", FUNC);
#endif /* QAK */
    /* Allocate space for the low & high bound arrays */
    lo_bounds = H5MM_malloc(file_space->extent.u.simple.rank *
			    sizeof(H5S_hyper_bound_t *));
    hi_bounds = H5MM_malloc(file_space->extent.u.simple.rank *
			    sizeof(H5S_hyper_bound_t *));

    /* Initialize to correct order to walk through arrays.
        (When another iteration order besides the default 'C' order is chosen,
        this is the correct place to change the order of the array iterations)
    */
    for(i=0; i<file_space->extent.u.simple.rank; i++) {
        lo_bounds[i]=file_space->select.sel_info.hslab.hyper_lst->lo_bounds[i];
        hi_bounds[i]=file_space->select.sel_info.hslab.hyper_lst->hi_bounds[i];
    } /* end for */

    /* Initialize parameter block for recursive calls */
    io_info.f=f;
    io_info.layout=layout;
    io_info.pline=pline;
    io_info.fill=fill;
    io_info.efl=efl;
    io_info.elmt_size=elmt_size;
    io_info.space=file_space;
    io_info.iter=file_iter;
    io_info.nelmts=nelmts;
    io_info.xfer_parms=xfer_parms;
    io_info.src=NULL;
    io_info.dst=_buf;

    /* Set the hyperslab size to copy */
    io_info.hsize[0]=1;
    H5V_array_fill(io_info.hsize,io_info.hsize,sizeof(io_info.hsize[0]),file_space->extent.u.simple.rank);
    io_info.hsize[file_space->extent.u.simple.rank]=elmt_size;

    io_info.lo_bounds=lo_bounds;
    io_info.hi_bounds=hi_bounds;

    /* Recursively input the hyperslabs currently defined */
    /* starting with the slowest changing dimension */
#ifdef QAK
    printf("%s: check 4.0\n",FUNC);
#endif /* QAK */
    num_read=H5S_hyper_fread(-1,&io_info);
#ifdef QAK
    printf("%s: check 5.0, num_read=%d\n",FUNC,(int)num_read);
#endif /* QAK */

    /* Release the memory we allocated */
    H5MM_xfree(lo_bounds);
    H5MM_xfree(hi_bounds);
    
    FUNC_LEAVE (num_read);
} /* H5S_hyper_fgath() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_fwrite
 *
 * Purpose:	Recursively scatters data points to a file using the parameters
 *      passed to H5S_hyper_fscat.
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
static size_t
H5S_hyper_fwrite (intn dim, H5S_hyper_io_info_t *io_info)
{
    hsize_t region_size;                /* Size of lowest region */
    uintn parm_init=0;          /* Whether one-shot parameters set up */
    hid_t reg_id;               /* ID of temporary region buffer */
    H5S_hyper_region_t *regions;  /* Pointer to array of hyperslab nodes overlapped */
    size_t num_regions;         /* number of regions overlapped */
    size_t i;                   /* Counters */
    intn j;
    size_t num_written=0;          /* Number of elements read */

    FUNC_ENTER (H5S_hyper_fwrite, 0);

    assert(io_info);

#ifdef QAK
    printf("%s: check 1.0\n", FUNC);
#endif /* QAK */
    /* Get a sorted list (in the next dimension down) of the regions which */
    /*  overlap the current index in this dim */
    if((reg_id=H5S_hyper_get_regions(&num_regions,dim,
            io_info->space->select.sel_info.hslab.hyper_lst->count,
            io_info->lo_bounds, io_info->hi_bounds,
            io_info->iter->hyp.pos,io_info->space->select.offset))>=0) {

        /* Get the pointer to the actual regions array */
        regions=H5TB_buf_ptr(reg_id);
#ifdef QAK
    printf("%s: check 1.1, regions=%p\n", FUNC,regions);
	printf("%s: check 1.2, rank=%d\n",
	       FUNC,(int)io_info->space->extent.u.simple.rank);
	for(i=0; i<num_regions; i++)
	    printf("%s: check 2.1, region #%d: start=%d, end=%d\n",
		   FUNC,i,(int)regions[i].start,(int)regions[i].end);
#endif /* QAK */

        /* Check if this is the second to last dimension in dataset */
        /*  (Which means that we've got a list of the regions in the fastest */
        /*   changing dimension and should input those regions) */
        if((dim+2)==io_info->space->extent.u.simple.rank) {

            /* perform I/O on data from regions */
            for(i=0; i<num_regions && io_info->nelmts>0; i++) {
                /* Compute the size of the region to read */
                region_size=MIN(io_info->nelmts, (regions[i].end-regions[i].start)+1);

                /* Check if this hyperslab block is cached or could be cached */
                if(!regions[i].node->cinfo.cached && (io_info->xfer_parms->cache_hyper && (io_info->xfer_parms->block_limit==0 || io_info->xfer_parms->block_limit>=(regions[i].node->cinfo.size*io_info->elmt_size)))) {
                    /* if we aren't cached, attempt to cache the block */
                    H5S_hyper_block_cache(regions[i].node,io_info,0);
                } /* end if */

                /* Write information to the cached block */
                if(regions[i].node->cinfo.cached) {
                    if(H5S_hyper_block_write(regions[i].node,io_info,region_size)<0)
                        HRETURN_ERROR (H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
                }
                else {
                    /* Set up hyperslab I/O parameters which apply to all regions */
                    if(!parm_init) {
                        /* Copy the location of the region in the file */
                        HDmemcpy(io_info->offset, io_info->iter->hyp.pos, (io_info->space->extent.u.simple.rank * sizeof(hssize_t)));
                        io_info->offset[io_info->space->extent.u.simple.rank]=0;

                        /* Set flag */
                        parm_init=1;
                    } /* end if */

                    io_info->hsize[io_info->space->extent.u.simple.rank-1]=region_size;
                    io_info->offset[io_info->space->extent.u.simple.rank-1]=regions[i].start;

                    /*
                     * Scatter to file.
                     */
                    if (H5F_arr_write (io_info->f, io_info->xfer_parms,
				       io_info->layout, io_info->pline,
				       io_info->fill, io_info->efl,
				       io_info->hsize, io_info->hsize, zero, io_info->offset,
				       io_info->src)<0) {
                        HRETURN_ERROR (H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
                    }
                } /* end else */

                /* Advance the pointer in the buffer */
                io_info->src = ((const uint8_t *)io_info->src) +
				   region_size*io_info->elmt_size;

                /* Increment the number of elements read */
                num_written+=region_size;

                /* Decrement the buffer left */
                io_info->nelmts-=region_size;

                /* Set the next position to start at */
                if(region_size==(hsize_t)((regions[i].end-regions[i].start)+1))
                    io_info->iter->hyp.pos[dim+1]=(-1);
                else
                    io_info->iter->hyp.pos[dim+1] = regions[i].start +
							region_size;

                /* Decrement the iterator count */
                io_info->iter->hyp.elmt_left-=region_size;
            } /* end for */
        } else { /* recurse on each region to next dimension down */

            /* Increment the dimension we are working with */
            dim++;

            /* Step through each region in this dimension */
            for(i=0; i<num_regions && io_info->nelmts>0; i++) {
                /* Step through each location in each region */
                for(j=regions[i].start; j<=regions[i].end && io_info->nelmts>0; j++) {
                    /* Set the correct position we are working on */
                    io_info->iter->hyp.pos[dim]=j;

                    /* Go get the regions in the next lower dimension */
                    num_written+=H5S_hyper_fwrite(dim, io_info);

                    /* Advance to the next row if we got the whole region */
                    if(io_info->iter->hyp.pos[dim+1]==(-1))
                        io_info->iter->hyp.pos[dim]=j+1;
                } /* end for */
                if(j>regions[i].end && io_info->iter->hyp.pos[dim+1]==(-1))
                    io_info->iter->hyp.pos[dim]=(-1);
            } /* end for */
        } /* end else */

        /* Release the temporary buffer */
        H5TB_release_buf(reg_id);
    } /* end if */

#ifdef QAK
    printf("%s: check 2.0\n", FUNC);
#endif /* QAK */
    FUNC_LEAVE (num_written);
}   /* H5S_hyper_fwrite() */

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
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_hyper_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill,
		 const struct H5O_efl_t *efl, size_t elmt_size,
		 const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		 size_t nelmts, const H5F_xfer_t *xfer_parms,
		 const void *_buf)
{
    H5S_hyper_bound_t **lo_bounds;    /* Lower (closest to the origin) bound array for each dimension */
    H5S_hyper_bound_t **hi_bounds;    /* Upper (farthest from the origin) bound array for each dimension */
    H5S_hyper_io_info_t io_info;  /* Block of parameters to pass into recursive calls */
    intn	i;				/*counters		*/
    size_t  num_written;       /* number of elements read into buffer */

    FUNC_ENTER (H5S_hyper_fscat, 0);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (_buf);

#ifdef QAK
    printf("%s: check 1.0\n", FUNC);
#endif /* QAK */
    /* Allocate space for the low & high bound arrays */
    lo_bounds = H5MM_malloc(file_space->extent.u.simple.rank *
			    sizeof(H5S_hyper_bound_t *));
    hi_bounds = H5MM_malloc(file_space->extent.u.simple.rank *
			    sizeof(H5S_hyper_bound_t *));

    /*
     * Initialize to correct order to walk through arrays.  (When another
     * iteration order besides the default 'C' order is chosen, this is the
     * correct place to change the order of the array iterations)
     */
    for(i=0; i<file_space->extent.u.simple.rank; i++) {
        lo_bounds[i]=file_space->select.sel_info.hslab.hyper_lst->lo_bounds[i];
        hi_bounds[i]=file_space->select.sel_info.hslab.hyper_lst->hi_bounds[i];
    } /* end for */

    /* Initialize parameter block for recursive calls */
    io_info.f=f;
    io_info.layout=layout;
    io_info.pline=pline;
    io_info.fill=fill;
    io_info.efl=efl;
    io_info.elmt_size=elmt_size;
    io_info.space=file_space;
    io_info.iter=file_iter;
    io_info.nelmts=nelmts;
    io_info.xfer_parms=xfer_parms;
    io_info.src=_buf;
    io_info.dst=NULL;

    /* Set the hyperslab size to copy */
    io_info.hsize[0]=1;
    H5V_array_fill(io_info.hsize,io_info.hsize,sizeof(io_info.hsize[0]),file_space->extent.u.simple.rank);
    io_info.hsize[file_space->extent.u.simple.rank]=elmt_size;

    io_info.lo_bounds=lo_bounds;
    io_info.hi_bounds=hi_bounds;

    /* Recursively input the hyperslabs currently defined */
    /* starting with the slowest changing dimension */
    num_written=H5S_hyper_fwrite(-1,&io_info);

    /* Release the memory we allocated */
    H5MM_xfree(lo_bounds);
    H5MM_xfree(hi_bounds);
    
#ifdef QAK
    printf("%s: check 2.0\n", FUNC);
#endif /* QAK */
    FUNC_LEAVE (num_written >0 ? SUCCEED : FAIL);
} /* H5S_hyper_fscat() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_mread
 *
 * Purpose:	Recursively gathers data points from memory using the
 *		parameters passed to H5S_hyper_mgath.
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
static size_t
H5S_hyper_mread (intn dim, H5S_hyper_io_info_t *io_info)
{
    hsize_t region_size;                /* Size of lowest region */
    hid_t reg_id;               /* ID of temporary region buffer */
    H5S_hyper_region_t *regions;  /* Pointer to array of hyperslab nodes overlapped */
    size_t num_regions;         /* number of regions overlapped */
    size_t i;                   /* Counters */
    intn j;
    size_t num_read=0;          /* Number of elements read */

    FUNC_ENTER (H5S_hyper_mread, 0);

    assert(io_info);

#ifdef QAK
    printf("%s: check 1.0, dim=%d\n",FUNC,dim);
#endif /* QAK */

    /* Get a sorted list (in the next dimension down) of the regions which */
    /*  overlap the current index in this dim */
    if((reg_id=H5S_hyper_get_regions(&num_regions,dim,
            io_info->space->select.sel_info.hslab.hyper_lst->count,
            io_info->lo_bounds, io_info->hi_bounds,
            io_info->iter->hyp.pos,io_info->space->select.offset))>=0) {

        /* Get the pointer to the actual regions array */
        regions=H5TB_buf_ptr(reg_id);

        /* Check if this is the second to last dimension in dataset */
        /*  (Which means that we've got a list of the regions in the fastest */
        /*   changing dimension and should input those regions) */
#ifdef QAK
	printf("%s: check 2.0, rank=%d, num_regions=%d\n",
	       FUNC, (int)io_info->space->extent.u.simple.rank,
	       (int)num_regions);
	for(i=0; i<num_regions; i++)
	    printf("%s: check 2.1, region #%d: start=%d, end=%d\n",
		   FUNC,i,(int)regions[i].start,(int)regions[i].end);
#endif /* QAK */

        if((dim+2)==io_info->space->extent.u.simple.rank) {

            /* Set up hyperslab I/O parameters which apply to all regions */

            /* Copy the location of the region in the file */
            HDmemcpy(io_info->offset, io_info->iter->hyp.pos, (io_info->space->extent.u.simple.rank * sizeof(hssize_t)));
            io_info->offset[io_info->space->extent.u.simple.rank]=0;

            /* perform I/O on data from regions */
            for(i=0; i<num_regions && io_info->nelmts>0; i++) {
                region_size=MIN(io_info->nelmts,(regions[i].end-regions[i].start)+1);
                io_info->hsize[io_info->space->extent.u.simple.rank-1]=region_size;
                io_info->offset[io_info->space->extent.u.simple.rank-1]=regions[i].start;
#ifdef QAK
		printf("%s: check 2.1, i=%d, region_size=%d\n",
		       FUNC,(int)i,(int)region_size);
#endif /* QAK */

                /*
                 * Gather from memory.
                 */
                if (H5V_hyper_copy (io_info->space->extent.u.simple.rank+1,
                        io_info->hsize, io_info->hsize, zero, io_info->dst,
                        io_info->mem_size, io_info->offset, io_info->src)<0) {
                    HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, 0,
				   "unable to gather data from memory");
                }

                /* Advance the pointer in the buffer */
                io_info->dst = ((uint8_t *)io_info->dst) + region_size*io_info->elmt_size;

                /* Increment the number of elements read */
                num_read+=region_size;

                /* Decrement the buffer left */
                io_info->nelmts-=region_size;

                /* Set the next position to start at */
                if(region_size==(hsize_t)((regions[i].end-regions[i].start)+1))
                    io_info->iter->hyp.pos[dim+1]=(-1);
                else
                    io_info->iter->hyp.pos[dim+1] =regions[i].start +
						       region_size;

                /* Decrement the iterator count */
                io_info->iter->hyp.elmt_left-=region_size;
            } /* end for */
        } else { /* recurse on each region to next dimension down */
#ifdef QAK
	    printf("%s: check 3.0, num_regions=%d\n",FUNC,(int)num_regions);
#endif /* QAK */

            /* Increment the dimension we are working with */
            dim++;

            /* Step through each region in this dimension */
            for(i=0; i<num_regions && io_info->nelmts>0; i++) {
                /* Step through each location in each region */
                for(j=regions[i].start; j<=regions[i].end && io_info->nelmts>0; j++) {
#ifdef QAK
		    printf("%s: check 4.0, dim=%d, location=%d\n",FUNC,dim,j);
#endif /* QAK */

                    /*
                     * If we are moving to a new position in this dim, reset
                     * the next lower dim. location.
                     */
                    if(io_info->iter->hyp.pos[dim]!=j)
                        io_info->iter->hyp.pos[dim+1]=(-1);

                    /* Set the correct position we are working on */
                    io_info->iter->hyp.pos[dim]=j;

                    /* Go get the regions in the next lower dimension */
                    num_read+=H5S_hyper_mread(dim, io_info);
                } /* end for */
            } /* end for */
        } /* end else */

        /* Release the temporary buffer */
        H5TB_release_buf(reg_id);
    } /* end if */

    FUNC_LEAVE (num_read);
}   /* H5S_hyper_mread() */

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
static size_t
H5S_hyper_mgath (const void *_buf, size_t elmt_size,
		 const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		 size_t nelmts, void *_tconv_buf/*out*/)
{
    H5S_hyper_bound_t **lo_bounds;    /* Lower (closest to the origin) bound array for each dimension */
    H5S_hyper_bound_t **hi_bounds;    /* Upper (farthest from the origin) bound array for each dimension */
    H5S_hyper_io_info_t io_info;  /* Block of parameters to pass into recursive calls */
    intn	i;
#ifdef QAK
    intn    j;            /* Counters		*/
#endif /* QAK */
    size_t  num_read;       /* number of elements read into buffer */

    FUNC_ENTER (H5S_hyper_mgath, 0);

#ifdef QAK
    printf("%s: check 1.0, elmt_size=%d, mem_space=%p\n",
	   FUNC,(int)elmt_size,mem_space);
    printf("%s: check 1.0, mem_iter=%p, nelmts=%d\n",FUNC,mem_iter,nelmts);
    printf("%s: check 1.0, _buf=%p, _tconv_buf=%p\n",FUNC,_buf,_tconv_buf);
#endif /* QAK */

    /* Check args */
    assert (elmt_size>0);
    assert (mem_space);
    assert (mem_iter);
    assert (nelmts>0);
    assert (_buf);
    assert (_tconv_buf);

#ifdef QAK
    printf("%s: check 2.0, mem_space->extent.u.simple.rank=%d\n",
	   FUNC, (int)mem_space->extent.u.simple.rank);
#endif /* QAK */

    /* Allocate space for the low & high bound arrays */
    lo_bounds = H5MM_malloc(mem_space->extent.u.simple.rank *
			    sizeof(H5S_hyper_bound_t *));
    hi_bounds = H5MM_malloc(mem_space->extent.u.simple.rank *
			    sizeof(H5S_hyper_bound_t *));

    /*
     * Initialize to correct order to walk through arrays.  (When another
     * iteration order besides the default 'C' order is chosen, this is the
     * correct place to change the order of the array iterations)
     */
#ifdef QAK
    printf("%s: check 3.0\n",FUNC);
#endif /* QAK */
    for(i=0; i<mem_space->extent.u.simple.rank; i++) {
        lo_bounds[i]=mem_space->select.sel_info.hslab.hyper_lst->lo_bounds[i];
        hi_bounds[i]=mem_space->select.sel_info.hslab.hyper_lst->hi_bounds[i];
#ifdef QAK
	printf("%s: check 3.1, lo[%d]=%p, hi[%d]=%p\n",
	       FUNC,i,lo_bounds[i],i,hi_bounds[i]);
        for(j=0; j<(int)mem_space->select.sel_info.hslab.hyper_lst->count; j++)
	    printf("%s: check 3.2, lo[%d][%d]=%d, hi[%d][%d]=%d\n",
		   FUNC, i, j, (int)lo_bounds[i][j].bound, i, j,
		   (int)hi_bounds[i][j].bound);
#endif /* QAK */
    } /* end for */

    /* Initialize parameter block for recursive calls */
    io_info.elmt_size=elmt_size;
    io_info.space=mem_space;
    io_info.iter=mem_iter;
    io_info.nelmts=nelmts;
    io_info.src=_buf;
    io_info.dst=_tconv_buf;

    /* Set up the size of the memory space */
    HDmemcpy(io_info.mem_size, mem_space->extent.u.simple.size,mem_space->extent.u.simple.rank*sizeof(hsize_t));
    io_info.mem_size[mem_space->extent.u.simple.rank]=elmt_size;

    /* Set the hyperslab size to copy */
    io_info.hsize[0]=1;
    H5V_array_fill(io_info.hsize, io_info.hsize, sizeof(io_info.hsize[0]),mem_space->extent.u.simple.rank);
    io_info.hsize[mem_space->extent.u.simple.rank]=elmt_size;

    io_info.lo_bounds=lo_bounds;
    io_info.hi_bounds=hi_bounds;

    /* Recursively input the hyperslabs currently defined */
    /* starting with the slowest changing dimension */
#ifdef QAK
    printf("%s: check 4.0\n",FUNC);
#endif /* QAK */
    num_read=H5S_hyper_mread(-1,&io_info);
#ifdef QAK
    printf("%s: check 5.0, num_read=%d\n",FUNC,(int)num_read);
#endif /* QAK */

    /* Release the memory we allocated */
    H5MM_xfree(lo_bounds);
    H5MM_xfree(hi_bounds);

    FUNC_LEAVE (num_read);
}   /* H5S_hyper_mgath() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_mwrite
 *
 * Purpose:	Recursively scatters data points from memory using the parameters
 *      passed to H5S_hyper_mscat.
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
static size_t
H5S_hyper_mwrite (intn dim, H5S_hyper_io_info_t *io_info)
{
    hsize_t region_size;                /* Size of lowest region */
    hid_t reg_id;               /* ID of temporary region buffer */
    H5S_hyper_region_t *regions;  /* Pointer to array of hyperslab nodes overlapped */
    size_t num_regions;         /* number of regions overlapped */
    size_t i;                   /* Counters */
    intn j;
    size_t num_read=0;          /* Number of elements read */

    FUNC_ENTER (H5S_hyper_mwrite, 0);

    assert(io_info);
#ifdef QAK
    printf("%s: check 1.0\n",FUNC);
#endif /* QAK */

    /* Get a sorted list (in the next dimension down) of the regions which */
    /*  overlap the current index in this dim */
    if((reg_id=H5S_hyper_get_regions(&num_regions,dim,
            io_info->space->select.sel_info.hslab.hyper_lst->count,
            io_info->lo_bounds, io_info->hi_bounds,
            io_info->iter->hyp.pos,io_info->space->select.offset))>=0) {

        /* Get the pointer to the actual regions array */
        regions=H5TB_buf_ptr(reg_id);

#ifdef QAK
	printf("%s: check 2.0, rank=%d\n",
	       FUNC,(int)io_info->space->extent.u.simple.rank);
	for(i=0; i<num_regions; i++)
	    printf("%s: check 2.1, region #%d: start=%d, end=%d\n",
		   FUNC,i,(int)regions[i].start,(int)regions[i].end);
#endif /* QAK */
        /* Check if this is the second to last dimension in dataset */
        /*  (Which means that we've got a list of the regions in the fastest */
        /*   changing dimension and should input those regions) */
        if((dim+2)==io_info->space->extent.u.simple.rank) {

            /* Set up hyperslab I/O parameters which apply to all regions */

            /* Copy the location of the region in the file */
            HDmemcpy(io_info->offset, io_info->iter->hyp.pos, (io_info->space->extent.u.simple.rank* sizeof(hssize_t)));
            io_info->offset[io_info->space->extent.u.simple.rank]=0;

#ifdef QAK
	    printf("%s: check 3.0\n",FUNC);
#endif /* QAK */
            /* perform I/O on data from regions */
            for(i=0; i<num_regions && io_info->nelmts>0; i++) {
                region_size=MIN(io_info->nelmts, (regions[i].end-regions[i].start)+1);
                io_info->hsize[io_info->space->extent.u.simple.rank-1]=region_size;
                io_info->offset[io_info->space->extent.u.simple.rank-1]=regions[i].start;

                /*
                 * Gather from memory.
                 */
                if (H5V_hyper_copy (io_info->space->extent.u.simple.rank+1,
				    io_info->hsize, io_info->mem_size, io_info->offset,
				    io_info->dst, io_info->hsize, zero,
				    io_info->src)<0) {
                    HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, 0, "unable to gather data from memory");
                }

                /* Advance the pointer in the buffer */
                io_info->src = ((const uint8_t *)io_info->src) +
				   region_size*io_info->elmt_size;

                /* Increment the number of elements read */
                num_read+=region_size;

                /* Decrement the buffer left */
                io_info->nelmts-=region_size;

                /* Set the next position to start at */
                if(region_size==(hsize_t)((regions[i].end-regions[i].start)+1))
                    io_info->iter->hyp.pos[dim+1]=(-1);
                else
                    io_info->iter->hyp.pos[dim+1] = regions[i].start +
							region_size;

                /* Decrement the iterator count */
                io_info->iter->hyp.elmt_left-=region_size;
            } /* end for */
        } else { /* recurse on each region to next dimension down */

            /* Increment the dimension we are working with */
            dim++;

#ifdef QAK
	    printf("%s: check 6.0, num_regions=%d\n",FUNC,(int)num_regions);
#endif /* QAK */
            /* Step through each region in this dimension */
            for(i=0; i<num_regions && io_info->nelmts>0; i++) {
                /* Step through each location in each region */
#ifdef QAK
		printf("%s: check 7.0, start[%d]=%d, end[%d]=%d, nelmts=%d\n",
		       FUNC, i, (int)regions[i].start, i,
		       (int)regions[i].end, (int)io_info->nelmts);
#endif /* QAK */
                for(j=regions[i].start;
		    j<=regions[i].end && io_info->nelmts>0;
		    j++) {

                    /*
		     * If we are moving to a new position in this dim, reset
		     * the next lower dim. location.
		     */
                    if(io_info->iter->hyp.pos[dim]!=j)
                        io_info->iter->hyp.pos[dim+1]=(-1);

                    /* Set the correct position we are working on */
                    io_info->iter->hyp.pos[dim]=j;

                    /* Go get the regions in the next lower dimension */
                    num_read+=H5S_hyper_mwrite(dim, io_info);
                } /* end for */
            } /* end for */
        } /* end else */

        /* Release the temporary buffer */
        H5TB_release_buf(reg_id);
    } /* end if */

    FUNC_LEAVE (num_read);
}   /* H5S_hyper_mwrite() */

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
		 size_t nelmts, void *_buf/*out*/)
{
    H5S_hyper_bound_t **lo_bounds;    /* Lower (closest to the origin) bound array for each dimension */
    H5S_hyper_bound_t **hi_bounds;    /* Upper (farthest from the origin) bound array for each dimension */
    H5S_hyper_io_info_t io_info;  /* Block of parameters to pass into recursive calls */
    intn	i;				/*counters		*/
    size_t  num_read;       /* number of elements read into buffer */

    FUNC_ENTER (H5S_hyper_mscat, 0);

    /* Check args */
    assert (elmt_size>0);
    assert (mem_space);
    assert (mem_iter);
    assert (nelmts>0);
    assert (_buf);
    assert (_tconv_buf);

    /* Allocate space for the low & high bound arrays */
    lo_bounds = H5MM_malloc(mem_space->extent.u.simple.rank *
			    sizeof(H5S_hyper_bound_t *));
    hi_bounds = H5MM_malloc(mem_space->extent.u.simple.rank *
			    sizeof(H5S_hyper_bound_t *));

    /*
     * Initialize to correct order to walk through arrays.  (When another
     * iteration order besides the default 'C' order is chosen, this is the
     * correct place to change the order of the array iterations)
     */
    for(i=0; i<mem_space->extent.u.simple.rank; i++) {
        lo_bounds[i]=mem_space->select.sel_info.hslab.hyper_lst->lo_bounds[i];
        hi_bounds[i]=mem_space->select.sel_info.hslab.hyper_lst->hi_bounds[i];
    } /* end for */

    /* Initialize parameter block for recursive calls */
    io_info.elmt_size=elmt_size;
    io_info.space=mem_space;
    io_info.iter=mem_iter;
    io_info.nelmts=nelmts;
    io_info.src=_tconv_buf;
    io_info.dst=_buf;

    /* Set up the size of the memory space */
    HDmemcpy(io_info.mem_size, mem_space->extent.u.simple.size,mem_space->extent.u.simple.rank*sizeof(hsize_t));
    io_info.mem_size[mem_space->extent.u.simple.rank]=elmt_size;

    /* Set the hyperslab size to copy */
    io_info.hsize[0]=1;
    H5V_array_fill(io_info.hsize, io_info.hsize, sizeof(io_info.hsize[0]), mem_space->extent.u.simple.rank);
    io_info.hsize[mem_space->extent.u.simple.rank]=elmt_size;

    io_info.lo_bounds=lo_bounds;
    io_info.hi_bounds=hi_bounds;

    /* Recursively input the hyperslabs currently defined */
    /* starting with the slowest changing dimension */
#ifdef QAK
    printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    num_read=H5S_hyper_mwrite(-1,&io_info);
#ifdef QAK
    printf("%s: check 2.0\n",FUNC);
#endif /* QAK */

    /* Release the memory we allocated */
    H5MM_xfree(lo_bounds);
    H5MM_xfree(hi_bounds);

    FUNC_LEAVE (num_read>0 ? SUCCEED : FAIL);
}   /* H5S_hyper_mscat() */

/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_bsearch
 PURPOSE
    Search for a boundary
 USAGE
    herr_t H5S_hyper_bsearch(key,barr,count)
        hssize_t size;              IN: Key we are searching for
        H5S_hyper_bount_t *barr;    IN: Pointer to the array of bounds
        size_t count;               IN: Number of elements in the bound array
 RETURNS
    The element number to insert in front of on success (the value in the 'count'
    parameter if the new bound should be added to end) or negative on failure.
 DESCRIPTION
    Finds the proper place to insert a boundary in a sorted boundary array.
    Uses a binary search algorithm for the actual searching.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static intn
H5S_hyper_bsearch(hssize_t size, H5S_hyper_bound_t *barr, size_t count)
{
    size_t lo, mid, hi;       /* Indices for the search */
    intn ret_value=-1;      /* Return value index */

    FUNC_ENTER (H5S_hyper_bsearch, FAIL);

    assert(barr);
    assert(count>0);

    /* Check bounds first */
    if(size<barr[0].bound)
        ret_value=0;
    else if(size>barr[count-1].bound)
        ret_value=(intn)count;
    else {      /* must be in the middle somewhere, go get it */
        lo=0;
        hi=count-1;
        do {
            /* Calc. the mid-point */
            mid=(hi+lo)/2;

            /* check for bounds only seperated by one element */
            if((hi-lo)<=1) {
                ret_value=(intn)hi;
                break;
            } else {    /* Divide and conquer! */
                if(size>barr[mid].bound)
                    lo=mid;
                else
                    hi=mid;
            } /* end else */
        } while(lo!=hi);
    } /* end else */
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_bsearch() */

/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_node_add
 PURPOSE
    Add a new node to a list of hyperslab nodes
 USAGE
    herr_t H5S_hyper_node_add(head, start, size)
        H5S_hyper_node_t *head;   IN: Pointer to head of hyperslab list
        intn endflag;             IN: "size" array actually contains "end" array
        intn rank;                IN: # of dimensions of the node
        const hssize_t *start;    IN: Offset of block
        const hsize_t *size;      IN: Size of block
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Adds a new hyperslab node to a list of them.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_hyper_node_add (H5S_hyper_node_t **head, intn endflag, intn rank, const hssize_t *start, const hsize_t *size)
{
    H5S_hyper_node_t *slab;     /* New hyperslab node to add */
    intn i;     /* Counters */
    herr_t ret_value=SUCCEED;

    FUNC_ENTER (H5S_hyper_node_add, FAIL);

    /* Check args */
    assert (head);
    assert (start);
    assert (size);

    /* Create new hyperslab node to insert */
    if((slab = H5MM_malloc(sizeof(H5S_hyper_node_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab node");
    if((slab->start = H5MM_malloc(sizeof(hsize_t)* rank))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab start boundary");
    if((slab->end = H5MM_malloc(sizeof(hsize_t)* rank))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab end boundary");

    /* Set boundary on new node */
    for(i=0; i<rank; i++) {
        slab->start[i]=start[i];
        if(endflag)
            slab->end[i]=size[i];
        else
            slab->end[i]=start[i]+size[i]-1;
    } /* end for */

    /* Prepend on list of hyperslabs for this selection */
    slab->next=*head;
    *head=slab;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_node_add() */

/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_node_prepend
 PURPOSE
    Prepend an existing node to an existing list of hyperslab nodes
 USAGE
    herr_t H5S_hyper_node_prepend(head, node)
        H5S_hyper_node_t **head;  IN: Pointer to pointer to head of hyperslab list
        H5S_hyper_node_t *node;   IN: Pointer to node to prepend
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Prepends an existing hyperslab node to a list of them.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_node_prepend (H5S_hyper_node_t **head, H5S_hyper_node_t *node)
{
    herr_t ret_value=SUCCEED;

    FUNC_ENTER (H5S_hyper_node_prepend, FAIL);

    /* Check args */
    assert (head);
    assert (node);

    /* Prepend on list of hyperslabs for this selection */
    node->next=*head;
    *head=node;

    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_node_prepend() */

/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_node_release
 PURPOSE
    Free the memory for a hyperslab node
 USAGE
    herr_t H5S_hyper_node_release(node)
        H5S_hyper_node_t *node;   IN: Pointer to node to free
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Frees a hyperslab node.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_hyper_node_release (H5S_hyper_node_t *node)
{
    herr_t ret_value=SUCCEED;

    FUNC_ENTER (H5S_hyper_node_release, FAIL);

    /* Check args */
    assert (node);

    /* Free the hyperslab node */
    node->start = H5MM_xfree(node->start);
    node->end = H5MM_xfree(node->end);
    H5MM_xfree(node);

    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_node_release() */

/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_add
 PURPOSE
    Add a block to hyperslab selection
 USAGE
    herr_t H5S_hyper_add(space, start, size)
        H5S_t *space;       	  IN: Pointer to dataspace
        const hssize_t *start;    IN: Offset of block
        const hsize_t *end;       IN: Offset of end of block
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Adds a block to an existing hyperslab selection.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_hyper_add (H5S_t *space, const hssize_t *start, const hsize_t *end)
{
    H5S_hyper_node_t *slab;     /* New hyperslab node to insert */
    H5S_hyper_bound_t *tmp;     /* Temporary pointer to an hyperslab bound array */
    intn bound_loc;             /* Boundary location to insert hyperslab */
    size_t elem_count;          /* Number of elements in hyperslab selection */
    intn i;     /* Counters */
    herr_t ret_value=SUCCEED;

    FUNC_ENTER (H5S_hyper_add, FAIL);

    /* Check args */
    assert (space);
    assert (start);
    assert (end);


#ifdef QAK
    printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    /* Create new hyperslab node to insert */
    if((slab = H5MM_malloc(sizeof(H5S_hyper_node_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab node");
    if((slab->start = H5MM_malloc(sizeof(hsize_t)*space->extent.u.simple.rank))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab start boundary");
    if((slab->end = H5MM_malloc(sizeof(hsize_t)*space->extent.u.simple.rank))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab end boundary");

#ifdef QAK
    printf("%s: check 2.0\n",FUNC);
#endif /* QAK */
    /* Set boundary on new node */
    for(i=0,elem_count=1; i<space->extent.u.simple.rank; i++) {
#ifdef QAK
	printf("%s: check 2.1, %d: start=%d, end=%d, elem_count=%d\n",
	       FUNC,(int)i,(int)start[i],(int)end[i],(int)elem_count);
#endif /* QAK */
        slab->start[i]=start[i];
        slab->end[i]=end[i];
        elem_count*=(end[i]-start[i])+1;
    } /* end for */

    /* Initialize caching parameters */
    slab->cinfo.cached=0;
    slab->cinfo.size=elem_count;
    slab->cinfo.wleft=slab->cinfo.rleft=0;
    slab->cinfo.block_id=(-1);
    slab->cinfo.block=slab->cinfo.wpos=slab->cinfo.rpos=NULL;

#ifdef QAK
    printf("%s: check 3.0, lo_bounds=%p, hi_bounds=%p\n",
	   FUNC, space->select.sel_info.hslab.hyper_lst->lo_bounds,
	   space->select.sel_info.hslab.hyper_lst->hi_bounds);
#endif /* QAK */
    /* Increase size of boundary arrays for dataspace's selection */
    for(i=0; i<space->extent.u.simple.rank; i++) {
        tmp=space->select.sel_info.hslab.hyper_lst->lo_bounds[i];
#ifdef QAK
	printf("%s: check 3.1, i=%d, space->sel_info.count=%d, tmp=%p\n",FUNC,(int)i, space->select.sel_info.hslab.hyper_lst->count,tmp);
#endif /* QAK */
        if((space->select.sel_info.hslab.hyper_lst->lo_bounds[i]=H5MM_realloc(tmp,sizeof(H5S_hyper_bound_t)*(space->select.sel_info.hslab.hyper_lst->count+1)))==NULL) {
            space->select.sel_info.hslab.hyper_lst->lo_bounds[i]=tmp;
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate hyperslab lo boundary array");
        } /* end if */
#ifdef QAK
	printf("%s: check 3.2, i=%d\n",FUNC,(int)i);
#endif /* QAK */
        tmp=space->select.sel_info.hslab.hyper_lst->hi_bounds[i];
        if((space->select.sel_info.hslab.hyper_lst->hi_bounds[i]=H5MM_realloc(tmp,sizeof(H5S_hyper_bound_t)*(space->select.sel_info.hslab.hyper_lst->count+1)))==NULL) {
            space->select.sel_info.hslab.hyper_lst->hi_bounds[i]=tmp;
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate hyperslab hi boundary array");
        } /* end if */
    } /* end for */

#ifdef QAK
    printf("%s: check 4.0\n",FUNC);
    {
        intn j;
        
        for(i=0; i<space->extent.u.simple.rank; i++) {
            for(j=0; j<(int)space->select.sel_info.hslab.hyper_lst->count; j++) {
		printf("%s: lo_bound[%d][%d]=%d(%p), "
		       "hi_bound[%d][%d]=%d(%p)\n",FUNC,
        i,j,(int)space->select.sel_info.hslab.hyper_lst->lo_bounds[i][j].bound,
            space->select.sel_info.hslab.hyper_lst->lo_bounds[i][j].node,
        i,j,(int)space->select.sel_info.hslab.hyper_lst->hi_bounds[i][j].bound,
            space->select.sel_info.hslab.hyper_lst->hi_bounds[i][j].node);
            }
        }
    }
#endif /* QAK */
    /* Insert each boundary of the hyperslab into the sorted lists of bounds */
    for(i=0; i<space->extent.u.simple.rank; i++) {
        /* Check if this is the first hyperslab inserted */
        if(space->select.sel_info.hslab.hyper_lst->count==0) {
#ifdef QAK
	    printf("%s: check 4.1, start[%d]=%d, end[%d]=%d\n",
		   FUNC, i, (int)slab->start[i],i,(int)slab->end[i]);
	    printf("%s: check 4.1,.hslab.hyper_lst->count=%d\n",
		   FUNC,(int)space->select.sel_info.hslab.hyper_lst->count);
#endif /* QAK */
            space->select.sel_info.hslab.hyper_lst->lo_bounds[i][0].bound=slab->start[i];
            space->select.sel_info.hslab.hyper_lst->lo_bounds[i][0].node=slab;
            space->select.sel_info.hslab.hyper_lst->hi_bounds[i][0].bound=slab->end[i];
            space->select.sel_info.hslab.hyper_lst->hi_bounds[i][0].node=slab;
        } /* end if */
        else {
#ifdef QAK
	    printf("%s: check 4.3, start[%d]=%d, end[%d]=%d\n",
		   FUNC,i,(int)slab->start[i],i,(int)slab->end[i]);
	    printf("%s: check 4.3,.hslab.hyper_lst->count=%d\n",
		   FUNC,(int)space->select.sel_info.hslab.hyper_lst->count);
#endif /* QAK */
            /* Take care of the low boundary first */
            /* Find the location to insert in front of */
            if((bound_loc=H5S_hyper_bsearch(slab->start[i],space->select.sel_info.hslab.hyper_lst->lo_bounds[i],
                    space->select.sel_info.hslab.hyper_lst->count))<0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                    "can't find location to insert hyperslab boundary");

#ifdef QAK
	    printf("%s: check 4.5, bound_loc=%d\n",FUNC,(int)bound_loc);
#endif /* QAK */
            /* Check if we need to move boundary elements */
            if(bound_loc!=(intn)space->select.sel_info.hslab.hyper_lst->count) {
                HDmemmove(&space->select.sel_info.hslab.hyper_lst->lo_bounds[i][bound_loc+1],
                    &space->select.sel_info.hslab.hyper_lst->lo_bounds[i][bound_loc],
                    sizeof(H5S_hyper_bound_t)*(space->select.sel_info.hslab.hyper_lst->count-bound_loc));
            } /* end if */
            space->select.sel_info.hslab.hyper_lst->lo_bounds[i][bound_loc].bound=slab->start[i];
            space->select.sel_info.hslab.hyper_lst->lo_bounds[i][bound_loc].node=slab;

            /* Take care of the high boundary next */
            /* Find the location to insert in front of */
            if((bound_loc=H5S_hyper_bsearch(slab->end[i],space->select.sel_info.hslab.hyper_lst->hi_bounds[i],
                    space->select.sel_info.hslab.hyper_lst->count))<0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                    "can't find location to insert hyperslab boundary");

            /* Check if we need to move boundary elements */
            if(bound_loc!=(intn)space->select.sel_info.hslab.hyper_lst->count) {
                HDmemmove(&space->select.sel_info.hslab.hyper_lst->hi_bounds[i][bound_loc+1],
                    &space->select.sel_info.hslab.hyper_lst->hi_bounds[i][bound_loc],
                    sizeof(H5S_hyper_bound_t)*(space->select.sel_info.hslab.hyper_lst->count-bound_loc));
            } /* end if */
            space->select.sel_info.hslab.hyper_lst->hi_bounds[i][bound_loc].bound=slab->end[i];
            space->select.sel_info.hslab.hyper_lst->hi_bounds[i][bound_loc].node=slab;
        } /* end else */
    } /* end for */

    /* Increment the number of bounds in the array */
    space->select.sel_info.hslab.hyper_lst->count++;
#ifdef QAK
    printf("%s: check 5.0, count=%d\n",FUNC,(int)space->select.sel_info.hslab.hyper_lst->count);
#endif /* QAK */
    
    /* Prepend on list of hyperslabs for this selection */
    slab->next=space->select.sel_info.hslab.hyper_lst->head;
    space->select.sel_info.hslab.hyper_lst->head=slab;

    /* Increment the number of elements in the hyperslab selection */
    space->select.num_elem+=elem_count;
#ifdef QAK
    printf("%s: check 6.0, elem_count=%d\n",FUNC,(int)elem_count);
    {
        intn j;
        
        for(i=0; i<space->extent.u.simple.rank; i++) {
            for(j=0; j<(int)space->select.sel_info.hslab.hyper_lst->count; j++) {
                printf("%s: lo_bound[%d][%d]=%d, hi_bound[%d][%d]=%d\n", FUNC,
                    i,j,(int)space->select.sel_info.hslab.hyper_lst->lo_bounds[i][j].bound,
                    i,j,(int)space->select.sel_info.hslab.hyper_lst->hi_bounds[i][j].bound);
            }
        }
    }
#endif /* QAK */

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_add() */

/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_clip
 PURPOSE
    Clip a list of nodes against the current selection
 USAGE
    herr_t H5S_hyper_clip(space, nodes, uniq, overlap)
        H5S_t *space;       	  IN: Pointer to dataspace
        H5S_hyper_node_t *nodes;  IN: Pointer to list of nodes
        H5S_hyper_node_t **uniq;  IN: Handle to list of non-overlapping nodes
        H5S_hyper_node_t **overlap;  IN: Handle to list of overlapping nodes
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Clips a list of hyperslab nodes against the current hyperslab selection.
    The list of non-overlapping and overlapping nodes which are generated from
    this operation are returned in the 'uniq' and 'overlap' pointers.  If
    either of those lists are not needed, they may be set to NULL and the
    list will be released.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Clipping a multi-dimensional space against another multi-dimensional
    space generates at most 1 overlapping region and 2*<rank> non-overlapping
    regions, falling into the following categories in each dimension:
        Case 1 - A overlaps B on both sides:
            node            <----AAAAAAAA--->
                clipped against:
            existing        <-----BBBBB----->
                generates:
            overlapping     <-----CCCCC----->
            non-overlapping <----D---------->
            non-overlapping <----------EE--->

        Case 2 - A overlaps B on one side: (need to check both sides!)
            Case 2a:
                node            <------AAAAAA--->
                    clipped against:
                existing        <-----BBBBB----->
                    generates:
                overlapping     <------CCCC----->
                non-overlapping <----------EE--->
            Case 2b:
                node            <---AAAAA------->
                    clipped against:
                existing        <-----BBBBB----->
                    generates:
                overlapping     <-----CCC------->
                non-overlapping <---EE---------->

        Case 3 - A is entirely within B:
            node            <------AA------->
                clipped against:
            existing        <-----BBBBB----->
                generates:
            overlapping     <------CC------->

        Case 4 - A is entirely outside B: (doesn't matter which side)
            node            <-----------AAA->
                clipped against:
            existing        <-----BBBBB----->
                generates:
            non-overlapping <-----------AAA->

    This algorithm could be sped up by keeping track of the last (existing)
    region the new node was compared against when it was split and resume
    comparing against the region following that one when it's returned to
    later (for non-overlapping blocks).

    Another optimization is to build a n-tree (not certain about how many
    times each dimension should be cut, but at least once) for the dataspace
    and build a list of existing blocks which overlap each "n"-tant and only
    compare the new nodes against existing node in the region of the n-tree
    which the are located in.

 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_hyper_clip (H5S_t *space, H5S_hyper_node_t *nodes, H5S_hyper_node_t **uniq,
        H5S_hyper_node_t **overlap)
{
    H5S_hyper_node_t *region,   /* Temp. hyperslab selection region pointer */
        *node,                  /* Temp. hyperslab node pointer */
        *next_node;             /* Pointer to next node in node list */
    hssize_t *start;            /* Temporary arrays of start & sizes (for splitting nodes) */
    hsize_t *end=NULL;          /* Temporary arrays of start & sizes (for splitting nodes) */
    intn rank;                  /* Cached copy of the rank of the dataspace */
    intn overlapped;            /* Flag for overlapping nodes */
    intn non_intersect;         /* Flag for non-intersecting nodes */
    intn i;     /* Counters */
    enum               /* Cases for edge overlaps */
        {OVERLAP_BOTH,OVERLAP_LOWER,OVERLAP_UPPER,WITHIN,NO_OVERLAP} clip_case;
    herr_t ret_value=SUCCEED;

    FUNC_ENTER (H5S_hyper_clip, FAIL);

    /* Check args */
    assert (space);
    assert (nodes);
    assert (uniq || overlap);

    /* Allocate space for the temporary starts & sizes */
    if((start = H5MM_malloc(sizeof(hssize_t)*space->extent.u.simple.rank))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab start array");
    if((end = H5MM_malloc(sizeof(hsize_t)*space->extent.u.simple.rank))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab size array");

    /* Set up local variables */
    rank=space->extent.u.simple.rank;

    /*
     * Cycle through all the hyperslab nodes, clipping them against the 
     * existing hyperslab selection.
     */
    node=nodes;
    while(node!=NULL) {
        /* Remove current node from head of list to evaulate it */
        next_node=node->next;   /* retain next node in list */
        if(nodes==node)
            nodes=nodes->next;  /* Move head of list */
        node->next=NULL;    /* just to be safe */

        overlapped=0;       /* Reset overlapped flag */
        region=space->select.sel_info.hslab.hyper_lst->head;
        while(region!=NULL && overlapped==0) {
            /* Check for intersection */
            for(i=0, non_intersect=0; i<rank && non_intersect==0; i++) {
                if(node->end[i]<region->start[i] || node->start[i]>region->end[i])
                    non_intersect=1;
            } /* end for */

            /* Only compare node with regions that actually intersect */
            if(non_intersect==0) {
                /* Compare the boundaries of the two objects in each dimension */
                for(i=0; i<rank && overlapped==0; i++) {
                    /* Find overlap case we are in */

                    /* True if case 1, 4 or 2b */
                    if(node->start[i]<region->start[i]) {
                        /* Test for case 4 */
                        /* NO_OVERLAP cases could be taken out, but are left in for clarity */
                        if(node->end[i]<region->start[i]) {
                            clip_case=NO_OVERLAP;
                            assert("invalid clipping case" && 0);
                        } /* end if */
                        else {
                            /* Test for case 2b */
                            if(node->end[i]<=region->end[i]) {
                                clip_case=OVERLAP_LOWER;
                            } /* end if */
                            /* Must be case 1 */
                            else {
                                clip_case=OVERLAP_BOTH;
                            } /* end else */
                        } /* end else */
                    } /* end if */
                    /* Case 2a, 3 or 4 (on the other side)*/
                    else {
                        /* Test for case 4 */
                        if(node->start[i]>region->end[i]) {
                            clip_case=NO_OVERLAP;
                            assert("invalid clipping case" && 0);
                        } /* end if */
                        /* Case 2a or 3 */
                        else {
                            /* Test for case 2a */
                            if(node->end[i]>region->end[i]) {
                                clip_case=OVERLAP_UPPER;
                            } /* end if */
                            /* Must be case 3 */
                            else {
                                clip_case=WITHIN;
                            } /* end else */
                        } /* end else */
                    } /* end else */
                    
                    if(clip_case!=WITHIN) {
                        /* Copy all the dimensions start & end points */
                        HDmemcpy(start,node->start,rank*sizeof(hssize_t));
                        HDmemcpy(end,node->end,rank*sizeof(hssize_t));
                    } /* end if */

                    /* Work on upper overlapping block */
                    if(clip_case==OVERLAP_BOTH || clip_case==OVERLAP_LOWER) {
                        /* Modify the end point in the current dimension of the overlap */
                        end[i]=region->start[i]-1;
                        /* Clip the existing non-overlapped portion off the current node */
                        node->start[i]=region->start[i];
                        /* Add the non-overlapping portion to the list of new nodes */
                        if(H5S_hyper_node_add(&nodes,1,rank,(const hssize_t *)start,(const hsize_t *)end)<0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslab");
                    } /* end if */

                    /* Work on lower overlapping block */
                    if(clip_case==OVERLAP_BOTH || clip_case==OVERLAP_UPPER) {
                        /* Modify the start & end point in the current dimension of the overlap */
                        start[i]=region->end[i]+1;
                        end[i]=node->end[i];
                        /* Clip the existing non-overlapped portion off the current node */
                        node->end[i]=region->end[i];
                        /* Add the non-overlapping portion to the list of new nodes */
                        if(H5S_hyper_node_add(&nodes,1,rank,(const hssize_t *)start,(const hsize_t *)end)<0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslab");
                    } /* end if */

                    /* Check if this is the last dimension */
                    /* Add the block to the "overlapped" list, if so */
                    /* Allow the algorithm to proceed to the next dimension otherwise */
                    if(i==(rank-1)) {   
                        if(overlap!=NULL) {
                            if(H5S_hyper_node_prepend(overlap,node)<0)
                                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslab");
                        }
                        else {  /* Free the node if we aren't going to keep it */
                            H5S_hyper_node_release(node);
                        } /* end else */
                        overlapped=1;   /* stop the algorithm for this block */
                    } /* end if */
                } /* end for */
            } /* end if */

            /* Advance to next hyperslab region */
            region=region->next;
        } /* end while */

        /* Check whether we should add the node to the non-overlapping list */
        if(!overlapped) {
            if(uniq!=NULL) {
                if(H5S_hyper_node_prepend(uniq,node)<0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINSERT, FAIL, "can't insert hyperslab");
            }
            else {  /* Free the node if we aren't going to keep it */
                H5S_hyper_node_release(node);
            } /* end else */
        } /* end if */

        /* Advance to next hyperslab node */
        node=next_node;

        /* Check if we've added more nodes from splitting to the list */
        if(node==NULL && nodes!=NULL)
            node=nodes;
    } /* end while */

done:
    if(start!=NULL)
        H5MM_xfree(start);
    if(end!=NULL)
        H5MM_xfree(end);

    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_clip() */

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
    H5S_hyper_node_t *curr,*next;   /* Pointer to hyperslab nodes */
    intn i;     /* Counters */

    FUNC_ENTER (H5S_hyper_release, FAIL);

    /* Check args */
    assert (space && H5S_SEL_HYPERSLABS==space->select.type);
#ifdef QAK
    printf("%s: check 1.0\n",FUNC);
#endif /* QAK */

    /* Reset the number of points selected */
    space->select.num_elem=0;

    /* Release the per-dimension selection info */
    if(space->select.sel_info.hslab.diminfo!=NULL)
        H5MM_xfree(space->select.sel_info.hslab.diminfo);
    space->select.sel_info.hslab.diminfo = NULL;

    /* Release hi and lo boundary information */
    for(i=0; i<space->extent.u.simple.rank; i++) {
        H5MM_xfree(space->select.sel_info.hslab.hyper_lst->lo_bounds[i]);
        space->select.sel_info.hslab.hyper_lst->lo_bounds[i] = NULL;
        H5MM_xfree(space->select.sel_info.hslab.hyper_lst->hi_bounds[i]);
        space->select.sel_info.hslab.hyper_lst->hi_bounds[i] = NULL;
    } /* end for */
    H5MM_xfree(space->select.sel_info.hslab.hyper_lst->lo_bounds);
    space->select.sel_info.hslab.hyper_lst->lo_bounds = NULL;
    H5MM_xfree(space->select.sel_info.hslab.hyper_lst->hi_bounds);
    space->select.sel_info.hslab.hyper_lst->hi_bounds = NULL;

    /* Release list of selected regions */
    curr=space->select.sel_info.hslab.hyper_lst->head;
    while(curr!=NULL) {
        next=curr->next;
        H5S_hyper_node_release(curr);
        curr=next;
    } /* end while */

    /* Release hyperslab selection node itself */
    H5MM_xfree(space->select.sel_info.hslab.hyper_lst);
    space->select.sel_info.hslab.hyper_lst=NULL;

#ifdef QAK
    printf("%s: check 2.0\n",FUNC);
#endif /* QAK */

    FUNC_LEAVE (SUCCEED);
}   /* H5S_hyper_release() */

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
    H5S_hyper_sel_iter_release
 PURPOSE
    Release hyperslab selection iterator information for a dataspace
 USAGE
    herr_t H5S_hyper_sel_iter_release(sel_iter)
        H5S_t *space;                   IN: Pointer to dataspace iterator is for
        H5S_sel_iter_t *sel_iter;       IN: Pointer to selection iterator
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Releases all information for a dataspace hyperslab selection iterator
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

    if(sel_iter->hyp.pos!=NULL)
        H5MM_xfree(sel_iter->hyp.pos);

    FUNC_LEAVE (SUCCEED);
}   /* H5S_hyper_sel_iter_release() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_compare_bounds
 *
 * Purpose:	Compares two bounds for equality
 *
 * Return:	an integer less than, equal to, or greater than zero if the first
 *          region is considered to be respectively less than, equal to, or
 *          greater than the second
 *
 * Programmer:	Quincey Koziol
 *              Friday, July 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5S_hyper_compare_bounds (const void *r1, const void *r2)
{
    if(((const H5S_hyper_bound_t *)r1)->bound<((const H5S_hyper_bound_t *)r2)->bound)
        return(-1);
    else
        if(((const H5S_hyper_bound_t *)r1)->bound>((const H5S_hyper_bound_t *)r2)->bound)
            return(1);
        else
            return(0);
}   /* end H5S_hyper_compare_bounds */

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
    H5S_hyper_list_t *new_hyper;    /* New hyperslab selection */
    H5S_hyper_node_t *curr, *new, *new_head;    /* Hyperslab information nodes */
    H5S_hyper_dim_t *new_diminfo=NULL;	/* New per-dimension info array[rank] */
    intn i;                     /* Counters */
    size_t u;                   /* Counters */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5S_hyper_copy, FAIL);

    assert(src);
    assert(dst);

#ifdef QAK
    printf("%s: check 3.0\n", FUNC);
#endif /* QAK */
    if(src->select.sel_info.hslab.diminfo!=NULL) {
        /* Create the per-dimension selection info */
        if((new_diminfo = H5MM_malloc(sizeof(H5S_hyper_dim_t)*src->extent.u.simple.rank))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate per-dimension array");

        /* Copy the per-dimension selection info */
        for(i=0; i<src->extent.u.simple.rank; i++) {
            new_diminfo[i].start = src->select.sel_info.hslab.diminfo[i].start;
            new_diminfo[i].stride = src->select.sel_info.hslab.diminfo[i].stride;
            new_diminfo[i].count = src->select.sel_info.hslab.diminfo[i].count;
            new_diminfo[i].block = src->select.sel_info.hslab.diminfo[i].block;
        } /* end for */
    } /* end if */
    dst->select.sel_info.hslab.diminfo = new_diminfo;

    /* Create the new hyperslab information node */
    if((new_hyper = H5MM_malloc(sizeof(H5S_hyper_list_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
            "can't allocate point node");

    /* Copy the basic hyperslab selection information */
    *new_hyper=*(src->select.sel_info.hslab.hyper_lst);

    /* Attach the hyperslab information to the destination dataspace */
    dst->select.sel_info.hslab.hyper_lst=new_hyper;
    
#ifdef QAK
    printf("%s: check 4.0\n", FUNC);
#endif /* QAK */
    /* Allocate space for the low & high bound arrays */
    if((new_hyper->lo_bounds = H5MM_malloc(sizeof(H5S_hyper_bound_t *)*src->extent.u.simple.rank))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
            "can't allocate point node");
    if((new_hyper->hi_bounds = H5MM_malloc(sizeof(H5S_hyper_bound_t *)*src->extent.u.simple.rank))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
            "can't allocate point node");
    for(i=0; i<src->extent.u.simple.rank; i++) {
        if((new_hyper->lo_bounds[i] = H5MM_malloc(sizeof(H5S_hyper_bound_t)*src->select.sel_info.hslab.hyper_lst->count))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate point node");
        if((new_hyper->hi_bounds[i] = H5MM_malloc(sizeof(H5S_hyper_bound_t)*src->select.sel_info.hslab.hyper_lst->count))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate point node");
    } /* end for */

#ifdef QAK
    printf("%s: check 5.0\n", FUNC);
#endif /* QAK */
    /* Copy the hyperslab selection nodes, adding them to the lo & hi bound arrays also */
    curr=src->select.sel_info.hslab.hyper_lst->head;
    new_head=NULL;
    u=0;
    while(curr!=NULL) {
#ifdef QAK
    printf("%s: check 5.1\n", FUNC);
#endif /* QAK */
        /* Create each point */
        if((new = H5MM_malloc(sizeof(H5S_hyper_node_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate point node");
        HDmemcpy(new,curr,sizeof(H5S_hyper_node_t));    /* copy caching information */
        if((new->start = H5MM_malloc(src->extent.u.simple.rank*sizeof(hssize_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate coordinate information");
        if((new->end = H5MM_malloc(src->extent.u.simple.rank*sizeof(hssize_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate coordinate information");
        HDmemcpy(new->start,curr->start,(src->extent.u.simple.rank*sizeof(hssize_t)));
        HDmemcpy(new->end,curr->end,(src->extent.u.simple.rank*sizeof(hssize_t)));
        new->next=NULL;

        /* Insert into low & high bound arrays */
        for(i=0; i<src->extent.u.simple.rank; i++) {
            new_hyper->lo_bounds[i][u].bound=new->start[i];
            new_hyper->lo_bounds[i][u].node=new;
            new_hyper->hi_bounds[i][u].bound=new->end[i];
            new_hyper->hi_bounds[i][u].node=new;
        } /* end for */
        u++;    /* Increment the location of the next node in the boundary arrays */

        /* Keep the order the same when copying */
        if(new_head==NULL)
            new_head=new_hyper->head=new;
        else {
            new_head->next=new;
            new_head=new;
        } /* end else */

        curr=curr->next;
    } /* end while */
#ifdef QAK
    printf("%s: check 6.0\n", FUNC);
#endif /* QAK */

    /* Sort the boundary arrays */
    for(i=0; i<src->extent.u.simple.rank; i++) {
        HDqsort(new_hyper->lo_bounds[i], new_hyper->count,
		sizeof(H5S_hyper_bound_t), H5S_hyper_compare_bounds);
        HDqsort(new_hyper->hi_bounds[i], new_hyper->count,
		sizeof(H5S_hyper_bound_t), H5S_hyper_compare_bounds);
    } /* end for */
#ifdef QAK
    printf("%s: check 7.0\n", FUNC);
#endif /* QAK */

done:
    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_copy() */

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
    H5S_hyper_node_t *curr;     /* Hyperslab information nodes */
    intn i;                     /* Counter */
    htri_t ret_value=TRUE;     /* return value */

    FUNC_ENTER (H5S_hyper_select_valid, FAIL);

    assert(space);

    /* Check each point to determine whether selection+offset is within extent */
    curr=space->select.sel_info.hslab.hyper_lst->head;
    while(curr!=NULL && ret_value==TRUE) {
        /* Check each dimension */
        for(i=0; i<space->extent.u.simple.rank; i++) {
            /* Check if an offset has been defined */
            /* Bounds check the selected point + offset against the extent */
            if(((curr->start[i]+space->select.offset[i])>(hssize_t)space->extent.u.simple.size[i])
                    || ((curr->start[i]+space->select.offset[i])<0)
                || ((curr->end[i]+space->select.offset[i])>(hssize_t)space->extent.u.simple.size[i])
                    || ((curr->end[i]+space->select.offset[i])<0)) {
                ret_value=FALSE;
                break;
            } /* end if */
        } /* end for */

        curr=curr->next;
    } /* end while */

    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_select_valid() */

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
    H5S_hyper_node_t *curr;     /* Hyperslab information nodes */
    hssize_t ret_value=FAIL;    /* return value */

    FUNC_ENTER (H5S_hyper_select_serial_size, FAIL);

    assert(space);

    /* Basic number of bytes required to serialize point selection:
     *  <type (4 bytes)> + <version (4 bytes)> + <padding (4 bytes)> + 
     *      <length (4 bytes)> + <rank (4 bytes)> + <# of blocks (4 bytes)> = 24 bytes
     */
    ret_value=24;

    /* Spin through hyperslabs to total the space needed to store them */
    curr=space->select.sel_info.hslab.hyper_lst->head;
    while(curr!=NULL) {
        /* Add 8 bytes times the rank for each element selected */
        ret_value+=8*space->extent.u.simple.rank;
        curr=curr->next;
    } /* end while */

    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_select_serial_size() */

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
    H5S_hyper_node_t *curr;     /* Hyperslab information nodes */
    uint8_t *lenp;          /* pointer to length location for later storage */
    uint32_t len=0;         /* number of bytes used */
    intn i;                 /* local counting variable */
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER (H5S_point_select_serialize, FAIL);

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
    UINT32ENCODE(buf, (uint32_t)space->select.sel_info.hslab.hyper_lst->count);
    len+=4;

    /* Encode each point in selection */
    curr=space->select.sel_info.hslab.hyper_lst->head;
    while(curr!=NULL) {
        /* Add 8 bytes times the rank for each element selected */
        len+=8*space->extent.u.simple.rank;

        /* Encode starting point */
        for(i=0; i<space->extent.u.simple.rank; i++)
            UINT32ENCODE(buf, (uint32_t)curr->start[i]);

        /* Encode ending point */
        for(i=0; i<space->extent.u.simple.rank; i++)
            UINT32ENCODE(buf, (uint32_t)curr->end[i]);

        curr=curr->next;
    } /* end while */

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
    int32_t rank;           	/* rank of points */
    size_t num_elem=0;      	/* number of elements in selection */
    hssize_t *start=NULL;	/* hyperslab start information */
    hsize_t *count=NULL;    	/* hyperslab count information */
    hssize_t *tstart=NULL;	/* temporary hyperslab pointers */
    hsize_t *tcount=NULL;	/* temporary hyperslab pointers */
    uintn i,j;              	/* local counting variables */
    herr_t ret_value=FAIL;  	/* return value */

    FUNC_ENTER (H5S_hyper_select_deserialize, FAIL);

    /* Check args */
    assert(space);
    assert(buf);

    /* Deserialize slabs to select */
    buf+=16;    /* Skip over selection header */
    INT32DECODE(buf,rank);  /* decode the rank of the point selection */
    if(rank!=space->extent.u.simple.rank)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "rank of pointer does not match dataspace");
    UINT32DECODE(buf,num_elem);  /* decode the number of points */

    /* Allocate space for the coordinates */
    if((start = H5MM_malloc(rank*sizeof(hssize_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab information");
    if((count = H5MM_malloc(rank*sizeof(hssize_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate hyperslab information");
    
    /* Retrieve the coordinates from the buffer */
    for(i=0; i<num_elem; i++) {
        /* Decode the starting points */
        for(tstart=start,j=0; j<(unsigned)rank; j++,tstart++)
            UINT32DECODE(buf, *tstart);

        /* Decode the ending points */
        for(tcount=count,j=0; j<(unsigned)rank; j++,tcount++)
            UINT32DECODE(buf, *tcount);

        /* Change the ending points into counts */
        for(tcount=count,tstart=start,j=0; j<(unsigned)rank; j++,tcount++)
            *tcount=(*tcount-*tstart)+1;

        /* Select or add the hyperslab to the current selection */
        if((ret_value=H5S_select_hyperslab(space,(i==0 ? H5S_SELECT_SET : H5S_SELECT_OR),start,NULL,count,NULL))<0) {
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection");
        } /* end if */
    } /* end for */

done:
    /* Free temporary buffers */
    H5MM_xfree(start);
    H5MM_xfree(count);

    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_select_deserialize() */

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
    H5S_hyper_node_t *node;     /* Hyperslab node */
    intn rank;                  /* Dataspace rank */
    intn i;                     /* index variable */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5S_hyper_bounds, FAIL);

    assert(space);
    assert(start);
    assert(end);

    /* Get the dataspace extent rank */
    rank=space->extent.u.simple.rank;

    /* Iterate through the node, copying each hyperslab's information */
    node=space->select.sel_info.hslab.hyper_lst->head;
    while(node!=NULL) {
        for(i=0; i<rank; i++) {
            if(start[i]>(hsize_t)(node->start[i]+space->select.offset[i]))
                start[i]=node->start[i]+space->select.offset[i];
            if(end[i]<(hsize_t)(node->end[i]+space->select.offset[i]))
                end[i]=node->end[i]+space->select.offset[i];
        } /* end for */
        node=node->next;
      } /* end while */

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
    H5S_hyper_node_t *node;     /* Hyperslab node */
    intn rank;                  /* Dataspace rank */
    intn i;                     /* index variable */

    FUNC_ENTER (H5S_hyper_select_contiguous, FAIL);

    assert(space);

    /* If there is more than one hyperslab in the selection, they are not contiguous */
    if(space->select.sel_info.hslab.hyper_lst->count>1)
    	ret_value=FALSE;
    else {	/* If there is one hyperslab, then it might be contiguous */
	/* Get the dataspace extent rank */
	rank=space->extent.u.simple.rank;

	/* Get the hyperslab node */
	node=space->select.sel_info.hslab.hyper_lst->head;

	/*
	 * For a hyperslab to be contiguous, it's size must be the same as the
	 * dataspace extent's in all but the slowest changing dimension
	 */
	ret_value=TRUE;	/* assume true and reset if the dimensions don't match */
	for(i=1; i<rank; i++) {
		if(((node->end[i]-node->start[i])+1)!=space->extent.u.simple.size[i]) {
			ret_value=FALSE;
			break;
		} /* end if */
	}
    } /* end else */
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_select_contiguous() */


/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_select_iterate_mem
 *
 * Purpose:	Recursively iterates over data points in memory using the parameters
 *      passed to H5S_hyper_select_iterate.
 *
 * Return:	Success:	Number of elements copied.
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 22, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5S_hyper_select_iterate_mem (intn dim, H5S_hyper_iter_info_t *iter_info)
{
    hsize_t offset;             /* offset of region in buffer */
    void *tmp_buf;              /* temporary location of the element in the buffer */
    hid_t reg_id;               /* ID of temporary region buffer */
    H5S_hyper_region_t *regions;  /* Pointer to array of hyperslab nodes overlapped */
    size_t num_regions;         /* number of regions overlapped */
    herr_t user_ret=0;          /* User's return value */
    size_t i;                   /* Counters */
    intn j;

    FUNC_ENTER (H5S_hyper_select_iterate_mem, 0);

    assert(iter_info);

    /* Get a sorted list (in the next dimension down) of the regions which */
    /*  overlap the current index in this dim */
    if((reg_id=H5S_hyper_get_regions(&num_regions,dim,
            iter_info->space->select.sel_info.hslab.hyper_lst->count,
            iter_info->lo_bounds, iter_info->hi_bounds,
            iter_info->iter->hyp.pos,iter_info->space->select.offset))>=0) {

        /* Get the pointer to the actual regions array */
        regions=H5TB_buf_ptr(reg_id);

        /* Check if this is the second to last dimension in dataset */
        /*  (Which means that we've got a list of the regions in the fastest */
        /*   changing dimension and should input those regions) */
        if((dim+2)==iter_info->space->extent.u.simple.rank) {
            HDmemcpy(iter_info->mem_offset, iter_info->iter->hyp.pos,(iter_info->space->extent.u.simple.rank*sizeof(hssize_t)));
            iter_info->mem_offset[iter_info->space->extent.u.simple.rank]=0;

            /* Iterate over data from regions */
            for(i=0; i<num_regions && user_ret==0; i++) {
                /* Set the location of the current hyperslab */
                iter_info->mem_offset[iter_info->space->extent.u.simple.rank-1]=regions[i].start;

                /* Get the offset in the memory buffer */
                offset=H5V_array_offset(iter_info->space->extent.u.simple.rank+1,
                    iter_info->mem_size,iter_info->mem_offset);
                tmp_buf=((char *)iter_info->src+offset);

                /* Iterate over each element in the current region */
                for(j=regions[i].start; j<=regions[i].end && user_ret==0; j++) {
                    /* Call the user's function */
                    user_ret=(*(iter_info->op))(tmp_buf,iter_info->dt,iter_info->space->extent.u.simple.rank,iter_info->mem_offset,iter_info->op_data);

                    /* Subtract the element from the selected region (not implemented yet) */

                    /* Increment the coordinate offset */
                    iter_info->mem_offset[iter_info->space->extent.u.simple.rank-1]=j;

                    /* Advance the pointer in the buffer */
                    tmp_buf=((char *)tmp_buf+iter_info->elem_size);
                } /* end for */

                /* Decrement the iterator count */
                iter_info->iter->hyp.elmt_left-=((regions[i].end-regions[i].start)+1);
            } /* end for */

            /* Set the next position to start at */
            iter_info->iter->hyp.pos[dim+1]=(-1);
        } else { /* recurse on each region to next dimension down */

            /* Increment the dimension we are working with */
            dim++;

            /* Step through each region in this dimension */
            for(i=0; i<num_regions && user_ret==0; i++) {
                /* Step through each location in each region */
                for(j=regions[i].start; j<=regions[i].end && user_ret==0; j++) {

                    /*
                     * If we are moving to a new position in this dim, reset
                     * the next lower dim. location.
                     */
                    if(iter_info->iter->hyp.pos[dim]!=j)
                        iter_info->iter->hyp.pos[dim+1]=(-1);

                    /* Set the correct position we are working on */
                    iter_info->iter->hyp.pos[dim]=j;

                    /* Go get the regions in the next lower dimension */
                    user_ret=H5S_hyper_select_iterate_mem(dim, iter_info);
                } /* end for */
            } /* end for */
        } /* end else */

        /* Release the temporary buffer */
        H5TB_release_buf(reg_id);
    } /* end if */

    FUNC_LEAVE (user_ret);
}   /* H5S_hyper_select_iterate_mem() */


/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_select_iterate
 PURPOSE
    Iterate over a hyperslab selection, calling a user's function for each
        element.
 USAGE
    herr_t H5S_hyper_select_iterate(buf, type_id, space, operator, operator_data)
        void *buf;      IN/OUT: Buffer containing elements to iterate over
        hid_t type_id;  IN: Datatype ID of BUF array.
        H5S_t *space;   IN: Dataspace object containing selection to iterate over
        H5D_operator_t operator; IN: Function pointer to the routine to be
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
H5S_hyper_select_iterate(void *buf, hid_t type_id, H5S_t *space, H5D_operator_t operator,
        void *operator_data)
{
    H5S_hyper_bound_t **lo_bounds;    /* Lower (closest to the origin) bound array for each dimension */
    H5S_hyper_bound_t **hi_bounds;    /* Upper (farthest from the origin) bound array for each dimension */
    H5S_hyper_iter_info_t iter_info;  /* Block of parameters to pass into recursive calls */
    H5S_sel_iter_t	iter;   /* selection iteration info*/
    size_t	min_elem_out=1; /* Minimum # of elements to output*/
    intn	i;				/*counters		*/
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5S_hyper_select_iterate, FAIL);

    assert(buf);
    assert(space);
    assert(operator);
    assert(H5I_DATATYPE == H5I_get_type(type_id));

    /* Initialize these before any errors can occur */
    HDmemset(&iter,0,sizeof(H5S_sel_iter_t));

    /* Initialize the selection iterator */
    if (H5S_hyper_init(NULL, space, &iter, &min_elem_out)<0) {
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		     "unable to initialize selection information");
    } 

    /* Allocate space for the low & high bound arrays */
    lo_bounds = H5MM_malloc(space->extent.u.simple.rank * sizeof(H5S_hyper_bound_t *));
    hi_bounds = H5MM_malloc(space->extent.u.simple.rank * sizeof(H5S_hyper_bound_t *));

    /*
     * Initialize to correct order to walk through arrays.  (When another
     * iteration order besides the default 'C' order is chosen, this is the
     * correct place to change the order of the array iterations)
     */
    for(i=0; i<space->extent.u.simple.rank; i++) {
        lo_bounds[i]=space->select.sel_info.hslab.hyper_lst->lo_bounds[i];
        hi_bounds[i]=space->select.sel_info.hslab.hyper_lst->hi_bounds[i];
    } /* end for */

    /* Initialize parameter block for recursive calls */
    iter_info.dt=type_id;
    iter_info.elem_size=H5Tget_size(type_id);
    iter_info.space=space;
    iter_info.iter=&iter;
    iter_info.src=buf;
    iter_info.lo_bounds=lo_bounds;
    iter_info.hi_bounds=hi_bounds;

    /* Set up the size of the memory space */
    HDmemcpy(iter_info.mem_size, space->extent.u.simple.size, space->extent.u.simple.rank*sizeof(hsize_t));
    iter_info.mem_size[space->extent.u.simple.rank]=iter_info.elem_size;

    /* Copy the location of the region in the file */
    iter_info.op=operator;
    iter_info.op_data=operator_data;

    /* Recursively input the hyperslabs currently defined */
    /* starting with the slowest changing dimension */
    ret_value=H5S_hyper_select_iterate_mem(-1,&iter_info);

    /* Release the memory we allocated */
    H5MM_xfree(lo_bounds);
    H5MM_xfree(hi_bounds);

    /* Release selection iterator */
    H5S_sel_iter_release(space,&iter);

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_hyper_select_iterate() */
