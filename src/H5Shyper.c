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
#include <H5Sprivate.h>
#include <H5Vprivate.h>
#include <H5MMprivate.h>

/* Interface initialization */
#define PABLO_MASK      H5S_hyper_mask
#define INTERFACE_INIT  NULL
static intn             interface_initialize_g = FALSE;

/* Local datatypes */
/* Parameter block for H5S_hyper_fread & H5S_hyper_fwrite */
typedef struct {
    H5F_t *f;
    const struct H5O_layout_t *layout;
	const struct H5O_compress_t *comp;
    const struct H5O_efl_t *efl;
    size_t elmt_size;
    const H5S_t *space;
    H5S_sel_iter_t *iter;
	size_t nelmts;
    H5D_transfer_t xfer_mode;
    const void *src;
    void *dst;
    H5S_hyper_bound_t **lo_bounds;
    H5S_hyper_bound_t **hi_bounds;
} H5S_hyper_fhyper_info_t;

/* Static function prototypes */
static intn H5S_hyper_bsearch(hssize_t size, H5S_hyper_bound_t *barr, size_t count);
static H5S_hyper_region_t *H5S_hyper_get_regions (size_t *num_regions, intn dim,
    size_t bound_count, H5S_hyper_bound_t **lo_bounds,
    H5S_hyper_bound_t **hi_bounds, hssize_t *pos);
static size_t H5S_hyper_fread (intn dim, H5S_hyper_fhyper_info_t *fhyper_info);
static size_t H5S_hyper_fwrite (intn dim, H5S_hyper_fhyper_info_t *fhyper_info);


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
herr_t
H5S_hyper_init (const struct H5O_layout_t __unused__ *layout,
	       const H5S_t *space, H5S_sel_iter_t *sel_iter)
{
    FUNC_ENTER (H5S_hyper_init, FAIL);

    /* Check args */
    assert (layout);
    assert (space && H5S_SEL_HYPERSLABS==space->select.type);
    assert (sel_iter);

    /* Initialize the number of points to iterate over */
    sel_iter->hyp.elmt_left=space->select.num_elem;

    /* Allocate the position & initialize to invalid location */
    sel_iter->hyp.pos = H5MM_malloc(space->extent.u.simple.rank*sizeof(hssize_t));
    sel_iter->hyp.pos[0]=(-1);
    H5V_array_fill(sel_iter->hyp.pos,sel_iter->hyp.pos,sizeof(hssize_t),space->extent.u.simple.rank);
    
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
size_t
H5S_hyper_favail (const H5S_t __unused__ *space, const H5S_sel_iter_t *sel_iter, size_t max)
{
    FUNC_ENTER (H5S_hyper_favail, FAIL);

    /* Check args */
    assert (space && H5S_SEL_HYPERSLABS==space->select.type);
    assert (sel_iter);

#ifdef QAK
printf("%s: max=%d\n",FUNC,(int)max);
#endif /* QAK */
    FUNC_LEAVE (MIN(sel_iter->hyp.elmt_left,max));
}   /* H5S_hyper_favail() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_get_regions
 *
 * Purpose:	Builds a sorted array of the overlaps in a dimension
 *
 * Return:	Success:	Pointer to valid array (num_regions parameter set to
 *                          array size)
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
static H5S_hyper_region_t *
H5S_hyper_get_regions (size_t *num_regions, intn dim, size_t bound_count,
    H5S_hyper_bound_t **lo_bounds, H5S_hyper_bound_t **hi_bounds, hssize_t *pos)
{
    H5S_hyper_region_t *ret_value=NULL;   /* Pointer to array to return */
    size_t num_reg=0,                   /* Number of regions in array */
        curr_reg=0;                     /* The current region we are working with */
    intn next_dim,                      /* Next fastest dimension */
        temp_dim;                       /* Temporary dim. holder */
    size_t i;                           /* Counters */

    FUNC_ENTER (H5S_hyper_get_regions, NULL);
    
    assert(num_regions);
    assert(lo_bounds);
    assert(hi_bounds);
    assert(pos);

#ifdef QAK
printf("%s: check 1.0, dim=%d\n",FUNC,dim);
for(i=0; i<2; i++)
    printf("%s: %d - pos=%d\n",FUNC,i,(int)pos[i]);
#endif /* QAK */

    /* Check if we need to generate a list of regions for the 0th dim. */
    if(dim<0) {
#ifdef QAK
printf("%s: check 1.1, bound_count=%d\n",FUNC,bound_count);
#endif /* QAK */
        for(i=0; i<bound_count; i++) {
            /* Skip past already iterated regions */
            if(pos[0]==(-1) || (pos[0]>=lo_bounds[0][i].bound && pos[0]<=hi_bounds[0][i].bound)) {
                /* Check if we've allocated the array yet */
                if(num_reg==0) {
                    /* Allocate array */
                    ret_value=H5MM_malloc(sizeof(H5S_hyper_region_t));

                    /* Initialize with first region */
                    ret_value[0].start=MAX(lo_bounds[0][i].bound,pos[0]);
                    ret_value[0].end=hi_bounds[0][i].bound;

                    /* Increment the number of regions */
                    num_reg++;
                } else {
                    /* Check if we should merge this region into the current region */
                    if(lo_bounds[0][i].bound<ret_value[curr_reg].end)
                        ret_value[curr_reg].end=MAX(hi_bounds[0][i].bound,ret_value[curr_reg].end);
                    else {  /* no overlap with previous region, add new region */
                        /* Enlarge array */
                        ret_value=H5MM_realloc(ret_value,sizeof(H5S_hyper_region_t)*(num_reg+1));

                        /* Initialize with new region */
                        ret_value[num_reg].start=lo_bounds[0][i].bound;
                        ret_value[num_reg].end=hi_bounds[0][i].bound;

                        /* Increment the number of regions & the current region */
                        num_reg++;
                        curr_reg++;
                    } /* end else */
                } /* end else */
            } /* end if */
        } /* end for */
    } else {    /* Generate list of regions based on the current position */
#ifdef QAK
printf("%s: check 2.0, bound_count=%d\n",FUNC,bound_count);
#endif /* QAK */
        next_dim=dim+1;
        for(i=0; i<bound_count; i++) {

            /* Check if each boundary overlaps in the higher dimensions */
            temp_dim=dim;
            while(temp_dim>=0 && pos[temp_dim]>=lo_bounds[temp_dim][i].bound &&
                    pos[temp_dim]<=hi_bounds[temp_dim][i].bound)
                temp_dim--;

            /* Yes, all previous positions match, this is a valid region */
            if(temp_dim<0) {
                /* Check if we've allocated the array yet */
                if(num_reg==0) {
                    /* Allocate array */
                    ret_value=H5MM_malloc(sizeof(H5S_hyper_region_t));

                    /* Initialize with first region */
                    ret_value[0].start=MAX(lo_bounds[next_dim][i].bound,pos[next_dim]);
                    ret_value[0].end=hi_bounds[next_dim][i].bound;

                    /* Increment the number of regions */
                    num_reg++;
                } else {
                    /* Check if we should merge this region into the current region */
                    if(lo_bounds[next_dim][i].bound<ret_value[curr_reg].end)
                        ret_value[curr_reg].end=MAX(hi_bounds[next_dim][i].bound,ret_value[curr_reg].end);
                    else {  /* no overlap with previous region, add new region */
                        /* Enlarge array */
                        ret_value=H5MM_realloc(ret_value,sizeof(H5S_hyper_region_t)*(num_reg+1));

                        /* Initialize with new region */
                        ret_value[num_reg].start=lo_bounds[next_dim][i].bound;
                        ret_value[num_reg].end=hi_bounds[next_dim][i].bound;

                        /* Increment the number of regions & the current region */
                        num_reg++;
                        curr_reg++;
                    } /* end else */
                } /* end else */
            } /* end if */
        } /* end for */
    } /* end else */

    /* Save the number of regions we generated */
    *num_regions=num_reg;

#ifdef QAK
printf("%s: check 10.0, ret_value=%p, num_reg=%d\n",FUNC,ret_value,num_reg);
#endif /* QAK */

    FUNC_LEAVE (ret_value);
} /* end H5S_hyper_get_regions() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_fread
 *
 * Purpose:	Recursively gathers data points from a file using the parameters
 *      passed to H5S_hyper_fgath.
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
H5S_hyper_fread (intn dim, H5S_hyper_fhyper_info_t *fhyper_info)
{
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of slab in file*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hsize_t region_size;                /* Size of lowest region */
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    H5S_hyper_region_t *regions;  /* Pointer to array of hyperslab nodes overlapped */
    size_t num_regions;         /* number of regions overlapped */
    size_t i;                   /* Counters */
    intn j;
    size_t num_read=0;          /* Number of elements read */

    FUNC_ENTER (H5S_hyper_fread, 0);

    assert(fhyper_info);

#ifdef QAK
printf("%s: check 1.0, dim=%d\n",FUNC,dim);
#endif /* QAK */

    /* Get a sorted list (in the next dimension down) of the regions which */
    /*  overlap the current index in this dim */
    if((regions=H5S_hyper_get_regions(&num_regions,dim,
            fhyper_info->space->select.sel_info.hyper_lst->count,
            fhyper_info->lo_bounds, fhyper_info->hi_bounds,
            fhyper_info->iter->hyp.pos))!=NULL) {

        /* Check if this is the second to last dimension in dataset */
        /*  (Which means that we've got a list of the regions in the fastest */
        /*   changing dimension and should input those regions) */
#ifdef QAK
printf("%s: check 2.0, rank=%d\n",FUNC,(int)fhyper_info->space->extent.u.simple.rank);
for(i=0; i<num_regions; i++)
    printf("%s: check 2.1, region #%d: start=%d, end=%d\n",FUNC,i,(int)regions[i].start,(int)regions[i].end);
#endif /* QAK */
        if((dim+2)==fhyper_info->space->extent.u.simple.rank) {

            /* Set up hyperslab I/O parameters which apply to all regions */

            /* Copy the location of the region in the file */
            HDmemcpy(file_offset,fhyper_info->iter->hyp.pos,fhyper_info->space->extent.u.simple.rank*sizeof(hssize_t));
            file_offset[fhyper_info->space->extent.u.simple.rank]=0;

            /* Set the hyperslab size to copy */
            hsize[0]=1;
            H5V_array_fill(hsize,hsize,sizeof(hsize[0]),fhyper_info->space->extent.u.simple.rank);
            hsize[fhyper_info->space->extent.u.simple.rank]=fhyper_info->elmt_size;

            /* Set the memory offset to the origin */
            HDmemset (zero, 0, fhyper_info->layout->ndims*sizeof(*zero));

            /* perform I/O on data from regions */
            for(i=0; i<num_regions && fhyper_info->nelmts>0; i++) {
#ifdef QAK
printf("%s: check 2.2, i=%d\n",FUNC,(int)i);
#endif /* QAK */
                region_size=MIN(fhyper_info->nelmts,(regions[i].end-regions[i].start)+1);
                hsize[fhyper_info->space->extent.u.simple.rank-1]=region_size;
                file_offset[fhyper_info->space->extent.u.simple.rank-1]=regions[i].start;

                /*
                 * Gather from file.
                 */
                if (H5F_arr_read (fhyper_info->f, fhyper_info->layout,
                        fhyper_info->comp, fhyper_info->efl, hsize, hsize, zero,
                        file_offset, fhyper_info->xfer_mode,
                        fhyper_info->dst/*out*/)<0) {
                    HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, 0, "read error");
                }
#ifdef QAK
printf("%s: check 2.3, region #%d\n",FUNC,(int)i);
for(j=0; j<fhyper_info->space->extent.u.simple.rank; j++)
    printf("%s: %d - pos=%d\n",FUNC,j,(int)fhyper_info->iter->hyp.pos[j]);
#endif /* QAK */

                /* Advance the pointer in the buffer */
                fhyper_info->dst=((uint8 *)fhyper_info->dst)+region_size*fhyper_info->elmt_size;

                /* Increment the number of elements read */
                num_read+=region_size;

                /* Decrement the buffer left */
                fhyper_info->nelmts-=region_size;

                /* Set the next position to start at */
                if(region_size==(hsize_t)((regions[i].end-regions[i].start)+1))
                    fhyper_info->iter->hyp.pos[dim+1]=(-1);
                else
                    fhyper_info->iter->hyp.pos[dim+1]=regions[i].start+region_size;

                /* Decrement the iterator count */
                fhyper_info->iter->hyp.elmt_left-=region_size;
            } /* end for */
        } else { /* recurse on each region to next dimension down */
#ifdef QAK
printf("%s: check 3.0, num_regions=%d\n",FUNC,(int)num_regions);
#endif /* QAK */

            /* Increment the dimension we are working with */
            dim++;

            /* Step through each region in this dimension */
            for(i=0; i<num_regions && fhyper_info->nelmts>0; i++) {
                /* Step through each location in each region */
                for(j=regions[i].start; j<=regions[i].end && fhyper_info->nelmts>0; j++) {
#ifdef QAK
printf("%s: check 4.0, dim=%d, location=%d\n",FUNC,dim,j);
#endif /* QAK */

                    /* Set the correct position we are working on */
                    fhyper_info->iter->hyp.pos[dim]=j;

                    /* Go get the regions in the next lower dimension */
                    num_read+=H5S_hyper_fread(dim, fhyper_info);
                } /* end for */
            } /* end for */
        } /* end else */

        /* Free the region space */
        H5MM_xfree(regions);
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
size_t
H5S_hyper_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
		size_t elmt_size, const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		size_t nelmts,
		const H5D_transfer_t xfer_mode, void *_buf/*out*/)
{
    H5S_hyper_bound_t **lo_bounds;    /* Lower (closest to the origin) bound array for each dimension */
    H5S_hyper_bound_t **hi_bounds;    /* Upper (farthest from the origin) bound array for each dimension */
    H5S_hyper_fhyper_info_t fhyper_info;  /* Block of parameters to pass into recursive calls */
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
    lo_bounds = H5MM_malloc(file_space->extent.u.simple.rank * sizeof(H5S_hyper_bound_t *));
    hi_bounds = H5MM_malloc(file_space->extent.u.simple.rank * sizeof(H5S_hyper_bound_t *));

    /* Initialize to correct order to walk through arrays.
        (When another iteration order besides the default 'C' order is chosen,
        this is the correct place to change the order of the array iterations)
    */
    for(i=0; i<file_space->extent.u.simple.rank; i++) {
        lo_bounds[i]=file_space->select.sel_info.hyper_lst->lo_bounds[i];
        hi_bounds[i]=file_space->select.sel_info.hyper_lst->hi_bounds[i];
    } /* end for */

    /* Initialize parameter block for recursive calls */
    fhyper_info.f=f;
    fhyper_info.layout=layout;
    fhyper_info.comp=comp;
    fhyper_info.efl=efl;
    fhyper_info.elmt_size=elmt_size;
    fhyper_info.space=file_space;
    fhyper_info.iter=file_iter;
    fhyper_info.nelmts=nelmts;
    fhyper_info.xfer_mode=xfer_mode;
    fhyper_info.src=NULL;
    fhyper_info.dst=_buf;
    fhyper_info.lo_bounds=lo_bounds;
    fhyper_info.hi_bounds=hi_bounds;

    /* Recursively input the hyperslabs currently defined */
    /* starting with the slowest changing dimension */
#ifdef QAK
printf("%s: check 4.0\n",FUNC);
#endif /* QAK */
    num_read=H5S_hyper_fread(-1,&fhyper_info);
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
H5S_hyper_fwrite (intn dim, H5S_hyper_fhyper_info_t *fhyper_info)
{
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of slab in file*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hsize_t region_size;                /* Size of lowest region */
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    H5S_hyper_region_t *regions;  /* Pointer to array of hyperslab nodes overlapped */
    size_t num_regions;         /* number of regions overlapped */
    size_t i;                   /* Counters */
    intn j;
    size_t num_written=0;          /* Number of elements read */

    FUNC_ENTER (H5S_hyper_fwrite, 0);

    assert(fhyper_info);

    /* Get a sorted list (in the next dimension down) of the regions which */
    /*  overlap the current index in this dim */
    if((regions=H5S_hyper_get_regions(&num_regions,dim,
            fhyper_info->space->select.sel_info.hyper_lst->count,
            fhyper_info->lo_bounds, fhyper_info->hi_bounds,
            fhyper_info->iter->hyp.pos))!=NULL) {

        /* Check if this is the second to last dimension in dataset */
        /*  (Which means that we've got a list of the regions in the fastest */
        /*   changing dimension and should input those regions) */
        if((dim+2)==fhyper_info->space->extent.u.simple.rank) {

            /* Set up hyperslab I/O parameters which apply to all regions */

            /* Copy the location of the region in the file */
            HDmemcpy(file_offset,fhyper_info->iter->hyp.pos,fhyper_info->space->extent.u.simple.rank*sizeof(hssize_t));
            file_offset[fhyper_info->space->extent.u.simple.rank]=0;

            /* Set the hyperslab size to copy */
            hsize[0]=1;
            H5V_array_fill(hsize,hsize,sizeof(hsize[0]),fhyper_info->space->extent.u.simple.rank);
            hsize[fhyper_info->space->extent.u.simple.rank]=fhyper_info->elmt_size;

            /* Set the memory offset to the origin */
            HDmemset (zero, 0, fhyper_info->layout->ndims*sizeof(*zero));

            /* perform I/O on data from regions */
            for(i=0; i<num_regions && fhyper_info->nelmts>0; i++) {
                region_size=MIN(fhyper_info->nelmts,(regions[i].end-regions[i].start)+1);
                hsize[fhyper_info->space->extent.u.simple.rank-1]=region_size;
                file_offset[fhyper_info->space->extent.u.simple.rank-1]=regions[i].start;

                /*
                 * Scatter from file.
                 */
                if (H5F_arr_write (fhyper_info->f, fhyper_info->layout,
                        fhyper_info->comp, fhyper_info->efl, hsize, hsize, zero,
                        file_offset, fhyper_info->xfer_mode,
                        fhyper_info->src)<0) {
                    HRETURN_ERROR (H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
                }

                /* Advance the pointer in the buffer */
                fhyper_info->src=((const uint8 *)fhyper_info->src)+region_size*fhyper_info->elmt_size;

                /* Increment the number of elements read */
                num_written+=region_size;

                /* Decrement the buffer left */
                fhyper_info->nelmts-=region_size;

                /* Set the next position to start at */
                if(region_size==(hsize_t)((regions[i].end-regions[i].start)+1))
                    fhyper_info->iter->hyp.pos[dim+1]=(-1);
                else
                    fhyper_info->iter->hyp.pos[dim+1]=regions[i].start+region_size;

                /* Decrement the iterator count */
                fhyper_info->iter->hyp.elmt_left-=region_size;
            } /* end for */
        } else { /* recurse on each region to next dimension down */

            /* Increment the dimension we are working with */
            dim++;

            /* Step through each region in this dimension */
            for(i=0; i<num_regions && fhyper_info->nelmts>0; i++) {
                /* Step through each location in each region */
                for(j=regions[i].start; j<=regions[i].end && fhyper_info->nelmts>0; j++) {

                    /* Set the correct position we are working on */
                    fhyper_info->iter->hyp.pos[dim]=j;

                    /* Go get the regions in the next lower dimension */
                    num_written+=H5S_hyper_fwrite(dim, fhyper_info);
                } /* end for */
            } /* end for */
        } /* end else */

        /* Free the region space */
        H5MM_xfree(regions);
    } /* end if */

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
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_hyper_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
		size_t elmt_size, const H5S_t *file_space, H5S_sel_iter_t *file_iter,
		size_t nelmts,
		const H5D_transfer_t xfer_mode, const void *_buf)
{
    H5S_hyper_bound_t **lo_bounds;    /* Lower (closest to the origin) bound array for each dimension */
    H5S_hyper_bound_t **hi_bounds;    /* Upper (farthest from the origin) bound array for each dimension */
    H5S_hyper_fhyper_info_t fhyper_info;  /* Block of parameters to pass into recursive calls */
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
    lo_bounds = H5MM_malloc(file_space->extent.u.simple.rank * sizeof(H5S_hyper_bound_t *));
    hi_bounds = H5MM_malloc(file_space->extent.u.simple.rank * sizeof(H5S_hyper_bound_t *));

    /* Initialize to correct order to walk through arrays.
        (When another iteration order besides the default 'C' order is chosen,
        this is the correct place to change the order of the array iterations)
    */
    for(i=0; i<file_space->extent.u.simple.rank; i++) {
        lo_bounds[i]=file_space->select.sel_info.hyper_lst->lo_bounds[i];
        hi_bounds[i]=file_space->select.sel_info.hyper_lst->hi_bounds[i];
    } /* end for */

    /* Initialize parameter block for recursive calls */
    fhyper_info.f=f;
    fhyper_info.layout=layout;
    fhyper_info.comp=comp;
    fhyper_info.efl=efl;
    fhyper_info.elmt_size=elmt_size;
    fhyper_info.space=file_space;
    fhyper_info.iter=file_iter;
    fhyper_info.nelmts=nelmts;
    fhyper_info.xfer_mode=xfer_mode;
    fhyper_info.src=_buf;
    fhyper_info.dst=NULL;
    fhyper_info.lo_bounds=lo_bounds;
    fhyper_info.hi_bounds=hi_bounds;

    /* Recursively input the hyperslabs currently defined */
    /* starting with the slowest changing dimension */
    num_written=H5S_hyper_fwrite(-1,&fhyper_info);

    /* Release the memory we allocated */
    H5MM_xfree(lo_bounds);
    H5MM_xfree(hi_bounds);
    
    FUNC_LEAVE (num_written);
} /* H5S_hyper_fscat() */

/*-------------------------------------------------------------------------
 * Function:	H5S_hyper_mread
 *
 * Purpose:	Recursively gathers data points from memory using the parameters
 *      passed to H5S_hyper_mgath.
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
H5S_hyper_mread (intn dim, H5S_hyper_fhyper_info_t *fhyper_info)
{
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];	/*size of memory buffer*/
    hssize_t	mem_offset[H5O_LAYOUT_NDIMS];	/*offset of slab in memory*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hsize_t region_size;                /* Size of lowest region */
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    H5S_hyper_region_t *regions;  /* Pointer to array of hyperslab nodes overlapped */
    size_t num_regions;         /* number of regions overlapped */
    size_t i;                   /* Counters */
    intn j;
    size_t num_read=0;          /* Number of elements read */

    FUNC_ENTER (H5S_hyper_mread, 0);

    assert(fhyper_info);

#ifdef QAK
printf("%s: check 1.0, dim=%d\n",FUNC,dim);
#endif /* QAK */

    /* Get a sorted list (in the next dimension down) of the regions which */
    /*  overlap the current index in this dim */
    if((regions=H5S_hyper_get_regions(&num_regions,dim,
            fhyper_info->space->select.sel_info.hyper_lst->count,
            fhyper_info->lo_bounds, fhyper_info->hi_bounds,
            fhyper_info->iter->hyp.pos))!=NULL) {

        /* Check if this is the second to last dimension in dataset */
        /*  (Which means that we've got a list of the regions in the fastest */
        /*   changing dimension and should input those regions) */
#ifdef QAK
printf("%s: check 2.0, rank=%d\n",FUNC,(int)fhyper_info->space->extent.u.simple.rank);
for(i=0; i<num_regions; i++)
    printf("%s: check 2.1, region #%d: start=%d, end=%d\n",FUNC,i,(int)regions[i].start,(int)regions[i].end);
#endif /* QAK */

        if((dim+2)==fhyper_info->space->extent.u.simple.rank) {

            /* Set up hyperslab I/O parameters which apply to all regions */

            /* Set up the size of the memory space */
            HDmemcpy(mem_size,fhyper_info->space->extent.u.simple.size,fhyper_info->space->extent.u.simple.rank*sizeof(hsize_t));
            mem_size[fhyper_info->space->extent.u.simple.rank]=fhyper_info->elmt_size;

            /* Copy the location of the region in the file */
            HDmemcpy(mem_offset,fhyper_info->iter->hyp.pos,fhyper_info->space->extent.u.simple.rank*sizeof(hssize_t));
            mem_offset[fhyper_info->space->extent.u.simple.rank]=0;

            /* Set the hyperslab size to copy */
            hsize[0]=1;
            H5V_array_fill(hsize,hsize,sizeof(hsize[0]),fhyper_info->space->extent.u.simple.rank);
            hsize[fhyper_info->space->extent.u.simple.rank]=fhyper_info->elmt_size;

            /* Set the memory offset to the origin */
            HDmemset (zero, 0, (fhyper_info->space->extent.u.simple.rank+1)*sizeof(*zero));

            /* perform I/O on data from regions */
            for(i=0; i<num_regions && fhyper_info->nelmts>0; i++) {
#ifdef QAK
printf("%s: check 2.1, i=%d\n",FUNC,(int)i);
#endif /* QAK */
                region_size=MIN(fhyper_info->nelmts,(regions[i].end-regions[i].start)+1);
                hsize[fhyper_info->space->extent.u.simple.rank-1]=region_size;
                mem_offset[fhyper_info->space->extent.u.simple.rank-1]=regions[i].start;

                /*
                 * Gather from memory.
                 */
                if (H5V_hyper_copy (fhyper_info->space->extent.u.simple.rank+1,
                        hsize, hsize, zero, fhyper_info->dst,
                        mem_size, mem_offset, fhyper_info->src)<0) {
                    HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, 0, "unable to gather data from memory");
                }

                /* Advance the pointer in the buffer */
                fhyper_info->dst=((uint8 *)fhyper_info->dst)+region_size*fhyper_info->elmt_size;

                /* Increment the number of elements read */
                num_read+=region_size;

                /* Decrement the buffer left */
                fhyper_info->nelmts-=region_size;

                /* Set the next position to start at */
                if(region_size==(hsize_t)((regions[i].end-regions[i].start)+1))
                    fhyper_info->iter->hyp.pos[dim+1]=(-1);
                else
                    fhyper_info->iter->hyp.pos[dim+1]=regions[i].start+region_size;

                /* Decrement the iterator count */
                fhyper_info->iter->hyp.elmt_left-=region_size;
            } /* end for */
        } else { /* recurse on each region to next dimension down */
#ifdef QAK
printf("%s: check 3.0, num_regions=%d\n",FUNC,(int)num_regions);
#endif /* QAK */

            /* Increment the dimension we are working with */
            dim++;

            /* Step through each region in this dimension */
            for(i=0; i<num_regions && fhyper_info->nelmts>0; i++) {
                /* Step through each location in each region */
                for(j=regions[i].start; j<=regions[i].end && fhyper_info->nelmts>0; j++) {
#ifdef QAK
printf("%s: check 4.0, dim=%d, location=%d\n",FUNC,dim,j);
#endif /* QAK */

                    /* Set the correct position we are working on */
                    fhyper_info->iter->hyp.pos[dim]=j;

                    /* Go get the regions in the next lower dimension */
                    num_read+=H5S_hyper_mread(dim, fhyper_info);
                } /* end for */
            } /* end for */
        } /* end else */

        /* Free the region space */
        H5MM_xfree(regions);
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
size_t
H5S_hyper_mgath (const void *_buf, size_t elmt_size,
		const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		size_t nelmts, void *_tconv_buf/*out*/)
{
    H5S_hyper_bound_t **lo_bounds;    /* Lower (closest to the origin) bound array for each dimension */
    H5S_hyper_bound_t **hi_bounds;    /* Upper (farthest from the origin) bound array for each dimension */
    H5S_hyper_fhyper_info_t fhyper_info;  /* Block of parameters to pass into recursive calls */
    intn	i;
#ifdef QAK
    intn    j;            /* Counters		*/
#endif /* QAK */
    size_t  num_read;       /* number of elements read into buffer */

    FUNC_ENTER (H5S_hyper_mgath, 0);

#ifdef QAK
printf("%s: check 1.0, elmt_size=%d, mem_space=%p\n",FUNC,(int)elmt_size,mem_space);
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
printf("%s: check 2.0, mem_space->extent.u.simple.rank=%d\n",FUNC, (int)mem_space->extent.u.simple.rank);
#endif /* QAK */

    /* Allocate space for the low & high bound arrays */
    lo_bounds = H5MM_malloc(mem_space->extent.u.simple.rank * sizeof(H5S_hyper_bound_t *));
    hi_bounds = H5MM_malloc(mem_space->extent.u.simple.rank * sizeof(H5S_hyper_bound_t *));

    /* Initialize to correct order to walk through arrays.
        (When another iteration order besides the default 'C' order is chosen,
        this is the correct place to change the order of the array iterations)
    */
#ifdef QAK
printf("%s: check 3.0\n",FUNC);
#endif /* QAK */
    for(i=0; i<mem_space->extent.u.simple.rank; i++) {
        lo_bounds[i]=mem_space->select.sel_info.hyper_lst->lo_bounds[i];
        hi_bounds[i]=mem_space->select.sel_info.hyper_lst->hi_bounds[i];
#ifdef QAK
printf("%s: check 3.1, lo[%d]=%p, hi[%d]=%p\n",FUNC,i,lo_bounds[i],i,hi_bounds[i]);
        for(j=0; j<mem_space->select.sel_info.hyper_lst->count; j++)
printf("%s: check 3.2, lo[%d][%d]=%d, hi[%d][%d]=%d\n",FUNC,i,j,(int)lo_bounds[i][j].bound,i,j,(int)hi_bounds[i][j].bound);
#endif /* QAK */
    } /* end for */

    /* Initialize parameter block for recursive calls */
    fhyper_info.elmt_size=elmt_size;
    fhyper_info.space=mem_space;
    fhyper_info.iter=mem_iter;
    fhyper_info.nelmts=nelmts;
    fhyper_info.src=_buf;
    fhyper_info.dst=_tconv_buf;
    fhyper_info.lo_bounds=lo_bounds;
    fhyper_info.hi_bounds=hi_bounds;

    /* Recursively input the hyperslabs currently defined */
    /* starting with the slowest changing dimension */
#ifdef QAK
printf("%s: check 4.0\n",FUNC);
#endif /* QAK */
    num_read=H5S_hyper_mread(-1,&fhyper_info);
#ifdef QAK
printf("%s: check 5.0\n",FUNC);
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
H5S_hyper_mwrite (intn dim, H5S_hyper_fhyper_info_t *fhyper_info)
{
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];	/*size of memory buffer*/
    hssize_t	mem_offset[H5O_LAYOUT_NDIMS];	/*offset of slab in file*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hsize_t region_size;                /* Size of lowest region */
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    H5S_hyper_region_t *regions;  /* Pointer to array of hyperslab nodes overlapped */
    size_t num_regions;         /* number of regions overlapped */
    size_t i;                   /* Counters */
    intn j;
    size_t num_read=0;          /* Number of elements read */

    FUNC_ENTER (H5S_hyper_mwrite, 0);

    assert(fhyper_info);
#ifdef QAK
printf("%s: check 1.0\n",FUNC);
#endif /* QAK */

    /* Get a sorted list (in the next dimension down) of the regions which */
    /*  overlap the current index in this dim */
    if((regions=H5S_hyper_get_regions(&num_regions,dim,
            fhyper_info->space->select.sel_info.hyper_lst->count,
            fhyper_info->lo_bounds, fhyper_info->hi_bounds,
            fhyper_info->iter->hyp.pos))!=NULL) {

#ifdef QAK
printf("%s: check 2.0, rank=%d\n",FUNC,(int)fhyper_info->space->extent.u.simple.rank);
for(i=0; i<num_regions; i++)
    printf("%s: check 2.1, region #%d: start=%d, end=%d\n",FUNC,i,(int)regions[i].start,(int)regions[i].end);
#endif /* QAK */
        /* Check if this is the second to last dimension in dataset */
        /*  (Which means that we've got a list of the regions in the fastest */
        /*   changing dimension and should input those regions) */
        if((dim+2)==fhyper_info->space->extent.u.simple.rank) {

            /* Set up hyperslab I/O parameters which apply to all regions */

            /* Set up the size of the memory space */
            HDmemcpy(mem_size,fhyper_info->space->extent.u.simple.size,fhyper_info->space->extent.u.simple.rank*sizeof(hsize_t));
            mem_size[fhyper_info->space->extent.u.simple.rank]=fhyper_info->elmt_size;

            /* Copy the location of the region in the file */
            HDmemcpy(mem_offset,fhyper_info->iter->hyp.pos,fhyper_info->space->extent.u.simple.rank*sizeof(hssize_t));
            mem_offset[fhyper_info->space->extent.u.simple.rank]=0;

            /* Set the hyperslab size to copy */
            hsize[0]=1;
            H5V_array_fill(hsize,hsize,sizeof(hsize[0]),fhyper_info->space->extent.u.simple.rank);
            hsize[fhyper_info->space->extent.u.simple.rank]=fhyper_info->elmt_size;

            /* Set the memory offset to the origin */
            HDmemset (zero, 0, (fhyper_info->space->extent.u.simple.rank+1)*sizeof(*zero));

#ifdef QAK
printf("%s: check 3.0\n",FUNC);
#endif /* QAK */
            /* perform I/O on data from regions */
            for(i=0; i<num_regions && fhyper_info->nelmts>0; i++) {
                region_size=MIN(fhyper_info->nelmts,(regions[i].end-regions[i].start)+1);
                hsize[fhyper_info->space->extent.u.simple.rank-1]=region_size;
                mem_offset[fhyper_info->space->extent.u.simple.rank-1]=regions[i].start;

                /*
                 * Gather from memory.
                 */
                if (H5V_hyper_copy (fhyper_info->space->extent.u.simple.rank+1,
                        hsize, mem_size, mem_offset, fhyper_info->dst,
                        hsize, zero, fhyper_info->src)<0) {
                    HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, 0, "unable to gather data from memory");
                }

                /* Advance the pointer in the buffer */
                fhyper_info->src=((const uint8 *)fhyper_info->src)+region_size*fhyper_info->elmt_size;

                /* Increment the number of elements read */
                num_read+=region_size;

                /* Decrement the buffer left */
                fhyper_info->nelmts-=region_size;

                /* Set the next position to start at */
                if(region_size==(hsize_t)((regions[i].end-regions[i].start)+1))
                    fhyper_info->iter->hyp.pos[dim+1]=(-1);
                else
                    fhyper_info->iter->hyp.pos[dim+1]=regions[i].start+region_size;

                /* Decrement the iterator count */
                fhyper_info->iter->hyp.elmt_left-=region_size;
            } /* end for */
        } else { /* recurse on each region to next dimension down */

            /* Increment the dimension we are working with */
            dim++;

            /* Step through each region in this dimension */
            for(i=0; i<num_regions && fhyper_info->nelmts>0; i++) {
                /* Step through each location in each region */
                for(j=regions[i].start; j<=regions[i].end && fhyper_info->nelmts>0; j++) {

                    /* Set the correct position we are working on */
                    fhyper_info->iter->hyp.pos[dim]=j;

                    /* Go get the regions in the next lower dimension */
                    num_read+=H5S_hyper_mwrite(dim, fhyper_info);
                } /* end for */
            } /* end for */
        } /* end else */

        /* Free the region space */
        H5MM_xfree(regions);
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
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, June 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_hyper_mscat (const void *_tconv_buf, size_t elmt_size,
		const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		size_t nelmts, void *_buf/*out*/)
{
    H5S_hyper_bound_t **lo_bounds;    /* Lower (closest to the origin) bound array for each dimension */
    H5S_hyper_bound_t **hi_bounds;    /* Upper (farthest from the origin) bound array for each dimension */
    H5S_hyper_fhyper_info_t fhyper_info;  /* Block of parameters to pass into recursive calls */
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
    lo_bounds = H5MM_malloc(mem_space->extent.u.simple.rank * sizeof(H5S_hyper_bound_t *));
    hi_bounds = H5MM_malloc(mem_space->extent.u.simple.rank * sizeof(H5S_hyper_bound_t *));

    /* Initialize to correct order to walk through arrays.
        (When another iteration order besides the default 'C' order is chosen,
        this is the correct place to change the order of the array iterations)
    */
    for(i=0; i<mem_space->extent.u.simple.rank; i++) {
        lo_bounds[i]=mem_space->select.sel_info.hyper_lst->lo_bounds[i];
        hi_bounds[i]=mem_space->select.sel_info.hyper_lst->hi_bounds[i];
    } /* end for */

    /* Initialize parameter block for recursive calls */
    fhyper_info.elmt_size=elmt_size;
    fhyper_info.space=mem_space;
    fhyper_info.iter=mem_iter;
    fhyper_info.nelmts=nelmts;
    fhyper_info.src=_tconv_buf;
    fhyper_info.dst=_buf;
    fhyper_info.lo_bounds=lo_bounds;
    fhyper_info.hi_bounds=hi_bounds;

    /* Recursively input the hyperslabs currently defined */
    /* starting with the slowest changing dimension */
#ifdef QAK
printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    num_read=H5S_hyper_mwrite(-1,&fhyper_info);
#ifdef QAK
printf("%s: check 2.0\n",FUNC);
#endif /* QAK */

    /* Release the memory we allocated */
    H5MM_xfree(lo_bounds);
    H5MM_xfree(hi_bounds);

    FUNC_LEAVE (SUCCEED);
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
    intn lo, mid, hi;       /* Indices for the search */
    intn ret_value=-1;      /* Return value index */

    FUNC_ENTER (H5S_hyper_bsearch, FAIL);

    assert(barr);
    assert(count>0);

    /* Check bounds first */
    if(size<barr[0].bound)
        ret_value=0;
    else if(size>barr[count-1].bound)
        ret_value=count;
    else {      /* must be in the middle somewhere, go get it */
        lo=0;
        hi=count-1;
        do {
            /* Calc. the mid-point */
            mid=(hi+lo)/2;

            /* check for bounds only seperated by one element */
            if((hi-lo)<=1) {
                ret_value=hi;
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
    H5S_hyper_add
 PURPOSE
    Add a block to hyperslab selection
 USAGE
    herr_t H5S_hyper_add(space, start, size)
        H5S_t *space;       	  IN: Pointer to dataspace
        const hssize_t *start;    IN: Offset of block
        const hsize_t *size;      IN: Size of block
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Adds a block to an existing hyperslab selection.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_hyper_add (H5S_t *space, const hssize_t *start, const hsize_t *size)
{
    H5S_hyper_node_t *slab;     /* New hyperslab node to insert */
    H5S_hyper_bound_t *tmp;     /* Temporary pointer to an hyperslab bound array */
    intn bound_loc;             /* Boundary location to insert hyperslab */
    size_t elem_count;          /* Number of elements in hyperslab selection */
    intn i;     /* Counters */
    herr_t ret_value=FAIL;
#ifdef QAK
extern int qak_debug;
#endif /* QAK */

    FUNC_ENTER (H5S_hyper_add, FAIL);

    /* Check args */
    assert (space);
    assert (start);
    assert (size);

#ifdef QAK
qak_debug=1;
#endif /* QAK */

#ifdef QAK
printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    /* Create new hyperslab node to insert */
    if((slab = H5MM_malloc(sizeof(H5S_hyper_node_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
            "can't allocate hyperslab node");
    if((slab->start = H5MM_malloc(sizeof(hsize_t)*space->extent.u.simple.rank))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
            "can't allocate hyperslab start boundary");
    if((slab->end = H5MM_malloc(sizeof(hsize_t)*space->extent.u.simple.rank))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
            "can't allocate hyperslab end boundary");

#ifdef QAK
printf("%s: check 2.0\n",FUNC);
#endif /* QAK */
    /* Set boundary on new node */
    for(i=0,elem_count=1; i<space->extent.u.simple.rank; i++) {
#ifdef QAK
printf("%s: check 2.1, %d: start=%d, size=%d, elem_count=%d\n",FUNC,(int)i,(int)start[i],(int)size[i],(int)elem_count);
#endif /* QAK */
        slab->start[i]=start[i];
        slab->end[i]=start[i]+size[i]-1;
        elem_count*=size[i];
    } /* end for */

#ifdef QAK
printf("%s: check 3.0, lo_bounds=%p, hi_bounds=%p\n",FUNC,
        space->select.sel_info.hyper_lst->lo_bounds, space->select.sel_info.hyper_lst->hi_bounds);
#endif /* QAK */
    /* Increase size of boundary arrays for dataspace's selection */
    for(i=0; i<space->extent.u.simple.rank; i++) {
#ifdef QAK
printf("%s: check 3.1, i=%d\n",FUNC,(int)i);
#endif /* QAK */
        tmp=space->select.sel_info.hyper_lst->lo_bounds[i];
        if((space->select.sel_info.hyper_lst->lo_bounds[i]=H5MM_realloc(tmp,sizeof(H5S_hyper_bound_t)*(space->select.sel_info.hyper_lst->count+1)))==NULL) {
            space->select.sel_info.hyper_lst->lo_bounds[i]=tmp;
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate hyperslab lo boundary array");
        } /* end if */
#ifdef QAK
printf("%s: check 3.2, i=%d\n",FUNC,(int)i);
#endif /* QAK */
        tmp=space->select.sel_info.hyper_lst->hi_bounds[i];
        if((space->select.sel_info.hyper_lst->hi_bounds[i]=H5MM_realloc(tmp,sizeof(H5S_hyper_bound_t)*(space->select.sel_info.hyper_lst->count+1)))==NULL) {
            space->select.sel_info.hyper_lst->hi_bounds[i]=tmp;
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate hyperslab hi boundary array");
        } /* end if */
    } /* end for */

#ifdef QAK
printf("%s: check 4.0\n",FUNC);
#endif /* QAK */
    /* Insert each boundary of the hyperslab into the sorted lists of bounds */
    for(i=0; i<space->extent.u.simple.rank; i++) {
        /* Check if this is the first hyperslab inserted */
        if(space->select.sel_info.hyper_lst->count==0) {
            space->select.sel_info.hyper_lst->lo_bounds[i][0].bound=slab->start[i];
            space->select.sel_info.hyper_lst->lo_bounds[i][0].node=slab;
            space->select.sel_info.hyper_lst->hi_bounds[i][0].bound=slab->end[i];
            space->select.sel_info.hyper_lst->hi_bounds[i][0].node=slab;
        } /* end if */
        else {
            /* Take care of the low boundary first */
            /* Find the location to insert in front of */
            if((bound_loc=H5S_hyper_bsearch(slab->start[i],space->select.sel_info.hyper_lst->lo_bounds[i],
                    space->select.sel_info.hyper_lst->count))<0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                    "can't find location to insert hyperslab boundary");

            /* Check if we need to move boundary elements */
            if(bound_loc!=(intn)space->select.sel_info.hyper_lst->count) {
                HDmemmove(space->select.sel_info.hyper_lst->lo_bounds[bound_loc+1],
                    space->select.sel_info.hyper_lst->lo_bounds[bound_loc],
                    sizeof(H5S_hyper_bound_t)*(space->select.sel_info.hyper_lst->count-bound_loc));
            } /* end if */
            space->select.sel_info.hyper_lst->lo_bounds[bound_loc][i].bound=slab->start[i];
            space->select.sel_info.hyper_lst->lo_bounds[bound_loc][i].node=slab;

            /* Take care of the high boundary next */
            /* Find the location to insert in front of */
            if((bound_loc=H5S_hyper_bsearch(slab->end[i],space->select.sel_info.hyper_lst->hi_bounds[i],
                    space->select.sel_info.hyper_lst->count))<0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                    "can't find location to insert hyperslab boundary");

            /* Check if we need to move boundary elements */
            if(bound_loc!=(intn)space->select.sel_info.hyper_lst->count) {
                HDmemmove(space->select.sel_info.hyper_lst->hi_bounds[bound_loc+1],
                    space->select.sel_info.hyper_lst->hi_bounds[bound_loc],
                    sizeof(H5S_hyper_bound_t)*(space->select.sel_info.hyper_lst->count-bound_loc));
            } /* end if */
            space->select.sel_info.hyper_lst->hi_bounds[bound_loc][i].bound=slab->end[i];
            space->select.sel_info.hyper_lst->hi_bounds[bound_loc][i].node=slab;
        } /* end else */
    } /* end for */
#ifdef QAK
printf("%s: check 5.0\n",FUNC);
#endif /* QAK */

    /* Increment the number of bounds in the array */
    space->select.sel_info.hyper_lst->count++;
    
    /* Prepend on list of hyperslabs for this selection */
    slab->next=space->select.sel_info.hyper_lst->head;
    space->select.sel_info.hyper_lst->head=slab;

    /* Increment the number of elements in the hyperslab selection */
    space->select.num_elem+=elem_count;
#ifdef QAK
printf("%s: check 6.0\n",FUNC);
#endif /* QAK */

done:
    FUNC_LEAVE (SUCCEED);
}   /* H5S_hyper_add() */

/*--------------------------------------------------------------------------
 NAME
    H5S_hyper_release
 PURPOSE
    Release hyperslab selection information for a dataspace
 USAGE
    herr_t H5S_hyper_release(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Releases all hyperslab selection information for a dataspace
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
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

    /* Release hi and lo boundary information */
    for(i=0; i<space->extent.u.simple.rank; i++) {
        H5MM_xfree(space->select.sel_info.hyper_lst->lo_bounds[i]);
        H5MM_xfree(space->select.sel_info.hyper_lst->hi_bounds[i]);
    } /* end for */
    H5MM_xfree(space->select.sel_info.hyper_lst->lo_bounds);
    H5MM_xfree(space->select.sel_info.hyper_lst->hi_bounds);

    /* Release list of selected regions */
    curr=space->select.sel_info.hyper_lst->head;
    while(curr!=NULL) {
        next=curr->next;
        H5MM_xfree(curr->start);
        H5MM_xfree(curr->end);
        H5MM_xfree(curr);
        curr=next;
    } /* end while */

    /* Release hyperslab selection node itself */
    H5MM_xfree(space->select.sel_info.hyper_lst);
    space->select.sel_info.hyper_lst=NULL;

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
H5S_point_npoints (const H5S_t *space)
{
    FUNC_ENTER (H5S_point_npoints, 0);

    /* Check args */
    assert (space);

    FUNC_LEAVE (space->select.num_elem);
}   /* H5S_point_npoints() */

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
    SUCCEED/FAIL
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
