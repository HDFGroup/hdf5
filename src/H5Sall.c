/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, June 16, 1998
 *
 * Purpose:	"All" selection data space I/O functions.
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Iprivate.h>
#include <H5Sprivate.h>
#include <H5Vprivate.h>
#include <H5Dprivate.h>

/* Interface initialization */
#define PABLO_MASK      H5Sall_mask
#define INTERFACE_INIT  NULL
static intn             interface_initialize_g = 0;

static herr_t H5S_all_init (const struct H5O_layout_t *layout,
			    const H5S_t *space, H5S_sel_iter_t *iter, size_t *min_elem_out);
static size_t H5S_all_favail (const H5S_t *space, const H5S_sel_iter_t *iter,
			      size_t max);
static size_t H5S_all_fgath (H5F_t *f, const struct H5O_layout_t *layout,
			     const struct H5O_pline_t *pline,
			     const struct H5O_fill_t *fill,
			     const struct H5O_efl_t *efl, size_t elmt_size,
			     const H5S_t *file_space,
			     H5S_sel_iter_t *file_iter, size_t nelmts,
			     const H5F_xfer_t *xfer_parms, void *buf/*out*/);
static herr_t H5S_all_fscat (H5F_t *f, const struct H5O_layout_t *layout,
			     const struct H5O_pline_t *pline,
			     const struct H5O_fill_t *fill,
			     const struct H5O_efl_t *efl, size_t elmt_size,
			     const H5S_t *file_space,
			     H5S_sel_iter_t *file_iter, size_t nelmts,
			     const H5F_xfer_t *xfer_parms, const void *buf);
static size_t H5S_all_mgath (const void *_buf, size_t elmt_size,
			     const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
			     size_t nelmts, void *_tconv_buf/*out*/);
static herr_t H5S_all_mscat (const void *_tconv_buf, size_t elmt_size,
			     const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
			     size_t nelmts, void *_buf/*out*/);

const H5S_fconv_t	H5S_ALL_FCONV[1] = {{
    "all", 					/*name			*/
    H5S_SEL_ALL,				/*selection type	*/
    H5S_all_init,				/*initialize		*/
    H5S_all_favail,				/*available		*/
    H5S_all_fgath,				/*gather		*/
    H5S_all_fscat,				/*scatter		*/
}};

const H5S_mconv_t	H5S_ALL_MCONV[1] = {{
    "all", 					/*name			*/
    H5S_SEL_ALL,				/*selection type	*/
    H5S_all_init,				/*initialize		*/
    H5S_all_mgath,				/*gather		*/
    H5S_all_mscat, 				/*scatter		*/
}};

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
static herr_t
H5S_all_init (const struct H5O_layout_t UNUSED *layout,
	       const H5S_t *space, H5S_sel_iter_t *sel_iter, size_t *min_elem_out)
{
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    intn	space_ndims;			/*dimensionality of space*/
    hsize_t	acc;				    /*accumulator		*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5S_all_init, FAIL);

    /* Check args */
    assert (layout);
    assert (space && H5S_SEL_ALL==space->select.type);
    assert (sel_iter);

    /* Initialize the number of elements to iterate over */
    sel_iter->all.elmt_left=H5S_get_simple_extent_npoints(space);

    /* Start at the upper left location */
    sel_iter->all.offset=0;

    /* Get the dimensions of the space, to set the min. # of elements */
    if ((space_ndims=H5S_get_simple_extent_dims (space, hsize, NULL))<0) {
        HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }

    /* Adjust the slowest varying dimension to account for strip mining */
    for (i=1, acc=1; i<space_ndims; i++)
        acc *= hsize[i];
    
    /* Set the minimum # of elements to output */
    *min_elem_out=acc;
    
    FUNC_LEAVE (SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_all_favail
 *
 * Purpose:	Figure out the optimal number of elements to transfer to/from
 *		the file.
 *
 * Return:	non-negative number of elements on success, negative on
 *		failure.
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5S_all_favail (const H5S_t *space, const H5S_sel_iter_t *sel_iter, size_t max)
{
    hsize_t	nelmts;
    int		m_ndims;	/* file dimensionality	*/
    hsize_t	size[H5O_LAYOUT_NDIMS];	/*size of selected hyperslab	*/
    hsize_t	acc;
    int		i;

    FUNC_ENTER (H5S_all_favail, FAIL);

    /* Check args */
    assert (space && H5S_SEL_ALL==space->select.type);
    assert (sel_iter);

    /*
     * The stripmine size is such that only the slowest varying dimension can
     * be split up.  We choose the largest possible strip mine size which is
     * not larger than the desired size.
     */
    m_ndims = H5S_get_simple_extent_dims (space, size, NULL);
    for (i=m_ndims-1, acc=1; i>0; --i)
        acc *= size[i];
    nelmts = (max/acc) * acc;
    if (nelmts<=0) {
        HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
		       "strip mine buffer is too small");
    }

    FUNC_LEAVE (MIN(sel_iter->all.elmt_left,nelmts));
}   /* H5S_all_favail() */

/*-------------------------------------------------------------------------
 * Function:	H5S_all_fgath
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
H5S_all_fgath (H5F_t *f, const struct H5O_layout_t *layout,
	       const struct H5O_pline_t *pline,
	       const struct H5O_fill_t *fill, const struct H5O_efl_t *efl,
	       size_t elmt_size, const H5S_t *file_space,
	       H5S_sel_iter_t *file_iter, size_t nelmts,
	       const H5F_xfer_t *xfer_parms, void *_buf/*out*/)
{
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of slab in file*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    uint8_t	*buf=(uint8_t*)_buf;		/*for pointer arithmetic*/
    hsize_t	acc;				/*accumulator		*/
    intn	space_ndims;			/*dimensionality of space*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5S_all_fgath, 0);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (buf);

    /*
     * Get hyperslab information to determine what elements are being
     * selected (there might eventually be other selection methods too).
     * We only support hyperslabs with unit sample because there's no way to
     * currently pass sample information into H5F_arr_read() much less
     * H5F_istore_read().
     */
    if ((space_ndims=H5S_get_simple_extent_dims (file_space, hsize, NULL))<0) {
        HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve hyperslab parameters");
    }

    if(space_ndims>0) {
        HDmemset(file_offset,0,sizeof(hssize_t)*space_ndims);

        /* Adjust the slowest varying dimension to take care of strip mining */
        for (i=1, acc=1; i<space_ndims; i++)
            acc *= hsize[i];
        assert (0==file_iter->all.offset % acc);
        assert (0==nelmts % acc);
        file_offset[0] += file_iter->all.offset / acc;
        hsize[0] = nelmts / acc;
    } /* end if */

    /* The fastest varying dimension is for the data point itself */
    file_offset[space_ndims] = 0;
    hsize[space_ndims] = elmt_size;
    HDmemset (zero, 0, (space_ndims+1)*sizeof(*zero));

    /*
     * Gather from file.
     */
    if (H5F_arr_read (f, xfer_parms, layout, pline, fill, efl, hsize, hsize,
		      zero, file_offset, buf/*out*/)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, 0, "read error");
    }

    /* Advance iterator */
    file_iter->all.elmt_left--;
    file_iter->all.offset+=nelmts;
    
    FUNC_LEAVE (nelmts);
} /* H5S_all_fgath() */

/*-------------------------------------------------------------------------
 * Function:	H5S_all_fscat
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
H5S_all_fscat (H5F_t *f, const struct H5O_layout_t *layout,
	       const struct H5O_pline_t *pline, const struct H5O_fill_t *fill,
	       const struct H5O_efl_t *efl, size_t elmt_size,
	       const H5S_t *file_space, H5S_sel_iter_t *file_iter,
	       size_t nelmts, const H5F_xfer_t *xfer_parms, const void *_buf)
{
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of hyperslab	*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero vector		*/
    const uint8_t *buf=(const uint8_t*)_buf;    /*for pointer arithmetic*/
    hsize_t	acc;				/*accumulator		*/
    intn	space_ndims;			/*space dimensionality	*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5S_all_fscat, FAIL);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (buf);

    /*
     * Get information to determine what elements are being selected.
     */
    if ((space_ndims=H5S_get_simple_extent_dims (file_space, hsize, NULL))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }
    
    if(space_ndims>0) {
        HDmemset(file_offset,0,sizeof(hssize_t)*space_ndims);

        /* Adjust the slowest varying dimension to account for strip mining */
        for (i=1, acc=1; i<space_ndims; i++)
            acc *= hsize[i];
        assert (0==file_iter->all.offset % acc);
        assert (0==nelmts % acc);
        file_offset[0] += file_iter->all.offset / acc;
        hsize[0] = nelmts / acc;
    } /* end if */
    
    /* The fastest varying dimension is for the data point itself */
    file_offset[space_ndims] = 0;
    hsize[space_ndims] = elmt_size;
    HDmemset (zero, 0, (space_ndims+1)*sizeof(*zero));

    /*
     * Scatter to file.
     */
    if (H5F_arr_write (f, xfer_parms, layout, pline, fill, efl, hsize, hsize,
		       zero, file_offset, buf)<0) {
        HRETURN_ERROR (H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error");
    }

    /* Advance iterator */
    file_iter->all.elmt_left--;
    file_iter->all.offset+=nelmts;
    
    FUNC_LEAVE (SUCCEED);
}   /* H5S_all_fscat() */

/*-------------------------------------------------------------------------
 * Function:	H5S_all_mgath
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
H5S_all_mgath (const void *_buf, size_t elmt_size,
	       const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
	       size_t nelmts, void *_tconv_buf/*out*/)
{
    hssize_t	mem_offset[H5O_LAYOUT_NDIMS];	/*slab offset in app buf*/
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];	/*total size of app buf	*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    const uint8_t *buf=(const uint8_t*)_buf;   /* Get local copies for address arithmetic */
    uint8_t	*tconv_buf=(uint8_t*)_tconv_buf;
    hsize_t	acc;				/*accumulator		*/
    intn	space_ndims;			/*dimensionality of space*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5S_all_mgath, 0);

    /* Check args */
    assert (buf);
    assert (elmt_size>0);
    assert (mem_space && H5S_SEL_ALL==mem_space->select.type);
    assert (mem_iter);
    assert (nelmts>0);
    assert (tconv_buf);

    /*
     * Retrieve information to determine what elements are being selected.
     */
    if ((space_ndims=H5S_get_simple_extent_dims (mem_space, hsize, NULL))<0) {
        HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve hyperslab parameters");
    }
    if(space_ndims>0) {
        HDmemset(mem_offset,0,sizeof(hssize_t)*space_ndims);

        if (H5S_get_simple_extent_dims (mem_space, mem_size, NULL)<0) {
            HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
                   "unable to retrieve data space dimensions");
        }

        /* Adjust the slowest varying dimension to account for strip mining */
        for (i=1, acc=1; i<space_ndims; i++)
            acc *= hsize[i];
        assert (0==mem_iter->all.offset % acc);
        assert (0==nelmts % acc);
        mem_offset[0] += mem_iter->all.offset / acc;
        hsize[0] = nelmts / acc;
    } /* end if */
    
    /* The fastest varying dimension is for the data point itself */
    mem_offset[space_ndims] = 0;
    mem_size[space_ndims] = elmt_size;
    hsize[space_ndims] = elmt_size;
    HDmemset (zero, 0, (space_ndims+1)*sizeof(*zero));

    /*
     * Scatter from conversion buffer to application memory.
     */
    if (H5V_hyper_copy (space_ndims+1, hsize, hsize, zero, tconv_buf,
			mem_size, mem_offset, buf)<0) {
        HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to scatter data to memory");
    }

    /* Advance iterator */
    mem_iter->all.elmt_left--;
    mem_iter->all.offset+=nelmts;
    
    FUNC_LEAVE (nelmts);
}   /* H5S_all_mgath() */

/*-------------------------------------------------------------------------
 * Function:	H5S_all_mscat
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
H5S_all_mscat (const void *_tconv_buf, size_t elmt_size,
	       const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
	       size_t nelmts, void *_buf/*out*/)
{
    hssize_t	mem_offset[H5O_LAYOUT_NDIMS];	/*slab offset in app buf*/
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];	/*total size of app buf	*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    uint8_t	*buf=(uint8_t*)_buf;   /* Get local copies for address arithmetic */
    const uint8_t *tconv_buf=(const uint8_t *)_tconv_buf;
    hsize_t	acc;				/*accumulator		*/
    intn	space_ndims;			/*dimensionality of space*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5S_all_mscat, FAIL);

    /* Check args */
    assert (tconv_buf);
    assert (elmt_size>0);
    assert (mem_space && H5S_SEL_ALL==mem_space->select.type);
    assert (mem_iter);
    assert (nelmts>0);
    assert (buf);

    /*
     * Retrieve information to determine what elements are being selected.
     */
    if ((space_ndims=H5S_get_simple_extent_dims (mem_space, hsize, NULL))<0) {
        HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }

    if(space_ndims>0) {
        HDmemset(mem_offset,0,sizeof(hssize_t)*space_ndims);

        if (H5S_get_simple_extent_dims (mem_space, mem_size, NULL)<0) {
            HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
                   "unable to retrieve data space dimensions");
        }

        /* Adjust the slowest varying dimension to take care of strip mining */
        for (i=1, acc=1; i<space_ndims; i++) {
            acc *= hsize[i];
        }
        assert (0==mem_iter->all.offset % acc);
        assert (0==nelmts % acc);
        mem_offset[0] += mem_iter->all.offset / acc;
        hsize[0] = nelmts / acc;
    } /* end if */

    /* The fastest varying dimension is for the data point itself */
    mem_offset[space_ndims] = 0;
    mem_size[space_ndims] = elmt_size;
    hsize[space_ndims] = elmt_size;
    HDmemset (zero, 0, (space_ndims+1)*sizeof(*zero));

    /*
     * Scatter from conversion buffer to application memory.
     */
    if (H5V_hyper_copy (space_ndims+1, hsize, mem_size, mem_offset, buf,
			hsize, zero, tconv_buf)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to scatter data to memory");
    }

    /* Advance iterator */
    mem_iter->all.elmt_left--;
    mem_iter->all.offset+=nelmts;
    
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_all_read
 *
 * Purpose:	Reads directly from file into application memory if possible.
 *
 * Return:	Success:	Non-negative. If data was read directly then
 *				MUST_CONVERT is set to zero, otherwise
 *				MUST_CONVERT is set to non-zero.
 *
 *		Failure:	Negative. Return value of MUST_CONVERT is
 *				undefined.
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 22, 1999
 *
 * Modifications:
 *    Modified to allow contiguous hyperslabs to be written out - QAK - 5/25/99
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_all_read(H5F_t *f, const H5O_layout_t *layout, const H5O_pline_t *pline,
	     const H5O_efl_t *efl, size_t elmt_size, const H5S_t *file_space,
	     const H5S_t *mem_space, const H5F_xfer_t *xfer_parms,
	     void *buf/*out*/, hbool_t *must_convert/*out*/)
{
    H5S_hyper_node_t *file_node,*mem_node;     /* Hyperslab node */
    hsize_t	mem_size,file_size;
    hssize_t	file_off,mem_off;
    hsize_t	size[H5S_MAX_RANK];
    hssize_t	file_offset[H5S_MAX_RANK];
    hssize_t	mem_offset[H5S_MAX_RANK];
    int		i;

    FUNC_ENTER(H5S_all_read, FAIL);
    *must_convert = TRUE;

    /* Check whether we can handle this */
    if (H5S_SIMPLE!=mem_space->extent.type) goto fall_through;
    if (H5S_SIMPLE!=file_space->extent.type) goto fall_through;
    if (mem_space->extent.u.simple.rank!=
	file_space->extent.u.simple.rank) goto fall_through;
    if (mem_space->select.type==H5S_SEL_HYPERSLABS) {
	if(mem_space->select.sel_info.hslab.hyper_lst->count>1) goto fall_through;
	mem_node=mem_space->select.sel_info.hslab.hyper_lst->head;
    } /* end if */
    else
    	if(mem_space->select.type!=H5S_SEL_ALL) goto fall_through;
    if (file_space->select.type==H5S_SEL_HYPERSLABS) {
	if(file_space->select.sel_info.hslab.hyper_lst->count>1) goto fall_through;
	file_node=file_space->select.sel_info.hslab.hyper_lst->head;
    } /* end if */
    else
    	if(file_space->select.type!=H5S_SEL_ALL) goto fall_through;

    /* Get information about memory and file */
    for (i=0; i<mem_space->extent.u.simple.rank; i++) {
	if (mem_space->extent.u.simple.max &&
	    mem_space->extent.u.simple.size[i]!=
	    mem_space->extent.u.simple.max[i]) goto fall_through;
	if (file_space->extent.u.simple.max &&
	    file_space->extent.u.simple.size[i]!=
	    file_space->extent.u.simple.max[i]) goto fall_through;
	if(mem_space->select.type==H5S_SEL_HYPERSLABS) {
	    mem_size=(mem_node->end[i]-mem_node->start[i])+1;
        mem_off=mem_node->start[i];
	} /* end if */
	else {
	    mem_size=mem_space->extent.u.simple.size[i];
        mem_off=0;
	} /* end else */
	if(file_space->select.type==H5S_SEL_HYPERSLABS) {
	    file_size=(file_node->end[i]-file_node->start[i])+1;
	    file_off=file_node->start[i];
	} /* end if */
	else {
	    file_size=file_space->extent.u.simple.size[i];
	    file_off=0;
	} /* end else */
	if (mem_size!=file_size) goto fall_through;
	size[i] = file_size;
	file_offset[i] = file_off;
	mem_offset[i] = mem_off;
    }
    size[i] = elmt_size;
    file_offset[i] = 0;
    mem_offset[i] = 0;

    /* Read data from the file */
    if (H5F_arr_read(f, xfer_parms, layout, pline, NULL, efl, size,
		     size, mem_offset, file_offset, buf/*out*/)<0) {
	HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
		      "unable to read data from the file");
    }
    *must_convert = FALSE;

 fall_through:
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_all_write
 *
 * Purpose:	Writes data directly to the file if possible.
 *
 * Return:	Success:	Non-negative. If data was written directly
 *				then MUST_CONVERT is set to zero, otherwise
 *				MUST_CONVERT is set to non-zero.
 *
 *		Failure:	Negative. Return value of MUST_CONVERT is
 *				undefined.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 21, 1999
 *
 * Modifications:
 *    Modified to allow contiguous hyperslabs to be written out - QAK - 5/25/99
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_all_write(H5F_t *f, const struct H5O_layout_t *layout,
	      const H5O_pline_t *pline, const H5O_efl_t *efl,
	      size_t elmt_size, const H5S_t *file_space,
	      const H5S_t *mem_space, const H5F_xfer_t *xfer_parms,
	      const void *buf, hbool_t *must_convert/*out*/)
{
    H5S_hyper_node_t *file_node,*mem_node;     /* Hyperslab node */
    hsize_t	mem_size,file_size;
    hssize_t	file_off,mem_off;
    hsize_t	size[H5S_MAX_RANK];
    hssize_t	file_offset[H5S_MAX_RANK];
    hssize_t	mem_offset[H5S_MAX_RANK];
    int		i;
    
    FUNC_ENTER(H5S_all_write, FAIL);
    *must_convert = TRUE;

    /* Check whether we can handle this */
    if (H5S_SIMPLE!=mem_space->extent.type) goto fall_through;
    if (H5S_SIMPLE!=file_space->extent.type) goto fall_through;
    if (mem_space->extent.u.simple.rank!=
	file_space->extent.u.simple.rank) goto fall_through;
    if (mem_space->select.type==H5S_SEL_HYPERSLABS) {
	if(mem_space->select.sel_info.hslab.hyper_lst->count>1) goto fall_through;
	mem_node=mem_space->select.sel_info.hslab.hyper_lst->head;
    } /* end if */
    else
    	if(mem_space->select.type!=H5S_SEL_ALL) goto fall_through;
    if (file_space->select.type==H5S_SEL_HYPERSLABS) {
	if(file_space->select.sel_info.hslab.hyper_lst->count>1) goto fall_through;
	file_node=file_space->select.sel_info.hslab.hyper_lst->head;
    } /* end if */
    else
    	if(file_space->select.type!=H5S_SEL_ALL) goto fall_through;

    /* Get information about memory and file */
    for (i=0; i<mem_space->extent.u.simple.rank; i++) {
	if (mem_space->extent.u.simple.max &&
	    mem_space->extent.u.simple.size[i]!=
	    mem_space->extent.u.simple.max[i]) goto fall_through;
	if (file_space->extent.u.simple.max &&
	    file_space->extent.u.simple.size[i]!=
	    file_space->extent.u.simple.max[i]) goto fall_through;
	if(mem_space->select.type==H5S_SEL_HYPERSLABS) {
	    mem_size=(mem_node->end[i]-mem_node->start[i])+1;
        mem_off=mem_node->start[i];
	} /* end if */
	else {
	    mem_size=mem_space->extent.u.simple.size[i];
        mem_off=0;
	} /* end else */
	if(file_space->select.type==H5S_SEL_HYPERSLABS) {
	    file_size=(file_node->end[i]-file_node->start[i])+1;
	    file_off=file_node->start[i];
	} /* end if */
	else {
	    file_size=file_space->extent.u.simple.size[i];
	    file_off=0;
	} /* end else */
	if (mem_size!=file_size) goto fall_through;
	size[i] = file_size;
	file_offset[i] = file_off;
	mem_offset[i] = mem_off;
    }
    size[i] = elmt_size;
    file_offset[i] = 0;
    mem_offset[i] = 0;

    /* Write data to the file */
    if (H5F_arr_write(f, xfer_parms, layout, pline, NULL, efl, size,
		      size, mem_offset, file_offset, buf)<0) {
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
		      "unable to write data to the file");
    }
    *must_convert = FALSE;


 fall_through:
    FUNC_LEAVE(SUCCEED);
}


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
H5S_all_release (H5S_t UNUSED *space)
{
    FUNC_ENTER (H5S_all_release, FAIL);

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
    intn i;     /* Counters */
    hsize_t ret_value;

    FUNC_ENTER (H5S_all_npoints, 0);

    /* Check args */
    assert (space);

    for(i=0, ret_value=1; i<space->extent.u.simple.rank; i++)
        ret_value*=space->extent.u.simple.size[i];
    
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

    FUNC_ENTER (H5S_all_select_serialize, FAIL);

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

    FUNC_ENTER (H5S_all_select_deserialize, FAIL);

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
    intn rank;                  /* Dataspace rank */
    intn i;                     /* index variable */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5S_all_bounds, FAIL);

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
    H5S_all_select_iterate
 PURPOSE
    Iterate over a "all" selection, calling a user's function for each
        element.
 USAGE
    herr_t H5S_all_select_iterate(buf, type_id, space, operator, operator_data)
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
H5S_all_select_iterate(void *buf, hid_t type_id, H5S_t *space, H5D_operator_t operator,
        void *operator_data)
{
    hsize_t	mem_size[H5O_LAYOUT_NDIMS]; /* Dataspace size */
    hsize_t	mem_offset[H5O_LAYOUT_NDIMS]; /* current coordinates */
    hsize_t offset;             /* offset of region in buffer */
    hsize_t nelemts;            /* Number of elements to iterate through */
    void *tmp_buf;              /* temporary location of the element in the buffer */
    intn rank;              /* Dataspace rank */
    intn index;             /* Index to increment */
    herr_t ret_value=0;     /* return value */

    FUNC_ENTER (H5S_all_select_iterate, 0);

    assert(buf);
    assert(space);
    assert(operator);
    assert(H5I_DATATYPE == H5I_get_type(type_id));

    /* Get the dataspace extent rank */
    rank=space->extent.u.simple.rank;

    /* Set up the size of the memory space */
    HDmemcpy(mem_size, space->extent.u.simple.size, rank*sizeof(hsize_t));
    mem_size[rank]=H5Tget_size(type_id);

    /* Set the coordinates to zero */
    HDmemset(mem_offset, 0, (rank+1)*sizeof(hsize_t));

    /* Get the number of elements to iterate through */
    nelemts=H5S_get_simple_extent_npoints(space);

    /* Iterate through the entire dataset */
    while(nelemts>0 && ret_value==0) {
        /* Get the offset in the memory buffer */
        offset=H5V_array_offset(rank+1,mem_size,mem_offset);
        tmp_buf=((char *)buf+offset);

        ret_value=(*operator)(tmp_buf,type_id,rank,mem_offset,operator_data);

        /* Advance the coordinate (currently in C memory order) */
        index=rank-1; /* Leave the byte offset in the element alone */
        while(++mem_offset[index]==mem_size[index] && index>=0) {
            mem_offset[index]=0;
            index--;
          } /* end while */

        /* Decrement the number of elements to iterate through */
        nelemts--;
      } /* end while */

    FUNC_LEAVE (ret_value);
}   /* H5S_all_select_iterate() */
