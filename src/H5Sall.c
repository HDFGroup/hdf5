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
#include <H5Sprivate.h>
#include <H5Vprivate.h>

/* Interface initialization */
#define PABLO_MASK      H5S_all_mask
#define INTERFACE_INIT  NULL
static intn             interface_initialize_g = FALSE;

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
herr_t
H5S_all_init (const struct H5O_layout_t __unused__ *layout,
	       const H5S_t *space, H5S_sel_iter_t *sel_iter)
{
    FUNC_ENTER (H5S_all_init, FAIL);

    /* Check args */
    assert (layout);
    assert (space && H5S_SEL_ALL==space->select.type);
    assert (sel_iter);

    /* Initialize the number of elements to iterate over */
    sel_iter->all.elmt_left=H5S_extent_npoints(space);

    /* Start at the upper left location */
    sel_iter->all.offset=0;
    
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
size_t
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
    m_ndims = H5S_extent_dims (space, size, NULL);
    for (i=m_ndims-1, acc=1; i>0; --i)
        acc *= size[i];
    nelmts = (max/acc) * acc;
    if (nelmts<=0) {
        HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, 0,
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
size_t
H5S_all_fgath (H5F_t *f, const struct H5O_layout_t *layout,
	       const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
	       size_t elmt_size, const H5S_t *file_space,
	       H5S_sel_iter_t *file_iter, size_t nelmts,
	       const H5D_transfer_t xfer_mode, void *_buf/*out*/)
{
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of slab in file*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    uint8 *buf=(uint8 *)_buf;   /* Alias for pointer arithmetic */
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
    if ((space_ndims=H5S_extent_dims (file_space, hsize, NULL))<0) {
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
    if (H5F_arr_read (f, layout, comp, efl, hsize, hsize, zero, file_offset,
		      xfer_mode, buf/*out*/)<0) {
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
H5S_all_fscat (H5F_t *f, const struct H5O_layout_t *layout,
	       const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
	       size_t elmt_size, const H5S_t *file_space,
	       H5S_sel_iter_t *file_iter, size_t nelmts,
	       const H5D_transfer_t xfer_mode, const void *_buf)
{
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of hyperslab	*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero vector		*/
    const uint8 *buf=(const uint8 *)_buf;   /* Alias for pointer arithmetic */
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
    if ((space_ndims=H5S_extent_dims (file_space, hsize, NULL))<0) {
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
    if (H5F_arr_write (f, layout, comp, efl, hsize, hsize, zero,
		       file_offset, xfer_mode, buf)<0) {
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
size_t
H5S_all_mgath (const void *_buf, size_t elmt_size,
	       const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
	       size_t nelmts, void *_tconv_buf/*out*/)
{
    hssize_t	mem_offset[H5O_LAYOUT_NDIMS];	/*slab offset in app buf*/
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];	/*total size of app buf	*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    const uint8 *buf=(const uint8 *)_buf;   /* Get local copies for address arithmetic */
    uint8 *tconv_buf=(uint8 *)_tconv_buf;
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
    if ((space_ndims=H5S_extent_dims (mem_space, hsize, NULL))<0) {
        HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve hyperslab parameters");
    }
    if(space_ndims>0) {
        HDmemset(mem_offset,0,sizeof(hssize_t)*space_ndims);

        if (H5S_extent_dims (mem_space, mem_size, NULL)<0) {
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
H5S_all_mscat (const void *_tconv_buf, size_t elmt_size,
	       const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
	       size_t nelmts, void *_buf/*out*/)
{
    hssize_t	mem_offset[H5O_LAYOUT_NDIMS];	/*slab offset in app buf*/
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];	/*total size of app buf	*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    uint8 *buf=(uint8 *)_buf;   /* Get local copies for address arithmetic */
    const uint8 *tconv_buf=(const uint8 *)_tconv_buf;
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
    if ((space_ndims=H5S_extent_dims (mem_space, hsize, NULL))<0) {
        HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }

    if(space_ndims>0) {
        HDmemset(mem_offset,0,sizeof(hssize_t)*space_ndims);

        if (H5S_extent_dims (mem_space, mem_size, NULL)<0) {
            HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
                   "unable to retrieve data space dimensions");
        }

        /* Adjust the slowest varying dimension to take care of strip mining */
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
    if (H5V_hyper_copy (space_ndims+1, hsize, mem_size, mem_offset, buf,
			hsize, zero, tconv_buf)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to scatter data to memory");
    }

    /* Advance iterator */
    mem_iter->all.elmt_left--;
    mem_iter->all.offset+=nelmts;
    
    FUNC_LEAVE (SUCCEED);
}   /* H5S_all_mscat() */

/*--------------------------------------------------------------------------
 NAME
    H5S_all_release
 PURPOSE
    Release all selection information for a dataspace
 USAGE
    herr_t H5S_all_release(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Releases "all" selection information for a dataspace
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_release (H5S_t *space)
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
