/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Wednesday, January 21, 1998
 *
 * Purpose:	Simple data space functions.
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Sprivate.h>
#include <H5Vprivate.h>

/* Interface initialization */
#define PABLO_MASK      H5S_simp_mask
#define INTERFACE_INIT  NULL
static intn             interface_initialize_g = FALSE;

/*-------------------------------------------------------------------------
 * Function:	H5S_simp_init
 *
 * Purpose:	Generates element numbering information for the data
 *		spaces involved in a data space conversion.
 *
 * Return:	Success:	Number of elements that can be efficiently
 *				transferred at a time.
 *
 *		Failure:	Zero
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5S_simp_init (const struct H5O_layout_t *layout, const H5S_t *mem_space,
	       const H5S_t *file_space, size_t desired_nelmts,
	       H5S_number_t *numbering/*out*/)
{
    size_t	nelmts;
    int		m_ndims, f_ndims;	/*mem, file dimensionality	*/
    size_t	size[H5O_LAYOUT_NDIMS];	/*size of selected hyperslab	*/
    size_t	acc;
    int		i;
    
    FUNC_ENTER (H5S_simp_init, 0);

    /* Check args */
    assert (layout);
    assert (mem_space && H5S_SIMPLE==mem_space->type);
    assert (file_space && H5S_SIMPLE==file_space->type);
    assert (numbering);

    /* Numbering is implied by the hyperslab, C order, no data here */
    HDmemset (numbering, 0, sizeof(H5S_number_t));

    /*
     * The stripmine size is such that only the slowest varying dimension can
     * be split up.  We choose the largest possible strip mine size which is
     * not larger than the desired size.
     */
    m_ndims = H5S_get_hyperslab (mem_space, NULL, size, NULL);
    for (i=m_ndims-1, acc=1; i>0; --i) acc *= size[i];
    nelmts = (desired_nelmts/acc) * acc;
    if (nelmts<=0) {
	HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, 0,
		       "strip mine buffer is too small");
    }

    /*
     * The value chosen for mem_space must be the same as the value chosen for
     * file_space.
     */
    f_ndims = H5S_get_hyperslab (file_space, NULL, size, NULL);
    if (m_ndims!=f_ndims) {
	nelmts = H5S_get_npoints (file_space);
	if (nelmts>desired_nelmts) {
	    HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, 0,
			   "strip mining not supported across "
			   "dimensionalities");
	}
	assert (nelmts==H5S_get_npoints (mem_space));
    } else {
	for (i=f_ndims-1, acc=1; i>0; --i) acc *= size[i];
	acc *= (desired_nelmts/acc);
	if (nelmts!=acc) {
	    HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, 0,
			   "unsupported strip mine size for shape change");
	}
    }
    
    FUNC_LEAVE (nelmts);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_simp_fgath
 *
 * Purpose:	Gathers data points from file F and accumulates them in the
 *		type conversion buffer BUF.  The LAYOUT argument describes
 *		how the data is stored on disk and EFL describes how the data
 *		is organized in external files.  ELMT_SIZE is the size in
 *		bytes of a datum which this function treats as opaque.
 *		FILE_SPACE describes the data space of the dataset on disk
 *		and the elements that have been selected for reading (via
 *		hyperslab, etc) and NUMBERING describes how those elements
 *		are numbered (initialized by the H5S_*_init() call). This
 *		function will copy at most NELMTS elements beginning at the
 *		element numbered START.
 *
 * Return:	Success:	Number of elements copied.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5S_simp_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		const struct H5O_efl_t *efl,
		size_t elmt_size, const H5S_t *file_space,
		const H5S_number_t *numbering, size_t start, size_t nelmts,
		void *buf/*out*/)
{
    size_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of slab in file*/
    size_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    size_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    size_t	sample[H5O_LAYOUT_NDIMS];	/*hyperslab sampling	*/
    size_t	acc;				/*accumulator		*/
#ifndef LATER
    intn	file_offset_signed[H5O_LAYOUT_NDIMS];
#endif
    intn	space_ndims;			/*dimensionality of space*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5S_simp_fgath, 0);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (numbering);
    assert (nelmts>0);
    assert (buf);

    /*
     * Get hyperslab information to determine what elements are being
     * selected (there might eventually be other selection methods too).
     * We only support hyperslabs with unit sample because there's no way to
     * currently pass sample information into H5F_arr_read() much less
     * H5F_istore_read().
     */
#ifdef LATER
    if ((space_ndims=H5S_get_hyperslab (file_space, file_offset,
					hsize, sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve hyperslab parameters");
    }
#else
    /* Argument type problems to be fixed later..... -RPM */
    if ((space_ndims=H5S_get_hyperslab (file_space, file_offset_signed,
					hsize, sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve hyperslab parameters");
    }
    for (i=0; i<space_ndims; i++) {
	assert (file_offset_signed[i]>=0);
	file_offset[i] = file_offset_signed[i];
    }
#endif

    /* Check that there is no subsampling of the hyperslab */
    for (i=0; i<space_ndims; i++) {
	if (sample[i]!=1) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, 0,
			   "hyperslab sampling is not implemented yet");
	}
    }

    /* Adjust the slowest varying dimension to take care of strip mining */
    for (i=1, acc=1; i<space_ndims; i++) acc *= hsize[i];
    assert (0==start % acc);
    assert (0==nelmts % acc);
    file_offset[0] += start / acc;
    hsize[0] = nelmts / acc;

    /* The fastest varying dimension is for the data point itself */
    file_offset[space_ndims] = 0;
    hsize[space_ndims] = elmt_size;
    HDmemset (zero, 0, layout->ndims*sizeof(size_t));

    /*
     * Gather from file.
     */
    if (H5F_arr_read (f, layout, efl, hsize, hsize, zero, file_offset,
		      buf/*out*/)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, 0, "read error");
    }

    FUNC_LEAVE (nelmts);
}
    
/*-------------------------------------------------------------------------
 * Function:	H5S_simp_mscat
 *
 * Purpose:	Scatters data points from the type conversion buffer
 *		TCONV_BUF to the application buffer BUF.  Each element is
 *		ELMT_SIZE bytes and they are organized in application memory
 *		according to MEM_SPACE.  The NUMBERING information together
 *		with START and NELMTS describe how the elements stored in
 *		TCONV_BUF are globally numbered.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_simp_mscat (const void *tconv_buf, size_t elmt_size,
		const H5S_t *mem_space, const H5S_number_t *numbering,
		size_t start, size_t nelmts, void *buf/*out*/)
{
    size_t	mem_offset[H5O_LAYOUT_NDIMS];	/*slab offset in app buf*/
    size_t	mem_size[H5O_LAYOUT_NDIMS];	/*total size of app buf	*/
    size_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    size_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    size_t	sample[H5O_LAYOUT_NDIMS];	/*hyperslab sampling	*/
    size_t	acc;				/*accumulator		*/
#ifndef LATER
    intn	mem_offset_signed[H5O_LAYOUT_NDIMS];
#endif
    intn	space_ndims;			/*dimensionality of space*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5S_simp_mscat, FAIL);

    /* Check args */
    assert (tconv_buf);
    assert (elmt_size>0);
    assert (mem_space && H5S_SIMPLE==mem_space->type);
    assert (numbering);
    assert (nelmts>0);
    assert (buf);

    /*
     * Retrieve hyperslab information to determine what elements are being
     * selected (there might be other selection methods in the future).  We
     * only handle hyperslabs with unit sample because there's currently no
     * way to pass sample information to H5V_hyper_copy().
     */
#ifdef LATER
    if ((space_ndims=H5S_get_hyperslab (mem_space, mem_offset, hsize,
					sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }
#else
    /* Argument type problems to be fixed later..... -RPM */
    if ((space_ndims=H5S_get_hyperslab (mem_space, mem_offset_signed,
					hsize, sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }
    for (i=0; i<space_ndims; i++) {
	assert (mem_offset_signed[i]>=0);
	mem_offset[i] = mem_offset_signed[i];
    }
#endif

    /* Check that there is no subsampling of the hyperslab */
    for (i=0; i<space_ndims; i++) {
	if (sample[i]!=1) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			   "hyperslab sampling is not implemented yet");
	}
    }
    if (H5S_get_dims (mem_space, mem_size, NULL)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve data space dimensions");
    }

    /* Adjust the slowest varying dimension to take care of strip mining */
    for (i=1, acc=1; i<space_ndims; i++) acc *= hsize[i];
    assert (0==start % acc);
    assert (0==nelmts % acc);
    mem_offset[0] += start / acc;
    hsize[0] = nelmts / acc;

    /* The fastest varying dimension is for the data point itself */
    mem_offset[space_ndims] = 0;
    mem_size[space_ndims] = elmt_size;
    hsize[space_ndims] = elmt_size;
    HDmemset (zero, 0, (space_ndims+1)*sizeof(size_t));

    /*
     * Scatter from conversion buffer to application memory.
     */
    if (H5V_hyper_copy (space_ndims+1, hsize, mem_size, mem_offset, buf,
			hsize, zero, tconv_buf)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to scatter data to memory");
    }

    FUNC_LEAVE (SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_simp_mgath
 *
 * Purpose:	Gathers dataset elements from application memory BUF and
 *		copies them into the data type conversion buffer TCONV_BUF.
 *		Each element is ELMT_SIZE bytes and arranged in application
 *		memory according to MEM_SPACE.  The elements selected from
 *		BUF by MEM_SPACE are numbered according to NUMBERING and the
 *		caller is requesting that at most NELMTS be gathered
 *		beginning with number START.  The elements are packed into
 *		TCONV_BUF in order of their NUMBERING.
 *
 * Return:	Success:	Number of elements copied.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5S_simp_mgath (const void *buf, size_t elmt_size,
		const H5S_t *mem_space, const H5S_number_t *numbering,
		size_t start, size_t nelmts, void *tconv_buf/*out*/)
{
    size_t	mem_offset[H5O_LAYOUT_NDIMS];	/*slab offset in app buf*/
    size_t	mem_size[H5O_LAYOUT_NDIMS];	/*total size of app buf	*/
    size_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    size_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    size_t	sample[H5O_LAYOUT_NDIMS];	/*hyperslab sampling	*/
    size_t	acc;				/*accumulator		*/
#ifndef LATER
    intn	mem_offset_signed[H5O_LAYOUT_NDIMS];
#endif
    intn	space_ndims;			/*dimensionality of space*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5S_simp_mgath, 0);

    /* Check args */
    assert (buf);
    assert (elmt_size>0);
    assert (mem_space && H5S_SIMPLE==mem_space->type);
    assert (numbering);
    assert (nelmts>0);
    assert (tconv_buf);

    /*
     * Retrieve hyperslab information to determine what elements are being
     * selected (there might be other selection methods in the future).  We
     * only handle hyperslabs with unit sample because there's currently no
     * way to pass sample information to H5V_hyper_copy().
     */
#ifdef LATER
    if ((space_ndims=H5S_get_hyperslab (mem_space, mem_offset, hsize,
					sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve hyperslab parameters");
    }
#else
    /* Argument type problems to be fixed later..... -RPM */
    if ((space_ndims=H5S_get_hyperslab (mem_space, mem_offset_signed,
					hsize, sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }
    for (i=0; i<space_ndims; i++) {
	assert (mem_offset_signed[i]>=0);
	mem_offset[i] = mem_offset_signed[i];
    }
#endif

    /* Check that there is no subsampling of the hyperslab */
    for (i=0; i<space_ndims; i++) {
	if (sample[i]!=1) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, 0,
			   "hyperslab sampling is not implemented yet");
	}
    }
    if (H5S_get_dims (mem_space, mem_size, NULL)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve data space dimensions");
    }

    /* Adjust the slowest varying dimension to account for strip mining */
    for (i=1, acc=1; i<space_ndims; i++) acc *= hsize[i];
    assert (0==start % acc);
    assert (0==nelmts % acc);
    mem_offset[0] += start / acc;
    hsize[0] = nelmts / acc;
    
    /* The fastest varying dimension is for the data point itself */
    mem_offset[space_ndims] = 0;
    mem_size[space_ndims] = elmt_size;
    hsize[space_ndims] = elmt_size;
    HDmemset (zero, 0, (space_ndims+1)*sizeof(size_t));

    /*
     * Scatter from conversion buffer to application memory.
     */
    if (H5V_hyper_copy (space_ndims+1, hsize, hsize, zero, tconv_buf,
			mem_size, mem_offset, buf)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to scatter data to memory");
    }

    FUNC_LEAVE (nelmts);
}

/*-------------------------------------------------------------------------
 * Function:	H5S_simp_fscat
 *
 * Purpose:	Scatters dataset elements from the type conversion buffer BUF
 *		to the file F where the data points are arranged according to
 *		the file data space FILE_SPACE and stored according to
 *		LAYOUT and EFL. Each element is ELMT_SIZE bytes and has a
 *		unique number according to NUMBERING.  The caller is
 *		requesting that NELMTS elements are coppied beginning with
 *		element number START.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_simp_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		const struct H5O_efl_t *efl,
		size_t elmt_size, const H5S_t *file_space,
		const H5S_number_t *numbering, size_t start, size_t nelmts,
		const void *buf)
{
    size_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of hyperslab	*/
    size_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    size_t	zero[H5O_LAYOUT_NDIMS];		/*zero vector		*/
    size_t	sample[H5O_LAYOUT_NDIMS];	/*hyperslab sampling	*/
    size_t	acc;				/*accumulator		*/
#ifndef LATER
    intn	file_offset_signed[H5O_LAYOUT_NDIMS];
#endif
    intn	space_ndims;			/*space dimensionality	*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5S_simp_fscat, FAIL);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (numbering);
    assert (nelmts>0);
    assert (buf);

    /*
     * Get hyperslab information to determine what elements are being
     * selected (there might eventually be other selection methods too).
     * We only support hyperslabs with unit sample because there's no way to
     * currently pass sample information into H5F_arr_read() much less
     * H5F_istore_read().
     */
#ifdef LATER
    if ((space_ndims=H5S_get_hyperslab (file_space, file_offset, hsize,
					sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }
#else
    /* Argument type problems to be fixed later..... -RPM */
    if ((space_ndims=H5S_get_hyperslab (file_space, file_offset_signed,
					hsize, sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }
    for (i=0; i<space_ndims; i++) {
	assert (file_offset_signed[i]>=0);
	file_offset[i] = file_offset_signed[i];
    }
#endif

    /* Check that there is no subsampling of the hyperslab */
    for (i=0; i<space_ndims; i++) {
	if (sample[i]!=1) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			   "hyperslab sampling is not implemented yet");
	}
    }

    /* Adjust the slowest varying dimension to account for strip mining */
    for (i=1, acc=1; i<space_ndims; i++) acc *= hsize[i];
    assert (0==start % acc);
    assert (0==nelmts % acc);
    file_offset[0] += start / acc;
    hsize[0] = nelmts / acc;
    
    /* The fastest varying dimension is for the data point itself */
    file_offset[space_ndims] = 0;
    hsize[space_ndims] = elmt_size;
    HDmemset (zero, 0, layout->ndims*sizeof(size_t));

    /*
     * Scatter to file.
     */
    if (H5F_arr_write (f, layout, efl, hsize, hsize, zero,
		       file_offset, buf)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_simp_read
 *
 * Purpose:	Reads a dataset from file F directly into application memory
 *		BUF performing data space conversion in a single step from
 *		FILE_SPACE to MEM_SPACE. The dataset is stored in the file
 *		according to the LAYOUT and EFL (external file list) and data
 *		point in the file is ELMT_SIZE bytes.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, March 12, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_simp_read (H5F_t *f, const struct H5O_layout_t *layout,
	       const struct H5O_efl_t *efl, size_t elmt_size,
	       const H5S_t *file_space, const H5S_t *mem_space,
	       void *buf/*out*/)
{
    size_t	hslab_size[H5O_LAYOUT_NDIMS];
    size_t	file_offset[H5O_LAYOUT_NDIMS];
    size_t	mem_size[H5O_LAYOUT_NDIMS];
    size_t	mem_offset[H5O_LAYOUT_NDIMS];
    int		i;

    FUNC_ENTER (H5S_simp_read, FAIL);

#ifndef NDEBUG
    assert (file_space->type==mem_space->type);
    assert (file_space->u.simple.rank==mem_space->u.simple.rank);
    for (i=0; i<file_space->u.simple.rank; i++) {
	if (file_space->hslab_def && mem_space->hslab_def) {
	    assert (1==file_space->h.stride[i]);
	    assert (1==mem_space->h.stride[i]);
	    assert (file_space->h.count[i]==mem_space->h.count[i]);
	} else if (file_space->hslab_def) {
	    assert (1==file_space->h.stride[i]);
	    assert (file_space->h.count[i]==mem_space->u.simple.size[i]);
	} else if (mem_space->hslab_def) {
	    assert (1==mem_space->h.stride[i]);
	    assert (file_space->u.simple.size[i]==mem_space->h.count[i]);
	} else {
	    assert (file_space->u.simple.size[i]==
		    mem_space->u.simple.size[i]);
	}
    }
#endif
	

    /*
     * Calculate size of hyperslab and offset of hyperslab into file and
     * memory.
     */
    if (file_space->hslab_def) {
	for (i=0; i<file_space->u.simple.rank; i++) {
	    hslab_size[i] = file_space->h.count[i];
	}
    } else {
	for (i=0; i<file_space->u.simple.rank; i++) {
	    hslab_size[i] = file_space->u.simple.size[i];
	}
    }
    for (i=0; i<mem_space->u.simple.rank; i++) {
	mem_size[i] = mem_space->u.simple.size[i];
    }
    if (file_space->hslab_def) {
	for (i=0; i<file_space->u.simple.rank; i++) {
	    file_offset[i] = file_space->h.start[i];
	}
    } else {
	for (i=0; i<file_space->u.simple.rank; i++) {
	    file_offset[i] = 0;
	}
    }
    if (mem_space->hslab_def) {
	for (i=0; i<mem_space->u.simple.rank; i++) {
	    mem_offset[i] = mem_space->h.start[i];
	}
    } else {
	for (i=0; i<mem_space->u.simple.rank; i++) {
	    mem_offset[i] = 0;
	}
    }
    hslab_size[file_space->u.simple.rank] = elmt_size;
    mem_size[file_space->u.simple.rank] = elmt_size;
    file_offset[file_space->u.simple.rank] = 0;
    mem_offset[file_space->u.simple.rank] = 0;
    
    /* Read the hyperslab */
    if (H5F_arr_read (f, layout, efl, hslab_size,
		      mem_size, mem_offset, file_offset, buf)<0) {
	HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL, "unable to read dataset");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_simp_write
 *
 * Purpose:	Write a dataset from application memory BUF directly into
 *		file F performing data space conversion in a single step from
 *		MEM_SPACE to FILE_SPACE. The dataset is stored in the file
 *		according to the LAYOUT and EFL (external file list) and data
 *		point in the file is ELMT_SIZE bytes.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, March 12, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_simp_write (H5F_t *f, const struct H5O_layout_t *layout,
		const struct H5O_efl_t *efl, size_t elmt_size,
		const H5S_t *file_space, const H5S_t *mem_space,
		const void *buf)
{
    size_t	hslab_size[H5O_LAYOUT_NDIMS];
    size_t	file_offset[H5O_LAYOUT_NDIMS];
    size_t	mem_size[H5O_LAYOUT_NDIMS];
    size_t	mem_offset[H5O_LAYOUT_NDIMS];
    int		i;

    FUNC_ENTER (H5S_simp_write, FAIL);

#ifndef NDEBUG
    assert (file_space->type==mem_space->type);
    assert (file_space->u.simple.rank==mem_space->u.simple.rank);
    for (i=0; i<file_space->u.simple.rank; i++) {
	if (file_space->hslab_def && mem_space->hslab_def) {
	    assert (1==file_space->h.stride[i]);
	    assert (1==mem_space->h.stride[i]);
	    assert (file_space->h.count[i]==mem_space->h.count[i]);
	} else if (file_space->hslab_def) {
	    assert (1==file_space->h.stride[i]);
	    assert (file_space->h.count[i]==mem_space->u.simple.size[i]);
	} else if (mem_space->hslab_def) {
	    assert (1==mem_space->h.stride[i]);
	    assert (file_space->u.simple.size[i]==mem_space->h.count[i]);
	} else {
	    assert (file_space->u.simple.size[i]==
		    mem_space->u.simple.size[i]);
	}
    }
#endif
	

    /*
     * Calculate size of hyperslab and offset of hyperslab into file and
     * memory.
     */
    if (file_space->hslab_def) {
	for (i=0; i<file_space->u.simple.rank; i++) {
	    hslab_size[i] = file_space->h.count[i];
	}
    } else {
	for (i=0; i<file_space->u.simple.rank; i++) {
	    hslab_size[i] = file_space->u.simple.size[i];
	}
    }
    for (i=0; i<mem_space->u.simple.rank; i++) {
	mem_size[i] = mem_space->u.simple.size[i];
    }
    if (file_space->hslab_def) {
	for (i=0; i<file_space->u.simple.rank; i++) {
	    file_offset[i] = file_space->h.start[i];
	}
    } else {
	for (i=0; i<file_space->u.simple.rank; i++) {
	    file_offset[i] = 0;
	}
    }
    if (mem_space->hslab_def) {
	for (i=0; i<mem_space->u.simple.rank; i++) {
	    mem_offset[i] = mem_space->h.start[i];
	}
    } else {
	for (i=0; i<mem_space->u.simple.rank; i++) {
	    mem_offset[i] = 0;
	}
    }
    hslab_size[file_space->u.simple.rank] = elmt_size;
    mem_size[file_space->u.simple.rank] = elmt_size;
    file_offset[file_space->u.simple.rank] = 0;
    mem_offset[file_space->u.simple.rank] = 0;
    
    /* Write the hyperslab */
    if (H5F_arr_write (f, layout, efl, hslab_size,
		       mem_size, mem_offset, file_offset, buf)<0) {
	HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
		       "unable to write dataset");
    }

    FUNC_LEAVE (SUCCEED);
}


	
    
