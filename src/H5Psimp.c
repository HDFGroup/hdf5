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
#include <H5Pprivate.h>
#include <H5Vprivate.h>

/* Interface initialization */
#define PABLO_MASK      H5P_simp_mask
#define INTERFACE_INIT  NULL
static intn             interface_initialize_g = FALSE;

/*-------------------------------------------------------------------------
 * Function:	H5P_simp_init
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
H5P_simp_init (const struct H5O_layout_t *layout, const H5P_t *mem_space,
	       const H5P_t *file_space, H5P_number_t *numbering/*out*/)
{
    size_t	nelmts;
    
    FUNC_ENTER (H5P_simp_init, 0);

    /* Check args */
    assert (layout);
    assert (mem_space && H5P_SIMPLE==mem_space->type);
    assert (file_space && H5P_SIMPLE==file_space->type);
    assert (numbering);

    /* Numbering is implied by the hyperslab, C order */
    HDmemset (numbering, 0, sizeof(H5P_number_t));

    /* Data can be efficiently copied at any size */
    nelmts = H5P_get_npoints (file_space);
    
    FUNC_LEAVE (nelmts);
}

/*-------------------------------------------------------------------------
 * Function:	H5P_simp_fgath
 *
 * Purpose:	Gathers data points from file F and accumulates them in the
 *		type conversion buffer BUF.  The LAYOUT argument describes
 *		how the data is stored on disk.  ELMT_SIZE is the size in
 *		bytes of a datum which this function treats as opaque.
 *		FILE_SPACE describes the data space of the dataset on disk
 *		and the elements that have been selected for reading (via
 *		hyperslab, etc) and NUMBERING describes how those elements
 *		are numbered (initialized by the H5P_*_init() call). This
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
H5P_simp_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		size_t elmt_size, const H5P_t *file_space,
		const H5P_number_t *numbering, intn start, intn nelmts,
		void *buf/*out*/)
{
    size_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of slab in file*/
    size_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    size_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    size_t	sample[H5O_LAYOUT_NDIMS];	/*hyperslab sampling	*/
#ifndef LATER
    intn	file_offset_signed[H5O_LAYOUT_NDIMS];
#endif
    intn	space_ndims;			/*dimensionality of space*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5P_simp_fgath, 0);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (numbering);
    assert (nelmts>0);
    assert (buf);

    /*
     * The prototype doesn't support strip mining.
     */
    assert (0==start);
    assert (nelmts==H5P_get_npoints (file_space));

    /*
     * Get hyperslab information to determine what elements are being
     * selected (there might eventually be other selection methods too).
     * We only support hyperslabs with unit sample because there's no way to
     * currently pass sample information into H5F_arr_read() much less
     * H5F_istore_read().
     */
#ifdef LATER
    if ((space_ndims=H5P_get_hyperslab (file_space, file_offset,
					hsize, sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve hyperslab parameters");
#else
    if ((space_ndims=H5P_get_hyperslab (file_space, file_offset_signed,
					hsize, sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve hyperslab parameters");
    }
    for (i=0; i<space_ndims; i++) {
	assert (file_offset_signed[i]>=0);
	file_offset[i] = file_offset_signed[i];
    }
#endif
    for (i=0; i<space_ndims; i++) {
	if (sample[i]!=1) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, 0,
			   "hyperslab sampling is not implemented yet");
	}
    }
    file_offset[space_ndims] = 0;
    hsize[space_ndims] = elmt_size;
    HDmemset (zero, 0, layout->ndims*sizeof(size_t));

    /*
     * Gather from file.
     */
    if (H5F_arr_read (f, layout, hsize, hsize, zero, file_offset,
		      buf/*out*/)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, 0, "read error");
    }

    FUNC_LEAVE (nelmts);
}
    
/*-------------------------------------------------------------------------
 * Function:	H5P_simp_mscat
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
H5P_simp_mscat (const void *tconv_buf, size_t elmt_size,
		const H5P_t *mem_space, const H5P_number_t *numbering,
		intn start, intn nelmts, void *buf/*out*/)
{
    size_t	mem_offset[H5O_LAYOUT_NDIMS];	/*slab offset in app buf*/
    size_t	mem_size[H5O_LAYOUT_NDIMS];	/*total size of app buf	*/
    size_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    size_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    size_t	sample[H5O_LAYOUT_NDIMS];	/*hyperslab sampling	*/
#ifndef LATER
    intn	mem_offset_signed[H5O_LAYOUT_NDIMS];
#endif
    intn	space_ndims;			/*dimensionality of space*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5P_simp_mscat, FAIL);

    /* Check args */
    assert (tconv_buf);
    assert (elmt_size>0);
    assert (mem_space && H5P_SIMPLE==mem_space->type);
    assert (numbering);
    assert (nelmts>0);
    assert (buf);

    /*
     * The prototype doesn't support strip mining.
     */
    assert (0==start);
    assert (nelmts==H5P_get_npoints (mem_space));

    /*
     * Retrieve hyperslab information to determine what elements are being
     * selected (there might be other selection methods in the future).  We
     * only handle hyperslabs with unit sample because there's currently no
     * way to pass sample information to H5V_hyper_copy().
     */
#ifdef LATER
    if ((space_ndims=H5P_get_hyperslab (mem_space, mem_offset, hsize,
					sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }
#else
    if ((space_ndims=H5P_get_hyperslab (mem_space, mem_offset_signed,
					hsize, sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }
    for (i=0; i<space_ndims; i++) {
	assert (mem_offset_signed[i]>=0);
	mem_offset[i] = mem_offset_signed[i];
    }
#endif
    for (i=0; i<space_ndims; i++) {
	if (sample[i]!=1) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			   "hyperslab sampling is not implemented yet");
	}
    }
    if (H5P_get_dims (mem_space, mem_size)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve data space dimensions");
    }
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
 * Function:	H5P_simp_mgath
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
H5P_simp_mgath (const void *buf, size_t elmt_size,
		const H5P_t *mem_space, const H5P_number_t *numbering,
		intn start, intn nelmts, void *tconv_buf/*out*/)
{
    size_t	mem_offset[H5O_LAYOUT_NDIMS];	/*slab offset in app buf*/
    size_t	mem_size[H5O_LAYOUT_NDIMS];	/*total size of app buf	*/
    size_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    size_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    size_t	sample[H5O_LAYOUT_NDIMS];	/*hyperslab sampling	*/
#ifndef LATER
    intn	mem_offset_signed[H5O_LAYOUT_NDIMS];
#endif
    intn	space_ndims;			/*dimensionality of space*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5P_simp_mgath, 0);

    /* Check args */
    assert (buf);
    assert (elmt_size>0);
    assert (mem_space && H5P_SIMPLE==mem_space->type);
    assert (numbering);
    assert (nelmts>0);
    assert (tconv_buf);

    /*
     * The prototype doesn't support strip mining.
     */
    assert (0==start);
    assert (nelmts==H5P_get_npoints (mem_space));

    /*
     * Retrieve hyperslab information to determine what elements are being
     * selected (there might be other selection methods in the future).  We
     * only handle hyperslabs with unit sample because there's currently no
     * way to pass sample information to H5V_hyper_copy().
     */
#ifdef LATER
    if ((space_ndims=H5P_get_hyperslab (mem_space, mem_offset, hsize,
					sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve hyperslab parameters");
    }
#else
    if ((space_ndims=H5P_get_hyperslab (mem_space, mem_offset_signed,
					hsize, sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }
    for (i=0; i<space_ndims; i++) {
	assert (mem_offset_signed[i]>=0);
	mem_offset[i] = mem_offset_signed[i];
    }
#endif
    for (i=0; i<space_ndims; i++) {
	if (sample[i]!=1) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, 0,
			   "hyperslab sampling is not implemented yet");
	}
    }
    if (H5P_get_dims (mem_space, mem_size)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve data space dimensions");
    }
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
 * Function:	H5P_simp_fscat
 *
 * Purpose:	Scatters dataset elements from the type conversion buffer BUF
 *		to the file F where the data points are arranged according to
 *		the file data space FILE_SPACE and stored according to
 *		LAYOUT. Each element is ELMT_SIZE bytes and has a unique
 *		number according to NUMBERING.  The caller is requesting that
 *		NELMTS elements are coppied beginning with element number
 *		START.
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
H5P_simp_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		size_t elmt_size, const H5P_t *file_space,
		const H5P_number_t *numbering, intn start, intn nelmts,
		const void *buf)
{
    size_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of hyperslab	*/
    size_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    size_t	zero[H5O_LAYOUT_NDIMS];		/*zero vector		*/
    size_t	sample[H5O_LAYOUT_NDIMS];	/*hyperslab sampling	*/
#ifndef LATER
    intn	file_offset_signed[H5O_LAYOUT_NDIMS];
#endif
    intn	space_ndims;			/*space dimensionality	*/
    intn	i;				/*counters		*/

    FUNC_ENTER (H5P_simp_fscat, FAIL);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (numbering);
    assert (nelmts>0);
    assert (buf);

    /*
     * The prototype doesn't support strip mining.
     */
    assert (0==start);
    assert (nelmts==H5P_get_npoints (file_space));
    
    /*
     * Get hyperslab information to determine what elements are being
     * selected (there might eventually be other selection methods too).
     * We only support hyperslabs with unit sample because there's no way to
     * currently pass sample information into H5F_arr_read() much less
     * H5F_istore_read().
     */
#ifdef LATER
    if ((space_ndims=H5P_get_hyperslab (file_space, file_offset, hsize,
					sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }
#else
    if ((space_ndims=H5P_get_hyperslab (file_space, file_offset_signed,
					hsize, sample))<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve hyperslab parameters");
    }
    for (i=0; i<space_ndims; i++) {
	assert (file_offset_signed[i]>=0);
	file_offset[i] = file_offset_signed[i];
    }
#endif
    for (i=0; i<space_ndims; i++) {
	if (sample[i]!=1) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			   "hyperslab sampling is not implemented yet");
	}
    }
    file_offset[space_ndims] = 0;
    hsize[space_ndims] = elmt_size;
    HDmemset (zero, 0, layout->ndims*sizeof(size_t));

    /*
     * Scatter to file.
     */
    if (H5F_arr_write (f, layout, hsize, hsize, zero, file_offset, buf)<0) {
	HRETURN_ERROR (H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error");
    }

    FUNC_LEAVE (SUCCEED);
}
