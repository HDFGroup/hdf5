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
    size_t	offset[H5O_LAYOUT_NDIMS];	/*offset of hyperslab	*/
    size_t	size[H5O_LAYOUT_NDIMS];		/*size of hyperslab	*/
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
     * Quincey, this is where we look at FILE_SPACE to decide what the
     * hyperslab is to read from disk.  For now, since the H5P interface
     * doesn't support hyperslabs, we'll assume the caller is asking for the
     * entire array.  --RPM
     */
    assert (nelmts == H5P_get_npoints (file_space));
    for (i=0; i<layout->ndims; i++) offset[i] = 0;
    i = H5P_get_dims (file_space, size);
    assert (i+1 == layout->ndims);
    size[i] = elmt_size;

    /*
     * Gather from file.
     */
    if (H5F_arr_read (f, layout, size, offset, offset, size, buf/*out*/)<0) {
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
     * Quincey, this is where we look at the hyperslab spec of MEM_SPACE to
     * figure out how to scatter.  For now we just assume that data points
     * are copied directly from TCONV_BUF to BUF.
     */
    HDmemcpy (buf, tconv_buf, nelmts*elmt_size);

    FUNC_LEAVE (SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5P_simp_mgath
 *
 * Purpose:	Gathers dataset elements from application memory and copies
 *		them into the data type conversion buffer.  NOT IMPLEMENTED
 *		YET.
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
H5P_simp_mgath (void)
{
    FUNC_ENTER (H5P_simp_mgath, 0);

    HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, 0,
		   "not implemented yet");

    FUNC_LEAVE (0);
}

/*-------------------------------------------------------------------------
 * Function:	H5P_simp_fscat
 *
 * Purpose:	Scatters dataset elements from the type conversion buffer to
 *		the file. NOT IMPLEMENTED YET.
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
H5P_simp_fscat (void)
{
    FUNC_ENTER (H5P_simp_fscat, FAIL);

    HRETURN_ERROR (H5E_DATASPACE, H5E_UNSUPPORTED, FAIL,
		   "not implemented yet");

    FUNC_LEAVE (FAIL);
}
