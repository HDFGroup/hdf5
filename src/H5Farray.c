/*
 * Copyright (C) 1998 Spizella Software
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <robb@arborea.spizella.com>
 *              Thursday, January 15, 1998
 *
 * Purpose:	Provides I/O facilities for multi-dimensional arrays of bytes
 *		stored with various layout policies.  If the caller is
 *		interested in arrays of elements >1 byte then add an extra
 *		dimension.  For example, a 10x20 array of int32 would
 *		translate to a 10x20x4 array of bytes at this level.
 */
#include <H5private.h>
#include <H5Dprivate.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MFprivate.h>
#include <H5Oprivate.h>
#include <H5Vprivate.h>

/* Interface initialization */
#define PABLO_MASK	H5F_arr_mask
#define INTERFACE_INIT	NULL
static intn interface_initialize_g = FALSE;



/*-------------------------------------------------------------------------
 * Function:	H5F_arr_create
 *
 * Purpose:	Creates an array of bytes.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, January 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_arr_create (H5F_t *f, struct H5O_layout_t *layout/*in,out*/)
{
    intn		i;
    size_t	nbytes;
   
    FUNC_ENTER (H5F_arr_create, FAIL);

    /* check args */
    assert (f);
    assert (layout);
    H5F_addr_undef (&(layout->addr)); /*just in case we fail*/
   
    switch (layout->type) {
    case H5D_CONTIGUOUS:
	/* Reserve space in the file for the entire array */
	for (i=0, nbytes=1; i<layout->ndims; i++) nbytes *= layout->dim[i];
	assert (nbytes>0);
	if (H5MF_alloc (f, H5MF_RAW, nbytes, &(layout->addr)/*out*/)<0) {
	    HRETURN_ERROR (H5E_IO, H5E_NOSPACE, FAIL,
			   "unable to reserve file space");
	}
	break;

    case H5D_CHUNKED:
	/* Create the root of the B-tree that describes chunked storage */
	if (H5F_istore_create (f, layout/*out*/)<0) {
	    HRETURN_ERROR (H5E_IO, H5E_CANTINIT, FAIL,
			   "unable to initialize chunked storage");
	}
	break;

    default:
	assert ("not implemented yet" && 0);
	HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
		       "unsupported storage layout");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_arr_read
 *
 * Purpose:	Reads a hyperslab of a file byte array into a hyperslab of
 *		a byte array in	memory.  The data is read from file F and the
 *		array's size and storage information is in LAYOUT.  The
 *		hyperslab offset is FILE_OFFSET[] in the file and
 *		MEM_OFFSET[] in memory (offsets are relative to the origin of
 *		the array) and the size of the hyperslab is HSLAB_SIZE[]. The
 *		total size of the file array is implied in the LAYOUT
 *		argument and the total size of the memory array is
 *		MEM_SIZE[]. The dimensionality of these vectors is implied by
 *		the LAYOUT argument.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, January 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_arr_read (H5F_t *f, const struct H5O_layout_t *layout,
	      const size_t _hslab_size[], const size_t mem_size[],
	      const size_t mem_offset[], const size_t file_offset[],
	      void *_buf/*out*/)
{
    uint8	*buf = (uint8 *)_buf;		/*cast for arithmetic	*/
    intn	file_stride[H5O_LAYOUT_NDIMS];	/*strides through file	*/
    intn	mem_stride[H5O_LAYOUT_NDIMS];	/*strides through memory*/
    size_t	hslab_size[H5O_LAYOUT_NDIMS];	/*hyperslab size	*/
    size_t	idx[H5O_LAYOUT_NDIMS];		/*multi-dim counter	*/
    size_t	mem_start, file_start;		/*byte offsets to start	*/
    size_t	elmt_size = 1;			/*bytes per element	*/
    size_t	nelmts, z;			/*number of elements	*/
    intn	ndims;				/*stride dimensionality	*/
    haddr_t	addr;				/*address in file	*/
    intn	i, j;				/*counters		*/
    hbool_t	carray;				/*carry for subtraction	*/
   
    FUNC_ENTER (H5F_arr_read, FAIL);

    /* Check args */
    assert (f);
    assert (layout);
    assert (_hslab_size);
    assert (file_offset);
    assert (mem_offset);
    assert (mem_size);
    assert (buf);

    /* Make a local copy of size so we can modify it */
    H5V_vector_cpy (layout->ndims, hslab_size, _hslab_size);

    switch (layout->type) {
    case H5D_CONTIGUOUS:
	/*
	 * Calculate the strides needed to walk through the array on disk
	 * and memory. Optimize the strides to result in the fewest number of
	 * I/O requests.
	 */
	ndims = layout->ndims;
	mem_start = H5V_hyper_stride (ndims, hslab_size, mem_size,
				      mem_offset, mem_stride/*out*/);
	file_start = H5V_hyper_stride (ndims, hslab_size, layout->dim,
				       file_offset, file_stride/*out*/);
	H5V_stride_optimize2 (&ndims, &elmt_size, hslab_size,
			      mem_stride, file_stride);

	/*
	 * Initialize loop variables.  The loop is a multi-dimensional loop
	 * that counts from SIZE down to zero and IDX is the counter.  Each
	 * element of IDX is treated as a digit with IDX[0] being the least
	 * significant digit.
	 */
	H5V_vector_cpy (ndims, idx, hslab_size);
	nelmts = H5V_vector_reduce_product (ndims, hslab_size);
	addr = layout->addr;
	H5F_addr_inc (&addr, file_start);
	buf += mem_start;

	/*
	 * Now begin to walk through the array, copying data from disk to
	 * memory.
	 */
	for (z=0; z<nelmts; z++) {

	    /* Read from file */
	    if (H5F_block_read (f, &addr, elmt_size, buf)<0) {
		HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL,
			       "block read failed");
	    }

	    /* Decrement indices and advance pointers */
	    for (j=ndims-1, carray=TRUE; j>=0 && carray; --j) {
		
		H5F_addr_inc (&addr, file_stride[j]);
		buf += mem_stride[j];

		if (--idx[j]) carray = FALSE;
		else idx[j] = hslab_size[j];
	    }
	}
	break;

    case H5D_CHUNKED:
	/*
	 * This method is unable to copy into a proper hyperslab.
	 */
	for (i=0; i<layout->ndims; i++) {
	    if (0!=mem_offset[i] || hslab_size[i]!=mem_size[i]) {
		HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
			       "unable to copy into a proper hyperslab");
	    }
	}
	if (H5F_istore_read (f, layout, file_offset, hslab_size, buf)<0) {
	    HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL, "chunked read failed");
	}
	break;

    default:
	assert ("not implemented yet" && 0);
	HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
		       "unsupported storage layout");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_arr_write
 *
 * Purpose:	Copies a hyperslab of a memory array to a hyperslab of a
 *		file array.  The data is written to file F and the file
 *		array's size and storage information is implied by LAYOUT.
 *		The hyperslab offset is FILE_OFFSET[] in the file and
 *		MEM_OFFSET[] in memory (offsets are relative to the origin of
 *		the array) and the size of the hyperslab is HSLAB_SIZE[].
 *		The total size of the file array is implied by the LAYOUT
 *		argument and the total size of the memory array is
 *		MEM_SIZE[].  The dimensionality of these vectors is implied
 *		by the LAYOUT argument.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, January 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_arr_write (H5F_t *f, const struct H5O_layout_t *layout,
	       const size_t _hslab_size[], const size_t mem_size[],
	       const size_t mem_offset[], const size_t file_offset[],
	       const void *_buf)
{
    const uint8	*buf = (const uint8 *)_buf;	/*cast for arithmetic	*/
    intn	file_stride[H5O_LAYOUT_NDIMS];	/*strides through file	*/
    intn	mem_stride[H5O_LAYOUT_NDIMS];	/*strides through memory*/
    size_t	hslab_size[H5O_LAYOUT_NDIMS];	/*hyperslab size	*/
    size_t	idx[H5O_LAYOUT_NDIMS];		/*multi-dim counter	*/
    size_t	mem_start, file_start;		/*byte offsets to start	*/
    size_t	elmt_size = 1;			/*bytes per element	*/
    size_t	nelmts, z;			/*number of elements	*/
    intn	ndims;				/*dimensionality	*/
    haddr_t	addr;				/*address in file	*/
    intn	i, j;				/*counters		*/
    hbool_t	carray;				/*carry for subtraction	*/
   
    FUNC_ENTER (H5F_arr_write, FAIL);

    /* Check args */
    assert (f);
    assert (layout);
    assert (_hslab_size);
    assert (file_offset);
    assert (mem_offset);
    assert (mem_size);
    assert (buf);

    /* Make a local copy of _size so we can modify it */
    H5V_vector_cpy (layout->ndims, hslab_size, _hslab_size);


    switch (layout->type) {
    case H5D_CONTIGUOUS:
	/*
	 * Calculate the strides needed to walk through the array on disk.
	 * Optimize the strides to result in the fewest number of I/O
	 * requests.
	 */
	ndims = layout->ndims;
	mem_start = H5V_hyper_stride (ndims, hslab_size, mem_size,
				      mem_offset, mem_stride/*out*/);
	file_start = H5V_hyper_stride (ndims, hslab_size, layout->dim,
				       file_offset, file_stride/*out*/);
	H5V_stride_optimize2 (&ndims, &elmt_size, hslab_size,
			      mem_stride, file_stride);

	/*
	 * Initialize loop variables.  The loop is a multi-dimensional loop
	 * that counts from SIZE down to zero and IDX is the counter.  Each
	 * element of IDX is treated as a digit with IDX[0] being the least
	 * significant digit.
	 */
	H5V_vector_cpy (ndims, idx, hslab_size);
	nelmts = H5V_vector_reduce_product (ndims, hslab_size);
	addr = layout->addr;
	H5F_addr_inc (&addr, file_start);
	buf += mem_start;

	/*
	 * Now begin to walk through the array, copying data from memory to
	 * disk.
	 */
	for (z=0; z<nelmts; z++) {

	    /* Write to file */
	    if (H5F_block_write (f, &addr, elmt_size, buf)<0) {
		HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
			       "block write failed");
	    }

	    /* Decrement indices and advance pointers */
	    for (j=ndims-1, carray=TRUE; j>=0 && carray; --j) {
		
		H5F_addr_inc (&addr, file_stride[j]);
		buf += mem_stride[j];
		
		if (--idx[j]) carray = FALSE;
		else idx[j] = hslab_size[j];
	    }

	}
	break;

    case H5D_CHUNKED:
	/*
	 * This method is unable to copy from a proper hyperslab.
	 */
	for (i=0; i<layout->ndims; i++) {
	    if (0!=mem_offset[i] || hslab_size[i]!=mem_size[i]) {
		HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
			       "unable to copy from a proper hyperslab");
	    }
	}
	if (H5F_istore_write (f, layout, file_offset, hslab_size, buf)<0) {
	    HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
			   "chunked write failed");
	}
	break;

    default:
	assert ("not implemented yet" && 0);
	HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
		       "unsupported storage layout");
    }

    FUNC_LEAVE (SUCCEED);
}
