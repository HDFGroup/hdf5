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
    hsize_t		nbytes;
   
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
 *		array's size and storage information is in LAYOUT.  External
 *		files are described according to the external file list, EFL.
 *		The hyperslab offset is FILE_OFFSET[] in the file and
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
	      const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
	      const hsize_t _hslab_size[], const hsize_t mem_size[],
	      const hssize_t mem_offset[], const hssize_t file_offset[],
	      void *_buf/*out*/)
{
    uint8	*buf = (uint8 *)_buf;		/*cast for arithmetic	*/
    hssize_t	file_stride[H5O_LAYOUT_NDIMS];	/*strides through file	*/
    hssize_t	mem_stride[H5O_LAYOUT_NDIMS];	/*strides through memory*/
    hsize_t	hslab_size[H5O_LAYOUT_NDIMS];	/*hyperslab size	*/
    hsize_t	idx[H5O_LAYOUT_NDIMS];		/*multi-dim counter	*/
    size_t	mem_start;			/*byte offset to start	*/
    hsize_t	file_start;			/*byte offset to start	*/
    hsize_t	elmt_size = 1;			/*bytes per element	*/
    size_t	nelmts, z;			/*number of elements	*/
    intn	ndims;				/*stride dimensionality	*/
    haddr_t	addr;				/*address in file	*/
    intn	i, j;				/*counters		*/
    hbool_t	carray;				/*carry for subtraction	*/
#ifdef HAVE_PARALLEL
    intn	is_collective;			/*collective access flag*/
#endif
   
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

#ifdef HAVE_PARALLEL
    is_collective = (f->shared->access_parms->driver==H5F_LOW_MPIO
	&& f->shared->access_parms->u.mpio.access_mode==H5D_XFER_COLLECTIVE);
    if (is_collective){
#ifdef AKC
	printf("%s: collective read requested\n", FUNC);
#endif
	if (layout->type != H5D_CONTIGUOUS)
	    HRETURN_ERROR (H5E_DATASET, H5E_READERROR, FAIL,
			   "collective access on non-contiguous datasets not "
			   "supported yet");
    }
#endif

    switch (layout->type) {
    case H5D_CONTIGUOUS:
	ndims = layout->ndims;
	/*
	 * Offsets must not be negative for this type of storage.
	 */
	for (i=0; i<ndims; i++) {
	    if (mem_offset[i]<0 || file_offset[i]<0) {
		HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL,
			       "negative offsets are not valid");
	    }
	}

	/*
	 * Compression cannot be used for contiguous data.
	 */
	if (comp && H5Z_NONE!=comp->method) {
	    HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL,
			   "compression is not allowed for contiguous data");
	}
	
	/*
	 * Calculate the strides needed to walk through the array on disk
	 * and memory. Optimize the strides to result in the fewest number of
	 * I/O requests.
	 */
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
	if (efl && efl->nused>0) {
	    H5F_addr_reset (&addr);
	} else {
	    addr = layout->addr;
	}
	H5F_addr_inc (&addr, file_start);
	buf += mem_start;

	/*
	 * Now begin to walk through the array, copying data from disk to
	 * memory.
	 */
#ifdef HAVE_PARALLEL
	if (is_collective){
	    /* Currently supports same number of collective access.
	     * Need to be changed LATER to combine all reads into one
	     * collective MPIO call.
	     */
	    unsigned long max, min, temp;

	    temp = nelmts;
	    assert(temp==nelmts);	/* verify no overflow */
	    MPI_Allreduce(&temp, &max, 1, MPI_UNSIGNED_LONG, MPI_MAX,
		f->shared->access_parms->u.mpio.comm);
	    MPI_Allreduce(&temp, &min, 1, MPI_UNSIGNED_LONG, MPI_MIN,
		f->shared->access_parms->u.mpio.comm);
#ifdef AKC
	    printf("nelmts=%lu, min=%lu, max=%lu\n", temp, min, max);
#endif
	    if (max != min)
		HRETURN_ERROR(H5E_DATASET, H5E_READERROR, FAIL,
			      "collective access with unequal number of "
			      "blocks not supported yet");
	}
#endif

	for (z=0; z<nelmts; z++) {

	    /* Read from file */
	    if (efl && efl->nused>0) {
		if (H5O_efl_read (f, efl, &addr, elmt_size, buf)<0) {
		    HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL,
				   "external data read failed");
		}
	    } else if (H5F_block_read (f, &addr, elmt_size, buf)<0) {
		HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL,
			       "block read failed");
	    }

	    /* Decrement indices and advance pointers */
	    for (j=ndims-1, carray=TRUE; j>=0 && carray; --j) {
		
		H5F_addr_adj (&addr, file_stride[j]);
		buf += mem_stride[j];

		if (--idx[j]) carray = FALSE;
		else idx[j] = hslab_size[j];
	    }
	}
	break;

    case H5D_CHUNKED:
	/*
	 * This method is unable to access external raw data files or to copy
	 * into a proper hyperslab.
	 */
	if (efl && efl->nused>0) {
	    HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
			   "chunking and external files are mutually "
			   "exclusive");
	}
	for (i=0; i<layout->ndims; i++) {
	    if (0!=mem_offset[i] || hslab_size[i]!=mem_size[i]) {
		HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
			       "unable to copy into a proper hyperslab");
	    }
	}
	if (H5F_istore_read (f, layout, comp, file_offset, hslab_size,
			     buf)<0) {
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
 *		The data is stored in external files according to the
 *		external file list, EFL. The hyperslab offset is
 *		FILE_OFFSET[] in the file and MEM_OFFSET[] in memory (offsets
 *		are relative to the origin of the array) and the size of the
 *		hyperslab is HSLAB_SIZE[].  The total size of the file array
 *		is implied by the LAYOUT argument and the total size of the
 *		memory array is MEM_SIZE[].  The dimensionality of these
 *		vectors is implied by the LAYOUT argument.
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
	       const struct H5O_compress_t *comp, const struct H5O_efl_t *efl,
	       const hsize_t _hslab_size[], const hsize_t mem_size[],
	       const hssize_t mem_offset[], const hssize_t file_offset[],
	       const void *_buf)
{
    const uint8	*buf = (const uint8 *)_buf;	/*cast for arithmetic	*/
    hssize_t	file_stride[H5O_LAYOUT_NDIMS];	/*strides through file	*/
    hssize_t	mem_stride[H5O_LAYOUT_NDIMS];	/*strides through memory*/
    hsize_t	hslab_size[H5O_LAYOUT_NDIMS];	/*hyperslab size	*/
    hsize_t	idx[H5O_LAYOUT_NDIMS];		/*multi-dim counter	*/
    hsize_t	mem_start;			/*byte offset to start	*/
    hsize_t	file_start;			/*byte offset to start	*/
    hsize_t	elmt_size = 1;			/*bytes per element	*/
    size_t	nelmts, z;			/*number of elements	*/
    intn	ndims;				/*dimensionality	*/
    haddr_t	addr;				/*address in file	*/
    intn	i, j;				/*counters		*/
    hbool_t	carray;				/*carry for subtraction	*/
#ifdef HAVE_PARALLEL
    intn	is_collective;			/*collective access flag*/
#endif
   
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

#ifdef HAVE_PARALLEL
    is_collective = (f->shared->access_parms->driver==H5F_LOW_MPIO
	&& f->shared->access_parms->u.mpio.access_mode==H5D_XFER_COLLECTIVE);
    if (is_collective){
#ifdef AKC
    printf("%s: collective write requested\n", FUNC);
#endif
	if (layout->type != H5D_CONTIGUOUS)
	    HRETURN_ERROR (H5E_DATASET, H5E_WRITEERROR, FAIL,
		"collective access on non-contiguous datasets not supported yet");
    }
#endif

    switch (layout->type) {
    case H5D_CONTIGUOUS:
	ndims = layout->ndims;
	/*
	 * Offsets must not be negative for this type of storage.
	 */
	for (i=0; i<ndims; i++) {
	    if (mem_offset[i]<0 || file_offset[i]<0) {
		HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
			       "negative offsets are not valid");
	    }
	}

	/*
	 * Compression cannot be used for contiguous data
	 */
	if (comp && H5Z_NONE!=comp->method) {
	    HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
			   "compression is not allowed for contiguous data");
	}
	
	/*
	 * Calculate the strides needed to walk through the array on disk.
	 * Optimize the strides to result in the fewest number of I/O
	 * requests.
	 */
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
	if (efl && efl->nused>0) {
	    H5F_addr_reset (&addr);
	} else {
	    addr = layout->addr;
	}
	H5F_addr_inc (&addr, file_start);
	buf += mem_start;

	/*
	 * Now begin to walk through the array, copying data from memory to
	 * disk.
	 */
#ifdef HAVE_PARALLEL
	if (is_collective){
	    /* Currently supports same number of collective access.
	     * Need to be changed LATER to combine all writes into one
	     * collective MPIO call.
	     */
	    unsigned long max, min, temp;

	    temp = nelmts;
	    assert(temp==nelmts);	/* verify no overflow */
	    MPI_Allreduce(&temp, &max, 1, MPI_UNSIGNED_LONG, MPI_MAX,
		f->shared->access_parms->u.mpio.comm);
	    MPI_Allreduce(&temp, &min, 1, MPI_UNSIGNED_LONG, MPI_MIN,
		f->shared->access_parms->u.mpio.comm);
#ifdef AKC
printf("nelmts=%lu, min=%lu, max=%lu\n", temp, min, max);
#endif
	    if (max != min)
		HRETURN_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL,
		    "collective access with unequal number of blocks not supported yet");
	}
#endif

	for (z=0; z<nelmts; z++) {

	    /* Write to file */
	    if (efl && efl->nused>0) {
		if (H5O_efl_write (f, efl, &addr, elmt_size, buf)<0) {
		    HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL,
				   "external data write failed");
		}
	    } else if (H5F_block_write (f, &addr, elmt_size, buf)<0) {
		HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
			       "block write failed");
	    }

	    /* Decrement indices and advance pointers */
	    for (j=ndims-1, carray=TRUE; j>=0 && carray; --j) {
		
		H5F_addr_adj (&addr, file_stride[j]);
		buf += mem_stride[j];
		
		if (--idx[j]) carray = FALSE;
		else idx[j] = hslab_size[j];
	    }

	}
	break;

    case H5D_CHUNKED:
	/*
	 * This method is unable to access external raw daa files or to copy
	 * from a proper hyperslab.
	 */
	if (efl && efl->nused>0) {
	    HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
			   "chunking and external files are mutually "
			   "exclusive");
	}
	for (i=0; i<layout->ndims; i++) {
	    if (0!=mem_offset[i] || hslab_size[i]!=mem_size[i]) {
		HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
			       "unable to copy from a proper hyperslab");
	    }
	}
	if (H5F_istore_write (f, layout, comp, file_offset, hslab_size,
			      buf)<0) {
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
