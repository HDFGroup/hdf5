/*
 * Copyright (C) 1998 Spizella Software
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <robb@arborea.spizella.com>
 *              Thursday, January 15, 1998
 *
 * Purpose:	Provides I/O facilities for multi-dimensional arrays of bytes
 *		stored with various layout policies.
 */
#include <H5private.h>
#include <H5Dprivate.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MFprivate.h>
#include <H5Oprivate.h>

/* Interface initialization */
#define PABLO_MASK	H5F_arr_mask
#define INTERFACE_INIT	NULL
static intn interface_initialize_g = FALSE;



/*-------------------------------------------------------------------------
 * Function:	H5F_arr_create
 *
 * Purpose:	Creates an array of bytes. When called to create an array of
 *		some type, the fastest varying dimension corresponds to an
 *		instance of that type.  That is, a 10x20 array of int32 is
 *		really a 10x20x4 array of bytes.
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
      break;
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_arr_read
 *
 * Purpose:	Reads a hyperslab of a file byte array into a byte array in
 *		memory which has the same dimensions as the hyperslab.
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
herr_t H5F_arr_read (H5F_t *f, const struct H5O_layout_t *layout, 
		     const size_t offset[], const size_t size[],
		     void *buf/*out*/)
{
   intn		i;
   size_t	nbytes;
   size_t	zero_offset[H5O_LAYOUT_NDIMS];
   
   FUNC_ENTER (H5F_arr_read, FAIL);

   /* Check args */
   assert (f);
   assert (layout);
   if (!offset) {
      HDmemset (zero_offset, 0, sizeof zero_offset);
      offset = zero_offset;
   }
   assert (size);
   assert (buf);

   switch (layout->type) {
   case H5D_CONTIGUOUS:
      /*
       * We currently only support complete I/O.
       */
      for (i=0; i<layout->ndims; i++) {
	 assert (0==offset[i]);
	 assert (size[i]==layout->dim[i]);
      }
      for (i=0, nbytes=1; i<layout->ndims; i++) nbytes *= layout->dim[i];
      if (H5F_block_read (f, &(layout->addr), nbytes, buf)<0) {
	 HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL, "block read failed");
      }
      break;

   case H5D_CHUNKED:
      if (H5F_istore_read (f, layout, offset, size, buf)<0) {
	 HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL, "chunked read failed");
      }
      break;

   default:
      assert ("not implemented yet" && 0);
      HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
		     "unsupported storage layout");
      break;
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_arr_write
 *
 * Purpose:	Writes an array to a hyperslab of a file byte array.  The
 *		memory array and the hyperslab are the same size.
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
herr_t H5F_arr_write (H5F_t *f, const struct H5O_layout_t *layout,
		      const size_t offset[], const size_t size[],
		      const void *buf)
{
   intn		i;
   size_t	nbytes;
   
   FUNC_ENTER (H5F_arr_write, FAIL);

   /* Check args */
   assert (f);
   assert (layout);
   assert (offset);
   assert (size);
   assert (buf);

   switch (layout->type) {
   case H5D_CONTIGUOUS:
      /*
       * We currently only support complete I/O.
       */
      for (i=0; i<layout->ndims; i++) {
	 assert (0==offset[i]);
	 assert (size[i]==layout->dim[i]);
      }
      for (i=0, nbytes=1; i<layout->ndims; i++) nbytes *= layout->dim[i];
      if (H5F_block_write (f, &(layout->addr), nbytes, buf)<0) {
	 HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");
      }
      break;

   case H5D_CHUNKED:
      if (H5F_istore_write (f, layout, offset, size, buf)<0) {
	 HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "chunked write failed");
      }
      break;

   default:
      assert ("not implemented yet" && 0);
      HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL,
		     "unsupported storage layout");
      break;
   }

   FUNC_LEAVE (SUCCEED);
}
   
