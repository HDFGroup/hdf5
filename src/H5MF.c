/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5MF.c
 * 			Jul 11 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		File memory management functions.
 *
 * Modifications:
 *
 * 	Robb Matzke, 5 Aug 1997
 *	Added calls to H5E.
 *
 *-------------------------------------------------------------------------
 */
#include <assert.h>
#include "hdf5.h"

#include "H5private.h"
#include "H5Fprivate.h"
#include "H5MFprivate.h"

#define PABLO_MASK	H5MF_mask

/* Is the interface initialized? */
static intn interface_initialize_g = FALSE;


/*-------------------------------------------------------------------------
 * Function:	H5MF_alloc
 *
 * Purpose:	Allocate at least SIZE bytes of file memory and return
 *		the address where that contiguous chunk of file memory
 *		exists.
 *
 * Return:	Success:	File address of new chunk.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5MF_alloc (hdf5_file_t *f, size_t size)
{
   haddr_t	addr;

   FUNC_ENTER (H5MF_alloc, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (f->logical_len>0);
   assert (size>0);

   /* reserve space from the end of the file */
   addr = f->logical_len;
   f->logical_len += size;

   FUNC_LEAVE (addr);
}


/*-------------------------------------------------------------------------
 * Function:	H5MF_free
 *
 * Purpose:	Frees part of a file, making that part of the file
 *		available for reuse.
 *
 * Note:	This version of the function doesn't do anything.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 17 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_free (hdf5_file_t *f, haddr_t addr, size_t size)
{
   FUNC_ENTER (H5MF_free, NULL, FAIL);

   /* check arguments */
   assert (f);
   if (addr<=0 || 0==size) HRETURN (SUCCEED);

#ifndef NDEBUG
   fprintf (stderr, "H5MF_free: lost %lu bytes of file storage\n",
	    (unsigned long)size);
#endif

   FUNC_LEAVE (SUCCEED);
}

