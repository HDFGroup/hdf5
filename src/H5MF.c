/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5MF.c
 * 			Jul 11 1997
 * 			Robb Matzke <robb@maya.nuance.com>
 *
 * Purpose:		File memory management functions.
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#include <assert.h>
#include "hdf5.h"

#include "H5Fprivate.h"
#include "H5MFprivate.h"


/*-------------------------------------------------------------------------
 * Function:	H5MF_alloc
 *
 * Purpose:	Allocate at least SIZE bytes of file memory and return
 *		the address where that contiguous chunk of file memory
 *		exists.
 *
 * Return:	Success:	File address of new chunk.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
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
   
   assert (f);
   assert (f->logical_len>0);
   assert (size>0);

   addr = f->logical_len;
   f->logical_len += size;
   return addr;
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
 *		robb@maya.nuance.com
 *		Jul 17 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_free (hdf5_file_t *f, haddr_t addr, size_t size)
{
   if (addr<=0 || 0==size) return 0;

   fprintf (stderr, "H5MF_free: lost %lu bytes of file storage\n",
	    (unsigned long)size);
   
   return 0;
}

