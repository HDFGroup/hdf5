/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5Hprivate.h
 * 			Jul 16 1997
 * 			Robb Matzke <robb@maya.nuance.com>
 *
 * Purpose:		
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Hprivate_H
#define _H5Hprivate_H

#include "H5Hproto.h"
#include "H5Fprivate.h"

#define H5H_MAGIC	"HEAP"		/*heap magic number		*/
#define H5H_SIZEOF_MAGIC 4

/*
 * Library prototypes...
 */
haddr_t H5H_new (hdf5_file_t *f, size_t size_hint);
void *H5H_read (hdf5_file_t *f, haddr_t addr, off_t offset, size_t size,
		void *buf);
const void *H5H_peek (hdf5_file_t *f, haddr_t addr, off_t offset);
off_t H5H_insert (hdf5_file_t *f, haddr_t addr, size_t size, const void *buf);
herr_t H5H_write (hdf5_file_t *f, haddr_t addr, off_t offset, size_t size,
		  const void *buf);
herr_t H5H_remove (hdf5_file_t *f, haddr_t addr, off_t offset, size_t size);
herr_t H5H_debug (hdf5_file_t *f, haddr_t addr, FILE *stream, intn indent,
		  intn fwidth);


#endif
