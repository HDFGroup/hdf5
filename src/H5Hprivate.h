/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5Hprivate.h
 * 			Jul 16 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Hprivate_H
#define _H5Hprivate_H
#include <H5Hpublic.h>

/* Private headers needed by this file. */
#include <H5private.h>
#include <H5Fprivate.h>

#define H5H_MAGIC	"HEAP"		/*heap magic number		*/
#define H5H_SIZEOF_MAGIC 4

#define H5H_SIZEOF_HDR(F)						      \
   (H5H_SIZEOF_MAGIC +			/*heap signature		*/    \
    H5F_SIZEOF_SIZE (F) +		/*data size			*/    \
    H5F_SIZEOF_OFFSET (F) +		/*free list head		*/    \
    H5F_SIZEOF_OFFSET (F))		/*data address			*/
   
#define H5H_SIZEOF_FREE(F)						      \
   (H5F_SIZEOF_OFFSET (F) +		/*ptr to next free block	*/    \
   H5F_SIZEOF_SIZE (F))			/*size of this free block	*/


typedef enum H5H_type_t {
   H5H_LOCAL	=0,			/*local symtab name heap	*/
   H5H_GLOBAL	=1			/*global small object heap	*/
} H5H_type_t;

/*
 * Library prototypes...
 */
haddr_t H5H_new (H5F_t *f, H5H_type_t type, size_t size_hint);
void *H5H_read (H5F_t *f, haddr_t addr, off_t offset, size_t size, void *buf);
const void *H5H_peek (H5F_t *f, haddr_t addr, off_t offset);
off_t H5H_insert (H5F_t *f, haddr_t addr, size_t size, const void *buf);
herr_t H5H_write (H5F_t *f, haddr_t addr, off_t offset, size_t size,
		  const void *buf);
herr_t H5H_remove (H5F_t *f, haddr_t addr, off_t offset, size_t size);
herr_t H5H_debug (H5F_t *f, haddr_t addr, FILE *stream, intn indent,
		  intn fwidth);

#endif
