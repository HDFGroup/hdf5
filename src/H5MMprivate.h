/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5MMprivate.h
 * 			Jul 10 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Private header for memory management.
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5MMprivate_H
#define _H5MMprivate_h

#include "H5MMproto.h"

/*
 * Library prototypes...
 */
void *H5MM_xmalloc (size_t size);
void *H5MM_xcalloc (size_t n, size_t size);
void *H5MM_xrealloc (void *mem, size_t size);
char *H5MM_xstrdup (const char *s);
void *H5MM_xfree (void *mem);


#endif
