/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5Gproto.h
 * 			Jul 11 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Public declarations for the H5G package (symbol
 *			tables).
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Gpublic_H
#define _H5Gpublic_H

/* Public headers needed by this file */
#include <sys/types.h>
#include <H5public.h>
#include <H5Apublic.h>

#ifdef __cplusplus
extern "C" {
#endif

herr_t H5Gnew (hid_t file, const char *name, size_t size_hint);
herr_t H5Gset (hid_t file, const char *name);
herr_t H5Gpush (hid_t file, const char *name);
herr_t H5Gpop (hid_t file);
   
#ifdef __cplusplus
}
#endif

#endif
