/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5MFprivate.h
 * 			Jul 11 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Private header file for file memory management.
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5MFprivate_H
#define _H5MFprivate_H
#include <H5MFpublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Fprivate.h>

/*
 * Library prototypes...
 */
haddr_t H5MF_alloc (H5F_t *f, size_t size);
herr_t H5MF_free (H5F_t *f, haddr_t addr, size_t size);

#endif
