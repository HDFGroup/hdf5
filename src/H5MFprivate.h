/*-------------------------------------------------------------------------
 * Copyright (C) 1997   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:             H5MFprivate.h
 *                      Jul 11 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Private header file for file memory management.
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
 * Feature: Define H5MF_DEBUG on the compiler command line if you want to
 *	    see diagnostics from this layer.
 */
#ifdef NDEBUG
#  undef H5MF_DEBUG
#endif

#define H5MF_META       0               /*request storage for meta data      */
#define H5MF_RAW        1               /*request storage for raw data       */

/*
 * Library prototypes...
 */
__DLL__ herr_t H5MF_alloc (H5F_t *f, intn, hsize_t size, haddr_t *addr/*out*/);
__DLL__ herr_t H5MF_xfree (H5F_t *f, const haddr_t *addr, hsize_t size);
__DLL__ herr_t H5MF_realloc (H5F_t *f, intn op, hsize_t orig_size,
		     const haddr_t *orig_addr, hsize_t new_size,
		     haddr_t *new_addr/*out*/);

#endif
