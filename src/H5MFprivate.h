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

#define H5MF_META       0       /*request storage for meta data           */
#define H5MF_RAW        1       /*request storage for raw data            */

/*
 * Library prototypes...
 */
herr_t                  H5MF_alloc(H5F_t *f, intn, size_t size, haddr_t *addr /*out */ );
herr_t                  H5MF_free(H5F_t *f, const haddr_t *addr, size_t size);

#endif
