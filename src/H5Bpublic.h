/*-------------------------------------------------------------------------
 * Copyright (C) 1997-2001 National Center for Supercomputing Applications
 *                         All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:             H5Bproto.h
 *                      Jul 10 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Public declarations for the H5B package.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Bpublic_H
#define _H5Bpublic_H

/* Public headers needed by this file */
#include "H5public.h"

/* B-tree IDs for various internal things. */
/* Not really a "public" symbol, but that should be OK -QAK */
typedef enum H5B_subid_t {
    H5B_SNODE_ID	 = 0,	/*B-tree is for symbol table nodes	     */
    H5B_ISTORE_ID	 = 1,	/*B-tree is for indexed object storage	     */
    H5B_NUM_BTREE_ID            /* Number of B-tree key IDs (must be last)   */
} H5B_subid_t;

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
}
#endif
#endif
