/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5BPprivate.h
 *			Apr 14 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Private header for library accessible B+ tree routines.
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5BPprivate_H
#define _H5BPprivate_H

/* Include package's public header */
#include "H5BPpublic.h"

/* Private headers needed by this file */
#include "H5Fprivate.h"		/* File access				*/
#include "H5MPprivate.h"	/* Memory Pools				*/

/**************************/
/* Library Private Macros */
/**************************/

     
/****************************/
/* Library Private Typedefs */
/****************************/

/* B+ tree IDs for various internal things. */
typedef enum H5BP_subid_t {
    H5BP_TEST_ID	 = 0,	/* B+ tree is for testing (do not use for actual data) */
    H5BP_NUM_BTREE_ID           /* Number of B-tree IDs (must be last)   */
} H5BP_subid_t;

/*
 * Each class of object that can be pointed to by a B+ tree has a
 * variable of this type that contains class variables and methods.
 */
typedef struct H5BP_class_t {
    H5BP_subid_t id;				/*id as found in file*/

    /* Copy native application record */
    void * (*copy)(const void *record);         /* Copy native record */

    /* Compare key */
    herr_t (*compare)(const void *key1, const void *key2);              /*  Compare two native keys */

    /* Encode & decode record values */
    /* (i.e. native application form <-> disk form) */
    herr_t (*raw_len)(const H5F_t *f, const void *record, size_t *len); /*  Determine length of raw (disk) form from native form */
    herr_t (*encode)(const H5F_t *f, const uint8_t *native, uint8_t *raw, size_t *rec_len); /*  Encode record from native form to disk storage form */
    void * (*decode)(const H5F_t *f, H5MP_pool_t *pool, const uint8_t *raw, size_t rec_len);               /*  Decode record from disk storage form to native form */

    /* Debug record values */
    herr_t (*debug) (FILE *stream, const H5F_t *f, hid_t dxpl_id,       /* Print a record for debugging */
        int indent, int fwidth, const void *record, const void *udata);

} H5BP_class_t;

/***************************************/
/* Library-private Function Prototypes */
/***************************************/
H5_DLL herr_t H5BP_create(H5F_t *f, hid_t dxpl_id, const H5BP_class_t *type,
    size_t node_size, unsigned split_percent, unsigned merge_percent,
    haddr_t *addr_p);
H5_DLL herr_t H5BP_insert(H5F_t *f, hid_t dxpl_id, const H5BP_class_t *type,
    haddr_t addr, void *udata);

#endif /* _H5BPprivate_H */

