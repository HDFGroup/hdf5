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
 * Created:		H5B2private.h
 *			Jan 31 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Private header for library accessible B-tree routines.
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5B2private_H
#define _H5B2private_H

/* Include package's public header */
#include "H5B2public.h"

/* Private headers needed by this file */
#include "H5Fprivate.h"		/* File access				*/

/**************************/
/* Library Private Macros */
/**************************/

/*
 * Feature: Define this constant if you want to check B-tree consistency
 *	    after each B-tree operation.  Note that this slows down the
 *	    library considerably! Debugging the B-tree depends on assert()
 *	    being enabled.
 */
/* #define H5B2_DEBUG */
     
/****************************/
/* Library Private Typedefs */
/****************************/

/* B-tree IDs for various internal things. */
/* Not really a "public" symbol, but that should be OK -QAK */
/* Note - if more of these are added, any 'K' values (for internal or leaf
 * nodes) they use will need to be stored in the file somewhere. -QAK
 */
typedef enum H5B2_subid_t {
    H5B2_GRP_NAME_ID	 = 0,	/* B-tree is for group links, ordered by name	*/
    H5B2_NUM_BTREE_ID           /* Number of B-tree key IDs (must be last)   */
} H5B2_subid_t;

/*
 * Each class of object that can be pointed to by a B-link tree has a
 * variable of this type that contains class variables and methods.
 */
typedef struct H5B2_class_t {
    H5B2_subid_t id;				/*id as found in file*/
    size_t	nkey_size;			/*size of native (memory) key*/

    /* Store & retrieve records */
    herr_t (*store)(const H5F_t *f, hid_t dxpl_id, const void *udata, void *nrecord);  /*  Store record in native key table */

    /* Compare records, according to a key */
    herr_t (*compare)(const H5F_t *f, hid_t dxpl_id, const void *rec1, const void *rec2);  /*  Compare two native records */

    /* Encode & decode record values */
    herr_t (*encode)(const H5F_t *f, uint8_t *raw, const void *record);  /*  Encode record from native form to disk storage form */
    herr_t (*decode)(const H5F_t *f, const uint8_t *raw, void *record);  /*  Decode record from disk storage form to native form */

    /* Debug record values */

} H5B2_class_t;

/***************************************/
/* Library-private Function Prototypes */
/***************************************/
H5_DLL herr_t H5B2_create(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    size_t node_size, size_t rkey_size,
    unsigned split_percent, unsigned merge_percent,
    haddr_t *addr_p);
H5_DLL herr_t H5B2_insert(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr, void *udata);

#endif /* _H5B2private_H */

