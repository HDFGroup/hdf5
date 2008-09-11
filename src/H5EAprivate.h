/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5EAprivate.h
 *			Jun 17 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Private header for library accessible extensible
 *                      array routines.
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5EAprivate_H
#define _H5EAprivate_H

/* Include package's public header */
#ifdef NOT_YET
#include "H5EApublic.h"
#endif /* NOT_YET */

/* Private headers needed by this file */
#include "H5Fprivate.h"		/* File access				*/


/**************************/
/* Library Private Macros */
/**************************/


/****************************/
/* Library Private Typedefs */
/****************************/

/* Extensible array class IDs */
typedef enum H5EA_cls_id_t {
    /* Start real class IDs at 0 -QAK */
    H5EA_CLS_TEST_ID,	        /* Extensible array is for testing (do not use for actual data) */
    H5EA_NUM_CLS_ID             /* Number of Extensible Array class IDs (must be last) */
} H5EA_cls_id_t;

/*
 * Each type of element that can be stored in an extesible array has a
 * variable of this type that contains class variables and methods.
 */
typedef struct H5EA_class_t {
    H5EA_cls_id_t id;           /* ID of Extensible Array class, as found in file */
    size_t nat_elmt_size;       /* Size of native (memory) element */

    /* Extensible array client callback methods */
    herr_t (*fill)(void *nat_blk, size_t nelmts);    /* Fill array of elements with encoded form of "missing element" value */
    herr_t (*encode)(void *raw, const void *elmt, size_t nelmts);   /* Encode elements from native form to disk storage form */
    herr_t (*decode)(const void *raw, void *elmt, size_t nelmts);   /* Decode elements from disk storage form to native form */
    herr_t (*debug)(FILE *stream, int indent, int fwidth, hsize_t idx, const void *elmt); /* Print an element for debugging */
} H5EA_class_t;

/* Extensible array creation parameters */
typedef struct H5EA_create_t {
    const H5EA_class_t *cls;            /* Class of extensible array to create */
    uint8_t raw_elmt_size;              /* Element size in file (in bytes) */
    uint8_t max_nelmts_bits;            /* Log2(Max. # of elements in array) - i.e. # of bits needed to store max. # of elements */
    uint8_t idx_blk_elmts;              /* # of elements to store in index block */
    uint8_t data_blk_min_elmts;         /* Min. # of elements per data block */
    uint8_t sup_blk_min_data_ptrs;      /* Min. # of data block pointers for a super block */
} H5EA_create_t;

/* Extensible array metadata statistics info */
typedef struct H5EA_stat_t {
    hsize_t nsuper_blks;        /* # of super blocks */
    hsize_t ndata_blks;         /* # of data blocks */
} H5EA_stat_t;

/* Extensible array info (forward decl - defined in H5EApkg.h) */
typedef struct H5EA_t H5EA_t;


/*****************************/
/* Library-private Variables */
/*****************************/


/***************************************/
/* Library-private Function Prototypes */
/***************************************/

/* General routines */
H5_DLL H5EA_t *H5EA_create(H5F_t *f, hid_t dxpl_id, const H5EA_create_t *cparam);
H5_DLL H5EA_t *H5EA_open(H5F_t *f, hid_t dxpl_id, haddr_t ea_addr,
    const H5EA_class_t *cls);
H5_DLL herr_t H5EA_get_nelmts(const H5EA_t *ea, hsize_t *nelmts);
H5_DLL herr_t H5EA_get_addr(const H5EA_t *ea, haddr_t *addr);
H5_DLL herr_t H5EA_set(const H5EA_t *ea, hid_t dxpl_id, hsize_t idx, const void *elmt);
herr_t H5EA_get(const H5EA_t *ea, hid_t dxpl_id, hsize_t idx, void *elmt);
H5_DLL herr_t H5EA_close(H5EA_t *ea, hid_t dxpl_id);
H5_DLL herr_t H5EA_delete(H5F_t *f, hid_t dxpl_id, haddr_t ea_addr);

/* Statistics routines */
H5_DLL herr_t H5EA_get_stats(const H5EA_t *ea, H5EA_stat_t *stats);

/* Debugging routines */
#ifdef H5EA_DEBUGGING
#endif /* H5EA_DEBUGGING */

#endif /* _H5EAprivate_H */

