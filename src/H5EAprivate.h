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

/* Extensible array creation parameters */
typedef struct H5EA_create_t {
    uint8_t elmt_size;                  /* Element size (in bytes) */
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
H5_DLL herr_t H5EA_get_nelmts(const H5EA_t *ea, hsize_t *nelmts);
H5_DLL herr_t H5EA_get_addr(const H5EA_t *ea, haddr_t *addr);
H5_DLL herr_t H5EA_delete(H5F_t *f, hid_t dxpl_id, haddr_t ea_addr);
H5_DLL herr_t H5EA_close(H5EA_t *ea, hid_t dxpl_id);

/* Statistics routines */
H5_DLL herr_t H5EA_get_stats(const H5EA_t *ea, H5EA_stat_t *stats);

/* Debugging routines */
#ifdef H5EA_DEBUGGING
#endif /* H5EA_DEBUGGING */

#endif /* _H5EAprivate_H */

