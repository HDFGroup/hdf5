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
 * Created:		H5HFprivate.h
 *			Feb 24 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Private header for library accessible fractal heap routines.
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5HFprivate_H
#define _H5HFprivate_H

/* Include package's public header */
#include "H5HFpublic.h"

/* Private headers needed by this file */
#include "H5Fprivate.h"		/* File access				*/

/**************************/
/* Library Private Macros */
/**************************/


/****************************/
/* Library Private Typedefs */
/****************************/

/* Types of heaps */
typedef enum {
    H5HF_ABSOLUTE,      /* The heap uses absolute internal addressing */
    H5HF_MAPPED         /* The heap maps internal addresses to allow compaction */
} H5HF_type_t;


/*****************************/
/* Library-private Variables */
/*****************************/
 
/***************************************/
/* Library-private Function Prototypes */
/***************************************/
H5_DLL herr_t H5HF_create(H5F_t *f, hid_t dxpl_id, H5HF_type_t type,
    haddr_t *addr_p);
H5_DLL herr_t H5HF_insert(H5F_t *f, hid_t dxpl_id, haddr_t addr, size_t size,
    const void *obj, void *id/*out*/);

#endif /* _H5HFprivate_H */


