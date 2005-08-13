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
 * Created:		H5SHprivate.h
 *			Mar 10 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Private header for library accessible segmented heap routines.
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5SHprivate_H
#define _H5SHprivate_H

/* Include package's public header */
#include "H5SHpublic.h"

/* Private headers needed by this file */
#include "H5Fprivate.h"		/* File access				*/

/**************************/
/* Library Private Macros */
/**************************/


/****************************/
/* Library Private Typedefs */
/****************************/

/* Type of data in heap */
typedef enum H5SH_data_type_t {
    H5SH_RAW,	        /* Heap data is "raw data" from dataset */
    H5SH_META	        /* Heap data is metadata about file's structure */
} H5SH_data_type_t;

/***************************************/
/* Library-private Function Prototypes */
/***************************************/
H5_DLL herr_t H5SH_create(H5F_t *f, hid_t dxpl_id, haddr_t *addr_p,
    H5SH_data_type_t heap_type, hsize_t min_size, hsize_t max_extend_size);
H5_DLL herr_t H5SH_alloc(H5F_t *f, hid_t dxpl_id, haddr_t addr, hsize_t size,
    haddr_t *obj_addr_p);

#endif /* _H5SHprivate_H */

