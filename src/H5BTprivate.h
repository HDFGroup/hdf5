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
 * Created:		H5BTprivate.h
 *			Mar 10 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Private header for library accessible block tracker routines.
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5BTprivate_H
#define _H5BTprivate_H

/* Include package's public header */
#include "H5BTpublic.h"

/* Private headers needed by this file */
#include "H5Fprivate.h"		/* File access				*/

/**************************/
/* Library Private Macros */
/**************************/

     
/****************************/
/* Library Private Typedefs */
/****************************/


/***************************************/
/* Library-private Function Prototypes */
/***************************************/
H5_DLL herr_t H5BT_create(H5F_t *f, hid_t dxpl_id, haddr_t *addr_p);
H5_DLL herr_t H5BT_insert(H5F_t *f, hid_t dxpl_id, haddr_t addr, haddr_t offset,
    hsize_t length);
H5_DLL herr_t H5BT_remove(H5F_t *f, hid_t dxpl_id, haddr_t addr, haddr_t offset,
    hsize_t length);
H5_DLL herr_t H5BT_get_total_size(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    hsize_t *tot_size);
H5_DLL herr_t H5BT_locate(H5F_t *f, hid_t dxpl_id, haddr_t addr, hsize_t size,
    haddr_t *locate_addr, hsize_t *locate_size);
H5_DLL herr_t H5BT_delete(H5F_t *f, hid_t dxpl_id, haddr_t addr);

#endif /* _H5BTprivate_H */

