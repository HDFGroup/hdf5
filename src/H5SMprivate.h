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

/*
 * Programmer:	James Laird <jlaird@ncsa.uiuc.edu>
 *		Thursday, March 2, 2006
 *
 * Purpose:	This file contains private declarations for the H5SM
 *              shared object header messages module.
 */
#ifndef _H5SMprivate_H
#define _H5SMprivate_H

#include "H5Oprivate.h"
#include "H5Pprivate.h"

/****************************/
/* Library Private Typedefs */
/****************************/

#define H5SM_MAX_NUM_INDEXES 6

/******************************/
/* Library Private Prototypes */
/******************************/
H5_DLL herr_t H5SM_init(H5F_t *f, H5P_genplist_t *fc_plist, hid_t dxpl_id);
H5_DLL htri_t H5SM_try_share(H5F_t *f, hid_t dxpl_id, unsigned type_id,
                  void *mesg);
H5_DLL herr_t H5SM_try_delete(H5F_t *f, hid_t dxpl_id, unsigned type_id, const H5O_shared_t *mesg);
H5_DLL herr_t H5SM_get_info(H5F_t *f, unsigned *index_flags, unsigned *minsizes,
                  unsigned *list_to_btree, unsigned *btree_to_list, hid_t dxpl_id);
H5_DLL htri_t H5SM_type_shared(H5F_t *f, unsigned type_id, hid_t dxpl_id);
H5_DLL haddr_t H5SM_get_fheap_addr(H5F_t *f, unsigned type_id, hid_t dxpl_id);
H5_DLL herr_t H5SM_reconstitute(H5O_shared_t *sh_mesg, const uint8_t *heap_id);

#endif /*_H5SMprivate_H*/

