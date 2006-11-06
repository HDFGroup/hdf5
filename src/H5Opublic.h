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
 * Created:             H5Opublic.h
 *                      Aug  5 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Public declarations for the H5O (object header)
 *                      package.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Opublic_H
#define _H5Opublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/* Flags for object copy (H5Ocopy) */
#define H5O_COPY_SHALLOW_HIERARCHY_FLAG (0x0001u)   /* Copy only immediate members */
#define H5O_COPY_EXPAND_SOFT_LINK_FLAG  (0x0002u)   /* Expand soft links into new objects */
#define H5O_COPY_EXPAND_EXT_LINK_FLAG   (0x0004u)   /* Expand external links into new objects */
#define H5O_COPY_EXPAND_REFERENCE_FLAG	(0x0008u)   /* Copy objects that are pointed by references */
#define H5O_COPY_WITHOUT_ATTR_FLAG      (0x0010u)   /* Copy object without copying attributes */
#define H5O_COPY_ALL                    (0x001Fu)   /* All object copying flags (for internal checking) */

typedef struct H5O_stat_t {
    hsize_t size;               /* Total size of object header in file */
    hsize_t free;               /* Free space within object header */
    unsigned nmesgs;            /* Number of object header messages */
    unsigned nchunks;           /* Number of object header chunks */
} H5O_stat_t;

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t H5Oopen(hid_t loc_id, const char *name, hid_t lapl_id);
H5_DLL hid_t H5Oopen_by_addr(hid_t loc_id, haddr_t addr);
H5_DLL herr_t H5Oincr_refcount(hid_t object_id);
H5_DLL herr_t H5Odecr_refcount(hid_t object_id);
H5_DLL herr_t H5Ocopy(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
    const char *dst_name, hid_t ocpypl_id, hid_t lcpl_id);
H5_DLL herr_t H5Oclose(hid_t object_id);

#ifdef __cplusplus
}
#endif
#endif
