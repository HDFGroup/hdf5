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
 * Created:             H5Lpublic.h
 *                      Dec 1 2005
 *                      James Laird
 *
 * Purpose:             Public declarations for the H5L package (links)
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Lpublic_H
#define _H5Lpublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"
#include "H5Tpublic.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Types of links */
typedef enum H5L_link_t {
    H5L_LINK_ERROR	= -1,
    H5L_LINK_HARD	= 0,
    H5L_LINK_SOFT	= 1
} H5L_link_t;

/* Metadata buffer for user query function */
typedef struct H5L_linkinfo_t {
    H5T_cset_t          cset;           /* Character set of link name   */
    time_t              ctime;          /* Creation time                */
    H5L_link_t          linkclass;      /* Type of link                 */
    union {
        haddr_t         objno;          /* Data stored in a hard link   */
        size_t          link_size;      /* Size of a soft link          */
    } u;
} H5L_linkinfo_t;

#define H5L_SAME_LOC 0

H5_DLL herr_t H5Llink(hid_t cur_loc_id, const char *cur_name,
                        hid_t obj_id, hid_t lcpl_id);
H5_DLL herr_t H5Lmove(hid_t src_loc, const char *src_name, hid_t dst_loc,
                        const char *dst_name, hid_t lcpl_id);
H5_DLL herr_t H5Lcopy(hid_t src_loc, const char *src_name, hid_t dst_loc,
                        const char *dst_name, hid_t lcpl_id);
H5_DLL herr_t H5Lcreate_hard(hid_t cur_loc, const char *cur_name,
		        hid_t dst_loc, const char *dst_name, hid_t lcpl_id);
H5_DLL herr_t H5Lcreate_soft(const char *target_path, hid_t loc,
                             const char *name, hid_t lcpl_id);
H5_DLL herr_t H5Lunlink(hid_t loc_id, const char *name);
H5_DLL herr_t H5Lget_linkval(hid_t loc_id, const char *name, size_t size,
			      char *buf/*out*/);
H5_DLL herr_t H5Lget_linkinfo(hid_t loc_id, const char *name,
                              H5L_linkinfo_t *linkbuf /*out*/);


#ifdef __cplusplus
}
#endif
#endif

