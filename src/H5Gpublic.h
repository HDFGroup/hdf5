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
 * Created:             H5Gpublic.h
 *                      Jul 11 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Public declarations for the H5G package (symbol
 *                      tables).
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Gpublic_H
#define _H5Gpublic_H

/* Public headers needed by this file */
#include <sys/types.h>

#include "H5public.h"
#include "H5Ipublic.h"
#include "H5Lpublic.h"
#include "H5Opublic.h"
#include "H5Tpublic.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * An object has a certain type. The first few numbers are reserved for use
 * internally by HDF5. Users may add their own types with higher values.  The
 * values are never stored in the file -- they only exist while an
 * application is running.  An object may satisfy the `isa' function for more
 * than one type.
 */
typedef enum H5G_obj_t {
    H5G_UNKNOWN = -1,		/* Unknown object type		*/
    H5G_GROUP,		        /* Object is a group		*/
    H5G_DATASET,		/* Object is a dataset		*/
    H5G_TYPE,			/* Object is a named data type	*/
    H5G_LINK,		        /* Object is a symbolic link	*/
    H5G_UDLINK,		        /* Object is a user-defined link */
    H5G_RESERVED_5,		/* Reserved for future use	*/
    H5G_RESERVED_6,		/* Reserved for future use	*/
    H5G_RESERVED_7		/* Reserved for future use	*/
} H5G_obj_t;

#define H5G_NTYPES	256		/* Max possible number of types	*/
#define H5G_NLIBTYPES	8		/* Number of internal types	*/
#define H5G_NUSERTYPES	(H5G_NTYPES-H5G_NLIBTYPES)
#define H5G_USERTYPE(X)	(8+(X))		/* User defined types		*/

/* Information about an object */
typedef struct H5G_stat_t {
    unsigned long 	fileno[2];	/*file number			*/
    unsigned long 	objno[2];	/*object number			*/
    unsigned 		nlink;		/*number of hard links to object*/
    H5G_obj_t 		type;		/*basic object type		*/
    time_t		mtime;		/*modification time		*/
    size_t		linklen;	/*symbolic link value length	*/
    H5O_stat_t          ohdr;           /* Object header information    */
} H5G_stat_t;

typedef herr_t (*H5G_iterate_t)(hid_t group, const char *name,
				void *op_data);

/* Flags for object copy (H5Gcopy) */
#define H5G_COPY_SHALLOW_HIERARCHY_FLAG         (0x0002u)   /* Copy only immediate members */
#define H5G_COPY_EXPAND_SOFT_LINK_FLAG          (0x0004u)   /* Expand soft links into new objects */
#define H5G_COPY_EXPAND_EXT_LINK_FLAG           (0x0008u)   /* Expand external links into new objects */
#define H5G_COPY_EXPAND_REFERENCE_FLAG      (0x0010u)   /* Copy objects that are pointed by references */
#define H5G_COPY_WITHOUT_ATTR_FLAG              (0x0020u)   /* Copy object without copying attributes */
#define H5G_COPY_ALL                            (0x003Fu)   /* All object copying flags (for internal checking) */

H5_DLL hid_t H5Gcreate(hid_t loc_id, const char *name, size_t size_hint);
H5_DLL hid_t H5Gopen(hid_t loc_id, const char *name);
H5_DLL hid_t H5Gopen_expand(hid_t loc_id, const char *name, hid_t gapl_id);
H5_DLL herr_t H5Gclose(hid_t group_id);
H5_DLL herr_t H5Giterate(hid_t loc_id, const char *name, int *idx,
			  H5G_iterate_t op, void *op_data);
H5_DLL herr_t H5Gget_num_objs(hid_t loc_id, hsize_t *num_objs);
H5_DLL ssize_t H5Gget_objname_by_idx(hid_t loc_id, hsize_t idx, char* name, size_t size);
H5_DLL H5G_obj_t H5Gget_objtype_by_idx(hid_t loc_id, hsize_t idx);
H5_DLL herr_t H5Gget_objinfo(hid_t loc_id, const char *name,
			      hbool_t follow_link, H5G_stat_t *statbuf/*out*/);
H5_DLL herr_t H5Gset_comment(hid_t loc_id, const char *name,
			      const char *comment);
H5_DLL int H5Gget_comment(hid_t loc_id, const char *name, size_t bufsize,
			   char *buf);
H5_DLL hid_t H5Gcreate_expand(hid_t loc_id, hid_t gcpl_id,
    hid_t gapl_id);
H5_DLL hid_t H5Gget_create_plist(hid_t group_id);
H5_DLL herr_t H5Gcopy(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
        const char *dst_name, hid_t ocpypl_id, hid_t lcpl_id);

/* Functions and variables defined for compatibility with previous versions
 * of the HDF5 API.
 * Use of these functions and variables is depreciated.
 */
H5_DLL herr_t H5Glink(hid_t cur_loc_id, H5L_type_t type,
               const char *cur_name, const char *new_name);
H5_DLL herr_t H5Gmove(hid_t src_loc_id, const char *src_name,
                      const char *dst_name);
H5_DLL herr_t H5Glink2(hid_t cur_loc_id, const char *cur_name,
                   H5L_type_t type, hid_t new_loc_id, const char *new_name);
H5_DLL herr_t H5Gmove2(hid_t src_loc_id, const char *src_name,
                       hid_t dst_loc_id, const char *dst_name);
H5_DLL herr_t H5Gunlink(hid_t loc_id, const char *name);
H5_DLL herr_t H5Gget_linkval(hid_t loc_id, const char *name,
                      size_t size, char *buf/*out*/);

#define H5G_LINK_ERROR H5L_TYPE_ERROR
#define H5G_LINK_HARD H5L_TYPE_HARD
#define H5G_LINK_SOFT H5L_TYPE_SOFT
#define H5G_link_t H5L_type_t
#define H5G_SAME_LOC H5L_SAME_LOC


#ifdef __cplusplus
}
#endif
#endif

