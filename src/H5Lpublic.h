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

/* Link classes.
 * Values less than 64 are reserved for the HDF5 library's internal use.
 * Values 64 to 255 are for "user-defined" link types; these types are
 * defined by HDF5 but their behavior can be overridden by users.
 * Users who want to create new classes of links should contact the HDF5
 * development team at hdfhelp@ncsa.uiuc.edu .
 * These values can never change because they appear in HDF5 files. 
 */
typedef int H5L_link_t;
#define H5L_LINK_ERROR    (-1)
#define H5L_LINK_HARD     0
#define H5L_LINK_SOFT     1
#define H5L_LINK_BUILTIN_MAX H5L_LINK_SOFT  /* Maximum value link value for "built-in" link types */
#define H5L_LINK_UD_MIN   64     /*link ids at or above this value are "user-defined" link types. */
#define H5L_LINK_EXTERNAL 64
#define H5L_LINK_MAX      255	 /*maximum link id                        */

/* Metadata buffer for user query function */
typedef struct H5L_linkinfo_t {
    H5T_cset_t          cset;           /* Character set of link name     */
    time_t              ctime;          /* Creation time                  */
    H5L_link_t          linkclass;      /* Type of link                   */
    union {
        haddr_t         address;        /* Address hard link points to    */
        size_t          link_size;      /* Size of a soft link or UD link */
    } u;
} H5L_linkinfo_t;

#define H5L_SAME_LOC 0

/* The H5L_link_class_t struct can be used to override the behavior of a
 * "user-defined" link class. Users should populate the struct with callback
 * functions defined below.
 */
/* Current version of the H5L_link_class_t struct */
#define H5L_LINK_CLASS_T_VERS (0)

/* Callback prototypes for user-defined links */
/* Link creation callback */
typedef herr_t (*H5L_create_func_t)(const char * link_name, hid_t loc_group, void * udata, size_t udata_size, hid_t lcpl_id);

/* Callback for when the link is moved */
typedef herr_t (*H5L_move_func_t)(const char * new_name, hid_t new_loc, void * udata, size_t udata_size);

/* Callback for when the link is moved */
typedef herr_t (*H5L_copy_func_t)(const char * new_name, hid_t new_loc, void * udata, size_t udata_size);

/* The actual link function, called during traversal */
typedef herr_t (*H5L_func_t)(const char * link_name, hid_t cur_group, void * udata, size_t udata_size, hid_t lapl_id);

/* Callback for when the link is deleted */
typedef herr_t (*H5L_delete_func_t)(const char * link_name, hid_t loc_group, void * udata, size_t udata_size);

/* Callback for querying the link */
/* Returns the size of the buffer needed */
typedef ssize_t (*H5L_query_func_t)(const char * link_name, void * udata, size_t udata_size, void * buf /*out*/, size_t buf_size);

/* User-defined link types */
typedef struct H5L_link_class_t {
    int version;                    /* Version number of this struct        */
    H5L_link_t id;                  /* Link type ID                         */
    const char *comment;            /* Comment for debugging                */
    H5L_create_func_t create_func;  /* Callback during link creation        */
    H5L_move_func_t move_func;      /* Callback after moving link           */
    H5L_copy_func_t copy_func;      /* Callback after copying link          */
    H5L_func_t trav_func;           /* The main traversal function          */
    H5L_delete_func_t del_func;     /* Callback for link deletion           */
    H5L_query_func_t query_func;    /* Callback for queries                 */
} H5L_link_class_t;

#define H5L_ELINK_PREFIX_PROP "elink_prefix"


/* Public prototypes */
H5_DLL herr_t H5Llink(hid_t cur_loc_id, const char *cur_name,
                        hid_t obj_id, hid_t lcpl_id, hid_t lapl_id);
H5_DLL herr_t H5Lmove(hid_t src_loc, const char *src_name, hid_t dst_loc,
                        const char *dst_name, hid_t lcpl_id, hid_t lapl_id);
H5_DLL herr_t H5Lcopy(hid_t src_loc, const char *src_name, hid_t dst_loc,
                        const char *dst_name, hid_t lcpl_id, hid_t lapl_id);
H5_DLL herr_t H5Lcreate_hard(hid_t cur_loc, const char *cur_name,
		        hid_t dst_loc, const char *dst_name, hid_t lcpl_id,
                        hid_t lapl_id);
H5_DLL herr_t H5Lcreate_soft(const char *target_path, hid_t cur_loc,
                             const char *cur_name, hid_t lcpl_id, hid_t lapl_id);
H5_DLL herr_t H5Lunlink(hid_t loc_id, const char *name, hid_t lapl_id);
H5_DLL herr_t H5Lget_linkval(hid_t loc_id, const char *name, size_t size,
			      char *buf/*out*/, hid_t lapl_id);
H5_DLL herr_t H5Lget_linkinfo(hid_t loc_id, const char *name,
                              H5L_linkinfo_t *linkbuf /*out*/, hid_t lapl_id);

/* UD link functions */
H5_DLL herr_t H5Lcreate_ud(hid_t link_loc_id, const char *link_name,
        H5L_link_t link_type, void * udata, size_t udata_size, hid_t lcpl_id,
        hid_t lapl_id);
H5_DLL herr_t H5Lregister(const H5L_link_class_t *cls);
H5_DLL herr_t H5Lunregister(H5L_link_t id);
H5_DLL htri_t H5Lis_registered(H5L_link_t id);

/* External link functions */
H5_DLL herr_t H5Lunpack_elink_val(char * ext_linkval/*in*/,
            char ** filename/*out*/, char** obj_path /*out*/);
H5_DLL herr_t H5Lcreate_external(const char *file_name, const char *obj_name,
        hid_t link_loc_id, const char *link_name, hid_t lcpl_id, hid_t lapl_id);

#ifdef __cplusplus
}
#endif
#endif

