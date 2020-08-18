/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:             H5Gpublic.h
 *                      Jul 11 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Public declarations for the H5G package
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Gpublic_H
#define _H5Gpublic_H

/* System headers needed by this file */
#include <sys/types.h>

/* Public headers needed by this file */
#include "H5public.h"		/* Generic Functions			*/
#include "H5Lpublic.h"		/* Links                                */
#include "H5Opublic.h"		/* Object headers			*/
#include "H5Tpublic.h"		/* Datatypes				*/

/*****************/
/* Public Macros */
/*****************/

/*******************/
/* Public Typedefs */
/*******************/

/* Types of link storage for groups */
typedef enum H5G_storage_type_t {
    H5G_STORAGE_TYPE_UNKNOWN = -1,	/* Unknown link storage type	*/
    H5G_STORAGE_TYPE_SYMBOL_TABLE,      /* Links in group are stored with a "symbol table" */
                                        /* (this is sometimes called "old-style" groups) */
    H5G_STORAGE_TYPE_COMPACT,		/* Links are stored in object header */
    H5G_STORAGE_TYPE_DENSE 		/* Links are stored in fractal heap & indexed with v2 B-tree */
} H5G_storage_type_t;

/* Information struct for group (for H5Gget_info/H5Gget_info_by_name/H5Gget_info_by_idx) */
typedef struct H5G_info_t {
    H5G_storage_type_t 	storage_type;	/* Type of storage for links in group */
    hsize_t 	nlinks;		        /* Number of links in group */
    int64_t     max_corder;             /* Current max. creation order value for group */
    hbool_t     mounted;                /* Whether group has a file mounted on it */
} H5G_info_t;


/********************/
/* Public Variables */
/********************/


/*********************/
/* Public Prototypes */
/*********************/
#ifdef __cplusplus
extern "C" {
#endif

/* --------------------------------------------------------------------------*/
/**\ingroup H5G
 *
 * \brief Creates a new group and links it into the file
 *
 * \fgdta_loc_id
 * \param[in] name      Name of the group to create
 * \lcpl_id
 * \gcpl_id
 * \gapl_id
 *
 * \return \hid_t{group}
 *
 * \details H5Gcreate2() creates a new group in a file. After a group
 * has been created, links to datasets and to other groups can be added.
 *
 * The \p loc_id and \p name parameters specify where the group is
 * located. \p loc_id may be a file, group, dataset, named datatype or
 * attribute in the file. If an attribute, dataset, or named datatype is
 * specified for \p loc_id then the group will be created at the location
 * where the attribute, dataset, or named datatype is attached. \p name
 * is the link to the group; \p name may be either an absolute path in
 * the file (the links from the root group to the new group) or a relative
 * path from \p loc_id (the link(s) from the group specified by \p loc_id
 * to the new group).
 *
 * \p lcpl_id, \p gcpl_id, and \p gapl_id are property list
 * identifiers. These property lists govern how the link to the group is
 * created, how the group is created, and how the group can be accessed in
 * the future, respectively. #H5P_DEFAULT can be passed in if the default
 * properties are appropriate for these property lists. Currently, there
 * are no APIs for the group access property list; use #H5P_DEFAULT.
 *
 * To conserve and release resources, the group should be closed when access
 * is no longer required.
 *
 * \since 1.8.0
 *
 * \see H5Gopen2(), H5Gclose()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Gcreate2(hid_t loc_id, const char *name, hid_t lcpl_id,
    hid_t gcpl_id, hid_t gapl_id);
H5_DLL hid_t H5Gcreate_anon(hid_t loc_id, hid_t gcpl_id, hid_t gapl_id);
/* --------------------------------------------------------------------------*/
/**\ingroup H5G
 *
 * \brief Opens an existing group in a file
 *
 * \fgdta_loc_id
 * \param[in] name      Name of the group to open
 * \gapl_id
 *
 * \return \hid_t{group}
 *
 * \details H5Gopen2() opens an existing group, name, at the location
 * specified by \p loc_id.
 *
 * With default settings, H5Gopen2() provides similar functionality to that
 * provided by H5Gopen(). The only difference is that H5Gopen2() can provide
 * a group access property list, \p gapl_id.
 *
 * H5Gopen2() returns a group identifier for the group that was opened. This
 * group identifier should be released by calling H5Gclose() when it is no
 * longer needed.
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gclose()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Gopen2(hid_t loc_id, const char *name, hid_t gapl_id);
H5_DLL hid_t H5Gget_create_plist(hid_t group_id);
H5_DLL herr_t H5Gget_info(hid_t loc_id, H5G_info_t *ginfo);
H5_DLL herr_t H5Gget_info_by_name(hid_t loc_id, const char *name, H5G_info_t *ginfo,
    hid_t lapl_id);
H5_DLL herr_t H5Gget_info_by_idx(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5G_info_t *ginfo,
    hid_t lapl_id);
/* --------------------------------------------------------------------------*/
/**\ingroup H5G
 *
 * \brief Closes the specified group
 *
 * \group_id
 *
 * \return \herr_t
 *
 * \details H5Gclose() releases resources used by a group which was
 * opened by H5Gcreate() or H5Gopen() After closing a group, \p group_id
 * cannot be used again until another H5Gcreate() or H5Gopen() is called on it.
 *
 * Failure to release a group with this call will result in resource leaks.
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gopen2()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Gclose(hid_t group_id);
H5_DLL herr_t H5Gflush(hid_t group_id);
H5_DLL herr_t H5Grefresh(hid_t group_id);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */

/* Link definitions */
#define H5G_SAME_LOC H5L_SAME_LOC
#define H5G_LINK_ERROR H5L_TYPE_ERROR
#define H5G_LINK_HARD H5L_TYPE_HARD
#define H5G_LINK_SOFT H5L_TYPE_SOFT
#define H5G_link_t H5L_type_t

/* Macros for types of objects in a group (see H5G_obj_t definition) */
#define H5G_NTYPES	256		/* Max possible number of types	*/
#define H5G_NLIBTYPES	8		/* Number of internal types	*/
#define H5G_NUSERTYPES	(H5G_NTYPES - H5G_NLIBTYPES)
#define H5G_USERTYPE(X)	(8 + (X))	/* User defined types		*/


/* Typedefs */

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

/* Prototype for H5Giterate() operator */
typedef herr_t (*H5G_iterate_t)(hid_t group, const char *name, void *op_data);

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


/* Function prototypes */
H5_DLL hid_t H5Gcreate1(hid_t loc_id, const char *name, size_t size_hint);
H5_DLL hid_t H5Gopen1(hid_t loc_id, const char *name);
H5_DLL herr_t H5Glink(hid_t cur_loc_id, H5G_link_t type, const char *cur_name,
    const char *new_name);
H5_DLL herr_t H5Glink2(hid_t cur_loc_id, const char *cur_name, H5G_link_t type,
    hid_t new_loc_id, const char *new_name);
H5_DLL herr_t H5Gmove(hid_t src_loc_id, const char *src_name,
    const char *dst_name);
H5_DLL herr_t H5Gmove2(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
    const char *dst_name);
H5_DLL herr_t H5Gunlink(hid_t loc_id, const char *name);
H5_DLL herr_t H5Gget_linkval(hid_t loc_id, const char *name, size_t size,
    char *buf/*out*/);
H5_DLL herr_t H5Gset_comment(hid_t loc_id, const char *name, const char *comment);
H5_DLL int H5Gget_comment(hid_t loc_id, const char *name, size_t bufsize,
    char *buf);
H5_DLL herr_t H5Giterate(hid_t loc_id, const char *name, int *idx,
        H5G_iterate_t op, void *op_data);
H5_DLL herr_t H5Gget_num_objs(hid_t loc_id, hsize_t *num_objs);
H5_DLL herr_t H5Gget_objinfo(hid_t loc_id, const char *name,
    hbool_t follow_link, H5G_stat_t *statbuf/*out*/);
H5_DLL ssize_t H5Gget_objname_by_idx(hid_t loc_id, hsize_t idx, char* name,
    size_t size);
H5_DLL H5G_obj_t H5Gget_objtype_by_idx(hid_t loc_id, hsize_t idx);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* _H5Gpublic_H */

