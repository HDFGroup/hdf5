/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:             H5Gpublic.h
 *                      Jul 11 1997
 *                      Robb Matzke
 *
 * Purpose:             Public declarations for the H5G package
 *
 *-------------------------------------------------------------------------
 */
#ifndef H5Gpublic_H
#define H5Gpublic_H

/* System headers needed by this file */
#include <sys/types.h>

/* Public headers needed by this file */
#include "H5public.h"  /* Generic Functions			*/
#include "H5Lpublic.h" /* Links                                */
#include "H5Opublic.h" /* Object headers			*/
#include "H5Tpublic.h" /* Datatypes				*/

/*****************/
/* Public Macros */
/*****************/

/*******************/
/* Public Typedefs */
/*******************/

/* Types of link storage for groups */
typedef enum H5G_storage_type_t {
    H5G_STORAGE_TYPE_UNKNOWN = -1, /* Unknown link storage type	*/
    H5G_STORAGE_TYPE_SYMBOL_TABLE, /* Links in group are stored with a "symbol table" */
                                   /* (this is sometimes called "old-style" groups) */
    H5G_STORAGE_TYPE_COMPACT,      /* Links are stored in object header */
    H5G_STORAGE_TYPE_DENSE         /* Links are stored in fractal heap & indexed with v2 B-tree */
} H5G_storage_type_t;

/* Information struct for group (for H5Gget_info/H5Gget_info_by_name/H5Gget_info_by_idx) */
//! [H5G_info_t_snip]
typedef struct H5G_info_t {
    H5G_storage_type_t storage_type; /* Type of storage for links in group */
    hsize_t            nlinks;       /* Number of links in group */
    int64_t            max_corder;   /* Current max. creation order value for group */
    hbool_t            mounted;      /* Whether group has a file mounted on it */
} H5G_info_t;
//! [H5G_info_t_snip]

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/
#ifdef __cplusplus
extern "C" {
#endif

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
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
 * \details H5Gcreate2() creates a new group in a file. After a
 *          group has been created, links to datasets and to other groups
 *          can be added.
 *
 *          The \p loc_id and \p name parameters specify where the group
 *          is located. \p loc_id may be a file, group, dataset, named
 *          datatype or attribute in the file. If an attribute, dataset,
 *          or named datatype is specified for \p loc_id then the group
 *          will be created at the location where the attribute, dataset,
 *          or named datatype is attached. \p name is the link to the group;
 *          \p name may be either an absolute path in the file (the links
 *          from the root group to the new group) or a relative path from
 *          \p loc_id (the link(s) from the group specified by \p loc_id
 *          to the new group).
 *
 *          \p lcpl_id, \p gcpl_id, and \p gapl_id are property list
 *          identifiers. These property lists govern how the link to the
 *          group is created, how the group is created, and how the group
 *          can be accessed in the future, respectively. #H5P_DEFAULT can
 *          be passed in if the default properties are appropriate for
 *          these property lists. Currently, there are no APIs for the
 *          group access property list; use #H5P_DEFAULT.
 *
 *          The group identifier should be closed by H5Gclose() when access
 *          is no longer required to prevent resource leaks.
 *
 * \since 1.8.0
 *
 * \see H5Gopen2(), H5Gclose()
 *
 */
H5_DLL hid_t H5Gcreate2(hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Asynchronous version of H5Gcreate2()
 *
 * \app_file
 * \app_func
 * \app_line
 * \fgdta_loc_id
 * \param[in] name      Name of the group to create
 * \lcpl_id
 * \gcpl_id
 * \gapl_id
 * \es_id
 *
 * \return \hid_t{group}
 *
 * \see H5Gcreate2()
 *
 */
H5_DLL hid_t H5Gcreate_async(const char *app_file, const char *app_func, unsigned app_line, hid_t loc_id,
                             const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id, hid_t es_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Creates a new empty group without linking it into the file structure
 *
 * \fgdta_loc_id
 * \gcpl_id
 * \gapl_id
 *
 * \return \hid_t{group}
 *
 * \details H5Gcreate_anon() creates a new empty group in the file
 *          specified by \p loc_id. With default settings, H5Gcreate_anon()
 *          provides similar functionality to that provided by
 *          H5Gcreate1(), with the differences described in the list below.
 *
 *          The new group’s creation and access properties are specified
 *          in \p gcpl_id and \p gapl_id, respectively.
 *
 *          H5Gcreate_anon() returns a new group identifier. This identifier
 *          must be linked into the HDF5 file structure with H5Olink()
 *          or it will be deleted from the file when the file is closed.
 *
 *          The differences between this function and H5Gcreate1() are
 *          as follows:
 *
 *          \li H5Gcreate1() does not provide for the use of custom property
 *              lists; H5Gcreate1() always uses default properties.
 *          \li H5Gcreate_anon() neither provides the new group’s name
 *              nor links it into the HDF5 file structure; those actions
 *              must be performed separately through a call to H5Olink(),
 *              which offers greater control over linking.
 *          \li H5Gcreate_anon() does not directly provide a hint mechanism
 *              for the group’s heap size. Comparable information can be
 *              included in the group creation property list \p gcpl_id through
 *              a H5Pset_local_heap_size_hint() call.
 *
 *          A group created with this function should be closed with
 *          H5Gclose() when the group is no longer needed so that resource
 *          leaks will not develop.
 *
 * \see H5Olink(), H5Dcreate(), Using Identifiers
 *
 * \since 1.8.0
 *
 */
H5_DLL hid_t H5Gcreate_anon(hid_t loc_id, hid_t gcpl_id, hid_t gapl_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Opens an existing group in a file
 *
 * \fgdta_loc_id
 * \param[in] name      Name of the group to open
 * \gapl_id
 *
 * \return \hid_t{group}
 *
 * \details H5Gopen2() opens an existing group, \p name, at the location
 *          specified by \p loc_id.
 *
 *          With default settings, H5Gopen2() provides similar functionality
 *          to that provided by H5Gopen(). The only difference is that
 *          H5Gopen2() can provide a group access property list, \p gapl_id.
 *
 *          H5Gopen2() returns a group identifier for the group that was
 *          opened. This group identifier should be released by H5Gclose()
 *          when it is no longer needed to prevent resource leaks.
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gclose()
 *
 */
H5_DLL hid_t H5Gopen2(hid_t loc_id, const char *name, hid_t gapl_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Asynchronous version of H5Gopen2()
 *
 * \app_file
 * \app_func
 * \app_line
 * \fgdta_loc_id
 * \param[in] name      Name of the group to open
 * \gapl_id
 * \es_id
 *
 * \return \hid_t{group}
 *
 * \see H5Gopen2()
 *
 */
H5_DLL hid_t H5Gopen_async(const char *app_file, const char *app_func, unsigned app_line, hid_t loc_id,
                           const char *name, hid_t gapl_id, hid_t es_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Gets a group creation property list identifier
 *
 * \group_id
 *
 * \return \hid_t{creation property list}
 *
 * \details H5Gget_create_plist() returns an identifier for the group creation
 *          property list associated with the group specified by \p group_id.
 *
 *          The creation property list identifier should be released with
 *          H5Gclose() to prevent resource leaks.
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gclose()
 *
 */
H5_DLL hid_t H5Gget_create_plist(hid_t group_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Retrieves information about a group
 *
 * \fgdta_loc_id
 * \param[out] ginfo Struct in which group information is returned
 *
 * \return \hid_t{group}
 *
 * \details H5Gget_info() retrieves information about the group at location
 *          specified by \p loc_id. The information is returned in the \p ginfo.
 *
 *          \p ginfo is an H5G_info_t struct and is defined (in H5Gpublic.h)
 *          as follows:
 *
 * \snippet this H5G_info_t_snip
 * Possible values of \p storage_type are:
 * \storage_type
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gclose()
 *
 */
H5_DLL herr_t H5Gget_info(hid_t loc_id, H5G_info_t *ginfo);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Asynchronous version of H5Gget_info()
 *
 * \app_file
 * \app_func
 * \app_line
 * \fgdta_loc_id
 * \param[out] ginfo Struct in which group information is returned
 * \es_id
 *
 * \return \hid_t{group}
 *
 * \see H5Gget_info()
 *
 */
H5_DLL herr_t H5Gget_info_async(const char *app_file, const char *app_func, unsigned app_line, hid_t loc_id,
                                H5G_info_t *ginfo /*out*/, hid_t es_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Retrieves information about a group by its name
 *
 * \fgdta_loc_id
 * \param[in] name      Name of the group to query
 * \param[out] ginfo Struct in which group information is returned
 * \lapl_id
 *
 * \return \herr_t
 *
 * \details H5Gget_info_by_name() retrieves information about the group \p name
 *          at location specified by \p loc_id. The information is returned in
 *          the \p ginfo struct.
 *
 *          If \p loc_id specifies the group for which information is queried,
 *          then the group's \p name can be a dot (.).
 *
 *          \p ginfo is an H5G_info_t struct and is defined (in H5Gpublic.h)
 *          as follows:
 *
 * \snippet this H5G_info_t_snip
 * Possible values of \p storage_type are:
 * \storage_type
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gclose()
 *
 */
H5_DLL herr_t H5Gget_info_by_name(hid_t loc_id, const char *name, H5G_info_t *ginfo, hid_t lapl_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Asynchronous version of H5Gget_info_by_name()
 *
 * \app_file
 * \app_func
 * \app_line
 * \fgdta_loc_id
 * \param[in] name      Name of the group to query
 * \param[out] ginfo Struct in which group information is returned
 * \lapl_id
 * \es_id
 *
 * \return \herr_t
 *
 * \see H5Gget_info_by_name()
 *
 */
H5_DLL herr_t H5Gget_info_by_name_async(const char *app_file, const char *app_func, unsigned app_line,
                                        hid_t loc_id, const char *name, H5G_info_t *ginfo /*out*/,
                                        hid_t lapl_id, hid_t es_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Retrieves information about a group, according to the group’s
 *        position within an index
 *
 * \fgdta_loc_id
 * \param[in] group_name Name of the group to query
 * \param[in] idx_type   Transient index identifying object
 * \param[in] order      Transient index identifying object
 * \param[in] n          Position in the index of the group to query
 * \param[out] ginfo     Struct in which group information is returned
 * \lapl_id
 *
 * \return Returns
 *      \li The size of the object name if successful, or
 *      \li 0 if no name is associated with the group identifier, or
 *      \li negative value, if failure occurred
 *
 * \details H5Gget_info_by_idx() retrieves the same information
 *          about a group as retrieved by the function H5Gget_info(),
 *          but the means of identifying the group differs; the group is
 *          identified by position in an index rather than by name.
 *
 *          \p loc_id and \p group_name specify the group containing
 *          the group for which information is sought. The groups in \p
 *          group_name are indexed by \p idx_type; the group for which
 *          information is retrieved is identified in that index by index
 *          order, \p order, and index position, \p n.
 *
 *          If \p loc_id specifies the group containing the group for
 *          which information is queried, \p group_name can be a dot (.).
 *
 *          Valid values for \p index_type are as follows:
 * \indexes
 *          The order in which the index is to be examined, as specified
 *          by \p order, can be one of the following:
 * \orders
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gclose()
 *
 */
H5_DLL herr_t H5Gget_info_by_idx(hid_t loc_id, const char *group_name, H5_index_t idx_type,
                                 H5_iter_order_t order, hsize_t n, H5G_info_t *ginfo, hid_t lapl_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Asynchronous version of H5Gcreate2()
 *
 * \app_file
 * \app_func
 * \app_line
 * \fgdta_loc_id
 * \param[in] group_name Name of the group to query
 * \param[in] idx_type   Transient index identifying object
 * \param[in] order      Transient index identifying object
 * \param[in] n          Position in the index of the group to query
 * \param[out] ginfo     Struct in which group information is returned
 * \lapl_id
 * \es_id
 *
 * \return Returns
 *      \li The size of the object name if successful, or
 *      \li 0 if no name is associated with the group identifier, or
 *      \li negative value, if failure occurred
 *
 * \see H5Gcreate2()
 *
 */
H5_DLL herr_t H5Gget_info_by_idx_async(const char *app_file, const char *app_func, unsigned app_line,
                                       hid_t loc_id, const char *group_name, H5_index_t idx_type,
                                       H5_iter_order_t order, hsize_t n, H5G_info_t *ginfo /*out*/,
                                       hid_t lapl_id, hid_t es_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Flushes all buffers associated with a group to disk
 *
 * \group_id
 *
 * \return \herr_t
 *
 * \details H5Gflush() causes all buffers associated with a group to be
 *          immediately flushed to disk without removing the data from
 *          the cache.
 *
 * \attention
 *          HDF5 does not possess full control over buffering. H5G_FLUSH
 *          flushes the internal HDF5 buffers and then asks the operating
 *          system (the OS) to flush the system buffers for the open
 *          files. After that, the OS is responsible for ensuring that
 *          the data is actually flushed to disk.
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gclose()
 *
 */
H5_DLL herr_t H5Gflush(hid_t group_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Refreshes all buffers associated with a group
 *
 * \group_id
 *
 * \return \herr_t
 *
 * \details H5Grefresh() causes all buffers associated with a group to be
 *          cleared and immediately re-loaded with updated contents from disk.
 *
 *          This function essentially closes the group, evicts all
 *          metadata associated with it from the cache, and then re-opens
 *          the group. The reopened group is automatically re-registered
 *          with the same identifier.
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gclose()
 *
 */
H5_DLL herr_t H5Grefresh(hid_t group_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Closes the specified group
 *
 * \group_id
 *
 * \return \herr_t
 *
 * \details H5Gclose() releases resources used by a group which was
 *          opened by H5Gcreate() or H5Gopen().  After closing a group,
 *          \p group_id cannot be used again until another H5Gcreate()
 *          or H5Gopen() is called on it.
 *
 *          Failure to release a group with this call will result in
 *          resource leaks.
 *
 * \since 1.0.0
 *
 * \version 1.4.0 Fortran function introduced in this release
 *
 */
H5_DLL herr_t H5Gclose(hid_t group_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Asynchronous version of H5Gcreate2()
 *
 * \app_file
 * \app_func
 * \app_line
 * \group_id
 * \es_id
 *
 * \return \herr_t
 *
 * \see H5Gcreate2()
 *
 */
H5_DLL herr_t H5Gclose_async(const char *app_file, const char *app_func, unsigned app_line, hid_t group_id,
                             hid_t es_id);

/* API Wrappers for async routines */
/* (Must be defined _after_ the function prototype) */
/* (And must only defined when included in application code, not the library) */
#ifndef H5G_MODULE
#define H5Gcreate_async(...)           H5Gcreate_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Gopen_async(...)             H5Gopen_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Gget_info_async(...)         H5Gget_info_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Gget_info_by_name_async(...) H5Gget_info_by_name_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Gget_info_by_idx_async(...)  H5Gget_info_by_idx_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Gclose_async(...)            H5Gclose_async(__FILE__, __func__, __LINE__, __VA_ARGS__)

/* Define "wrapper" versions of function calls, to allow compile-time values to
 *      be passed in by language wrapper or library layer on top of HDF5.
 */
#define H5Gcreate_async_wrap           H5_NO_EXPAND(H5Gcreate_async)
#define H5Gopen_async_wrap             H5_NO_EXPAND(H5Gopen_async)
#define H5Gget_info_async_wrap         H5_NO_EXPAND(H5Gget_info_async)
#define H5Gget_info_by_name_async_wrap H5_NO_EXPAND(H5Gget_info_by_name_async)
#define H5Gget_info_by_idx_async_wrap  H5_NO_EXPAND(H5Gget_info_by_idx_async)
#define H5Gclose_async_wrap            H5_NO_EXPAND(H5Gclose_async)
#endif /* H5G_MODULE */

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */

/* Link definitions */
#define H5G_SAME_LOC   H5L_SAME_LOC
#define H5G_LINK_ERROR H5L_TYPE_ERROR
#define H5G_LINK_HARD  H5L_TYPE_HARD
#define H5G_LINK_SOFT  H5L_TYPE_SOFT
#define H5G_link_t     H5L_type_t

/* Macros for types of objects in a group (see H5G_obj_t definition) */
#define H5G_NTYPES      256 /* Max possible number of types	*/
#define H5G_NLIBTYPES   8   /* Number of internal types	*/
#define H5G_NUSERTYPES  (H5G_NTYPES - H5G_NLIBTYPES)
#define H5G_USERTYPE(X) (8 + (X)) /* User defined types		*/

/* Typedefs */

/*
 * An object has a certain type. The first few numbers are reserved for use
 * internally by HDF5. Users may add their own types with higher values.  The
 * values are never stored in the file -- they only exist while an
 * application is running.  An object may satisfy the `isa' function for more
 * than one type.
 */
typedef enum H5G_obj_t {
    H5G_UNKNOWN = -1, /* Unknown object type		*/
    H5G_GROUP,        /* Object is a group		*/
    H5G_DATASET,      /* Object is a dataset		*/
    H5G_TYPE,         /* Object is a named data type	*/
    H5G_LINK,         /* Object is a symbolic link	*/
    H5G_UDLINK,       /* Object is a user-defined link */
    H5G_RESERVED_5,   /* Reserved for future use	*/
    H5G_RESERVED_6,   /* Reserved for future use	*/
    H5G_RESERVED_7    /* Reserved for future use	*/
} H5G_obj_t;

/** Define the operator function pointer for for H5Giterate() */
//! [H5G_iterate_t_snip]
typedef herr_t (*H5G_iterate_t)(hid_t group, const char *name, void *op_data);
//! [H5G_iterate_t_snip]

/** Information about an object */
//! [H5G_stat_t_snip]
typedef struct H5G_stat_t {
    unsigned long fileno[2]; /*file number			*/
    unsigned long objno[2];  /*object number			*/
    unsigned      nlink;     /*number of hard links to object*/
    H5G_obj_t     type;      /*basic object type		*/
    time_t        mtime;     /*modification time		*/
    size_t        linklen;   /*symbolic link value length	*/
    H5O_stat_t    ohdr;      /* Object header information    */
} H5G_stat_t;
//! [H5G_stat_t_snip]

/* Function prototypes */
H5_DLL hid_t     H5Gcreate1(hid_t loc_id, const char *name, size_t size_hint);
H5_DLL hid_t     H5Gopen1(hid_t loc_id, const char *name);
H5_DLL herr_t    H5Glink(hid_t cur_loc_id, H5G_link_t type, const char *cur_name, const char *new_name);
H5_DLL herr_t    H5Glink2(hid_t cur_loc_id, const char *cur_name, H5G_link_t type, hid_t new_loc_id,
                          const char *new_name);
H5_DLL herr_t    H5Gmove(hid_t src_loc_id, const char *src_name, const char *dst_name);
H5_DLL herr_t    H5Gmove2(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, const char *dst_name);
H5_DLL herr_t    H5Gunlink(hid_t loc_id, const char *name);
H5_DLL herr_t    H5Gget_linkval(hid_t loc_id, const char *name, size_t size, char *buf /*out*/);
H5_DLL herr_t    H5Gset_comment(hid_t loc_id, const char *name, const char *comment);
H5_DLL int       H5Gget_comment(hid_t loc_id, const char *name, size_t bufsize, char *buf);
H5_DLL herr_t    H5Giterate(hid_t loc_id, const char *name, int *idx, H5G_iterate_t op, void *op_data);
H5_DLL herr_t    H5Gget_num_objs(hid_t loc_id, hsize_t *num_objs);
H5_DLL herr_t    H5Gget_objinfo(hid_t loc_id, const char *name, hbool_t follow_link,
                                H5G_stat_t *statbuf /*out*/);
H5_DLL ssize_t   H5Gget_objname_by_idx(hid_t loc_id, hsize_t idx, char *name, size_t size);
H5_DLL H5G_obj_t H5Gget_objtype_by_idx(hid_t loc_id, hsize_t idx);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* H5Gpublic_H */
