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

/**
 * \brief Information struct for group (for
 *        H5Gget_info/H5Gget_info_by_name/H5Gget_info_by_idx)
 */
//! [H5G_info_t_snip]
typedef struct H5G_info_t {
    H5G_storage_type_t 	storage_type;	/* Type of storage for links in group */
    hsize_t 	nlinks;		            /* Number of links in group */
    int64_t     max_corder; /* Current max. creation order value for group */
    hbool_t     mounted;    /* Whether group has a file mounted on it */
} H5G_info_t;
//!< [H5G_info_t_snip]


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
H5_DLL hid_t H5Gcreate2(hid_t loc_id, const char *name, hid_t lcpl_id,
    hid_t gcpl_id, hid_t gapl_id);

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
 * \brief Retrieves comment from an object
 *
 * \fgdt_loc_id
 * \param[in] name    Name of the object whose comment is to be retrieved
 * \param[in] bufsize Size of \p buf
 * \param[out] buf    Buffer for the comment
 *
 * \return Returns the number of characters in the comment, if successful;
 *         otherwise, returns a negative value.
 *
 * \details H5Gget_comment() retrieves the comment for the the object
 *          specified by \p loc_id and name. The comment is returned in the
 *          buffer \p comment.
 *
 *          \p loc_id can specify any object in the file. \p name can be
 *          one of the following:
 *          \li The name of the object relative to loc_id 
 *          \li An absolute name of the object, starting from /,
 *              the file’s root group
 *          \li A dot (.), if loc_id fully specifies the object
 *
 *          H5Gget_comment() returns the number of characters in
 *          the comment, counting the null terminator, if successful.
 *          The value returned may be larger than \p bufsize.
 *
 *          At most bufsize characters, including a null terminator, are
 *          returned in comment. The returned value is not null terminated
 *          if the comment is longer than the supplied buffer. If the
 *          size of the comment is unknown, a preliminary H5Gget_comment()
 *          call will return the size of the comment, including space for
 *          the null terminator.
 *
 *          If an object does not have a comment, an empty string is
 *          returned in comment.
 *
 * \version 1.8.0 The function H5Gget_comment() was deprecated in this
 *                release, in favor of H5Oget_comment().
 * \since 1.8.0
 *
 */
H5_DLL int H5Gget_comment(hid_t loc_id, const char *name, size_t bufsize,
    char *buf);

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
 *          <table>
 *          <tr><td>\c H5G_STORAGE_TYPE_COMPACT</td>
 *              <td>Compact storage</td></tr>
 *          <tr><td>\c H5G_STORAGE_TYPE_DENSE</td>
 *              <td>Indexed storage</td></tr>
 *          <tr><td>\c H5G_STORAGE_TYPE_SYMBOL_TABLE</td>
 *              <td>Symbol tables, the original HDF5 structure</td></tr>
 *          </table>
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gclose()
 *
 */
H5_DLL herr_t H5Gget_info(hid_t loc_id, H5G_info_t *ginfo);

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
 *          Valid values for index_type are as follows:
 *
 *          <table>
 *          <tr><td>\c H5_INDEX_NAME</td>
 *              <td>An alpha-numeric index by group name </td></tr>
 *          <tr><td>\c H5_INDEX_CRT_ORDER</td>
 *              <td>An index by creation order </td></tr>
 *          </table>
 *
 *          The order in which the index is to be examined, as specified
 *          by order, can be one of the following:
 *
 *          <table>
 *          <tr><td>\c H5_ITER_INC</td>
 *              <td>The count is from beginning of the index, i.e., top-down.
 *                  </td></tr>
 *          <tr><td>\c H5_ITER_DEC</td>
 *              <td>The count is from the end of the index, i.e., bottom-up.
 *                  </td></tr>
 *          <tr><td>\c ndim</td>
 *              <td>HDF5 counts through the index in the fastest-available
 *                  order. No information is provided as to the order, but
 *                  HDF5 ensures that no element in the index will be
 *                  overlooked.</td></tr>
 *          </table>
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gclose()
 *
 */
H5_DLL herr_t H5Gget_info_by_idx(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5G_info_t *ginfo,
    hid_t lapl_id);

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
 *          <table>
 *          <tr><td>\c H5G_STORAGE_TYPE_COMPACT</td>
 *              <td>Compact storage</td></tr>
 *          <tr><td>\c H5G_STORAGE_TYPE_DENSE</td>
 *              <td>Indexed storage</td></tr>
 *          <tr><td>\c H5G_STORAGE_TYPE_SYMBOL_TABLE</td>
 *              <td>Symbol tables, the original HDF5 structure</td></tr>
 *          </table>
 *
 * \since 1.8.0
 *
 * \see H5Gcreate2(), H5Gclose()
 *
 */
H5_DLL herr_t H5Gget_info_by_name(hid_t loc_id, const char *name,
    H5G_info_t *ginfo, hid_t lapl_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Retrieves the name of the object that the symbolic link points to
 *
 * \loc_id
 * \param[in] name Symbolic link to the object whose name is to be returned
 * \param[in] size Size of \p buf
 * \param[out] buf Buffer to hold the name of the object being sought
 *
 * \return Returns a non-negative value if successful. Otherwise, returns
 *         a negative value.
 *
 * \details H5Gget_linkval() returns size characters of the
 *          name of the object that the symbolic link \p name points to.
 *
 *          The parameter \p loc_id is a file or group identifier.
 *
 *          The parameter \p name must be a symbolic link pointing to the
 *          desired object and must be defined relative to \p loc_id.
 *
 *          If \p size is smaller than the size of the returned object
 *          name, then the name stored in the buffer value will not be
 *          null terminated.
 *
 *          This function fails if \p name is not a symbolic link. The
 *          presence of a symbolic link can be tested by passing zero for
 *          \p size and NULL for \p value.
 *
 *          This function should be used only after H5Lget_info1() (or
 *          the deprecated function H5Gget_objinfo()) has been called to
 *          verify that \p name is a symbolic link.
 *
 * \version 1.8.0 The function H5Gget_linkval() was deprecated in this
 *                release, in favor of H5Lget_val().
 * \since?
 *
 */
H5_DLL herr_t H5Gget_linkval(hid_t loc_id, const char *name, size_t size,
    char *buf/*out*/);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Returns number of objects in the group
 *
 * \loc_id
 * \param[out] num_objs Number of objects in the group
 *
 * \return \herr_t
 *
 * \details H5Gget_num_objs() returns number of objects in
 *          a group specified by its identifier \p loc_id. If a file
 *          identifier is passed in, then the number of objects in the
 *          root group is returned.
 *
 * \version 1.8.0 The function H5Gget_num_objs() was deprecated in this
 *                release, in favor of H5Gget_info().
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Gget_num_objs(hid_t loc_id, hsize_t *num_objs);

TOPP

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

/** Define the operator function pointer for for H5Giterate() */
//! [H5G_iterate_t_snip]
typedef herr_t (*H5G_iterate_t)(hid_t group, const char *name, void *op_data);
//!< [H5G_iterate_t_snip]

/** Information about an object */
//! [H5G_stat_t_snip]
typedef struct H5G_stat_t {
    unsigned long 	fileno[2];	/*file number			*/
    unsigned long 	objno[2];	/*object number			*/
    unsigned 		nlink;		/*number of hard links to object*/
    H5G_obj_t 		type;		/*basic object type		*/
    time_t		mtime;		/*modification time		*/
    size_t		linklen;	/*symbolic link value length	*/
    H5O_stat_t          ohdr;           /* Object header information    */
} H5G_stat_t;
//!< [H5G_stat_t_snip]

/* Function prototypes */

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Creates a new empty group and links it to a location in the file
 *
 * \fgdta_loc_id
 * \param[in] name Absolute or relative name of the group to create
 * \param[in] size_hint Optional parameter indicating the number of bytes
 *                      to reserve for the name of the new group
 *
 * \return \hid_t{group}
 *
 * \deprecated As of HDF5-1.8.0, this function has been deprecated in favor
 *             of the function H5Gcreate2() and the macro H5Gcreate().
 *
 * \details H5Gcreate1() creates a new group with the specified name at
 *          the specified location, \p loc_id.
 *
 *          \p loc_id may be a file, group, dataset, named datatype or
 *          attribute.  If an attribute, dataset, or named datatype is
 *          specified for loc_id then the group will be created at the
 *          location where the attribute, dataset, or named datatype is
 *          attached. The name, name, must not already be taken by some
 *          other object and all parent groups must already exist.
 *
 *          \p name can be a relative path based at \p loc_id or an
 *          absolute path from the root of the file. Use of this function
 *          requires that any intermediate groups specified in the path
 *          already exist.
 *
 *          The length of a group name, or of the name of any object within
 *          a group, is not limited.
 *
 *          \p size_hint is a hint for the number of bytes to reserve to
 *          store the names which will be eventually added to the new
 *          group. Passing a value of zero for \p size_hint is usually
 *          adequate since the library is able to dynamically resize
 *          the name heap, but a correct hint may result in better
 *          performance.  A conservative estimate could result in multiple
 *          system-level I/O requests to read the group name heap; a liberal
 *          estimate could result in a single large I/O request even when
 *          the group has just a few names. HDF5 stores each name with a
 *          null terminator.  If a non-positive value is supplied for \p
 *          size_hint, then a default size is chosen.
 *
 *          The return value is a group identifier for the open group. This
 *          group identifier should be closed by calling H5Gclose() when
 *          it is no longer needed.
 *
 *          See H5Gcreate_anon() for a discussion of the differences
 *          between H5Gcreate1 and H5Gcreate_anon().
 *
 *
 * \version 1.8.0 Function was renamed to H5Gcreate1() and deprecated.
 * \version 1.4.0 Fortran function introduced in this release
 *
 * \since 1.0.0
 *
 */
H5_DLL hid_t H5Gcreate1(hid_t loc_id, const char *name, size_t size_hint);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Opens an existing group for modification
 *
 * \loc_id
 * \param[in] name Name of the group to open
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.8.0, this function has been deprecated in favor
 *             of the function H5Gopen2() and the macro H5Gopen().
 *
 * \details H5Gopen1() opens an existing group with the specified \p name
 *          at the specified location, \p loc_id.
 *
 *          H5Gopen1()  returns a group identifier for the group that was
 *          opened. This group identifier should be released by calling
 *          H5Gclose() when it is no longer needed.
 *
 * \version 1.8.0 Function was renamed to H5Gopen1() and deprecated.
 * \version 1.4.0 Fortran function introduced in this release
 *
 * \since 1.0.0
 *
 */
H5_DLL hid_t H5Gopen1(hid_t loc_id, const char *name);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Creates a link between two existing objects
 *
 * \param[in] cur_loc_id File or group identifier
 * \param[in] type       Link type
 * \param[in] cur_name   Current name
 * \param[in] new_name   New name to link to
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.8.0, this function has been deprecated in favor
 *             of the functions H5Lcreate_hard() and H5Lcreate_soft().
 *
 * \details H5Glink() creates a new name for an object that has some current
 *          name, possibly one of many names it currently has.
 *
 *          If link_type is #H5G_LINK_HARD, then current_name must specify
 *          the name of an existing object and both names are interpreted
 *          relative to \p cur_loc_id, which is either a file identifier or
 *          a group identifier.
 *
 *          If \p link_type is #H5G_LINK_SOFT, then \p current_name can
 *          be anything and is interpreted at lookup time relative to the
 *          group which contains the final component of \p new_name. For
 *          instance, if \p current_name is <tt>./foo</tt>, \p new_name is
 *          <tt>./x/y/bar</tt>, and a request is made for <tt>./x/y/bar</tt>,
 *          then the actual object looked up is <tt>./x/y/./foo</tt>.
 *
 * \version 1.8.0 The function H5Glink() was deprecated in this release
 *
 * \since?
 *
 */
H5_DLL herr_t H5Glink(hid_t cur_loc_id, H5G_link_t type, const char *cur_name,
    const char *new_name);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Creates a link between two existing objects
 *
 * \param[in] cur_loc_id The file or group identifier for the original object
 * \param[in] cur_name Current name
 * \param[in] type     Link type
 * \param[in] new_loc_id The file or group identifier for the new link
 * \param[in] new_name New name
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.8.0, this function has been deprecated in favor
 *             of the functions H5Lcreate_hard() and H5Lcreate_soft().
 *
 * \details H5Glink() creates a new name for an object that has some current
 *          name, possibly one of many names it currently has.
 *
 *          If \p link_type is H5G_LINK_HARD, then current_name must specify
 *          the name of an existing object. In this case, \p current_name
 *          and \p new_name are interpreted relative to \p curr_loc_id
 *          and \p new_loc_id, respectively, which are either file or
 *          group identifiers.
 *
 *          If \p link_type is #H5G_LINK_SOFT, then \p current_name can
 *          be anything and is interpreted at lookup time relative to the
 *          group which contains the final component of \p new_name. For
 *          instance, if \p current_name is <tt>./foo</tt>, \p new_name is
 *          <tt>./x/y/bar</tt>, and a request is made for <tt>./x/y/bar</tt>,
 *          then the actual object looked up is <tt>./x/y/./foo</tt>.
 *
 * \version 1.8.0 The function H5Glink() was deprecated in this release
 *
 * \since?
 *
 */
H5_DLL herr_t H5Glink2(hid_t cur_loc_id, const char *cur_name, H5G_link_t type,
    hid_t new_loc_id, const char *new_name);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Renames an object within an HDF5 file
 *
 * \param[in] src_loc_id The file or group identifier for the original object
 * \param[in] src_name   Object's original name
 * \param[in] dst_name   Object's new name
 *
 * \return \herr_t
 *
 * \details H5Gmove() renames an object within an HDF5 file. The
 *          original name, \p src_name, is unlinked from the group graph
 *          and the new name, \p dst_name, is inserted as an atomic
 *          operation. Both names are interpreted relative to \p loc_id,
 *          which is either a file or a group identifier.
 *
 * \attention
 *          Exercise care in moving groups as it is possible to render
 *          data in a file inaccessible with H5Gmove(). See The Group
 *          Interface in the HDF5 User's Guide.
 *
 * \version 1.8.0 The function H5Gmove() was deprecated in this release,
 *                in favor of H5Lmove().
 * \since?
 *
 */
H5_DLL herr_t H5Gmove(hid_t src_loc_id, const char *src_name,
    const char *dst_name);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Renames an object within an HDF5 file
 *
 * \param[in] src_loc_id Original file or group identifier
 * \param[in] src_name   Object's original name
 * \param[in] dst_loc_id Destination file or group identifier
 * \param[in] dst_name   Object's new name
 *
 * \return \herr_t
 *
 * \details H5Gmove2() renames an object within an HDF5
 *          file. The original name, \p src_name, is unlinked from the
 *          group graph and the new name, \p dst_name, is inserted as an
 *          atomic operation.
 *
 *          \p src_name and \p dst_name are interpreted relative to \p
 *          src_loc_id and \p dst_loc_id, respectively, which are either
 *          file or group identifiers.
 *
 * \attention
 *          Exercise care in moving groups as it is possible to render
 *          data in a file inaccessible with H5G_MOVE2. See The Group
 *          Interface in the HDF5 User's Guide.
 *
 * \version 1.8.0 The function H5Gmove2() was deprecated in this release,
 *                in favor of H5Lmove().
 * \since?
 *
 */
H5_DLL herr_t H5Gmove2(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
    const char *dst_name);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Removes the link to an object from a group
 *
 * \loc_id
 * \param[in] name Name of the object to unlink
 *
 * \return \herr_t
 *
 * \version 1.8.0 The function H5Gunlink() was deprecated in this release,
 *                in favor of H5Ldelete().
 *
 * \details H5Gunlink() removes the object specified by \p name
 *          from the group graph and decrements the link count for the
 *          object to which name points. This action eliminates any
 *          association between name and the object to which name pointed.
 *
 *          Object headers keep track of how many hard links refer to an
 *          object; when the link count reaches zero, the object can be
 *          removed from the file. Objects which are open are not removed
 *          until all identifiers to the object are closed.
 *
 *          If the link count reaches zero, all file space associated
 *          with the object will be released, i.e., identified in memory
 *          as freespace. If any object identifier is open for the object,
 *          the space will not be released until after the object identifier
 *          is closed.
 *
 *          Note that space identified as freespace is available for
 *          re-use only as long as the file remains open; once a file has
 *          been closed, the HDF5 library loses track of freespace. See
 *          “Freespace Management” in the HDF5 User's Guide for
 *          further details.
 *
 * \attention
 *          Exercise care in unlinking groups as it is possible to render
 *          data in a file inaccessible with H5Gunlink(). See The Group
 *          Interface in the HDF5 User's Guide.
 *
 * \version 1.8.0 The function H5Gunlink() was deprecated in this release,
 *                in favor of H5Ldelete().
 * \since?
 *
 * \see H5Gcreate2(), H5Gopen2()
 *
 */
H5_DLL herr_t H5Gunlink(hid_t loc_id, const char *name);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Sets comment for specified object
 *
 * \fgdt_loc_id
 * \param[in] name Name of the object whose comment is to be set or reset
 * \param[in] comment The new comment
 *
 * \return \herr_t
 *
 * \details H5Gset_comment() sets the comment for the object
 *          specified by \p loc_id and \p name to \p comment. Any previously
 *          existing comment will be overwritten.
 *
 *          \p loc_id can specify any object in the file. \p name can be
 *          one of the following:
 *          \li The name of the object relative to loc_id 
 *          \li An absolute name of the object, starting from /,
 *              the file’s root group
 *          \li A dot (.), if loc_id fully specifies the object
 *
 *          If comment is the empty string or a null pointer, the comment
 *          message will be removed from the object.  Comments should be
 *          relatively short, null-terminated, ASCII strings.
 *
 *          Comments can be attached to any object that has an object
 *          header, e.g., datasets, groups, and named datatypes, but not
 *          symbolic links.
 *
 * \version 1.8.0 The function H5Gset_comment() was deprecated in this
 *                release, in favor of H5Oset_comment().
 * \since?
 *
 */
H5_DLL herr_t H5Gset_comment(hid_t loc_id, const char *name, const char *comment);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Iterates an operation over the entries of a group
 *
 * \fgdt_loc_id
 * \param[in]     name Group over which the iteration is performed
 * \param[in,out] idx  Location at which to begin the iteration
 * \param[in]     op   Operation to be performed on an object at each iteration
 * \param[in,out] op_data Data associated with the operation
 *
 * \return Returns the return value of the last operator or zero if all group
 *         members were processed. Otherwise returns a negative value when
 *         failure occurs.
 *
 * \details H5Giterate() iterates over the members of the group \p name in
 *          the file or group specified with \p loc_id.
 *          
 *          For each object in the group, the \p op_data and some
 *          additional information, specified below, are passed to the \p op
 *          function. The iteration begins with the \p idx'th object in the
 *          group and the next element to be processed by the operator is
 *          returned in \p idx. If \p idx is NULL, then the iterator starts
 *          at the first group member; since no stopping point is returned
 *          in this case, the iterator cannot be restarted if one of the
 *          calls to its operator returns non-zero. H5Giterate() does not
 *          recursively follow links into subgroups of the specified group.
 *          
 *          The prototype for H5G_iterate_t is:
 *          
 *          \snippet this H5G_iterate_t_snip
 *          
 *          The operator function receives the group identifier for the
 *          group being iterated over, \p loc_id, the name of the current
 *          object within the group, \p name, and the pointer to the
 *          operator data passed in to H5Giterate(), \p op_data.
 *          
 *          The return values from an operator are:
 *          
 *          \li Zero causes the iterator to continue, returning zero when
 *              all group members have been processed.
 *          \li Positive causes the iterator to immediately return that
 *              positive value, indicating short-circuit success. The iterator
 *              can be restarted at the next group member.
 *          \li Negative causes the iterator to immediately return that value,
 *              indicating failure. The iterator can be restarted at the next
 *              group member.
 *
 *          H5Giterate() assumes that the membership of the group identified
 *          by \p name remains unchanged throughout the iteration. If
 *          the membership changes during the iteration, the function's
 *          behavior is undefined.
 *
 *          H5Giterate() is not recursive. For example, if a member
 *          of \p name is found to be a group, call it \p subgroup_a,
 *          H5Giterate() does not examine the members of \p subgroup_a. When
 *          recursive iteration is required, the application must handle
 *          the recursion, explicitly calling H5Giterate() on discovered
 *          subgroups.
 *
 * \version 1.8.0 The function H5Giterate() was deprecated in this
 *                release, in favor of H5Literate1().
 * \since?
 *
 */
H5_DLL herr_t H5Giterate(hid_t loc_id, const char *name, int *idx,
        H5G_iterate_t op, void *op_data);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Returns information about an object
 *
 * \loc_id
 * \param[in] name Name of the object for which status is being sought
 * \param[in] follow_link Link flag
 * \param[out] statbuf Buffer in which to return information about the object
 *
 * \return Returns a non-negative value if successful, with the fields in
 *         \p statbuf (if non-null) assigned with values. Otherwise, returns
 *         a negative value.
 *
 * \deprecated As of HDF5-1.8.0, this function has been deprecated in favor
 *             of the function H5Oget_info() and H5Lget_info1().
 *
 * \details H5Gget_objinfo() returns information about the specified object
 *          through \p statbuf.
 *
 *          A file or group identifier, \p loc_id, and an object name,
 *          \p name, relative to \p loc_id, are commonly used to specify
 *          the object. However, if the object identifier is already
 *          known to the application, an alternative approach is to use
 *          that identifier, \p obj_id, in place of \p loc_id, and a dot
 *          (.) in place of \p name. Thus, the alternative versions of the
 *          first portion of an H5Gget_objinfo() call would be as follows:
 *
 *          H5Gget_objinfo(loc_id, name, ...) 
 *          H5Gget_objinfo(obj_id, ".", ...)
 *
 *          If the object is a symbolic link and \p follow_link is zero
 *          (0), then the information returned describes the link itself;
 *          otherwise the link is followed and the information returned
 *          describes the object to which the link points. If \p follow_link
 *          is non-zero but the final symbolic link is dangling (does not
 *          point to anything), then an error will be returned. The fields
 *          in \p statbuf are undefined for an error. The existence of
 *          an object can be tested by calling this function with a null
 *          \p statbuf.
 *
 *          H5Gget_objinfo() fills in the following data structure
 *          (defined in H5Gpublic.h):
 *
 *          \snippet this H5G_stat_t_snip
 *
 *          where H5O_stat_t (defined in H5Opublic.h) is:
 *
 *          \snippet H5Opublic.h H5O_stat_t_snip
 *
 *          The fields \p fileno and \p objno contain four values which
 *          uniquely identify an object among those HDF5 files which are
 *          open: if all four values are the same between two objects,
 *          then the two objects are the same (provided both files are
 *          still open).
 *
 * \note
 *          \li If a file is closed and re-opened, the value in \p fileno
 *              will change.
 *          \li If a VFL driver either does not or cannot detect that two
 *              H5Fopen() calls referencing the same file actually open the
 *              same file, each will get a different \p fileno.
 * 
 *          The field \p nlink is the number of hard links to the object or
 *          zero when information is being returned about a symbolic link
 *          (symbolic links do not have hard links but all other objects
 *          always have at least one).
 *          
 *          The field \p type contains the type of the object, either of the
 *          following: #H5G_GROUP, #H5G_DATASET, #H5G_LINK, or #H5G_TYPE.
 *          
 *          The field \p mtime contains the modification time.
 *          
 *          If information is being returned about a symbolic link then
 *          \p linklen will be the length of the link value (the name of
 *          the pointed-to object with the null terminator); otherwise \p
 *          linklen will be zero.
 *          
 *          The fields in the \c H5O_stat_t struct contain information
 *          about the object header for the object queried:
 *          
 *          size    The total size of all the object header information in
 *                  the file (for all chunks).
 *          free    The size of unused space in the object header.
 *          nmesgs  The number of object header messages.
 *          nchunks The number of chunks the object header is broken up into.
 *
 *          Other fields may be added to this structure in the future.
 *
 *          Some systems will be able to record the time accurately but
 *          unable to retrieve the correct time; such systems (e.g.,
 *          Irix64) will report an mtime value of 0 (zero).
 *
 * \version 1.8.0 The function H5Gget_objinfo was deprecated in this release.
 *
 * \version 1.6.1 Two new fields were added to the H5G_stat_t struct in this release.
 *
 */
H5_DLL herr_t H5Gget_objinfo(hid_t loc_id, const char *name,
    hbool_t follow_link, H5G_stat_t *statbuf/*out*/);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Returns a name of an object specified by an index (deprecated)
 *
 * \fgdta_loc_id
 * \param[in] idx      Transient index identifying object
 * \param[in,out] name  Buffer for name
 * \param[in] size      Name length
 *
 * \return Returns
 *      \li The size of the object name if successful, or
 *      \li 0 if no name is associated with the group identifier, or
 *      \li negative value, if failure occurred
 *
 * \details H5Gget_objname_by_idx() returns the name of the object specified by
 *          the index \p idx at the location specified by \p loc_id.
 *
 *          \p loc_id may be a group or a file, which is the file's root group.
 *
 *          \p idx is the transient index used to iterate through the
 *          objects in the group. The value of \p idx is any nonnegative
 *          number less than the total number of objects in the group,
 *          which is returned by the function H5Gget_num_objs(). Note
 *          that this is a transient index; an object may have a different
 *          index each time a group is opened.
 *
 *          The object name is returned in the user-specified buffer
 *          \p name.
 *
 *          If the size of the provided buffer \p name is less or equal
 *          the actual object name length, the object name is truncated
 *          to <tt>size - 1</tt> characters.
 *
 *          Note that if the size of the object's name is unkown, a
 *          preliminary call to H5Gget_objname_by_idx() with name set to
 *          NULL will return the length of the object's name. A second
 *          call to H5Gget_objname_by_idx() can then be used to retrieve
 *          the actual name.
 *
 * \since 1.8.0
 *
 * \version 1.8.0 The function deprecated in this release.
 *
 */
H5_DLL ssize_t H5Gget_objname_by_idx(hid_t loc_id, hsize_t idx, char* name,
    size_t size);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5G
 *
 * \brief Closes the specified group
 *
 * \fgdta_loc_id
 * \param[in] idx      Transient index identifying object
 *
 * \return \herr_t
 *
 * \details H5Gget_objtype_by_idx() returns the type of the
 *          object specified by the index \p idx in the group \p loc_id.
 *
 *          The group is specified by a group identifier \p loc_id. A
 *          file identifier may also be passed in for \p loc_id; which
 *          indicates the file's root group.
 *
 *          \p idx is the transient index used to iterate through the
 *          objects in the group. This parameter is described in more
 *          detail in the discussion of H5Gget_objname_by_idx.
 *
 *          The object type is returned as the function return value:
 *
 *          <table>
 *          <tr><td>\c H5G_GROUP</td>
 *              <td>0</td>
 *              <td>Object is a group</td></tr>
 *          <tr><td>\c H5G_DATASET</td>
 *              <td>1</td>
 *              <td>Object is a dataset</td></tr>
 *          <tr><td>\c H5G_TYPE</td>
 *              <td>2</td>
 *              <td>Object is a named datatype</td></tr>
 *          <tr><td>\c H5G_LINK</td>
 *              <td>3</td>
 *              <td>Object is a symbolic link</td></tr>
 *          <tr><td>\c H5G_UDLINK</td>
 *              <td>4</td>
 *              <td>Object is a user-defined link</td></tr>
 *          </table>
 *
 * \since 1.8.0
 *
 * \version 1.8.0 The function was deprecated in this release.
 *
 */
H5_DLL H5G_obj_t H5Gget_objtype_by_idx(hid_t loc_id, hsize_t idx);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* _H5Gpublic_H */

