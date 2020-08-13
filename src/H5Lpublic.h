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
#include "H5public.h"		/* Generic Functions			*/
#include "H5Ipublic.h"		/* IDs			  		*/
#include "H5Opublic.h"		/* Object Headers			*/
#include "H5Tpublic.h"		/* Datatypes				*/

/*****************/
/* Public Macros */
/*****************/

/** Maximum length of a link's name (encoded in a 32-bit unsigned integer) */
#define H5L_MAX_LINK_NAME_LEN   ((uint32_t)(-1))  /* (4GB - 1) */

/** Macro to indicate operation occurs on same location */
#define H5L_SAME_LOC (hid_t)0

/** Current version of the H5L_class_t struct */
#define H5L_LINK_CLASS_T_VERS       1

#ifdef __cplusplus
extern "C" {
#endif

/*******************/
/* Public Typedefs */
/*******************/

/** Link class types.
 * Values less than 64 are reserved for the HDF5 library's internal
 * use. Values 64 to 255 are for "user-defined" link class types;
 * these types are defined by HDF5 but their behavior can be overridden
 * by users. Users who want to create new classes of links should
 * contact the HDF5 development team at help@hdfgroup.org. These
 * values can never change because they appear in HDF5 files.
 */
typedef enum {
    H5L_TYPE_ERROR = (-1),      /**< Invalid link type id         */
    H5L_TYPE_HARD = 0,          /**< Hard link id                 */
    H5L_TYPE_SOFT = 1,          /**< Soft link id                 */
    H5L_TYPE_EXTERNAL = 64,     /**< External link id             */
    H5L_TYPE_MAX = 255	        /**< Maximum link type id         */
} H5L_type_t;

/** Maximum value link value for "built-in" link types */
#define H5L_TYPE_BUILTIN_MAX H5L_TYPE_SOFT
/** Link ids at or above this value are "user-defined" link types. */
#define H5L_TYPE_UD_MIN      H5L_TYPE_EXTERNAL

/** Information struct for link (for H5Lget_info2() / H5Lget_info_by_idx2())
 * H5O_token_t version used in VOL layer and future public API calls
 */
typedef struct {
    H5L_type_t          type;           /**< Type of link                   */
    hbool_t             corder_valid;   /**< Indicate if creation order is valid */
    int64_t             corder;         /**< Creation order                 */
    H5T_cset_t          cset;           /**< Character set of link name     */
    union {
        H5O_token_t     token;          /**< Token of location that hard link points to */
        size_t          val_size;       /**< Size of a soft link or UD link value */
    } u;
} H5L_info2_t;

/* The H5L_class_t struct can be used to override the behavior of a
 * "user-defined" link class. Users should populate the struct with callback
 * functions defined below.
 */
/* Callback prototypes for user-defined links */
/** Link creation callback */
typedef herr_t (*H5L_create_func_t)(const char *link_name, hid_t loc_group,
    const void *lnkdata, size_t lnkdata_size, hid_t lcpl_id);

/** Callback for when the link is moved */
typedef herr_t (*H5L_move_func_t)(const char *new_name, hid_t new_loc,
    const void *lnkdata, size_t lnkdata_size);

/** Callback for when the link is copied */
typedef herr_t (*H5L_copy_func_t)(const char *new_name, hid_t new_loc,
    const void *lnkdata, size_t lnkdata_size);

/** Callback during link traversal */
typedef hid_t (*H5L_traverse_func_t)(const char *link_name, hid_t cur_group,
    const void *lnkdata, size_t lnkdata_size, hid_t lapl_id, hid_t dxpl_id);

/** Callback for when the link is deleted */
typedef herr_t (*H5L_delete_func_t)(const char *link_name, hid_t file,
    const void *lnkdata, size_t lnkdata_size);

/** Callback for querying the link. Returns the size of the buffer needed */
typedef ssize_t (*H5L_query_func_t)(const char *link_name, const void *lnkdata,
    size_t lnkdata_size, void *buf /*out*/, size_t buf_size);

typedef struct {
    int version;                    /**< Version number of this struct        */
    H5L_type_t id;                  /**< Link type ID                         */
    const char *comment;            /**< Comment for debugging                */
    H5L_create_func_t create_func;  /**< Callback during link creation        */
    H5L_move_func_t move_func;      /**< Callback after moving link           */
    H5L_copy_func_t copy_func;      /**< Callback after copying link          */
    H5L_traverse_func_t trav_func;  /**< Callback during link traversal       */
    H5L_delete_func_t del_func;     /**< Callback for link deletion           */
    H5L_query_func_t query_func;    /**< Callback for queries                 */
} H5L_class_t;

/** Prototype for H5Literate2() / H5Literate_by_name2() operator
 * H5O_token_t version used in VOL layer and future public API calls
 */
typedef herr_t (*H5L_iterate2_t)(hid_t group, const char *name, const H5L_info2_t *info,
    void *op_data);

/** Callback for external link traversal */
typedef herr_t (*H5L_elink_traverse_t)(const char *parent_file_name,
    const char *parent_group_name, const char *child_file_name,
    const char *child_object_name, unsigned *acc_flags, hid_t fapl_id,
    void *op_data);


/********************/
/* Public Variables */
/********************/


/*********************/
/* Public Prototypes */
/*********************/

/**\ingroup H5L
 *
 * \brief Moves a link within an HDF5 file
 *
 * \fgdta_loc_id{src_loc}
 * \param[in] src_name Original link name
 * \fgdta_loc_id{dst_loc}
 * \param[in] dst_name New link name
 * \lcpl_id
 * \lapl_id
 *
 * \return \herr_t
 *
 * \todo We need to get the location ID story straight!
 *
 * \details H5Lmove() moves a link within an HDF5 file. The original link,
 *          \p src_name, is removed from \p src_loc and the new link,
 *          \p dst_name, is inserted at dst_loc. This change is
 *          accomplished as an atomic operation.
 *
 *          \p src_loc and \p src_name identify the original link.
 *          \p src_loc is the original location identifier; \p src_name is
 *          the path to the link and is interpreted relative to \p src_loc.
 *
 *          \p dst_loc and \p dst_name identify the new link. \p dst_loc is
 *          either a file or group identifier; \p dst_name is the path to
 *          the link and is interpreted relative to \p dst_loc.
 *
 *          \p lcpl_id and \p lapl_id are the link creation and link access
 *          property lists, respectively, associated with the new link,
 *          \p dst_name.
 *
 *          Through these property lists, several properties are available to
 *          govern the behavior of H5Lmove(). The property controlling creation
 *          of missing intermediate groups is set in the link creation property
 *          list with H5Pset_create_intermediate_group(); H5Lmove() ignores any
 *          other properties in the link creation property list. Properties
 *          controlling character encoding, link traversals, and external link
 *          prefixes are set in the link access property list with
 *          H5Pset_char_encoding(), H5Pset_nlinks(), and H5Pset_elink_prefix(),
 *          respectively.
 *
 * \note Note that H5Lmove() does not modify the value of the link; the new
 *       link points to the same object as the original link pointed to.
 *       Furthermore, if the object pointed to by the original link was already
 *       open with a valid object identifier, that identifier will remain valid
 *       after the call to H5Lmove().
 *
 * \attention Exercise care in moving links as it is possible to render data in
 *            a file inaccessible with H5L_MOVE. If the link being moved is on
 *            the only path leading to an HDF5 object, that object may become
 *            permanently inaccessible in the file.
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Lmove(hid_t src_loc, const char *src_name, hid_t dst_loc,
    const char *dst_name, hid_t lcpl_id, hid_t lapl_id);
/**\ingroup H5L
 *
 * \brief Creates an identical copy of a link with the same creation time and
 *        target.  The new link can have a different name and be in a different
 *        location than the original.
 *
 * \fgdt_loc_id{src_loc_id}
 * \param[in] src_name   Name of the link to be copied
 * \fgdt_loc_id{dst_loc_id}
 * \param[in] dst_name   Name to be assigned to the new copy
 * \lcpl_id
 * \lapl_id
 * \return \herr_t
 *
 * \details H5Lcopy() copies the link specified by \p src_name from the location
 *          specified by \p src_loc_id to the location specified by
 *          \p dst_loc_id. The new copy of the link is created with the name
 *          \p dst_name.
 *
 *          If \p dst_loc_id is a file identifier, \p dst_name will be
 *          interpreted relative to that file’s root group.
 *
 *          The new link is created with the creation and access property lists
 *          specified by \p lcpl_id and \p lapl_id. The interpretation of
 *          \p lcpl_id is limited in the manner described in the next paragraph.
 *
 *          H5Lcopy() retains the creation time and the target of the original
 *          link. However, since the link may be renamed, the character
 *          encoding is that specified in \p lcpl_id rather than that of the
 *          original link. Other link creation properties are ignored.
 *
 *          If the link is a soft link, also known as a symbolic link, its
 *          target is interpreted relative to the location of the copy.
 *
 *          Several properties are available to govern the behavior of
 *          H5Lcopy(). These properties are set in the link creation and access
 *          property lists, \p lcpl_id and \p lapl_id, respectively. The
 *          property controlling creation of missing intermediate groups is set
 *          in the link creation property list with
 *          H5Pset_create_intermediate_group(); this function ignores any
 *          other properties in the link creation property list. Properties
 *          controlling character encoding, link traversals, and external link
 *          prefixes are set in the link access property list with
 *          H5Pset_char_encoding(), H5Pset_nlinks(), and
 *          H5Pset_elink_prefix().
 *
 * \note H5Lcopy() does not affect the object that the link points to.
 *
 * \attention H5Lcopy() cannot copy hard links across files as a hard link is
 *            not valid without a target object; to copy objects from one file
 *            to another, see H5Ocopy().
 *
 * \author Lames Laird
 *
 * \date Wednesday, March 29, 2006
 *
 * \since 1.8.0
 *
 * \see H5Ocopy()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Lcopy(hid_t src_loc, const char *src_name, hid_t dst_loc,
    const char *dst_name, hid_t lcpl_id, hid_t lapl_id);
/**\ingroup H5L
 *
 * \brief Creates a hard link to an object
 *
 * \fgdta_loc_id{cur_loc}
 * \param[in] cur_name Name of the target object, which must already exist
 * \fgdta_loc_id{dst_loc}
 * \param[in] dst_name The name of the new link
 * \lcpl_id
 * \lapl_id
 *
 * \return \herr_t
 *
 * \todo We need to get the location ID story straight!
 *
 * \details H5Lcreate_hard() creates a new hard link to a pre-existing object
 *          in an HDF5 file.
 *
 *          \p cur_loc and \p cur_name specify the location
 *          and name, respectively, of the target object, i.e., the object that
 *          the new hard link points to. \p dst_loc and \p dst_name specify the
 *          location and name, respectively, of the new hard link.
 *
 *          \p cur_name and \p dst_name are interpreted relative to \p cur_loc
 *          and \p dst_loc, respectively. If \p cur_loc and \p dst_loc are the
 *          same location, the HDF5 macro #H5L_SAME_LOC can be used for either
 *          parameter (but not both).
 *
 *          \p lcpl_id and \p lapl_id are the link creation and access property
 *          lists associated with the new link.
 *
 * \note Hard and soft links are for use only if the target object is in the
 *       current file. If the desired target object is in a different file from
 *       the new link, an external link may be created with
 *       H5Lcreate_external().
 *
 * \note The HDF5 library keeps a count of all hard links pointing to an
 *       object; if the hard link count reaches zero (0), the object will be
 *       deleted from the file. Creating new hard links to an object will
 *       prevent it from being deleted if other links are removed. The
 *       library maintains no similar count for soft links and they can dangle.
 *
 * \note The new link may be one of many that point to that object.
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Lcreate_hard(hid_t cur_loc, const char *cur_name,
    hid_t dst_loc, const char *dst_name, hid_t lcpl_id, hid_t lapl_id);
/**\ingroup H5L
 *
 * \brief Creates a soft link
 *
 * \param[in] link_target An HDF5 path name
 * \fgdta_loc_id{link_loc_id}
 * \param[in] link_name The name of the new link
 * \lcpl_id
 * \lapl_id
 *
 * \return \herr_t
 *
 * \todo We need to get the location ID story straight!
 *
 * \details H5Lcreate_soft() creates a new soft link to an object in an HDF5
 *          file.
 *
 *          \p link_target specifies the HDF5 path name the soft link contains.
 *          \p link_target can be an arbitrary HDF5 path name and is
 *          interpreted only at lookup time. This path may be absolute in the
 *          file or relative to \p link_loc_id.
 *
 *          \p link_loc_id and \p link_name specify the location and name,
 *          respectively, of the new soft link. \p link_name is interpreted
 *          relative to \p link_loc_id and must contain only the name of the soft
 *          link; \p link_name may not contain any additional path elements.
 *
 *          If \p link_loc_id is a group identifier, the object pointed to by
 *          \p link_name will be accessed as a member of that group. If
 *          \p link_loc_id is a file identifier, the object will be accessed as a
 *          member of the file's root group.
 *
 *          \p lcpl_id and \p lapl_id are the link creation and access property
 *          lists associated with the new link.
 *
 *          For instance, if target_path is \c ./foo, \p link_loc_id specifies
 *          \c ./x/y/bar, and the name of the new link is \c new_link, then a
 *          subsequent request for \c ./x/y/bar/new_link will return same the
 *          object as would be found at \c ./foo.
 *
 * \note H5Lcreate_soft() is for use only if the target object is in the
 *       current file. If the desired target object is in a different file from
 *       the new link, use H5Lcreate_external() to create an external link.
 *
 * \note Soft links and external links are also known as symbolic links as they
 *       use a name to point to an object; hard links employ an object’s
 *       address in the file.
 *
 * \note Unlike hard links, a soft link in an HDF5 file is allowed to dangle,
 *       meaning that the target object need not exist at the time that the
 *       link is created.
 *
 * \note The HDF5 library does not keep a count of soft links as it does of
 *       hard links.
 *
 * \note The new link may be one of many that point to that object.
 *
 * \see H5Lcreate_hard(), H5Lcreate_external()
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Lcreate_soft(const char *link_target, hid_t link_loc_id,
    const char *link_name, hid_t lcpl_id, hid_t lapl_id);
H5_DLL herr_t H5Ldelete(hid_t loc_id, const char *name, hid_t lapl_id);
H5_DLL herr_t H5Ldelete_by_idx(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t lapl_id);
H5_DLL herr_t H5Lget_val(hid_t loc_id, const char *name, void *buf/*out*/,
    size_t size, hid_t lapl_id);
H5_DLL herr_t H5Lget_val_by_idx(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n,
    void *buf/*out*/, size_t size, hid_t lapl_id);
H5_DLL htri_t H5Lexists(hid_t loc_id, const char *name, hid_t lapl_id);
H5_DLL herr_t H5Lget_info2(hid_t loc_id, const char *name,
    H5L_info2_t *linfo /*out*/, hid_t lapl_id);
H5_DLL herr_t H5Lget_info_by_idx2(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n,
    H5L_info2_t *linfo /*out*/, hid_t lapl_id);
H5_DLL ssize_t H5Lget_name_by_idx(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n,
    char *name /*out*/, size_t size, hid_t lapl_id);
/**\ingroup H5L
 *
 * \brief Iterates over links in a group, with user callback routine,
 *        according to the order within an index.
 *
 * \group_id
 * \idx_type
 * \order
 * \param[in,out] idx_p    Pointer to an iteration index to allow
 *                         continuing a previous iteration
 * \op
 * \op_data
 * \return \success{The return value of the first operator that returns
 *                  non-zero, or zero if all members were processed with no
 *                  operator returning non-zero.}
 * \return \failure{Negative if an error occurs in the library, or the negative
 *                  value returned by one of the operators.}
 *
 * \details H5Literate2() iterates through the links in a file or
 *          group, \p group_id, in the order of the specified
 *          index, \p idx_type, using a user-defined callback routine
 *          \p op. H5Literate2() does not recursively follow links into
 *          subgroups of the specified group.
 *
 *          Three parameters are used to manage progress of the iteration:
 *          \p idx_type, \p order, and \p idx_p.
 *
 *          \p idx_type specifies the index to be used. If the links have
 *          not been indexed by the index type, they will first be sorted by
 *          that index then the iteration will begin; if the links have been
 *          so indexed, the sorting step will be unnecessary, so the iteration
 *          may begin more quickly.
 *
 *          \p order specifies the order in which objects are to be inspected
 *          along the index \p idx_type.
 *
 *          \p idx_p tracks the iteration and allows an iteration to be
 *          resumed if it was stopped before all members were processed. It is
 *          passed in by the application with a starting point and returned by
 *          the library with the point at which the iteration stopped.
 *
 *          \p op_data is a user-defined pointer to the data required to
 *          process links in the course of the iteration. This pointer is
 *          passed back to each step of the iteration in the \p op callback
 *          function's \p op_data parameter. \p op is invoked for each link
 *          encounter.
 *
 *          \p op_data is passed to and from each iteration and can be used to
 *          supply or aggregate information across iterations.
 *
 * \remark Same pattern of behavior as H5Giterate().
 *
 * \note This function is also available through the H5Literate() macro.
 *
 * \warning The behavior of H5Literate2() is undefined if the link
 *          membership of \p group_id changes during the iteration.
 *          This does not limit the ability to change link destinations
 *          while iterating, but caution is advised.
 *
 *
 * \since 1.12.0
 *
 * \see H5Literate_by_name2(), H5Lvisit2(), H5Lvisit_by_name2()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Literate2(hid_t grp_id, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t *idx, H5L_iterate2_t op, void *op_data);
H5_DLL herr_t H5Literate_by_name2(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx,
    H5L_iterate2_t op, void *op_data, hid_t lapl_id);
H5_DLL herr_t H5Lvisit2(hid_t grp_id, H5_index_t idx_type, H5_iter_order_t order,
    H5L_iterate2_t op, void *op_data);
H5_DLL herr_t H5Lvisit_by_name2(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, H5L_iterate2_t op,
    void *op_data, hid_t lapl_id);

/* UD link functions */
H5_DLL herr_t H5Lcreate_ud(hid_t link_loc_id, const char *link_name,
    H5L_type_t link_type, const void *udata, size_t udata_size, hid_t lcpl_id,
    hid_t lapl_id);
H5_DLL herr_t H5Lregister(const H5L_class_t *cls);
H5_DLL herr_t H5Lunregister(H5L_type_t id);
H5_DLL htri_t H5Lis_registered(H5L_type_t id);

/* External link functions */
H5_DLL herr_t H5Lunpack_elink_val(const void *ext_linkval/*in*/, size_t link_size,
   unsigned *flags, const char **filename/*out*/, const char **obj_path /*out*/);
/**\ingroup H5L
 *
 * \brief Creates an external link, a soft link to an object in a different file.
 *
 * \param[in] file_name   Name of the target file containing the target object.
 * \param[in] obj_name    Path within the target file to the target object
 * \fgdt_loc_id{link_loc_id}
 * \param[in] link_name   Name of the new link, relative to \p link_loc_id
 * \lcpl_id
 * \lapl_id
 * \return \herr_t
 *
 * \details H5Lcreate_external() creates a new external link. An external link
 *          is a soft link to an object in a different HDF5 file from the
 *          location of the link, i.e., to an external object.
 *
 *          \p file_name identifies the target file containing the target
 *          object; \p obj_name specifies the path of the target object within
 *          that file. \p obj_name must be an absolute pathname in
 *          \p file_name, i.e., it must start at the target file’s root group,
 *          but it is not interpreted until an application attempts to traverse
 *          it.
 *
 *          \p link_loc_id and \p link_name specify the location and name,
 *          respectively, of the new link. \p link_name is interpreted relative
 *          to \p link_loc_id.
 *
 *          \p lcpl_id is the link creation property list used in creating the
 *          new link.
 *
 *          \p lapl_id is the link access property list used in traversing the
 *          new link. Note that an external file opened by the traversal of an
 *          external link is always opened with the weak file close degree
 *          property setting, #H5F_CLOSE_WEAK (see H5Pset_fclose_degree());
 *          any file close degree property setting in \p lapl_id is ignored.
 *
 *          An external link behaves similarly to a soft link, and like a soft
 *          link in an HDF5 file, it may dangle: the target file and object
 *          need not exist at the time that the external link is created.
 *
 *          When the external link \p link_name is accessed, the library will
 *          search for the target file \p file_name as described below:
 *
 *          - If \p file_name is a relative pathname, the following steps are
 *            performed:
 *            - The library will get the prefix(es) set in the environment
 *              variable \c HDF5_EXT_PREFIX and will try to prepend each prefix
 *              to \p file_name to form a new \p file_name.
 *            - If the new \p file_name does not exist or if \c HDF5_EXT_PREFIX
 *              is not set, the library will get the prefix set via
 *              H5Pset_elink_prefix() and prepend it to \p file_name to form a
 *              new \p file_name.
 *            - If the new \p file_name does not exist or no prefix is being
 *              set by H5Pset_elink_prefix(), then the path of the file
 *              associated with \p link_loc_id is obtained. This path can be
 *              the absolute path or the current working directory plus the
 *              relative path of that file when it is created/opened. The
 *              library will prepend this path to \p file_name to form a new
 *              \p file_name.
 *            - If the new \p file_name does not exist, then the library will
 *              look for \p file_name and will return failure/success
 *              accordingly.
 *          - If \p file_name is an absolute pathname, the library will first
 *            try to find \p file_name. If \p file_name does not exist,
 *            \p file_name is stripped of directory paths to form a new
 *            \p file_name. The search for the new \p file_name then follows
 *            the same steps as described above for a relative pathname. See
 *            examples below illustrating how target_file_name is stripped to
 *            form a new \p file_name.
 *
 *          Note that \p file_name is considered to be an absolute pathname
 *          when the following condition is true:
 *
 *          - For Unix, the first character of \p file_name is a slash (\c /).
 *            For example, consider a \p file_name of \c /tmp/A.h5.
 *            If that target file does not exist, the new \p file_name after
 *            stripping will be \c A.h5.
 *          - For Windows, there are 6 cases:
 *            -# \p file_name is an absolute drive with absolute pathname.
 *               For example, consider a \p file_name of \c /tmp/A.h5. If that
 *               target file does not exist, the new \p file_name after
 *               stripping will be \c A.h5.
 *            -# \p file_name is an absolute pathname without specifying drive
 *               name. For example, consider a \p file_name of \c /tmp/A.h5.
 *               If that target file does not exist, the new \p file_name after
 *               stripping will be \c A.h5.
 *            -# \p file_name is an absolute drive with relative pathname.
 *               For example, consider a \p file_name of \c /tmp/A.h5. If that
 *               target file does not exist, the new \p file_name after
 *               stripping will be \c tmp\A.h5.
 *            -# \p file_name is in UNC (Uniform Naming Convention) format with
 *               server name, share name, and pathname. For example, consider
 *               a \p file_name of \c /tmp/A.h5. If that target file does not
 *               exist, the new \p file_name after stripping will be \c A.h5.
 *            -# \p file_name is in Long UNC (Uniform Naming Convention) format
 *               with server name, share name, and pathname. For example,
 *               consider a \p file_name of \c /tmp/A.h5. If that target file
 *               does not exist, the new \p file_name after stripping will be
 *               \c A.h5.
 *            -# \p file_name is in Long UNC (Uniform Naming Convention) format
 *               with an absolute drive and an absolute pathname. For example,
 *               consider a \p file_name of \c /tmp/A.h5. If that target file
 *               does not exist, the new \p file_name after stripping will be
 *               \c A.h5.
 *
 *          The library opens target file \p file_name with the file access
 *          property list that is set via H5Pset_elink_fapl() when the external
 *          link link_name is accessed. If no such property list is set, the
 *          library uses the file access property list associated with the file
 *          of \p link_loc_id to open the target file.
 *
 *          If an application requires additional control over file access
 *          flags or the file access property list, see H5Pset_elink_cb(); this
 *          function enables the use of an external link callback function as
 *          described in H5L_elink_traverse_t().
 *
 * \attention A file close degree property setting (H5Pset_fclose_degree()) in
 *            the external link file access property list or in the external
 *            link callback function will be ignored. A file opened by means of
 *            traversing an external link is always opened with the weak file
 *            close degree property setting, #H5F_CLOSE_WEAK .
 *
 * \author Quincey Koziol
 *
 * \date Wednesday, May 18, 2005
 *
 * \since 1.8.0
 *
 * \see H5Lcreate_hard(), H5Lcreate_soft(), H5Lcreate_ud()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Lcreate_external(const char *file_name, const char *obj_name,
    hid_t link_loc_id, const char *link_name, hid_t lcpl_id, hid_t lapl_id);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */

/* Previous versions of the H5L_class_t struct */
#define H5L_LINK_CLASS_T_VERS_0     0


/* Typedefs */

/* Information struct for link (for H5Lget_info1/H5Lget_info_by_idx1) */
typedef struct {
    H5L_type_t          type;           /* Type of link                   */
    hbool_t             corder_valid;   /* Indicate if creation order is valid */
    int64_t             corder;         /* Creation order                 */
    H5T_cset_t          cset;           /* Character set of link name     */
    union {
        haddr_t         address;        /* Address hard link points to    */
        size_t          val_size;       /* Size of a soft link or UD link value */
    } u;
} H5L_info1_t;

/* Callback during link traversal */
typedef hid_t (*H5L_traverse_0_func_t)(const char *link_name, hid_t cur_group,
    const void *lnkdata, size_t lnkdata_size, hid_t lapl_id);

/* User-defined link types */
typedef struct {
    int version;                    /* Version number of this struct        */
    H5L_type_t id;                  /* Link type ID                         */
    const char *comment;            /* Comment for debugging                */
    H5L_create_func_t create_func;  /* Callback during link creation        */
    H5L_move_func_t move_func;      /* Callback after moving link           */
    H5L_copy_func_t copy_func;      /* Callback after copying link          */
    H5L_traverse_0_func_t trav_func; /* Callback during link traversal       */
    H5L_delete_func_t del_func;     /* Callback for link deletion           */
    H5L_query_func_t query_func;    /* Callback for queries                 */
} H5L_class_0_t;

/* Prototype for H5Literate1/H5Literate_by_name1() operator */
typedef herr_t (*H5L_iterate1_t)(hid_t group, const char *name, const H5L_info1_t *info,
    void *op_data);


/* Function prototypes */
H5_DLL herr_t H5Lget_info1(hid_t loc_id, const char *name,
    H5L_info1_t *linfo /*out*/, hid_t lapl_id);
H5_DLL herr_t H5Lget_info_by_idx1(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n,
    H5L_info1_t *linfo /*out*/, hid_t lapl_id);
/**\ingroup H5L
 *
 * \brief Iterates over links in a group, with user callback routine,
 *        according to the order within an index.
 *
 * \group_id
 * \idx_type
 * \order
 * \param[in,out] idx_p    Pointer to an iteration index to allow
 *                         continuing a previous iteration
 * \op
 * \op_data
 * \return \success{The return value of the first operator that returns
 *                  non-zero, or zero if all members were processed with no
 *                  operator returning non-zero.}
 * \return \failure{Negative if an error occurs in the library, or the negative
 *                  value returned by one of the operators.}
 *
 * \deprecated Deprecated in favor of H5Literate2().
 *
 * \details H5Literate1() iterates through the links in a file or
 *          group, \p group_id, in the order of the specified
 *          index, \p idx_type, using a user-defined callback routine
 *          \p op. H5Literate1() does not recursively follow links into
 *          subgroups of the specified group.
 *
 *          Three parameters are used to manage progress of the iteration:
 *          \p idx_type, \p order, and \p idx_p.
 *
 *          \p idx_type specifies the index to be used. If the links have
 *          not been indexed by the index type, they will first be sorted by
 *          that index then the iteration will begin; if the links have been
 *          so indexed, the sorting step will be unnecessary, so the iteration
 *          may begin more quickly.
 *
 *          \p order specifies the order in which objects are to be inspected
 *          along the index \p idx_type.
 *
 *          \p idx_p tracks the iteration and allows an iteration to be
 *          resumed if it was stopped before all members were processed. It is
 *          passed in by the application with a starting point and returned by
 *          the library with the point at which the iteration stopped.
 *
 *          \p op_data is a user-defined pointer to the data required to
 *          process links in the course of the iteration. This pointer is
 *          passed back to each step of the iteration in the \p op callback
 *          function's \p op_data parameter. \p op is invoked for each link
 *          encounter.
 *
 *          \p op_data is passed to and from each iteration and can be used to
 *          supply or aggregate information across iterations.
 *
 * \remark Same pattern of behavior as H5Giterate().
 *
 * \note This function is also available through the H5Literate() macro.
 *
 * \warning The behavior of H5Literate1() is undefined if the link
 *          membership of \p group_id changes during the iteration.
 *          This does not limit the ability to change link destinations
 *          while iterating, but caution is advised.
 *
 *
 * \version 1.12.0 Function was deprecated in this release.
 * \since 1.8.0
 *
 * \see H5Literate_by_name2(), H5Lvisit2(), H5Lvisit_by_name2()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Literate1(hid_t grp_id, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t *idx, H5L_iterate1_t op, void *op_data);
H5_DLL herr_t H5Literate_by_name1(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx,
    H5L_iterate1_t op, void *op_data, hid_t lapl_id);
H5_DLL herr_t H5Lvisit1(hid_t grp_id, H5_index_t idx_type, H5_iter_order_t order,
    H5L_iterate1_t op, void *op_data);
H5_DLL herr_t H5Lvisit_by_name1(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, H5L_iterate1_t op,
    void *op_data, hid_t lapl_id);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* _H5Lpublic_H */
