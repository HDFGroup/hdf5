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
 * Created:             H5Opublic.h
 *                      Aug  5 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Public declarations for the H5O (object header)
 *                      package.
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Opublic_H
#define _H5Opublic_H

/* Public headers needed by this file */
#include "H5public.h"		/* Generic Functions			*/
#include "H5Ipublic.h"		/* IDs			  		*/
#include "H5Lpublic.h"		/* Links		  		*/

/*****************/
/* Public Macros */
/*****************/

/* Flags for object copy (H5Ocopy) */
#define H5O_COPY_SHALLOW_HIERARCHY_FLAG (0x0001u)   /* Copy only immediate members */
#define H5O_COPY_EXPAND_SOFT_LINK_FLAG  (0x0002u)   /* Expand soft links into new objects */
#define H5O_COPY_EXPAND_EXT_LINK_FLAG   (0x0004u)   /* Expand external links into new objects */
#define H5O_COPY_EXPAND_REFERENCE_FLAG	(0x0008u)   /* Copy objects that are pointed by references */
#define H5O_COPY_WITHOUT_ATTR_FLAG      (0x0010u)   /* Copy object without copying attributes */
#define H5O_COPY_PRESERVE_NULL_FLAG     (0x0020u)   /* Copy NULL messages (empty space) */
#define H5O_COPY_MERGE_COMMITTED_DTYPE_FLAG (0x0040u)   /* Merge committed datatypes in dest file */
#define H5O_COPY_ALL                    (0x007Fu)   /* All object copying flags (for internal checking) */

/* Flags for shared message indexes.
 * Pass these flags in using the mesg_type_flags parameter in
 * H5P_set_shared_mesg_index.
 * (Developers: These flags correspond to object header message type IDs,
 * but we need to assign each kind of message to a different bit so that
 * one index can hold multiple types.)
 */
#define H5O_SHMESG_NONE_FLAG    0x0000          /* No shared messages */
#define H5O_SHMESG_SDSPACE_FLAG ((unsigned)1 << 0x0001) /* Simple Dataspace Message.  */
#define H5O_SHMESG_DTYPE_FLAG   ((unsigned)1 << 0x0003) /* Datatype Message.  */
#define H5O_SHMESG_FILL_FLAG    ((unsigned)1 << 0x0005) /* Fill Value Message. */
#define H5O_SHMESG_PLINE_FLAG   ((unsigned)1 << 0x000b) /* Filter pipeline message.  */
#define H5O_SHMESG_ATTR_FLAG    ((unsigned)1 << 0x000c) /* Attribute Message.  */
#define H5O_SHMESG_ALL_FLAG     (H5O_SHMESG_SDSPACE_FLAG | H5O_SHMESG_DTYPE_FLAG | H5O_SHMESG_FILL_FLAG | H5O_SHMESG_PLINE_FLAG | H5O_SHMESG_ATTR_FLAG)

/* Object header status flag definitions */
#define H5O_HDR_CHUNK0_SIZE             0x03    /* 2-bit field indicating # of bytes to store the size of chunk 0's data */
#define H5O_HDR_ATTR_CRT_ORDER_TRACKED  0x04    /* Attribute creation order is tracked */
#define H5O_HDR_ATTR_CRT_ORDER_INDEXED  0x08    /* Attribute creation order has index */
#define H5O_HDR_ATTR_STORE_PHASE_CHANGE 0x10    /* Non-default attribute storage phase change values stored */
#define H5O_HDR_STORE_TIMES             0x20    /* Store access, modification, change & birth times for object */
#define H5O_HDR_ALL_FLAGS       (H5O_HDR_CHUNK0_SIZE | H5O_HDR_ATTR_CRT_ORDER_TRACKED | H5O_HDR_ATTR_CRT_ORDER_INDEXED | H5O_HDR_ATTR_STORE_PHASE_CHANGE | H5O_HDR_STORE_TIMES)

/* Maximum shared message values.  Number of indexes is 8 to allow room to add
 * new types of messages.
 */
#define H5O_SHMESG_MAX_NINDEXES 8
#define H5O_SHMESG_MAX_LIST_SIZE 5000

/* Flags for H5Oget_info.
 * Theses flags determine which fields will be filled in in the H5O_info_t
 * struct.
 */
#define H5O_INFO_BASIC          0x0001u         /* Fill in the fileno, addr, type, and rc fields */
#define H5O_INFO_TIME           0x0002u         /* Fill in the atime, mtime, ctime, and btime fields */
#define H5O_INFO_NUM_ATTRS      0x0004u         /* Fill in the num_attrs field */
#define H5O_INFO_ALL            (H5O_INFO_BASIC | H5O_INFO_TIME | H5O_INFO_NUM_ATTRS)

/* Flags for H5Oget_native_info.
 * Theses flags determine which fields will be filled in in the H5O_native_info_t
 * struct.
 */
#define H5O_NATIVE_INFO_HDR            0x0008u         /* Fill in the hdr field */
#define H5O_NATIVE_INFO_META_SIZE      0x0010u         /* Fill in the meta_size field */
#define H5O_NATIVE_INFO_ALL            (H5O_NATIVE_INFO_HDR | H5O_NATIVE_INFO_META_SIZE)

/* Convenience macro to check if the token is the 'undefined' token value */
#define H5O_IS_TOKEN_UNDEF(token)    (!HDmemcmp(&(token), &(H5O_TOKEN_UNDEF), sizeof(H5O_token_t)))


/*******************/
/* Public Typedefs */
/*******************/

//! [H5O_type_t_snip] 

/* Types of objects in file */
typedef enum H5O_type_t {
    H5O_TYPE_UNKNOWN = -1,	/* Unknown object type		*/
    H5O_TYPE_GROUP,	        /* Object is a group		*/
    H5O_TYPE_DATASET,		/* Object is a dataset		*/
    H5O_TYPE_NAMED_DATATYPE, 	/* Object is a named data type	*/
    H5O_TYPE_MAP,               /* Object is a map */
    H5O_TYPE_NTYPES             /* Number of different object types (must be last!) */
} H5O_type_t;

//! [H5O_type_t_snip] 

/* Information struct for object header metadata (for H5Oget_info/H5Oget_info_by_name/H5Oget_info_by_idx) */
//! [H5O_hdr_info_t_snip] 

typedef struct H5O_hdr_info_t {
    unsigned version;		/* Version number of header format in file */
    unsigned nmesgs;		/* Number of object header messages */
    unsigned nchunks;		/* Number of object header chunks */
    unsigned flags;             /* Object header status flags */
    struct {
        hsize_t total;		/* Total space for storing object header in file */
        hsize_t meta;		/* Space within header for object header metadata information */
        hsize_t mesg;		/* Space within header for actual message information */
        hsize_t free;		/* Free space within object header */
    } space;
    struct {
        uint64_t present;	/* Flags to indicate presence of message type in header */
        uint64_t shared;	/* Flags to indicate message type is shared in header */
    } mesg;
} H5O_hdr_info_t;

//! [H5O_hdr_info_t_snip] 

//! [H5O_info2_t_snip]

/* Data model information struct for objects */
/* (For H5Oget_info / H5Oget_info_by_name / H5Oget_info_by_idx version 3) */
typedef struct H5O_info2_t {
    unsigned long   fileno;     /* File number that object is located in */
    H5O_token_t     token;      /* Token representing the object        */
    H5O_type_t 	    type;       /* Basic object type (group, dataset, etc.) */
    unsigned        rc;         /* Reference count of object            */
    time_t          atime;      /* Access time                          */
    time_t          mtime;      /* Modification time                    */
    time_t          ctime;      /* Change time                          */
    time_t          btime;      /* Birth time                           */
    hsize_t         num_attrs;  /* # of attributes attached to object   */
} H5O_info2_t;

//! [H5O_info2_t_snip]

/* Native file format information struct for objects */
/* (For H5Oget_native_info / H5Oget_native_info_by_name / H5Oget_native_info_by_idx) */
typedef struct H5O_native_info_t {
    H5O_hdr_info_t      hdr;            /* Object header information */
    /* Extra metadata storage for obj & attributes */
    struct {
        H5_ih_info_t   obj;             /* v1/v2 B-tree & local/fractal heap for groups, B-tree for chunked datasets */
        H5_ih_info_t   attr;            /* v2 B-tree & heap for attributes */
    } meta_size;
} H5O_native_info_t;

/* Typedef for message creation indexes */
typedef uint32_t H5O_msg_crt_idx_t;

/* Prototype for H5Ovisit/H5Ovisit_by_name() operator (version 3) */
typedef herr_t (*H5O_iterate2_t)(hid_t obj, const char *name, const H5O_info2_t *info,
    void *op_data);

typedef enum H5O_mcdt_search_ret_t {
    H5O_MCDT_SEARCH_ERROR = -1,	/* Abort H5Ocopy */
    H5O_MCDT_SEARCH_CONT,	/* Continue the global search of all committed datatypes in the destination file */
    H5O_MCDT_SEARCH_STOP	/* Stop the search, but continue copying.  The committed datatype will be copied but not merged. */
} H5O_mcdt_search_ret_t;

/* Callback to invoke when completing the search for a matching committed datatype from the committed dtype list */
typedef H5O_mcdt_search_ret_t (*H5O_mcdt_search_cb_t)(void *op_data);


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
 * \ingroup H5O
 *
 * \brief Opens an object in an HDF5 file by location identifier and path name.
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] name Path to the object; relative to \p loc_id
 * \lapl_id
 *
 * \return \hid_tv{object}
 *
 * \details H5Oopen() opens a group, dataset, or committed (named) datatype
 *          specified by a location, \p loc_id, and a path name, \p name, in an HDF5 file.
 *
 *          This function opens the object in the same manner as H5Gopen(), H5Topen(), and H5Dopen().
 *          However, H5Oopen() does not require the type of object to be known beforehand.  
 *          This can be useful with user-defined links, for instance, when only a path may be known.
 *
 *          H5Oopen() cannot be used to open a dataspace, attribute, property list, or file.
 *
 *          Once an object of unknown type has been opened with H5Oopen(), 
 *          the type of that object can be determined by means of an H5Iget_type() call.
 *
 *          \p loc_id may be a file, group, dataset, named datatype, or attribute.
 *          If an attribute is specified for \p loc_id then the object where the 
 *          attribute is attached will be accessed.
 *
 *          \p name must be the path to that object relative to \p loc_id.
 *
 *          \p lapl_id is the link access property list associated with the link pointing to 
 *          the object.  If default link access properties are appropriate, this can be 
 *          passed in as #H5P_DEFAULT.
 *
 *          When it is no longer needed, the opened object should be closed with
 *          H5Oclose(), H5Gclose(), H5Tclose(), or H5Dclose().
 *
 * \todo Check I split history into version and since
 *
 * \version 1.8.1 Fortran subroutine introduced in this release.
 *
 * \since 1.8.0 
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Oopen(hid_t loc_id, const char *name, hid_t lapl_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Opens an object in an HDF5 file using its VOL independent token
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] token Object token
 *
 * \return \hid_ti{object}
 *
 * \details H5Oopen_by_token() opens an object specified by the object 
 *          identifier, \p loc_id and object token, \p token.
 *
 * \par Example
 *      An example snippet from examples/h5_extlink.c:
 *      \snippet h5_extlink.c H5Open_by_token_snip
 *
 * \since 1.12.0 
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Oopen_by_token(hid_t loc_id, H5O_token_t token);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Opens the nth object in a group
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] group_name Name of group, relative to \p loc_id, in which object is located
 * \idx_type
 * \order
 * \param[in] n Object to open
 * \lapl_id
 *
 * \return \hid_tv{object}
 *
 * \details H5Open_by_idx() opens the nth object in the group specified by \p loc_id 
 *          and \p group_name.
 *
 *          \p loc_id specifies a location identifier.  \p group_name specifies the group relative to
 *          \p loc_id in which the object can be found.  If \p loc_id fully specifies the group 
 *          in which the object resides, \p group_name can be a dot (.).
 *
 *          The specific object to be opened within the group is specified by the three parameters:
 *          \p idx_type, \p order and \p n.
 *
 *          \p idx_type specifies the type of index by which objects are ordered.
 *          Valid index types include the following:
 *
 *          \indexes
 *
 *          \p order specifies the order in which the objects are to be referenced for the purposes 
 *          of this function.  Valid orders include the following:
 *
 *          \orders
 *
 *          Note that for #H5_ITER_NATIVE, rather than implying a particular order, 
 *          it instructs the HDF5 library to iterate through the objects in the fastest 
 *          available order, i.e., in a natural order.
 * 
 *          \p n specifies the position of the object within the index.  Note that this count is 
 *          zero-based; 0 (zero) indicates that the function will return the value of the first object; 
 *          if \p n is 5, the function will return the value of the sixth object; etc.
 *
 *          \p lapl_id specifies the link access property list to be used in accessing the object.
 *
 *          An object opened with this function should be closed when it is no longer needed so that 
 *          resource leaks will not develop.  H5Oclose() can be used to close groups, datasets,
 *          or committed datatypes.
 *
 * \todo Check I split history into version and since
 *
 * \version 1.8.1 Fortran subroutine introduced in this release.
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Oopen_by_idx(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t lapl_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Determines whether a link resolves to an actual object.
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] name The name of the link to check
 * \lapl_id
 *
 * \return Returns a positive value if the object pointed to by the \p loc_id and \p name combination exists. 
 * \return Returns 0 if the object pointed to by the \p loc_id and \p name combination does not exist.
 * \return Returns a negatvie value when the function fails.
 *
 * \details H5Oexists_by_name() allows an application to determine whether
 *          the link \p name in the group or file specified with \p loc_id
 *          resolves to an HDF5 object to open or if the link dangles. The
 *          link may be of any type, but hard links will always resolve
 *          to objects and do not need to be verified.
 *
 *          Note that H5Oexists_by_name() verifies only that the target
 *          object exists. If \p name includes either a relative path or
 *          an absolute path to the target link, intermediate steps
 *          along the path must be verified before the existence of
 *          the target link can be safely checked. If the path is not
 *          verified and an intermediate element of the path does not
 *          exist, H5Oexists_by_name() will fail. The example in the next
 *          paragraph illustrates one step-by-step method for verifying
 *          the existence of a link with a relative or absolute path.
 *
 * \par Example
 *          Use the following steps to verify the existence of
 *          the link \c datasetD in the \c group group1/group2/softlink_to_group3/,
 *          where \c group1 is a member of the group specified by \c loc_id:
 *
 * \par
 *          <ol type="1">
 *          <li>First use H5Lexists() to verify that a link named \c group1 exists.</li>
 *          <li>If \c group1 exists, use H5Oexists_by_name() to verify that the
 *             link \c group1 resolves to an object.</li>
 *          <li>If \c group1 exists, use  H5Lexists() again, this time with name
 *             set to \c group1/group2, to verify that the link \c group2 exists
 *             in \c group1.</li>
 *          <li>If the \c group2 link exists, use H5Oexists_by_name() to verify
 *             that \c group1/group2 resolves to an object.</li>
 *          <li>If \c group2 exists, use  H5Lexists() again, this time with name
 *             set to \c group1/group2/softlink_to_group3, to verify that the
 *             link \c softlink_to_group3 exists in \c group2.</li>
 *          <li>If the \c softlink_to_group3 link exists, use H5Oexists_by_name()
 *             to verify that \c group1/group2/softlink_to_group3 resolves to
 *             an object.</li>
 *          <li>If \c softlink_to_group3 exists, you can now safely use H5Lexists
 *             with name set to \c group1/group2/softlink_to_group3/datasetD to
 *             verify that the target link, \c datasetD, exists.</li>
 *          <li>And finally, if the link \c datasetD exists, use H5Oexists_by_name
 *             to verify that \c group1/group2/softlink_to_group3/datasetD
 *             resolves to an object.</li>
 *          </ol>
 *
 * \par
 *          If the link to be verified is specified with an absolute path,
 *          the same approach should be used, but starting with the first
 *          link in the file’s root group. For instance, if \c datasetD
 *          were in \c /group1/group2/softlink_to_group3, the first call to
 *          H5Lexists() would have name set to \c /group1.
 *
 * \par
 *          If the link to be verified is specified with an absolute path,
 *          Note that this is an outline and does not include all necessary
 *          details. Depending on circumstances, for example, an application
 *          may need to verify the type of an object also.
 *
 * \warning \Bold{Failure Modes:}
 * \warning If \p loc_id and \p name both exist but the combination does not
 *          resolve to an object, the function will return 0 (zero);
 *          the function does not fail in this case.
 * \warning If either the location or the link specified by the \p loc_id
 *          and \p name combination does not exist, the function will fail,
 *          returning a negative value.
 * \warning Note that verifying the existence of an object within an HDF5
 *          file is a multistep process. An application can be certain the
 *          object does not exist only if H5Lexists()  and H5Oexists_by_name()
 *          have been used to verify the existence of the links and groups
 *          in the hierarchy above that object. The example above, in the
 *          function description, provides a step-by-step description of
 *          that verification process.
 *
 * \todo Check I split history into version and since
 *
 * \version 1.8.11 Fortran subroutine introduced in this release.
 *
 * \since 1.8.5
 *
 *-------------------------------------------------------------------------
 */
H5_DLL htri_t H5Oexists_by_name(hid_t loc_id, const char *name, hid_t lapl_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Retrieves the metadata for an object specified by an identifier
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[out] oinfo Buffer in which to return object information
 * \param[in] fields Flags specifying the fields to include in \p oinfo
 *
 * \return \herr_t
 *
 * \details H5Oget_info3() specifies an object by its identifier, \p loc_id , and 
 *          retrieves the metadata describing that object in \p oinfo , an \ref H5O_info2_t \c struct.
 *
 *          The \ref H5O_info2_t \c struct is defined in H5Opublic.h as follows :
 *          \snippet this H5O_info2_t_snip
 *
 *          Note the following about \ref H5O_info2_t :
 *          - Of the four time fields (\c atime, \c mtime, \c ctime, and \c btime) only \c ctime  has been implemented.
 *          - The \c atime value is the last time the object was read or written.
 *          - The \c mtime value is the last time the raw data in the object was changed.
 *          - The \c ctime value is the last time the metadata for the object was changed.
 *          - The \c btime value is the time the object was created.
 *          .
 *
 *          The \ref H5O_token_t is defined in H5public.h as follows:
 *          \snippet H5public.h H5O_token_t_snip
 *
 *          The \ref H5O_type_t \c enum indicates the object type and 
 *          is defined in H5Opublic.h as follows:
 *          \snippet this H5O_type_t_snip
 *      
 *          Note that the object retrieved as indicated by \p loc_id
 *          refers only to the types specified by \ref H5O_type_t.
 *
 *          The \p fields parameter contains flags to determine which fields will be filled in 
 *          the \ref H5O_info2_t \c struct returned in \p oinfo. 
 *          These flags are defined in the H5Opublic.h file:
 *
 *          \obj_info_fields
 *
 * \par Example
 *      An example snippet from examples/h5_attribute.c:
 * \par
 *      \snippet h5_attribute.c H5Oget_info3_snip
 *
 * \note If you are iterating through a lot of different objects to
 *       retrieve information via the H5Oget_info() family of routines,
 *       you may see memory building up. This can be due to memory
 *       allocation for metadata such as object headers and messages
 *       when the iterated objects are put into the metadata cache.
 * \note
 *       If the memory buildup is not desirable, you can configure a
 *       smaller cache via H5Fset_mdc_config() or set the file access
 *       property list via H5Pset_mdc_config(). A smaller sized cache
 *       will force metadata entries to be evicted from the cache,
 *       thus freeing the memory associated with the entries.
 *
 * \since 1.12.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oget_info3(hid_t loc_id, H5O_info2_t *oinfo, unsigned fields);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Retrieves the metadata for an object, identifying the object by 
 *        location and relative name
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] name Name of group, relative to \p loc_id
 * \param[out] oinfo Buffer in which to return object information
 * \param[in] fields Flags specifying the fields to include in \p oinfo
 * \lapl_id
 *
 * \return \herr_t
 *
 * \details H5Oget_info_by_name3() specifies an object’s location and name, 
 *          \p loc_id and \p name, respectively, and retrieves the metadata 
 *          describing that object in \p oinfo, an \ref H5O_info2_t struct.
 *
 *          \p oinfo, in which the object information is returned, is a \c struct of
 *          type \ref H5O_info2_t, which is defined in H5Opublic.h in the HDF5 source code:
 *
 *          \snippet this H5O_info2_t_snip
 *
 *          Note the following about \ref H5O_info2_t :
 *          - Of the four time fields (\c atime, \c mtime, \c ctime, and \c btime) only \c ctime  has been implemented.
 *          - The \c atime value is the last time the object was read or written.
 *          - The \c mtime value is the last time the raw data in the object was changed.
 *          - The \c ctime value is the last time the metadata for the object was changed.
 *          - The \c btime value is the time the object was created.
 *          .
 *
 *          The \ref H5O_token_t is defined in H5public.h as follows:
 *          \snippet H5public.h H5O_token_t_snip
 *
 *          The \ref H5O_type_t \c enum indicates the object type and 
 *          is defined in H5Opublic.h as follows:
 *          \snippet this H5O_type_t_snip
 *
 *          Note that the object retrieved as indicated by \p loc_id
 *          refers only to the types specified by \ref H5O_type_t.
 *
 *          The \p fields parameter contains flags to determine which fields will be filled in 
 *          the \ref H5O_info2_t \c struct returned in \p oinfo. 
 *          These flags are defined in the H5Opublic.h file:
 *
 *          \obj_info_fields
 *
 *          The link access property list, \c lapl_id, is not currently used; 
 *          it should be passed in as #H5P_DEFAULT.
 *
 * \par Example
 *      An example snippet from test/vol.c:
 *      \snippet vol.c H5Oget_info_by_name3_snip
 *
 * \since 1.12.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oget_info_by_name3(hid_t loc_id, const char *name, H5O_info2_t *oinfo,
    unsigned fields, hid_t lapl_id);


/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Retrieves the metadata for an object, identifying the object 
 *        by an index position
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] group_name Name of group in which object is located
 * \idx_type
 * \order
 * \param[in] n Position within the index
 * \param[out] oinfo Buffer in which to return object information
 * \param[in] fields Flags specifying the fields to include in \p oinfo
 * \lapl_id
 *
 * \return \herr_t
 *
 * \details H5Oget_info_by_idx3() retrieves the metadata describing an
 *          object in the \c struct \p oinfo, as specified by the location,
 *          \p loc_id, group name, \p group_name, the index by which objects
 *          in that group are tracked, \p idx_type, the order by which the
 *          index is to be traversed, \p order, and an object’s position
 *          \p n within that index.
 *
 *          If \p loc_id fully specifies the group in which the object resides,
 *          i\p group_name can be a dot (\c .).
 *
 *          \p idx_type is of type \ref H5_index_t, defined in H5public.h as:
 *          \snippet H5public.h H5_index_t_snip
 *
 *          \p order is of type \ref H5_iter_order_t defined in H5public.h as:
 *          \snippet H5public.h H5_iter_order_t_snip
 *
 *          \p oinfo, in which the object information is returned, is a \c struct of
 *          type \ref H5O_info2_t, which is defined in H5Opublic.h in the HDF5 source code:
 *          \snippet this H5O_info2_t_snip
 *
 *          Note the following about \ref H5O_info2_t :
 *          - Of the four time fields (\c atime, \c mtime, \c ctime, and \c btime) only \c ctime  has been implemented.
 *          - The \c atime value is the last time the object was read or written.
 *          - The \c mtime value is the last time the raw data in the object was changed.
 *          - The \c ctime value is the last time the metadata for the object was changed.
 *          - The \c btime value is the time the object was created.
 *          .
 *
 *          \ref H5O_token_t is defined in H5public.h as follows:
 *          \snippet H5public.h H5O_token_t_snip
 *
 *          The \ref H5O_type_t \c enum indicates the object type and 
 *          is defined in H5Opublic.h as follows:
 *          \snippet this H5O_type_t_snip
 *
 *          Note that the object retrieved as indicated by \p loc_id
 *          refers only to the types specified by \ref H5O_type_t.
 *
 *          The \p fields parameter contains flags to determine which fields will be filled in 
 *          the \ref H5O_info2_t \c struct returned in \p oinfo. 
 *          These flags are defined in the H5Opublic.h file:
 *          \obj_info_fields
 *
 *          The link access property list, \c lapl_id, is not currently used; 
 *          it should be passed in as #H5P_DEFAULT.
 *
 * \par Example
 *      An example snippet from test/titerate.c:
 *      \snippet titerate.c H5Oget_info_by_idx3_snip
 *
 * \todo Check: I modify description for several parameters
 *
 * \since 1.12.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oget_info_by_idx3(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5O_info2_t *oinfo,
    unsigned fields, hid_t lapl_id);
H5_DLL herr_t H5Oget_native_info(hid_t loc_id, H5O_native_info_t *oinfo, unsigned fields);
H5_DLL herr_t H5Oget_native_info_by_name(hid_t loc_id, const char *name, H5O_native_info_t *oinfo,
    unsigned fields, hid_t lapl_id);
H5_DLL herr_t H5Oget_native_info_by_idx(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5O_native_info_t *oinfo,
    unsigned fields, hid_t lapl_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Creates a hard link to an object in an HDF5 file
 *
 * \param[in] object_id Object to be linked
 * \param[in] new_loc_id Location identifier at which object is to be linked; 
 *                       may be a file, group, dataset, named datatype or attribute identifier.
 * \param[in] new_link_name Name of link to be created, relative to \p new_loc_id.
 * \lcpl_id
 * \lapl_id
 *
 * \return \herr_t
 *
 * \details H5Olink() creates a new hard link to an object in an HDF5 file.
 *          \p new_loc_id and \p \p new_link_name specify the location and name of the
 *          new link while \p object_id identifies the object that the link
 *          points to.
 *
 *          H5Olink() is designed for two purposes:
 *          - To create the first hard link to an object that has just
 *            been created with H5Dcreate_anon(), H5Gcreate_anon(), or
 *            H5Tcommit_anon().  
 *          - To add additional structure to an existing
 *            file so that, for example, an object can be shared among
 *            multiple groups.
 *
 *          \p lcpl and \p lapl are the link creation and access property lists
 *          associated with the new link.
 *
 * \par Example:
 *      To create a new link to an object while simultaneously creating
 *      missing intermediate groups: Suppose that an application must
 *      create the group C with the path /A/B01/C but may not know
 *      at run time whether the groups A and B01 exist. The following
 *      code ensures that those groups are created if they are missing:
 * \par
 * \code
 *
 *      //Creates a link creation property list (LCPL).
 *      hid_t lcpl_id = H5Pcreate(H5P_LINK_CREATE);  
 *
 *      //Sets "create missing intermediate groups" property in that LCPL.
 *      int status = H5Pset_create_intermediate_group(lcpl_id, TRUE);
 *
 *      //Creates a group without linking it into the file structure.
 *      hid_t gid  = H5Gcreate_anon(file_id, H5P_DEFAULT, H5P_DEFAULT);
 *
 *      //Links group into file structure.
 *      status = H5Olink(gid, file_id, "/A/B01/C", lcpl_id, H5P_DEFAULT);
 *
 * \endcode
 *
 * \par 
 *      Note that unless the object is intended to be temporary,
 *      the H5O_LINK call is mandatory if an object created with one
 *      of the H5*_CREATE_ANON functions (or with H5T_COMMIT_ANON)
 *      is to be retained in the file; without an H5O_LINK call,
 *      the object will not be linked into the HDF5 file structure
 *      and will be deleted when the file is closed.
 *
 * \todo Correction in the example code
 * \todo Check I split history into version and since
 *
 * \version 1.8.1 Fortran subroutine introduced in this release.
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Olink(hid_t obj_id, hid_t new_loc_id, const char *new_name,
    hid_t lcpl_id, hid_t lapl_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Increments an object reference count
 *
 * \fgdta_loc_obj_id{object_id}
 *
 * \return \herr_t
 *
 * \details H5Oincr_refcount() increments the hard link reference count for an object.
 *          It should be used any time a user-defined link that references
 *          an object by address is added. When the link is deleted,
 *          H5Odecr_refcount() should be used.
 *
 *          An object’s reference count is the number of hard links in the
 *          file that point to that object. See the “Programming Model”
 *          section of the HDF5 Groups chapter in the -- <em>HDF5 User’s Guide</em>
 *          for a more complete discussion of reference counts.
 *
 *          If a user application needs to determine an object’s reference
 *          count, an H5Oget_info() call is required; the reference count
 *          is returned in the \c rc field of the \ref H5O_info_t \c struct.
 *
 * \warning This function must be used with care! 
 * \warning Improper use can lead to inaccessible data, wasted space in the file, 
 *          or <b><em>file corruption</em></b>.
 *
 * \todo Check reference to user's guide
 *
 * \version 1.8.11 Fortran subroutine introduced in this release.
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oincr_refcount(hid_t object_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Decrements an object reference count
 *
 * \fgdta_loc_obj_id{object_id}
 *
 * \return \herr_t
 *
 * \details H5Odecr_refcount() decrements the hard link reference count for an object.
 *          It should be used any time a user-defined link that references
 *          an object by address is deleted. In general, H5Oincr_refcount() will have
 *          been used previously, when the link was created.
 *
 *          An object’s reference count is the number of hard links in the
 *          file that point to that object. See the “Programming Model”
 *          section of the HDF5 Groups chapter in the <em>HDF5 User’s Guide</em>
 *          for a more complete discussion of reference counts.
 *
 *          If a user application needs to determine an object’s reference
 *          count, an H5Oget_info() call is required; the reference count
 *          is returned in the \c rc field of the \ref H5O_info_t \c struct.
 *
 * \warning This function must be used with care! 
 * \warning Improper use can lead to inaccessible data, wasted space in the file, 
 *          or <b><em>file corruption</em></b>.
 *
 * \todo Check reference user's guide; check H5O_info_t 
 * \todo Check I split history into version and since
 *
 * \version 1.8.11 Fortran subroutine introduced in this release.
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Odecr_refcount(hid_t object_id);

H5_DLL herr_t H5Ocopy(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
    const char *dst_name, hid_t ocpypl_id, hid_t lcpl_id);


/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Sets comment for specified object
 *
 * \fgdta_loc_obj_id{obj_id}
 * \param[in] comment The new comment
 *
 * \return \herr_t
 *
 * \details H5Oset_comment() sets the comment for the specified object
 *          to the contents of \p comment. Any previously existing comment
 *          is overwritten.
 *
 *          The target object is specified by an identifier, \p obj_id.
 *          If \p comment is the empty string or a null pointer, any existing
 *          comment message is removed from the object.
 *
 *          Comments should be relatively short, null-terminated, ASCII strings.
 *
 *          Comments can be attached to any object that has an object
 *          header. Datasets, groups, and committed (named) datatypes have
 *          object headers. Symbolic links do not have object headers.
 *
 *          If a comment is being added to an object attribute, this comment
 *          will be attached to the object to which the attribute belongs
 *          and not to the attribute itself.
 *
 * \todo Check: portal says deprecated but src is not; 
 * \todo Check I split history into version and since
 *
 * \version 1.8.11 Fortran subroutine introduced in this release.  
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oset_comment(hid_t obj_id, const char *comment);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Sets comment for specified object
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] name Name of the object whose comment is to be set or reset
 * \param[in] comment The new comment
 * \lapl_id
 *
 * \return \herr_t
 *
 * \details H5Oset_comment_by_name() sets the comment for the specified object
 *          to the contents of \p comment. Any previously existing comment
 *          is overwritten.
 * 
 *          The target object is specified by \p loc_id and \p name.
 *          \p loc_id can specify any object in the file. 
 *          \p name can be one of the following:
 *          <ul>
 *          <li> The name of the object specified as a path relative to \p loc_id 
 *          <li> An absolute name of the object, starting from \c /, the file’s root group
 *          <li> A dot (\c .), if \p loc_id fully specifies the object
 *          </ul>
 *          If \p comment is the empty string or a null pointer, any existing
 *          comment message is removed from the object.
 *
 *          Comments should be relatively short, null-terminated, ASCII strings.
 *
 *          Comments can be attached to any object that has an object
 *          header. Datasets, groups, and committed (named) datatypes have
 *          object headers. Symbolic links do not have object headers.
 *
 *          If a comment is being added to an object attribute, this comment
 *          will be attached to the object to which the attribute belongs
 *          and not to the attribute itself.
 *
 *          \p lapl_id contains a link access property list identifier. A
 *          link access property list can come into play when traversing
 *          links to access an object.
 *
 * \todo Check: portal says deprecated but src is not; 
 * \todo Check I split history into version and since
 *
 * \version 1.8.11 Fortran subroutine introduced in this release.  
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oset_comment_by_name(hid_t loc_id, const char *name, 
    const char *comment, hid_t lapl_id);


/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Retrieves comment for specified object
 *
 * \fgdta_loc_obj_id{obj_id}
 * \param[out] comment The comment
 * \param[in] bufsize Anticipated required size of the comment buffer
 *
 * \return Upon success, returns the number of characters in the
 *         comment, not including the \c NULL terminator, or zero (\c 0) if
 *         the object has no comment. The value returned may be larger
 *         than \p bufsize. Otherwise returns a negative value.
 *
 * \details H5Oget_comment() retrieves the comment for the specified object in
 *          the buffer \p comment.
 *           
 *          The target object is specified by an identifier, \p object_id.
 *
 *          The size in bytes of the buffer \p comment, including the \c NULL
 *          terminator, is specified in \p bufsize. If \p bufsize is unknown,
 *          a preliminary H5Oget_comment() call with the pointer \p comment
 *          set to \c NULL will return the size of the comment <em>without</em>
 *          the \c NULL terminator.
 *
 *          If \p bufsize is set to a smaller value than described above,
 *          only \p bufsize bytes of the comment, without a \c NULL terminator,
 *          are returned in \p comment.
 *
 *          If an object does not have a comment, the empty string is
 *          returned in \p comment.
 *
 * \todo Check I split history into version and since
 *
 * \version 1.8.11 Fortran subroutine introduced in this release.  
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL ssize_t H5Oget_comment(hid_t obj_id, char *comment, size_t bufsize); 
H5_DLL ssize_t H5Oget_comment_by_name(hid_t loc_id, const char *name,
    char *comment, size_t bufsize, hid_t lapl_id);


H5_DLL herr_t H5Ovisit3(hid_t obj_id, H5_index_t idx_type, H5_iter_order_t order,
    H5O_iterate2_t op, void *op_data, unsigned fields);
H5_DLL herr_t H5Ovisit_by_name3(hid_t loc_id, const char *obj_name,
    H5_index_t idx_type, H5_iter_order_t order, H5O_iterate2_t op,
    void *op_data, unsigned fields, hid_t lapl_id);

H5_DLL herr_t H5Oclose(hid_t object_id);
H5_DLL herr_t H5Oflush(hid_t obj_id);
H5_DLL herr_t H5Orefresh(hid_t oid);
H5_DLL herr_t H5Odisable_mdc_flushes(hid_t object_id);
H5_DLL herr_t H5Oenable_mdc_flushes(hid_t object_id);
H5_DLL herr_t H5Oare_mdc_flushes_disabled(hid_t object_id, hbool_t *are_disabled);
H5_DLL herr_t H5Otoken_cmp(hid_t loc_id, const H5O_token_t *token1, const H5O_token_t *token2,
    int *cmp_value);
H5_DLL herr_t H5Otoken_to_str(hid_t loc_id, const H5O_token_t *token, char **token_str);
H5_DLL herr_t H5Otoken_from_str(hid_t loc_id, const char *token_str, H5O_token_t *token);

/* The canonical 'undefined' token value */
#define H5O_TOKEN_UNDEF (H5OPEN H5O_TOKEN_UNDEF_g)
H5_DLLVAR const H5O_token_t H5O_TOKEN_UNDEF_g;

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */

/* Deprecated flags for earlier versions of H5Oget_info* */
#define H5O_INFO_HDR            0x0008u         /* Fill in the hdr field */
#define H5O_INFO_META_SIZE      0x0010u         /* Fill in the meta_size field */
#undef H5O_INFO_ALL
#define H5O_INFO_ALL            (H5O_INFO_BASIC | H5O_INFO_TIME | H5O_INFO_NUM_ATTRS | H5O_INFO_HDR | H5O_INFO_META_SIZE)

/* Typedefs */

/** A struct that's part of the H5G_stat_t structure (deprecated) */
//! [H5O_stat_t_snip]
typedef struct H5O_stat_t {
    hsize_t size;               /* Total size of object header in file */
    hsize_t free;               /* Free space within object header */
    unsigned nmesgs;            /* Number of object header messages */
    unsigned nchunks;           /* Number of object header chunks */
} H5O_stat_t;
//!< [H5O_stat_t_snip]

//! [H5O_info1_t_snip]

/* Information struct for object */
/* (For H5Oget_info/H5Oget_info_by_name/H5Oget_info_by_idx versions 1 & 2) */
typedef struct H5O_info1_t {
    unsigned long 	fileno;		/* File number that object is located in */
    haddr_t 		addr;		/* Object address in file	*/
    H5O_type_t 		type;		/* Basic object type (group, dataset, etc.) */
    unsigned 		rc;		/* Reference count of object    */
    time_t		atime;		/* Access time			*/
    time_t		mtime;		/* Modification time		*/
    time_t		ctime;		/* Change time			*/
    time_t		btime;		/* Birth time			*/
    hsize_t 		num_attrs;	/* # of attributes attached to object */
    H5O_hdr_info_t      hdr;            /* Object header information */
    /* Extra metadata storage for obj & attributes */
    struct {
        H5_ih_info_t   obj;             /* v1/v2 B-tree & local/fractal heap for groups, B-tree for chunked datasets */
        H5_ih_info_t   attr;            /* v2 B-tree & heap for attributes */
    } meta_size;
} H5O_info1_t;

//! [H5O_info1_t_snip]

/* Prototype for H5Ovisit/H5Ovisit_by_name() operator (versions 1 & 2) */
typedef herr_t (*H5O_iterate1_t)(hid_t obj, const char *name, const H5O_info1_t *info,
    void *op_data);


/* Function prototypes */

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Opens an object using its address within an HDF5 file.
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] addr Object's address in the file
 *
 * \return \hid_tv{object}
 *
 * \deprecated As of HDF5-1.12 this function has been deprecated in favor of
 *             the function H5Oopen_by_token().
 *
 * \details H5Open_by_addr() opens a group, dataset, or committed (named) datatype using its 
 *          address within an HDF5 file, \p addr. The resulting opened object is identical to 
 *          an object opened with H5Oopen() and should be closed with H5Oclose() or an 
 *          object-type-specific closing function (such as H5Gclose()) when no longer needed.
 *
 *          \p loc_id is a location identifier in the file.
 *
 *          The object’s address within the file, \p addr, is the byte offset of the first byte 
 *          of the object header from the beginning of the HDF5 file space, i.e., from the 
 *          beginning of the super block (see the “HDF5 Storage Model” section of the The 
 *          HDF5 Data Model and File Structure chapter of the <em>HDF5 User's Guide</em>.)
 *
 *          \p addr can be obtained via either of two function calls. H5Gget_objinfo() returns
 *          the object’s address in the \c objno field of the \ref H5G_stat_t \c struct; 
 *          H5Lget_info() returns the address in the \c address field of the \ref H5L_info_t \c struct.
 *
 *          The address of the HDF5 file on a physical device has no effect on H5Oopen_by_addr(), 
 *          nor does the use of any file driver. As stated above, the object address is its 
 *          offset within the HDF5 file; HDF5’s file drivers will transparently map this to an 
 *          address on a storage device.
 *
 * \warning This function must be used with care! 
 * \warning Improper use can lead to inaccessible data, wasted space in the file, 
 *          or <b><em>file corruption</em></b>. 
 * \warning This function is dangerous if called on an invalid address. The risk can be safely 
 *          overcome by retrieving the object address with H5Gget_objinfo() or H5Lget_info()
 *          immediately before calling H5Oopen_by_addr(). The immediacy of the operation can be 
 *          important; if time has elapsed and the object has been deleted from the file, 
 *          the address will be invalid and file corruption can result.
 *
 * \todo Check detail: reference to the the user guide; 
 * \todo Check detail: correction: H5L_infolinfo_t struct; warning: correction: H5Lget_linkinfo
 * \todo Check I split history into version and since
 *
 * \version 1.8.4 Fortran subroutine added in this release.
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Oopen_by_addr(hid_t loc_id, haddr_t addr);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Retrieves the metadata for an object specified by an identifier
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[out] oinfo Buffer in which to return object information
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.12 this function has been deprecated in favor of
 *             the function H5Oget_info3() or the macro #H5Oget_info.
 *
 * \details H5Oget_info1() specifies an object by its identifier, \p loc_id , and 
 *          retrieves the metadata describing that object in \p oinfo , 
 *          defined as a \c struct of type \ref H5O_info1_t :
 *
 *          \snippet this H5O_info1_t_snip
 *
 *          Note the following about \ref H5O_info1_t :
 *          - Of the four time fields (\c atime, \c mtime, \c ctime, and \c btime) only \c ctime  has been implemented.
 *          - The \c atime value is the last time the object was read or written.
 *          - The \c mtime value is the last time the raw data in the object was changed.
 *          - The \c ctime value is the last time the metadata for the object was changed.
 *          - The \c btime value is the time the object was created.
 *          - The fields nested in the \c meta_size field are for internal library use only.
 *          .
 *
 *          The \ref H5O_type_t \c enum indicates the object type and 
 *          is defined in H5Opublic.h as follows:
 *          \snippet this H5O_type_t_snip
 *
 *          Note that the object retrieved as indicated by \p loc_id
 *          refers only to the types specified by \ref H5O_type_t.
 *
 *          An \ref H5O_hdr_info_t \c struct holds object header metadata and is 
 *          defined in H5Opublic.h as follows:
 *          \snippet this H5O_hdr_info_t_snip
 *
 *          Valid values for the \c version field are \ref H5O_VERSION_1 and \ref H5O_VERSION_2. 
 *          Version 2 of the object header is smaller and more efficient than version 1.
 *
 *          Please be aware that the information held by \ref H5O_hdr_info_t may only be useful to 
 *          developers with extensive HDF5 experience.
 *
 * \note If you are iterating through a lot of different objects to
 *       retrieve information via the H5Oget_info() family of routines,
 *       you may see memory building up. This can be due to memory
 *       allocation for metadata such as object headers and messages
 *       when the iterated objects are put into the metadata cache.
 * \note
 *       If the memory buildup is not desirable, you can configure a
 *       smaller cache via H5Fset_mdc_config() or set the file access
 *       property list via H5Pset_mdc_config(). A smaller sized cache
 *       will force metadata entries to be evicted from the cache,
 *       thus freeing the memory associated with the entries.
 *
 * \todo Check H5O_VERSION_1 and H5O_VERSION_2: highlighted?
 *
 * \par Version
 * <table>
 *  <tr>
 *      <td>1.10.5</td>
 *      <td>The macro #H5Oget_info was removed and the 
 *          function H5Oget_info1() was copied to H5Oget_info().</td>
 *  </tr>
 *  <tr>
 *      <td>1.10.3</td>
 *      <td>Function H5Oget_info() was copied to H5Oget_info1(), and 
 *          the macro #H5Oget_info was created.</td>
 *  </tr>
 *  <tr>
 *      <td>1.8.15</td>
 *      <td>Added a note about the valid values for the \c version field in 
 *          the \ref H5O_hdr_info_t structure.
 *      <\td>
 *  </tr>
 *  <tr>
 *      <td>1.8.11</td>
 *      <td>Fortran subroutine introduced in this release.</td>
 *  </tr>
 *  <tr>
 *      <td>1.8.10</td>
 *      <td>Added \ref H5O_type_t structure to the Description section.</td>
 *  </tr>
 *  <tr>
 *      <td></td> 
 *      <td>Separated \ref H5O_hdr_info_t structure from \ref H5O_info_t in the Description section.</td>
 *  </tr>
 *  <tr>
 *      <td></td> 
 *      <td>Clarified the definition and implementation of the time fields.</td> 
 *  </tr>
 *  <tr>
 *      <td>1.8.0</td>
 *      <td>Function introduced in this release.</td>
 *  </tr>
 * </table>
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oget_info1(hid_t loc_id, H5O_info1_t *oinfo);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Retrieves the metadata for an object, identifying the object 
 *        by location and relative name
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] name Name of group, relative to \p loc_id
 * \param[out] oinfo Buffer in which to return object information
 * \lapl_id
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.12 this function has been deprecated in favor of
 *             the function H5Oget_info_by_name2() or the macro #H5Oget_info_by_name.
 *
 * \details H5Oget_info_by_name1() specifies an object’s location and name, \p loc_id 
 *          and \p name, respectively, and retrieves the metadata describing that object
 *          in \p oinfo, an \ref H5O_info1_t \c struct.
 *          
 *          The \c struct \ref H5O_info1_t is defined in H5Opublic.h and described
 *          in the H5Oget_info1() function entry.
 *          
 *          The link access property list, \p lapl_id, is not currently used;
 *          it should be passed in as #H5P_DEFAULT.
 *
 * \todo Check: exchange history 1.8.8 and 1.8.0
 *
 * \par Version
 * <table>
 *  <tr>
 *      <td>1.10.5</td>
 *      <td>The macro #H5Oget_info_by_name was removed and the function 
 *          H5Oget_info_by_name1() was copied to H5Oget_info_by_name().</td>
 *  </tr>
 *  <tr>
 *      <td>1.10.3</td>
 *      <td>Function H5Oget_info_by_name() was copied to H5Oget_info_by_name1() and 
 *          the macro #H5Oget_info_by_name was created.</td>
 *  </tr>
 *  <tr>
 *      <td>1.8.8</td>
 *      <td>Fortran 2003 subroutine and \ref h5o_info_t derived type introduced 
 *          in this release.</td>
 *  </tr>
 *  <tr>
 *      <td>1.8.0</td>
 *      <td>C function introduced in this release.</td>
 *  </tr>
 * </table>
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oget_info_by_name1(hid_t loc_id, const char *name, H5O_info1_t *oinfo,
    hid_t lapl_id);


/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Retrieves the metadata for an object, identifying the object 
 *        by an index position
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] group_name Name of group in which object is located
 * \idx_type
 * \order
 * \param[in] n Position within the index
 * \param[out] oinfo Buffer in which to return object information
 * \param[in] fields Flags specifying the fields to include in \p oinfo
 * \lapl_id
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.12 this function has been deprecated in favor of
 *             the function H5Oget_info_by_idx3() or the macro H5Oget_info_by_idx().
 *
 * \details H5Oget_info_by_idx1() retrieves the metadata describing an
 *          object in the \c struct \p oinfo, as specified by the location,
 *          \p loc_id, group name, \p group_name, the index by which objects
 *          in that group are tracked, \p idx_type, the order by which the
 *          index is to be traversed, \p order, and an object’s position
 *          \p n within that index.
 *
 *          If \p loc_id fully specifies the group in which the object resides,
 *          \p group_name can be a dot (\c .).
 *
 *          \p idx_type is of type \ref H5_index_t, defined in H5public.h as:
 *          \snippet H5public.h H5_index_t_snip
 *
 *          \p order is of type \ref H5_iter_order_t defined in H5public.h as:
 *          \snippet H5public.h H5_iter_order_t_snip
 *
 *          \p oinfo, in which the object information is returned, is a \c struct of
 *          type \ref H5O_info1_t .
 *          \snippet this H5O_info1_t_snip
 *
 *          The link access property list, \c lapl_id, is not currently used; 
 *          it should be passed in as #H5P_DEFAULT.
 *
 * \todo Check: I modify description for several parameters
 *
 *
 * \par Version
 * <table>
 *  <tr>
 *      <td>1.10.5</td>
 *      <td>The macro #H5Oget_info_by_idx was removed and the function 
 *          H5Oget_info_by_idx() was copied to H5Oget_info_by_idx1().</td>
 *  </tr>
 *  <tr>
 *      <td>1.10.3</td>
 *      <td>Function H5Oget_info_by_idx() was copied to H5Oget_info_by_idx1() and 
 *          the macro #H5Oget_info_by_idx was created.</td>
 *  </tr>
 *  <tr>
 *      <td>1.8.11</td>
 *      <td>Fortran subroutine introduced in this release.</td>
 *  </tr>
 *  <tr>
 *      <td>1.8.0</td>
 *      <td>Function introduced in this release.</td>
 *  </tr>
 * </table>
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oget_info_by_idx1(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5O_info1_t *oinfo,
    hid_t lapl_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Retrieves the metadata for an object specified by an identifier
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[out] oinfo Buffer in which to return object information
 * \param[in] fields Flags specifying the fields to include in \p oinfo
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.12 this function has been deprecated in favor of
 *             the function H5Oget_info3() or the macro H5Oget_info().
 *
 * \details H5Oget_info2() specifies an object by its identifier, \p loc_id , and 
 *          retrieves the metadata describing that object in \p oinfo , an \ref H5O_info1_t \c struct.
 *          This \c struct type is described in H5Oget_info1().
 *
 *          The \p fields parameter contains flags to determine which fields will be filled in 
 *          the \ref H5O_info1_t \c struct returned in \p oinfo. 
 *          These flags are defined in the H5Opublic.h file:
 *
 *          \obj_info_fields
 *
 * \note If you are iterating through a lot of different objects to
 *       retrieve information via the H5Oget_info() family of routines,
 *       you may see memory building up. This can be due to memory
 *       allocation for metadata such as object headers and messages
 *       when the iterated objects are put into the metadata cache.
 * \note
 *       If the memory buildup is not desirable, you can configure a
 *       smaller cache via H5Fset_mdc_config() or set the file access
 *       property list via H5Pset_mdc_config(). A smaller sized cache
 *       will force metadata entries to be evicted from the cache,
 *       thus freeing the memory associated with the entries.
 *
 * \since 1.10.3
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oget_info2(hid_t loc_id, H5O_info1_t *oinfo, unsigned fields);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Retrieves the metadata for an object, identifying the object 
 *        by location and relative name
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] name Name of group, relative to \p loc_id
 * \param[out] oinfo Buffer in which to return object information
 * \param[in] fields Flags specifying the fields to include in \p oinfo
 * \lapl_id
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.12 this function has been deprecated in favor of
 *             the function H5Oget_info_by_name3() or the macro H5Oget_info_by_name().
 *
 * \details H5Oget_info_by_name2() specifies an object’s location and name, \p loc_id and
 *          \p name, respectively, and retrieves the metadata describing
 *          that object in \p oinfo, an \ref H5O_info1_t \c struct.
 *          
 *          The \c struct \ref H5O_info1_t is defined in H5Opublic.h and described
 *          in the H5Oget_info1() function entry.
 *
 *          The \p fields parameter contains flags to determine which fields
 *          will be filled in in the \ref H5O_info1_t \c struct returned in
 *          \p oinfo. These flags are defined in the H5Opublic.h file:
 *
 *          \obj_info_fields
 *
 *          The link access property list, \p lapl_id, is not currently used; 
 *          it should be passed in as #H5P_DEFAULT.
 *
 * \todo Check parameter loc_id description; check object_name v.s. name
 *
 * \since 1.10.3
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oget_info_by_name2(hid_t loc_id, const char *name, H5O_info1_t *oinfo,
    unsigned fields, hid_t lapl_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5O
 *
 * \brief Retrieves the metadata for an object, identifying the object 
 *        by an index position
 *
 * \fgdta_loc_obj_id{loc_id}
 * \param[in] group_name Name of group in which object is located
 * \idx_type
 * \order
 * \param[in]  n Position within the index
 * \param[out] oinfo Buffer in which to return object information
 * \param[in] fields Flags specifying the fields to include in \p oinfo
 * \lapl_id
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.12 this function is deprecated in favor of
 *             the function H5Oget_info_by_idx3() or the macro H5Oget_info_by_idx().
 *
 * \details H5Oget_info_by_idx2() retrieves the metadata describing an
 *          object in the \c struct \p oinfo, as specified by the location,
 *          \p loc_id, group name, \p group_name, the index by which objects
 *          in that group are tracked, \p idx_type, the order by which the
 *          index is to be traversed, \p order, and an object’s position
 *          \p n within that index.
 *
 *          \p oinfo, in which the object information is returned, is a \c struct of
 *          type \ref H5O_info1_t.  This and other \c struct types used 
 *          by H5Oget_info_by_idx2() are described in  H5Oget_info_by_idx1().
 *
 *          If \p loc_id fully specifies the group in which the object resides,
 *          i\p group_name can be a dot (\c .).
 *
 *          The \p fields parameter contains flags to determine which fields will be
 *          filled in the \ref H5O_info1_t \c struct returned in \p oinfo. 
 *          These flags are defined in the H5Opublic.h file:
 *          \obj_info_fields
 *
 *          The link access property list, \c lapl_id, is not currently used; 
 *          it should be passed in as #H5P_DEFAULT.
 *
 * \todo Check: I modify description for several parameters
 *
 *
 * \since 1.10.3
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Oget_info_by_idx2(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5O_info1_t *oinfo,
    unsigned fields, hid_t lapl_id);
H5_DLL herr_t H5Ovisit1(hid_t obj_id, H5_index_t idx_type, H5_iter_order_t order,
    H5O_iterate1_t op, void *op_data);


H5_DLL herr_t H5Ovisit_by_name1(hid_t loc_id, const char *obj_name,
    H5_index_t idx_type, H5_iter_order_t order, H5O_iterate1_t op,
    void *op_data, hid_t lapl_id);
H5_DLL herr_t H5Ovisit2(hid_t obj_id, H5_index_t idx_type, H5_iter_order_t order,
    H5O_iterate1_t op, void *op_data, unsigned fields);
H5_DLL herr_t H5Ovisit_by_name2(hid_t loc_id, const char *obj_name,
    H5_index_t idx_type, H5_iter_order_t order, H5O_iterate1_t op,
    void *op_data, unsigned fields, hid_t lapl_id);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* _H5Opublic_H */

