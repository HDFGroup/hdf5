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
#define H5O_COPY_ALL                    (0x003Fu)   /* All object copying flags (for internal checking) */

/* Flags for shared message indexes.
 * Pass these flags in using the mesg_type_flags parameter in
 * H5P_set_shared_mesg_index.
 * (Developers: These flags correspond to object header message type_ids,
 * but we need to assign each kind of message to a different bit so that
 * one index can hold multiple types.)
 */
#define H5O_MESG_NONE_FLAG     0x0000          /* No shared messages */
#define H5O_MESG_SDSPACE_FLAG  0x0001          /* Simple Dataspace Message.  */
#define H5O_MESG_DTYPE_FLAG    0x0002          /* Datatype Message.  */
#define H5O_MESG_FILL_FLAG     0x0004          /* Fill Value Message. */
#define H5O_MESG_PLINE_FLAG    0x0008          /* Filter pipeline message.  */
#define H5O_MESG_ATTR_FLAG     0x0010          /* Attribute Message.  */
#define H5O_MESG_ALL_FLAG      (H5O_MESG_SDSPACE_FLAG | H5O_MESG_DTYPE_FLAG | H5O_MESG_FILL_FLAG | H5O_MESG_PLINE_FLAG | H5O_MESG_ATTR_FLAG)

/* Maximum shared message values.  Number of indexes is 8 to allow room to add
 * new types of messages.
 */
#define H5O_SHMESG_MAX_NINDEXES 8
#define H5O_SHMESG_MAX_LIST_SIZE 5000

/*******************/
/* Public Typedefs */
/*******************/
/* A struct that's part of the H5G_stat_t routine (deprecated) */
typedef struct H5O_stat_t {
    hsize_t size;               /* Total size of object header in file */
    hsize_t free;               /* Free space within object header */
    unsigned nmesgs;            /* Number of object header messages */
    unsigned nchunks;           /* Number of object header chunks */
} H5O_stat_t;

/* Types of objects in file */
typedef enum H5O_type_t {
    H5O_TYPE_UNKNOWN = -1,	/* Unknown object type		*/
    H5O_TYPE_GROUP,	        /* Object is a group		*/
    H5O_TYPE_DATASET,		/* Object is a dataset		*/
    H5O_TYPE_NAMED_DATATYPE 	/* Object is a named data type	*/
} H5O_type_t;

/* Information struct for object (for H5Oget_info/H5Oget_info_by_idx) */
typedef struct H5O_info_t {
    unsigned long 	fileno;		/* File number that object is located in */
    haddr_t 		addr;		/* Object address in file	*/
    H5O_type_t 		type;		/* Basic object type (group, dataset, etc.) */
    unsigned 		rc;		/* Reference count of object    */
    time_t		atime;		/* Access time			*/
    time_t		mtime;		/* Modification time		*/
    time_t		ctime;		/* Change time			*/
    time_t		btime;		/* Birth time			*/
    hsize_t 		num_attrs;	/* # of attributes attached to object */
    struct {
        unsigned version;		/* Version number of header format in file */
        unsigned nmesgs;		/* Number of object header messages */
        unsigned nchunks;		/* Number of object header chunks */
        hsize_t hdr_size;		/* Total size of object header in file */
        hsize_t meta_space;		/* Space within header for object header metadata information */
        hsize_t mesg_space;		/* Space within header for actual message information */
        hsize_t free_space;		/* Free space within object header */
        uint64_t msg_present;		/* Flags to indicate presence of message type in header */
        uint64_t msg_shared;		/* Flags to indicate message type is shared in header */
    } hdr;
    hsize_t             meta_size;      /* Size of additional metadata for an object */
                                        /* (B-tree & heap for groups, B-tree for chunked dataset, etc.) */
} H5O_info_t;

/* Typedef for message creation indexes */
typedef uint32_t H5O_msg_crt_idx_t;


/********************/
/* Public Variables */
/********************/


#ifdef __cplusplus
extern "C" {
#endif

/*********************/
/* Public Prototypes */
/*********************/
H5_DLL hid_t H5Oopen(hid_t loc_id, const char *name, hid_t lapl_id);
H5_DLL hid_t H5Oopen_by_addr(hid_t loc_id, haddr_t addr);
H5_DLL hid_t H5Oopen_by_idx(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t lapl_id);
H5_DLL herr_t H5Oget_info(hid_t loc_id, const char *name, H5O_info_t *oinfo,
    hid_t lapl_id);
H5_DLL herr_t H5Oget_info_by_idx(hid_t loc_id, const char *group_name,
    H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5O_info_t *oinfo,
    hid_t lapl_id);
H5_DLL herr_t H5Oincr_refcount(hid_t object_id);
H5_DLL herr_t H5Odecr_refcount(hid_t object_id);
H5_DLL herr_t H5Ocopy(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
    const char *dst_name, hid_t ocpypl_id, hid_t lcpl_id);
H5_DLL herr_t H5Oclose(hid_t object_id);

#ifdef __cplusplus
}
#endif
#endif /* _H5Opublic_H */

