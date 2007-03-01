/* * * * * * * * * * * * * * * * * * * * * * *fs * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5Oprivate.h
 *			Aug  5 1997
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Object header private include file.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Oprivate_H
#define _H5Oprivate_H

/* Include the public header file for this API */
#include "H5Opublic.h"          /* Object header functions              */

/* Public headers needed by this file */
#include "H5Dpublic.h"          /* Dataset functions                    */
#include "H5Lpublic.h"		/* Link functions                       */
#include "H5Spublic.h"		/* Dataspace functions			*/

/* Private headers needed by this file */
#include "H5Fprivate.h"		/* File access				*/
#include "H5SLprivate.h"	/* Skip lists				*/
#include "H5Tprivate.h"		/* Datatype functions			*/
#include "H5Zprivate.h"         /* I/O pipeline filters			*/

/* Forward references of package typedefs */
typedef struct H5O_msg_class_t H5O_msg_class_t;
typedef struct H5O_t H5O_t;

/* Values used to create the shared message & attribute heaps */
/* (Note that these parameters have been tuned so that the resulting heap ID
 *      is exactly 8 bytes.  This is an efficient size as it can be stored
 *      directly in an 8 byte integer in memory, think carefully before changing it.
 *      -QAK)
 */
#define H5O_FHEAP_MAN_WIDTH                     4
#define H5O_FHEAP_MAN_START_BLOCK_SIZE          1024
#define H5O_FHEAP_MAN_MAX_DIRECT_SIZE           (64 * 1024)
#define H5O_FHEAP_MAN_MAX_INDEX                 40
#define H5O_FHEAP_MAN_START_ROOT_ROWS           1
#define H5O_FHEAP_CHECKSUM_DBLOCKS              TRUE
#define H5O_FHEAP_MAX_MAN_SIZE                  (4 * 1024)
#define H5O_FHEAP_ID_LEN                        8

/* Object header macros */
#define H5O_MESG_MAX_SIZE	65536	/*max obj header message size	     */
#define H5O_ALL		(-1)		/* Operate on all messages of type   */
#define H5O_FIRST	(-2)		/* Operate on first message of type  */

/* Flags needed when encoding messages */
#define H5O_MSG_FLAG_CONSTANT	0x01u
#define H5O_MSG_FLAG_SHARED	0x02u
#define H5O_MSG_FLAG_DONTSHARE	0x04u
#define H5O_MSG_FLAG_FAIL_IF_UNKNOWN 0x08u
#define H5O_MSG_FLAG_CURRENT 0x10u
#define H5O_MSG_FLAG_BITS	(H5O_MSG_FLAG_CONSTANT|H5O_MSG_FLAG_SHARED|H5O_MSG_FLAG_DONTSHARE)

/* Flags for updating messages */
#define H5O_UPDATE_TIME         0x01u

/* Hash value constants */
#define H5O_HASH_SIZE 32

/* ========= Object Creation properties ============ */
#define H5O_CRT_ATTR_MAX_COMPACT_NAME	"max compact attr"      /* Max. # of attributes to store compactly */
#define H5O_CRT_ATTR_MIN_DENSE_NAME	"min dense attr"	/* Min. # of attributes to store densely */
#define H5O_CRT_OHDR_FLAGS_NAME		"object header flags"	/* Object header flags */

/* ========= Object Copy properties ============ */
#define H5O_CPY_OPTION_NAME 		"copy object"           /* Copy options */

/* Fractal heap ID type for shared message & attribute heap IDs. */
typedef uint64_t H5O_fheap_id_t;

/* The object location information for an object */
typedef struct H5O_loc_t {
    H5F_t       *file;          /* File that object header is located within */
    haddr_t     addr;           /* File address of object header */
    hbool_t     holding_file;   /* True if this object header has incremented
                                 * its file's count of open objects. */
} H5O_loc_t;

/* Settings/flags for copying an object */
typedef struct H5O_copy_t {
    hbool_t copy_shallow;               /* Flag to perform shallow hierarchy copy */
    hbool_t expand_soft_link;           /* Flag to expand soft links */
    hbool_t expand_ext_link;            /* Flag to expand external links */
    hbool_t expand_ref;                 /* Flag to expand object references */
    hbool_t copy_without_attr;          /* Flag to not copy attributes */
    hbool_t preserve_null;              /* Flag to not delete NULL messages */
    int curr_depth;                     /* Current depth in hierarchy copied */
    int max_depth;                      /* Maximum depth in hierarchy to copy */
    H5SL_t *map_list;                   /* Skip list to hold address mappings */
} H5O_copy_t;

/* Header message IDs */
#define H5O_NULL_ID	0x0000          /* Null Message.  */
#define H5O_SDSPACE_ID	0x0001          /* Simple Dataspace Message.  */
#define H5O_LINFO_ID    0x0002          /* Link Info Message. */
#define H5O_DTYPE_ID	0x0003          /* Datatype Message.  */
#define H5O_FILL_ID     0x0004          /* Fill Value Message. (Old)  */
#define H5O_FILL_NEW_ID 0x0005          /* Fill Value Message. (New)  */
#define H5O_LINK_ID     0x0006          /* Link Message. */
#define H5O_EFL_ID	0x0007          /* External File List Message  */
#define H5O_LAYOUT_ID	0x0008          /* Data Storage Layout Message.  */
#define H5O_BOGUS_ID	0x0009          /* "Bogus" Message.  */
#define H5O_GINFO_ID	0x000a          /* Group Info Message.  */
#define H5O_PLINE_ID	0x000b          /* Filter pipeline message.  */
#define H5O_ATTR_ID	0x000c          /* Attribute Message.  */
#define H5O_NAME_ID	0x000d          /* Object name message.  */
#define H5O_MTIME_ID	0x000e          /* Modification time message. (Old)  */
#define H5O_SHMESG_ID   0x000f          /* Shared message "SOHM" table. */
#define H5O_CONT_ID	0x0010          /* Object header continuation message.  */
#define H5O_STAB_ID	0x0011          /* Symbol table message.  */
#define H5O_MTIME_NEW_ID 0x0012         /* Modification time message. (New)  */
#define H5O_BTREEK_ID   0x0013          /* v1 B-tree 'K' values message.  */
#define H5O_DRVINFO_ID  0x0014          /* Driver info message.  */


/* Shared object message flags.
 * Shared objects can be committed, in which case the shared message contains
 * the location of the object header that holds the message, or shared in the
 * heap, in which case the shared message holds their heap ID.
 */
#define H5O_NOT_SHARED 0
#define H5O_SHARED_IN_HEAP_FLAG 0x01
#define H5O_COMMITTED_FLAG 0x02

#define H5O_IS_SHARED(F)        (((F) & (H5O_SHARED_IN_HEAP_FLAG | H5O_COMMITTED_FLAG)) ? TRUE : FALSE)

/*
 * Shared object header message info.
 * This needs to go first because other messages can be shared and
 * include a H5O_shared_t struct
 * The oloc shouldn't ever be holding open a file; if it ever is (if
 * H5O_loc_hold_file was ever called on it) it won't be closed properly,
 * since shared messages don't close their olocs.
 */
typedef struct H5O_shared_t {
    unsigned flags;             /* flags describing how message is shared */
    union {
        H5O_loc_t	oloc;		/* object location info		     */
        H5O_fheap_id_t  heap_id;        /* ID within the SOHM heap           */
    } u;
} H5O_shared_t;


/*
 * Link Info Message.
 * (Contains dynamic information about links in a group)
 * (Data structure in memory)
 * (if the fields in this struct are changed, remember to change the default
 *      link info structure in src/H5Gprivate.h - QAK)
 * (if the fields in this struct are changed, also look at the code that
 *      creates intermediate groups in src/H5Gtraverse.c - QAK)
 */
typedef struct H5O_linfo_t {
    /* (creation order info) */
    hbool_t     index_corder;           /* Are creation order values indexed on links? */
    int64_t     min_corder;             /* Current min. creation order value for group */
    int64_t     max_corder;             /* Current max. creation order value for group */
    haddr_t     corder_bt2_addr;        /* Address of v2 B-tree for indexing creation order values of links */

    /* (storage management info) */
    hsize_t     nlinks;                 /* Number of links in the group      */
    haddr_t     link_fheap_addr;        /* Address of fractal heap for storing "dense" links */
    haddr_t     name_bt2_addr;          /* Address of v2 B-tree for indexing names of links */
} H5O_linfo_t;

/*
 * New Fill Value Message.
 * (Data structure in memory for both "old" and "new" fill value messages)
 *
 * The new fill value message is fill value plus
 * space allocation time, fill value writing time, whether fill
 * value is defined, and the location of the message if it's shared
 */

typedef struct H5O_fill_t {
    H5O_shared_t        sh_loc;         /* Shared message info (must be first) */

    H5T_t		*type;		/*type. Null implies same as dataset */
    ssize_t		size;		/*number of bytes in the fill value  */
    void		*buf;		/*the fill value		     */
    H5D_alloc_time_t	alloc_time;	/* time to allocate space	     */
    H5D_fill_time_t	fill_time;	/* time to write fill value	     */
    hbool_t		fill_defined;   /* whether fill value is defined     */
} H5O_fill_t;

/*
 * Link message.
 * (Data structure in memory)
 */
typedef struct H5O_link_hard_t {
    haddr_t	addr;			/* Object header address */
} H5O_link_hard_t;

typedef struct H5O_link_soft_t {
    char	*name;			/* Destination name */
} H5O_link_soft_t;

typedef struct H5O_link_ud_t {
    void	*udata;			/* Opaque data supplied by the user */
    size_t       size;                  /* Size of udata */
} H5O_link_ud_t;

typedef struct H5O_link_t {
    H5L_type_t  type;                   /* Type of link */
    hbool_t     corder_valid;           /* Creation order for link is valid (not stored) */
    int64_t     corder;                 /* Creation order for link (stored if it's valid) */
    H5T_cset_t  cset;                   /* Character set of link name	*/
    char	*name;			/* Link name */
    union {
        H5O_link_hard_t hard;           /* Information for hard links */
        H5O_link_soft_t soft;           /* Information for soft links */
        H5O_link_ud_t ud;               /* Information for user-defined links */
    } u;
} H5O_link_t;

/*
 * External File List Message
 * (Data structure in memory)
 */
#define H5O_EFL_ALLOC		16	/*number of slots to alloc at once   */
#define H5O_EFL_UNLIMITED	H5F_UNLIMITED /*max possible file size	     */

typedef struct H5O_efl_entry_t {
    size_t	name_offset;		/*offset of name within heap	     */
    char	*name;			/*malloc'd name			     */
    off_t	offset;			/*offset of data within file	     */
    hsize_t	size;			/*size allocated within file	     */
} H5O_efl_entry_t;

typedef struct H5O_efl_t {
    haddr_t	heap_addr;		/*address of name heap		     */
    size_t	nalloc;			/*number of slots allocated	     */
    size_t	nused;			/*number of slots used		     */
    H5O_efl_entry_t *slot;		/*array of external file entries     */
} H5O_efl_t;

/*
 * Data Layout Message.
 * (Data structure in memory)
 */
#define H5O_LAYOUT_NDIMS	(H5S_MAX_RANK+1)

typedef struct H5O_layout_contig_t {
    haddr_t	addr;			/* File address of data              */
    hsize_t     size;                   /* Size of data in bytes             */
} H5O_layout_contig_t;

typedef struct H5O_layout_chunk_t {
    haddr_t	addr;			/* File address of B-tree            */
    unsigned	ndims;			/* Num dimensions in chunk           */
    size_t	dim[H5O_LAYOUT_NDIMS];	/* Size of chunk in elements         */
    size_t      size;                   /* Size of chunk in bytes            */
    H5RC_t     *btree_shared;           /* Ref-counted info for B-tree nodes */
} H5O_layout_chunk_t;

typedef struct H5O_layout_compact_t {
    hbool_t     dirty;                  /* Dirty flag for compact dataset    */
    size_t      size;                   /* Size of buffer in bytes           */
    void        *buf;                   /* Buffer for compact dataset        */
} H5O_layout_compact_t;

typedef struct H5O_layout_t {
    H5D_layout_t type;			/* Type of layout                    */
    unsigned version;                   /* Version of message                */
    union {
        H5O_layout_contig_t contig;     /* Information for contiguous layout */
        H5O_layout_chunk_t chunk;       /* Information for chunked layout    */
        H5O_layout_compact_t compact;   /* Information for compact layout    */
    } u;
} H5O_layout_t;

/* Enable reading/writing "bogus" messages */
/* #define H5O_ENABLE_BOGUS */

#ifdef H5O_ENABLE_BOGUS
/*
 * "Bogus" Message.
 * (Data structure in memory)
 */
#define H5O_BOGUS_VALUE         0xdeadbeef
typedef struct H5O_bogus_t {
    unsigned u;                         /* Hold the bogus info */
} H5O_bogus_t;
#endif /* H5O_ENABLE_BOGUS */

/*
 * Group info message.
 * (Contains constant information about a group)
 * (Data structure in memory)
 * (if the fields in this struct are changed, remember to change the default
 *      group info structure in src/H5Gprivate.h - QAK)
 */
typedef struct H5O_ginfo_t {
    /* "Old" format group info (not stored) */
    uint32_t    lheap_size_hint;        /* Local heap size hint              */

    /* "New" format group info (stored) */
    /* (creation order info) */
    hbool_t     track_corder;           /* Are creation order values tracked on links? */

    /* (storage management info) */
    uint16_t	max_compact;		/* Maximum # of compact links        */
    uint16_t	min_dense;		/* Minimum # of "dense" links        */

    /* (initial object header size info) */
    uint16_t	est_num_entries;	/* Estimated # of entries in group   */
    uint16_t	est_name_len;		/* Estimated length of entry name    */
} H5O_ginfo_t;

/*
 * Filter pipeline message.
 * (Data structure in memory)
 */
typedef struct H5O_pline_t {
    H5O_shared_t        sh_loc;         /* Shared message info (must be first) */

    size_t	nalloc;			/*num elements in `filter' array     */
    size_t	nused;			/*num filters defined		     */
    H5Z_filter_info_t *filter;		/*array of filters		     */
} H5O_pline_t;

/*
 * Object name message.
 * (Data structure in memory)
 */

typedef struct H5O_name_t {
    char	*s;			/*ptr to malloc'd memory	     */
} H5O_name_t;

/*
 * Object header continuation message.
 * (Data structure in memory)
 */

typedef struct H5O_cont_t {
    haddr_t	addr;			/*address of continuation block	     */
    size_t	size;			/*size of continuation block	     */

    /* the following field(s) do not appear on disk */
    unsigned	chunkno;		/*chunk this mesg refers to	     */
} H5O_cont_t;

/*
 * Symbol table message.
 * (Data structure in memory)
 */
typedef struct H5O_stab_t {
    haddr_t	btree_addr;		/*address of B-tree		     */
    haddr_t	heap_addr;		/*address of name heap		     */
} H5O_stab_t;

/*
 * Shared message table message
 * Information about file-wide shared message table, stored in superblock
 * extension
 * (Data structure in memory)
 */
typedef struct H5O_shmesg_table_t {
    haddr_t		addr;	        /*file address of SOHM table */
    unsigned		version;	/*SOHM table version number */
    unsigned		nindexes;	/*number of indexes in the table */
} H5O_shmesg_table_t;

/*
 * v1 B-tree 'K' value message
 * Information about file-wide non-default v1 B-tree 'K' values, stored in
 * superblock extension
 * (Data structure in memory)
 */
typedef struct H5O_btreek_t {
    unsigned        btree_k[H5B_NUM_BTREE_ID];  /* B-tree internal node 'K' values */
    unsigned        sym_leaf_k;                 /* Symbol table leaf node's 'K' value */
} H5O_btreek_t;

/*
 * Driver info message
 * Information about driver info, stored in superblock extension
 * (Data structure in memory)
 */
typedef struct H5O_drvinfo_t {
    char                name[9];                /* Driver name */
    size_t		len;                    /* Length of encoded buffer */
    uint8_t            *buf;                    /* Buffer for encoded info */
} H5O_drvinfo_t;


/* Typedef for iteration operations */
typedef herr_t (*H5O_operator_t)(const void *mesg/*in*/, unsigned idx,
    void *operator_data/*in,out*/);

/* Forward declarations for prototype arguments */
struct H5P_genplist_t;
struct H5SL_t;
struct H5O_t;

/* Object header routines */
H5_DLL herr_t H5O_init(void);
H5_DLL herr_t H5O_create(H5F_t *f, hid_t dxpl_id, size_t size_hint,
    hid_t ocpl_id, H5O_loc_t *loc/*out*/);
H5_DLL herr_t H5O_open(const H5O_loc_t *loc);
H5_DLL herr_t H5O_close(H5O_loc_t *loc);
H5_DLL int H5O_link(const H5O_loc_t *loc, int adjust, hid_t dxpl_id);
H5_DLL struct H5O_t *H5O_protect(H5O_loc_t *loc, hid_t dxpl_id);
H5_DLL herr_t H5O_unprotect(H5O_loc_t *loc, struct H5O_t *oh);
H5_DLL herr_t H5O_touch(H5O_loc_t *loc, hbool_t force, hid_t dxpl_id);
H5_DLL herr_t H5O_touch_oh(H5F_t *f, hid_t dxpl_id, struct H5O_t *oh,
    hbool_t force);
#ifdef H5O_ENABLE_BOGUS
H5_DLL herr_t H5O_bogus(H5O_loc_t *loc, hid_t dxpl_id);
H5_DLL herr_t H5O_bogus_oh(H5F_t *f, hid_t dxpl_id, struct H5O_t *oh);
#endif /* H5O_ENABLE_BOGUS */
H5_DLL herr_t H5O_delete(H5F_t *f, hid_t dxpl_id, haddr_t addr);
H5_DLL herr_t H5O_get_info(H5O_loc_t *oloc, H5O_info_t *oinfo, hid_t dxpl_id);
H5_DLL herr_t H5O_obj_type(const H5O_loc_t *loc, H5O_type_t *obj_type, hid_t dxpl_id);
H5_DLL herr_t H5O_get_create_plist(const H5O_loc_t *loc, hid_t dxpl_id, struct H5P_genplist_t *oc_plist);

/* Object header message routines */
H5_DLL herr_t H5O_msg_create(const H5O_loc_t *loc, unsigned type_id, unsigned mesg_flags,
    unsigned update_flags, void *mesg, hid_t dxpl_id);
H5_DLL herr_t H5O_msg_append(H5F_t *f, hid_t dxpl_id, struct H5O_t *oh, unsigned type_id,
    unsigned mesg_flags, unsigned update_flags, void *mesg);
H5_DLL herr_t H5O_msg_write(const H5O_loc_t *loc, unsigned type_id, unsigned flags,
    unsigned update_flags, void *mesg, hid_t dxpl_id);
H5_DLL void *H5O_msg_read(const H5O_loc_t *loc, unsigned type_id, void *mesg,
    hid_t dxpl_id);
H5_DLL herr_t H5O_msg_reset(unsigned type_id, void *native);
H5_DLL void *H5O_msg_free(unsigned type_id, void *mesg);
H5_DLL void *H5O_msg_copy(unsigned type_id, const void *mesg, void *dst);
H5_DLL int H5O_msg_count(const H5O_loc_t *loc, unsigned type_id, hid_t dxpl_id);
H5_DLL htri_t H5O_msg_exists(H5O_loc_t *loc, unsigned type_id, hid_t dxpl_id);
H5_DLL herr_t H5O_msg_remove(H5O_loc_t *loc, unsigned type_id, int sequence,
    hbool_t adj_link, hid_t dxpl_id);
H5_DLL herr_t H5O_msg_remove_op(const H5O_loc_t *loc, unsigned type_id, int sequence,
    H5O_operator_t op, void *op_data, hbool_t adj_link, hid_t dxpl_id);
H5_DLL herr_t H5O_msg_iterate(const H5O_loc_t *loc, unsigned type_id, H5O_operator_t op,
    void *op_data, hid_t dxpl_id);
H5_DLL size_t H5O_msg_raw_size(const H5F_t *f, unsigned type_id,
    hbool_t disable_shared, const void *mesg);
H5_DLL size_t H5O_msg_mesg_size(const H5F_t *f, unsigned type_id, const void *mesg,
    size_t extra_raw);
H5_DLL htri_t H5O_msg_is_shared(unsigned type_id, const void *mesg);
H5_DLL htri_t H5O_msg_can_share(unsigned type_id, const void *mesg);
H5_DLL herr_t H5O_msg_set_share(unsigned type_id, H5O_shared_t *share, void *mesg);
H5_DLL herr_t H5O_msg_reset_share(unsigned type_id, void *mesg);
H5_DLL herr_t H5O_msg_encode(H5F_t *f, unsigned type_id, hbool_t disable_shared,
    unsigned char *buf, const void *obj);
H5_DLL void* H5O_msg_decode(H5F_t *f, hid_t dxpl_id, unsigned type_id, 
    const unsigned char *buf);
H5_DLL herr_t H5O_msg_delete(H5F_t *f, hid_t dxpl_id, unsigned type_id, const void *mesg);

/* Object copying routines */
H5_DLL herr_t H5O_copy_header_map(const H5O_loc_t *oloc_src, H5O_loc_t *oloc_dst /*out */,
    hid_t dxpl_id, H5O_copy_t *cpy_info, hbool_t inc_depth);
H5_DLL herr_t H5O_copy_expand_ref(H5F_t *file_src, void *_src_ref, hid_t dxpl_id,
    H5F_t *file_dst, void *_dst_ref, size_t ref_count, H5R_type_t ref_type,
    H5O_copy_t *cpy_info);

/* Debugging routines */
H5_DLL herr_t H5O_debug_id(unsigned type_id, H5F_t *f, hid_t dxpl_id, const void *mesg, FILE *stream, int indent, int fwidth);
H5_DLL herr_t H5O_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE * stream, int indent,
			 int fwidth);

/* These functions operate on object locations */
H5_DLL herr_t H5O_loc_reset(H5O_loc_t *loc);
H5_DLL herr_t H5O_loc_copy(H5O_loc_t *dst, const H5O_loc_t *src, H5_copy_depth_t depth);
H5_DLL herr_t H5O_loc_hold_file(H5O_loc_t *loc);
H5_DLL herr_t H5O_loc_free(H5O_loc_t *loc);

/* Layout operators */
H5_DLL size_t H5O_layout_meta_size(const H5F_t *f, const void *_mesg);

/* EFL operators */
H5_DLL hsize_t H5O_efl_total_size(H5O_efl_t *efl);

/* Fill value operators */
H5_DLL herr_t H5O_fill_reset_dyn(H5O_fill_t *fill);
H5_DLL herr_t H5O_fill_convert(H5O_fill_t *fill, H5T_t *type, hbool_t *fill_changed, hid_t dxpl_id);

/* Link operators */
H5_DLL herr_t H5O_link_delete(H5F_t *f, hid_t dxpl_id, const void *_mesg);

/* Shared message operators */
H5_DLL herr_t H5O_shared_copy(void *dst, const H5O_shared_t *src);

#endif /* _H5Oprivate_H */

