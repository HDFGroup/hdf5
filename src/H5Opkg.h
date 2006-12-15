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

#ifndef H5O_PACKAGE
#error "Do not include this file outside the H5O package!"
#endif

#ifndef _H5Opkg_H
#define _H5Opkg_H

/* Get package's private header */
#include "H5Oprivate.h"		/* Object headers		  	*/

/* Other private headers needed by this file */
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5ACprivate.h"	/* Metadata cache			*/

/* Object header macros */
#define H5O_NMESGS	8 		/*initial number of messages	     */
#define H5O_NCHUNKS	2		/*initial number of chunks	     */
#define H5O_MIN_SIZE	32		/*min obj header data size	     */

/* Versions of object header structure */

/* Initial version of the object header format */
#define H5O_VERSION_1		1

/* Revised version - leaves out reserved bytes and alignment padding, and adds
 *      magic number as prefix and checksum
 */
#define H5O_VERSION_2		2

/* The latest version of the format.  Look through the 'flush' 
 *      and 'size' callback for places to change when updating this. */
#define H5O_VERSION_LATEST	H5O_VERSION_2

/*
 * Align messages on 8-byte boundaries because we would like to copy the
 * object header chunks directly into memory and operate on them there, even
 * on 64-bit architectures.  This allows us to reduce the number of disk I/O
 * requests with a minimum amount of mem-to-mem copies.
 *
 * Note: We no longer attempt to do this. - QAK, 10/16/06
 */
#define H5O_ALIGN_OLD(X)	(8 * (((X) + 7) / 8))
#define H5O_ALIGN_VERS(V, X)						      \
     (((V) == H5O_VERSION_1) ?						      \
		H5O_ALIGN_OLD(X)					      \
        :								      \
		(X)							      \
    )
#define H5O_ALIGN_OH(O, X)						      \
     H5O_ALIGN_VERS((O)->version, X)
#define H5O_ALIGN_F(F, X)						      \
     H5O_ALIGN_VERS((H5F_USE_LATEST_FORMAT(F) ? H5O_VERSION_LATEST : H5O_VERSION_1), X)

/* Size of signature information (on disk) */
#define H5O_SIZEOF_MAGIC                4

/* Object header signatures */
#define H5O_HDR_MAGIC                   "OHDR"          /* Header */
#define H5O_CHK_MAGIC                   "OCHK"          /* Continuation chunk */

/* Size of checksum (on disk) */
#define H5O_SIZEOF_CHKSUM               4

/*
 * Size of object header prefix.
 */
#define H5O_SIZEOF_HDR_VERS(V,SOFS,SOFA)				      \
     (((V) == H5O_VERSION_1) ?						      \
        H5O_ALIGN_OLD(1 +	/*version number	*/		      \
                  1 +		/*reserved 		*/		      \
                  2 +		/*number of messages	*/		      \
                  4 +		/*reference count	*/		      \
                  4)		/*chunk data size	*/		      \
        :								      \
		(H5O_SIZEOF_MAGIC +	/*magic number  	*/	      \
                  1 +		/*version number 	*/		      \
                  2 +		/*number of messages	*/		      \
                  4 +		/*reference count	*/		      \
                  4 +		/*access time		*/		      \
                  4 +		/*modification time	*/		      \
                  4 +		/*change time		*/		      \
                  4 +		/*birth time		*/		      \
                  2 +		/*max compact attributes	*/	      \
                  2 +		/*min dense attributes	*/		      \
                  (SOFS) +	/*# of attributes	*/		      \
                  (SOFA) +	/*addr of attribute heap	*/	      \
                  (SOFA) +	/*addr of attribute name index	*/	      \
                  4 +		/*chunk data size	*/		      \
                  H5O_SIZEOF_CHKSUM)	/*checksum size	        */	      \
    )
#define H5O_SIZEOF_HDR_OH(O)						      \
     H5O_SIZEOF_HDR_VERS((O)->version, (O)->sizeof_size, (O)->sizeof_addr)
#define H5O_SIZEOF_HDR_F(F)						      \
     H5O_SIZEOF_HDR_VERS(H5F_USE_LATEST_FORMAT(F) ? H5O_VERSION_LATEST : H5O_VERSION_1, H5F_SIZEOF_SIZE(F), H5F_SIZEOF_ADDR(F))

/*
 * Size of object header message prefix
 */
#define H5O_SIZEOF_MSGHDR_VERS(V)					      \
     (((V) == H5O_VERSION_1) ?						      \
         H5O_ALIGN_OLD(2 +	/*message type		*/			      \
                   2 +	/*sizeof message data	*/			      \
                   1 +	/*flags              	*/			      \
                   3)	/*reserved		*/			      \
        :								      \
                (1 +	/*message type		*/			      \
                   2 + 	/*sizeof message data	*/			      \
                   1)	/*flags              	*/			      \
    )
#define H5O_SIZEOF_MSGHDR_OH(O)						      \
     H5O_SIZEOF_MSGHDR_VERS((O)->version)
#define H5O_SIZEOF_MSGHDR_F(F)						      \
     H5O_SIZEOF_MSGHDR_VERS(H5F_USE_LATEST_FORMAT(F) ? H5O_VERSION_LATEST : H5O_VERSION_1)

/*
 * Size of chunk "header" for each chunk
 */
#define H5O_SIZEOF_CHKHDR_VERS(V)					      \
     (((V) == H5O_VERSION_1) ?						      \
                   0 +	/*no magic #  */				      \
                   0 	/*no checksum */				      \
        :								      \
                   H5O_SIZEOF_MAGIC + 	/*magic #  */			      \
                   H5O_SIZEOF_CHKSUM 	/*checksum */			      \
    )
#define H5O_SIZEOF_CHKHDR_OH(O)						      \
     H5O_SIZEOF_CHKHDR_VERS((O)->version)

/*
 * Size of checksum for each chunk
 */
#define H5O_SIZEOF_CHKSUM_VERS(V)					      \
     (((V) == H5O_VERSION_1) ?						      \
                   0 	/*no checksum */				      \
        :								      \
                   H5O_SIZEOF_CHKSUM 	/*checksum */			      \
    )
#define H5O_SIZEOF_CHKSUM_OH(O)						      \
     H5O_SIZEOF_CHKSUM_VERS((O)->version)


struct H5O_msg_class_t {
    unsigned	id;				 /*message type ID on disk   */
    const char	*name;				 /*for debugging             */
    size_t	native_size;			 /*size of native message    */
    void	*(*decode)(H5F_t*, hid_t, const uint8_t*);
    herr_t	(*encode)(H5F_t*, uint8_t*, const void*);
    void	*(*copy)(const void *, void *);  /*copy native value         */
    size_t	(*raw_size)(const H5F_t*, const void*);/*sizeof encoded message	*/
    herr_t	(*reset)(void *);		 /*free nested data structs  */
    herr_t	(*free)(void *);		 /*free main data struct  */
    herr_t	(*del)(H5F_t *, hid_t, const void *, hbool_t); /* Delete space in file referenced by this message */
    herr_t	(*link)(H5F_t *, hid_t, const void *); /* Increment any links in file reference by this message */
    void	*(*get_share)(const void*, struct H5O_shared_t*);    /* Get shared information */
    herr_t	(*set_share)(void*, const struct H5O_shared_t*);    /* Set shared information */
    htri_t	(*is_shared)(const void*);      /* Is message shared? */
    herr_t	(*pre_copy_file)(H5F_t *, const H5O_msg_class_t *, const void *, hbool_t *, const H5O_copy_t *, void *); /*"pre copy" action when copying native value to file */
    void	*(*copy_file)(H5F_t *, const H5O_msg_class_t *, void *, H5F_t *, hid_t, H5O_copy_t *, void *); /*copy native value to file */
    herr_t	(*post_copy_file)(const H5O_loc_t *, const void *, H5O_loc_t *, void *, hid_t, H5O_copy_t *); /*"post copy" action when copying native value to file */
    herr_t	(*debug)(H5F_t*, hid_t, const void*, FILE*, int, int);
};

typedef struct H5O_mesg_t {
    const H5O_msg_class_t	*type;	/*type of message		     */
    hbool_t		dirty;		/*raw out of date wrt native	     */
    uint8_t		flags;		/*message flags			     */
    unsigned		chunkno;	/*chunk number for this mesg	     */
    void		*native;	/*native format message		     */
    uint8_t		*raw;		/*ptr to raw data		     */
    size_t		raw_size;	/*size with alignment		     */
} H5O_mesg_t;

typedef struct H5O_chunk_t {
    hbool_t	dirty;			/*dirty flag			     */
    haddr_t	addr;			/*chunk file address		     */
    size_t	size;			/*chunk size			     */
    size_t	gap;			/*space at end of chunk too small for null message */
    uint8_t	*image;			/*image of file			     */
} H5O_chunk_t;

struct H5O_t {
    H5AC_info_t cache_info; /* Information for H5AC cache functions, _must_ be */
                            /* first field in structure */

    /* General information (not stored) */
    size_t      sizeof_size;            /* Size of file sizes 		     */
    size_t      sizeof_addr;            /* Size of file addresses	     */

    /* Object information (stored) */
    unsigned	version;		/*version number		     */
    unsigned	nlink;			/*link count			     */

    /* Time information (stored, for versions > 1) */
    time_t      atime;                  /*access time 			     */
    time_t      mtime;                  /*modification time 		     */
    time_t      ctime;                  /*change time 			     */
    time_t      btime;                  /*birth time 			     */

    /* Attribute information (stored, for versions > 1) */
    unsigned	max_compact;		/* Maximum # of compact attributes   */
    unsigned	min_dense;		/* Minimum # of "dense" attributes   */
    hsize_t     nattrs;                 /* Number of attributes in the group */
    haddr_t     attr_fheap_addr;        /* Address of fractal heap for storing "dense" attributes */
    haddr_t     name_bt2_addr;          /* Address of v2 B-tree for indexing names of attributes */

    /* Message management (stored, in chunks) */
    size_t	nmesgs;			/*number of messages		     */
    size_t	alloc_nmesgs;		/*number of message slots	     */
    H5O_mesg_t	*mesg;			/*array of messages		     */
    size_t      skipped_mesg_size;      /*size of skipped messages (for sanity checking) */

    /* Chunk management (not stored) */
    size_t	nchunks;		/*number of chunks		     */
    size_t	alloc_nchunks;		/*chunks allocated		     */
    H5O_chunk_t *chunk;			/*array of chunks		     */
};

/* Callback information for copying dataset */
typedef struct {
    struct H5S_extent_t *src_space_extent;     /* Copy of dataspace extent for dataset */
    H5T_t *src_dtype;                   /* Copy of datatype for dataset */
    H5O_pline_t *src_pline;             /* Copy of filter pipeline for dataet */
} H5D_copy_file_ud_t;

/* Class for types of objects in file */
typedef struct H5O_obj_class_t {
    H5O_type_t	type;				/*object type on disk	     */
    const char	*name;				/*for debugging		     */
    void       *(*get_copy_file_udata)(void);	/*retrieve user data for 'copy file' operation */
    void	(*free_copy_file_udata)(void *); /*free user data for 'copy file' operation */
    htri_t	(*isa)(H5O_t *);		/*if a header matches an object class */
    hid_t	(*open)(H5G_loc_t *, hid_t );	/*open an object of this class */
    H5O_loc_t	*(*get_oloc)(hid_t );		/*get the object header location for an object */
} H5O_obj_class_t;

/* Node in skip list to map addresses from one file to another during object header copy */
typedef struct H5O_addr_map_t {
    haddr_t     src_addr;               /* Address of object in source file */
    haddr_t     dst_addr;               /* Address of object in destination file */
    hbool_t     is_locked;              /* Indicate that the destination object is locked currently */
    hsize_t     inc_ref_count;          /* Number of deferred increments to reference count */
} H5O_addr_map_t;


/* Typedef for "internal library" iteration operations */
typedef herr_t (*H5O_lib_operator_t)(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned sequence, unsigned *oh_flags_ptr/*out*/, void *operator_data/*in,out*/);

/* Some syntactic sugar to make the compiler happy with two different kinds of iterator callbacks */
typedef union {
    H5O_operator_t app_op;      /* Application callback for each message */
    H5O_lib_operator_t lib_op;  /* Library internal callback for each message */
} H5O_mesg_operator_t;


/* H5O inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_OHDR[1];

/* Header message ID to class mapping */
H5_DLLVAR const H5O_msg_class_t *const H5O_msg_class_g[19];

/* Header object ID to class mapping */
H5_DLLVAR const H5O_obj_class_t *const H5O_obj_class_g[3];

/* Declare external the free list for H5O_t's */
H5FL_EXTERN(H5O_t);

/* Declare external the free list for H5O_mesg_t sequences */
H5FL_SEQ_EXTERN(H5O_mesg_t);

/* Declare external the free list for H5O_chunk_t sequences */
H5FL_SEQ_EXTERN(H5O_chunk_t);

/* Declare external the free list for chunk_image blocks */
H5FL_BLK_EXTERN(chunk_image);

/*
 * Object header messages
 */

/* Null Message. (0x0000) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_NULL[1];

/* Simple Dataspace Message. (0x0001) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_SDSPACE[1];

/* Link Information Message. (0x0002) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_LINFO[1];

/* Datatype Message. (0x0003) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_DTYPE[1];

/* Old Fill Value Message. (0x0004) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_FILL[1];

/* New Fill Value Message. (0x0005) */
/*
 * The new fill value message is fill value plus
 * space allocation time and fill value writing time and whether fill
 * value is defined.
 */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_FILL_NEW[1];

/* Link Message. (0x0006) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_LINK[1];

/* External File List Message. (0x0007) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_EFL[1];

/* Data Layout Message. (0x0008) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_LAYOUT[1];

#ifdef H5O_ENABLE_BOGUS
/* "Bogus" Message. (0x0009) */
/*
 * Used for debugging - should never be found in valid HDF5 file.
 */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_BOGUS[1];
#endif /* H5O_ENABLE_BOGUS */

/* Group Information Message. (0x000a) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_GINFO[1];

/* Filter pipeline message. (0x000b) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_PLINE[1];

/* Attribute Message. (0x000c) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_ATTR[1];

/* Object name message. (0x000d) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_NAME[1];

/* Modification Time Message. (0x000e) */
/*
 * The message is just a `time_t'.
 * (See also the "new" modification time message)
 */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_MTIME[1];

/* Shared Object Message. (0x000f) */
/*
 * This message ID never really appears in an object
 * header.  Instead, bit 2 of the `Flags' field will be set and the ID field
 * will be the ID of the pointed-to message.
 */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_SHARED[1];

/* Object Header Continuation Message. (0x0010) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_CONT[1];

/* Symbol Table Message. (0x0011) */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_STAB[1];

/* New Modification Time Message. (0x0012) */
/*
 * The message is just a `time_t'.
 */
H5_DLLVAR const H5O_msg_class_t H5O_MSG_MTIME_NEW[1];


/*
 * Object header "object" types
 */

/* Group Object. (H5G_GROUP - 0) */
H5_DLLVAR const H5O_obj_class_t H5O_OBJ_GROUP[1];

/* Dataset Object. (H5G_DATASET - 1) */
H5_DLLVAR const H5O_obj_class_t H5O_OBJ_DATASET[1];

/* Datatype Object. (H5G_TYPE - 2) */
H5_DLLVAR const H5O_obj_class_t H5O_OBJ_DATATYPE[1];


/* Package-local function prototypes */
H5_DLL herr_t H5O_flush_msgs(H5F_t *f, H5O_t *oh);
H5_DLL herr_t H5O_delete_mesg(H5F_t *f, hid_t dxpl_id, H5O_mesg_t *mesg,
    hbool_t adj_link);
H5_DLL const H5O_obj_class_t *H5O_obj_class_real(H5O_t *oh);

/* Object header message routines */
H5_DLL herr_t H5O_msg_append_real(H5F_t *f, hid_t dxpl_id, H5O_t *oh,
    const H5O_msg_class_t *type, unsigned mesg_flags, unsigned update_flags,
    void *mesg, unsigned *oh_flags_ptr);
H5_DLL void *H5O_msg_read_real(H5F_t *f, H5O_t *oh, unsigned type_id,
    int sequence, void *mesg, hid_t dxpl_id);
H5_DLL void *H5O_msg_free_real(const H5O_msg_class_t *type, void *mesg);
H5_DLL herr_t H5O_msg_free_mesg(H5O_mesg_t *mesg);
H5_DLL htri_t H5O_msg_exists_oh(struct H5O_t *oh, unsigned type_id, int sequence);
H5_DLL void * H5O_msg_copy_file(const H5O_msg_class_t *copy_type,
    const H5O_msg_class_t *mesg_type, H5F_t *file_src, void *mesg_src,
    H5F_t *file_dst, hid_t dxpl_id, hbool_t *shared, H5O_copy_t *cpy_info, void *udata);
H5_DLL herr_t H5O_msg_iterate_real(H5F_t *f, H5O_t *oh, const H5O_msg_class_t *type,
    hbool_t internal, H5O_mesg_operator_t op, void *op_data, hid_t dxpl_id,
    unsigned *oh_flags_ptr);

/* Object header allocation routines */
H5_DLL unsigned H5O_alloc(H5F_t *f, hid_t dxpl_id, H5O_t *oh,
    const H5O_msg_class_t *type, size_t size, hbool_t * oh_dirtied_ptr);
H5_DLL herr_t H5O_condense_header(H5F_t *f, H5O_t *oh, hid_t dxpl_id);
H5_DLL herr_t H5O_release_mesg(H5F_t *f, hid_t dxpl_id, H5O_t *oh,
    H5O_mesg_t *mesg, hbool_t delete_mesg, hbool_t adj_link);

/* Shared object operators */
H5_DLL void * H5O_shared_read(H5F_t *f, hid_t dxpl_id, const H5O_shared_t *shared,
    const H5O_msg_class_t *type, void *mesg);

/* Attribute operations */
H5_DLL herr_t H5O_attr_create(const H5O_loc_t *loc, hid_t dxpl_id, H5A_t *attr);
H5_DLL H5A_t *H5O_attr_open(const H5O_loc_t *loc, const char *name,
    hid_t dxpl_id);
H5_DLL herr_t H5O_attr_write(const H5O_loc_t *loc, hid_t dxpl_id,
    H5A_t *attr);
H5_DLL herr_t H5O_attr_rename(const H5O_loc_t *loc, hid_t dxpl_id,
    const char *old_name, const char *new_name);
H5_DLL herr_t H5O_attr_iterate(hid_t loc_id, const H5O_loc_t *loc, hid_t dxpl_id,
    unsigned skip, unsigned *last_attr, H5A_operator_t op, void *op_data);
H5_DLL herr_t H5O_attr_remove(const H5O_loc_t *loc, const char *name,
    hid_t dxpl_id);
H5_DLL int H5O_attr_count(const H5O_loc_t *loc, hid_t dxpl_id);
H5_DLL htri_t H5O_attr_exists(const H5O_loc_t *loc, const char *name, hid_t dxpl_id);

/* These functions operate on object locations */
H5_DLL H5O_loc_t *H5O_get_loc(hid_t id);

/* Useful metadata cache callbacks */
H5_DLL herr_t H5O_dest(H5F_t *f, H5O_t *oh);

/* Testing functions */
#ifdef H5O_TESTING
H5_DLL htri_t H5O_is_attr_dense_test(hid_t oid);
#endif /* H5O_TESTING */

/* Object header debugging routines */
#ifdef H5O_DEBUG
H5_DLL herr_t H5O_assert(const H5O_t *oh);
#endif /* H5O_DEBUG */
H5_DLL herr_t H5O_debug_real(H5F_t *f, hid_t dxpl_id, H5O_t *oh, haddr_t addr, FILE *stream, int indent, int fwidth);

#endif /* _H5Opkg_H */


