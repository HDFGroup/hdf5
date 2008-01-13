/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

#ifndef H5O_PACKAGE
#error "Do not include this file outside the H5O package!"
#endif

#ifndef _H5Opkg_H
#define _H5Opkg_H

/* Get package's private header */
#include "H5Oprivate.h"		/* Object headers		  	*/

/* Other private headers needed by this file */
#include "H5ACprivate.h"	/* Metadata cache			*/

/*
 * Align messages on 8-byte boundaries because we would like to copy the
 * object header chunks directly into memory and operate on them there, even
 * on 64-bit architectures.  This allows us to reduce the number of disk I/O
 * requests with a minimum amount of mem-to-mem copies.
 */
#define H5O_ALIGN(X)		(8*(((X)+8-1)/8))

/* Object header macros */
#define H5O_NMESGS	32		/*initial number of messages	     */
#define H5O_NCHUNKS	2		/*initial number of chunks	     */

/* Version of object header structure */
#define H5O_VERSION		1

/*
 * Size of object header header.
 */
#define H5O_SIZEOF_HDR(F)						      \
    H5O_ALIGN(1 +		/*version number	*/		      \
	      1 +		/*alignment		*/		      \
	      2 +		/*number of messages	*/		      \
	      4 +		/*reference count	*/		      \
	      4)		/*header data size	*/

/*
 * Size of message header
 */
#define H5O_SIZEOF_MSGHDR(F)						      \
     H5O_ALIGN(2 +	/*message type		*/			      \
	       2 +	/*sizeof message data	*/			      \
	       4)	/*reserved		*/

struct H5O_msg_class_t {
    unsigned	id;				 /*message type ID on disk   */
    const char	*name;				 /*for debugging             */
    size_t	native_size;			 /*size of native message    */
    void	*(*decode)(H5F_t*, hid_t, const uint8_t*);
    herr_t	(*encode)(H5F_t*, uint8_t*, const void*);
    void	*(*copy)(const void*, void*, unsigned);    /*copy native value         */
    size_t	(*raw_size)(const H5F_t*, const void*);/*sizeof raw val	     */
    herr_t	(*reset)(void *);		 /*free nested data structs  */
    herr_t	(*free)(void *);		 /*free main data struct  */
    herr_t	(*del)(H5F_t *, hid_t, const void *, hbool_t); /* Delete space in file referenced by this message */
    herr_t	(*link)(H5F_t *, hid_t, const void *); /* Increment any links in file reference by this message */
    herr_t	(*get_share)(H5F_t*, const void*, struct H5O_shared_t*);    /* Get shared information */
    herr_t	(*set_share)(H5F_t*, void*, const struct H5O_shared_t*);    /* Set shared information */
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
    uint8_t	*image;			/*image of file			     */
} H5O_chunk_t;

struct H5O_t {
    H5AC_info_t cache_info; /* Information for H5AC cache functions, _must_ be */
                            /* first field in structure */
    unsigned	version;		/*version number		     */
    int		nlink;			/*link count			     */
    size_t	nmesgs;			/*number of messages		     */
    size_t	alloc_nmesgs;		/*number of message slots	     */
    H5O_mesg_t	*mesg;			/*array of messages		     */
    size_t	nchunks;		/*number of chunks		     */
    size_t	alloc_nchunks;		/*chunks allocated		     */
    H5O_chunk_t *chunk;			/*array of chunks		     */
};

/* H5O inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_OHDR[1];

/* Header message ID to class mapping */
H5_DLLVAR const H5O_msg_class_t *const H5O_msg_class_g[19];

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

/* Package-local function prototypes */
H5_DLL herr_t H5O_init(void);
H5_DLL herr_t H5O_free_mesg(H5O_mesg_t *mesg);
H5_DLL void * H5O_read_real(const H5G_entry_t *ent, const H5O_msg_class_t *type,
        int sequence, void *mesg, hid_t dxpl_id);
H5_DLL void * H5O_free_real(const H5O_msg_class_t *type, void *mesg);
H5_DLL herr_t H5O_alloc_msgs(H5O_t *oh, size_t min_alloc);

/* Shared object operators */
H5_DLL void * H5O_shared_read(H5F_t *f, hid_t dxpl_id, H5O_shared_t *shared,
    const H5O_msg_class_t *type, void *mesg);

/* Symbol table operators */
H5_DLL void *H5O_stab_fast(const H5G_cache_t *cache, const struct H5O_msg_class_t *type,
			    void *_mesg);

/* Useful metadata cache callbacks */
H5_DLL herr_t H5O_dest(H5F_t *f, H5O_t *oh);

#endif /* _H5Opkg_H */

