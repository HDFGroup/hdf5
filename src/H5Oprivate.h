/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *			All rights reserved.
 *
 *-------------------------------------------------------------------------
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

#include <H5Opublic.h>

/* Private headers neede by this file */
#include <H5private.h>
#include <H5Fprivate.h>
#include <H5Gprivate.h>
#include <H5Tprivate.h>
#include <H5Pprivate.h>

/*
 * Align messages on 8-byte boundaries because we would like to copy the
 * object header chunks directly into memory and operate on them there, even
 * on 64-bit architectures.  This allows us to reduce the number of disk I/O
 * requests with a minimum amount of mem-to-mem copies.
 */
#define H5O_ALIGN(X)		(8*(((X)+8-1)/8))

#define H5O_MIN_SIZE	H5O_ALIGN(32)	/*min obj header data size	     */
#define H5O_NMESGS	32		/*initial number of messages	     */
#define H5O_NCHUNKS	8		/*initial number of chunks	     */
#define H5O_NEW_MESG	(-1)		/*new message			     */
#define H5O_ALL		(-1)		/*delete all messages of type	     */

/* Flags which are part of the message id */
#define H5O_FLAG_CONSTANT	0x8000
#define H5O_FLAG_BITS		0x8000
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

typedef struct H5O_class_t {
    intn	id;				    /*message type ID on disk*/
    const char	*name;				 /*message name for debugging*/
    size_t	native_size;			     /*size of native message*/
    void	*(*decode) (H5F_t *, size_t, const uint8 *);
    herr_t	(*encode) (H5F_t *, size_t, uint8 *, const void *);
    void	*(*copy) (const void *, void *);     /*copy native value     */
    size_t	(*raw_size) (H5F_t *, const void *); /*sizeof raw val	     */
    herr_t	(*reset) (void *);		/*free nested data structures*/
    herr_t	(*debug) (H5F_t *, const void *, FILE *, intn, intn);
} H5O_class_t;

typedef struct H5O_mesg_t {
    const H5O_class_t	*type;		/*type of message		     */
    hbool_t		dirty;		/*raw out of date wrt native	     */
    hbool_t		constant;	/*is message constant?		     */
    void		*native;	/*native format message		     */
    uint8		*raw;		/*ptr to raw data		     */
    size_t		raw_size;	/*size with alignment		     */
    intn		chunkno;	/*chunk number for this mesg	     */
} H5O_mesg_t;

typedef struct H5O_chunk_t {
    hbool_t	dirty;			/*dirty flag			     */
    haddr_t	addr;			/*chunk file address		     */
    size_t	size;			/*chunk size			     */
    uint8	*image;			/*image of file			     */
} H5O_chunk_t;

typedef struct H5O_t {
    hbool_t	dirty;			/*out of data wrt disk		     */
    intn	version;		/*version number		     */
    intn	nlink;			/*link count			     */
    intn	nmesgs;			/*number of messages		     */
    intn	alloc_nmesgs;		/*number of message slots	     */
    H5O_mesg_t	*mesg;			/*array of messages		     */
    intn	nchunks;		/*number of chunks		     */
    intn	alloc_nchunks;		/*chunks allocated		     */
    H5O_chunk_t *chunk;			/*array of chunks		     */
} H5O_t;

/*
 * Null Message.
 */
#define H5O_NULL_ID	0x0000
extern const H5O_class_t H5O_NULL[1];

/*
 * Simple Data Space Message.
 */
#define H5O_SDSPACE_ID	0x0001
extern const H5O_class_t H5O_SDSPACE[1];

/* operates on an H5P_simple_t struct */

/*
 * Data Type Message.
 */
#define H5O_DTYPE_ID	0x0003
extern const H5O_class_t H5O_DTYPE[1];

/* operates on an H5T_t struct */

/*
 * Data Layout Message
 */
#define H5O_LAYOUT_ID		0x0008
#define H5O_LAYOUT_NDIMS	32
extern const H5O_class_t H5O_LAYOUT[1];

typedef struct H5O_layout_t {
    int		type;			/*type of layout, H5D_layout_t	     */
    haddr_t	addr;			/*file address of data or B-tree     */
    uintn	ndims;			/*num dimensions in stored data	     */
    size_t	dim[H5O_LAYOUT_NDIMS];	/*size of data or chunk		     */
} H5O_layout_t;

/*
 * External File List Message
 */
#define H5O_EFL_ID		0x0009
extern const H5O_class_t H5O_EFL[1];

typedef struct H5O_efl_t {
    haddr_t	heap_addr;		/*address of name heap		     */
    uintn	nalloc;			/*number of slots allocated	     */
    uintn	nused;			/*number of slots used		     */
    size_t	*offset;		/*array of name offsets in heap	     */
} H5O_efl_t;

/*
 * Object name message.
 */
#define H5O_NAME_ID	0x000d
extern const H5O_class_t H5O_NAME[1];

typedef struct H5O_name_t {
    const char	*s;			/*ptr to malloc'd memory	     */
} H5O_name_t;

/*
 * Object header continuation message.
 */
#define H5O_CONT_ID	0x0010
extern const H5O_class_t H5O_CONT[1];

typedef struct H5O_cont_t {
    haddr_t	addr;			/*address of continuation block	     */
    size_t	size;			/*size of continuation block	     */

    /* the following field(s) do not appear on disk */
    intn	chunkno;		/*chunk this mesg refers to	     */
} H5O_cont_t;

/*
 * Symbol table message.
 */
#define H5O_STAB_ID	0x0011
extern const H5O_class_t H5O_STAB[1];

void *H5O_stab_fast (const H5G_cache_t *cache, const H5O_class_t *type,
		     void *_mesg);

typedef struct H5O_stab_t {
    haddr_t	btree_addr;		/*address of B-tree		     */
    haddr_t	heap_addr;		/*address of name heap		     */
} H5O_stab_t;

herr_t H5O_create (H5F_t *f, size_t size_hint, H5G_entry_t *ent/*out*/);
herr_t H5O_open (H5F_t *f, H5G_entry_t *ent);
herr_t H5O_close (H5G_entry_t *ent);
intn H5O_link (H5G_entry_t *ent, intn adjust);
void *H5O_read (H5G_entry_t *ent, const H5O_class_t *type, intn sequence,
		void *mesg);
intn H5O_modify (H5G_entry_t *ent, const H5O_class_t *type, intn overwrite,
		 uintn flags, const void *mesg);
herr_t H5O_remove (H5G_entry_t *ent, const H5O_class_t *type, intn sequence);
herr_t H5O_reset (const H5O_class_t *type, void *native);
herr_t H5O_debug (H5F_t *f, const haddr_t *addr, FILE * stream, intn indent,
		  intn fwidth);
#endif
