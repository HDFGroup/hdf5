/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5Oprivate.h
 * 			Aug  5 1997
 * 			Robb Matzke <matzke@llnl.gov>
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

#define H5O_MIN_SIZE	32		/*min obj header data size	*/
#define H5O_NMESGS	32		/*initial number of messages	*/
#define H5O_NCHUNKS	8		/*initial number of chunks	*/
#define H5O_NEW_MESG	(-1)		/*new message			*/
#define H5O_NO_ADDR	(-1)		/*no disk address yet		*/
#define H5O_ALL		(-1)		/*delete all messages of type	*/

#define H5O_VERSION	1
#define H5O_ALIGNMENT	4
#define H5O_ALIGN(X,A)	((X)=(A)*(((X)+(A)-1)/(A)))

#define H5O_SIZEOF_HDR(F)						      \
   (1 +		/*version number	*/				      \
    1 +		/*alignment		*/				      \
    2 +		/*number of messages	*/				      \
    4 +		/*reference count	*/				      \
    4)		/*header data size	*/

typedef struct H5O_class_t {
   intn		id;			/*message type ID on disk	*/
   const char	*name;			/*message name for debugging	*/
   size_t	native_size;		/*size of native message	*/
   H5G_type_t	cache_type;		/*type field in symbol table	*/
   void		*(*decode)(H5F_t*,size_t,const uint8*);
   herr_t	(*encode)(H5F_t*,size_t,uint8*,const void*);
   void		*(*fast)(const H5G_cache_t*, void*);/*get from of entry	*/
   hbool_t	(*cache)(H5G_type_t*, H5G_cache_t*,const void*); /*into entry*/
   void		*(*copy)(const void*,void*); /*copy native value	*/
   size_t	(*raw_size)(H5F_t*,const void*); /*sizeof raw val	*/
   herr_t	(*reset)(void*); /*free nested data structures		*/
   herr_t	(*debug)(H5F_t*,const void*, FILE*, intn, intn);
} H5O_class_t;

typedef struct H5O_mesg_t {
   const H5O_class_t *type;		/*type of message		*/
   hbool_t	dirty;			/*raw out of date wrt native	*/
   void		*native;		/*native format message		*/
   uint8	*raw;			/*ptr to raw data		*/
   size_t	raw_size;		/*size with alignment		*/
   intn		chunkno;		/*chunk number for this mesg	*/
} H5O_mesg_t;

typedef struct H5O_chunk_t {
   hbool_t	dirty;			/*dirty flag			*/
   haddr_t	addr;			/*chunk file address		*/
   size_t	size;			/*chunk size			*/
   uint8	*image;			/*image of file			*/
} H5O_chunk_t;
   
typedef struct H5O_t {
   hbool_t	dirty;			/*out of data wrt disk		*/
   intn		version;		/*version number		*/
   intn		alignment;		/*message alignment		*/
   intn		nlink;			/*link count			*/
   intn		nmesgs;			/*number of messages		*/
   intn		alloc_nmesgs;		/*number of message slots	*/
   H5O_mesg_t	*mesg;			/*array of messages		*/
   intn		nchunks;		/*number of chunks		*/
   intn		alloc_nchunks;		/*chunks allocated		*/
   H5O_chunk_t	*chunk;			/*array of chunks		*/
} H5O_t;

/*
 * Null message.
 */
#define H5O_NULL_ID	0x0000
extern const H5O_class_t H5O_NULL[1];

/*
 * Simple Dimensionality message.
 */
#define H5O_SIM_DIM_ID	0x0001
extern const H5O_class_t H5O_SIM_DIM[1];

typedef H5P_sdim_t H5O_sim_dim_t;

/*
 * Simple Datatype message.
 */
#define H5O_SIM_DTYPE_ID	0x0003
extern const H5O_class_t H5O_SIM_DTYPE[1];

typedef h5_atomic_type_t H5O_sim_dtype_t;

/*
 * Standard Data Storage message.
 */
#define H5O_STD_STORE_ID	0x0005
extern const H5O_class_t H5O_STD_STORE[1];

typedef struct H5O_std_store_t {
   haddr_t	off;
   haddr_t	len;
} H5O_std_store_t;

/*
 * Indexed Data Storage message.
 */
#define H5O_ISTORE_ID		0x0008
#define H5O_ISTORE_NDIMS	32
extern const H5O_class_t H5O_ISTORE[1];

typedef struct H5O_istore_t {
   haddr_t	btree_addr;		/*file address of B-tree	*/
   uintn	ndims;			/*num dimensions in stored data	*/
   size_t	alignment[H5O_ISTORE_NDIMS];	/*algn in logical space	*/
} H5O_istore_t;

/*
 * Object name message.
 */
#define H5O_NAME_ID	0x000d
extern const H5O_class_t H5O_NAME[1];

typedef struct H5O_name_t {
   const char	*s;			/*ptr to malloc'd memory	*/
} H5O_name_t;

/*
 * Object header continuation message.
 */
#define H5O_CONT_ID	0x0010
extern const H5O_class_t H5O_CONT[1];

typedef struct H5O_cont_t {
   haddr_t	addr;			/*address of continuation block	*/
   size_t	size;			/*size of continuation block	*/

   /* the following field(s) do not appear on disk */
   intn		chunkno;		/*chunk this mesg refers to	*/
} H5O_cont_t;

/*
 * Symbol table message.
 */
#define H5O_STAB_ID	0x0011
extern const H5O_class_t H5O_STAB[1];

typedef struct H5O_stab_t {
   haddr_t	btree_addr;		/*address of B-tree		*/
   haddr_t	heap_addr;		/*address of name heap		*/
} H5O_stab_t;



haddr_t H5O_new (H5F_t *f, intn nlink, size_t size_hint);
intn H5O_link (H5F_t *f, H5G_entry_t *ent, intn adjust);
void *H5O_read (H5F_t *f, haddr_t addr, H5G_entry_t *ent,
		const H5O_class_t *type, intn sequence, void *mesg);
const void *H5O_peek (H5F_t *f, haddr_t addr, const H5O_class_t *type,
		      intn sequence);
intn H5O_modify (H5F_t *f, haddr_t addr, H5G_entry_t *ent,
		 const H5O_class_t *type, intn overwrite, const void *mesg);
herr_t H5O_remove (H5F_t *f, haddr_t addr, H5G_entry_t *ent,
		   const H5O_class_t *type, intn sequence);
herr_t H5O_reset (const H5O_class_t *type, void *native);
herr_t H5O_debug (H5F_t *f, haddr_t addr, FILE *stream,
		  intn indent, intn fwidth);

#endif
