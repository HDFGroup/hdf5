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
#include "H5Oproto.h"

#include "H5Fprivate.h"
#include "H5Gprivate.h"

#define H5O_MIN_SIZE	64		/*min obj header data size	*/
#define H5O_NMESGS	64		/*initial number of messages	*/
#define H5O_NCHUNKS	64		/*initial number of chunks	*/
#define H5O_NEW_MESG	(-1)		/*new message			*/
#define H5O_NO_ADDR	(-1)		/*no disk address yet		*/

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
   void		*(*decode)(hdf5_file_t*,const uint8*); /*decode mesg	*/
   herr_t	(*encode)(hdf5_file_t*,size_t,uint8*,const void*);
   void		*(*fast)(const H5G_entry_t*, void*); /*get from stab ent */
   hbool_t	(*cache)(H5G_entry_t*,const void*); /*put into entry	*/
   void		*(*copy)(const void*,void*); /*copy native value	*/
   size_t	(*size)(hdf5_file_t*,const void*);  /*size of raw value	*/
   void		*(*free)(void*);	/*free native data struct	*/
   herr_t	(*debug)(hdf5_file_t*,const void*, FILE*, intn, intn);
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
   haddr_t	btree;			/*address of B-tree		*/
   haddr_t	heap;			/*address of name heap		*/
} H5O_stab_t;


haddr_t H5O_new (hdf5_file_t *f, intn nlink, size_t size_hint);
intn H5O_link (hdf5_file_t *f, haddr_t addr, H5G_entry_t *ent, intn adjust);
void *H5O_read (hdf5_file_t *f, haddr_t addr, H5G_entry_t *ent,
		const H5O_class_t *type, intn sequence, void *mesg);
const void *H5O_peek (hdf5_file_t *f, haddr_t addr, const H5O_class_t *type,
		      intn sequence);
intn H5O_modify (hdf5_file_t *f, haddr_t addr, H5G_entry_t *ent,
		 hbool_t *ent_modified, const H5O_class_t *type,
		 intn overwrite, const void *mesg);
herr_t H5O_debug (hdf5_file_t *f, haddr_t addr, FILE *stream,
		  intn indent, intn fwidth);

#endif
