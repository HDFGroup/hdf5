/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5ACprivate.h
 * 			Jul  9 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Constants and typedefs available to the rest of the
 *			library.
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5ACprivate_H
#define _H5ACprivate_H
#include <H5ACpublic.h>		/*public prototypes			*/

/* Pivate headers needed by this header */
#include <H5private.h>
#include <H5Fprivate.h>

/*
 * Class methods pertaining to caching.  Each type of cached object will
 * have a constant variable with permanent life-span that describes how
 * to cache the object.  That variable will be of type H5AC_class_t and
 * have the following required fields...
 *
 * LOAD:	Loads an object from disk to memory.  The function
 *		should allocate some data structure and return it.
 *
 * FLUSH:	Writes some data structure back to disk.  It would be
 *		wise for the data structure to include dirty flags to
 *		indicate whether it really needs to be written.  This
 *		function is also responsible for freeing memory allocated
 *		by the LOAD method if the DEST argument is non-zero.
 */
typedef struct H5AC_class_t {
   void		*(*load)(H5F_t*, haddr_t addr, void *udata);
   herr_t	(*flush)(H5F_t*, hbool_t dest, haddr_t addr, void *thing);
} H5AC_class_t;

/*
 * A cache has a certain number of entries.  Objects are mapped into a
 * cache entry by hashing the object's file address.  Each file has its
 * own cache, an array of slots.
 */
#define H5AC_NSLOTS	10330	/*prime number tend to work best	*/
#define H5AC_HASH(F,ADDR) ((unsigned)(ADDR) % (F)->shared->cache->nslots)

typedef struct H5AC_prot_t {
   const H5AC_class_t *type;	/*type of protected thing		*/
   haddr_t	addr;		/*address of protected thing		*/
   void		*thing;		/*(possible) protected thing		*/
} H5AC_prot_t;

typedef struct H5AC_slot_t {
   const H5AC_class_t *type;	/*type of object stored here		*/
   haddr_t	addr;		/*file address for object		*/
   void		*thing;		/*the thing which is cached		*/
   intn		nprots;		/*number of things protected		*/
   intn		aprots;		/*nelmts of `prot' array		*/
   H5AC_prot_t	*prot;		/*array of protected things		*/
} H5AC_slot_t;

typedef struct H5AC_t {
   intn		nslots;		/*number of cache slots			*/
   H5AC_slot_t	*slot;		/*the cache slots			*/
   intn		nprots;		/*number of protected objects		*/
} H5AC_t;


/*
 * Library prototypes.
 */
herr_t H5AC_dest (H5F_t *f);
void *H5AC_find_f (H5F_t *f, const H5AC_class_t *type, haddr_t addr,
		   void *udata);
void * H5AC_protect (H5F_t *f, const H5AC_class_t *type, haddr_t addr,
		     void *udata);
herr_t H5AC_unprotect (H5F_t *f, const H5AC_class_t *type, haddr_t addr,
		       void *thing);
herr_t H5AC_flush (H5F_t *f, const H5AC_class_t *type, haddr_t addr,
		   hbool_t destroy);
herr_t H5AC_new (H5F_t *f, intn size_hint);
herr_t H5AC_rename (H5F_t *f, const H5AC_class_t *type, haddr_t old,
		    haddr_t new);
herr_t H5AC_set (H5F_t *f, const H5AC_class_t *type, haddr_t addr,
		 void *thing);

#define H5AC_find(F,TYPE,ADDR,UDATA)					      \
   (((F)->shared->cache->slot[H5AC_HASH(F,ADDR)].type==(TYPE) &&	      \
     (F)->shared->cache->slot[H5AC_HASH(F,ADDR)].addr==(ADDR)) ?	      \
    (F)->shared->cache->slot[H5AC_HASH(F,ADDR)].thing :			      \
    H5AC_find_f (F, TYPE, ADDR, UDATA))
      

#endif /* !_H5ACprivate_H */
