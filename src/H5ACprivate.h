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
   void		*(*load)(hdf5_file_t*, haddr_t addr, const void *udata);
   herr_t	(*flush)(hdf5_file_t*, hbool_t dest, haddr_t addr,
			 void *thing);
} H5AC_class_t;

/*
 * A cache has a certain number of entries.  Objects are mapped into a
 * cache entry by hashing the object's file address.  Each file has its
 * own cache, an array of slots.
 */
#define H5AC_NSLOTS	10330	/*prime number tend to work best	*/
#define H5AC_HASH(ADDR)	((unsigned)(ADDR) % H5AC_NSLOTS)

typedef struct H5AC_cache_t {
   const H5AC_class_t *type;	/*type of object stored here		*/
   haddr_t	addr;		/*file address for object		*/
   void		*thing;		/*the thing which is cached		*/
} H5AC_cache_t;

/*
 * Library prototypes.
 */
herr_t H5AC_dest (hdf5_file_t *f);
void *H5AC_find_f (hdf5_file_t *f, const H5AC_class_t *type, haddr_t addr,
		   const void *udata);
herr_t H5AC_flush (hdf5_file_t *f, const H5AC_class_t *type, haddr_t addr,
		   hbool_t destroy);
herr_t H5AC_new (hdf5_file_t *f);
herr_t H5AC_rename (hdf5_file_t *f, const H5AC_class_t *type,
		    haddr_t old, haddr_t new);
herr_t H5AC_set (hdf5_file_t *f, const H5AC_class_t *type, haddr_t addr,
		 void *thing);

#define H5AC_find(F,TYPE,ADDR,UDATA)					      \
   (((F)->cache[H5AC_HASH(ADDR)].type==(TYPE) &&			      \
     (F)->cache[H5AC_HASH(ADDR)].addr==(ADDR)) ?			      \
    (F)->cache[H5AC_HASH(ADDR)].thing :					      \
    H5AC_find_f (F, TYPE, ADDR, UDATA))

#endif /* !_H5ACprivate_H */
