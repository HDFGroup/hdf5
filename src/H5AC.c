/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		hdf5cache.c
 * 			Jul  9 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Functions in this file implement a cache for
 *			things which exist on disk.  All "things" associated
 *			with a particular HDF file share the same cache; each
 *			HDF file has it's own cache.
 *
 * Modifications:
 *
 * 	Robb Matzke, 4 Aug 1997
 *	Added calls to H5E.
 *
 *-------------------------------------------------------------------------
 */
#include <assert.h>
#include "hdf5.h"

#include "H5private.h"
#include "H5ACprivate.h"
#include "H5MMprivate.h"

#define PABLO_MASK	H5AC_mask
#define HASH(addr)	((unsigned)(addr) % H5AC_NSLOTS)

static int	interface_initialize_g = FALSE;		/*initialized?*/


/*-------------------------------------------------------------------------
 * Function:	H5AC_new
 *
 * Purpose:	Initialize the cache just after a file is opened.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  9 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_new (hdf5_file_t *f)
{
   FUNC_ENTER (H5AC_new, NULL, FAIL);

   assert (f);
   assert (NULL==f->cache);

   f->cache = H5MM_xcalloc (H5AC_NSLOTS, sizeof (H5AC_cache_t));

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5AC_dest
 *
 * Purpose:	Flushes all data to disk and destroys the cache.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  9 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_dest (hdf5_file_t *f)
{
   FUNC_ENTER (H5AC_dest, NULL, FAIL);

   assert (f);
   assert (f->cache);
   
   if (H5AC_flush (f, NULL, 0, TRUE)<0) {
      HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL);
   }

   f->cache = H5MM_xfree (f->cache);
   FUNC_LEAVE (SUCCEED);
}
   

/*-------------------------------------------------------------------------
 * Function:	H5AC_find
 *
 * Purpose:	Given an object type and the address at which that object
 *		is located in the file, return a pointer to the object.
 *		The optional UDATA structure is passed down to the function
 *		that is responsible for loading the object into memory.
 *
 * Return:	Success:	Pointer to the object.  The pointer is
 *				valid until some other cache function
 *				is called.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  9 1997
 *
 * Modifications:
 *
 * 	Robb Matzke, 4 Aug 1997
 *	Fails immediately if the cached object is at the correct address
 *	but is of the wrong type.  This happens if the caller doesn't know
 *	what type of object is at the address and calls this function with
 *	various type identifiers until one succeeds (cf., the debugger).
 *
 *-------------------------------------------------------------------------
 */
void *
H5AC_find (hdf5_file_t *f, const H5AC_class_t *type, haddr_t addr,
	   const void *udata)
{
   unsigned	idx = HASH(addr);
   herr_t	status;
   void		*thing = NULL;
   herr_t	(*flush)(hdf5_file_t*,hbool_t,haddr_t,void*)=NULL;

   FUNC_ENTER (H5AC_find, NULL, NULL);

   assert (f);
   assert (f->cache);
   assert (type);
   assert (type->load);
   assert (type->flush);

   /*
    * Return right away if the item is in the cache.
    */
   if (f->cache[idx].type==type && f->cache[idx].addr==addr) {
      return f->cache[idx].thing;
   }

   /*
    * Fail if the item in the cache is at the correct address but is
    * of the wrong type.
    */
   if (f->cache[idx].type && f->cache[idx].addr==addr &&
       f->cache[idx].type!=type) {
      HRETURN_ERROR (H5E_CACHE, H5E_BADTYPE, NULL);
   }

   /*
    * Load a new thing.  If it can't be loaded, then return an error
    * without preempting anything.
    */
   if (NULL==(thing=(type->load)(f, addr, udata))) {
      HRETURN_ERROR (H5E_CACHE, H5E_CANTLOAD, NULL);
   }

   /*
    * Free the previous cache entry if there is one.
    */
   if (f->cache[idx].type) {
      flush = f->cache[idx].type->flush;
      status = (flush)(f, TRUE, f->cache[idx].addr, f->cache[idx].thing);
      if (status<0) {
	 /*
	  * The old thing could not be removed from the stack.
	  * Release the new thing and fail.
	  */
	 if ((type->flush)(f, TRUE, addr, thing)<0) {
	    HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, NULL);
	 }
	 HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, NULL);
      }
   }

   /*
    * Make the cache point to the new thing.
    */
   f->cache[idx].type = type;
   f->cache[idx].addr = addr;
   f->cache[idx].thing = thing;

   FUNC_LEAVE (thing);
}


/*-------------------------------------------------------------------------
 * Function:	H5AC_flush
 *
 * Purpose:	Flushes (and destroys if DESTROY is non-zero) the specified
 *		entry from the cache.  If the entry type is CACHE_FREE then
 *		all types of entries are flushed.  If the ADDR is zero then
 *		all entries of the specified type are flushed.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  9 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_flush (hdf5_file_t *f, const H5AC_class_t *type, haddr_t addr,
	    hbool_t destroy)
{
   uintn	i = HASH(addr);
   herr_t	status;
   herr_t	(*flush)(hdf5_file_t*,hbool_t,haddr_t,void*)=NULL;

   FUNC_ENTER (H5AC_flush, NULL, FAIL);

   assert (f);
   assert (f->cache);

   if (!type || 0==addr) {
      /*
       * Look at all cache entries.
       */
      for (i=0; i<H5AC_NSLOTS; i++) {
	 if (NULL==f->cache[i].type) continue;
	 if ((!type || type==f->cache[i].type) &&
	     (0==addr || addr==f->cache[i].addr)) {
	    flush = f->cache[i].type->flush;
	    status = (flush)(f, destroy, f->cache[i].addr,
			     f->cache[i].thing);
	    if (status<0) {
	       HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL);
	    }
	    if (destroy) f->cache[i].type = NULL;
	 }
      }

   } else if (f->cache[i].type==type && f->cache[i].addr==addr) {
      /*
       * Flush just this entry.
       */
      flush = f->cache[i].type->flush;
      status =  (flush) (f, destroy, f->cache[i].addr, f->cache[i].thing);
      if (status<0) {
	 HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL);
      }
      if (destroy) f->cache[i].type = NULL;

   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5AC_set
 *
 * Purpose:	Adds the specified thing to the cache.  The thing need not
 *		exist on disk yet, but it must have an address and disk
 *		space reserved.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  9 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_set (hdf5_file_t *f, const H5AC_class_t *type, haddr_t addr, void *thing)
{
   herr_t	status;
   uintn	idx = HASH (addr);
   herr_t	(*flush)(hdf5_file_t*,hbool_t,haddr_t,void*)=NULL;

   FUNC_ENTER (H5AC_set, NULL, FAIL);

   assert (f);
   assert (f->cache);
   assert (type);
   assert (type->flush);
   assert (addr>=0);
   assert (thing);

   if (f->cache[idx].type) {
      flush = f->cache[idx].type->flush;
      status = (flush)(f, TRUE, f->cache[idx].addr, f->cache[idx].thing);
      if (status<0) {
	 HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL);
      }
   }

   f->cache[idx].type = type;
   f->cache[idx].addr = addr;
   f->cache[idx].thing = thing;

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5AC_rename
 *
 * Purpose:	Use this function to notify the cache that an object's
 *		file address changed.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  9 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_rename (hdf5_file_t *f, const H5AC_class_t *type,
	     haddr_t old_addr, haddr_t new_addr)
{
   uintn	old_idx = HASH (old_addr);
   uintn	new_idx = HASH (new_addr);
   herr_t	(*flush)(hdf5_file_t*, hbool_t, haddr_t, void*);
   herr_t	status;

   FUNC_ENTER (H5AC_rename, NULL, FAIL);

   assert (f);
   assert (f->cache);
   assert (type);
   assert (old_addr>=0);
   assert (new_addr>=0);

   if (f->cache[old_idx].type!=type || f->cache[old_idx].addr!=old_addr) {
      HRETURN (SUCCEED);
   }

   if (old_idx==new_idx) {
      f->cache[old_idx].addr = new_addr;
      HRETURN (SUCCEED);
   }
      
   /*
    * Free the item from the destination cache line.
    */
   if (f->cache[new_idx].type) {
      flush = f->cache[new_idx].type->flush;
      status = (flush)(f, TRUE, f->cache[new_idx].addr,
		       f->cache[new_idx].thing);
      if (status<0) {
	 HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL);
      }
   }

   /*
    * Move the source to the destination (it might not be cached)
    */
   f->cache[new_idx].type = f->cache[old_idx].type;
   f->cache[new_idx].addr = new_addr;
   f->cache[new_idx].thing = f->cache[old_idx].thing;
   f->cache[old_idx].type = NULL;

   FUNC_LEAVE (SUCCEED);
}

