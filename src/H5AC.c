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
#include <H5private.h>
#include <H5ACprivate.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>

/*
 * Sorting the cache by address before flushing is sometimes faster
 * than flushing in cache order.
 */
/* #define SORT_BY_ADDR */

/*
 * Debug H5AC_protect() and H5AC_unprotect() by insuring that nothing
 * accesses protected objects.  NDEBUG must not be defined in order for
 * this to have any effect.
 */
/* #define H5AC_DEBUG_PROTECT */

/*
 * Private file-scope variables.
 */
#define PABLO_MASK	H5AC_mask
static int	interface_initialize_g = FALSE;		/*initialized?*/

#ifdef SORT_BY_ADDR
static H5AC_t *current_cache_g = NULL;			/*for sorting */
#endif


/*-------------------------------------------------------------------------
 * Function:	H5AC_new
 *
 * Purpose:	Initialize the cache just after a file is opened.  The
 *		SIZE_HINT is the number of cache slots desired.  If you
 *		pass an invalid value then H5AC_NSLOTS is used.  You can
 *		turn off caching by using 1 for the SIZE_HINT value.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul  9 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_new (hdf5_file_t *f, intn size_hint)
{
   FUNC_ENTER (H5AC_new, NULL, FAIL);

   assert (f);
   assert (NULL==f->cache);
   if (size_hint<1) size_hint = H5AC_NSLOTS;

   f->cache = H5MM_xcalloc (1, sizeof (H5AC_t));
   f->cache->nslots = size_hint;
   f->cache->slot = H5MM_xcalloc (f->cache->nslots, sizeof (H5AC_slot_t));

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5AC_dest
 *
 * Purpose:	Flushes all data to disk and destroys the cache.
 *		This function fails if any object are protected since the
 *		resulting file might not be consistent.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
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

#if defined(H5AC_DEBUG_PROTECT) && !defined(NDEBUG)
   {
      intn	i;
      for (i=0; i<f->cache->nslots; i++) {
	 f->cache->slot[i].prot = H5MM_xfree (f->cache->slot[i].prot);
	 f->cache->slot[i].aprots = 0;
	 f->cache->slot[i].nprots = 0;
      }
   }
#endif

   f->cache->slot = H5MM_xfree (f->cache->slot);
   f->cache->nslots = 0;
   f->cache = H5MM_xfree (f->cache);
   FUNC_LEAVE (SUCCEED);
}
   

/*-------------------------------------------------------------------------
 * Function:	H5AC_find_f
 *
 * Purpose:	Given an object type and the address at which that object
 *		is located in the file, return a pointer to the object.
 *		The optional UDATA structure is passed down to the function
 *		that is responsible for loading the object into memory.
 *		The pointer is guaranteed to be valid until the next call
 *		to an H5AC function (if you want a pointer which is valid
 *		indefinately then see H5AC_protect()).
 *
 * 		If H5AC_DEBUG_PROTECT is defined then this function also
 *		checks that the requested object is not currently
 *		protected since it is illegal to modify a protected object
 *		except through the pointer returned by H5AC_protect().
 *
 * Return:	Success:	Pointer to the object.  The pointer is
 *				valid until some other cache function
 *				is called.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
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
H5AC_find_f (hdf5_file_t *f, const H5AC_class_t *type, haddr_t addr,
	     void *udata)
{
   unsigned	idx;
   herr_t	status;
   void		*thing = NULL;
   herr_t	(*flush)(hdf5_file_t*,hbool_t,haddr_t,void*)=NULL;
   H5AC_slot_t	*slot = NULL;

   FUNC_ENTER (H5AC_find, NULL, NULL);

   assert (f);
   assert (f->cache);
   assert (type);
   assert (type->load);
   assert (type->flush);
   idx = H5AC_HASH (f, addr);
   slot = f->cache->slot + idx;
   
   /*
    * Return right away if the item is in the cache.
    */
   if (slot->type==type && slot->addr==addr) {
      HRETURN (slot->thing);
   }

   /*
    * Fail if the item in the cache is at the correct address but is
    * of the wrong type.
    */
   if (slot->type && slot->type!=type && slot->addr==addr) {
      HRETURN_ERROR (H5E_CACHE, H5E_BADTYPE, NULL);
   }

#if defined(H5AC_DEBUG_PROTECT) && !defined(NDEBUG)
   /*
    * Check that the requested thing isn't protected, for protected things
    * can only be modified through the pointer already handed out by the
    * H5AC_protect() function.
    */
   {
      intn i;
      for (i=0; i<slot->nprots; i++) {
	 assert (addr!=slot->prot[i].addr);
      }
   }
#endif

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
   if (slot->type) {
      flush = slot->type->flush;
      status = (flush)(f, TRUE, slot->addr, slot->thing);
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
   slot->type = type;
   slot->addr = addr;
   slot->thing = thing;

   FUNC_LEAVE (thing);
}


/*-------------------------------------------------------------------------
 * Function:	H5AC_compare
 *
 * Purpose:	Compare two hash entries by address.  Unused entries are
 *		all equal to one another and greater than all used entries.
 *
 * Return:	Success:	-1, 0, 1
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 12 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef SORT_BY_ADDR
static int
H5AC_compare (const void *_a, const void *_b)
{
   intn 	a = *((const intn *)_a);
   intn		b = *((const intn *)_b);

   assert (current_cache_g);

   if (NULL==current_cache_g.slot[a].type) return 1;
   if (NULL==current_cache_g.slot[b].type) return -1;

   if (current_cache_g.slot[a].addr < current_cache_g.slot[b].addr) return -1;
   if (current_cache_g.slot[a].addr > current_cache_g.slot[b].addr) return 1;
   return 0;
}
#endif


/*-------------------------------------------------------------------------
 * Function:	H5AC_flush
 *
 * Purpose:	Flushes (and destroys if DESTROY is non-zero) the specified
 *		entry from the cache.  If the entry TYPE is CACHE_FREE and
 *		ADDR is zero then all types of entries are flushed. If TYPE
 *		is CACHE_FREE and ADDR is non-zero, then whatever is cached
 *		at ADDR is flushed.  Otherwise the thing at ADDR is flushed
 *		if it is the correct type.
 *
 * 		If there are protected objects they will not be flushed.
 *		However, an attempt will be made to flush all non-protected
 *		items before this function returns failure.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
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
   uintn	i;
   herr_t	status;
   herr_t	(*flush)(hdf5_file_t*,hbool_t,haddr_t,void*)=NULL;
   H5AC_slot_t	*slot;
   intn		*map=NULL;

   FUNC_ENTER (H5AC_flush, NULL, FAIL);

   assert (f);
   assert (f->cache);
   i = H5AC_HASH (f, addr);

   if (0==addr) {

#ifdef SORT_BY_ADDR
      /*
       * Sort the cache entries by address since flushing them in
       * ascending order by address may be much more efficient.
       */
      map = H5MM_xmalloc (f->cache->nslots * sizeof(intn));
      for (i=0; i<f->cache->nslots; i++) map[i] = i;
      assert (NULL==current_cache_g);
      current_cache_g = f->cache;
      HDqsort (map, f->cache->nslots, sizeof(intn), H5AC_compare);
      current_cache_g = NULL;
#endif

      /*
       * Look at all cache entries.
       */
      for (i=0; i<f->cache->nslots; i++) {
#ifdef SORT_BY_ADDR
	 slot = f->cache->slot + map[i];
	 if (NULL==slot->type) break; /*the rest are empty*/
#else
	 slot = f->cache->slot + i;
	 if (NULL==slot->type) continue;
#endif
	 if (!type || type==slot->type) {
	    flush = slot->type->flush;
	    status = (flush)(f, destroy, slot->addr, slot->thing);
	    if (status<0) {
	       map = H5MM_xfree (map);
	       HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL);
	    }
	    if (destroy) slot->type = NULL;
	 }
      }
      map = H5MM_xfree (map);

   } else if ((!type || f->cache->slot[i].type==type) &&
	      f->cache->slot[i].addr==addr) {
      /*
       * Flush just this entry.
       */
      flush = f->cache->slot[i].type->flush;
      status =  (flush) (f, destroy, f->cache->slot[i].addr,
			 f->cache->slot[i].thing);
      if (status<0) {
	 HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL);
      }
      if (destroy) f->cache->slot[i].type = NULL;

   }

   /*
    * If there are protected objects then fail.  However, everything
    * else should have been flushed.
    */
   if (f->cache->nprots>0) {
      HRETURN_ERROR (H5E_CACHE, H5E_PROTECT, FAIL);
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
 * 		If H5AC_DEBUG_PROTECT is defined then this function checks
 *		that the object being inserted isn't a protected object.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
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
   uintn	idx;
   herr_t	(*flush)(hdf5_file_t*,hbool_t,haddr_t,void*)=NULL;
   H5AC_slot_t	*slot = NULL;

   FUNC_ENTER (H5AC_set, NULL, FAIL);

   assert (f);
   assert (f->cache);
   assert (type);
   assert (type->flush);
   assert (addr>=0);
   assert (thing);
   idx = H5AC_HASH (f, addr);
   slot = f->cache->slot + idx;

#if defined(H5AC_DEBUG_PROTECT) && !defined(NDEBUG)
   {
      intn i;
      for (i=0; i<slot->nprots; i++) {
	 assert (addr!=slot->prot[i].addr);
      }
   }
#endif
   
   if (slot->type) {
      flush = slot->type->flush;
      status = (flush)(f, TRUE, slot->addr, slot->thing);
      if (status<0) {
	 HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL);
      }
   }

   slot->type = type;
   slot->addr = addr;
   slot->thing = thing;

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5AC_rename
 *
 * Purpose:	Use this function to notify the cache that an object's
 *		file address changed.
 *
 * 		If H5AC_DEBUG_PROTECT is defined then this function checks
 *		that the old and new addresses don't correspond to the
 *		address of a protected object.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
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
   uintn	old_idx, new_idx;
   herr_t	(*flush)(hdf5_file_t*, hbool_t, haddr_t, void*);
   herr_t	status;

   FUNC_ENTER (H5AC_rename, NULL, FAIL);

   assert (f);
   assert (f->cache);
   assert (type);
   assert (old_addr>=0);
   assert (new_addr>=0);
   old_idx = H5AC_HASH (f, old_addr);
   new_idx = H5AC_HASH (f, new_addr);

#if defined(H5AC_DEBUG_PROTECT) && !defined(NDEBUG)
   {
      int i;

      for (i=0; i<f->cache->slot[old_idx].nprots; i++) {
	 assert (old_addr!=f->cache->slot[old_idx].prot[i].addr);
      }
      for (i=0; i<f->cache->slot[new_idx].nprots; i++) {
	 assert (new_addr!=f->cache->slot[new_idx].prot[i].addr);
      }
   }
#endif
      
   /*
    * We don't need to do anything if the object isn't cached or if the
    * new hash value is the same as the old one.
    */
   if (f->cache->slot[old_idx].type!=type ||
       f->cache->slot[old_idx].addr!=old_addr) {
      HRETURN (SUCCEED);
   }
   if (old_idx==new_idx) {
      f->cache->slot[old_idx].addr = new_addr;
      HRETURN (SUCCEED);
   }
      
   /*
    * Free the item from the destination cache line.
    */
   if (f->cache->slot[new_idx].type) {
      flush = f->cache->slot[new_idx].type->flush;
      status = (flush)(f, TRUE, f->cache->slot[new_idx].addr,
		       f->cache->slot[new_idx].thing);
      if (status<0) {
	 HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL);
      }
   }

   /*
    * Move the source to the destination (it might not be cached)
    */
   f->cache->slot[new_idx].type = f->cache->slot[old_idx].type;
   f->cache->slot[new_idx].addr = new_addr;
   f->cache->slot[new_idx].thing = f->cache->slot[old_idx].thing;
   f->cache->slot[old_idx].type = NULL;

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5AC_protect
 *
 * Purpose:	Similar to H5AC_find() except the object is removed from
 *		the cache and given to the caller, preventing other parts
 *		of the program from modifying the protected object or
 *		preempting it from the cache.
 *
 * 		The caller must call H5AC_unprotect() when finished with
 *		the pointer.
 *
 * 		If H5AC_DEBUG_PROTECT is defined then we check that the
 *		requested object isn't already protected.
 *
 * Return:	Success:	Ptr to the object.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Sep  2 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5AC_protect (hdf5_file_t *f, const H5AC_class_t *type, haddr_t addr,
	      void *udata)
{
   int		idx;
   void		*thing = NULL;
   H5AC_slot_t	*slot = NULL;
   
   FUNC_ENTER (H5AC_protect, NULL, NULL);

   /* check args */
   assert (f);
   assert (f->cache);
   assert (type);
   assert (type->load);
   assert (type->flush);
   idx = H5AC_HASH (f, addr);
   slot = f->cache->slot+idx;

   if (slot->type==type && slot->addr==addr) {
      /*
       * The object is already cached; simply remove it from the cache.
       */
      thing = slot->thing;
      slot->type = NULL;
      slot->addr = 0;
      slot->thing = NULL;

   } else if (slot->type && slot->addr==addr) {
      /*
       * Right address but wrong object type.
       */
      HRETURN_ERROR (H5E_CACHE, H5E_BADTYPE, NULL);
	      
   } else {
#if defined(H5AC_DEBUG_PROTECT) && !defined(NDEBUG)
      /*
       * Check that the requested thing isn't protected, for protected things
       * can only be modified through the pointer already handed out by the
       * H5AC_protect() function.
       */
      intn i;
      for (i=0; i<slot->nprots; i++) {
	 assert (addr!=slot->prot[i].addr);
      }
#endif

      /*
       * Load a new thing.  If it can't be loaded, then return an error
       * without preempting anything.
       */
      if (NULL==(thing=(type->load)(f, addr, udata))) {
	 HRETURN_ERROR (H5E_CACHE, H5E_CANTLOAD, NULL);
      }
   }

#if defined(H5AC_DEBUG_PROTECT) && !defined(NDEBUG)
   /*
    * Add the protected object to the protect debugging fields of the
    * cache.
    */
   if (slot->nprots>=slot->aprots) {
      slot->aprots += 10;
      slot->prot = H5MM_xrealloc (slot->prot,
				  slot->aprots * sizeof(H5AC_prot_t));
   }
   slot->prot[slot->nprots].type = type;
   slot->prot[slot->nprots].addr = addr;
   slot->prot[slot->nprots].thing = thing;
   slot->nprots += 1;
#endif

   f->cache->nprots += 1;
   FUNC_LEAVE (thing);
}


/*-------------------------------------------------------------------------
 * Function:	H5AC_unprotect
 *
 * Purpose:	This function should be called to undo the effect of
 *		H5AC_protect().  The TYPE and ADDR arguments should be the
 *		same as the corresponding call to H5AC_protect() and the
 *		THING argument should be the value returned by H5AC_protect().
 *
 * 		If H5AC_DEBUG_PROTECT is defined then this function fails
 *		if the TYPE and ADDR arguments are not what was used when the
 *		object was protected or if the object was never protected.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Sep  2 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_unprotect (hdf5_file_t *f, const H5AC_class_t *type, haddr_t addr,
		void *thing)
{
   herr_t	status;
   uintn	idx;
   herr_t	(*flush)(hdf5_file_t*,hbool_t,haddr_t,void*)=NULL;
   H5AC_slot_t	*slot = NULL;
   
   FUNC_ENTER (H5AC_unprotect, NULL, FAIL);

   /* check args */
   assert (f);
   assert (f->cache);
   assert (type);
   assert (type->flush);
   assert (addr>=0);
   assert (thing);
   idx = H5AC_HASH (f, addr);
   slot = f->cache->slot + idx;

   /*
    * Flush any object already in the cache at that location.  It had
    * better not be another copy of the protected object.
    */
   if (slot->type) {
      assert (slot->addr!=addr);
      flush = slot->type->flush;
      status = (flush)(f, TRUE, slot->addr, slot->thing);
      if (status<0) {
	 HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL);
      }
   }

#if defined(H5AC_DEBUG_PROTECT) && !defined(NDEBUG)
   /*
    * Remove the object's protect data to indicate that it is no longer
    * protected.
    */
   {
      int found, i;
      for (i=0,found=FALSE; i<slot->nprots && !found; i++) {
	 if (addr==slot->prot[i].addr) {
	    assert (slot->prot[i].type==type);
	    HDmemmove (slot->prot+i, slot->prot+i+1,
		       ((slot->nprots-i)-1) * sizeof(H5AC_prot_t));
	    slot->nprots -= 1;
	    found = TRUE;
	 }
      }
      assert (found);
   }
#endif

   /*
    * Insert the object back into the cache; it is no longer protected.
    */
   slot->type = type;
   slot->addr = addr;
   slot->thing = thing;
   f->cache->nprots -= 1;

   FUNC_LEAVE (SUCCEED);
}
