/*
 * Copyright (C) 1997 National Center for Supercomputing Applications.
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Thursday, September 18, 1997
 */
#define H5G_PACKAGE	/*suppress error message about including H5Gpkg.h*/

#include <H5private.h>			/*library			*/
#include <H5Eprivate.h>			/*error handling		*/
#include <H5Gpkg.h>			/*symbol tables			*/
#include <H5Hprivate.h>			/*heap functions		*/
#include <H5MMprivate.h>		/*memory management		*/
#include <H5Oprivate.h>			/*object header messages	*/

/*
 * FEATURE: If this macro is defined then H5G_shadow_check() is occassionally
 *	    (actually, quite often) called to check the consistency of the
 *	    shadow table.  If there's something wrong with the table then
 *	    abort() is called. Shadow table checking is a rather expensive
 *	    operation.
 */
/* #define H5G_DEBUG_SHADOWS */

#define PABLO_MASK	H5G_shadow_mask

/* Is the interface initialized? */
static hbool_t interface_initialize_g = FALSE;

typedef struct H5G_hash_t {
   haddr_t		dir_addr;
   H5G_shadow_t		*head;
   struct H5G_hash_t	*next;
   struct H5G_hash_t	*prev;
} H5G_hash_t;


/*-------------------------------------------------------------------------
 * Function:	H5G_shadow_check
 *
 * Purpose:	Checks the shadow data structures for validity.  This is a
 *		debugging function only--it aborts on failure!
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Sunday, September 21, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5G_DEBUG_SHADOWS
void
H5G_shadow_check (H5F_t *f)
{
   H5G_hash_t	*hash=NULL;
   H5G_shadow_t	*shadow=NULL, *prev_shadow=NULL;
   uintn	idx;
   hbool_t	shadow_error=FALSE;
   uintn	nerrors=0;
   static int	ncalls=0;

   ncalls++;

   for (idx=0; idx<f->nshadows; idx++) {
      for (hash=f->shadow[idx]; hash; hash=hash->next) {
	 for (shadow=hash->head,prev_shadow=NULL;
	      shadow;
	      shadow=shadow->next) {
	    shadow_error = FALSE;
	    
	    /* Each shadow has a name and the names are in order */
	    if (!shadow->name) {
	       fprintf (stderr, "name=NULL, ");
	       shadow_error = TRUE;
	    }
	    if (prev_shadow && strcmp (prev_shadow->name, shadow->name)>=0) {
	       fprintf (stderr, "names not sorted, ");
	       shadow_error = TRUE;
	    }
	    
	    /* Valid directory addresses */
	    if (shadow->dir_addr<0 || (shadow->dir_addr==0 && idx!=0)) {
	       fprintf (stderr, "dir_addr=%lu, ",
			(unsigned long)(shadow->dir_addr));
	       shadow_error = TRUE;
	    } else if (shadow->dir_addr!=hash->dir_addr) {
	       fprintf (stderr, "dir_addr=%lu (not %lu), ",
			(unsigned long)(shadow->dir_addr),
			(unsigned long)(hash->dir_addr));
	    }
	    
	    /* Linked to symbol table entry */
	    if (shadow->main && shadow!=shadow->main->shadow) {
	       fprintf (stderr, "entry linkage problem, ");
	       shadow_error = TRUE;
	    }

	    /* Shadow linked list is consistent */
	    if (shadow->prev && prev_shadow!=shadow->prev) {
	       fprintf (stderr, "shadow linked list problem, ");
	       shadow_error = TRUE;
	    }
	    prev_shadow = shadow;

	    /* If an error occurred then print other info */
	    if (shadow_error) {
	       fprintf (stderr, "idx=%u, shadow=0x%08lx, dir_addr=%lu\n",
			idx, (unsigned long)shadow,
			(unsigned long)(shadow->dir_addr));
	       nerrors++;
	    }
	 }
      }
   }
   if (nerrors) {
      fprintf (stderr, "Error in H5G_shadow_check, call %d\n",  ncalls);
      abort ();
   }
}
#endif


/*-------------------------------------------------------------------------
 * Function:	H5G_shadow_p
 *
 * Purpose:	Determines if ENT is a shadow or a real symbol table entry.
 *
 * Return:	Success:	Non-zero if ENT is a shadow; zero otherwise.
 *
 *		Failure:	FALSE
 *
 * Programmer:	Robb Matzke
 *              Thursday, September 18, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5G_shadow_p (H5G_entry_t *ent)
{
   H5G_shadow_t		tmp;
   size_t 		delta = (char*)&(tmp.entry) - (char*)&tmp;
   hbool_t		ret_value = FALSE;
   
   FUNC_ENTER (H5G_shadow_p, NULL, FALSE);

   if (!ent || !ent->shadow) return FALSE;
   ret_value = ((char*)ent - (char*)(ent->shadow) == delta);

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_shadow_dissociate
 *
 * Purpose:	Removes the association between a shadow and its entry or an
 *		entry and its shadow.  The ENT argument can be a shadow or a
 *		cached entry.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, September 18, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_shadow_dissociate (H5G_entry_t *ent)
{
   FUNC_ENTER (H5G_shadow_dissociate, NULL, FAIL);

   if (H5G_shadow_p (ent)) {
      if (ent->shadow->main) {
	 ent->shadow->main->shadow = NULL;
	 ent->shadow->main = NULL;
      }
   } else if (ent && ent->shadow) {
      ent->shadow->main = NULL;
      ent->shadow = NULL;
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_shadow_sync
 *
 * Purpose:	Synchronizes a shadow with an entry by copying the
 *		shadow contents to the entry if the shadow is dirty,
 *		and then clearing the shadow dirty bit.  You may call this
 *		function with either a shadow or a real entry.
 *
 *		If ENT is a shadow, then the shadow is synchronized only if
 *		the entry is currently cached.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, September 18, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_shadow_sync (H5G_entry_t *ent)
{
   H5G_shadow_t	*shadow = NULL;
   FUNC_ENTER (H5G_shadow_sync, NULL, FAIL);

   /*
    * If the caller supplied us with a shadow instead of the main entry, then
    * adjust the arguments.
    */
   if (H5G_shadow_p (ent)) {
      shadow = ent->shadow;
      ent = shadow->main;
   } else {
      shadow = ent->shadow;
   }

   if (shadow && shadow->entry.dirty) {
      if (!ent) {
	 /* Main entry is not cached */
	 HRETURN_ERROR (H5E_SYM, H5E_NOTCACHED, FAIL);
      }
      *ent = shadow->entry;
      shadow->entry.dirty = FALSE;
   }
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_shadow_list
 *
 * Purpose:	Returns a doubly linked list of shadows for the symbol
 *		table whose header address is DIR_ADDR.
 *
 * Return:	Success:	Ptr shadow list or null.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, September 17, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_shadow_t *
H5G_shadow_list (H5F_t *f, haddr_t dir_addr)
{
   uintn	idx = dir_addr % f->shared->nshadows;
   H5G_hash_t	*bucket = NULL;

   FUNC_ENTER (H5G_shadows, NULL, NULL);

   for (bucket=f->shared->shadow[idx]; bucket; bucket=bucket->next) {
      if (bucket->dir_addr==dir_addr) {
	 HRETURN (bucket->head);
      }
   }
   FUNC_LEAVE (NULL);
}
   

/*-------------------------------------------------------------------------
 * Function:	H5G_shadow_assoc_node
 *
 * Purpose:	Given a new symbol table node and a symbol table header
 *		address, associate entries in that node with their shadow if
 *		they have one.
 *
 * 		SYM must be an uncached or protected symbol table node.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL, if an error occurs then none of the
 *				entries are associated with shadows.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, September 17, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_shadow_assoc_node (H5F_t *f, H5G_node_t *sym, H5G_ac_ud1_t *ac_udata)
{
   H5G_shadow_t *shadow = NULL;
   const char	*s = NULL;
   intn		i = 0;
   haddr_t	heap_addr;

   FUNC_ENTER (H5G_shadow_assoc_node, NULL, FAIL);

   /* Check arguments */
   assert (f);			/* The file			*/
   assert (sym);		/* The symbol table node	*/
   assert (ac_udata);		/* The symbol table header info	*/

#ifdef H5G_DEBUG_SHADOWS
   H5G_shadow_check (f);
#endif

   if ((shadow=H5G_shadow_list (f, ac_udata->dir_addr))) {
      heap_addr = ac_udata->heap_addr;

      while (i<sym->nsyms && shadow) {
	 /* Advance the Entry ptr until it gets to the next shadow. */
	 while (i<sym->nsyms &&
		(s=H5H_peek (f, heap_addr, sym->entry[i].name_off)) &&
		strcmp (s, shadow->name)<0) i++;

	 /* Advance the Shadow ptr until it gets to the next entry. */
	 while (i<sym->nsyms && s && shadow &&
		strcmp (s,  shadow->name)>0) shadow = shadow->next;

	 /* Did we find a match? */
	 if (i<sym->nsyms && s && shadow && !strcmp (s,  shadow->name)) {
	    shadow->main = sym->entry + i;
	    sym->entry[i].shadow = shadow;
	    i++;
	    shadow = shadow->next;
	 }
      }
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_shadow_open
 *
 * Purpose:	Given a handle to an already open object, or given a
 *		pointer to the cached symbol table entry for that
 *		object, open the object (again) and return a handle
 *		to it.
 *
 * 		DIR can be the null pointer if `ent' is the root entry.
 *
 * Return:	Success:	Handle to open object
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, September 18, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_entry_t *
H5G_shadow_open (H5F_t *f, H5G_entry_t *dir, H5G_entry_t *ent)
{
   H5G_shadow_t	*shadow = NULL;
   H5O_stab_t 	stab;
   const char 	*s = NULL;
   H5G_hash_t	*hash = NULL;
   H5G_shadow_t	*hash_ent = NULL, *prev_ent = NULL;
   uintn	idx;
   H5O_name_t	name_mesg = {NULL};
   H5G_entry_t	*ret_value = NULL;
   haddr_t	dir_addr;
   
   FUNC_ENTER (H5G_shadow_open, NULL, NULL);

   /* check args */
   assert (f);
   assert (ent==f->shared->root_sym || dir);
   assert (ent);
   dir_addr = dir ? dir->header : 0;

   if ((shadow = ent->shadow)) {
      /*
       * Object is already open.  Open it again.
       */
      shadow->nrefs += 1;
      HRETURN (&(shadow->entry));
   }

   
   shadow = H5MM_xcalloc (1, sizeof(H5G_shadow_t));
   if (ent==f->shared->root_sym && 0==dir_addr) {
      /*
       * We're opening the root entry.
       */
      if (H5O_read (f, NO_ADDR, ent, H5O_NAME, 0, &name_mesg)) {
	 shadow->name = H5MM_xstrdup (name_mesg.s);
	 H5O_reset (H5O_NAME, &name_mesg);
      } else {
	 shadow->name = H5MM_xstrdup ("Root Object");
      }

   } else {
      /*
       * Some entry other than the root.
       */
      if (NULL==H5O_read (f, NO_ADDR, dir, H5O_STAB, 0, &stab)) {
	 HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, NULL);
      }
      if (NULL==(s=H5H_peek (f, stab.heap_addr, ent->name_off))) {
	 HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, NULL);
      }
      shadow->name = H5MM_xstrdup (s);
   }

   /*
    * Build the new shadow.
    */
   ent->shadow = shadow;
   shadow->main = ent;
   shadow->nrefs = 1;
   shadow->entry = *ent;
   shadow->entry.dirty = FALSE;
   shadow->dir_addr = dir_addr;

   /*
    * Link it into the shadow heap
    */
   idx = dir_addr % f->shared->nshadows;
   for (hash=f->shared->shadow[idx]; hash; hash=hash->next) {
      if (hash->dir_addr==dir_addr) break;
   }
   if (!hash) {
      hash = H5MM_xcalloc (1, sizeof(H5G_hash_t));
      hash->dir_addr = dir_addr;
      hash->next = f->shared->shadow[idx];
      f->shared->shadow[idx] = hash;
      if (hash->next) hash->next->prev = hash;
   }
   if (hash->head) {
      for (hash_ent=hash->head,prev_ent=NULL;
	   hash_ent;
	   hash_ent=hash_ent->next) {
	 if (strcmp (shadow->name, hash_ent->name)<0) break;
	 prev_ent = hash_ent;
      }
      if (hash_ent) {
	 /* Insert SHADOW before HASH_ENT */
	 if (hash_ent->prev) hash_ent->prev->next = shadow;
	 else hash->head = shadow;
	 shadow->prev = hash_ent->prev;
	 shadow->next = hash_ent;
	 hash_ent->prev = shadow;
      } else {
	 /* Append SHADOW to list */
	 assert (prev_ent && NULL==prev_ent->next);
	 prev_ent->next = shadow;
	 shadow->prev = prev_ent;
      }
   } else {
      /* Insert shadow at head of list */
      shadow->next = hash->head;
      if (hash->head) hash->head->prev = shadow;
      hash->head = shadow;
   }

#ifdef H5G_DEBUG_SHADOWS
   H5G_shadow_check (f);
#endif

   ret_value = &(shadow->entry);

 done:
   if (!ret_value) {
      if (shadow) {
	 H5MM_xfree (shadow->name);
	 H5MM_xfree (shadow);
      }
   }
   
   FUNC_LEAVE (ret_value);
}



/*-------------------------------------------------------------------------
 * Function:	H5G_shadow_close
 *
 * Purpose:	Closes an open object.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, September 18, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_shadow_close (H5F_t *f, H5G_entry_t *ent)
{
   uintn	idx;
   H5G_hash_t	*hash=NULL;
   H5G_shadow_t	*hash_ent=NULL, *shadow=NULL;
   
   FUNC_ENTER (H5G_shadow_close, NULL, FAIL);

   /* check args */
   assert (ent);
   assert (H5G_shadow_p (ent));
   assert (ent->shadow->nrefs>0);
   shadow = ent->shadow;

   /* clean the shadow */
   if (1==shadow->nrefs && ent->dirty) {
      if (!shadow->main &&
	  NULL==H5G_stab_find (f, shadow->dir_addr, NULL, shadow->name)) {
	 HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
      }
      assert (shadow->main);
      *(shadow->main) = *ent;
      ent->dirty = FALSE;
   }

   /* close */
   shadow->nrefs -= 1;

   if (0==shadow->nrefs) {
      /* dissociate shadow and entry */
      H5G_shadow_dissociate (ent);

      /* find symtabs shadow list */
      idx = shadow->dir_addr % f->shared->nshadows;
      for (hash=f->shared->shadow[idx]; hash; hash=hash->next) {
	 if (hash->dir_addr==shadow->dir_addr) break;
      }
      assert (hash);

      /* find shadow in shadow list */
      for (hash_ent=hash->head; hash_ent; hash_ent=hash_ent->next) {
	 if (hash_ent==shadow) break;
      }
      assert (hash_ent);

      /* remove shadow from shadow list */
      if (hash_ent->prev) hash_ent->prev->next = hash_ent->next;
      else hash->head = hash_ent->next;
      if (hash_ent->next) hash_ent->next->prev = hash_ent->prev;
      H5MM_xfree (shadow->name);
      H5MM_xfree (shadow);

      /* remove symtab's shadow list if empty */
      if (!hash->head) {
	 if (hash->prev) hash->prev->next = hash->next;
	 else f->shared->shadow[idx] = hash->next;
	 if (hash->next) hash->next->prev = hash->prev;
	 H5MM_xfree (hash);
      }
   }

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5G_shadow_move
 *
 * Purpose:	Moves the SHADOW for some entry to correspond to a
 *		NEW_ENTRY. The DIR_ADDR is the address for the directory
 *		which contains NEW_ENTRY.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_shadow_move (H5F_t *f, H5G_shadow_t *shadow, const char *new_name,
		 H5G_entry_t *new_entry, haddr_t dir_addr)
{
   H5G_hash_t	*hash;
   uintn	idx;
   
   FUNC_ENTER (H5G_shadow_move, NULL, FAIL);

   assert (shadow);
   assert (new_entry);
   assert (dir_addr>0);

   if (0==shadow->dir_addr) {
      /*
       * We're moving the shadow for the root object.  This simplifies things
       * greatly since it implies that this is the only shadow currently
       * defined for the entire file.
       */
      idx = dir_addr % f->shared->nshadows;
      assert (NULL==f->shared->shadow[idx]); /*Nothing at new idx...	*/
      hash = f->shared->shadow[0];
      assert (hash);			/*..but root idx has something. */
      assert (0==hash->dir_addr);	/*..and it's the root something	*/
      assert (NULL==hash->next);	/*..and just that		*/
      assert (hash->head==shadow);	/*..and exactly that		*/

      /* Move root entry to new hash bucket */
      f->shared->shadow[idx] = hash;
      f->shared->shadow[0] = NULL;
      hash->dir_addr = dir_addr;

      /* Associate SHADOW with NEW_ENTRY */
      shadow->dir_addr = dir_addr;
      shadow->main = new_entry;
      new_entry->shadow = shadow;

      /* Give the shadow a new name */
      H5MM_xfree (shadow->name);
      shadow->name = H5MM_xstrdup (new_name);
      
   } else {
      /*
       * Other shadows never move.
       */
      assert (shadow->dir_addr==dir_addr);
      shadow->main = new_entry;
      new_entry->shadow = shadow;
   }

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5G_shadow_flush
 *
 * Purpose:	Flush all open object information to the main cache.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL if INVALIDATE is non-zero and there are
 *				open objects.
 *
 * Programmer:	Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_shadow_flush (H5F_t *f, hbool_t invalidate)
{
   uintn	idx;
   H5G_hash_t	*hash = NULL;
   H5G_shadow_t	*shadow = NULL;
   intn		nfound=0;

   FUNC_ENTER (H5G_shadow_flush, NULL, FAIL);

   for (idx=0; idx<f->shared->nshadows; idx++) {
      for (hash=f->shared->shadow[idx]; hash; hash=hash->next) {
	 for (shadow=hash->head; shadow; shadow=shadow->next) {
	    /*
	     * If the shadow is dirty, then transfer the shadow info to the
	     * symbol table node.
	     */
	    if (shadow->entry.dirty) {
	       if (!shadow->main &&
		   NULL==H5G_stab_find (f, shadow->dir_addr, NULL,
					shadow->name)) {
		  HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
	       }
	       assert (shadow->main);
	       *(shadow->main) = shadow->entry;
	       shadow->entry.dirty = FALSE;
	       nfound++;

	    }
#ifndef NDEBUG
	    /*
	     * This is usually a bad thing--an hdf5 programmer forgot to close
	     * some object before closing the file.  Since this is hard to
	     * debug, we'll be nice and print the names here.  We don't know
	     * the full name, but we'll print the file address (relative to
	     * the boot block) of the object header for the directory that
	     * contains the open object.
	     */
	    if (invalidate) {
	       fprintf (stderr, "Open object <%lu>/%s",
			(unsigned long)(shadow->dir_addr),
			shadow->name);
	       if (shadow->nrefs>1) {
		  fprintf (stderr,  " (%d times)", shadow->nrefs);
	       }
	       fputc ('\n', stderr);
	    }
#endif
	 }
      }
   }

   if (invalidate && nfound) {
      /*
       * No clean easy way to do this, just leak the memory.  If we free a
       * shadow and then something else tries to access it (perhaps to close
       * it) then they trample on freed memory.
       */
      HRETURN_ERROR (H5E_SYM, H5E_UNSUPPORTED, FAIL);
   }

   FUNC_LEAVE (SUCCEED);
}
