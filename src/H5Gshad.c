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

#define PABLO_MASK	H5G_shadow_mask

/* Interface initialization */
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT	NULL

typedef struct H5G_hash_t {
   haddr_t		grp_addr;
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
#ifdef H5G_DEBUG
static void
H5G_shadow_check (H5F_t *f)
{
   H5G_hash_t	*hash=NULL;
   H5G_shadow_t	*shadow=NULL, *prev_shadow=NULL;
   uintn	idx;
   hbool_t	shadow_error=FALSE;
   uintn	nerrors=0;
   static int	ncalls=0;

   if (0==ncalls++) {
      fprintf (stderr, "HDF5-DIAG: debugging group shadows (expensive)\n");
   }

   for (idx=0; idx<f->shared->nshadows; idx++) {
      for (hash=f->shared->shadow[idx]; hash; hash=hash->next) {
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
	    
	    /*
	     * Valid group addresses.  The root (which is hashed to entry
	     * zero) always has an undefined group address.
	     */
	    if (idx==0 &&
		!H5F_addr_defined (&(shadow->grp_addr)) &&
		!H5F_addr_defined (&(hash->grp_addr))) {
	       /*
	        * The shadow for the root object always has an undefined
	        * group address.
	        */
	       
	    } else if (!H5F_addr_defined (&(shadow->grp_addr)) ||
		       !H5F_addr_defined (&(hash->grp_addr))) {
	       /*
	        * Non-root objects must have defined group addresses.
	        */
	       fprintf (stderr, "grp_addr=");
	       H5F_addr_print (stderr, &(shadow->grp_addr));
	       fprintf (stderr, ", hash_addr=");
	       H5F_addr_print (stderr, &(hash->grp_addr));
	       fprintf (stderr, ", ");
	       shadow_error = TRUE;
	       
	    } else if (H5F_addr_ne (&(shadow->grp_addr), &(hash->grp_addr))) {
	       /*
	        * Something's wrong with the data structure.  The hash
	        * address should always be the same as the shadow group
	        * address.
	        */
	       fprintf (stderr, "grp_addr=");
	       H5F_addr_print (stderr, &(shadow->grp_addr));
	       fprintf (stderr, " (should be ");
	       H5F_addr_print (stderr, &(hash->grp_addr));
	       fprintf (stderr, "), ");
	       shadow_error = TRUE;
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
	       fprintf (stderr, "idx=%u, shadow=0x%08lx, grp_addr=",
			idx, (unsigned long)shadow);
	       H5F_addr_print (stderr, &(shadow->grp_addr));
	       fprintf (stderr, "\n");
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
   
   FUNC_ENTER (H5G_shadow_p, FALSE);

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
   FUNC_ENTER (H5G_shadow_dissociate, FAIL);

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
   FUNC_ENTER (H5G_shadow_sync, FAIL);

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
	 HRETURN_ERROR (H5E_SYM, H5E_NOTCACHED, FAIL,
			"main entry is not cached");
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
 *		table whose header address is GRP_ADDR.
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
H5G_shadow_list (H5F_t *f, const haddr_t *grp_addr)
{
   uintn	idx = H5F_addr_hash (grp_addr, f->shared->nshadows);
   H5G_hash_t	*bucket = NULL;

   FUNC_ENTER (H5G_shadows, NULL);

   for (bucket=f->shared->shadow[idx]; bucket; bucket=bucket->next) {
      if (0==idx &&
	  !H5F_addr_defined (&(bucket->grp_addr)) &&
	  !H5F_addr_defined (grp_addr)) {
	 HRETURN (bucket->head);/*shadow list for root object*/
      } else if (H5F_addr_eq (&(bucket->grp_addr), grp_addr)) {
	 HRETURN (bucket->head);/*shadow list for other objects*/
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
H5G_shadow_assoc_node (H5F_t *f, H5G_node_t *sym, const H5G_ac_ud1_t *ac_udata)
{
   H5G_shadow_t *shadow = NULL;
   const char	*s = NULL;
   intn		i = 0;
   haddr_t	heap_addr;

   FUNC_ENTER (H5G_shadow_assoc_node, FAIL);

   /* Check arguments */
   assert (f);			/* The file			*/
   assert (sym);		/* The symbol table node	*/
   assert (ac_udata);		/* The symbol table header info	*/

#ifdef H5G_DEBUG
   H5G_shadow_check (f);
#endif

   if ((shadow=H5G_shadow_list (f, &(ac_udata->grp_addr)))) {
      heap_addr = ac_udata->heap_addr;

      while (i<sym->nsyms && shadow) {
	 /* Advance the Entry ptr until it gets to the next shadow. */
	 while (i<sym->nsyms &&
		(s=H5H_peek (f, &heap_addr, sym->entry[i].name_off)) &&
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
 *		If ENT refers to the root object, then GRP can be a null
 *		pointer or a pointer to an entry with an invalid header
 *		address.
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
H5G_shadow_open (H5F_t *f, H5G_entry_t *grp, H5G_entry_t *ent)
{
   H5G_shadow_t	*shadow = NULL;
   H5O_stab_t 	stab;
   const char 	*s = NULL;
   H5G_hash_t	*hash = NULL;
   H5G_shadow_t	*hash_ent = NULL, *prev_ent = NULL;
   uintn	idx;
   H5O_name_t	name_mesg = {NULL};
   H5G_entry_t	*ret_value = NULL;
   haddr_t	grp_header;
   
   FUNC_ENTER (H5G_shadow_open, NULL);

   /* check args */
   assert (f);
   assert (ent);

   if ((shadow = ent->shadow)) {
      /* Object is already open.  Open it again */
      shadow->nrefs += 1;
      HRETURN (&(shadow->entry));
   }

   /*
    * If the root object is being opened then the GRP argument is optional.
    * If it's supplied then it had better have an undefined header address.
    * For all other objects the GRP argument is required to have a valid
    * header address.
    */
   if (ent==f->shared->root_sym) {
      assert (!grp || !H5F_addr_defined (&(grp->header)));
      H5F_addr_undef (&grp_header);
      idx = 0;
   } else {
      assert (grp && H5F_addr_defined (&(grp->header)));
      grp_header = grp->header;
      idx = H5F_addr_hash (&grp_header, f->shared->nshadows);
   }
   
   /*
    * Build the new shadow.
    */
   shadow = H5MM_xcalloc (1, sizeof(H5G_shadow_t));
   ent->shadow = shadow;
   shadow->main = ent;
   shadow->nrefs = 1;
   shadow->entry = *ent;
   shadow->entry.dirty = FALSE;
   shadow->grp_addr = grp_header;
   
   /*
    * Give the shadow a name.  Obtaining the name might remove ENT from the
    * cache, so we're careful not to reference it again.
    */
   if (ent==f->shared->root_sym) {
      /*
       * We're opening the root entry.  Get the name from the name message or
       * use a generic default.
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
      if (NULL==H5O_read (f, NO_ADDR, grp, H5O_STAB, 0, &stab)) {
	 HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, NULL,
		      "unable to read symbol table object header message");
      }
      if (NULL==(s=H5H_peek (f, &(stab.heap_addr), ent->name_off))) {
	 HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, NULL,
		      "unable to read symbol name");
      }
      shadow->name = H5MM_xstrdup (s);
   }
   ent = NULL; /*previous ops might have invalidated it*/
   

   /*
    * Link it into the shadow heap
    */
   for (hash=f->shared->shadow[idx]; hash; hash=hash->next) {
      if (0==idx &&
	  !H5F_addr_defined (&(hash->grp_addr)) &&
	  !H5F_addr_defined (&grp_header)) {
	 break; /*shadow list for root object*/
      } else if (H5F_addr_eq (&(hash->grp_addr), &grp_header)) {
	 break; /*shadow list for other objects*/
      }
   }
   if (!hash) {
      hash = H5MM_xcalloc (1, sizeof(H5G_hash_t));
      hash->grp_addr = grp_header;
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

#ifdef H5G_DEBUG
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
   
   FUNC_ENTER (H5G_shadow_close, FAIL);

   /* check args */
   assert (ent);
   assert (H5G_shadow_p (ent));
   assert (ent->shadow->nrefs>0);
   shadow = ent->shadow;

   /* clean the shadow */
   if (1==shadow->nrefs && ent->dirty) {
      if (!shadow->main &&
	  NULL==H5G_stab_find (f, &(shadow->grp_addr), NULL, shadow->name)) {
	 HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL,
			"unable to find shadow name in symbol table");
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

      /*
       * find symtabs shadow list -- if this is a shadow of the root object
       * then use zero for the hash value.  The root object always has an
       * undefined group address.
       */
      if (H5F_addr_defined (&(shadow->grp_addr))) {
	 idx = H5F_addr_hash (&(shadow->grp_addr), f->shared->nshadows);
      } else {
	 idx = 0;
      }
      for (hash=f->shared->shadow[idx]; hash; hash=hash->next) {
	 if (0==idx &&
	     !H5F_addr_defined (&(hash->grp_addr)) &&
	     !H5F_addr_defined (&(shadow->grp_addr))) {
	    break; /*shadow list for root object*/
	 } else if (H5F_addr_eq (&(hash->grp_addr), &(shadow->grp_addr))) {
	    break; /*shadow list for other objects*/
	 }
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
 *		NEW_ENTRY. The GRP_ADDR is the address for the group
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
		 H5G_entry_t *new_entry, const haddr_t *grp_addr)
{
   H5G_hash_t	*hash;
   uintn	idx;
   
   FUNC_ENTER (H5G_shadow_move, FAIL);

   assert (shadow);
   assert (new_entry);
   assert (grp_addr && H5F_addr_defined (grp_addr));

   if (!H5F_addr_defined (&(shadow->grp_addr))) {
      /*
       * We're moving the shadow for the root object.  This simplifies things
       * greatly since it implies that this is the only shadow currently
       * defined for the entire file.
       */
      idx = H5F_addr_hash (grp_addr, f->shared->nshadows);
      assert (NULL==f->shared->shadow[idx]); /*Nothing at new idx...	*/
      hash = f->shared->shadow[0];
      assert (hash);			/*..but root idx has something. */
      assert (!H5F_addr_defined (&(hash->grp_addr)));/*..it's the root obj*/
      assert (NULL==hash->next);	/*..and just that		*/
      assert (hash->head==shadow);	/*..and exactly that		*/

      /* Move root entry to new hash bucket */
      f->shared->shadow[idx] = hash;
      f->shared->shadow[0] = NULL;
      hash->grp_addr = *grp_addr;

      /* Associate SHADOW with NEW_ENTRY */
      shadow->grp_addr = *grp_addr;
      shadow->main = new_entry;
      new_entry->shadow = shadow;

      /* Give the shadow a new name */
      H5MM_xfree (shadow->name);
      shadow->name = H5MM_xstrdup (new_name);
      
   } else {
      /*
       * Other shadows never move.
       */
      assert (H5F_addr_eq (&(shadow->grp_addr), grp_addr));
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

   FUNC_ENTER (H5G_shadow_flush, FAIL);

   for (idx=0; idx<f->shared->nshadows; idx++) {
      for (hash=f->shared->shadow[idx]; hash; hash=hash->next) {
	 for (shadow=hash->head; shadow; shadow=shadow->next) {
	    /*
	     * If the shadow is dirty, then transfer the shadow info to the
	     * symbol table node.
	     */
	    if (shadow->entry.dirty) {
	       if (0==idx && !H5F_addr_defined (&(shadow->grp_addr)) &&
		   shadow->main==f->shared->root_sym) {
		  /*
		   * The shadow for the root entry gets copied back into the
		   * root symbol
		   */
		  *f->shared->root_sym = shadow->entry;
	       } else {
		  /*
		   * Other shadows get copied back into the symbol table.
		   */
		  if (!shadow->main &&
		      NULL==H5G_stab_find (f, &(shadow->grp_addr), NULL,
					   shadow->name)) {
		     HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL,
				    "unable to find shadow name in symbol "
				    "table");
		  }
		  assert (shadow->main);
		  *(shadow->main) = shadow->entry;
		  shadow->entry.dirty = FALSE;
		  nfound++;
	       }
	    }
#ifndef NDEBUG
	    /*
	     * This is usually a bad thing--an hdf5 programmer forgot to close
	     * some object before closing the file.  Since this is hard to
	     * debug, we'll be nice and print the names here.  We don't know
	     * the full name, but we'll print the relative file address
	     * of the object header for the group that contains the open
	     * object.
	     */
	    if (invalidate) {
	       fprintf (stderr, "HDF5-DIAG: warning: open object <");
	       H5F_addr_print (stderr, &(shadow->grp_addr));
	       fprintf (stderr, ">/%s", shadow->name);
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
      HRETURN_ERROR (H5E_SYM, H5E_UNSUPPORTED, FAIL,
		     "leaking memory due to shadow errors");
   }

   FUNC_LEAVE (SUCCEED);
}
