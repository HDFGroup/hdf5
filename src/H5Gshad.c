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

/* Is the interface initialized? */
static hbool_t interface_initialize_g = FALSE;

/* Shadow hash table */
#define H5G_NSHADOWS	10331

typedef struct H5G_hash_t {
   haddr_t		dir_addr;
   H5G_shadow_t		*head;
   struct H5G_hash_t	*next;
   struct H5G_hash_t	*prev;
} H5G_hash_t;

static H5G_hash_t *H5G_shadow_g[H5G_NSHADOWS];



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
H5G_shadow_list (haddr_t dir_addr)
{
   uintn	idx = dir_addr % H5G_NSHADOWS;
   H5G_hash_t	*bucket = NULL;

   FUNC_ENTER (H5G_shadows, NULL, NULL);

   for (bucket=H5G_shadow_g[idx]; bucket; bucket=bucket->next) {
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
H5G_shadow_assoc_node (hdf5_file_t *f, H5G_node_t *sym, H5G_entry_t *self)
{
   H5G_shadow_t *shadow = NULL;
   H5O_stab_t	stab;
   const char	*s = NULL;
   intn		i = 0;

   FUNC_ENTER (H5G_shadow_assoc_node, NULL, FAIL);

   /* Check arguments */
   assert (f);			/* The file			*/
   assert (sym);		/* The symbol table node	*/
   assert (self);		/* The symbol table header info	*/

   if ((shadow=H5G_shadow_list (self->header))) {

      /* We need the heap address so we can see the symbol names */
      if (NULL==H5O_read (f, self->header, self, H5O_NAME, 0, &stab)) {
	 HRETURN_ERROR (H5E_SYM, H5E_BADMESG, FAIL);
      }

      while (i<sym->nsyms && shadow) {

	 /* Advance the Entry ptr until it gets to the next shadow. */
	 while (i<sym->nsyms &&
		(s=H5H_peek (f, stab.heap_addr, sym->entry[i].name_off)) &&
		strcmp (s, shadow->name)<0) i++;

	 /* Advance the Shadow ptr until it gets to the next entry. */
	 while (i<sym->nsyms && s && shadow &&
		strcmp (s,  shadow->name)>0) shadow = shadow->next;

	 /* Did we find a match? */
	 if (i<sym->nsyms && s && shadow && !strcmp (s,  shadow->name)) {
	    shadow->main = sym->entry + i;
	    sym->entry[i].shadow = shadow;
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
H5G_shadow_open (hdf5_file_t *f, H5G_entry_t *dir, H5G_entry_t *ent)
{
   H5G_shadow_t	*shadow = NULL;
   H5O_stab_t 	stab;
   const char 	*s = NULL;
   H5G_hash_t	*hash = NULL;
   H5G_shadow_t	*hash_ent = NULL;
   uintn	idx;
   H5O_name_t	name_mesg = {NULL};
   H5G_entry_t	*ret_value = NULL;
   
   FUNC_ENTER (H5G_shadow_open, NULL, NULL);

   /* check args */
   assert (f);
   assert (dir);
   assert (ent);

   if (ent->shadow) {
      /*
       * Object is already open.  Open it again.
       */
      ent->shadow->nrefs += 1;
      HRETURN (ent);
      
   } else {
      shadow = H5MM_xcalloc (1, sizeof(H5G_shadow_t));

      if (ent==f->root_sym && dir->header<=0) {
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
      shadow->dir_addr = dir->header;
      
      /*
       * Link it into the shadow heap
       */
      idx = dir->header % H5G_NSHADOWS;
      for (hash=H5G_shadow_g[idx]; hash; hash=hash->next) {
	 if (hash->dir_addr==dir->header) break;
      }
      if (!hash) {
	 hash = H5MM_xcalloc (1, sizeof(H5G_hash_t));
	 hash->dir_addr = dir->header;
	 hash->next = H5G_shadow_g[idx];
	 H5G_shadow_g[idx] = hash;
      }
      for (hash_ent=hash->head; hash_ent; hash_ent=hash_ent->next) {
	 if (strcmp (shadow->name, hash_ent->name)<0) break;
      }
      if (hash_ent) {
	 if (hash_ent->prev) hash_ent->prev->next = shadow;
	 else hash->head = shadow;
	 shadow->prev = hash_ent->prev;
	 shadow->next = hash_ent;
	 hash_ent->prev = shadow;
      } else {
	 hash->head = shadow;
      }
   }

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
H5G_shadow_close (hdf5_file_t *f, H5G_entry_t *ent)
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
      idx = shadow->dir_addr % H5G_NSHADOWS;
      for (hash=H5G_shadow_g[idx]; hash; hash=hash->next) {
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
	 else H5G_shadow_g[idx] = hash->next;
	 if (hash->next) hash->next->prev = hash->prev;
	 H5MM_xfree (hash);
      }
   }

   FUNC_LEAVE (SUCCEED);
}
