/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5G.c
 * 			Jul 18 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Symbol table functions.  The functions that
 *			begin with `H5G_stab_' don't understand the
 *			directory hierarchy; they operate on a single
 *			symbol table at a time.
 *
 * 			The functions that begin with `H5G_node_' operate
 *			on the leaf nodes of a symbol table B-tree.  They
 *			should be defined in the H5Gnode.c file.
 *
 * 			The remaining functions know about the directory
 *			hierarchy.
 *
 * Modifications:
 *
 *	Robb Matzke, 5 Aug 1997
 *	Added calls to H5E.
 *
 *-------------------------------------------------------------------------
 */
#include <assert.h>

#include "hdf5.h"

#define H5G_INIT_HEAP		8192

/* Packages needed by this file... */
#include "H5private.h"
#include "H5Bprivate.h"
#include "H5Gprivate.h"
#include "H5Hprivate.h"
#include "H5MMprivate.h"
#include "H5Oprivate.h"

#define PABLO_MASK	H5G_mask


/* Is the interface initialized? */
static intn interface_initialize_g = FALSE;


/*-------------------------------------------------------------------------
 * Function:	H5G_component
 *
 * Purpose:	Returns the pointer to the first component of the
 *		specified name by skipping leading slashes.  Returns
 *		the size in characters of the component through SIZE_P not
 *		counting leading slashes or the null terminator.
 *
 * Return:	Success:	Ptr into NAME.
 *
 *		Failure:	Ptr to the null terminator of NAME.
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static const char *
H5G_component (const char *name, size_t *size_p)
{
   assert (name);
   
   while ('/'==*name) name++;
   if (size_p) *size_p = strcspn (name, "/");
   return name;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_basename
 *
 * Purpose:	Returns a pointer into NAME for the start of the last
 *		component of NAME.  On return, the optional SIZE_P is
 *		initialized to point to the size of the base name not
 *		counting trailing slashes or the null character.
 *
 * Return:	Success:	Ptr to base name within NAME with SIZE_P
 *				pointing to the number of characters in the
 *				base name.
 *
 *		Failure:	Ptr to the null terminator of NAME.
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static const char *
H5G_basename (const char *name, size_t *size_p)
{
   const char	*s;
   
   assert (name);

   s = name + strlen(name);
   while (s>name && '/'==s[-1]) --s; /*skip past trailing slashes*/
   while (s>name && '/'!=s[-1]) --s; /*skip past base name*/

   /*
    * If the input was the name of the root directory `/' (or
    * equivalent) then return the null string.
    */
   if ('/'==*s) {
      if (size_p) *size_p = 0;
      return s + strlen(s); /*null terminator*/
   }

   if (size_p) *size_p = strcspn (s, "/");
   return s;
}
   

/*-------------------------------------------------------------------------
 * Function:	H5G_namei
 *
 * Purpose:	Given a name (absolute or relative) return the symbol table
 *		entry for that name and for the directory that contains the
 *		base name.  These entries (DIR_ENT and BASE_ENT) are returned
 *		through memory passed into the function by the caller.  Either
 *		or both pointers may be null.  Absolute names are looked up
 *		relative to the root directory of file F while relative
 *		names are traversed beginning at the CWD argument.
 *
 * 		Consecutive slash characters are treated like single
 *		slash characters.  Trailing slashes are ignored. The
 *		component `.' is recognized as the current directory
 *		during the traversal (initially CWD), but the component
 *		`..' is not internally recognized (it is recognized if
 * 		such a name appears in the symbol table).
 *
 * 		As a special case, if the NAME is the string `/' (or
 *		equivalent) then DIR_ENT and BASE_ENT are both initialized
 *		to the contents of the root symbol table entry.  However,
 *		the contents of the root symbol table entry may be
 *		uninitialized.
 *
 * 		If the name cannot be fully resolved, then REST will
 *		point to the part of NAME where the traversal failed
 *		(REST will always point to a relative name) and BASE_ENT
 * 		will not be initialized.  DIR_ENT will be initialized with
 *		information about the directory (or other object) at which
 *		the traversal failed.  However, if the name can be fully
 *		resolved, then REST points to the null terminator of NAME.
 *
 * Return:	Success:	SUCCEED if the name can be fully
 *				resolved.
 *
 *		Failure:	FAIL if something bad happened (REST and
 *				DIR_ENT have undefined values).
 *				
 *				-2 if the name could not be fully resolved
 * 				(REST and DIR_ENT are initialized).
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_namei (hdf5_file_t *f, H5G_entry_t *cwd, const char *name,
	   const char **rest, H5G_entry_t *dir_ent, H5G_entry_t *base_ent)
{
   H5G_entry_t	ent[2];
   H5G_entry_t	*tmp, *dir, *base;	/*ptrs to DIR and BASE entries	*/
   size_t	nchars;			/*component name length		*/
   char		comp[1024];		/*component name buffer		*/
   
   FUNC_ENTER (H5G_namei, NULL, FAIL);

   /* check args */
   assert (f);
   assert (f->root_sym);
   assert (name && *name);
   assert (cwd || '/'==*name);

   /* starting point */
   dir = ent+0;
   base = ent+1;
   if ('/'==*name) {
      ent[0] = ent[1] = *(f->root_sym);
   } else {
      ent[0] = ent[1] = *cwd;
   }

   /* traverse the name */
   while ((name=H5G_component (name, &nchars)) && *name) {

      /*
       * The special name `.'.
       */
      if ('.'==name[0] && !name[1]) continue;

      /*
       * Advance.
       */
      tmp=dir; dir=base; base=tmp; 	/*swap*/
      if (rest) *rest = name;

      /*
       * Copy the component name into a null-terminated buffer so
       * we can pass it down to the other symbol table functions.
       */
      if (nchars+1 > sizeof(comp)) {
	 /* component name is too long */
	 if (dir_ent) *dir_ent = *dir;
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_COMPLEN, -2);
      }
      HDmemcpy (comp, name, nchars);
      comp[nchars] = '\0';

      /*
       * Look for the component in the current symbol table.
       */
      if (H5G_stab_find (f, dir, comp, base)<0) {
	 /* component not found */
	 if (dir_ent) *dir_ent = *dir;
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, -2);
      }

      /* next component */
      name += nchars;
   }
   
   /* output parameters */
   if (rest) *rest = name; /*final null*/
   if (dir_ent) *dir_ent = *dir;
   if (base_ent) *base_ent = *base;
   
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_mkroot
 *
 * Purpose:	Creates the root directory if it doesn't exist; otherwise
 *		nothing happens.  If the root symbol table previously
 *		pointed to something other than a directory, then that
 *		object is made a member of the root directory and is
 *		given a name corresponding to the object name message.
 *		If the root object doesn't have an object name message
 *		then the name `Root Object' is used.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_mkroot (hdf5_file_t *f, size_t size_hint)
{
   H5O_stab_t	stab;			/*symbol table message		*/
   H5O_name_t	name;			/*object name message		*/
   H5G_entry_t	root;			/*old root entry		*/
   const char	*root_name=NULL;	/*name of old root object	*/
   intn		nlinks;			/*number of links		*/
   
   FUNC_ENTER (H5G_mkroot, NULL, FAIL);

   /*
    * Is there already a root object that needs to move into the new
    * root symbol table?
    */
   name.s = NULL;
   if (f->root_sym->header>0) {
      if (H5O_read (f, f->root_sym->header, f->root_sym, H5O_STAB, 0, &stab)) {
	 /* root directory already exists */
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_EXISTS, FAIL);
      } else if (H5O_read (f, f->root_sym->header, f->root_sym, H5O_NAME,
			   0, &name)) {
	 root = *(f->root_sym);
	 root_name = name.s; /*dont reset name until root_name is done*/
      } else {
	 root = *(f->root_sym);
	 root_name = "Root Object";
      }
   }

   /*
    * Create the root directory.
    */
   if (H5G_stab_new (f, f->root_sym, size_hint)<0) {
      H5O_reset (H5O_NAME, &name);
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, FAIL); /*can't create dir*/
   }

   /*
    * Increase the link count for the root symbol table!
    */
   nlinks = H5O_link (f, f->root_sym->header, f->root_sym, 1);
   assert (1==nlinks);

   /*
    * Insert the old root object.  It should already have a link count
    * of 1.
    */
   if (root_name) {
      nlinks = H5O_link (f, root.header, &root, 0);
      assert (1==nlinks);
	 
      if (H5G_stab_insert (f, f->root_sym, root_name, &root)) {
	 /* can't insert old root object in new root directory */
	 H5O_reset (H5O_NAME, &name);
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, FAIL);
      }
      H5O_reset (H5O_NAME, &name);
   }
   f->root_type=H5F_ROOT_DIRECTORY; /* set the root symbol type be a directory */

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_new
 *
 * Purpose:	Creates a new empty directory with the specified name.  The
 *		name is either an absolute name or is relative to the
 *		directory whose symbol table entry is CWD. On return, the
 *		optional DIR_ENT pointer is initialized with the symbol
 *		table entry for the new directory's parent and ENT will
 *		contain the symbol table entry for the new directory.
 *
 * 		Do not use this function to create the root symbol table
 *		since it is a special case.  Use H5G_mkroot() instead.
 *		Creating `/' with this function will return failure.
 *
 * Return:	Success:	SUCCEED, if DIR_ENT is not the null pointer
 *				then it will be initialized with the
 *				symbol table entry for the new directory.
 *
 *		Failure:	FAIL, the memory pointed to by CWD is
 *				not modified.
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_new (hdf5_file_t *f, H5G_entry_t *cwd, H5G_entry_t *dir_ent,
	 const char *name, size_t size_hint, H5G_entry_t *ent)
{
   const char	*rest=NULL;
   H5G_entry_t	_parent, _child;
   herr_t	status;
   char		_comp[1024];
   size_t	nchars;
   
   FUNC_ENTER (H5G_new, NULL, FAIL);

   /* check args */
   assert (f);
   assert (name && *name);
   assert (cwd || '/'==*name);
   if (!dir_ent) dir_ent = &_parent;
   if (!ent) ent = &_child;

   /* lookup name */
   status = H5G_namei (f, cwd, name, &rest, dir_ent, NULL);
   if (status<0 && !rest) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, FAIL); /*lookup failed*/
   } else if (0==status) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_EXISTS, FAIL); /*already exists*/
   }
   H5ECLEAR; /*it's OK that we didn't find it*/

   /* should be one null-terminated component left */
   rest = H5G_component (rest, &nchars);
   assert (rest && *rest);
   if (rest[nchars]) {
      if (H5G_component (rest+nchars, NULL)) {
	 /* missing component */
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, FAIL);
      } else if (nchars+1 > sizeof _comp) {
	 /* component name is too long */
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_COMPLEN, FAIL);
      } else {
	 /* null terminate */
	 memcpy (_comp, rest, nchars);
	 _comp[nchars] = '\0';
	 rest = _comp;
      }
   }
   
   /* create directory */
   if (H5G_stab_new (f, ent, size_hint)<0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, FAIL); /*can't create dir*/
   }

   /* insert child name into parent */
   if (H5G_stab_insert (f, dir_ent, rest, ent)<0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, FAIL); /*can't insert*/
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_find
 *
 * Purpose:	Finds an object with the specified NAME in file F.  If the
 *		name is relative then it is interpretted relative to CWD,
 *		a symbol table entry for a symbol table.  On successful return,
 *		DIR_ENT (if non-null) will be initialized with the symbol table
 *		information for the directory in which the object appears
 *		and ENT will be initialized with the symbol table entry for
 *		the object.
 *
 * 		ENT is optional when the caller is interested only in the
 *		existence of the object.
 *
 * Return:	Success:	SUCCEED with DIR_ENT and ENT initialized.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 12 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_find (hdf5_file_t *f, H5G_entry_t *cwd, H5G_entry_t *dir_ent,
	  const char *name, H5G_entry_t *ent)
{
   FUNC_ENTER (H5G_find, NULL, FAIL);

   /* check args */
   assert (f);
   assert (name && *name);
   assert (cwd || '/'==*name);

   if (H5G_namei (f, cwd, name, NULL, dir_ent, ent)<0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, FAIL); /*object not found*/
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_insert
 *
 * Purpose:	Inserts symbol table ENT into the directory hierarchy
 *		giving it the specified NAME.  If NAME is relative then
 *		it is interpreted with respect to the CWD pointer.  If
 *		non-null, DIR_ENT will be initialized with the symbol table
 *		entry for the directory which contains the new ENT.
 *
 * 		NAME must not be the name of the root symbol table entry
 *		('/') since that is a special case.  If NAME is the root
 * 		symbol table entry, then this function will return failure.
 *
 * 		Inserting an object entry into the symbol table increments
 *		the link counter for that object.
 *
 * Return:	Success:	SUCCEED with optional DIR_ENT initialized with
 *				the symbol table entry for the directory
 *				which contains the new ENT.
 *
 *		Failure:	FAIL (DIR_ENT is not modified).
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_insert (hdf5_file_t *f, H5G_entry_t *cwd, H5G_entry_t *dir_ent,
	    const char *name, H5G_entry_t *ent)
{
   herr_t	status;
   const char	*rest=NULL;
   H5G_entry_t	_parent;
   size_t	nchars;
   char		_comp[1024];
   
   FUNC_ENTER (H5G_insert, NULL, FAIL);

   /* check args */
   assert (f);
   assert (name && *name);
   assert (cwd || '/'==*name);
   assert (ent);
   if (!dir_ent) dir_ent = &_parent;

   /* lookup name */
   status = H5G_namei (f, cwd, name, &rest, dir_ent, NULL);
   if (status<0 && !rest) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, FAIL); /*lookup failed*/
   } else if (0==status) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_EXISTS, FAIL); /*already exists*/
   }
   H5ECLEAR; /*it's OK that we didn't find it*/

   /* should be one null-terminated component left */
   rest = H5G_component (rest, &nchars);
   assert (rest && *rest);
   if (rest[nchars]) {
      if (H5G_component (rest+nchars, NULL)) {
	 /* missing component */
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, FAIL);
      } else if (nchars+1 > sizeof _comp) {
	 /* component name is too long */
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_COMPLEN, FAIL);
      } else {
	 /* null terminate */
	 memcpy (_comp, rest, nchars);
	 _comp[nchars] = '\0';
	 rest = _comp;
      }
   }

   /* increment the link count */
   if (H5O_link (f, ent->header, ent, 1)<0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_LINK, FAIL); /*can't increase linkage*/
   }

   /* insert entry into parent */
   if (H5G_stab_insert (f, dir_ent, rest, ent)<0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, FAIL); /*can't insert*/
   }
      
   FUNC_LEAVE (SUCCEED);
}
   

/*-------------------------------------------------------------------------
 * Function:	H5G_modify
 *
 * Purpose:	Modifies the symbol table entry for the object with the
 *		specified NAME by copying the new symbol table entry ENT
 *		over the top of the old one.  If NAME is relative then it
 *		is interpreted with respect to the CWD pointer.  If non-null,
 *		DIR_ENT will be initialized with the symbol table entry for the
 *		directory which contains the new ENT.
 *
 * 		Do not use this function to change the entry for the root
 *		symbol since that's a special case.  This function returns
 *		failure if that is attempted.
 *
 * Return:	Success:	SUCCEED with optional DIR_ENT initialized with
 *				the symbol table entry for the directory
 *				which contains the new ENT.
 *
 *		Failure:	FAIL (DIR_ENT is not modified).
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_modify (hdf5_file_t *f, H5G_entry_t *cwd, H5G_entry_t *dir_ent,
	    const char *name, H5G_entry_t *ent)
{
   const char	*rest=NULL;
   H5G_entry_t	_parent;
   size_t	nchars;
   
   FUNC_ENTER (H5G_modify, NULL, FAIL);

   /* check args */
   assert (f);
   assert (name && *name);
   assert (cwd || '/'==*name);
   assert (ent);
   if (!dir_ent) dir_ent = &_parent;

   /* lookup name */
   if (H5G_namei (f, cwd, name, &rest, dir_ent, NULL)<0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, FAIL); /*entry not found*/
   }

   /* get the base name */
   rest = H5G_basename (name, &nchars);

   /* cannot modify the root symbol entry */
   if (!*rest) HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, FAIL);

   /* modify entry in parent */
   if (H5G_stab_modify (f, dir_ent, rest, ent)<0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, FAIL); /*can't modify*/
   }

   FUNC_LEAVE (SUCCEED);
}
   


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_new
 *
 * Purpose:	Creates a new empty symbol table (object header, name heap,
 *		and B-tree).  The caller can specify an initial size for the
 *		name heap.
 *
 * 		In order for the B-tree to operate correctly, the first
 *		item in the heap is the empty string, and must appear at
 *		heap offset zero.
 *
 * Return:	Success:	Address of new symbol table header.  If
 *				the caller supplies a symbol table entry
 *				SELF then it will be initialized to point to
 *				this symbol table.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5G_stab_new (hdf5_file_t *f, H5G_entry_t *self, size_t init)
{
   off_t	name;				/*offset of "" name	*/
   haddr_t	addr;				/*object header address	*/
   H5O_stab_t	stab;				/*symbol table message	*/

   FUNC_ENTER (H5G_stab_new, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   init = MAX(init, H5H_SIZEOF_FREE(f)+2);

   /* Create symbol table private heap */
   if ((stab.heap = H5H_new (f, H5H_LOCAL, init))<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
   }
   if ((name = H5H_insert (f, stab.heap, 1, "")<0)) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
   }
   if (0!=name) {
      /*
       * B-tree's won't work if the first name isn't at the beginning
       * of the heap.
       */
      HRETURN_ERROR (H5E_INTERNAL, H5E_CANTINIT, FAIL);
   }

   /* Create the B-tree */
   if ((stab.btree = H5B_new (f, H5B_SNODE))<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
   }

   /*
    * Create symbol table object header.  It has a zero link count
    * since nothing refers to it yet.  The link count will be
    * incremented if the object is added to the directory hierarchy.
    */
   if ((addr = H5O_new (f, 0, 4+2*H5F_SIZEOF_OFFSET(f)))<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
   }
   
   /* insert the symbol table message */
   if (self) {
      self->name_off = 0;
      self->header = addr;
      self->type = H5G_NOTHING_CACHED;
   }
   if (H5O_modify(f, addr, self, NULL, H5O_STAB, H5O_NEW_MESG, &stab)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
   }

   FUNC_LEAVE (addr);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_find
 *
 * Purpose:	Finds a symbol named NAME in the symbol table whose
 *		description is stored in SELF in file F and returns a
 *		copy of the symbol table entry through the ENT argument.
 *
 * Return:	Success:	Address corresponding to the name.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5G_stab_find (hdf5_file_t *f, H5G_entry_t *self, const char *name,
	       H5G_entry_t *ent)
{
   H5G_node_ud1_t       udata;		/*data to pass through B-tree	*/
   H5O_stab_t		stab;		/*symbol table message		*/

   FUNC_ENTER (H5G_stab_find, NULL, FAIL);

   /* Check arguments */
   assert (f);
   assert (self && self->header>=0);
   assert (name && *name);

   /* set up the udata */
   if (NULL==H5O_read (f, self->header, self, H5O_STAB, 0, &stab)) {
      HRETURN_ERROR (H5E_SYM, H5E_BADMESG, FAIL);
   }
   udata.operation = H5G_OPER_FIND;
   udata.name = name;
   udata.heap = stab.heap;

   /* search the B-tree */
   if (H5B_find (f, H5B_SNODE, stab.btree, &udata)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
   }

   /* return the result */
   if (ent) *ent = udata.entry;
   FUNC_LEAVE (udata.entry.header);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_modify
 *
 * Purpose:	Modifies the entry for an existing symbol.  The name of the
 *		symbol is NAME in the symbol table described by SELF in
 *		file F.  ENT is the new symbol table entry to use for the
 *		symbol.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_stab_modify (hdf5_file_t *f, H5G_entry_t *self, const char *name,
		 H5G_entry_t *ent)
{
   H5G_node_ud1_t	udata;		/*data to pass through B-tree	*/
   H5O_stab_t		stab;		/*symbol table message		*/

   FUNC_ENTER (H5G_stab_modify, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (self && self->header>=0);
   assert (name && *name);
   assert (ent);

   /* set up the udata */
   if (NULL==H5O_read (f, self->header, self, H5O_STAB, 0, &stab)) {
      HRETURN_ERROR (H5E_SYM, H5E_BADMESG, FAIL);
   }
   udata.operation = H5G_OPER_MODIFY;
   udata.name = name;
   udata.heap = stab.heap;
   udata.entry = *ent;

   /* search and modify the B-tree */
   if (H5B_find (f, H5B_SNODE, stab.btree, &udata)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_insert
 *
 * Purpose:	Insert a new symbol into the table described by SELF in
 *		file F.  The name of the new symbol is NAME and its symbol
 *		table entry is ENT.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_stab_insert (hdf5_file_t *f, H5G_entry_t *self, const char *name,
		 H5G_entry_t *ent)
{
   H5O_stab_t		stab;		/*symbol table message		*/
   H5G_node_ud1_t	udata;		/*data to pass through B-tree	*/

   FUNC_ENTER (H5G_stab_insert, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (self && self->header>=0);
   assert (name && *name);
   assert (ent);
   
   /* initialize data to pass through B-tree */
   if (NULL==H5O_read (f, self->header, self, H5O_STAB, 0, &stab)) {
      HRETURN_ERROR (H5E_SYM, H5E_BADMESG, FAIL);
   }
   udata.name = name;
   udata.heap = stab.heap;
   udata.entry = *ent;

   /* insert */
   if (H5B_insert (f, H5B_SNODE, stab.btree, &udata)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINSERT, FAIL);
   }

   /* update the name offset in the entry */
   ent->name_off = udata.entry.name_off;
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_list
 *
 * Purpose:	Returns a list of all the symbols in a symbol table.
 *		The caller allocates an array of pointers which this
 *		function will fill in with malloc'd names.  The caller
 *		also allocates an array of symbol table entries which will
 *		be filled in with data from the symbol table.  Each of these
 *		arrays should have at least MAXENTRIES elements.
 *
 * Return:	Success:	The total number of symbols in the
 *				symbol table.  This may exceed MAXENTRIES,
 *				but at most MAXENTRIES values are copied
 *				into the NAMES and ENTRIES arrays.
 *
 *		Failure:	FAIL, the pointers in NAMES are undefined but
 *			 	no memory is allocated.  The values in
 *				ENTRIES are undefined.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5G_stab_list (hdf5_file_t *f, H5G_entry_t *self, intn maxentries,
	       char *names[], H5G_entry_t entries[])
{
   H5G_node_list_t	udata;
   H5O_stab_t		stab;
   intn			i;

   FUNC_ENTER (H5G_stab_list, NULL, FAIL);

   /* check args */
   assert (f);
   assert (self && self->header>=0);
   assert (maxentries>=0);

   /* initialize data to pass through B-tree */
   if (NULL==H5O_read (f, self->header, self, H5O_STAB, 0, &stab)) {
      HRETURN_ERROR (H5E_SYM, H5E_BADMESG, FAIL);
   }
   udata.entry = entries;
   udata.name = names;
   udata.heap = stab.heap;
   udata.maxentries = maxentries;
   udata.nsyms = 0;
   if (names) HDmemset (names, 0, maxentries);

   /* list */
   if (H5B_list (f, H5B_SNODE, stab.btree, &udata)<0) {
      if (names) {
	 for (i=0; i<maxentries; i++) H5MM_xfree (names[i]);
      }
      HRETURN_ERROR (H5E_SYM, H5E_CANTLIST, FAIL);
   }

   FUNC_LEAVE (udata.nsyms);
}
   

/*-------------------------------------------------------------------------
 * Function:	H5G_decode_vec
 *
 * Purpose:	Same as H5G_decode() except it does it for an array of
 *		symbol table entries.
 *
 * Return:	Success:	SUCCEED, with *pp pointing to the first byte
 *				after the last symbol.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_decode_vec (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent, intn n)
{
   intn		i;

   FUNC_ENTER (H5G_decode_vec, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);
   assert (n>=0);

   /* decode entries */
   for (i=0; i<n; i++) {
      if (H5G_decode (f, pp, ent+i)<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTDECODE, FAIL);
      }
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_decode
 *
 * Purpose:	Decodes a symbol table entry pointed to by `*pp'.
 *
 * Return:	Success:	SUCCEED with *pp pointing to the first byte
 *				following the symbol table entry.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_decode (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent)
{
   uint8	*p_ret = *pp;

   FUNC_ENTER (H5G_decode, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);

   /* decode header */
   H5F_decode_offset (f, *pp, ent->name_off);
   H5F_decode_offset (f, *pp, ent->header);
   UINT32DECODE (*pp, ent->type);

   /* decode scratch-pad */
   switch (ent->type) {
   case H5G_NOTHING_CACHED:
      break;

   case H5G_CACHED_SDATA:
      UINT32DECODE (*pp, ent->cache.sdata.nt);
      UINT32DECODE (*pp, ent->cache.sdata.ndim);
      UINT32DECODE (*pp, ent->cache.sdata.dim[0]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[1]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[2]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[3]);
      break;

   case H5G_CACHED_STAB:
      UINT32DECODE (*pp, ent->cache.stab.btree);
      UINT32DECODE (*pp, ent->cache.stab.heap);
      break;

   default:
      abort();
   }

   *pp = p_ret + H5G_SIZEOF_ENTRY(f);
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_encode_vec
 *
 * Purpose:	Same as H5G_encode() except it does it for an array of
 *		symbol table entries.
 *
 * Return:	Success:	SUCCEED, with *pp pointing to the first byte
 *				after the last symbol.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_encode_vec (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent, intn n)
{
   intn		i;

   FUNC_ENTER (H5G_encode_vec, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);
   assert (n>=0);

   /* encode entries */
   for (i=0; i<n; i++) {
      if (H5G_encode (f, pp, ent+i)<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTENCODE, FAIL);
      }
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_encode
 *
 * Purpose:	Encodes the specified symbol table entry into the buffer
 *		pointed to by *pp.
 *
 * Return:	Success:	SUCCEED, with *pp pointing to the first byte
 *				after the symbol table entry.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 * 	Robb Matzke, 8 Aug 1997
 *	Writes zeros for the bytes that aren't used so the file doesn't
 *	contain junk.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_encode (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent)
{
   uint8	*p_ret = *pp + H5G_SIZEOF_ENTRY(f);

   FUNC_ENTER (H5G_encode, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);

   /* encode header */
   H5F_encode_offset (f, *pp, ent->name_off);
   H5F_encode_offset (f, *pp, ent->header);
   UINT32ENCODE (*pp, ent->type);

   /* encode scratch-pad */
   switch (ent->type) {
   case H5G_NOTHING_CACHED:
      break;

   case H5G_CACHED_SDATA:
      UINT32ENCODE (*pp, ent->cache.sdata.nt);
      UINT32ENCODE (*pp, ent->cache.sdata.ndim);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[0]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[1]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[2]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[3]);
      break;

   case H5G_CACHED_STAB:
      UINT32ENCODE (*pp, ent->cache.stab.btree);
      UINT32ENCODE (*pp, ent->cache.stab.heap);
      break;

   default:
      abort();
   }

   /* fill with zero */
   while (*pp<p_ret) *(*pp)++ = 0;
   
   *pp = p_ret;
   FUNC_LEAVE (SUCCEED);
}


