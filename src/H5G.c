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
 * 	Robb Matzke, 30 Aug 1997
 *	Added `Errors:' field to function prologues.
 *
 * 	Robb Matzke, 18 Sep 1997
 *	Added shadow entry support.
 *
 *-------------------------------------------------------------------------
 */
#define H5G_PACKAGE	/*suppress error message about including H5Gpkg.h*/

/* Packages needed by this file... */
#include <H5private.h>
#include <H5Bprivate.h>
#include <H5Eprivate.h>
#include <H5Gpkg.h>
#include <H5Hprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define H5G_INIT_HEAP		8192
#define PABLO_MASK		H5G_mask

/* Is the interface initialized? */
static hbool_t interface_initialize_g = FALSE;



/*-------------------------------------------------------------------------
 * Function:	H5G_component
 *
 * Purpose:	Returns the pointer to the first component of the
 *		specified name by skipping leading slashes.  Returns
 *		the size in characters of the component through SIZE_P not
 *		counting leading slashes or the null terminator.
 *
 * Errors:
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
   if (size_p) *size_p = HDstrcspn (name, "/");
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
 * Errors:
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
#if 0
static const char *
H5G_basename (const char *name, size_t *size_p)
{
   const char	*s;
   
   assert (name);

   s = name + HDstrlen(name);
   while (s>name && '/'==s[-1]) --s; /*skip past trailing slashes*/
   while (s>name && '/'!=s[-1]) --s; /*skip past base name*/

   /*
    * If the input was the name of the root directory `/' (or
    * equivalent) then return the null string.
    */
   if ('/'==*s) {
      if (size_p) *size_p = 0;
      return s + HDstrlen(s); /*null terminator*/
   }

   if (size_p) *size_p = HDstrcspn (s, "/");
   return s;
}
#endif
   

/*-------------------------------------------------------------------------
 * Function:	H5G_namei
 *
 * Purpose:	(Partially) translates a name to a symbol table entry.
 *
 *		Given a name (absolute or relative) return the symbol table
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
 * 		If the name cannot be fully resolved, then REST will
 *		point to the part of NAME where the traversal failed
 *		(REST will always point to a relative name) and this
 *		function will return null. DIR_ENT will be initialized with
 *		information about the directory (or other object) at which
 *		the traversal failed.  However, if the name can be fully
 *		resolved, then REST points to the null terminator of NAME.
 *
 * 		As a special case, if the NAME is the name `/' (or
 *		equivalent) then DIR_ENT is initialized to all zero
 *		and a pointer to the root symbol table entry is returned.
 *
 * 		As a special case, if the NAME is the string `/foo' (or
 *		equivalent) and the root symbol table entry points to a
 *		non-directory object with a name message with the value
 *		`foo' then DIR_ENT is initialized to all zero and a pointer
 * 		to the root symbol table entry is returned.
 *
 * Errors:
 *		DIRECTORY COMPLEN       Component is too long. 
 *		DIRECTORY NOTFOUND      Component not found. 
 *		DIRECTORY NOTFOUND      Root not found. 
 *
 * Return:	Success:	Pointer to a cached symbol table entry if the
 *				name can be fully resolved. The pointer is
 *				valid until one of the H5AC (cache) functions
 *				is called.
 *
 *		Failure:	Null if the name could not be fully resolved.
 *				REST and DIR_ENT are initialized (possibly to
 *				zero if the failure occurred soon enough).
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5G_entry_t *
H5G_namei (hdf5_file_t *f, H5G_entry_t *cwd, const char *name,
	   const char **rest, H5G_entry_t *dir_ent)
{
   H5G_entry_t  dir;			/*entry for current directory	*/
   size_t	nchars;			/*component name length		*/
   char		comp[1024];		/*component name buffer		*/
   hbool_t	aside = FALSE;		/*did we look at a name message?*/
   H5G_entry_t	*ret_value=NULL;	/*return value			*/

   /* clear output args before FUNC_ENTER() in case it fails */
   if (rest) *rest = name;
   if (dir_ent) memset (dir_ent, 0, sizeof(H5G_entry_t));
   
   FUNC_ENTER (H5G_namei, NULL, NULL);

   /* check args */
   assert (f);
   assert (f->root_sym);
   assert (name && *name);
   assert (cwd || '/'==*name);

   /* starting point */
   if ('/'==*name) {
      if (f->root_sym->header<=0) {
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, NULL);
      }
      ret_value = f->root_sym;
      dir = *(f->root_sym);
   } else {
      ret_value = cwd;
      dir = *cwd;
   }

   /* traverse the name */
   while ((name=H5G_component (name, &nchars)) && *name) {

      /*
       * The special name `.' is a no-op.
       */
      if ('.'==name[0] && !name[1]) continue;

      /*
       * Advance to the next component of the name.
       */
      dir = *ret_value;
      ret_value = NULL;
      if (rest) *rest = name;

      /*
       * Copy the component name into a null-terminated buffer so
       * we can pass it down to the other symbol table functions.
       */
      if (nchars+1 > sizeof(comp)) {
	 /* component is too long */
	 if (dir_ent) *dir_ent = dir;
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_COMPLEN, NULL);
      }
      HDmemcpy (comp, name, nchars);
      comp[nchars] = '\0';

      if (NULL==(ret_value=H5G_stab_find (f, NO_ADDR, &dir, comp))) {
	 /*
	  * Component was not found in the current symbol table, possibly
	  * because DIR isn't a symbol table.  If it is the root symbol then
	  * see if it has the appropriate name field.  The ASIDE variable
	  * prevents us from saying `/foo/foo' where the root object has
	  * the name `foo'.
	  */
	 H5O_name_t mesg={0};
	 if (!aside && dir.header==f->root_sym->header &&
	     H5O_read (f, dir.header, &dir, H5O_NAME, 0, &mesg) &&
	     !HDstrcmp (mesg.s, comp)) {
	    H5O_reset (H5O_NAME, &mesg);
	    ret_value = f->root_sym;
	    aside = TRUE;
	 } else {
	    /* component not found */
	    H5O_reset (H5O_NAME, &mesg);
	    if (dir_ent) *dir_ent = dir;
	    HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, NULL);
	 }
      }

      /* next component */
      name += nchars;
   }

   /* output parameters */
   if (rest) *rest = name; /*final null*/
   if (dir_ent) {
      if (ret_value->header == f->root_sym->header) {
	 HDmemset (dir_ent, 0, sizeof(H5G_entry_t)); /*root has no parent*/
      } else {
	 *dir_ent = dir;
      }
   }

   /* Perhaps the root object doesn't even exist! */
   if (ret_value->header<=0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, NULL); /*root not found*/
   }
   
   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_mkroot
 *
 * Purpose:	Creates the root directory if it doesn't exist; otherwise
 *		nothing happens.  If the root symbol table entry previously
 *		pointed to something other than a directory, then that object
 *		is made a member of the root directory and is given a name
 *		corresponding to the object's name message (the name message
 *		is removed).  If the root object doesn't have a name message
 *		then the name `Root Object' is used.
 *
 * Warning:	This function has a few subtleties. Be warned!
 *
 * Errors:
 *		DIRECTORY CANTINIT      Can't create root. 
 *		DIRECTORY CANTINIT      Can't insert old root object in
 *		                        new root directory. 
 *		DIRECTORY EXISTS        Root directory already exists. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL.  This function returns -2 if the
 *				failure is because a root directory already
 *				exists.
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
H5G_mkroot (hdf5_file_t *f, size_t size_hint)
{
   H5G_entry_t	*handle=NULL;		/*handle to open object		*/
   herr_t	ret_value=FAIL;		/*return value			*/
   H5O_name_t	name={NULL};		/*object name			*/
   H5O_stab_t	stab;			/*symbol table message		*/
   H5G_entry_t	*ent_ptr=NULL;		/*pointer to a symbol table entry*/
   const char	*obj_name=NULL;		/*name of old root object	*/
   
   FUNC_ENTER (H5G_mkroot, NULL, FAIL);

   /*
    * Make sure that the file descriptor has the latest info -- someone might
    * have the root object open.
    */
   H5G_shadow_sync (f->root_sym);
   
   /*
    * If we already have a root object, then open it and get it's name. The
    * root object had better not already be a directory.  Once the old root
    * object is opened and we have a HANDLE, set the dirty bit on the handle.
    * This causes the handle data to be written back into f->root_sym by
    * H5G_close() if something goes wrong before the old root object is
    * re-inserted back into the directory hierarchy.  We might leak file
    * memory, but at least we don't loose the original root object.
    */
   if (f->root_sym->header>0) {
      if (H5O_read (f, NO_ADDR, f->root_sym, H5O_STAB, 0, &stab)) {
	 /* root directory already exists */
	 HGOTO_ERROR (H5E_DIRECTORY, H5E_EXISTS, -2);
      } else if (NULL==(handle=H5G_shadow_open (f, NULL, f->root_sym))) {
	 /* can't open root object */
	 HGOTO_ERROR (H5E_DIRECTORY, H5E_CANTINIT, FAIL);
      } else if (NULL==H5O_read (f, NO_ADDR, handle, H5O_NAME, 0, &name)) {
	 obj_name = "Root Object";
      } else {
	 obj_name = name.s; /*don't reset message until the end!*/
      }
      handle->dirty = TRUE;
   }

   /*
    * Create the new root directory directly into the file descriptor. If
    * something goes wrong at this step, closing the `handle' will rewrite
    * info back into f->root_sym because we set the dirty bit.
    */
   if (H5G_stab_new (f, f->root_sym, size_hint)<0) {
      HGOTO_ERROR (H5E_DIRECTORY, H5E_CANTINIT, FAIL); /*cant create root*/
   }
   if (1!=H5O_link (f, f->root_sym, 1)) {
      HGOTO_ERROR (H5E_DIRECTORY, H5E_LINK, FAIL);
   }

   /*
    * If there was a previous root object then insert it into the new root
    * symbol table with the specified name.  Inserting the object will update
    * the handle to point to the new symbol table slot instead of f->root_sym.
    */
   if (obj_name) {
      if (1!=H5O_link (f, handle, 0)) {
	 HGOTO_ERROR (H5E_DIRECTORY, H5E_LINK, FAIL);
      }
      if (NULL==(ent_ptr=H5G_stab_insert (f, f->root_sym, obj_name,
					  handle))) {
	 HGOTO_ERROR (H5E_DIRECTORY, H5E_CANTINIT, FAIL);
      }
      
      /*
       * Remove all `name' messages from the old root object.  The only time
       * a name message should ever appear is to give the root object a name,
       * but the old root object is no longer the root object.
       */
      H5O_remove (f, NO_ADDR, handle, H5O_NAME, H5O_ALL);
      H5ECLEAR; /*who really cares?*/
   }
   
   ret_value = SUCCEED;
   
 done:
   /*
    * If the handle is closed before the H5G_stab_insert() call that
    * reinserts the root object into the directory hierarchy, then
    * H5G_close() will reset f->root_sym to point to the old root symbol and
    * the new root directory (if it was created) will be unlinked from the
    * directory hierarchy (and memory leaked).
    */
   if (handle) H5G_close (f, handle);
   H5O_reset (H5O_NAME, &name);
   
   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_mkdir
 *
 * Purpose: 	Creates a new empty directory with the specified name,
 *		opening it as an object. The name is either an absolute name
 *		or is relative to the current working directory.
 *
 * 		A root directory is created implicitly by this function
 *		when necessary.  Calling this function with the name "/"
 *		(or any equivalent name) will result in an H5E_EXISTS
 * 		failure.
 *
 * Errors:
 *		DIRECTORY CANTINIT      Can't create dir. 
 *		DIRECTORY CANTINIT      Can't insert. 
 *		DIRECTORY CANTINIT      Lookup failed. 
 *		DIRECTORY COMPLEN       Component is too long. 
 *		DIRECTORY EXISTS        Already exists. 
 *		DIRECTORY NOTFOUND      Missing component. 
 *
 * Return:	Success:	A handle to the open directory.  Please call
 *				H5G_close() when you're done with it.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_entry_t *
H5G_mkdir (hdf5_file_t *f, const char *name, size_t size_hint)
{
   const char	*rest=NULL;		/*the base name			*/
   H5G_entry_t	*cwd=NULL;		/*current working directory	*/
   H5G_entry_t	dir_ent;		/*directory containing new dir	*/
   H5G_entry_t	ent;			/*new directory entry		*/
   H5G_entry_t	*ent_ptr=NULL;		/*ptr to new directory entry	*/
   H5G_entry_t	*ret_value=NULL;	/*handle return value		*/
   char		_comp[1024];		/*name component		*/
   size_t	nchars;			/*number of characters in compon*/
   herr_t	status;			/*function return status	*/
   
   FUNC_ENTER (H5G_mkdir, NULL, NULL);

   /* check args */
   assert (f);
   assert (name && *name);
#ifndef LATER
   /* Get current working directory */
   H5G_shadow_sync (f->root_sym);
   cwd = f->root_sym;
#endif
   assert (cwd || '/'==*name);

   /*
    * Try to create the root directory.  Ignore the error if this function
    * fails because the root directory already exists.
    */
   if ((status=H5G_mkroot (f, H5G_SIZE_HINT))<0 && -2!=status) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL);
   }
   H5ECLEAR;

   /* lookup name */
   if (H5G_namei (f, cwd, name, &rest, &dir_ent)) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_EXISTS, NULL); /*already exists*/
   }
   H5ECLEAR; /*it's OK that we didn't find it*/

   /* should be one null-terminated component left */
   rest = H5G_component (rest, &nchars);
   assert (rest && *rest);
   if (rest[nchars]) {
      if (H5G_component (rest+nchars, NULL)) {
	 /* missing component */
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, NULL);
      } else if (nchars+1 > sizeof _comp) {
	 /* component is too long */
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_COMPLEN, NULL);
      } else {
	 /* null terminate */
	 HDmemcpy (_comp, rest, nchars);
	 _comp[nchars] = '\0';
	 rest = _comp;
      }
   }
   
   /* create directory */
   if (H5G_stab_new (f, &ent, size_hint)<0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL); /*can't create dir*/
   }

   /* insert child name into parent */
   if (NULL==(ent_ptr=H5G_stab_insert (f, &dir_ent, rest, &ent))) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL); /*can't insert*/
   }

   /* open the directory */
   if (NULL==(ret_value=H5G_shadow_open (f, &dir_ent, ent_ptr))) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL); /*can't open*/
   }

   FUNC_LEAVE (ret_value);
}



/*-------------------------------------------------------------------------
 * Function:	H5G_pushd
 *
 * Purpose:	Pushes a new current working directory onto the stack.
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
H5G_pushd (hdf5_file_t *f, const char *name)
{
   FUNC_ENTER (H5G_pushd, NULL, FAIL);

#ifndef LATER
   /*
    * Current working directories are not implemented yet.
    */
   if (strcmp (name, "/")) {
      HRETURN_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);
   }
#endif
   
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_popd
 *
 * Purpose:	Pops the top current working directory off the stack.
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
H5G_popd (hdf5_file_t *f)
{
   FUNC_ENTER (H5G_popd, NULL, FAIL);

#ifndef LATER
   /* CWD is not implemented yet. */
#endif

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_create
 *
 * Purpose:	Creates a new empty object header, gives it a name, opens
 *		the object for modification, and returns a handle to the
 *		object.  The initial size of the object header can be
 *		supplied with the OHDR_HINT argument.
 *
 * Return:	Success:	A handle for the object.  Be sure to
 *				eventually close it.
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
H5G_entry_t *
H5G_create (hdf5_file_t *f, const char *name, size_t ohdr_hint)
{
   H5G_entry_t	ent;			/*entry data for the new object	*/
   H5G_entry_t	*ent_ptr;		/*ptr into symbol node for entry*/
   H5G_entry_t	*cwd=NULL;		/*ptr to CWD handle		*/
   const char	*rest = NULL;		/*part of name not existing yet	*/
   H5G_entry_t	dir;			/*entry for dir to contain obj	*/
   H5G_entry_t	*ret_value=NULL;	/*the object handle		*/
   size_t	nchars;			/*number of characters in name	*/
   char		_comp[1024];		/*name component		*/
   
   FUNC_ENTER (H5G_create, NULL, NULL);

   /* Check args. */
   assert (f);
   assert (name && *name);
   HDmemset (&ent, 0, sizeof(H5G_entry_t));

   /*
    * Get the current working directory.
    */
#ifndef LATER
   H5G_shadow_sync (f->root_sym);
   cwd = f->root_sym;
#endif

   /*
    * Look up the name -- it shouldn't exist yet.
    */
   if (H5G_namei (f, cwd, name, &rest, &dir)) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_EXISTS, NULL); /*already exists*/
   }
   H5ECLEAR; /*it's OK that we didn't find it*/
   rest = H5G_component (rest, &nchars);

   if (!rest || !*rest) {
      /*
       * The caller is attempting to insert a root object that either
       * doesn't have a name or we shouldn't interfere with the name
       * it already has as a message.
       */
      if (f->root_sym->header>0) {
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_EXISTS, NULL); /*root exists*/
      }
      if ((ent.header = H5O_new (f, 0, ohdr_hint))<0) {
	 /* can't create header */
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL);
      }
      if (1!=H5O_link (f, &ent, 1)) {
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_LINK, NULL); /*bad link count*/
      }
      *(f->root_sym) = ent;
      if (NULL==(ret_value=H5G_shadow_open (f, &dir, f->root_sym))) {
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL);
      }
      HRETURN (ret_value);
   }

   /*
    * There should be one component left.  Make sure it's null
    * terminated.
    */
   if (rest[nchars]) {
      if (H5G_component (rest+nchars, NULL)) {
	 /* component not found */
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, NULL);
      } else if (nchars+1 > sizeof _comp) {
	 /* component is too long */
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_COMPLEN, NULL);
      } else {
	 /* null terminate */
	 HDmemcpy (_comp, rest, nchars);
	 _comp[nchars] = '\0';
	 rest = _comp;
      }
   }

   /*
    * Create the object header.
    */
   if ((ent.header = H5O_new (f, 0, ohdr_hint))<0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL);
   }
   

   if (f->root_sym->header<=0) {
      /*
       * This will be the only object in the file. Insert it as the root
       * object and add a name messaage to the object header (or modify
       * the first one we find). Although the header exists we can guarantee
       * that it isn't open since it has no name.
       */
      H5O_name_t name_mesg;
      name_mesg.s = rest;
      if (H5O_modify (f, NO_ADDR, &ent, H5O_NAME, 0, &name_mesg)<0) {
	 /* cannot add/change name message */
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL);
      }
      if (1!=H5O_link (f, &ent, 1)) {
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_LINK, NULL); /*bad link count*/
      }
      *(f->root_sym) = ent;
      if (NULL==(ret_value=H5G_shadow_open (f, &dir, f->root_sym))) {
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL);
      }
      HRETURN (ret_value);
   } else {
      /*
       * Make sure the root directory exists.  Ignore the failure if it's
       * because the directory already exists.
       */
      hbool_t update_dir = (dir.header==f->root_sym->header);
      herr_t status = H5G_mkroot (f, H5G_SIZE_HINT);
      if (status<0 && -2!=status) {
	 HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL);
      }
      H5ECLEAR;
      if (update_dir) dir = *(f->root_sym);
   }
   
   /*
    * This is the normal case.  The object is just being inserted as a normal
    * entry into a symbol table.
    */
   if (H5O_link (f, &ent, 1)<0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_LINK, NULL); /*link inc failure*/
   }
   if (NULL==(ent_ptr=H5G_stab_insert (f, &dir, rest, &ent))) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL); /*can't insert*/
   }
   if (NULL==(ret_value=H5G_shadow_open (f, &dir, ent_ptr))) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTINIT, NULL); /*can't open object*/
   }
   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_open
 *
 * Purpose:	Opens an object.  That is, it prepares the object for
 *		modification by returning a handle to the object
 *		symbol table entry. Opening an object twice with the
 *		same name (or more precisely, through the same final
 *		symbol table entry) will return pointers to the same
 *		H5G_entry_t struct. But opening an object through
 *		different final H5G_entry_t structs (which implies
 *		different names) returns pointers to different
 *		structs.  The structs that are returned should be
 *		released by calling H5G_close().
 *
 * Return:	Success:	Ptr to a handle for the object.
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
H5G_entry_t *
H5G_open (hdf5_file_t *f, const char *name)
{
   H5G_entry_t	*ent=NULL;
   H5G_entry_t	*ret_value=NULL;
   H5G_entry_t	dir;
   H5G_entry_t	*cwd=NULL;
   
   FUNC_ENTER (H5G_open, NULL, NULL);

   /* check args */
   assert (f);
   if (!name || !*name) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_BADVALUE, NULL);
   }

   /* Get CWD */
#ifndef LATER
   H5G_shadow_sync (f->root_sym);
   cwd = f->root_sym;
#endif
   assert (cwd || '/'==*name);

   if (f->root_sym->header<=0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, NULL); /*object not found*/
   }
   if (NULL==(ent=H5G_namei (f, cwd, name, NULL, &dir))) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, NULL); /*object not found*/
   }
   if (NULL==(ret_value=H5G_shadow_open (f, &dir, ent))) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_CANTOPENOBJ, NULL);
   }

   FUNC_LEAVE (ret_value);
}



/*-------------------------------------------------------------------------
 * Function:	H5G_close
 *
 * Purpose:	Closes an object that was open for modification.
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
H5G_close (hdf5_file_t *f, H5G_entry_t *ent)
{
   FUNC_ENTER (H5G_close, NULL, FAIL);

   assert (f);

   if (ent && H5G_shadow_close (f,  ent)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTFLUSH, FAIL);
   }
   
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_find
 *
 * Purpose:	Finds an object with the specified NAME in file F.  If
 *		the name is relative then it is interpretted relative
 *		to CWD, a symbol table entry for a symbol table.  On
 *		successful return, DIR_ENT (if non-null) will be
 *		initialized with the symbol table information for the
 *		directory in which the object appears (or all zero if
 *		the returned object is the root object) and ENT will
 *		be initialized with the symbol table entry for the
 *		object (ENT is optional when the caller is interested
 *		only in the existence of the object).
 *
 * 		This function will fail if the root object is
 * 		requested and there is none.
 *
 * Errors:
 *		DIRECTORY NOTFOUND      Object not found. 
 *
 * Return:	Success:	SUCCEED with DIR_ENT and ENT initialized. ENT
 *				is intended for immediate read-only access.
 *				If the object that ENT refers to is open
 *				through the ENT entry (see H5G_open()) then
 *				the returned ENT will contain the latest
 *				information.  However, subsequent changes to
 *				the symbol table entry will not be reflected
 *				in ENT since it is a copy of the symbol table.
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
   H5G_entry_t	*ent_p = NULL;
   FUNC_ENTER (H5G_find, NULL, FAIL);

   /* check args */
   assert (f);
   assert (name && *name);
   assert (cwd || '/'==*name);

   if (f->root_sym->header<=0) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, FAIL); /*object not found*/
   }

   if (NULL==(ent_p=H5G_namei (f, cwd, name, NULL, dir_ent))) {
      HRETURN_ERROR (H5E_DIRECTORY, H5E_NOTFOUND, FAIL); /*object not found*/
   }

   if (ent) *ent = *ent_p;
   FUNC_LEAVE (SUCCEED);
}

