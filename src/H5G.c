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
 *			naming system; they operate on a single
 *			symbol table at a time.
 *
 * 			The functions that begin with `H5G_node_' operate
 *			on the leaf nodes of a symbol table B-tree.  They
 *			should be defined in the H5Gnode.c file.
 *
 *			The remaining functions know how to traverse the
 *			group directed graph
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

/* Interface initialization */
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT		NULL


/*-------------------------------------------------------------------------
 * Function:	H5Gnew
 *
 * Purpose:	Creates a new group in FILE and gives it the specified
 *		NAME. Unless NAME begins with `/' it is relative to the
 *		current working group.
 *
 * 		The optional SIZE_HINT specifies how much file space to
 *		reserve to store the names that will appear in this
 *		group (an even number of characters, counting the null
 *		terminator, is allocated for each name). If a non-positive
 *		value is supplied for the SIZE_HINT then a default size is
 *		chosen.
 *
 * See also:	H5Gset(), H5Gpush(), H5Gpop()
 *
 * Errors:
 *		ARGS      BADTYPE       Not a file atom. 
 *		ARGS      BADVALUE      No name given. 
 *		ATOM      BADATOM       Can't unatomize file. 
 *		SYM       CANTINIT      Can't close handle. 
 *		SYM       CANTINIT      Can't create group. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gnew (hid_t file, const char *name, size_t size_hint)
{
   H5F_t	*f=NULL;
   H5G_entry_t	*grp_handle=NULL;
   
   FUNC_ENTER (H5Gnew, FAIL);

   /* Check/fix arguments */
   if (!name || !*name) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no name given");
   }
   if (H5_FILE!=H5Aatom_group (file)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a file atom");
   }
   if (NULL==(f=H5Aatom_object (file))) {
      HRETURN_ERROR (H5E_ATOM, H5E_BADATOM, FAIL, "can't unatomize file");
   }
   
   /* Create the group */
   if (NULL==(grp_handle=H5G_new (f, name, size_hint))) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL,
		     "can't create group");
   }

   /* Close the group handle */
   if (H5G_close (f, grp_handle)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "can't close handle");
   }

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5Gset
 *
 * Purpose:	Sets the working group for file handle FILE to the
 *		specified NAME.  Unless NAME begins with a `/' it is
 *		interpretted relative to the current working group.
 *
 *		Each file handle maintains its own notion of the current
 *		working group.  That is, if a single file is opened with
 *		multiple calls to H5Fopen(), which returns multiple file
 *		handles, then each handle's current working group can be
 *		set independently of the other file handles for that file.
 *
 * See also:	H5Gpush(), H5Gpop()
 *
 * Errors:
 *		ARGS      BADTYPE       Not a file atom. 
 *		ARGS      BADVALUE      No name given. 
 *		ATOM      BADATOM       Can't unatomize file. 
 *		SYM       CANTINIT      Can't change current working group. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gset (hid_t file, const char *name)
{
   H5F_t	*f=NULL;
   
   FUNC_ENTER (H5Gset, FAIL);

   /* Check/fix arguments */
   if (!name || !*name) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no name given");
   }
   if (H5_FILE!=H5Aatom_group (file)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a file atom");
   }
   if (NULL==(f=H5Aatom_object (file))) {
      HRETURN_ERROR (H5E_ATOM, H5E_BADATOM, FAIL, "can't unatomize file");
   }

   if (H5G_set (f, name)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL,
		     "can't change current working group");
   }

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5Gpush
 *
 * Purpose:	Similar to H5Gset() except the new working group is pushed
 *		on a stack.
 *
 *		Each file handle maintains its own notion of the current
 *		working group.  That is, if a single file is opened with
 *		multiple calls to H5Fopen(), which returns multiple file
 *		handles, then each handle's current working group can be
 *		set independently of the other file handles for that file.
 *
 * See also:	H5Gset(), H5Gpop()
 *
 * Errors:
 *		ARGS      BADTYPE       Not a file atom. 
 *		ARGS      BADVALUE      No name given. 
 *		ATOM      BADATOM       Can't unatomize file. 
 *		SYM       CANTINIT      Can't change current working group. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gpush (hid_t file, const char *name)
{
   H5F_t	*f=NULL;
   
   FUNC_ENTER (H5Gpush, FAIL);

   /* Check/fix arguments */
   if (!name || !*name) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no name given");
   }
   if (H5_FILE!=H5Aatom_group (file)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a file atom");
   }
   if (NULL==(f=H5Aatom_object (file))) {
      HRETURN_ERROR (H5E_ATOM, H5E_BADATOM, FAIL, "can't unatomize file");
   }

   if (H5G_push (f, name)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL,
		     "can't change current working group");
   }

   FUNC_LEAVE (SUCCEED);
}
   


/*-------------------------------------------------------------------------
 * Function:	H5Gpop
 *
 * Purpose:	Removes the top (latest) entry from the working group stack
 *		and sets the current working group to the previous value.
 *
 *		Each file handle maintains its own notion of the current
 *		working group.  That is, if a single file is opened with
 *		multiple calls to H5Fopen(), which returns multiple file
 *		handles, then each handle's current working group can be
 *		set independently of the other file handles for that file.
 *
 * See also:	H5Gset(), H5Gpush()
 *
 * Errors:
 *		ARGS      BADTYPE       Not a file atom. 
 *		ATOM      BADATOM       Can't unatomize file. 
 *		SYM       CANTINIT      Stack is empty. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL.  The final entry cannot be popped from
 *				the group stack (but it can be changed
 *				with H5Gset()).
 *
 * Programmer:	Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gpop (hid_t file)
{
   H5F_t	*f=NULL;
   
   FUNC_ENTER (H5Gpop, FAIL);

   /* Check/fix arguments */
   if (H5_FILE!=H5Aatom_group (file)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a file atom");
   }
   if (NULL==(f=H5Aatom_object (file))) {
      HRETURN_ERROR (H5E_ATOM, H5E_BADATOM, FAIL, "can't unatomize file");
   }

   if (H5G_pop (f)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL,
		     "stack is empty");
   }

   FUNC_LEAVE (SUCCEED);
}
   



/*
 *-------------------------------------------------------------------------
 *-------------------------------------------------------------------------
 *   N O   A P I   F U N C T I O N S   B E Y O N D   T H I S   P O I N T
 *-------------------------------------------------------------------------
 *------------------------------------------------------------------------- 
 */







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
 *		matzke@llnl.gov
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
 * Function:	H5G_namei
 *
 * Purpose:	(Partially) translates a name to a symbol table entry.
 *
 *		Given a name (absolute or relative) return the symbol table
 *		entry for that name and for the group that contains the
 *		base name.  These entries (GRP_ENT and BASE_ENT) are returned
 *		through memory passed into the function by the caller.  Either
 *		or both pointers may be null.  Absolute names are looked up
 *		relative to the root group of file F while relative
 *		names are traversed beginning at the CWG argument.
 *
 * 		Consecutive slash characters are treated like single
 *		slash characters.  Trailing slashes are ignored. The
 *		component `.' is recognized as the current group
 *		during the traversal (initially CWG), but the component
 *		`..' is not internally recognized (it is recognized if
 * 		such a name appears in the symbol table).
 *
 * 		If the name cannot be fully resolved, then REST will
 *		point to the part of NAME where the traversal failed
 *		(REST will always point to a relative name) and this
 *		function will return null. GRP_ENT will be initialized with
 *		information about the group (or other object) at which
 *		the traversal failed.  However, if the name can be fully
 *		resolved, then REST points to the null terminator of NAME.
 *
 * 		As a special case, if the NAME is the name `/' (or
 *		equivalent) then GRP_ENT is initialized to all zero
 *		and an invalid header address and a pointer to the root
 *		symbol table entry is returned.
 *
 * 		As a special case, if the NAME is the string `/foo' (or
 *		equivalent) and the root symbol table entry points to a
 *		non-group object with a name message with the value
 *		`foo' then GRP_ENT is initialized to all zero (except for an
 * 		invalid header address) and a pointer to the root symbol
 *		table entry is returned.
 *
 * Errors:
 *		SYM       COMPLEN       Component is too long. 
 *		SYM       NOTFOUND      Component not found. 
 *		SYM       NOTFOUND      No root group. 
 *		SYM       NOTFOUND      Root not found. 
 *
 * Return:	Success:	Pointer to a cached symbol table entry if the
 *				name can be fully resolved. The pointer is
 *				valid until one of the H5AC (cache) functions
 *				is called.
 *
 *		Failure:	Null if the name could not be fully resolved.
 *				REST and GRP_ENT are initialized (possibly to
 *				zero if the failure occurred soon enough).
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5G_entry_t *
H5G_namei (H5F_t *f, H5G_entry_t *cwg, const char *name,
	   const char **rest/*out*/, H5G_entry_t *grp_ent/*out*/)
{
   H5G_entry_t  grp;			/*entry for current group	*/
   size_t	nchars;			/*component name length		*/
   char		comp[1024];		/*component name buffer		*/
   hbool_t	aside = FALSE;		/*did we look at a name message?*/
   H5G_entry_t	*ret_value=NULL;	/*return value			*/

   /* clear output args before FUNC_ENTER() in case it fails */
   if (rest) *rest = name;
   if (grp_ent) {
      memset (grp_ent, 0, sizeof(H5G_entry_t));
      H5F_addr_undef (&(grp_ent->header));
   }
   
   FUNC_ENTER (H5G_namei, NULL);

   /* check args */
   assert (f);
   assert (f->shared->root_sym);
   assert (name && *name);
   assert (cwg || '/'==*name);

   /* starting point */
   if ('/'==*name) {
      if (!H5F_addr_defined (&(f->shared->root_sym->header))) {
	 HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, NULL,
			"no root group");
      }
      ret_value = f->shared->root_sym;
      grp = *(f->shared->root_sym);
   } else {
      ret_value = cwg;
      grp = *cwg;
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
      grp = *ret_value;
      ret_value = NULL;
      if (rest) *rest = name;

      /*
       * Copy the component name into a null-terminated buffer so
       * we can pass it down to the other symbol table functions.
       */
      if (nchars+1 > sizeof(comp)) {
	 if (grp_ent) *grp_ent = grp;
	 HRETURN_ERROR (H5E_SYM, H5E_COMPLEN, NULL, "component is too long");
      }
      HDmemcpy (comp, name, nchars);
      comp[nchars] = '\0';

      if (NULL==(ret_value=H5G_stab_find (f, NO_ADDR, &grp, comp))) {
	 /*
	  * Component was not found in the current symbol table, possibly
	  * because GRP isn't a symbol table.  If it is the root symbol then
	  * see if it has the appropriate name field.  The ASIDE variable
	  * prevents us from saying `/foo/foo' where the root object has
	  * the name `foo'.
	  */
	 H5O_name_t mesg={0};
	 if (!aside &&
	     H5F_addr_defined (&(grp.header)) &&
	     H5F_addr_defined (&(f->shared->root_sym->header)) &&
	     H5F_addr_eq (&(grp.header), &(f->shared->root_sym->header)) &&
	     H5O_read (f, &(grp.header), &grp, H5O_NAME, 0, &mesg) &&
	     !HDstrcmp (mesg.s, comp)) {
	    H5O_reset (H5O_NAME, &mesg);
	    ret_value = f->shared->root_sym;
	    aside = TRUE;
	 } else {
	    H5O_reset (H5O_NAME, &mesg);
	    if (grp_ent) *grp_ent = grp;
	    HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, NULL, "component not found");
	 }
      }

      /* next component */
      name += nchars;
   }

   /* output parameters */
   if (rest) *rest = name; /*final null*/
   if (grp_ent) {
      if (H5F_addr_eq (&(ret_value->header), &(f->shared->root_sym->header))) {
	 HDmemset (grp_ent, 0, sizeof(H5G_entry_t)); /*root has no parent*/
	 H5F_addr_undef (&(grp_ent->header));
      } else {
	 *grp_ent = grp;
      }
   }

   /* Perhaps the root object doesn't even exist! */
   if (!H5F_addr_defined (&(ret_value->header))) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, NULL, "root not found");
   }
   
   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_mkroot
 *
 * Purpose:	Creates the root group if it doesn't exist; otherwise
 *		nothing happens.  If the root symbol table entry previously
 *		pointed to something other than a group, then that object
 *		is made a member of the root group and is given a name
 *		corresponding to the object's name message (the name message
 *		is removed).  If the root object doesn't have a name message
 *		then the name `Root Object' is used.
 *
 * Warning:	This function has a few subtleties. Be warned!
 *
 * Errors:
 *		SYM       CANTINIT      Can't open root object. 
 *		SYM       CANTINIT      Can't reinsert old root object. 
 *		SYM       CANTINIT      Cant create root. 
 *		SYM       EXISTS        Root group already exists. 
 *		SYM       LINK          Bad link count on old root object. 
 *		SYM       LINK          Cant create root. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL.  This function returns -2 if the
 *				failure is because a root group already
 *				exists.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_mkroot (H5F_t *f, size_t size_hint)
{
   H5G_entry_t	*handle=NULL;		/*handle to open object		*/
   herr_t	ret_value=FAIL;		/*return value			*/
   H5O_name_t	name={NULL};		/*object name			*/
   H5O_stab_t	stab;			/*symbol table message		*/
   H5G_entry_t	*ent_ptr=NULL;		/*pointer to a symbol table entry*/
   const char	*obj_name=NULL;		/*name of old root object	*/
   
   FUNC_ENTER (H5G_mkroot, FAIL);

   /*
    * Make sure that the file descriptor has the latest info -- someone might
    * have the root object open.
    */
   H5G_shadow_sync (f->shared->root_sym);
   
   /*
    * If we already have a root object, then open it and get it's name. The
    * root object had better not already be a group.  Once the old root
    * object is opened and we have a HANDLE, set the dirty bit on the handle.
    * This causes the handle data to be written back into f->root_sym by
    * H5G_close() if something goes wrong before the old root object is
    * re-inserted back into the group directed graph.  We might leak file
    * memory, but at least we don't loose the original root object.
    */
   if (H5F_addr_defined (&(f->shared->root_sym->header))) {
      if (H5O_read (f, NO_ADDR, f->shared->root_sym, H5O_STAB, 0, &stab)) {
	 HGOTO_ERROR (H5E_SYM, H5E_EXISTS, -2,
		      "root group already exists");
      } else if (NULL==(handle=H5G_shadow_open (f, NULL,
						f->shared->root_sym))) {
	 HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL,
		      "can't open root object");
      } else if (NULL==H5O_read (f, NO_ADDR, handle, H5O_NAME, 0, &name)) {
	 obj_name = "Root Object";
      } else {
	 obj_name = name.s; /*don't reset message until the end!*/
      }
      handle->dirty = TRUE;
   }

   /*
    * Create the new root group directly into the file descriptor. If
    * something goes wrong at this step, closing the `handle' will rewrite
    * info back into f->root_sym because we set the dirty bit.
    */
   if (H5G_stab_create (f, f->shared->root_sym, size_hint)<0) {
      HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "cant create root");
   }
   if (1!=H5O_link (f, f->shared->root_sym, 1)) {
      HGOTO_ERROR (H5E_SYM, H5E_LINK, FAIL,
		   "internal error (wrong link count)");
   }

   /*
    * If there was a previous root object then insert it into the new root
    * symbol table with the specified name.  Inserting the object will update
    * the handle to point to the new symbol table slot instead of f->root_sym.
    */
   if (obj_name) {
      if (1!=H5O_link (f, handle, 0)) {
	 HGOTO_ERROR (H5E_SYM, H5E_LINK, FAIL,
		      "bad link count on old root object");
      }
      if (NULL==(ent_ptr=H5G_stab_insert (f, f->shared->root_sym, obj_name,
					  handle))) {
	 HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL,
		      "can't reinsert old root object");
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
    * reinserts the root object into the group directed graph, then
    * H5G_close() will reset f->root_sym to point to the old root symbol and
    * the new root group (if it was created) will be unlinked from the
    * group directed graph (and memory leaked).
    */
   if (handle) H5G_close (f, handle);
   H5O_reset (H5O_NAME, &name);
   
   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_new
 *
 * Purpose: 	Creates a new empty group with the specified name,
 *		opening it as an object. The name is either an absolute name
 *		or is relative to the current working group.
 *
 * 		A root group is created implicitly by this function
 *		when necessary.  Calling this function with the name "/"
 *		(or any equivalent name) will result in an H5E_EXISTS
 * 		failure.
 *
 * Errors:
 *		SYM       CANTINIT      Can't create grp. 
 *		SYM       CANTINIT      Can't create root group. 
 *		SYM       CANTINIT      Can't insert. 
 *		SYM       CANTINIT      Can't open. 
 *		SYM       COMPLEN       Component is too long. 
 *		SYM       EXISTS        Already exists. 
 *		SYM       NOTFOUND      Missing component. 
 *
 * Return:	Success:	A handle to the open group.  Please call
 *				H5G_close() when you're done with it.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_entry_t *
H5G_new (H5F_t *f, const char *name, size_t size_hint)
{
   const char	*rest=NULL;		/*the base name			*/
   H5G_entry_t	*cwg=NULL;		/*current working group		*/
   H5G_entry_t	grp_ent;		/*group containing new group	*/
   H5G_entry_t	ent;			/*new group entry		*/
   H5G_entry_t	*ent_ptr=NULL;		/*ptr to new group entry	*/
   H5G_entry_t	*ret_value=NULL;	/*handle return value		*/
   char		_comp[1024];		/*name component		*/
   size_t	nchars;			/*number of characters in compon*/
   herr_t	status;			/*function return status	*/
   
   FUNC_ENTER (H5G_new, NULL);

   /* check args */
   assert (f);
   assert (name && *name);
#ifndef LATER
   /* Get current working group */
   H5G_shadow_sync (f->shared->root_sym);
   cwg = f->shared->root_sym;
#endif
   assert (cwg || '/'==*name);

   /*
    * Try to create the root group.  Ignore the error if this function
    * fails because the root group already exists.
    */
   if ((status=H5G_mkroot (f, H5G_SIZE_HINT))<0 && -2!=status) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL,
		     "can't create root group");
   }
   H5ECLEAR;

   /* lookup name */
   if (H5G_namei (f, cwg, name, &rest, &grp_ent)) {
      HRETURN_ERROR (H5E_SYM, H5E_EXISTS, NULL, "already exists");
   }
   H5ECLEAR; /*it's OK that we didn't find it*/

   /* should be one null-terminated component left */
   rest = H5G_component (rest, &nchars);
   assert (rest && *rest);
   if (rest[nchars]) {
      if (H5G_component (rest+nchars, NULL)) {
	 HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, NULL,
			"missing component");
      } else if (nchars+1 > sizeof _comp) {
	 HRETURN_ERROR (H5E_SYM, H5E_COMPLEN, NULL,
			"component is too long");
      } else {
	 /* null terminate */
	 HDmemcpy (_comp, rest, nchars);
	 _comp[nchars] = '\0';
	 rest = _comp;
      }
   }
   
   /* create group */
   if (H5G_stab_create (f, &ent, size_hint)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL, "can't create grp");
   }

   /* insert child name into parent */
   if (NULL==(ent_ptr=H5G_stab_insert (f, &grp_ent, rest, &ent))) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL, "can't insert");
   }

   /* open the group */
   if (NULL==(ret_value=H5G_shadow_open (f, &grp_ent, ent_ptr))) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL, "can't open");
   }

   FUNC_LEAVE (ret_value);
}



/*-------------------------------------------------------------------------
 * Function:	H5G_set
 *
 * Purpose:	Sets the current working group to be the specified name.
 *		This affects only the top item on the group stack for the
 *		specified file as accessed through this file handle.  If the
 *		file is opened multiple times, then the current working group
 *		for this file handle is the only one that is changed.
 *
 * Errors:
 *		SYM       CWG           Can't open group. 
 *		SYM       CWG           Couldn't close previous c.w.g. 
 *		SYM       CWG           Not a group. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_set (H5F_t *f, const char *name)
{
   H5G_entry_t	*handle=NULL;
   H5O_stab_t	stab_mesg;
   herr_t	ret_value=FAIL;
   
   FUNC_ENTER (H5G_set, FAIL);

   if (NULL==(handle=H5G_open (f, name))) {
      HGOTO_ERROR (H5E_SYM, H5E_CWG, FAIL, "can't open group");
   }
   if (NULL==H5O_read (f, NO_ADDR, handle, H5O_NAME, 0, &stab_mesg)) {
      HGOTO_ERROR (H5E_SYM, H5E_CWG, FAIL, "not a group");
   }

   /*
    * If there is no stack then create one, otherwise close the current
    * working group.
    */
   if (!f->cwg_stack) {
      f->cwg_stack = H5MM_xcalloc (1, sizeof(H5G_cwgstk_t));
      f->cwg_stack->handle = handle;
   } else {
      if (H5G_close (f, f->cwg_stack->handle)<0) {
	 HGOTO_ERROR (H5E_SYM, H5E_CWG, FAIL,
		      "couldn't close previous c.w.g.");
      }
      f->cwg_stack->handle = handle;
   }
   ret_value = SUCCEED;

 done:
   if (ret_value<0 && handle) {
      H5G_close (f, handle);
   }
   
   FUNC_LEAVE (ret_value);
}



/*-------------------------------------------------------------------------
 * Function:	H5G_getcwg
 *
 * Purpose:	Returns a handle for the current working group.  If there
 *		is no current working group then a pointer to the root
 *		symbol is returned but that object is not opened (and it
 *		might not even be a group).
 *
 * Return:	Success:	Ptr to open group handle with exceptions
 *				noted above.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5G_entry_t *
H5G_getcwg (H5F_t *f)
{
   H5G_entry_t	*handle=NULL;
   
   FUNC_ENTER (H5G_getcwg, NULL);
   
   if (f->cwg_stack && f->cwg_stack->handle) {
      handle = f->cwg_stack->handle;
   } else {
      H5G_shadow_sync (f->shared->root_sym);
      handle = f->shared->root_sym;
   }

   FUNC_LEAVE (handle);
}



/*-------------------------------------------------------------------------
 * Function:	H5G_push
 *
 * Purpose:	Pushes a new current working group onto the stack.
 *
 * Errors:
 *		SYM       CWG           Can't open group. 
 *		SYM       CWG           Not a group. 
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
H5G_push (H5F_t *f, const char *name)
{
   H5G_entry_t	*handle=NULL;
   H5G_cwgstk_t	*stack=NULL;
   H5O_stab_t	stab_mesg;
   herr_t	ret_value = FAIL;
   
   FUNC_ENTER (H5G_push, FAIL);

   if (NULL==(handle=H5G_open (f, name))) {
      HGOTO_ERROR (H5E_SYM, H5E_CWG, FAIL, "can't open group");
   }
   if (NULL==H5O_read (f, NO_ADDR, handle, H5O_NAME, 0, &stab_mesg)) {
      HGOTO_ERROR (H5E_SYM, H5E_CWG, FAIL, "not a group");
   }

   /*
    * Push a new entry onto the stack.
    */
   stack = H5MM_xcalloc (1, sizeof(H5G_cwgstk_t));
   stack->handle = handle;
   stack->next = f->cwg_stack;
   f->cwg_stack = stack;
   ret_value = SUCCEED;

 done:
   if (ret_value<0 && handle) {
      H5G_close (f, handle);
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_pop
 *
 * Purpose:	Pops the top current working group off the stack.
 *
 * Errors:
 *		SYM       CWG           Can't close current working group. 
 *		SYM       CWG           Stack is empty. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL if the stack is empty.
 *
 * Programmer:	Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_pop (H5F_t *f)
{
   H5G_cwgstk_t	*stack=NULL;
   
   FUNC_ENTER (H5G_pop, FAIL);

   if ((stack=f->cwg_stack)) {
      if (H5G_close (f, stack->handle)<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CWG, FAIL,
			"can't close current working group");
      }
      f->cwg_stack = stack->next;
      stack->handle = NULL;
      H5MM_xfree (stack);
   } else {
      HRETURN_ERROR (H5E_SYM, H5E_CWG, FAIL, "stack is empty");
   }

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
 * Errors:
 *		SYM       CANTINIT      Bad link count. 
 *		SYM       CANTINIT      Can't create header. 
 *		SYM       CANTINIT      Can't create root group. 
 *		SYM       CANTINIT      Can't insert. 
 *		SYM       CANTINIT      Can't open object. 
 *		SYM       CANTINIT      Cannot add/change name message. 
 *		SYM       CANTINIT      Create the object header. 
 *		SYM       COMPLEN       Component is too long. 
 *		SYM       EXISTS        Already exists. 
 *		SYM       EXISTS        Root exists. 
 *		SYM       LINK          Bad link count. 
 *		SYM       LINK          Link inc failure. 
 *		SYM       NOTFOUND      Component not found. 
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
H5G_create (H5F_t *f, const char *name, size_t ohdr_hint)
{
   H5G_entry_t	ent;			/*entry data for the new object	*/
   H5G_entry_t	*ent_ptr;		/*ptr into symbol node for entry*/
   H5G_entry_t	*cwg=NULL;		/*ptr to c.w.g. handle		*/
   const char	*rest = NULL;		/*part of name not existing yet	*/
   H5G_entry_t	grp;			/*entry for group to contain obj*/
   H5G_entry_t	*ret_value=NULL;	/*the object handle		*/
   size_t	nchars;			/*number of characters in name	*/
   char		_comp[1024];		/*name component		*/
   
   FUNC_ENTER (H5G_create, NULL);

   /* Check args. */
   assert (f);
   assert (name && *name);
   HDmemset (&ent, 0, sizeof(H5G_entry_t));

   /*
    * Get the current working group.
    */
   cwg = H5G_getcwg (f);

   /*
    * Look up the name -- it shouldn't exist yet.
    */
   if (H5G_namei (f, cwg, name, &rest, &grp)) {
      HRETURN_ERROR (H5E_SYM, H5E_EXISTS, NULL, "already exists");
   }
   H5ECLEAR; /*it's OK that we didn't find it*/
   rest = H5G_component (rest, &nchars);

   if (!rest || !*rest) {
      /*
       * The caller is attempting to insert a root object that either
       * doesn't have a name or we shouldn't interfere with the name
       * it already has as a message.
       */
      if (H5F_addr_defined (&(f->shared->root_sym->header))) {
	 HRETURN_ERROR (H5E_SYM, H5E_EXISTS, NULL, "root exists");
      }
      if (H5O_create (f, 0, ohdr_hint, &(ent.header)/*out*/)<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL,
			"can't create header");
      }
      if (1!=H5O_link (f, &ent, 1)) {
	 HRETURN_ERROR (H5E_SYM, H5E_LINK, NULL, "bad link count");
      }
      *(f->shared->root_sym) = ent;
      if (NULL==(ret_value=H5G_shadow_open (f, NULL, f->shared->root_sym))) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL,
			"can't open root object");
      }
      HRETURN (ret_value);
   }

   /*
    * There should be one component left.  Make sure it's null
    * terminated.
    */
   if (rest[nchars]) {
      if (H5G_component (rest+nchars, NULL)) {
	 HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, NULL,
			"component not found");
      } else if (nchars+1 > sizeof _comp) {
	 HRETURN_ERROR (H5E_SYM, H5E_COMPLEN, NULL,
			"component is too long");
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
   if (H5O_create (f, 0, ohdr_hint, &(ent.header)/*out*/)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL,
		     "can't create object header");
   }
   

   if (!H5F_addr_defined (&(f->shared->root_sym->header))) {
      /*
       * This will be the only object in the file. Insert it as the root
       * object and add a name messaage to the object header (or modify
       * the first one we find). Although the header exists we can guarantee
       * that it isn't open since it has no name.
       */
      H5O_name_t name_mesg;
      name_mesg.s = rest;
      if (H5O_modify (f, NO_ADDR, &ent, H5O_NAME, 0, &name_mesg)<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL,
			"cannot add/change name message");
      }
      if (1!=H5O_link (f, &ent, 1)) {
	 HRETURN_ERROR (H5E_SYM, H5E_LINK, NULL, "bad link count");
      }
      *(f->shared->root_sym) = ent;
      if (NULL==(ret_value=H5G_shadow_open (f, &grp, f->shared->root_sym))) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL,
			"can't open root object");
      }
      HRETURN (ret_value);
   } else {
      /*
       * Make sure the root group exists.  Ignore the failure if it's
       * because the group already exists.
       */
      hbool_t update_grp = H5F_addr_eq (&(grp.header),
					&(f->shared->root_sym->header));
      herr_t status = H5G_mkroot (f, H5G_SIZE_HINT);
      if (status<0 && -2!=status) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL,
			"can't create root group");
      }
      H5ECLEAR;
      if (update_grp) grp = *(f->shared->root_sym);
   }
   
   /*
    * This is the normal case.  The object is just being inserted as a normal
    * entry into a symbol table.
    */
   if (H5O_link (f, &ent, 1)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_LINK, NULL, "link inc failure");
   }
   if (NULL==(ent_ptr=H5G_stab_insert (f, &grp, rest, &ent))) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL, "can't insert");
   }
   if (NULL==(ret_value=H5G_shadow_open (f, &grp, ent_ptr))) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, NULL, "can't open object");
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
 * Errors:
 *		SYM       BADVALUE      Check args. 
 *		SYM       CANTOPENOBJ   Can't open obj. 
 *		SYM       NOTFOUND      Object not found. 
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
H5G_open (H5F_t *f, const char *name)
{
   H5G_entry_t	*ent=NULL;
   H5G_entry_t	*ret_value=NULL;
   H5G_entry_t	grp;
   H5G_entry_t	*cwg=NULL;
   
   FUNC_ENTER (H5G_open, NULL);

   /* check args */
   assert (f);
   if (!name || !*name) {
      HRETURN_ERROR (H5E_SYM, H5E_BADVALUE, NULL, "no name");
   }

   /* Get CWG */
   cwg = H5G_getcwg (f);
   assert (cwg || '/'==*name);

   if (!H5F_addr_defined (&(f->shared->root_sym->header))) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, NULL, "object not found");
   }
   if (NULL==(ent=H5G_namei (f, cwg, name, NULL, &grp))) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, NULL, "object not found");
   }
   if (NULL==(ret_value=H5G_shadow_open (f, &grp, ent))) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTOPENOBJ, NULL, "can't open obj");
   }

   FUNC_LEAVE (ret_value);
}



/*-------------------------------------------------------------------------
 * Function:	H5G_close
 *
 * Purpose:	Closes an object that was open for modification.
 *
 * Errors:
 *		SYM       CANTFLUSH     Can't close object. 
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
H5G_close (H5F_t *f, H5G_entry_t *ent)
{
   FUNC_ENTER (H5G_close, FAIL);

   assert (f);

   if (ent && H5G_shadow_close (f,  ent)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTFLUSH, FAIL,
		     "can't close object");
   }
   
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_find
 *
 * Purpose:	Finds an object with the specified NAME in file F.  If
 *		the name is relative then it is interpretted relative
 *		to the current working group.  On successful return,
 *		GRP_ENT (if non-null) will be initialized with the symbol
 *		table information for the group in which the object
 *		appears (or all zero if the returned object is the root
 *		object) and ENT will be initialized with the symbol table
 *		entry for the object (ENT is optional when the caller is
 *		interested only in the existence of the object).
 *
 * 		This function will fail if the root object is
 * 		requested and there is none.
 *
 * Errors:
 *		SYM       NOTFOUND      Object not found. 
 *
 * Return:	Success:	SUCCEED with GRP_ENT and ENT initialized. ENT
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
 *		matzke@llnl.gov
 *		Aug 12 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_find (H5F_t *f, const char *name,
	  H5G_entry_t *grp_ent/*out*/, H5G_entry_t *ent/*out*/)
{
   H5G_entry_t	*ent_p = NULL;
   H5G_entry_t	*cwg = NULL;
   
   FUNC_ENTER (H5G_find, FAIL);

   /* check args */
   assert (f);
   assert (name && *name);
   cwg = H5G_getcwg (f);
   assert (cwg || '/'==*name);

   if (!H5F_addr_defined (&(f->shared->root_sym->header))) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");
   }

   if (NULL==(ent_p=H5G_namei (f, cwg, name, NULL, grp_ent))) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");
   }

   if (ent) *ent = *ent_p;
   FUNC_LEAVE (SUCCEED);
}

