/*
 * Copyright (C) 1997 National Center for Supercomputing Applications
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Friday, September 19, 1997
 *
 */
#define H5G_PACKAGE

#include <H5private.h>
#include <H5ACprivate.h>
#include <H5Eprivate.h>
#include <H5Gpkg.h>
#include <H5Hprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK	H5G_stab_mask
static hbool_t interface_initialize_g = FALSE;


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
 * Errors:
 *		INTERNAL  CANTINIT      B-tree's won't work if the first
 *		                        name isn't at the beginning of the
 *		                        heap. 
 *		SYM       CANTINIT      Can't create B-tree. 
 *		SYM       CANTINIT      Can't create header. 
 *		SYM       CANTINIT      Can't create heap. 
 *		SYM       CANTINIT      Can't create message. 
 *		SYM       CANTINIT      Can't initialize heap. 
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
H5G_stab_new (H5F_t *f, H5G_entry_t *self, size_t init)
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
   if ((stab.heap_addr = H5H_new (f, H5H_LOCAL, init))<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL); /*can't create heap*/
   }
   if ((name = H5H_insert (f, stab.heap_addr, 1, "")<0)) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL); /*can't initialize heap*/
   }
   if (0!=name) {
      /*
       * B-tree's won't work if the first name isn't at the beginning
       * of the heap.
       */
      HRETURN_ERROR (H5E_INTERNAL, H5E_CANTINIT, FAIL);
   }

   /* Create the B-tree */
   if ((stab.btree_addr = H5B_new (f, H5B_SNODE))<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL); /*can't create B-tree*/
   }

   /*
    * Create symbol table object header.  It has a zero link count
    * since nothing refers to it yet.  The link count will be
    * incremented if the object is added to the directory hierarchy.
    */
   if ((addr = H5O_new (f, 0, 4+2*H5F_SIZEOF_OFFSET(f)))<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL); /*can't create header*/
   }
   
   /* insert the symbol table message */
   if (self) {
      memset (self, 0, sizeof(H5G_entry_t));
      self->header = addr;
   }
   if (H5O_modify(f, addr, self, H5O_STAB, H5O_NEW_MESG, &stab)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL); /*can't create message*/
   }

   FUNC_LEAVE (addr);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_find
 *
 * Purpose:	Finds a symbol named NAME in the symbol table whose
 *		description is stored in SELF in file F and returns a
 *		pointer to the symbol table entry.  SELF is optional if the
 *		symbol table address is supplied through ADDR.
 *
 * Errors:
 *		SYM       BADMESG       Can't read message. 
 *		SYM       NOTFOUND      Not found. 
 *
 * Return:	Success:	Pointer to the symbol table entry.
 *				The pointer is intended for immediate
 *				read-only access since it points
 *				directly to an entry in a cached
 *				symbol table node.  The pointer is
 *				guaranteed to be valid only until the
 *				next call to one of the H5AC functions.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_entry_t *
H5G_stab_find (H5F_t *f, haddr_t addr, H5G_entry_t *self,
	       const char *name)
{
   H5G_bt_ud1_t         udata;		/*data to pass through B-tree	*/
   H5O_stab_t		stab;		/*symbol table message		*/

   FUNC_ENTER (H5G_stab_find, NULL, NULL);

   /* Check arguments */
   assert (f);
   if (addr<=0 && (!self || self->header<=0)) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, NULL);
   }
   if (!name || !*name) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, NULL);
   }
   if (addr<=0) addr = self->header;

   /* set up the udata */
   if (NULL==H5O_read (f, addr, self, H5O_STAB, 0, &stab)) {
      HRETURN_ERROR (H5E_SYM, H5E_BADMESG, NULL); /*can't read message*/
   }
   udata.operation = H5G_OPER_FIND;
   udata.name = name;
   udata.heap_addr = stab.heap_addr;
   udata.dir_addr = addr;
   udata.node_ptr = NULL;

   /* search the B-tree */
   if (H5B_find (f, H5B_SNODE, stab.btree_addr, &udata)<0) {
      if (udata.node_ptr) {
	 H5AC_unprotect (f, H5AC_SNODE, udata.node_addr, udata.node_ptr);
      }
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, NULL); /*not found*/
   }

   /* Unprotect the symbol table node */
   if (udata.node_ptr) {
      if (H5AC_unprotect (f, H5AC_SNODE, udata.node_addr, udata.node_ptr)<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_PROTECT, NULL);
      }
   }

   /* return the result */
   FUNC_LEAVE (udata.entry_ptr);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_insert
 *
 * Purpose:	Insert a new symbol into the table described by SELF in
 *		file F.  The name of the new symbol is NAME and its symbol
 *		table entry is ENT.
 *
 * Errors:
 *		SYM       BADMESG       Can't read message. 
 *		SYM       CANTINSERT    Can't insert entry. 
 *
 * Return:	Success:	Pointer to the cached symbol table entry.
 *				This is a pointer directly into a symbol
 *				table node and will become invalid on the
 *				next call to the H5AC package.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 * 	Robb Matzke, 18 Sep 1997
 *	If ENT has a shadow, then the shadow will be associated with the
 *	entry when it is added to the symbol table.
 *
 *-------------------------------------------------------------------------
 */
H5G_entry_t *
H5G_stab_insert (H5F_t *f, H5G_entry_t *self, const char *name,
		 H5G_entry_t *ent)
{
   H5O_stab_t		stab;		/*symbol table message		*/
   H5G_bt_ud1_t		udata;		/*data to pass through B-tree	*/

   FUNC_ENTER (H5G_stab_insert, NULL, NULL);

   /* check arguments */
   assert (f);
   assert (self && self->header>=0);
   assert (name && *name);
   assert (ent);
   
   /* initialize data to pass through B-tree */
   if (NULL==H5O_read (f, self->header, self, H5O_STAB, 0, &stab)) {
      HRETURN_ERROR (H5E_SYM, H5E_BADMESG, NULL); /*can't read message*/
   }

   udata.operation = H5G_OPER_INSERT;
   udata.name = name;
   udata.heap_addr = stab.heap_addr;
   udata.dir_addr = self->header;
   udata.entry = *ent;
   udata.entry.name_off = -1;
   udata.node_ptr = NULL;

   /* insert */
   if (H5B_insert (f, H5B_SNODE, stab.btree_addr, &udata)<0) {
      if (udata.node_ptr) {
	 H5AC_unprotect (f, H5AC_SNODE, udata.node_addr, udata.node_ptr);
      }
      HRETURN_ERROR (H5E_SYM, H5E_CANTINSERT, NULL); /*can't insert entry*/
   }

   /*
    * The H5G_node_insert() called H5AC_protect() for the node so that the
    * udata.entry_ptr field didn't become invalid on the way back out of the
    * B-tree code.  We unprotect it now, but the pointer will remain valid
    * until the next call to H5AC.
    */
   if (H5AC_unprotect (f, H5AC_SNODE, udata.node_addr, udata.node_ptr)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_PROTECT, NULL); /*can't unprotect*/
   }

   /* update the name offset in the entry */
   ent->name_off = udata.entry.name_off;
   FUNC_LEAVE (udata.entry_ptr);
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
 * Errors:
 *		SYM       BADMESG       Not a symbol table. 
 *		SYM       CANTLIST      B-tree list failure. 
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
H5G_stab_list (H5F_t *f, H5G_entry_t *self, intn maxentries,
	       char *names[], H5G_entry_t entries[])
{
   H5G_bt_ud2_t		udata;
   H5O_stab_t		stab;
   intn			i;

   FUNC_ENTER (H5G_stab_list, NULL, FAIL);

   /* check args */
   assert (f);
   assert (self && self->header>=0);
   assert (maxentries>=0);

   /* initialize data to pass through B-tree */
   if (NULL==H5O_read (f, self->header, self, H5O_STAB, 0, &stab)) {
      HRETURN_ERROR (H5E_SYM, H5E_BADMESG, FAIL); /*not a symbol table*/
   }
   udata.entry = entries;
   udata.name = names;
   udata.dir_addr = self->header;
   udata.heap_addr = stab.heap_addr;
   udata.maxentries = maxentries;
   udata.nsyms = 0;
   if (names) HDmemset (names, 0, maxentries);

   /* list */
   if (H5B_list (f, H5B_SNODE, stab.btree_addr, &udata)<0) {
      if (names) {
	 for (i=0; i<maxentries; i++) H5MM_xfree (names[i]);
      }
      HRETURN_ERROR (H5E_SYM, H5E_CANTLIST, FAIL); /*B-tree list failure*/
   }

   FUNC_LEAVE (udata.nsyms);
}
   
