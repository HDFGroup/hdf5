/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		tohdr.c
 * 			Aug  6 1997
 * 			Robb Matzke <robb@maya.nuance.com>
 *
 * Purpose:		
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#include "testhdf5.h"

#include "H5ACprivate.h"
#include "H5Fprivate.h"
#include "H5Gprivate.h"
#include "H5Oprivate.h"


/*-------------------------------------------------------------------------
 * Function:	test_ohdr
 *
 * Purpose:	Test object headers.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
test_ohdr (void)
{
   hatom_t	fid;
   hdf5_file_t	*f;
   haddr_t	oh;
   H5O_stab_t	stab, ro;
   herr_t	status;
   void		*ptr;
   H5G_entry_t	ent;
   hbool_t	ent_mod;
   int		i;
   
   MESSAGE (5, print_func("Testing Object Headers\n"););

   /* create the file */
   fid = H5Fcreate ("tohdr.h5", H5ACC_OVERWRITE, 0, 0);
   CHECK (fid, FAIL, "H5Fcreate");
   f = H5Aatom_object (fid);
   CHECK (f, NULL, "H5Aatom_object");

   /* the new object header */
   MESSAGE (8, print_func("Creating new object header...\n"););
   oh = H5O_new (f, 1, 64);

   /*
    * Test creation of a new message.
    */
   MESSAGE (8, print_func("Creating new message...\n"););
   stab.btree = 11111111;
   stab.heap  = 22222222;
   status = H5O_modify (f, oh, NULL, NULL, H5O_STAB, H5O_NEW_MESG, &stab);
   CHECK (status, FAIL, "H5O_modify");

   H5AC_flush (f, NULL, 0, TRUE);
   ptr = H5O_read (f, oh, NULL, H5O_STAB, 0, &ro);
   CHECK_PTR (ptr, "H5O_read");
   VERIFY (ptr, &ro, "H5O_read");
   VERIFY (ro.btree, stab.btree, "H5O_read");
   VERIFY (ro.heap, stab.heap, "H5O_read");

   /*
    * Test modification of an existing message.
    */
   MESSAGE (8, print_func("Modifying message...\n"););
   stab.btree = 33333333;
   stab.heap  = 44444444;
   status = H5O_modify (f, oh, NULL, NULL, H5O_STAB, 0, &stab);
   CHECK (status, FAIL, "H5O_modify");

   H5AC_flush (f, NULL, 0, TRUE);
   ptr = H5O_read (f, oh, NULL, H5O_STAB, 0, &ro);
   CHECK_PTR (ptr, "H5O_read");
   VERIFY (ptr, &ro, "H5O_read");
   VERIFY (ro.btree, stab.btree, "H5O_read");
   VERIFY (ro.heap, stab.heap, "H5O_read");

   /*
    * Test creation of a second message of the same type with a symbol
    * table.
    */
   MESSAGE (8, print_func("Creating a duplicate message...\n"););
   ent.header = 0;
   ent.type = H5G_NOTHING_CACHED;
   stab.btree = 55555555;
   stab.heap  = 66666666;
   status = H5O_modify (f, oh, &ent, &ent_mod, H5O_STAB, H5O_NEW_MESG, &stab);
   CHECK (status, FAIL, "H5O_modify");
   VERIFY (ent_mod, TRUE, "H5O_modify");
   VERIFY (ent.type, H5G_CACHED_STAB, "H5O_modify");
   VERIFY (ent.cache.stab.heap, stab.heap, "H5O_modify");
   VERIFY (ent.cache.stab.btree, stab.btree, "H5O_modify");

   H5AC_flush (f, NULL, 0, TRUE);
   ptr = H5O_read (f, oh, NULL, H5O_STAB, 1, &ro);
   CHECK_PTR (ptr, "H5O_read");
   VERIFY (ptr, &ro, "H5O_read");
   VERIFY (ro.btree, stab.btree, "H5O_read");
   VERIFY (ro.heap, stab.heap, "H5O_read");

   /*
    * Test modification of the second message with a symbol table.
    */
   MESSAGE (8, print_func("Modifying the duplicate message...\n"););
   stab.btree = 77777777;
   stab.heap  = 88888888;
   status = H5O_modify (f, oh, &ent, &ent_mod, H5O_STAB, 1, &stab);
   CHECK (status, FAIL, "H5O_modify");
   VERIFY (ent_mod, TRUE, "H5O_modify");
   VERIFY (ent.type, H5G_CACHED_STAB, "H5O_modify");
   VERIFY (ent.cache.stab.heap, stab.heap, "H5O_modify");
   VERIFY (ent.cache.stab.btree, stab.btree, "H5O_modify");

   H5AC_flush (f, NULL, 0, TRUE);
   ptr = H5O_read (f, oh, NULL, H5O_STAB, 1, &ro);
   CHECK_PTR (ptr, "H5O_read");
   VERIFY (ptr, &ro, "H5O_read");
   VERIFY (ro.btree, stab.btree, "H5O_read");
   VERIFY (ro.heap, stab.heap, "H5O_read");

   /*
    * Test creation of a bunch of messages one after another to see
    * what happens when the object header overflows in core.
    */
   MESSAGE (8, print_func("Overflowing header in core...\n"););
   for (i=0; i<40; i++) {
      stab.btree = (i+1)*1000 + 1;
      stab.heap  = (i+1)*1000 + 2;
      status = H5O_modify (f, oh, NULL, NULL, H5O_STAB, H5O_NEW_MESG, &stab);
      CHECK (status, FAIL, "H5O_modify");
   }
   H5AC_flush (f, NULL, 0, TRUE);

   /*
    * Test creation of a bunch of messages one after another to see
    * what happens when the object header overflows on disk.
    */
   MESSAGE (8, print_func("Overflowing header on disk...\n"););
   for (i=0; i<10; i++) {
      stab.btree = (i+1)*1000 + 10;
      stab.heap  = (i+1)*1000 + 20;
      status = H5O_modify (f, oh, NULL, NULL, H5O_STAB, H5O_NEW_MESG, &stab);
      CHECK (status, FAIL, "H5O_modify");
      H5AC_flush (f, NULL, 0, TRUE);
   }
      

   

   /* close the file */
   H5Fclose (fid);
}
