/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		tstab.c
 * 			Aug  7 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#include <testhdf5.h>

#include <H5private.h>
#include <H5ACprivate.h>
#include <H5Cprivate.h>
#include <H5Fprivate.h> 
#include <H5Gprivate.h>
#include <H5Oprivate.h>

/*
 * This file needs to access private datatypes from the H5G package.
 */
#define H5G_PACKAGE
#include <H5Gpkg.h>


/*-------------------------------------------------------------------------
 * Function:	test_1
 *
 * Purpose:	Tests the non-directory features of the HDF5 file.  If the
 *		file has just one non-directory object, then that object
 *		should be the root object and there is no directory.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 29 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_1 (void)
{
   hid_t	fid;
   H5F_t	*f;
   H5G_entry_t	ent1, ent2, obj_ent, dir_ent;
   herr_t	status;
   H5O_name_t	name_mesg;
   void		*status_ptr;
   hbool_t	b;
   
   MESSAGE (2, ("........non-directory files\n"));

   /*
    * Test 1A: Create an empty file and add a non-directory object
    * to the file with the name `/'.  The object should become the
    * root object and should not have a name message.
    */

   /* create the file */
   fid = H5Fcreate ("tstab1.h5", H5ACC_OVERWRITE, 0, 0);
   CHECK (fid, FAIL, "H5Fcreate");
   f = H5Aatom_object (fid);
   CHECK (f, NULL, "H5Aatom_object");

   /* create the object */
   status = H5O_create (f, 0, &ent1);
   CHECK_I (status, "H5O_create");
   status = H5G_insert ("/", &ent1);
   CHECK_I (status, "H5G_insert");
   status = H5O_close (&ent1);
   CHECK_I (status, "H5O_close");

   /* look for a name message -- it shouldn't be present */
   status_ptr = H5O_read (&ent1, H5O_NAME, 0, &name_mesg);
   VERIFY (status_ptr, NULL, "H5O_read [didn't fail but should have]");
   
   /*
    * Test 1B: Attempt to read the root object using the name `/'.
    */
   memset (&dir_ent, 0xff, sizeof(H5G_entry_t));
   memset (&obj_ent, 0xff, sizeof(H5G_entry_t));
   status = H5G_find (f, "/", &dir_ent, &obj_ent);
   CHECK_I (status, "H5G_find");

   /* Is it really the root object? */
   b = H5F_addr_defined (&(dir_ent.header));
   VERIFY (b, FALSE, "H5G_insert");
   b = H5F_addr_eq (&(obj_ent.header), &(ent1.header));
   VERIFY (b, TRUE, "H5G_insert");

   
   /*
    * Test 1C: Add a second object to the file to see if the first object
    * gets moved into the new root directory along with the second object.
    */

   /* create the object */
   status = H5O_create (f, 0, &ent2);
   CHECK_I (status, "H5O_create");
   status = H5G_insert ("/second", &ent2);
   CHECK_I (status, "H5G_insert");
   status = H5O_close (&ent2);
   CHECK_I (status, "H5O_close");

   /* try to read the first object */
   HDmemset (&obj_ent, 0xff, sizeof(H5G_entry_t));
   status = H5G_find (f, "/Root Object", NULL, &obj_ent);
   CHECK_I (status, "H5G_find");
   b = H5F_addr_defined (&(obj_ent.header));
   VERIFY (b, TRUE, "H5G_insert");
   b = H5F_addr_eq (&(obj_ent.header), &(ent1.header));
   VERIFY (b, TRUE, "H5G_create");

   /* close the file */
   H5Fclose (fid);

   
   
   /*
    * Test 1D: Create an empty file and add a non-directory object
    * to the file with the name `/foo'.  The object should become the
    * root object and should have a name message with the value `foo'.
    */

   /* create the file */
   fid = H5Fcreate ("tstab1.h5", H5ACC_OVERWRITE, 0, 0);
   CHECK (fid, FAIL, "H5Fcreate");
   f = H5Aatom_object (fid);
   CHECK (f, NULL, "H5Aatom_object");

   /* create the object */
   status = H5O_create (f, 0, &ent1);
   CHECK_I (status, "H5O_create");
   status = H5G_insert ("/foo", &ent1);
   CHECK_I (status, "H5G_insert");
   status = H5O_close (&ent1);
   CHECK_I (status, "H5O_close");

   /* does it have the correct name message? */
   status_ptr = H5O_read (&ent1, H5O_NAME, 0, &name_mesg);
   CHECK_PTR (status_ptr, "H5O_read");
   CHECK_PTR (name_mesg.s, "H5O_read");
   VERIFY (strcmp(name_mesg.s, "foo"), 0, "H5G_insert");
   if (status_ptr) H5O_reset (H5O_NAME, &name_mesg); /*free message data*/


   /*
    * Test 1E: Try to read the root object with the name `/' and `/foo'
    */
   HDmemset (&dir_ent, 0, sizeof(H5G_entry_t));
   HDmemset (&obj_ent, 0, sizeof(H5G_entry_t));
   status = H5G_find (f, "/", &dir_ent, &obj_ent);
   CHECK_I (status, "H5G_find");
   b = H5F_addr_defined (&(dir_ent.header));
   VERIFY (b, FALSE, "H5G_insert");
   b = H5F_addr_eq (&(obj_ent.header), &(ent1.header));
   VERIFY (b, TRUE, "H5G_insert");

   /* now as `/foo' */
   HDmemset (&dir_ent, 0, sizeof(H5G_entry_t));
   HDmemset (&obj_ent, 0, sizeof(H5G_entry_t));
   status = H5G_find (f, "/foo", &dir_ent, &obj_ent);
   CHECK_I (status, "H5G_find");
   b = H5F_addr_defined (&(dir_ent.header));
   VERIFY (b, FALSE, "H5G_insert");
   b = H5F_addr_eq (&(obj_ent.header), &(ent1.header));
   VERIFY (b, TRUE, "H5G_insert");

   
   /*
    * Test 1F: Create another object.  This should create a root directory
    * and move the previous root object into that directory.
    */

   /* create the object */
   status = H5O_create (f, 0, &ent2);
   CHECK_I (status, "H5O_create");
   status = H5G_insert ("/second", &ent2);
   CHECK_I (status, "H5G_insert");
   status = H5O_close (&ent2);
   CHECK_I (status, "H5O_close");

   /* try to read the first object */
   HDmemset (&obj_ent, 0, sizeof(H5G_entry_t));
   status = H5G_find (f, "/foo", NULL, &obj_ent);
   CHECK_I (status, "H5G_find");
   b = H5F_addr_eq (&(obj_ent.header), &(ent1.header));
   VERIFY (b, TRUE, "H5G_insert");

   /* the first object should not have a name message */
   status_ptr = H5O_read (&ent1, H5O_NAME, 0, &name_mesg);
   VERIFY (status_ptr, NULL, "H5O_read [didn't fail but should have]");

   /* close the file */
   status = H5Fclose (fid);
   CHECK_I (status, "H5Fclose");
}


/*-------------------------------------------------------------------------
 * Function:	test_2
 *
 * Purpose:	Creates a really large directory.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 29 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_2 (void)
{
   hid_t	fid, props, dir;
   H5F_t	*f;
   int		i;
   char		name[256];
   herr_t	status;
   int		nsyms = 5000;
   
   MESSAGE (2, ("........large directories\n"));

   /*
    * Use larger symbol table data structures to be more efficient, use
    * defaults to bang harder on the library for testing.
    */
   props = H5Ccreate (H5C_FILE_CREATE);
#if 1
   H5Cset_sym_k (props, 16, 16);
#endif

   /* create the file */
   fid = H5Fcreate ("tstab2.h5", H5ACC_OVERWRITE, props, 0);
   CHECK (fid, FAIL, "H5Fcreate");
   f = H5Aatom_object (fid);
   CHECK (f, NULL, "H5Aatom_object");
   f->intent |= H5F_ACC_DEBUG;

   /*
    * Create a directory that has so many entries that the root
    * of the B-tree ends up splitting.
    */
   dir = H5Gcreate (fid, "/big", nsyms*16+2);
   CHECK_I (dir, "H5Gcreate");
   status = H5Gclose (dir);
   CHECK_I (status, "H5Gclose");
   status = H5Gset (fid, "/big");
   CHECK_I (status, "H5Gset");
   
   
   for (i=0; i<nsyms; i++) {
      sprintf (name, "%05d%05d", rand()%100000, i);
      MESSAGE (8, ("%s\n", name));
      dir = H5Gcreate (fid, name, 0);
      CHECK_I (dir, "H5Gcreate");
      status = H5Gclose (dir);
      CHECK_I (status, "H5Gclose");
   }
   
   /* close the file */
   status = H5Fclose (fid);
   CHECK_I (status, "H5Fclose");
}   



/*-------------------------------------------------------------------------
 * Function:	test_stab
 *
 * Purpose:	Test symbol tables
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		matzke@viper.llnl.gov
 *		Aug  7 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
test_stab (void)
{
   test_1();
   test_2();
}
