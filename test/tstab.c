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
#include <H5Fprivate.h> 
#include <H5Gprivate.h>
#include <H5Oprivate.h>


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
   hatom_t	fid;
   hdf5_file_t	*f;
   H5G_entry_t	ent, ent2, dir_ent;
   herr_t	status;
   H5O_name_t	name_mesg;
   void		*status_ptr;
   
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
   ent.header = H5O_new (f, 0, 64);
   CHECK_I (ent.header, "H5O_new");
   ent.type = H5G_NOTHING_CACHED;

   /* give the object a name */
   status = H5G_insert (f, f->root_sym, &dir_ent, "/", &ent);
   CHECK_I (status, "H5G_insert");

   /* is it really the root symbol? */
   VERIFY (dir_ent.header, 0, "H5G_insert");
   VERIFY (ent.header, f->root_sym->header, "H5G_insert");

   /* look for a name message -- it shouldn't be present */
   status_ptr = H5O_read (f, ent.header, &ent, H5O_NAME, 0, &name_mesg);
   VERIFY (status_ptr, NULL, "H5O_read [didn't fail but should have]");
   


   /*
    * Test 1B: Attempt to read the root object using the name `/'.
    */
   
   HDmemset (&dir_ent, 0, sizeof(H5G_entry_t));
   HDmemset (&ent2, 0, sizeof(H5G_entry_t));
   status = H5G_find (f, NULL, &dir_ent, "/", &ent2);
   CHECK_I (status, "H5G_find");
   VERIFY (dir_ent.header, 0, "H5G_find");
   VERIFY (ent2.header, ent.header, "H5G_find");

   
   
   /*
    * Test 1C: Add a second object to the file to see if the first object
    * gets moved into the new root directory along with the second object.
    */

   /* create the object */
   ent2.header = H5O_new (f, 0, 64);
   CHECK_I (ent2.header, "H5O_new");
   ent2.type = H5G_NOTHING_CACHED;

   /* give the object a name */
   status = H5G_insert (f, f->root_sym, &dir_ent, "/second", &ent2);
   CHECK_I (status, "H5G_insert");
   CHECK (dir_ent.header, 0, "H5G_insert");

   /* try to read the first object */
   HDmemset (&ent2, 0, sizeof(H5G_entry_t));
   status = H5G_find (f, NULL, NULL, "/Root Object", &ent2);
   CHECK_I (status, "H5G_find");
   VERIFY (ent2.header, ent.header, "H5G_find");

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
   ent.header = H5O_new (f, 0, 64);
   CHECK_I (ent.header, "H5O_new");
   ent.type = H5G_NOTHING_CACHED;

   /* give the object a name */
   status = H5G_insert (f, f->root_sym, &dir_ent, "/foo", &ent);
   CHECK_I (status, "H5G_insert");

   /* is it really the root symbol? */
   VERIFY (dir_ent.header, 0, "H5G_insert");
   VERIFY (ent.header, f->root_sym->header, "H5G_insert");

   /* does it have the correct name message? */
   status_ptr = H5O_read (f, ent.header, &ent, H5O_NAME, 0, &name_mesg);
   CHECK_PTR (status_ptr, "H5O_read");
   CHECK_PTR (name_mesg.s, "H5O_read");
   VERIFY (strcmp(name_mesg.s, "foo"), 0, "H5O_read");



   /*
    * Test 1E: Try to read the root object with the name `/' and `/foo'
    */

   HDmemset (&dir_ent, 0, sizeof(H5G_entry_t));
   HDmemset (&ent2, 0, sizeof(H5G_entry_t));
   status = H5G_find (f, NULL, &dir_ent, "/", &ent2);
   CHECK_I (status, "H5G_find");
   VERIFY (dir_ent.header, 0, "H5G_find");
   VERIFY (ent2.header, ent.header, "H5G_find");

   HDmemset (&dir_ent, 0, sizeof(H5G_entry_t));
   HDmemset (&ent2, 0, sizeof(H5G_entry_t));
   status = H5G_find (f, NULL, &dir_ent, "/foo", &ent2);
   CHECK_I (status, "H5G_find");
   VERIFY (dir_ent.header, 0, "H5G_find");
   VERIFY (ent2.header, ent.header, "H5G_find");


   
   /*
    * Test 1F: Create another object.  This should create a root directory
    * and move the previous root object into that directory.
    */

   /* create the object */
   ent2.header = H5O_new (f, 0, 64);
   CHECK_I (ent2.header, "H5O_new");
   ent2.type = H5G_NOTHING_CACHED;

   /* give the object a name */
   status = H5G_insert (f, f->root_sym, &dir_ent, "/second", &ent2);
   CHECK_I (status, "H5G_insert");
   CHECK (dir_ent.header, 0, "H5G_insert");

   /* try to read the first object */
   HDmemset (&ent2, 0, sizeof(H5G_entry_t));
   status = H5G_find (f, NULL, NULL, "/foo", &ent2);
   CHECK_I (status, "H5G_find");
   VERIFY (ent2.header, ent.header, "H5G_find");

   /* the first object should not have a name message */
   status_ptr = H5O_read (f, ent.header, &ent, H5O_NAME, 0, &name_mesg);
   VERIFY (status_ptr, NULL, "H5O_read [didn't fail but should have]");

   /* close the file */
   H5Fclose (fid);
   
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
   hatom_t	fid;
   hdf5_file_t	*f;
   H5G_entry_t	cwd, sub;
   int		i;
   haddr_t	addr;
   char		name[256];
   herr_t	status;
   int		nsyms = 5000;
   
   MESSAGE (2, ("........large directories\n"));

   /* create the file */
   fid = H5Fcreate ("tstab2.h5", H5ACC_OVERWRITE, 0, 0);
   CHECK (fid, FAIL, "H5Fcreate");
   f = H5Aatom_object (fid);
   CHECK (f, NULL, "H5Aatom_object");

   /*
    * Create a directory that has so many entries that the root
    * of the B-tree ends up splitting.
    */
   status = H5G_new (f, NULL, NULL, "/big", nsyms*12+2, &cwd);
   CHECK_I (status, "H5G_new");
   addr = H5G_stab_new (f, &sub, 0);
   CHECK_I (addr, "H5G_stab_new");
   MESSAGE (8, ("Address %lu\n", (unsigned long)addr));
   
   
   for (i=0; i<nsyms; i++) {
      sprintf (name, "%05d%05d", rand()%100000, i);
      MESSAGE (8, ("%s\n", name));
#if 1
      status = H5G_insert (f, &cwd, NULL, name, &sub);
      CHECK_I (status, "H5G_insert");
#else
      status = H5G_stab_new (f, &sub, 0);
      CHECK_I (status, "H5G_stab_new");
      status = H5G_stab_insert (f, &cwd, name, &sub);
      CHECK_I (status, "H5G_stab_insert");
#endif
   }
   

   /* close the file */
   H5Fclose (fid);
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
