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
#include "testhdf5.h"

#include "H5ACprivate.h"
#include "H5Fprivate.h"
#include "H5Gprivate.h"
#include "H5Oprivate.h"


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
   hatom_t	fid;
   hdf5_file_t	*f;
   H5G_entry_t	cwd, sub;
   int		i;
   char		name[256];
   herr_t	status;
   int		nsyms = 5000;
   
   MESSAGE (5, print_func("Testing Symbol Tables\n"););

   /* create the file */
   fid = H5Fcreate ("tstab.h5", H5ACC_OVERWRITE, 0, 0);
   CHECK (fid, FAIL, "H5Fcreate");
   f = H5Aatom_object (fid);
   CHECK (f, NULL, "H5Aatom_object");

   /* create a new root symbol table */
   status = H5G_mkroot (f, 100);
   CHECK_I (status, "H5G_mkroot");

   /*
    * Create a directory that has so many entries that the root
    * of the B-tree ends up splitting.
    */
   status = H5G_new (f, NULL, NULL, "/big", nsyms*10+2, &cwd);
   CHECK_I (status, "H5G_new");
   H5G_stab_new (f, &sub, 0);
   status = H5O_link (f, sub.header, &sub, nsyms-1);
   CHECK_I (status, "H5O_link");
   
   
   for (i=0; i<5000; i++) {
      sprintf (name, "sub%05d", i);
      MESSAGE (8, print_func ("%s\n", name););
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
