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
   haddr_t	addr;
   H5G_entry_t	root, sub;
   int		i;
   char		name[256];
   herr_t	status;
   
   MESSAGE (5, print_func("Testing Symbol Tables\n"););

   /* create the file */
   fid = H5Fcreate ("tstab.h5", H5ACC_OVERWRITE, 0, 0);
   CHECK (fid, FAIL, "H5Fcreate");
   f = H5Aatom_object (fid);
   CHECK (f, NULL, "H5Aatom_object");

   /* create a new symbol table */
   addr = H5G_stab_new (f, &root, 100);
   CHECK_I (addr, "H5G_stab_new");
   MESSAGE (8, print_func ("Root address is %lu\n", (unsigned long)addr););

   /* create some empty symbol tables as objects in the root stab */
   for (i=0; i<16384; i++) {
      addr = H5G_stab_new (f, &sub, 0);
      CHECK_I (addr, "H5G_stab_new");

      sprintf (name, "sub%05d", i);
      MESSAGE (8, print_func ("%s\n", name););
      status = H5G_stab_insert (f, &root, name, &sub);
      CHECK_I (status, "H5G_stab_insert");
   }
   

   /* close the file */
   H5Fclose (fid);
}
