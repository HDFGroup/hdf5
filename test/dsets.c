/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, December  9, 1997
 *
 * Purpose:	Tests the dataset interface (H5D)
 */
#include <assert.h>
#include <hdf5.h>
#include <math.h>
#include <stdio.h>
#include <unistd.h>

#define AT() printf ("   at %s:%d in %s()...\n",			    \
		     __FILE__, __LINE__, __FUNCTION__);



/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Attempts to create a dataset.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_create (hid_t file)
{
   hid_t	dataset, space;
   intn		dims[2];
   herr_t	status;
   
   printf ("%-70s", "Testing create/open/close");


   /* Create the data space */
   space = H5Pcreate (H5P_SIMPLE);
   dims[0] = 256;
   dims[1] = 512;
   status = H5Pset_space (space, 2, dims);
   assert (status>=0);

   /* Create the dataset */
   dataset = H5Dcreate (file, "dataset_01", H5T_NATIVE_DOUBLE, space,
			H5C_DEFAULT);
   if (dataset<0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Cannot create initial dataset.\n");
      }
      goto error;
   }

   /* Close the dataset */
   if (H5Dclose (dataset)<0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Cannot close initial dataset.\n");
      }
      goto error;
   }

   /* Try creating a dataset that already exists.  This should fail. */
   dataset = H5Dcreate (file, "dataset_01", H5T_NATIVE_DOUBLE, space,
			H5C_DEFAULT);
   if (dataset>=0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Library allowed overwrite of existing dataset.\n");
      }
      goto error;
   }

   /* Try opening and closing an existing dataset */
   dataset = H5Dopen (file, "dataset_01");
   if (dataset<0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Cannot open dataset `dataset_01'.\n");
      }
      goto error;
   }
   if (H5Dclose (dataset)<0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Cannot close dataset.\n");
      }
      goto error;
   }

   /* Try opening a non-existent dataset. This should fail */
   dataset = H5Dopen (file, "does_not_exist");
   if (dataset>=0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Opened a non-existent dataset.\n");
      }
      goto error;
   }
   
   puts (" PASSED");
   return SUCCEED;

 error:
   return FAIL;
}



/*-------------------------------------------------------------------------
 * Function:	test_simple_io
 *
 * Purpose:	Tests simple I/O.  That is, reading and writing a complete
 *		multi-dimensional array without data type or data space
 *		conversions, without compression, and stored contiguously.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, December 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_simple_io (hid_t file)
{
   hid_t	dataset, space;
   herr_t	status;
   int		points[100][200], check[100][200];
   int		i, j, n, dims[2];

   printf ("%-70s", "Testing simple I/O");

   /* Initialize the dataset */
   for (i=n=0; i<100; i++) {
      for (j=0; j<100; j++) {
	 points[i][j] = n++;
      }
   }

   /* Create the data space */
   space = H5Pcreate (H5P_SIMPLE);
   dims[0] = 100;
   dims[1] = 200;
   status = H5Pset_space (space, 2, dims);
   assert (status>=0);

   /* Create the dataset */
   dataset = H5Dcreate (file, "dataset_02", H5T_NATIVE_INT, space,
			H5C_DEFAULT);
   assert (dataset>=0);

   /* Write the data to the dataset */
   status = H5Dwrite (dataset, H5T_NATIVE_INT, H5P_ALL, H5C_DEFAULT, points);
   if (status<0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   H5Dwrite() failed\n");
      }
      goto error;
   }

   /* Read the dataset back */
   status = H5Dread (dataset, H5T_NATIVE_INT, H5P_ALL, H5C_DEFAULT, check);
   if (status<0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   H5Dread() failed\n");
      }
      goto error;
   }

   /* Check that the values read are the same as the values written */
   for (i=0; i<100; i++) {
      for (j=0; j<200; j++) {
	 if (points[i][j]!=check[i][j]) {
	    puts ("*FAILED*");
	    if (!isatty (1)) {
	       AT ();
	       printf ("   Read different values than written.\n");
	       printf ("   At index %d,%d\n", i, j);
	    }
	    goto error;
	 }
      }
   }

   puts (" PASSED");
   return SUCCEED;

 error:
   return FAIL;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests the dataset interface (H5D)
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(1)
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
   hid_t	file;
   herr_t	status;
   intn		nerrors = 0;
   

   unlink ("dataset.h5");
   file = H5Fcreate ("dataset.h5", H5ACC_DEFAULT, H5C_DEFAULT, H5C_DEFAULT);
   assert (file>=0);

   status = test_create (file);
   nerrors += status<0 ? 1 : 0;

   status = test_simple_io (file);
   nerrors += status<0 ? 1 : 0;

   status = H5Fclose (file);

   if (nerrors) {
      printf ("***** %d DATASET TEST%s FAILED! *****\n",
	      nerrors, 1==nerrors?"":"S");
      if (isatty (1)) {
	 printf ("(Redirect output to a pager or a file to see debug "
		 "output)\n");
      }
      exit (1);
   }

   printf ("All dataset tests passed.\n");
   exit (0);
}
