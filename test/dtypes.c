/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, December  9, 1997
 *
 * Purpose:	Tests the data type interface (H5T)
 */
#include <hdf5.h>
#include <stdio.h>
#include <unistd.h>


#ifndef HAVE_FUNCTION
#define __FUNCTION__ ""
#endif
#define AT() printf ("   at %s:%d in %s()...\n",			    \
		     __FILE__, __LINE__, __FUNCTION__);



/*-------------------------------------------------------------------------
 * Function:	test_classes
 *
 * Purpose:	Test type classes
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
test_classes (void)
{
   H5T_class_t	type_class;

   printf ("%-70s", "Testing H5Tget_class()");

   if (H5T_FIXED!=(type_class=H5Tget_class (H5T_NATIVE_INT))) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Invalid type class for H5T_NATIVE_INT\n");
      }
      goto error;
   }

   if (H5T_FLOAT!=(type_class=H5Tget_class (H5T_NATIVE_DOUBLE))) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Invalid type class for H5T_NATIVE_DOUBLE\n");
      }
      goto error;
   }


   
   puts (" PASSED");
   return SUCCEED;

 error:
   return FAIL;
}



/*-------------------------------------------------------------------------
 * Function:	test_copy
 *
 * Purpose:	Are we able to copy a data type?
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
test_copy (void)
{
   hid_t	a_copy;
   
   printf ("%-70s", "Testing H5Tcopy()");

   if ((a_copy=H5Tcopy (H5T_NATIVE_SHORT))<0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Cannot copy a builtin type.\n");
      }
      goto error;
   }

   if (H5Tclose (a_copy)<0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Cannot close the copied type.\n");
      }
      goto error;
   }

   if (H5Tclose (H5T_NATIVE_CHAR)>=0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Was able to free a built-in type.\n");
      }
      goto error;
   }

   puts (" PASSED");
   return SUCCEED;

 error:
   return FAIL;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test the data type interface.
 *
 * Return:	Success:	
 *
 *		Failure:	
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
   herr_t	status;
   intn		nerrors = 0;

   H5init ();
   
   status = test_classes ();
   nerrors += status<0 ? 1 : 0;

   status = test_copy ();
   nerrors += status<0 ? 1 : 0;
   

   if (nerrors) {
      printf ("***** %d DATA TYPE TEST%s FAILED! *****\n",
	      nerrors, 1==nerrors?"":"S");
      if (isatty (1)) {
	 printf ("(Redirect output to a pager or a file to see debug "
		 "output)\n");
      }
      exit (1);
   }

   printf ("All data type tests passed.\n");
   exit (0);
}
	 
