/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer: 	Robb Matzke <matzke@llnl.gov>
 *             	Wednesday, October 15, 1997
 *
 * Purpose:	Tests various aspects of indexed raw data storage.
 */
#include <H5private.h>
#include <H5Fprivate.h>
#include <H5Gprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>
#include <H5Vprivate.h>

#define FILENAME	"istore.h5"

#define AT() printf ("   at %s:%d in %s()...\n",			    \
		     __FILE__, __LINE__, __FUNCTION__);
   


/*-------------------------------------------------------------------------
 * Function:	print_array
 *
 * Purpose:	Prints the values in an array
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
print_array (uint8 *array, size_t nx, size_t ny, size_t nz)
{
   int		i, j, k;

   for (i=0; i<nx; i++) {
      if (nz>1) {
	 printf ("i=%d:\n", i);
      } else {
	 printf ("%03d:", i);
      }
      
      for (j=0; j<ny; j++) {
	 if (nz>1) printf ("%03d:", j);
	 for (k=0; k<nz; k++) {
	    printf (" %3d", *array++);
	 }
	 if (nz>1) printf ("\n");
      }
      printf ("\n");
   }
}



/*-------------------------------------------------------------------------
 * Function:	new_object
 *
 * Purpose:	Creates a new object that refers to a indexed storage of raw
 *		data.  No raw data is stored.
 *
 * Return:	Success:	Handle to a new open object.
 *
 *		Failure:	NULL, error message printed.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5G_entry_t *
new_object (H5F_t *f, const char *name, size_t ndims)
{
   H5G_entry_t	*handle = NULL;
   H5O_istore_t	istore;
   intn		i;
   
   /* Create the object symbol table entry and header */
   if (NULL==(handle=H5G_create (f, name, 64))) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   H5G_create (f, name=\"%s\") = NULL\n", name);
      }
      return NULL;
   }

   /* Add the indexed-storage message */
   memset (&istore, 0, sizeof istore);
   istore.ndims = ndims;
   for (i=0; i<ndims; i++) istore.alignment[i] = 2;

   if (H5O_modify (f, H5O_NO_ADDR, handle, H5O_ISTORE, H5O_NEW_MESG,
		   &istore)<0) {
      printf ("*FAILED*\n");
      if (!isatty (1)) {
	 AT();
	 printf ("   H5G_modify istore message failure\n");
      }
      return NULL;
   }

   return handle;
}


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Creates a named object that refers to indexed storage of raw
 *		data.  No raw data is stored.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_create (H5F_t *f, const char *prefix)
{
   H5G_entry_t	*handle = NULL;
   intn		i;
   char		name[256];
   
   printf ("%-70s", "Testing istore create");
   fflush (stdout);

   for (i=1; i<=H5O_ISTORE_NDIMS; i++) {
      sprintf (name, "%s_%02d", prefix, i);
      if (NULL==(handle=new_object (f, name, i))) return FAIL;
      H5G_close (f, handle);
   }

   puts (" PASSED");
   return SUCCEED;
}



/*-------------------------------------------------------------------------
 * Function:	test_extend
 *
 * Purpose:	Creates an empty object and then writes to it in such a way
 *		as to always extend the object's domain.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_extend (H5F_t *f, const char *prefix,
	     size_t nx, size_t ny, size_t nz)
{
   H5G_entry_t	*handle = NULL;
   int		i, j, k, ndims, ctr;
   uint8	*buf=NULL, *check=NULL, *whole=NULL;
   char		dims[64], s[256], name[256];
   size_t	offset[3];
   size_t	max_corner[3];
   size_t	size[3];
   size_t	whole_size[3];
   H5O_istore_t	istore;

   if (!nz) {
      if (!ny) {
	 ndims = 1;
	 ny = nz = 1;
	 sprintf (dims, "%d", nx);
      } else {
	 ndims = 2;
	 nz = 1;
	 sprintf (dims, "%dx%d", nx, ny);
      }
   } else {
      ndims = 3;
      sprintf (dims, "%dx%dx%d", nx, ny, nz);
   }


   sprintf (s, "Testing istore extend: %s", dims);
   printf ("%-70s", s);
   fflush (stdout);
   buf = H5MM_xmalloc (nx*ny*nz);
   check = H5MM_xmalloc (nx*ny*nz);
   whole = H5MM_xcalloc (nx*ny*nz, 1);

   /* Build the new empty object */
   sprintf (name, "%s_%s", prefix, dims);
   if (NULL==(handle=new_object (f, name, ndims))) {
      if (!isatty (1)) {
	 AT ();
	 printf ("   Cannot create %d-d object `%s'\n", ndims, name);
      }
      goto error;
   }
   if (NULL==H5O_read (f, H5O_NO_ADDR, handle, H5O_ISTORE, 0, &istore)) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Unable to read istore message\n");
      }
      goto error;
   }
   if (ndims!=istore.ndims) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Header read error: istore.ndims != %d\n", ndims);
      }
      goto error;
   }

   whole_size[0] = nx;
   whole_size[1] = ny;
   whole_size[2] = nz;
   max_corner[0] = 0;
   max_corner[1] = 0;
   max_corner[2] = 0;

   for (ctr=0; H5V_vector_lt (ndims, max_corner, whole_size); ctr++) {

      /* Size and location */
      if (0==ctr) {
	 offset[0] = offset[1] = offset[2] = 0;
	 size[0] = size[1] = size[2] = 1;
      } else {
	 for (i=0; i<ndims; i++) {
	    if (ctr % ndims == i) {
	       offset[i] = max_corner[i];
	       size[i] = 1;
	       if (offset[i]+size[i]>whole_size[i]) continue;
	    } else {
	       offset[i] = 0;
	       size[i] = max_corner[i];
	    }
	 }
      }

#if 0
      if (0==ctr) printf ("\n");
      printf ("   Insert: ctr=%d, corner=(%d", ctr, offset[0]);
      if (ndims>1) printf (",%d", offset[1]);
      if (ndims>2) printf (",%d", offset[2]);
      printf ("), size=(%d", size[0]);
      if (ndims>1) printf (",%d", size[1]);
      if (ndims>2) printf (",%d", size[2]);
      printf (")\n");
#endif
      
      /* Fill the source array */
      memset (buf, 128+ctr, size[0]*size[1]*size[2]);

      /* Write to disk */
      if (H5F_istore_write (f, &istore, offset, size, buf)<0) {
	 puts ("*FAILED*");
	 if (!isatty (1)) {
	    AT ();
	    printf ("   Write failed: ctr=%d\n", ctr);
	 }
	 goto error;
      }

      /* Read from disk */
      memset (check, 0xff, size[0]*size[1]*size[2]);
      if (H5F_istore_read (f, &istore, offset, size, check)<0) {
	 puts ("*FAILED*");
	 if (!isatty (1)) {
	    AT ();
	    printf ("   Read failed: ctr=%d\n", ctr);
	 }
	 goto error;
      }
      if (memcmp (buf, check, size[0]*size[1]*size[2])) {
	 puts ("*FAILED*");
	 if (!isatty (1)) {
	    AT ();
	    printf ("   Read check failed: ctr=%d\n", ctr);
	    printf ("   Wrote:\n");
	    print_array (buf, size[0], size[1], size[2]);
	    printf ("   Read:\n");
	    print_array (buf, size[0], size[1], size[2]);
	 }
	 goto error;
      }
      
      /* Write to `whole' buffer for later checking */
      H5V_hyper_copy (ndims, size,
		      whole_size, offset, whole,	/*dst*/
		      size, H5V_ZERO, buf);		/*src*/

      /* Update max corner */
      for (i=0; i<ndims; i++) {
	 max_corner[i] = MAX (max_corner[i], offset[i]+size[i]);
      }
   }

   /* Update the object header */
   H5O_modify (f, H5O_NO_ADDR, handle, H5O_ISTORE, 0, &istore);


   /* Now read the entire array back out and check it */
   memset (buf, 0xff, nx*ny*nz);
   if (H5F_istore_read (f, &istore, H5V_ZERO, whole_size, buf)<0) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Read failed for whole array\n");
      }
      goto error;
   }
   for (i=0; i<nx; i++) {
      for (j=0; j<ny; j++) {
	 for (k=0; k<nz; k++) {
	    if (whole[i*ny*nz + j*nz + k] != buf[i*ny*nz + j*nz + k]) {
	       puts ("*FAILED*");
	       if (!isatty (1)) {
		  AT ();
		  printf ("   Check failed at i=%d", i);
		  if (ndims>1) printf (", j=%d", j);
		  if (ndims>2) printf (", k=%d\n", k);
		  printf ("   Check array is:\n");
		  print_array (whole, nx, ny, nz);
		  printf ("   Value read is:\n");
		  print_array (buf, nx, ny, nz);
	       }
	       goto error;
	    }
	 }
      }
   }
   
   H5G_close (f, handle);
   puts (" PASSED");
   H5MM_xfree (buf);
   H5MM_xfree (check);
   H5MM_xfree (whole);
   return SUCCEED;

 error:
   H5MM_xfree (buf);
   H5MM_xfree (check);
   H5MM_xfree (whole);
   return FAIL;
}
      

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests indexed storage stuff.
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(non-zero)
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
   H5F_t	*f;
   herr_t	status;
   int		nerrors = 0;
   
   /* Create the test file */
   if (NULL==(f=H5F_open (FILENAME, H5F_ACC_CREAT|H5F_ACC_WRITE|H5F_ACC_TRUNC,
			  NULL))) {
      printf ("Cannot create file %s; test aborted\n", FILENAME);
      exit (1);
   }

   /*----------------------
    * INDEXED STORAGE TESTS
    *---------------------- 
    */
   status = test_create (f, "test_create_1");
   nerrors += status<0 ? 1 : 0;

   status = test_extend (f, "test_extend_1", 10, 0, 0);
   nerrors += status<0 ? 1 : 0;
   status = test_extend (f, "test_extend_1", 10, 10, 0);
   nerrors += status<0 ? 1 : 0;
   status = test_extend (f, "test_extend_1", 10, 10, 10);
   nerrors += status<0 ? 1 : 0;
   
   
   


   /* Close the test file and exit */
   H5F_close (f);
   if (nerrors) {
      printf ("***** %d I-STORE TEST%s FAILED! *****\n",
	      nerrors, 1==nerrors?"":"S");
      if (isatty (1)) {
	 printf ("(Redirect output to a pager or a file to see "
		 "debug output)\n");
      }
      exit (1);
   }

   printf ("All i-store tests passed.\n");
   exit (0);
}
