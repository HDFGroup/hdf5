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
#include <H5Cprivate.h>
#include <H5Fprivate.h>
#include <H5Gprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>
#include <H5Vprivate.h>

#if 0
#  define FILETYPE	H5F_LOW_DFLT
#  define FILENAME	"istore.h5"
#elif 1
#  define FILETYPE	H5F_LOW_FAM
#  define FILENAME	"istore-%05d.h5"
#  define TEST_FAMILY	1
#else
#  define FILETYPE	H5F_LOW_SPLIT
#  define FILENAME	"istore-split"
#endif

#define TEST_SMALL	0x0001
#define TEST_MEDIUM	0x0002
#define TEST_LARGE	0x0004

#ifndef HAVE_FUNCTION
#define __FUNCTION__ ""
#endif
#define AT() printf ("   at %s:%d in %s()...\n",			    \
		     __FILE__, __LINE__, __FUNCTION__);

size_t align_g[3] = {50, 50, 50};


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
   size_t	alignment[H5O_ISTORE_NDIMS];
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
   for (i=0; i<ndims; i++) {
      if (i<NELMTS (align_g)) {
	 alignment[i] = align_g[i];
      } else {
	 alignment[i] = 2;
      }
   }
   
   H5F_istore_create (f, &istore, ndims, alignment);
   if (H5O_modify (f, NO_ADDR, handle, H5O_ISTORE, H5O_NEW_MESG,
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
 *		as to always extend the object's domain without creating
 *		holes and without causing the object to become concave.
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
   size_t	nelmts;
   H5O_istore_t	istore;

   if (!nz) {
      if (!ny) {
	 ndims = 1;
	 ny = nz = 1;
	 sprintf (dims, "%lu", (unsigned long)nx);
      } else {
	 ndims = 2;
	 nz = 1;
	 sprintf (dims, "%lux%lu", (unsigned long)nx, (unsigned long)ny);
      }
   } else {
      ndims = 3;
      sprintf (dims, "%lux%lux%lu",
	       (unsigned long)nx, (unsigned long)ny, (unsigned long)nz);
   }


   sprintf (s, "Testing istore extend: %s", dims);
   printf ("%-70s", s);
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
   if (NULL==H5O_read (f, NO_ADDR, handle, H5O_ISTORE, 0, &istore)) {
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
	 nelmts = 1;
      } else {
	 for (i=0, nelmts=1; i<ndims; i++) {
	    if (ctr % ndims == i) {
	       offset[i] = max_corner[i];
	       size[i] = MIN (1, whole_size[i]-offset[i]);
	    } else {
	       offset[i] = 0;
	       size[i] = max_corner[i];
	    }
	    nelmts *= size[i];
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
      printf ("), %d element%s", nelmts, 1==nelmts?"":"s");
      if (0==nelmts) printf (" *SKIPPED*");
      printf ("\n");
#endif
      
      /* Fill the source array */
      if (0==nelmts) continue;
      memset (buf, 128+ctr, nelmts);

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
      memset (check, 0xff, nelmts);
      if (H5F_istore_read (f, &istore, offset, size, check)<0) {
	 puts ("*FAILED*");
	 if (!isatty (1)) {
	    AT ();
	    printf ("   Read failed: ctr=%d\n", ctr);
	 }
	 goto error;
      }
      if (memcmp (buf, check, nelmts)) {
	 puts ("*FAILED*");
	 if (!isatty (1)) {
	    AT ();
	    printf ("   Read check failed: ctr=%d\n", ctr);
	    printf ("   Wrote:\n");
	    print_array (buf, size[0], size[1], size[2]);
	    printf ("   Read:\n");
	    print_array (check, size[0], size[1], size[2]);
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
		  if (ndims>2) printf (", k=%d", k);
		  printf ("\n   Check array is:\n");
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
 * Function:	test_sparse
 *
 * Purpose:	Creates a sparse matrix consisting of NBLOCKS randomly placed
 *		blocks each of size NX,NY,NZ.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_sparse (H5F_t *f, const char *prefix, size_t nblocks,
	     size_t nx, size_t ny, size_t nz)
{
   intn		ndims, ctr;
   char		dims[64], s[256], name[256];
   size_t	offset[3], size[3], total=0;
   H5G_entry_t	*handle = NULL;
   H5O_istore_t	istore;
   uint8	*buf = NULL;
   
   if (!nz) {
      if (!ny) {
	 ndims = 1;
	 ny = nz = 1;
	 sprintf (dims, "%lu", (unsigned long)nx);
      } else {
	 ndims = 2;
	 nz = 1;
	 sprintf (dims, "%lux%lu", (unsigned long)nx, (unsigned long)ny);
      }
   } else {
      ndims = 3;
      sprintf (dims, "%lux%lux%lu",
	       (unsigned long)nx, (unsigned long)ny, (unsigned long)nz);
   }

   sprintf (s, "Testing istore sparse: %s", dims);
   printf ("%-70s", s);
   buf = H5MM_xmalloc (nx*ny*nz);

   /* Build the new empty object */
   sprintf (name, "%s_%s", prefix, dims);
   if (NULL==(handle=new_object (f, name, ndims))) {
      if (!isatty (1)) {
	 AT ();
	 printf ("   Cannot create %d-d object `%s'\n", ndims, name);
      }
      goto error;
   }
   if (NULL==H5O_read (f, NO_ADDR, handle, H5O_ISTORE, 0, &istore)) {
      puts ("*FAILED*");
      if (!isatty (1)) {
	 AT ();
	 printf ("   Unable to read istore message\n");
      }
      goto error;
   }

   for (ctr=0; ctr<nblocks; ctr++) {
      offset[0] = rand () % 1000000;
      offset[1] = rand () % 1000000;
      offset[2] = rand () % 1000000;
      size[0] = nx;
      size[1] = ny;
      size[2] = nz;
      memset (buf, 128+ctr, nx*ny*nz);

      /* write to disk */
      if (H5F_istore_write (f, &istore, offset, size, buf)<0) {
	 puts ("*FAILED*");
	 if (!isatty (1)) {
	    AT ();
	    printf ("   Write failed: ctr=%d\n", ctr);
	    printf ("   offset=(%lu", (unsigned long)(offset[0]));
	    if (ndims>1) printf (",%lu", (unsigned long)(offset[1]));
	    if (ndims>2) printf (",%lu", (unsigned long)(offset[2]));
	    printf ("), size=(%lu", (unsigned long)(size[0]));
	    if (ndims>1) printf (",%lu", (unsigned long)(size[1]));
	    if (ndims>2) printf (",%lu", (unsigned long)(size[2]));
	    printf (")\n");
	 }
	 goto error;
      }
      total += nx*ny*nz;
#if 0
      printf ("ctr: ctr=%d, total=%lu\n", ctr, (unsigned long)total);
#endif

      /* We don't test reading yet.... */
   }
   

   H5G_close (f, handle);
   puts (" PASSED");
   H5MM_xfree (buf);
   return SUCCEED;
   
 error:
   H5MM_xfree (buf);
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
main (int argc, char *argv[])
{
   H5F_t	*f;
   herr_t	status;
   int		nerrors = 0;
   uintn	size_of_test;
   size_t	offset_size;
   H5G_entry_t	*ent = NULL;
   hid_t	template_id;
   H5F_create_t	*creation_template = NULL;

   setbuf (stdout, NULL);

   /* Parse arguments or assume `small' */
   if (1==argc) {
      size_of_test = TEST_SMALL;
   } else {
      intn i;
      for (i=1,size_of_test=0; i<argc; i++) {
	 if (!strcmp (argv[i], "small")) {
	    size_of_test |= TEST_SMALL;
	 } else if (!strcmp (argv[i], "medium")) {
	    size_of_test |= TEST_MEDIUM;
	 } else if (!strcmp (argv[i], "large")) {
	    size_of_test |= TEST_LARGE;
	 } else {
	    printf ("unrecognized argument: %s\n", argv[i]);
	    exit (1);
	 }
      }
   }
   printf ("Test sizes: ");
   if (size_of_test & TEST_SMALL) printf (" SMALL");
   if (size_of_test & TEST_MEDIUM) printf (" MEDIUM");
   if (size_of_test & TEST_LARGE) printf (" LARGE");
   printf ("\n");

   /*
    * Use larger file addresses...
    */
   offset_size = 8;
   template_id = H5Ccreate (H5C_FILE_CREATE);
   H5Cset_prop (template_id, H5F_SIZEOF_ADDR, offset_size);
   creation_template = H5Aatom_object (template_id);
   
   /* Create the test file */
   if (NULL==(f=H5F_open (FILETYPE, FILENAME,
			  (H5F_ACC_CREAT|H5F_ACC_WRITE|H5F_ACC_TRUNC|
			   H5F_ACC_DEBUG),
			  creation_template))) {
      printf ("Cannot create file %s; test aborted\n", FILENAME);
      exit (1);
   }

#ifdef TEST_FAMILY
   {
      /*
       * For testing file families, fool the library into thinking it already
       * allocated a whole bunch of data.
       */
      haddr_t	addr;
      addr.offset = 8 * ((uint64)1<<30); /*8 GB*/
      H5F_low_seteof (f->shared->lf, &addr);
   }
#endif

   /*
    * By creating a group we cause the library to emit it's debugging
    * diagnostic messages before we begin testing...
    */
   ent = H5G_new (f, "flushing_diagnostics", 0);
   H5G_close (f, ent);
   ent = NULL;


   /*
    * Creation test: Creates empty objects with various raw data sizes
    * and alignments.
    */
   status = test_create (f, "create");
   nerrors += status<0 ? 1 : 0;

   if (size_of_test & TEST_SMALL) {
      status = test_extend (f, "extend", 10, 0, 0);
      nerrors += status<0 ? 1 : 0;
      status = test_extend (f, "extend", 10, 10, 0);
      nerrors += status<0 ? 1 : 0;
      status = test_extend (f, "extend", 10, 10, 10);
      nerrors += status<0 ? 1 : 0;
   }
   if (size_of_test & TEST_MEDIUM) {
      status = test_extend (f, "extend", 10000, 0, 0);
      nerrors += status<0 ? 1 : 0;
      status = test_extend (f, "extend", 2500, 10, 0);
      nerrors += status<0 ? 1 : 0;
      status = test_extend (f, "extend", 10, 400, 10);
      nerrors += status<0 ? 1 : 0;
   }
   if (size_of_test & TEST_SMALL) {
      status = test_sparse (f, "sparse", 100, 5, 0, 0);
      nerrors += status<0 ? 1 : 0;
      status = test_sparse (f, "sparse", 100, 3, 4, 0);
      nerrors += status<0 ? 1 : 0;
      status = test_sparse (f, "sparse", 100, 2, 3, 4);
      nerrors += status<0 ? 1 : 0;
   }
   if (size_of_test & TEST_MEDIUM) {
      status = test_sparse (f, "sparse", 1000, 30, 0, 0);
      nerrors += status<0 ? 1 : 0;
      status = test_sparse (f, "sparse", 2000, 7, 3, 0);
      nerrors += status<0 ? 1 : 0;
      status = test_sparse (f, "sparse", 2000, 4, 2, 3);
      nerrors += status<0 ? 1 : 0;
   }
   if (size_of_test & TEST_LARGE) {
      status = test_sparse (f, "sparse", 800, 50, 50, 50);
      nerrors += status<0 ? 1 : 0;
   }
   
   
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
