/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"
#include "h5test.h"
#include "h5repack.h"
#include "testh5repack.h"
#include "h5diff.h"

const char *H5REPACK_FILENAMES[] = {
   "test_bigout",
    NULL
};

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: Executes h5repack tests
 *
 * Return: Success: zero
 *  Failure: non-zero
 *
 * Programmer:  Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             January, 6, 2004
 *
 *-------------------------------------------------------------------------
 */

const char *progname = "h5repacktst";
int d_status = EXIT_SUCCESS;

int main (int argc, const char *argv[])
{
 pack_opt_t  pack_options;
 diff_opt_t  diff_options;
#if defined (H5_HAVE_FILTER_SZIP)
 int szip_can_encode = 0;
#endif
 hid_t       fapl;

 /* initialize */
 memset(&diff_options, 0, sizeof (diff_opt_t));
 memset(&pack_options, 0, sizeof (pack_opt_t));

 /* run tests  */
 puts("Testing h5repack:");

 /* setup */
 h5_reset();
 fapl = h5_fileaccess();


 /* make the test files */
 if (make_testfiles()<0)
  goto error;

/*-------------------------------------------------------------------------
 * Format of the tests:
 *
 * 1) make a copy of the file with h5repack
 * 2) use the h5diff utility to compare the input and output file;
 *     it returns RET==0 if the objects have the same data
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * file with fill values
 *-------------------------------------------------------------------------
 */
 TESTING("    copy of datasets (fill values)");
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME0,FNAME0OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME0,FNAME0OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME0OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_cmpdcpl(FNAME0,FNAME0OUT)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 PASSED();

 /*return 0;*/

/*-------------------------------------------------------------------------
 * file with all kinds of dataset datatypes
 *-------------------------------------------------------------------------
 */
 TESTING("    copy of datasets (all datatypes)");
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME1,FNAME1OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME1,FNAME1OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME1OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_cmpdcpl(FNAME1,FNAME1OUT)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 PASSED();

/*-------------------------------------------------------------------------
 * file with attributes
 *-------------------------------------------------------------------------
 */
 TESTING("    copy of datasets (attributes)");
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME2,FNAME2OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME2,FNAME2OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME2OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_cmpdcpl(FNAME2,FNAME2OUT)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 PASSED();


/*-------------------------------------------------------------------------
 * file with hardlinks
 *-------------------------------------------------------------------------
 */
 TESTING("    copy of datasets (hardlinks)");
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME3,FNAME3OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME3,FNAME3OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME3OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_cmpdcpl(FNAME3,FNAME3OUT)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();

/*-------------------------------------------------------------------------
 * alloc early test
 *-------------------------------------------------------------------------
 */
 TESTING("    copy of allocation early file");
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME5,FNAME5OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME5,FNAME5OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME5OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
  PASSED();

/*-------------------------------------------------------------------------
 * the remaining files differ in the dcpl's
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * deflate
 *-------------------------------------------------------------------------
 */
 TESTING("    adding deflate filter");

#ifdef H5_HAVE_FILTER_DEFLATE

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset1:GZIP=9",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 PASSED();
#else
 SKIPPED();
#endif

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */

 TESTING("    adding deflate filter to all");

#ifdef H5_HAVE_FILTER_DEFLATE

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("GZIP=1",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif

/*-------------------------------------------------------------------------
 * SZIP
 *-------------------------------------------------------------------------
 */

 TESTING("    adding szip filter");

#if defined (H5_HAVE_FILTER_SZIP)
if (h5tools_can_encode(H5Z_FILTER_SZIP) == 1) {
   szip_can_encode = 1;
}

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

if (szip_can_encode) {
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset2:SZIP=8,EC",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset2:CHUNK=20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
} else {
 SKIPPED();
}
#else
 SKIPPED();
#endif


/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */
TESTING("    adding szip filter to all");

#if defined (H5_HAVE_FILTER_SZIP)
if (szip_can_encode) {
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("SZIP=8,NN",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
} else {
 SKIPPED();
}
#else
 SKIPPED();
#endif


 TESTING("    addding shuffle filter");

#ifdef H5_HAVE_FILTER_SHUFFLE

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset1:SHUF",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */

TESTING("    addding shuffle filter to all");

#ifdef H5_HAVE_FILTER_SHUFFLE

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("SHUF",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif



 TESTING("    adding checksum filter");

#ifdef H5_HAVE_FILTER_FLETCHER32

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset1:FLET",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */


 TESTING("    adding checksum filter to all");

#ifdef H5_HAVE_FILTER_FLETCHER32

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("FLET",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif


 TESTING("    filter queue fletcher, shuffle, deflate, szip");

/*-------------------------------------------------------------------------
 * add some filters
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CHUNK 20x10",&pack_options)<0)
  TEST_ERROR;

#if defined (H5_HAVE_FILTER_FLETCHER32)
 if (h5repack_addfilter("dset1:FLET",&pack_options)<0)
  TEST_ERROR;
#endif

#ifdef H5_HAVE_FILTER_SHUFFLE
 if (h5repack_addfilter("dset1:SHUF",&pack_options)<0)
  TEST_ERROR;
#endif

#if defined (H5_HAVE_FILTER_SZIP)
if (szip_can_encode) {
 if (h5repack_addfilter("dset1:SZIP=8,NN",&pack_options)<0)
  TEST_ERROR;
}
#endif

#ifdef H5_HAVE_FILTER_DEFLATE
 if (h5repack_addfilter("dset1:GZIP=1",&pack_options)<0)
  TEST_ERROR;
#endif

 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();


 TESTING("    adding layout chunked");

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 PASSED();

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */
 TESTING("    adding layout chunked to all");

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();

 TESTING("    adding layout contiguous");

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CONTI",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();

 TESTING("    adding layout contiguous to all");

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CONTI",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();

 TESTING("    adding layout compact");

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:COMPA",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 PASSED();

 TESTING("    adding layout compact to all");

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("COMPA",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();


 TESTING("    layout compact to contiguous conversion");

/*-------------------------------------------------------------------------
 * layout compact to contiguous conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_compact:CONTI",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
  PASSED();

 TESTING("    layout compact to chunk conversion");

/*-------------------------------------------------------------------------
 * layout compact to chunk conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_compact:CHUNK=2x5",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
  PASSED();

  TESTING("    layout compact to compact conversion");

/*-------------------------------------------------------------------------
 * layout compact to compact conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_compact:COMPA",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
  PASSED();

 TESTING("    layout contiguous to compact conversion");
/*-------------------------------------------------------------------------
 * layout contiguous to compact conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_contiguous:COMPA",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
  PASSED();

 TESTING("    layout contiguous to chunk conversion");
/*-------------------------------------------------------------------------
 * layout contiguous to chunk conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_contiguous:CHUNK=3x6",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
  PASSED();

  TESTING("    layout contiguous to contiguous conversion");

/*-------------------------------------------------------------------------
 * layout contiguous to contiguous conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_contiguous:CONTI",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
  PASSED();

 TESTING("    layout chunked to compact conversion");
/*-------------------------------------------------------------------------
 * layout chunked to compact conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_chunk:COMPA",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
  PASSED();

 TESTING("    layout chunked to contiguous conversion");

/*-------------------------------------------------------------------------
 * layout chunked to contiguous conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_chunk:CONTI",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
  PASSED();

 TESTING("    layout chunked to chunk conversion");
/*-------------------------------------------------------------------------
 * layout chunked to chunked conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_chunk:CHUNK=18x13",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
  PASSED();


/*-------------------------------------------------------------------------
 * the following tests assume the input files have filters
 * FNAME7     "test_szip.h5"
 * FNAME8     "test_deflate.h5"
 * FNAME9     "test_shuffle.h5"
 * FNAME10    "test_fletcher32.h5"
 * FNAME11    "test_all.h5"
 *-------------------------------------------------------------------------
 */


 TESTING("    copy of szip filter");

#if defined (H5_HAVE_FILTER_SZIP)
if (szip_can_encode) {
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME7,FNAME7OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME7,FNAME7OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME7OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
} else {
 SKIPPED();
}
#else
 SKIPPED();
#endif

  TESTING("    removing szip filter");

#if defined (H5_HAVE_FILTER_SZIP)
if (szip_can_encode) {
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset_szip:NONE",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME7,FNAME7OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME7,FNAME7OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME7OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
} else {
 SKIPPED();
}
#else
 SKIPPED();
#endif


  TESTING("    copy of deflate filter");

#ifdef H5_HAVE_FILTER_DEFLATE
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME8,FNAME8OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME8,FNAME8OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME8OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif


 TESTING("    removing deflate filter");

#ifdef H5_HAVE_FILTER_DEFLATE
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset_deflate:NONE",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME8,FNAME8OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME8,FNAME8OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME8OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif



 TESTING("    copy of shuffle filter");

#ifdef H5_HAVE_FILTER_SHUFFLE
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME9,FNAME9OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME9,FNAME9OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME9OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif

 TESTING("    removing shuffle filter");

#ifdef H5_HAVE_FILTER_SHUFFLE
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset_shuffle:NONE",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME9,FNAME9OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME9,FNAME9OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME9OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif

 TESTING("    copy of fletcher filter");

#ifdef H5_HAVE_FILTER_FLETCHER32
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME10,FNAME10OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME10,FNAME10OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME10OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif

  TESTING("    removing fletcher filter");

#ifdef H5_HAVE_FILTER_FLETCHER32
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset_fletcher32:NONE",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME10,FNAME10OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME10,FNAME10OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME10OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif


 TESTING("    copy of nbit filter");

#ifdef H5_HAVE_FILTER_NBIT
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME12,FNAME12OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME12,FNAME12OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME12OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif

  TESTING("    removing nbit filter");

#ifdef H5_HAVE_FILTER_NBIT
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset_nbit:NONE",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME12,FNAME12OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME12,FNAME12OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME12OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif

  TESTING("    adding nbit filter");

#ifdef H5_HAVE_FILTER_NBIT
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset_int31:NBIT",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME12,FNAME12OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME12,FNAME12OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME12OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif
 TESTING("    copy of scaleoffset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME13,FNAME13OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME13,FNAME13OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME13OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif

  TESTING("    removing scaleoffset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset_scaleoffset:NONE",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME13,FNAME13OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME13,FNAME13OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME13OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif


  TESTING("    adding scaleoffset filter");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset_none:SOFF=31,IN",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME13,FNAME13OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME13,FNAME13OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME13OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif

/*-------------------------------------------------------------------------
 * file with all filters
 *  dset_all
 *  dset_deflate
 *  dset_szip
 *  dset_shuffle
 *  dset_fletcher32
 *-------------------------------------------------------------------------
 */


 TESTING("    filter conversion from deflate to szip");

#if defined (H5_HAVE_FILTER_SZIP) \
    && defined (H5_HAVE_FILTER_DEFLATE) \
    && defined (H5_HAVE_FILTER_FLETCHER32) && defined (H5_HAVE_FILTER_SHUFFLE)

if (szip_can_encode) {
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset_deflate:SZIP=8,NN",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME11,FNAME11OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME11,FNAME11OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME11OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
} else {
 SKIPPED();
}
#else
 SKIPPED();
#endif

 TESTING("    filter conversion from szip to deflate");

#if defined (H5_HAVE_FILTER_SZIP) \
    && defined (H5_HAVE_FILTER_DEFLATE) \
    && defined (H5_HAVE_FILTER_FLETCHER32) && defined (H5_HAVE_FILTER_SHUFFLE)

if (szip_can_encode) {
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset_szip:GZIP=1",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME11,FNAME11OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME11,FNAME11OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME11OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
} else {
 SKIPPED();
}
#else
 SKIPPED();
#endif


/*-------------------------------------------------------------------------
 * test the NONE global option
 *-------------------------------------------------------------------------
 */

  TESTING("    removing all filters");

#if defined (H5_HAVE_FILTER_SZIP) && defined (H5_HAVE_FILTER_DEFLATE) \
    && defined (H5_HAVE_FILTER_FLETCHER32) && defined (H5_HAVE_FILTER_SHUFFLE)

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("NONE",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME11,FNAME11OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME11,FNAME11OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME11OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();
#else
 SKIPPED();
#endif

/*-------------------------------------------------------------------------
 * test a big file
 *-------------------------------------------------------------------------
 */
 TESTING("    big file");
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME14,FNAME14OUT,&pack_options) < 0)
  TEST_ERROR;
 if (h5diff(FNAME14,FNAME14OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME14OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 PASSED();


/*-------------------------------------------------------------------------
 * end
 *-------------------------------------------------------------------------
 */

 puts("All h5repack tests passed.");

 h5_cleanup(H5REPACK_FILENAMES, fapl); 

 return 0;

error:
 puts("***** H5REPACK TESTS FAILED *****");
 return 1;

}



