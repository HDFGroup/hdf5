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
#include "h5diff.h"

/*-------------------------------------------------------------------------
 * Function: test_copy
 *
 * Purpose: 
 *
 * 1) make a copy with no filters 
 * 2) use the h5diff utility to compare the input and output file; 
 *     it returns RET==0 if the objects have the same data
 *
 * Return: Success: zero
 *  Failure: 1
 *
 * Programmer: Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             September, 19, 2003
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy(void)
{
 pack_opt_t  pack_options;
 diff_opt_t  diff_options;
 memset(&diff_options, 0, sizeof (diff_opt_t));

 TESTING("    copy with no filters");
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME1,FNAME1OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME1,FNAME1OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME1OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 PASSED(); 

 TESTING("    copy of attributes");
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME2,FNAME2OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME2,FNAME2OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME2OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 PASSED();      

 TESTING("    copy of hardlinks");
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME3,FNAME3OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME3,FNAME3OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME3OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 PASSED();          
 
 return 0; 
 
error:                                                       
 return 1;

}

/*-------------------------------------------------------------------------
 * Function: test_filter_deflate
 *
 * Purpose: 
 *
 * 1) compress/chunk FILENAME with the DEFLATE filter
 * 2) use the h5diff utility to compare the input and output file; 
 *     it returns RET==0 if the objects have the same data
 * 3) use API functions to verify the compression/chunking input on the output file
 *
 * Return: Success: zero
 *  Failure: 1
 *
 * Programmer: Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             September, 19, 2003
 *
 *
 *-------------------------------------------------------------------------
 */
static int
test_filter_deflate(void)
{
 pack_opt_t  pack_options;
 diff_opt_t  diff_options;
 memset(&diff_options, 0, sizeof (diff_opt_t));
 memset(&pack_options, 0, sizeof (pack_opt_t));

 TESTING("    deflate filter");

#ifdef H5_HAVE_FILTER_DEFLATE

/*-------------------------------------------------------------------------
 * test no object option (preserve old filters)
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;


/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset1:GZIP 9",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CHUNK 20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("GZIP 9",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CHUNK 20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
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
 return 0; 
 
#ifdef H5_HAVE_FILTER_DEFLATE
error:                                                       
 return 1;
#endif


}


/*-------------------------------------------------------------------------
 * Function: test_filter_szip
 *
 * Purpose: 
 *
 * 1) compress/chunk FILENAME with the SZIP filter
 * 2) use the h5diff utility to compare the input and output file; 
 *     it returns RET==0 if the objects have the same data
 * 3) use API functions to verify the compression/chunking input on the output file
 *
 * Return: Success: zero
 *  Failure: 1
 *
 * Programmer: Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             December, 19, 2003
 *
 *
 *-------------------------------------------------------------------------
 */
static int
test_filter_szip(void)
{
 pack_opt_t  pack_options;
 diff_opt_t  diff_options;
 memset(&diff_options, 0, sizeof (diff_opt_t));
 memset(&pack_options, 0, sizeof (pack_opt_t));

 TESTING("    szip filter");

#ifdef H5_HAVE_FILTER_SZIP

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset2:SZIP 8",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset2:CHUNK 20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */


#if 0
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("SZIP 8",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CHUNK 20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
#endif
 
 
 PASSED();  
#else
 SKIPPED();
#endif
 return 0; 
 
#ifdef H5_HAVE_FILTER_SZIP
error:                                                       
 return 1;
#endif

}


/*-------------------------------------------------------------------------
 * Function: test_filter_shuffle
 *
 * Purpose: 
 *
 * 1) compress/chunk FILENAME with the shuffle filter
 * 2) use the h5diff utility to compare the input and output file; 
 *     it returns RET==0 if the objects have the same data
 * 3) use API functions to verify the compression/chunking input on the output file
 *
 * Return: Success: zero
 *  Failure: 1
 *
 * Programmer: Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             September, 19, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_filter_shuffle(void)
{
 pack_opt_t  pack_options;
 diff_opt_t  diff_options;
 memset(&diff_options, 0, sizeof (diff_opt_t));
 memset(&pack_options, 0, sizeof (pack_opt_t));

 TESTING("    shuffle filter");

#ifdef H5_HAVE_FILTER_SHUFFLE

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset1:SHUF",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CHUNK 20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("SHUF",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CHUNK 20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
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
 return 0; 
 
#ifdef H5_HAVE_FILTER_SHUFFLE
error:                                                       
 return 1;
#endif


}


/*-------------------------------------------------------------------------
 * Function: test_filter_checksum 
 *
 * Purpose: 
 *
 * 1) compress/chunk FILENAME with the checksum filter
 * 2) use the h5diff utility to compare the input and output file; 
 *     it returns RET==0 if the objects have the same data
 * 3) use API functions to verify the compression/chunking input on the output file
 *
 * Return: Success: zero
 *  Failure: 1
 *
 * Programmer: Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             September, 19, 2003
 *
 *-------------------------------------------------------------------------
 */
static int
test_filter_checksum(void)
{
 pack_opt_t  pack_options;
 diff_opt_t  diff_options;
 memset(&diff_options, 0, sizeof (diff_opt_t));
 memset(&pack_options, 0, sizeof (pack_opt_t));

 TESTING("    checksum filter");

#ifdef H5_HAVE_FILTER_FLETCHER32

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset1:FLET",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CHUNK 20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("FLET",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CHUNK 20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
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
 return 0; 
 
#ifdef H5_HAVE_FILTER_FLETCHER32
error:                                                       
 return 1;
#endif


}


/*-------------------------------------------------------------------------
 * Function: test_layout_chunked 
 *
 * Purpose: 
 *
 * 1) test the CHUNK layout options
 * 2) use the h5diff utility to compare the input and output file; 
 *     it returns RET==0 if the objects have the same data
 * 3) use API functions to verify the layout input on the output file
 *
 * Return: Success: zero
 *  Failure: 1
 *
 * Programmer: Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             December 30, 2003
 *
 *-------------------------------------------------------------------------
 */
static int
test_layout_chunked(void)
{
 pack_opt_t  pack_options;
 diff_opt_t  diff_options;
 memset(&diff_options, 0, sizeof (diff_opt_t));
 memset(&pack_options, 0, sizeof (pack_opt_t));

 TESTING("    layout chunked");

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CHUNK 20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CHUNK 20x10",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

 PASSED();  
 return 0; 
 
error:                                                       
 return 1;


}


/*-------------------------------------------------------------------------
 * Function: test_layout_contiguous
 *
 * Purpose: 
 *
 * 1) test the CONTI layout options
 * 2) use the h5diff utility to compare the input and output file; 
 *     it returns RET==0 if the objects have the same data
 * 3) use API functions to verify the layout input on the output file
 *
 * Return: Success: zero
 *  Failure: 1
 *
 * Programmer: Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             December 30, 2003
 *
 *-------------------------------------------------------------------------
 */
static int
test_layout_contiguous(void)
{
 pack_opt_t  pack_options;
 diff_opt_t  diff_options;
 memset(&diff_options, 0, sizeof (diff_opt_t));
 memset(&pack_options, 0, sizeof (pack_opt_t));

 TESTING("    layout contiguous");

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CONTI",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CONTI",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 
 PASSED();  
 return 0; 
 
error:                                                       
 return 1;


}


/*-------------------------------------------------------------------------
 * Function: test_layout_compact
 *
 * Purpose: 
 *
 * 1) test the COMPA layout options
 * 2) use the h5diff utility to compare the input and output file; 
 *     it returns RET==0 if the objects have the same data
 * 3) use API functions to verify the layout input on the output file
 *
 * Return: Success: zero
 *  Failure: 1
 *
 * Programmer: Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             December 30, 2003
 *
 *-------------------------------------------------------------------------
 */
static int
test_layout_compact(void)
{
 pack_opt_t  pack_options;
 diff_opt_t  diff_options;
 memset(&diff_options, 0, sizeof (diff_opt_t));
 memset(&pack_options, 0, sizeof (pack_opt_t));

 TESTING("    layout compact");

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:COMPA",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

/*-------------------------------------------------------------------------
 * test all objects option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("COMPA",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FNAME4,FNAME4OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 
 PASSED();  
 return 0; 
 
error:                                                       
 return 1;


}




/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: Executes h5repack tests
 *
 * Return: Success: zero
 *  Failure: non-zero
 *
 * Programmer: Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             September, 19, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int main (void)
{
 int  nerrors=0;

 /* run tests  */
 puts("Testing h5repack:");

 /* make the test files */
 if (make_testfiles()<0)
  goto error;

#if 1

 /* test a copy with no filters */
 nerrors += test_copy();

 /* test a copy with the deflate filter */
 nerrors += test_filter_deflate();

 /* test a copy with the szip filter */
 nerrors += test_filter_szip();

 /* test a copy with the shuffle filter */
 nerrors += test_filter_shuffle();

 /* test a copy with the checksum filter */
 nerrors += test_filter_checksum();

 /* test a copy with layout CHUNK options */
 nerrors += test_layout_chunked();

#endif

#if 0

 /* test a copy with layout CONTI options */
 nerrors += test_layout_contiguous();

 /* test a copy with layout COMPA options */
 nerrors += test_layout_compact();

#endif


 /* check for errors */
 if (nerrors)
  goto error;
 puts("All h5repack tests passed.");
 
 return 0;
 
error:
 puts("***** H5REPACK TESTS FAILED *****");
 return 1;


 return 0;
}

