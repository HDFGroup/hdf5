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

#if 0
#define PACK_DEBUG
#endif

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: Executes h5repack tests
 *
 * Return: Success: zero
 *  Failure: non-zero
 *
 * Programmer: Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             January, 6, 2004
 *
 *-------------------------------------------------------------------------
 */


int main (void)
{
 pack_opt_t  pack_options;
 diff_opt_t  diff_options;
 memset(&diff_options, 0, sizeof (diff_opt_t));
 memset(&pack_options, 0, sizeof (pack_opt_t));

 /* run tests  */
 puts("Testing h5repack:");

 /* make the test files */
 if (make_testfiles()<0)
  goto error;

/*-------------------------------------------------------------------------
 * Purpose: 
 *
 * 1) make a copy with no filters 
 * 2) use the h5diff utility to compare the input and output file; 
 *     it returns RET==0 if the objects have the same data
 *-------------------------------------------------------------------------
 */
 
 TESTING("    copy of datasets");

/*-------------------------------------------------------------------------
 * file with all kinds of dataset datatypes
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME1,FNAME1OUT,&pack_options)<0)
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
 
 TESTING("    copy of attributes");
/*-------------------------------------------------------------------------
 * file with attributes
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME2,FNAME2OUT,&pack_options)<0)
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
 TESTING("    copy of hardlinks");

/*-------------------------------------------------------------------------
 * file with hardlinks
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack(FNAME3,FNAME3OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FNAME3,FNAME3OUT,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 if (h5repack_verify(FNAME3OUT,&pack_options)<=0)
  TEST_ERROR;
 if (h5repack_cmpdcpl(FNAME3,FNAME3OUT)<=0)
  TEST_ERROR;
  if (h5repack_end (&pack_options)<0)
  TEST_ERROR;

/*-------------------------------------------------------------------------
 * file with filters. we cannot compare with cmpdcpl, because the current
 * configuration might not have saved datasets with deflate and SZIP filters
 *-------------------------------------------------------------------------
 */
 PASSED();
 TESTING("    copy of datasets with filters");
 
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

 PASSED();

 TESTING("    removing all filters");

/*-------------------------------------------------------------------------
 * test the NONE global option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("NONE",&pack_options)<0)
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

 TESTING("    removing one filter");

/*-------------------------------------------------------------------------
 * test the NONE specific option; uncompress a dataset
 *-------------------------------------------------------------------------
 */

#ifdef H5_HAVE_FILTER_DEFLATE
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset_gzip:NONE",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset1:NONE",&pack_options)<0)
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
 if (h5repack_addfilter("dset1:GZIP=9",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
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
 if (h5repack_addfilter("GZIP=9",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
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



 TESTING("    szip filter");

#ifdef H5_HAVE_FILTER_SZIP

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset2:SZIP=8",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset2:CHUNK=20x10",&pack_options)<0)
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
 if (h5repack_addfilter("SZIP=8",&pack_options)<0)
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
 if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
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
 if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
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
 if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
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
 if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
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

 TESTING("    layout chunked");

/*-------------------------------------------------------------------------
 * test an individual object option
 *-------------------------------------------------------------------------
 */

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
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
 if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
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


 TESTING("    layout compact to contiguous conversion");

/*-------------------------------------------------------------------------
 * layout compact to contiguous conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_compact:CONTI",&pack_options)<0)
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
  
 TESTING("    layout compact to chunk conversion");

/*-------------------------------------------------------------------------
 * layout compact to chunk conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_compact:CHUNK=2x5",&pack_options)<0)
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

  TESTING("    layout compact to compact conversion");
  
/*-------------------------------------------------------------------------
 * layout compact to compact conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_compact:COMPA",&pack_options)<0)
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
  
 TESTING("    layout contiguous to compact conversion");
/*-------------------------------------------------------------------------
 * layout contiguous to compact conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_contiguous:COMPA",&pack_options)<0)
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
  
 TESTING("    layout contiguous to chunk conversion");
/*-------------------------------------------------------------------------
 * layout contiguous to chunk conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_contiguous:CHUNK=3x6",&pack_options)<0)
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

  TESTING("    layout contiguous to contiguous conversion");

/*-------------------------------------------------------------------------
 * layout contiguous to contiguous conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_contiguous:CONTI",&pack_options)<0)
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

 TESTING("    layout chunked to compact conversion");
/*-------------------------------------------------------------------------
 * layout chunked to compact conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_chunk:COMPA",&pack_options)<0)
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

 TESTING("    layout chunked to contiguous conversion");

/*-------------------------------------------------------------------------
 * layout chunked to contiguous conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_chunk:CONTI",&pack_options)<0)
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

 TESTING("    layout chunked to chunk conversion");
/*-------------------------------------------------------------------------
 * layout chunked to chunked conversion
 *-------------------------------------------------------------------------
 */
 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addlayout("dset_chunk:CHUNK=18x13",&pack_options)<0)
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


 TESTING("    filter queue");

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
 if (h5repack_addfilter("dset1:SHUF",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset1:SZIP=8",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addfilter("dset1:GZIP=1",&pack_options)<0)
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

 puts("All h5repack tests passed.");
 
 return 0;
 
error:
 puts("***** H5REPACK TESTS FAILED *****");
 return 1;

}



