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
#include "test_h5repack_add.h"

/*-------------------------------------------------------------------------
 * Function:	test
 *
 * Purpose:	
 *
 * 1) compress/chunk FILENAME with some compression/chunking options
 * 2) use the h5diff utility to compare the input and output file; 
 *     it returns RET==0 if the objects have the same data
 * 3) use API functions to verify the compression/chunking input on the otput file
 *
 * Return:	Success:	zero
 *		Failure:	1
 *
 * Programmer:	Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             September, 19, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test(void)
{
 pack_opt_t  pack_options;
 diff_opt_t  diff_options;
 memset(&diff_options, 0, sizeof (diff_opt_t));

 TESTING("    deflate filter");

 if (h5repack_init (&pack_options, 0)<0)
  TEST_ERROR;
 if (h5repack_addcomp("dset1:GZIP 9",&pack_options)<0)
  TEST_ERROR;
 if (h5repack_addchunk("dset1:5x4",&pack_options)<0)
  TEST_ERROR;
 if (h5repack(FILENAME,FILENAME_OUT,&pack_options)<0)
  TEST_ERROR;
 if (h5repack_end (&pack_options)<0)
  TEST_ERROR;
 if (h5diff(FILENAME,FILENAME,NULL,NULL,&diff_options) == 1)
  TEST_ERROR;
 
 PASSED();                                                 
 return 0; 
 
error:                                                       
 return 1;

}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Executes h5repack tests
 *
 * Return:	Success:	zero
 *		Failure:	non-zero
 *
 * Programmer:	Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             September, 19, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int main (void)
{
 int		nerrors=0;

 /* run tests  */
 puts("Testing h5repack:");
 nerrors += make_dsets();
 nerrors += test();


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

