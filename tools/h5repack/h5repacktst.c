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

#define FILENAME "h5repacktst.h5"


/*-------------------------------------------------------------------------
 * Function:	test
 *
 * Purpose:	test h5repack
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
test()
{
 hid_t fid;  /* File ID */
 hid_t fapl; /* File access property list */
 hid_t dsid; /* Dataset ID */
 hid_t sid;  /* Dataspace ID */
 
 TESTING("    h5repack");
 
 if((fid = H5Fcreate (FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))<0)
  TEST_ERROR;
 
 if((sid = H5Screate (H5S_SCALAR))<0)
  TEST_ERROR;
 
 if((dsid = H5Dcreate (fid, "dset1", H5T_NATIVE_INT, sid, H5P_DEFAULT))<0)
  TEST_ERROR;
 
 if(H5Dclose(dsid)<0)
  TEST_ERROR;
 
 if(H5Fclose(fid)<0)
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
 nerrors += test();


 /* check for errors */
 if (nerrors)
  goto error;
 puts("All h5repack passed.");
 
 return 0;
 
error:
 puts("***** H5REPACK TESTS FAILED *****");
 return 1;


 return 0;
}