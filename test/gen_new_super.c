/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, July 15, 2003
 *
 * Purpose:	Create a file which will have the newer superblock format.
 *		This program is used to create the test file `tsupern.h5' which
 *      has the new format for superblock information.
 *		To build the test file, this program MUST be compiled and linked with
 *      the hdf5-1.6+ series of libraries and the generated test file must be
 *      put into the 'test' directory in the 1.4+ branch of the library.
 */

#include <assert.h>
#include "hdf5.h"

#define TESTFILE   "tsupern.h5"
#define ISTORE_IK  64


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Create a file with a new version (>0) of the superblock
 *
 * Return:	Success:
 *		Failure:
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 15, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       file;           /* File IDs for old & new files */
    hid_t       fcpl;           /* File creation property list */
    herr_t      ret;            /* Generic return value */

    /* Create a file creation property list */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    assert(fcpl>=0);

    ret=H5Pset_istore_k(fcpl,ISTORE_IK);
    assert(ret>=0);

    /* Creating a file with the non-default file creation property list should
     * create a version 1 superblock
     */

    /* Create file with custom file creation property list */
    file= H5Fcreate(TESTFILE, H5F_ACC_TRUNC , fcpl, H5P_DEFAULT);
    assert(file>=0);

    /* Close FCPL */
    ret=H5Pclose(fcpl);
    assert(ret>=0);

    /* Close file */
    ret=H5Fclose(file);
    assert(ret>=0);

    return 0;
}

