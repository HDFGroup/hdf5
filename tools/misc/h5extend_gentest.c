/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*********************************************************************
*
* Purpose:      Create testfile for use with the h5extend tests.
* 
* Developer:    Mike McGreevy
*               March 3, 2011
*
*********************************************************************/

#include "hdf5.h"
#include "H5private.h"

#define TESTFILE "h5extend_tfile.h5"
#define TESTFILE_MULTI "h5extend_multifile.h5"

static int create_file (hid_t fapl, const char* filename);

int main(void)
{
    hid_t fapl;

    if(create_file(H5P_DEFAULT, TESTFILE) < 0)
        return -1;

    /* create a multi file driver */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS))<0)
        return -1;

    if (H5Pset_fapl_multi(fapl, NULL, NULL, NULL, NULL, TRUE) < 0)
        return -1;

    if(create_file(fapl, TESTFILE_MULTI) < 0)
        return -1;

    if(H5Pclose(fapl) < 0)
        return -1;

    /* Return */
    return 0;
} /* main */

static int create_file (hid_t fapl, const char* filename)
{
    /* Variables */
    hid_t fcpl,fid,sid,did = -1;         /* Object Descriptors */

    /* Create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) return -1;

    /* Enable 'avoid truncate' feature */
    if (H5Pset_avoid_truncate(fcpl, H5F_AVOID_TRUNCATE_ALL) < 0) return -1;

    /* Create a file that avoids truncation */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        return -1;

    /* Close the fcpl */
    if (H5Pclose(fcpl) < 0) return -1;

    /* Create dataspace for dataset */
    if ((sid = H5Screate(H5S_SCALAR)) < 0) return -1;

    /* Create dataset */
    if ((did = H5Dcreate2(fid, "Dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT,
                          H5P_DEFAULT, H5P_DEFAULT)) < 0) return -1;

    /* Close the file, dataspace, and dataset */
    if(H5Sclose(sid) < 0) return -1;
    if(H5Dclose(did) < 0) return -1;

    if(H5Fclose(fid) < 0) return -1;

    /* Re-open file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) return -1;

    /* Unlink the dataset, reducing the 'EOA' value (but not EOF) */
    if (H5Ldelete(fid, "Dataset", H5P_DEFAULT) < 0) return -1;

    /* Close file */
    if (H5Fclose(fid) < 0) return -1;  

    return 0;
}
