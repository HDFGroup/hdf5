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

/*
 * Programmer:  Pedro Vicente <pvn@hdfgroup.edu>
 *              April 7, 2008
 *
 * Purpose:     Tests the H5Dget_chunk_info API function
 */


#include "hdf5.h"
#include "h5test.h"

#define H5FILE_NAME "chunk_info.h5"
#define DATASETNAME "2d"
#define RANK         2


int main( void )
{
    
    hid_t   fid;
    hid_t   did;
    hid_t   sid;
    hid_t   pid;
    hsize_t dims[2]  = { 4, 4};       
    hsize_t chunk_dims[2] = { 2, 2 };
    int     fillvalue = 0;

    /* create a new file using default properties. */
    if ((fid = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    /* create the data space */
    if ((sid = H5Screate_simple(RANK, dims, dims)) < 0) TEST_ERROR;

    TESTING("chunk info");

   /*-------------------------------------------------------------------------
    * create a dataset
    *-------------------------------------------------------------------------
    */

    /* modify dataset creation properties, i.e. enable chunking. */
    if ((pid = H5Pcreate (H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if (H5Pset_chunk(pid, RANK, chunk_dims) < 0) TEST_ERROR;
    if (H5Pset_fill_value(pid, H5T_NATIVE_INT, &fillvalue) < 0) TEST_ERROR;

    /* create a new dataset */
    if((did = H5Dcreate2(fid , DATASETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR;


   /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */

    if (H5Dclose(did) < 0) TEST_ERROR
    if (H5Sclose(sid) < 0) TEST_ERROR
    if (H5Pclose(pid) < 0) TEST_ERROR
    if (H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
  
    puts("All chunk info tests passed.");
    return 0;


error:
    H5Dclose( did );
    H5Sclose( sid );
    H5Pclose( pid  );
    H5Fclose( fid );
    H5_FAILED();
    return 1;
}



