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
 *              This program writes a 4x4 dataset by iterating on 2x2 chunks
 *               at a time
 */


#include "hdf5.h"
#include "h5test.h"

#define PRINT_DATA
#define H5FILE_NAME "chunk_info.h5"
#define DATASETNAME "2d"
#define RANK         2


int main( void )
{

    hid_t   fid;      /* file ID */
    hid_t   did;      /* dataset ID */
    hid_t   f_sid;    /* file space ID */
    hid_t   m_sid;    /* memory space ID */
    hid_t   pid;      /* property list ID */
    hsize_t start[2]; /* chunk location to start writing */
    hsize_t dims[2]  = { 4, 4};
    hsize_t chunk_dims[2] = { 2, 2 };
    int     chunk_data[2][2] = { {1, 1}, {1, 1} };
    int     buf[4][4];
    int     fillvalue = 0;
    int     i, j, ii, jj;

    /* create a new file using default properties. */
    if ((fid = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    /* create the file space */
    if ((f_sid = H5Screate_simple(RANK, dims, dims)) < 0) TEST_ERROR;
    /* create the memory space with chunk dimensions */
    if ((m_sid = H5Screate_simple(RANK, chunk_dims, chunk_dims)) < 0) TEST_ERROR;
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(m_sid, H5S_SELECT_SET, start, NULL, chunk_dims, NULL) < 0) TEST_ERROR;

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
    if((did = H5Dcreate2(fid , DATASETNAME, H5T_NATIVE_INT, f_sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR;


   /*-------------------------------------------------------------------------
    * write the dataset in 2x2 chunks
    *-------------------------------------------------------------------------
    */

    /* iterate in dim 0 */
    for (j = 0; j < chunk_dims[0]; j++)
    {

        /* reset start in dim 1 */
        start[1] = 0;

        /* iterate in dim 1 */
        for (i = 0; i < chunk_dims[1]; i++)
        {

            /* select file hyperslab to save a 2x2 chunk */
            if (H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, NULL, chunk_dims, NULL) < 0) TEST_ERROR;

            /* write the data to the hyperslab. */
            if (H5Dwrite(did, H5T_NATIVE_INT, m_sid, f_sid, H5P_DEFAULT, chunk_data) < 0) TEST_ERROR;

            /* read back and display complete dataset 4x4 */
            if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;


#if defined (PRINT_DATA)
            printf("\n");
            printf("dataset: \n");
            for (jj = 0; jj < dims[0]; jj++) {
                for (ii = 0; ii < dims[1]; ii++) printf("%d ", buf[jj][ii]);
                printf("\n");
            }
#endif


            /* increment start in dim 1 */
            start[1] += 2;


        }

        /* increment start in dim 0 */
        start[0] += 2;
    }



   /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */

    if (H5Dclose(did) < 0) TEST_ERROR
    if (H5Sclose(f_sid) < 0) TEST_ERROR
    if (H5Sclose(m_sid) < 0) TEST_ERROR
    if (H5Pclose(pid) < 0) TEST_ERROR
    if (H5Fclose(fid) < 0) TEST_ERROR

    PASSED();

    puts("All chunk info tests passed.");
    return 0;


error:
    H5Dclose( did );
    H5Sclose( f_sid );
    H5Sclose( m_sid );
    H5Pclose( pid  );
    H5Fclose( fid );
    H5_FAILED();
    return 1;
}



