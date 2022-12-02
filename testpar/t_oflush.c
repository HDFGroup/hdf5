/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Test for H5Oflush.  For the current design, H5Oflush doesn't work correctly
 * with parallel.  It causes an assertion failure in metadata cache during
 * H5Fclose.  This test makes sure H5Oflush fails for dataset, group, and named
 * datatype properly until the problem is solved. */

#include "testphdf5.h"
#include "H5Dprivate.h"
#include "H5private.h"

#define DATASETNAME "IntArray"
#define NX          5
#define NY          6
#define RANK        2

void
test_oflush(void)
{
    int         mpi_size, mpi_rank;
    hid_t       file, dataset;
    hid_t       dataspace;
    hid_t       fapl_id;
    const char *filename;
    hid_t       gid, dtype_flush;
    hsize_t     dimsf[2];
    herr_t      ret;
    int         data[NX][NY];
    int         i, j;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    /* Make sure MPIO driver is used */
    fapl_id = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, FACC_MPIO);
    VRFY((fapl_id >= 0), "fapl creation succeeded");

    /* Data buffer initialization */
    for (j = 0; j < NX; j++)
        for (i = 0; i < NY; i++)
            data[j][i] = i + j;

    filename = GetTestParameters();

    file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file >= 0), "file creation succeeded");

    /* Describe the size of the array and create the data space for fixed
     * size dataset */
    dimsf[0] = NX;
    dimsf[1] = NY;

    dataspace = H5Screate_simple(RANK, dimsf, NULL);
    VRFY((dataspace >= 0), "data space creation succeeded");

    /* Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties */
    dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "dataset creation succeeded");

    /* Write the data to the dataset using default transfer properties */
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    VRFY((ret >= 0), "dataset creation succeeded");

    /* Make sure H5Oflush fails with dataset */
    H5E_BEGIN_TRY
    {
        ret = H5Oflush(dataset);
    }
    H5E_END_TRY
    VRFY((ret < 0), "H5Oflush should fail as expected");

    H5Sclose(dataspace);
    H5Dclose(dataset);

    /* Create a group */
    gid = H5Gcreate(file, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((gid >= 0), "group creation succeeded");

    /* Make sure H5Oflush fails with group */
    H5E_BEGIN_TRY
    {
        ret = H5Oflush(gid);
    }
    H5E_END_TRY
    VRFY((ret < 0), "H5Oflush should fail as expected");

    H5Gclose(gid);

    /* Create a named datatype */
    dtype_flush = H5Tcopy(H5T_NATIVE_INT);
    H5Tcommit(file, "dtype", dtype_flush, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Make sure H5Oflush fails with named datatype */
    H5E_BEGIN_TRY
    {
        ret = H5Oflush(dtype_flush);
    }
    H5E_END_TRY
    VRFY((ret < 0), "H5Oflush should fail as expected");

    H5Tclose(dtype_flush);

    /* Close and release resources */
    H5Fclose(file);
    H5Pclose(fapl_id);
}
