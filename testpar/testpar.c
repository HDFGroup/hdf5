/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose: Provides support functions for hdf5 parallel tests.
 */

#include "testpar.h"

#define MAX_RANK 2

/*
 * Create the appropriate File access property list
 */
hid_t
create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type)
{
    hid_t  ret_pl = H5I_INVALID_HID;
    herr_t ret;      /* generic return value */
    int    mpi_rank; /* mpi variables */

    /* need the rank for error checking macros */
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if ((ret_pl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        return H5I_INVALID_HID;

    if (l_facc_type == FACC_DEFAULT)
        return ret_pl;

    if (l_facc_type == FACC_MPIO) {
        /* set Parallel access with communicator */
        if ((ret = H5Pset_fapl_mpio(ret_pl, comm, info)) < 0)
            return H5I_INVALID_HID;
        if ((ret = H5Pset_all_coll_metadata_ops(ret_pl, true)) < 0)
            return H5I_INVALID_HID;
        if ((ret = H5Pset_coll_metadata_write(ret_pl, true)) < 0)
            return H5I_INVALID_HID;
        return ret_pl;
    }

    if (l_facc_type == (FACC_MPIO | FACC_SPLIT)) {
        hid_t mpio_pl;

        if ((mpio_pl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
            return H5I_INVALID_HID;
        /* set Parallel access with communicator */
        if ((ret = H5Pset_fapl_mpio(mpio_pl, comm, info)) < 0)
            return H5I_INVALID_HID;

        /* setup file access template */
        if ((ret_pl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
            return H5I_INVALID_HID;
        /* set Parallel access with communicator */
        if ((ret = H5Pset_fapl_split(ret_pl, ".meta", mpio_pl, ".raw", mpio_pl)) < 0)
            return H5I_INVALID_HID;
        if (H5Pclose(mpio_pl) < 0)
            return H5I_INVALID_HID;

        return ret_pl;
    }

    /* unknown file access types */
    return ret_pl;
}

/*
 * Setup the coordinates for point selection.
 */
void
point_set(hsize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[], size_t num_points,
          hsize_t coords[], int order)
{
    hsize_t i, j, k = 0, m, n, s1, s2;

    HDcompile_assert(MAX_RANK == 2);

    if (OUT_OF_ORDER == order)
        k = (num_points * MAX_RANK) - 1;
    else if (IN_ORDER == order)
        k = 0;

    s1 = start[0];
    s2 = start[1];

    for (i = 0; i < count[0]; i++)
        for (j = 0; j < count[1]; j++)
            for (m = 0; m < block[0]; m++)
                for (n = 0; n < block[1]; n++)
                    if (OUT_OF_ORDER == order) {
                        coords[k--] = s2 + (stride[1] * j) + n;
                        coords[k--] = s1 + (stride[0] * i) + m;
                    }
                    else if (IN_ORDER == order) {
                        coords[k++] = s1 + stride[0] * i + m;
                        coords[k++] = s2 + stride[1] * j + n;
                    }
}
