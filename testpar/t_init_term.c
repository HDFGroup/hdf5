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
 * Programmer:  Mohamad Chaarawi
 *              June 2015
 *
 * Purpose: This test checks for the correct initialization and
 * termination of the HDF5 library with MPI init and finalize.
 */

#include "testphdf5.h"

int nerrors = 0;			/* errors count */

const char *FILENAME[] = {
    "after_mpi_fin",
    NULL
};

int
main (int argc, char **argv)
{
    int         mpi_size, mpi_rank;
    MPI_Comm    comm  = MPI_COMM_WORLD;

    /* Initialize and finalize MPI */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);  

    if(MAINPROCESS)
	TESTING("Usage of Serial HDF5 after MPI_Finalize() is called");

    MPI_Finalize();

    nerrors += GetTestNumErrs();

    /* test if we can initialize the library with MPI being finalized
       and create a file serially */
    H5open();

    if(mpi_rank == 0) {
        char	filename[1024];
        hid_t   file_id;

        h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        VRFY((file_id >= 0), "H5Fcreate succeeded");
        H5Fclose(file_id);
        file_id = -1;
    }

    H5close();

    if(MAINPROCESS) {
        if(0 == nerrors)
            PASSED()
        else
	    H5_FAILED()
    }

    return (nerrors!=0);
}
