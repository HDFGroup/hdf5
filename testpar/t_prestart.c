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
 * Programmer:  Mohamad Chaarawi
 *              February 2015
 *
 * Purpose: This test opens a file created by the t_pshutdown program
 * and makes sure the objects created are there.
 */

#include "testphdf5.h"

int nerrors = 0;			/* errors count */

const char *FILENAME[] = {
    "shutdown",
    NULL
};

int
main (int argc, char **argv)
{
    hid_t       file_id, dset_id, grp_id;
    hid_t       fapl, sid, mem_dataspace;
    herr_t      ret;
    char	filename[1024];
    int         mpi_size, mpi_rank, ndims, i, j;
    MPI_Comm    comm  = MPI_COMM_WORLD;
    MPI_Info    info  = MPI_INFO_NULL;
    hsize_t     dims[RANK];
    hsize_t     start[RANK];
    hsize_t     count[RANK];
    hsize_t     stride[RANK];
    hsize_t     block[RANK];
    DATATYPE   *data_array = NULL, *dataptr;	/* data buffer */

    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);  

    if(MAINPROCESS)
	TESTING("proper shutdown of HDF5 library");
 
    /* Set up file access property list with parallel I/O access */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fapl >= 0), "H5Pcreate succeeded");
    ret = H5Pset_fapl_mpio(fapl, comm, info);
    VRFY((ret >= 0), "");

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
    VRFY((file_id >= 0), "H5Fopen succeeded");

    grp_id = H5Gopen2(file_id, "Group", H5P_DEFAULT);
    VRFY((grp_id >= 0), "H5Gopen succeeded");

    dset_id = H5Dopen2(grp_id, "Dataset", H5P_DEFAULT);
    VRFY((dset_id >= 0), "H5Dopen succeeded");

    sid = H5Dget_space(dset_id);
    VRFY((dset_id >= 0), "H5Dget_space succeeded");

    ndims = H5Sget_simple_extent_dims(sid, dims, NULL);
    VRFY((ndims == 2), "H5Sget_simple_extent_dims succeeded");
    VRFY(dims[0] == ROW_FACTOR*mpi_size, "Wrong dataset dimensions");
    VRFY(dims[1] == COL_FACTOR*mpi_size, "Wrong dataset dimensions");

    /* allocate memory for data buffer */
    data_array = (DATATYPE *)HDmalloc(dims[0]*dims[1]*sizeof(DATATYPE));
    VRFY((data_array != NULL), "data_array HDmalloc succeeded");

    /* Each process takes a slabs of rows. */
    block[0] = dims[0]/mpi_size;
    block[1] = dims[1];
    stride[0] = block[0];
    stride[1] = block[1];
    count[0] = 1;
    count[1] = 1;
    start[0] = mpi_rank*block[0];
    start[1] = 0;

    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* write data independently */
    ret = H5Dread(dset_id, H5T_NATIVE_INT, mem_dataspace, sid,
                  H5P_DEFAULT, data_array);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    dataptr = data_array;

    for (i=0; i < block[0]; i++){
	for (j=0; j < block[1]; j++){
	    if(*dataptr != mpi_rank+1) {
                HDprintf("Dataset Verify failed at [%lu][%lu](row %lu, col %lu): expect %d, got %d\n",
                       (unsigned long)i, (unsigned long)j,
                       (unsigned long)(i+start[0]), (unsigned long)(j+start[1]),
                       mpi_rank+1, *(dataptr));
                nerrors ++;
            }
            dataptr++;
	}
    }
    MPI_Finalize();
    HDremove(filename);

    /* release data buffers */
    if(data_array) 
        HDfree(data_array);

    nerrors += GetTestNumErrs();

    if(MAINPROCESS) {
        if(0 == nerrors)
            PASSED()
        else
	    H5_FAILED()
    }

    return (nerrors!=0);
}
