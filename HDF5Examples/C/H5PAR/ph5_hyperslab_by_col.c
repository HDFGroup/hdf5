/*
 *  This example writes data to the HDF5 file by columns.
 *  Number of processes is assumed to be 2.
 */

#include "hdf5.h"
#include "stdlib.h"

#define H5FILE_NAME "SDS_col.h5"
#define DATASETNAME "IntArray"
#define NX          8 /* dataset dimensions */
#define NY          6
#define RANK        2

int
main(int argc, char **argv)
{
    /*
     * HDF5 APIs definitions
     */
    hid_t   file_id, dset_id;    /* file and dataset identifiers */
    hid_t   filespace, memspace; /* file and memory dataspace identifiers */
    hsize_t dimsf[2];            /* dataset dimensions */
    hsize_t dimsm[2];            /* dataset dimensions */
    int    *data;                /* pointer to data buffer to write */
    hsize_t count[2];            /* hyperslab selection parameters */
    hsize_t stride[2];
    hsize_t block[2];
    hsize_t offset[2];
    hid_t   plist_id; /* property list identifier */
    int     i, j, k;
    herr_t  status;

    /*
     * MPI variables
     */
    int      mpi_size, mpi_rank;
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    /*
     * Initialize MPI
     */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);
    /*
     * Exit if number of processes is not 2
     */
    if (mpi_size != 2) {
        printf("This example to set up to use only 2 processes \n");
        printf("Quitting...\n");
        return 0;
    }

    /*
     * Set up file access property list with parallel I/O access
     */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(plist_id, comm, info);

    /*
     * OPTIONAL: It is generally recommended to set collective
     *           metadata reads on FAPL to perform metadata reads
     *           collectively, which usually allows datasets
     *           to perform better at scale, although it is not
     *           strictly necessary.
     */
    H5Pset_all_coll_metadata_ops(plist_id, true);

    /*
     * OPTIONAL: It is generally recommended to set collective
     *           metadata writes on FAPL to perform metadata writes
     *           collectively, which usually allows datasets
     *           to perform better at scale, although it is not
     *           strictly necessary.
     */
    H5Pset_coll_metadata_write(plist_id, true);

    /*
     * Create a new file collectively and release property list identifier.
     */
    file_id = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, plist_id);
    H5Pclose(plist_id);

    /*
     * Create the dataspace for the dataset.
     */
    dimsf[0]  = NX;
    dimsf[1]  = NY;
    dimsm[0]  = NX;
    dimsm[1]  = NY / 2;
    filespace = H5Screate_simple(RANK, dimsf, NULL);
    memspace  = H5Screate_simple(RANK, dimsm, NULL);

    /*
     * Create the dataset with default properties and close filespace.
     */
    dset_id =
        H5Dcreate(file_id, DATASETNAME, H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Sclose(filespace);

    /*
     * Each process defines dataset in memory and writes it to the hyperslab
     * in the file.
     */
    count[0]  = 1;
    count[1]  = dimsm[1];
    offset[0] = 0;
    offset[1] = mpi_rank;
    stride[0] = 1;
    stride[1] = 2;
    block[0]  = dimsf[0];
    block[1]  = 1;

    /*
     * Select hyperslab in the file.
     */
    filespace = H5Dget_space(dset_id);
    H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block);

    /*
     * Initialize data buffer
     */
    data = (int *)malloc(sizeof(int) * (size_t)dimsm[0] * (size_t)dimsm[1]);
    for (i = 0; i < dimsm[0] * dimsm[1]; i = i + dimsm[1]) {
        k = 1;
        for (j = 0; j < dimsm[1]; j++) {
            data[i + j] = (mpi_rank + 1) * k;
            k           = k * 10;
        }
    }

    /*
     * Create property list for collective dataset write.
     */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE);

    status = H5Dwrite(dset_id, H5T_NATIVE_INT, memspace, filespace, plist_id, data);
    free(data);

    /*
     * Close/release resources.
     */
    H5Dclose(dset_id);
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Pclose(plist_id);
    H5Fclose(file_id);

    if (mpi_rank == 0)
        printf("PHDF5 example finished with no errors\n");

    MPI_Finalize();

    return 0;
}
