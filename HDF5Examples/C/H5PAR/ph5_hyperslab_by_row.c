/*
 *  This example writes data to the HDF5 file by rows.
 *  Number of processes is assumed to be 1 or multiples of 2 (up to 8)
 */

#include "hdf5.h"
#include "stdlib.h"

#define H5FILE_NAME "SDS_row.h5"
#define DATASETNAME "IntArray"
#define NX          8 /* dataset dimensions */
#define NY          5
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
    int    *data;                /* pointer to data buffer to write */
    hsize_t count[2];            /* hyperslab selection parameters */
    hsize_t offset[2];
    hid_t   plist_id; /* property list identifier */
    int     i;
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
    filespace = H5Screate_simple(RANK, dimsf, NULL);

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
    count[0]  = dimsf[0] / mpi_size;
    count[1]  = dimsf[1];
    offset[0] = mpi_rank * count[0];
    offset[1] = 0;
    memspace  = H5Screate_simple(RANK, count, NULL);

    /*
     * Select hyperslab in the file.
     */
    filespace = H5Dget_space(dset_id);
    H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, count, NULL);

    /*
     * Initialize data buffer
     */
    data = (int *)malloc(sizeof(int) * count[0] * count[1]);
    for (i = 0; i < count[0] * count[1]; i++) {
        data[i] = mpi_rank + 10;
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
