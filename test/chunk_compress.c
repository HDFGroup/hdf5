#include <mpi.h>
#include <stdlib.h>
#include <stdio.h>

#include "hdf5.h"


#define _MPI
#define _DSET2
//#define _COMPRESS

#define NPROC 4
#define CHUNK0 4

// Equivalent to original gist
// Works on 1.10.5 with patch, crashes on 1.10.5 vanilla and hangs on 1.10.6
//#define CHUNK1 32768
//#define NCHUNK1 32

// Works on 1.10.5 with and without patch and 1.10.6
//#define CHUNK1 256
//#define NCHUNK1 8192

// Works on 1.10.5 with and without patch and 1.10.6
//#define CHUNK1 512
//#define NCHUNK1 8192

// Crashes on 1.10.5 with and without patch, 1.10.6 and 1.12.0
#define CHUNK1 256
#define NCHUNK1 16384

int main(int argc, char **argv) {

    int mpi_size = 1, mpi_rank = 0;
    hid_t fapl_id, file_id, dset_space, dcpl_id, ds, ds2, propid, mem_dspace, sel_dspace;

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
#ifdef _MPI
    MPI_Comm comm  = MPI_COMM_WORLD;
    MPI_Info info  = MPI_INFO_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    H5Pset_fapl_mpio(fapl_id, comm, info);
#endif

    printf("MPI rank [%i/%i]\n", mpi_rank, mpi_size);

    printf("rank=%i creating file\n", mpi_rank);
    file_id = H5Fcreate("test1.h5", H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    H5Pclose(fapl_id);

    // Define total dataset size
    hsize_t dset_dims[2] = {NPROC * CHUNK0, CHUNK1 * NCHUNK1};
    dset_space = H5Screate_simple (2, dset_dims, NULL);
    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);

    // Set chunking and compression params
    hsize_t chunk_dims[2] = {CHUNK0, CHUNK1};
    H5Pset_chunk(dcpl_id, 2, chunk_dims);

#ifdef _COMPRESS
    H5Pset_deflate(dcpl_id, 9);
#endif


    // Define selection
    hsize_t sel_dims[2] = {CHUNK0, CHUNK1 * NCHUNK1};
    sel_dspace = H5Screate_simple(2, dset_dims, NULL);
    hsize_t offset[2] = {mpi_rank * sel_dims[0], 0};
    printf("rank=%i creating selection [%llu:%llu, %llu:%llu]\n",
           mpi_rank, offset[0], offset[0] + sel_dims[0], offset[1], offset[1] + sel_dims[1]);
    H5Sselect_hyperslab(sel_dspace, H5S_SELECT_SET,
                        offset, NULL, sel_dims, NULL);

    // Set the dspace for the input data
    mem_dspace = H5Screate_simple (2, sel_dims, NULL);

    propid = H5Pcreate(H5P_DATASET_XFER);
#ifdef _MPI
    H5Pset_dxpl_mpio(propid, H5FD_MPIO_COLLECTIVE);
#endif

    // Create the dataset
    printf("rank=%i creating dataset1\n", mpi_rank);
    ds = H5Dcreate (file_id, "dset1", H5T_NATIVE_FLOAT, dset_space, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);

    // Create array of data to write
    // Initialise with random data.
    int totalsize = sel_dims[0] * sel_dims[1];
    float *data = (float *)malloc(sizeof(float) * totalsize);
    for(int i = 0; i < totalsize; i++) {
        data[i] = (float)drand48();
    }

    printf("rank=%i writing dataset1\n", mpi_rank);
    H5Dwrite(ds, H5T_NATIVE_FLOAT, mem_dspace, sel_dspace, propid, data);
    printf("rank=%i finished writing dataset1\n", mpi_rank);

#ifdef _DSET2
    // Create the dataset
    printf("rank=%i creating dataset2\n", mpi_rank);
    ds2 = H5Dcreate (file_id, "dset2", H5T_NATIVE_FLOAT, dset_space, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    
    // Generate new data and write
    printf("rank=%i writing dataset2\n", mpi_rank);
    for(int i = 0; i < totalsize; i++) {
        data[i] = drand48();
    }
    H5Dwrite(ds2, H5T_NATIVE_FLOAT, mem_dspace, sel_dspace, propid, data);
    
    H5Dclose(ds2);
#endif

    // Close down everything
    printf("rank=%i closing everything\n", mpi_rank);
    H5Dclose(ds);
    H5Sclose(dset_space);
    H5Sclose(sel_dspace);
    H5Sclose(mem_dspace);
    H5Pclose(dcpl_id);
    H5Pclose(propid);
    H5Fclose(file_id);

    free(data);

    MPI_Finalize();
    return 0;
 }

