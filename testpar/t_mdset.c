#include <testphdf5.h>

#define DIM  2
#define SIZE 32

void multiple_dset_write(char *filename, int ndatasets)
{
    int i, j, n, mpi_size, mpi_rank;
    hid_t iof, plist, dataset, memspace, filespace;
    hssize_t chunk_origin [DIM];
    hsize_t chunk_dims [DIM], file_dims [DIM];
    hsize_t count[DIM]={1,1};
    double outme [SIZE][SIZE];
    char dname [100];


    MPI_Comm_rank (MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size (MPI_COMM_WORLD, &mpi_size);

    VRFY((mpi_size <= SIZE), "mpi_size <= SIZE");

    chunk_origin [0] = mpi_rank * (SIZE / mpi_size);
    chunk_origin [1] = 0;
    chunk_dims [0] = SIZE / mpi_size;
    chunk_dims [1] = SIZE;

    for (i = 0; i < DIM; i++)
	file_dims [i] = SIZE;

    plist = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(plist, MPI_COMM_WORLD, MPI_INFO_NULL);
    iof = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, plist);
    H5Pclose (plist);

    memspace = H5Screate_simple (DIM, chunk_dims, NULL);
    filespace = H5Screate_simple (DIM, file_dims, NULL);
    H5Sselect_hyperslab (filespace, H5S_SELECT_SET, chunk_origin, chunk_dims, count, chunk_dims);

    for (n = 0; n < ndatasets; n++) {
	sprintf (dname, "dataset %d", n);
	dataset = H5Dcreate (iof, dname, H5T_NATIVE_DOUBLE, filespace, H5P_DEFAULT);
	VRFY((dataset > 0), dname); 

	/* calculate data to write */
	for (i = 0; i < SIZE; i++)
	    for (j = 0; j < SIZE; j++)
	        outme [i][j] = n*1000 + mpi_rank;

	H5Dwrite (dataset, H5T_NATIVE_DOUBLE, memspace, filespace, H5P_DEFAULT, outme);

	H5Dclose (dataset);
	if (! ((n+1) % 10)) {
	    printf("created %d datasets\n", n+1);
	    MPI_Barrier(MPI_COMM_WORLD);
	}
    }

    H5Sclose (filespace);
    H5Sclose (memspace);
    H5Fclose (iof);
}
