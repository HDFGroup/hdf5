#include <testphdf5.h>

#define DIM  2
#define SIZE 10
#define NUMITEMS 500 /* 988 */

void multiple_dset_write(char *filename)
{
  int i, j, nprocs, rank;
  hid_t iof, plist, dataset, memspace, filespace;
  hssize_t chunk_origin [DIM];
  hsize_t chunk_dims [DIM], file_dims [DIM];
  double outme [SIZE][SIZE];

  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  MPI_Comm_size (MPI_COMM_WORLD, &nprocs);

  chunk_origin [0] = 0;
  chunk_origin [1] = rank * (SIZE / nprocs);
  chunk_dims [0] = SIZE;
  chunk_dims [1] = SIZE / nprocs;

  for (i = 0; i < DIM; i++)
    file_dims [i] = SIZE;

  for (i = 0; i < SIZE; i++)
    for (j = 0; j < SIZE; j++)
      outme [i][j] = rank;

  plist = H5Pcreate (H5P_FILE_ACCESS);
  H5Pset_fapl_mpio(plist, MPI_COMM_WORLD, MPI_INFO_NULL);
  iof = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, plist);
  H5Pclose (plist);

  memspace = H5Screate_simple (DIM, chunk_dims, NULL);
  filespace = H5Screate_simple (DIM, file_dims, NULL);
  H5Sselect_hyperslab (filespace, H5S_SELECT_SET, chunk_origin, NULL, chunk_dims
, NULL);

  for (i = 0; i < NUMITEMS; i++) {
    char dname [100];

    sprintf (dname, "dataset %d", i);
    dataset = H5Dcreate (iof, dname, H5T_NATIVE_FLOAT, filespace, H5P_DEFAULT);
    if (dataset < 0) {
      fprintf (stderr, "proc %d: failed to create dataset %d\n", rank, i);
      exit (-1);
    }

    H5Dwrite (dataset, H5T_NATIVE_DOUBLE, memspace, filespace, H5P_DEFAULT, outme);

    H5Dclose (dataset);
  }

  H5Sclose (filespace);
  H5Sclose (memspace);
  H5Fclose (iof);
}
