#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mpi.h"
#include "hdf5.h"

static char *random_hdf5_text = 
  "Now is the time for all first-time-users of HDF5 to read their manual or go thru the tutorials!\n\
While you\'re at it, now is also the time to read up on MPI-IO.";

static char *datafile_relocated = "relocated_super.h5";
hbool_t pass = true;


static void
generate_test_file( int mpi_rank, int mpi_size )
{
     FILE *header;
     char *datafile_base = "mytestfile.h5";
     char *prologue_file = "hdf5_readme.txt";
     hid_t file_id, memspace, filespace, attr_id, fapl_id, dxpl_id, dset_id;
     hsize_t i, offset, count = 1000;
     hsize_t dims[1] = {0};
     float nextValue, data_slice[count];

     pass = true;

     nextValue = (float)(mpi_rank * count);
     for(i=0; i<count; i++) {
          data_slice[i] = nextValue;
	  nextValue += 1;
     }

     /* create the file (parallel) */
     fapl_id = H5Pcreate(H5P_FILE_ACCESS);
     H5Pset_fapl_mpio(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);
     file_id = H5Fcreate(datafile_base, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
     if ( file_id < 0 ) {
          pass = false;
          HDfprintf(stderr, "FATAL: H5Fcreate failed!\n");
     }

     if ( pass ) {
          dxpl_id = H5Pcreate(H5P_DATASET_XFER);
	  H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE);
     }

     dims[0] = count;
     memspace  = H5Screate_simple(1, dims, NULL);
     dims[0] *= mpi_size;
     filespace = H5Screate_simple(1, dims, NULL);
     offset = mpi_rank * count;
     H5Sselect_hyperslab(filespace, H5S_SELECT_SET, &offset, NULL, &count, NULL);
     
     dset_id = H5Dcreate2(file_id, "dataset0", H5T_NATIVE_FLOAT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
     if ( dset_id < 0 ) {
          pass = false;
          HDfprintf(stderr, "FATAL: H5Dcreate2 failed!\n");
     }

     if ( pass ) {
          H5Dwrite(dset_id, H5T_NATIVE_FLOAT, memspace, filespace, dxpl_id, data_slice);
     }
     H5Dclose(dset_id);
     H5Sclose(memspace);
     H5Sclose(filespace);

     /* finished creating a data file */
     H5Fclose(file_id);  
     H5Pclose(dxpl_id);

     if ( mpi_rank > 0 ) return;

     /* ----  mpi_rank 0 ------*/
     header = fopen( prologue_file, "w+");
     if (header == NULL) {
          pass = false;
          HDfprintf(stderr, "FATAL: Unable to create a simple txt file\n");
	  return;
     }
     else {
          size_t bytes_written, bytes_to_write = strlen(random_hdf5_text);
	  bytes_written = fwrite( random_hdf5_text, 1, bytes_to_write , header);
	  if (bytes_written == 0) {
	       pass = false;
	       HDfprintf(stderr, "FATAL: Unable to write a simple txt file\n");
	  }
	  fclose(header);
     }

     if ( pass ) {
          char cmd[256];
	  sprintf(cmd, "../tools/src/h5jam/h5jam -i %s -u %s -o %s", 
		  datafile_base, prologue_file, datafile_relocated);
	  system(cmd);
	  unlink(datafile_base);
          unlink(prologue_file);
     }
}


static void
test_parallel_read( int mpi_rank, int mpi_size )
{
     int status, errors =  0;
     hid_t access_plist = -1, dataset = -1;
     hid_t file_id = -1, memspace = -1, dataspace = -1;
     hsize_t i, offset, count = 1000;
     hsize_t dims[1] = {0};
     float nextValue, data_slice[count];
     herr_t ret;

     access_plist = H5Pcreate(H5P_FILE_ACCESS);
     if (access_plist >= 0) {
          ret = H5Pset_fapl_mpio(access_plist, MPI_COMM_WORLD, MPI_INFO_NULL);
     } else pass = false;
     if (ret >= 0) {
          file_id = H5Fopen(datafile_relocated,H5F_ACC_RDONLY,access_plist);
     } else pass = false;
     if (file_id >= 0) {
          dataset = H5Dopen2(file_id, "dataset0", H5P_DEFAULT);
     } else pass = false;
     if (dataset >= 0) {
          dims[0] = count;
          memspace = H5Screate_simple(1, dims, NULL);
     } else pass = false;
     if ( memspace >= 0 ) {
          dataspace = H5Dget_space(dataset);
     } else pass = false;
     if ( dataspace >= 0 ) {
	  offset = mpi_rank * count;
	  ret = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, &offset, NULL, &count, NULL);
     } else pass = false;
     if ( ret >= 0 ) {
          ret = H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, data_slice);
     } else pass = false;
     if (ret >= 0) {
          nextValue = (float)(mpi_rank * count);
	  for (i=0; i < count; i++) {
               if (data_slice[i] != nextValue) pass = false;
	       nextValue += 1;
	  }
     } else pass = false;

     status = ( pass ? 0 : -1 );
     MPI_Allreduce( &status, &errors, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD );

     if ( mpi_rank == 0) 
          HDfprintf(stderr, "H5Fopen/H5Dread/data_validation %s\n", ((errors == 0) ? "succeeded" : "FAILED"));
     
     H5Pclose(access_plist);
     H5Dclose(dataset);
     H5Fclose(file_id);  

     /* Cleanup */
     unlink(datafile_relocated);

     return;     
}


int
main( int argc, char **argv)
{
     int status, errors, mpi_rank, mpi_size;

     if ((status = MPI_Init(&argc, &argv)) != MPI_SUCCESS) {
          HDfprintf(stderr, "FATAL: Unable to initialize MPI\n");
	  exit(1);
     }
     if ((status = MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank)) != MPI_SUCCESS) {
          HDfprintf(stderr, "FATAL: MPI_Comm_rank returned an error\n");
	  exit(2);
     }
     if ((status = MPI_Comm_size(MPI_COMM_WORLD, &mpi_size)) != MPI_SUCCESS) {
          HDfprintf(stderr, "FATAL: MPI_Comm_size returned an error\n");
	  exit(2);
     }

     generate_test_file( mpi_rank, mpi_size );
     status = ( pass ? 0 : -1 );

     /* Synch all ranks before attempting the parallel read */
     if ( MPI_Allreduce( &status, &errors, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD ) != MPI_SUCCESS) {
          pass = false;
          if (mpi_rank == 0) HDfprintf(stderr, "FATAL: MPI_Allreduce returned an error\n");
     }

     if ( errors == 0 ) {
          test_parallel_read( mpi_rank, mpi_size );
     }

     MPI_Finalize();
     return 0;
}
