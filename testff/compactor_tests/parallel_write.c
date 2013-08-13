#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "mpi.h"
#include "hdf5.h"

#define NX     8                      /* dataset dimensions */
#define NY     8 
#define RANK   2



int main (int argc, char **argv){
  
  const char filename[] = "eff_file.h5";
  hid_t file_id;
  hid_t dataspaceID, dataspace2;
  hid_t dset_id;
  hid_t fapl_id, dxpl_id;
  
  const unsigned int nelem = 60;
  int *data = NULL;
  unsigned int i = 0;
  hsize_t dimsf[2];
  hsize_t count[2], offset[2];

  int my_rank, my_size, ret;
  int provided;
  hid_t event_q, int_id;
  
  int num_requests = 0;  
  H5_status_t *status = NULL;

  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
  if(MPI_THREAD_MULTIPLE != provided) {
    fprintf(stderr, "MPI does not have MPI_THREAD_MULTIPLE support\n");
    exit(1);
  }

  EFF_init (MPI_COMM_WORLD, MPI_INFO_NULL);

  MPI_Comm_rank (MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size (MPI_COMM_WORLD, &my_size);

  fprintf(stderr, "APP processes = %d, my rank is %d\n", my_size, my_rank);

  fapl_id = H5Pcreate (H5P_FILE_ACCESS);
  H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

  event_q = H5EQcreate(fapl_id);
  assert(event_q);

  file_id = H5Fcreate_ff(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, event_q);
  assert(file_id);


  dimsf[0] = NX;
  dimsf[1] = NY;

  /*  int_id = H5Tcopy(H5T_STD_I32LE);
  ret = H5Tcommit_ff(file_id, "int", int_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
		     0, event_q);
    
		     assert(ret == 0);*/

  dataspaceID = H5Screate_simple(RANK, dimsf, NULL); 

  dset_id = H5Dcreate_ff(file_id, "D1" , H5T_NATIVE_INT, dataspaceID,
			 H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
  assert(dset_id);



  count[0] = NY/my_size;
  count[1] = 8;
  offset[0] = my_rank * count[0];
  offset[1] = 0;

  dataspace2 = H5Screate_simple(RANK, count, NULL);
  

  H5Sselect_hyperslab(dataspaceID, 
		      H5S_SELECT_SET,
		      offset,
		      NULL,
		      count,
		      NULL);

  
  data = (int *) malloc(sizeof(int)*count[0]*count[1]);
  for (i=0; i < count[0]*count[1]; i++) {
    data[i] = my_rank + 10;
  }
  
 
  ret = H5Dwrite_ff(dset_id, 
		    H5T_NATIVE_INT,
		    dataspace2, 
		    dataspaceID,
		    H5P_DEFAULT,
		    data,
		    0,
		    event_q);
  assert(ret == 0);
  


  assert(H5Dclose_ff(dset_id, event_q) == 0);
  H5Sclose(dataspaceID);
  H5Sclose(dataspace2);
  H5Pclose(fapl_id);
  assert(H5Fclose_ff(file_id, event_q) == 0);
  H5EQwait(event_q, &num_requests, &status);
  free(status);
  H5EQclose (event_q);

  free(data);
  fprintf(stderr, "\n*****************************************************************************************************************\n");
  fprintf(stderr, "Finalize EFF stack\n");
  fprintf(stderr, "*****************************************************************************************************************\n");
  EFF_finalize();
  MPI_Finalize();
  
  return 0;
}
