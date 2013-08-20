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
  Benchmark for Exascale Fast forward operations
  Programmer : Vishwanath Venkatesan
*/


#include "ffbench.h"

int main (int argc, char **argv){


  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Info info = MPI_INFO_NULL;
  int provided;
  int ret_value = FFB_SUCCESS;
  hid_t file_id, dset_id;
  hid_t fapl_id;
  int rank, ret = FFB_SUCCESS;
  int size;
  hid_t *f_dataspaces, *m_dataspaces;
  hid_t event_q, int_id;
  int num_requests = 0;  
  H5_status_t *status = NULL;
  hsize_t **count=NULL, **offset=NULL;
  int num_dataspaces, i;
  int *data = NULL, k ,j, count_size = 1;
  double start, end;
  double totallength = 0;
  double totaltime = 0, max_time;

  MPI_Init_thread(&argc,&argv, MPI_THREAD_MULTIPLE, &provided);
  if(MPI_THREAD_MULTIPLE != provided) {
    DEBUG_LINE("MPI does not have MPI_THREAD_MULTIPLE support\n");
    ret_value = FFB_FAIL;
    goto exit;
  }

  EFF_init (comm, info);
  if (argc < 2){
    DEBUG_ERROR ("Error!!! Configuration file needed to start \n")
    EFF_finalize();
    MPI_Finalize();    
    goto exit;
  }

  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  
  FFbench_read_config_file (argv[1], comm);
  if (!rank){
    fprintf(stdout, "\n\n*****************************************\n");
    fprintf(stdout, "EFF Benchmark Suite\nBenchmark Settings\n");
    fprintf(stdout, "*****************************************\n\n");    
    FFbench_print_args();
    fprintf(stdout, "*****************************************\n\n");    
  }


  if (FFB_SUCCESS != FFbench_create_file_and_dataset (&file_id,
						      &dset_id,
						      &fapl_id,
						      rank,
						      comm,
						      info)){
    DEBUG_ERROR("Error in File/dataset creation")
  }
  

  if (FFB_SUCCESS != FFbench_create_dataspaces(&f_dataspaces,
					       &m_dataspaces,
					       &num_dataspaces,
					       &count,
					       &offset,
					       rank,
					       size)){
    DEBUG_ERROR("Error in creating datasets for the benchmark\n");
  }

  event_q = H5EQcreate(fapl_id);
  assert(event_q);

  
  start = MPI_Wtime();

  for (i = 0; i < num_dataspaces; i++){

    count_size = 1;

    for ( j = 0; j < input_args->n_coords; j++){
      count_size *= count[i][j];
    }

    data = (int *) malloc
      (sizeof(int)*count_size);
    
    totallength += count_size * sizeof(int);

    for (k=0; k < count_size; k++) {
      data[k] = rank + 1001 + i;
    }
    
    ret = H5Dwrite_ff(dset_id, 
		      H5T_NATIVE_INT,
		      m_dataspaces[i], 
		      f_dataspaces[i],
		      H5P_DEFAULT,
		      data,
		      0,
		      event_q);
    assert(ret == 0);

   
    H5EQwait(event_q, &num_requests, &status);
    free(status);
    
    if (data)
      free (data);
  }
  end = MPI_Wtime();
  totaltime = difftime (end, start);
  totallength *= size;

  
  MPI_Reduce(&totaltime, &max_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
  
  if (!rank){
    
    totallength /= 1024*1024;
     
    
    
    fprintf (stderr,
	     "Approx. %lf MB data took %lf(s) with write bandwidth of %lf MB/s\n",
	     totallength,
	     max_time,
	     totallength/max_time);
  }
  

  
  if (f_dataspaces)
    free(f_dataspaces);
  if (m_dataspaces)
    free(m_dataspaces);
  if (count){
    for (i = 0; i < num_dataspaces; i++)
      free(count[i]);
    free(count);
  }

  if (offset){
    for (i = 0; i < num_dataspaces; i++)
      free(offset[i]);
    free(offset);
  }


  H5Pclose(fapl_id);
  H5Dclose(dset_id);
  H5Fclose(file_id);
  
  EFF_finalize();
  MPI_Finalize();
 exit:
  return ret_value;
}
