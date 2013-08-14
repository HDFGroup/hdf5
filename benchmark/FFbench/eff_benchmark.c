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
  int rank;
  int size;

  MPI_Init_thread(&argc,&argv, MPI_THREAD_MULTIPLE, &provided);
  if(MPI_THREAD_MULTIPLE != provided) {
    DEBUG_LINE("MPI does not have MPI_THREAD_MULTIPLE support\n");
    ret_value = FFB_FAIL;
    goto exit;
  }

  if (argc < 2){
    DEBUG_LINE ("Configuration file needed to start \n")
    MPI_Finalize();    
    goto exit;
  }

  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  
  FFbench_read_config_file (argv[1], comm);
  FFbench_print_args();
  EFF_init (comm, info);
  if (FFB_SUCCESS != FFbench_create_file_and_dataset (&file_id,
						      &dset_id,
						      &fapl_id,
						      rank,
						      comm,
						      info)){
    DEBUG_LINE("Error in File/dataset creation")
  }
  




  H5Pclose(fapl_id);
  H5Dclose(dset_id);
  H5Fclose(file_id);
  
  EFF_finalize();
  MPI_Finalize();
 exit:
  return ret_value;
}
