

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

#include "ffbench_common.h"
#include "hdf5.h"
#include "mpi.h"


#ifndef _FFBENCH_H_
#define _FFBENCH_H_


#define NAME_SIZE 1024
#define MAX_DIMS 32

/*FFbench : Main file and all necessary
  functions 
  Programmer : Vishwanath Venkatesan*/

/* Declarations necessary for 
   the benchmark*/


typedef struct{
  char filename[NAME_SIZE];
  char dsetname[NAME_SIZE];
  hsize_t n_coords;
  hsize_t coords[MAX_DIMS];
  int rw_flag;
  int num_ops;
  int overlap;

}file_args;

file_args *input_args;



int FFbench_initialize(int argc, char **argv);
int FFbench_read_config_file (char *fname, MPI_Comm comm);
int FFbench_string_trim(char *str);
void FFbench_print_args();
int FFbench_create_file_and_dataset (hid_t *file_id, hid_t *dset_id,
				     hid_t *fapl_id,
				     int rank,
				     MPI_Comm comm,
				     MPI_Info info);
  

int FFbench_create_dataspaces(hid_t **f_dataspaces,
			      hid_t **m_dataspaces,
			      int *_num_dataspaces,
			      hsize_t ***_count,
			      hsize_t ***_offset,
			      int rank,
			      int size);





#endif
