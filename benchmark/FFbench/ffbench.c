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


#include "ffbench.h"


/******************************************
/*FFbench : Main file and all necessary
  functions 

  Programmer : Vishwanath Venkatesan
***********************************************/

/*-------------------------------------------------------------------------
 *  Function:      FFbench_string_trim
 *
 *  Purpose :      Function to trim whitespaces from a string
 *
 *  Return:        SUCCESS : FFB_SUCCESS
 *                 FAILURE : FFB_FAIL
 *
 *  Programmer:    Vishwanath Venkatesan
 *                 August, 2013
 *--------------------------------------------------------------------------
 */


int FFbench_string_trim (char *str){
  
  int ret_value = FFB_SUCCESS;
  char *end = NULL;
  
  /* Trim leading space*/
  while(isspace(*str)) str++;

  /* Trim trailing space */
  end = str + strlen(str) - 1;
  while(end > str && isspace(*end)) end--;

  /* Write new null terminator */
  *(end+1) = 0;

  return  ret_value;
}


/*-------------------------------------------------------------------------
 *  Function:      FFbench_read_config_file 
 *
 *  Purpose :      Function to read the config file for the benchmark
 *
 *  Return:        SUCCESS : FFB_SUCCESS
 *                 FAILURE : FFB_FAIL
 *
 *  Programmer:    Vishwanath Venkatesan
 *                 August, 2013
 *--------------------------------------------------------------------------
 */


int FFbench_read_config_file (char *fname, 
			      MPI_Comm comm){
  
  int ret_value = FFB_SUCCESS;
  FILE *fp;
  char *tmp_string = NULL;
  int cnt = 0, i;
  char *charLoc = NULL;
  int rank;

  MPI_Comm_rank (comm, &rank);

  input_args = (file_args *) malloc (sizeof(file_args));
  if (NULL == input_args){
    DEBUG_LINE ("Error allocating File Args struct")
  }

  if (!rank){

    fp = fopen (fname, "r");
    
    
    tmp_string = (char *) malloc (NAME_SIZE * sizeof (char));
    if (NULL == tmp_string){
      DEBUG_LINE("Error in allcating Dsetname\n")
      ret_value = FFB_FAIL;
    }
    
    while (fgets (tmp_string, NAME_SIZE, fp) != NULL){
      
      charLoc = strchr (tmp_string, '#');
      if (charLoc != NULL){
	continue;
      }
      switch(cnt){
      case 0:
	strcpy (input_args->filename, tmp_string);      
	FFbench_string_trim(input_args->filename);
	if (strlen(input_args->filename) == 0){
	  DEBUG_LINE("Empty Filename \n")
	  ret_value = FFB_FAIL;
	  goto exit;
	}
	break;
      case 1:
	strcpy (input_args->dsetname, tmp_string);
	FFbench_string_trim(input_args->dsetname);
	if (strlen(input_args->dsetname) == 0){
	  DEBUG_LINE("Empty Dsetname\n")
	  ret_value = FFB_FAIL;
	  goto exit;
	}
	break;
      case 2:
	input_args->n_coords = atol(tmp_string);
	if (input_args->n_coords == 0){
	  DEBUG_LINE ("Coordinates cannot be 0\n")
	  ret_value = FFB_FAIL;
	  goto exit;
	}
	break;
      case 3:
	input_args->coords[0] = atol(tmp_string);
	for ( i = 1; i < input_args->n_coords; i++){
	  tmp_string[0] = 0;
	  fgets (tmp_string, NAME_SIZE, fp);
	  input_args->coords[i] = atol(tmp_string);
	}
	break;
      case 4:
	FFbench_string_trim(tmp_string);
	if(!strcmp("READ", tmp_string)){
	  input_args->rw_flag = READ;
	}
	else{					
	  input_args->rw_flag = WRITE;
	}
	break;
      case 5:
	input_args->num_ops = atoi(tmp_string);
	break;
      case 6:
	input_args->overlap = atoi(tmp_string);
	break;
      } 
      cnt++;
      tmp_string[0] = 0;
    }
  }
  /*Lets broadcast this input array to all processes!*/
  MPI_Bcast( (void *)input_args, 
	     sizeof(*input_args),
	     MPI_BYTE,
	     0,
	     comm);

  
 exit:
    return ret_value;
}

/*-------------------------------------------------------------------------
 *  Function:      FFbench_print_args
 *
 *  Purpose :      Print the created input structure from config file
 *
 *  Return:        Void routine
 *
 *  Programmer:    Vishwanath Venkatesan
 *                 August, 2013
 *--------------------------------------------------------------------------
 */

void FFbench_print_args(){
  
  hsize_t j;

  DEBUG_PRINT("Filename %s, Dataset: %s, ncoords: %lli, rw: %d, overlap: %d \n",
	      input_args->filename,
	      input_args->dsetname,
	      input_args->n_coords,
	      input_args->rw_flag,
	      input_args->overlap)

  for (j =0; j< input_args->n_coords;
       j++){
  
     DEBUG_PRINT("coord[%lli]: %lli\n",j , input_args->coords[j])
  }
	      
	      
}

/*-------------------------------------------------------------------------
 *  Function:      FFbench_create_file
 *
 *  Purpose :      Function to read the config file for the benchmark
 *
 *  Return:        SUCCESS : FFB_SUCCESS
 *                 FAILURE : FFB_FAIL
 *
 *  Programmer:    Vishwanath Venkatesan
 *                 August, 2013
 *--------------------------------------------------------------------------
 */


int FFbench_create_file_and_dataset (hid_t *file_id, hid_t *dset_id,
				     hid_t *fapl_id,
				     int rank,
				     MPI_Comm comm,
				     MPI_Info info){
  
  int ret_value = FFB_SUCCESS;
  hid_t fapl_self_id;
  hid_t tmp_file_id, tmp_dset_id;
  hid_t dataspace;
  hsize_t *dimsf, j;
  

  if (!rank){
    
    dimsf = (hsize_t *) malloc (input_args->n_coords * sizeof(hsize_t));
    if (NULL == dimsf){
      DEBUG_LINE ("Error in allocating dimsf\n");
      ret_value = FFB_FAIL;
      goto exit;
    }
    
    for (j = 0; j < input_args->n_coords;
	 j++){
      dimsf[j] = input_args->coords[j];
    }
    dataspace = H5Screate_simple (input_args->n_coords, 
				  dimsf,
				  NULL);

    
    fapl_self_id = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_self_id, MPI_COMM_SELF, MPI_INFO_NULL);
    tmp_file_id = H5Fcreate(input_args->filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_self_id);
    tmp_dset_id = H5Dcreate(tmp_file_id,input_args->dsetname, 
			    H5T_NATIVE_INT,dataspace,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Pclose(fapl_self_id);
    H5Dclose(tmp_dset_id);
    H5Sclose(dataspace);
    H5Fclose(tmp_file_id);
  }
  
  MPI_Barrier(comm);

  *fapl_id = H5Pcreate (H5P_FILE_ACCESS);
  H5Pset_fapl_iod(*fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

  *file_id = H5Fopen(input_args->filename, H5F_ACC_RDWR, *fapl_id);
  *dset_id = H5Dopen(*file_id,
		     input_args->dsetname,H5P_DEFAULT); 
  
		     


 exit:
  return ret_value;
}
