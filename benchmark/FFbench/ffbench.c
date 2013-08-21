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
  int rank, size = 0;

  MPI_Comm_rank (comm, &rank);
  MPI_Comm_size (comm, &size);

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
	input_args->n_coords = atol(tmp_string) ;
	if (input_args->n_coords == 0){
	  DEBUG_LINE ("Coordinates cannot be 0\n")
	  ret_value = FFB_FAIL;
	  goto exit;
	}
	break;
      case 3:
	input_args->coords[0] = atol(tmp_string) * size;
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

  fprintf(stdout, "Filename: %s \nDataset Name: %s \nncoords: %lli \n",
	  input_args->filename,
	  input_args->dsetname,
	  input_args->n_coords);
  
  fprintf (stdout, "Coords: {");
  for (j =0; j< input_args->n_coords - 1;
       j++){
  
    fprintf(stdout,"%lli, ",input_args->coords[j]);
  }
  fprintf (stdout, "%lli}\n",
	   input_args->coords[input_args->n_coords - 1]);
  if (input_args->rw_flag == WRITE){
    fprintf (stdout, "Perform %d WRITE operation\n",
	     input_args->num_ops);
  }
  else{
    fprintf (stdout, "Perform %d READ operation\n",
	     input_args->num_ops);
  }
  if (input_args->overlap == 0){
    fprintf (stdout,"Overlap : NO \n");
  }
  else
    fprintf (stdout,"Overlap : YES \n");
	      
}

/*-------------------------------------------------------------------------
 *  Function:      FFbench_create_file_and_dataset
 *
 *  Purpose :      1. Creates a file and a dataset using one process
 *                 2. Closes the file and dataset
 *                 3. Opens the file in all the other processes.  
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
  if (!rank){
    if (dimsf)
      free(dimsf);
  }
  return ret_value;
}

/*-------------------------------------------------------------------------
 *  Function:      FFbench_create_dataspaces
 *
 *  Purpose :      Creates all the required dataspaces for the given 
 *                 scenario of write/read.
 *
 *  Return:        SUCCESS : FFB_SUCCESS
 *                 FAILURE : FFB_FAIL
 *
 *  Programmer:    Vishwanath Venkatesan
 *                 August, 2013
 *--------------------------------------------------------------------------
 */

int FFbench_create_dataspaces(hid_t **f_dataspaces,
			      hid_t **m_dataspaces,
			      int *_num_dataspaces,
			      hsize_t ***_count,
			      hsize_t ***_offset,
			      int rank,
			      int size){

  
  hid_t *memory_spaces = NULL;
  hid_t *file_spaces  = NULL;
  int num_dataspaces  = 0, i =0;
  hsize_t coords  = 0, j = 0;
  hsize_t **count = NULL;
  hsize_t **offset = NULL;
  hsize_t *dimsf = NULL, tmp = 0;
  int ret_value = FFB_SUCCESS;
  
  dimsf = (hsize_t *) malloc (input_args->n_coords *
			      sizeof(hsize_t));
  if (NULL == dimsf){
    DEBUG_ERROR ("Error in allocation dimsf")
    ret_value = FFB_FAIL;
  }
  
  for (j = 0; j < input_args->n_coords;
       j++){
    dimsf[j] = input_args->coords[j];
  }

  if (!input_args->overlap){

    if ( input_args->n_coords > 1){
      coords = input_args->coords[1];
    }
    else
      coords = input_args->coords[0];

    if (((coords %(input_args->num_ops * size)) == 0)){
      
      num_dataspaces  = input_args->num_ops;
      
      if (num_dataspaces < 1){
	DEBUG_ERROR ("Error num_dataspaces cannot be less than 1");
	ret_value = FFB_FAIL;  
      }

    }
    else{
      DEBUG_ERROR("ERROR! the Y coords are not exactly divisible\n")
      ret_value = FFB_FAIL;	
    }

    
    memory_spaces = (hid_t *) malloc (num_dataspaces *
				      sizeof(hid_t));
    if (NULL == memory_spaces){
      DEBUG_ERROR ("Error allocating memory spaces \n")
      ret_value = FFB_FAIL;	
    }

    file_spaces = (hid_t *) malloc (num_dataspaces *
				    sizeof(hid_t));
 
    if (NULL == file_spaces){
      DEBUG_ERROR ("Error allocating file spaces\n")
      ret_value = FFB_FAIL;	
    }

    count = (hsize_t **)malloc (num_dataspaces *
				sizeof(hsize_t *));
    if (NULL == count){
      DEBUG_ERROR ("Error Allocating count array\n")
      ret_value = FFB_FAIL;
    }
    
    offset = (hsize_t **) malloc (num_dataspaces *
				  sizeof(hsize_t *));
    if (NULL == offset){
      DEBUG_ERROR ("Error while allocating offset \n")
      ret_value = FFB_FAIL;
    }

    for (i = 0; i < num_dataspaces; i++){

      count[i] = (hsize_t *) malloc (input_args->n_coords *
				     sizeof(hsize_t));
      if (NULL == count[i]){
	DEBUG_ERROR ("Error while allocating count[i] \n");
	ret_value = FFB_FAIL;
      }
      
      offset[i] = (hsize_t *) malloc (input_args->n_coords *
				      sizeof(hsize_t));
      if (NULL == offset[i]){
	DEBUG_ERROR ("Error while allocating offset[i] \n");
	ret_value = FFB_FAIL;
      }

      file_spaces[i] = H5Screate_simple(input_args->n_coords,
					dimsf,
					NULL);
    }

    count[0][0] = dimsf[0]/(size * input_args->num_ops); 
    for (j = 1; j < input_args->n_coords; j++){
      count[0][j] = dimsf[j];
    }
    offset[0][0] = rank * count[0][0];
    offset[0][1] = 0;
    for ( i = 1; i < num_dataspaces; i++){
      count[i][0] = dimsf[0]/(size * input_args->num_ops);
      for (j = 1; j < input_args->n_coords; j++){
	count[i][j] = dimsf[j];
	offset[i][j] = 0;
      }
      offset[i][0] = offset[i - 1][0] + size * count[i][0];

    }      

    for ( i = 0; i < num_dataspaces; i++){

#if 0
      DEBUG_PRINT("%d: count[%d][0]: %lli, count[%d][1]: %lli off[%d][0]: %lli, off[%d][1]: %lli\n",
		  rank, i, count[i][0], i, count[i][1], i, offset[i][0], i, offset[i][1]);
#endif
 
      memory_spaces[i] = H5Screate_simple(input_args->n_coords,
					  count[i],
					  NULL);

      H5Sselect_hyperslab (file_spaces[i],
			   H5S_SELECT_SET,
			   offset[i],
			   NULL,
			   count[i],
			   NULL);


    }
  }
  else{
    
    if (size > 1){
      DEBUG_ERROR("Overlap write measurements cannot happen on more than one node\n");
      ret_value = FFB_FAIL;	
    }
    
    if ( input_args->n_coords > 1){
      coords = input_args->coords[1];
    }
    else
      coords = input_args->coords[0];
    
    if (((coords % (input_args->num_ops)) == 0)){
      
      num_dataspaces = input_args->num_ops;
      
      if (num_dataspaces < 1){
	DEBUG_ERROR ("Error num_dataspaces cannot be less than 1");
	ret_value = FFB_FAIL;
      }
    }
    else{
      DEBUG_ERROR("ERROR! the Y coords are not exactly divisible \n");
      ret_value = FFB_FAIL;
    }
    
    memory_spaces = (hid_t *) malloc (num_dataspaces *
				      sizeof(hid_t));
    if (NULL == memory_spaces){
      DEBUG_ERROR ("Error allocating memory spaces \n");
      ret_value = FFB_FAIL;	
    }
    
    file_spaces = (hid_t *) malloc (num_dataspaces *
				    sizeof(hid_t));
    if (NULL == file_spaces){
      DEBUG_ERROR ("Error Allocating file spaces");
      ret_value = FFB_FAIL;
    }

    count = (hsize_t **) malloc (num_dataspaces *
				 sizeof(hsize_t *));
    if (NULL == count){
      DEBUG_ERROR ("Error  Allocation count array");
      ret_value = FFB_FAIL;
    }
			
    offset = (hsize_t **) malloc (num_dataspaces *
				  sizeof(hsize_t *));
    if (NULL == offset){
      DEBUG_ERROR ("Error Allocating offset array");
      ret_value = FFB_FAIL;
    }
    
    for (i = 0; i < num_dataspaces; i++){

      count[i] = (hsize_t *) malloc (input_args->n_coords *
				     sizeof(hsize_t));
      if (NULL == count[i]){
	DEBUG_ERROR ("Error while allocating count[i] \n");
	ret_value = FFB_FAIL;
      }
      
      offset[i]= (hsize_t *) malloc (input_args->n_coords *
				     sizeof(hsize_t));
      if (NULL == offset[i]){
	DEBUG_ERROR ("Error while allocating offset[i] \n");
	ret_value = FFB_FAIL;
      }

      file_spaces[i] = H5Screate_simple(input_args->n_coords,
					dimsf,
					NULL);
    }

    count[0][0] = dimsf[0]/(input_args->num_ops);
    for (j = 1; j < input_args->n_coords; j++){
      count[0][j] = dimsf[j];
    }

    offset[0][0] = rank;
    offset[0][1] = 0;
    for (i = 1; i< num_dataspaces; i++){
      count[i][0] = dimsf[0]/(input_args->num_ops) ;
      for (j = 1; j< input_args->n_coords; j++){
	count[i][j] = dimsf[j];
	offset[i][j] = 0;
      }
      offset[i][0] = offset[i - 1][0] + count[i][0] - (count[i-1][0]/4);
    }


    for (i = 0; i< num_dataspaces; i++){
#if 0
      DEBUG_PRINT("%d: count[%d][0]: %lli, count[%d][1]: %lli off[%d][0]: %lli, off[%d][1]: %lli\n",
		  rank, i, count[i][0], i, count[i][1], i, offset[i][0], i, offset[i][1]);
#endif
 
      memory_spaces[i] = H5Screate_simple(input_args->n_coords,
					  count[i],
					  NULL);
      
      H5Sselect_hyperslab (file_spaces[i],
			   H5S_SELECT_SET,
			   offset[i],
			   NULL,
			   count[i],
			   NULL);


    }



  }
  
  
  if (dimsf)
    free(dimsf);

 exit:
  *m_dataspaces = memory_spaces;
  *f_dataspaces = file_spaces;
  *_num_dataspaces = num_dataspaces;
  *_count = count;
  *_offset = offset;
  
  
  return ret_value;
}
