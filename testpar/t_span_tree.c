
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* 
 *  This program shows how the H5Sselect_hyperslab and H5Sselect_elements
 *  functions are used to write selected data from memory to the file.
 *  Program takes 48 elements from the linear buffer and writes them into
 *  the matrix using 3x2 blocks, (4,3) stride and (2,4) count. 
 *  Then four elements  of the matrix are overwritten with the new values and 
 *  file is closed. Program reopens the file and selects the union of two 
 *  hyperslabs in the dataset in the file. Then it reads the selection into the
 *  memory dataset preserving the shape of the selection.
 */ 
 
#include "hdf5.h"
#include "H5private.h"
#include "testphdf5.h"


void t_span_tree()
{

   char *filename;
   hid_t   acc_plist,xfer_plist;
   hid_t   file, dataset;           /* File and dataset identifiers */
   hid_t   mid1, mid2, mid, fid;    /* Dataspace identifiers */
   hid_t   plist;                   /* Dataset property list identifier */

   hsize_t dim1[] = {MSPACE1_DIM};  /* Dimension size of the first dataset 
                                       (in memory) */ 
   hsize_t dim2[] = {MSPACE2_DIM};  /* Dimension size of the second dataset
                                       (in memory */ 
   hsize_t fdim[] = {FSPACE_DIM1, FSPACE_DIM2}; 
                                    /* Dimension sizes of the dataset (on disk) */
   hsize_t mdim[] = {MSPACE_DIM1, MSPACE_DIM2}; /* Dimension sizes of the 
                                                   dataset in memory when we
                                                   read selection from the
                                                   dataset on the disk */

   hssize_t start[2]; /* Start of hyperslab */
   hsize_t stride[2]; /* Stride of hyperslab */
   hsize_t count[2];  /* Block count */
   hsize_t block[2];  /* Block sizes */

   hssize_t coord[NPOINTS][FSPACE_RANK]; /* Array to store selected points 
                                            from the file dataspace */ 
   herr_t ret;
   unsigned i,j;
   int fillvalue = 0;   /* Fill value for the dataset */

   int    matrix_out[MSPACE_DIM1][MSPACE_DIM2]; /* Buffer to read from the 
                                                   dataset */
   int    vector[MSPACE1_DIM];
   int    values[] = {53, 59, 61, 67};  /* New values to be written */

   int    mpi_size,mpi_rank;
   hsize_t testdims[2];
   MPI_Comm comm = MPI_COMM_WORLD;
   MPI_Info info = MPI_INFO_NULL;

   /*set up MPI parameters */
   MPI_Comm_size(comm,&mpi_size);
   MPI_Comm_rank(comm,&mpi_rank);

   printf("mpi_size %d\n",mpi_size);
   printf("mpi_rank %d\n",mpi_rank);

   /* Obtain file name */
   filename = (char *) GetTestParameters();    


   /*
    * Buffers' initialization.
    */
   vector[0] = vector[MSPACE1_DIM - 1] = -1;
   for (i = 1; i < MSPACE1_DIM - 1; i++) vector[i] = i;

   acc_plist = H5Pcreate(H5P_FILE_ACCESS);
   ret       = H5Pset_fapl_mpio(acc_plist,comm,info);
   
   /*
    * Create a file.
    */
   file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_plist);

   /*
    * Create property list for a dataset and set up fill values.
    */
   plist = H5Pcreate(H5P_DATASET_CREATE);
   ret   = H5Pset_fill_value(plist, H5T_NATIVE_INT, &fillvalue); 

    /* 
     * Create dataspace for the dataset in the file.
     */
    fid = H5Screate_simple(FSPACE_RANK, fdim, NULL);

    /*
     * Create dataset in the file. Notice that creation
     * property list plist is used.
     */
    dataset = H5Dcreate(file, "Matrix in file", H5T_NATIVE_INT, fid, plist);


    /*
     * Select hyperslab for the dataset in the file, using 3x2 blocks, 
     * (4,3) stride and (1,4) count starting at the position (0,1)
       for the first selection
     */
    start[0]  = 0; start[1]  = 1;
    stride[0] = 4; stride[1] = 3;
    count[0]  = 1; count[1]  = 4;    
    block[0]  = 3; block[1]  = 2;
    ret = H5Sselect_hyperslab(fid, H5S_SELECT_SET, start, stride, count, block);

    if(ret < 0) printf("hyperslab selection is wrong. \n");    

    /*
     * Select hyperslab for the dataset in the file, using 3x2*4 blocks, 
     * stride 1  and (1,1) count starting at the position (4,0).
     */
    /*    start[0]  = 4; start[1]  = 0;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;    
    block[0]  = 3; block[1]  = 8;*/

    start[0]  = 4; start[1]  = 0;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;    
    block[0]  = 3; block[1]  = 8;
    ret = H5Sselect_hyperslab(fid, H5S_SELECT_OR, start, stride, count, block);
 
    if(ret < 0) printf("hyperslab selection is wrong with or selection. \n");
    /*
     * Create dataspace for the first dataset.
     */
    mid1 = H5Screate_simple(MSPACE1_RANK, dim1, NULL);

    /*
     * Select hyperslab. 
     * We will use 48 elements of the vector buffer starting at the second element.
     * Selected elements are 1 2 3 . . . 48
     */
    start[0]  = 1;
    stride[0] = 1;
    count[0]  = 48;
    block[0]  = 1;
    ret = H5Sselect_hyperslab(mid1, H5S_SELECT_SET, start, stride, count, block);
 
    /*
     * Write selection from the vector buffer to the dataset in the file.
     *
     * File dataset should look like this:       
     *                    0  1  2  0  3  4  0  5  6  0  7  8 
     *                    0  9 10  0 11 12  0 13 14  0 15 16
     *                    0 17 18  0 19 20  0 21 22  0 23 24
     *                    0  0  0  0  0  0  0  0  0  0  0  0
     *                    0 25 26  0 27 28  0 29 30  0 31 32
     *                    0 33 34  0 35 36  0 37 38  0 39 40
     *                    0 41 42  0 43 44  0 45 46  0 47 48
     *                    0  0  0  0  0  0  0  0  0  0  0  0
     */

     xfer_plist = H5Pcreate(H5P_DATASET_XFER);

     ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);

    for (i = 0; i < MSPACE_DIM1; i++) {
       for (j = 0; j < MSPACE_DIM2; j++)
            matrix_out[i][j] = 3;
    }

    /* ret = H5Dwrite(dataset, H5T_NATIVE_INT, mid1, fid, xfer_plist, vector);*/
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, mid1, fid, H5P_DEFAULT, vector);
    if(ret < 0) printf("failing to write data .\n");

    /* 
     * Initialize data buffer.
     */
    for (i = 0; i < MSPACE_DIM1; i++) {
       for (j = 0; j < MSPACE_DIM2; j++)
            matrix_out[i][j] = 1;
    }

    for (i=0; i < MSPACE_DIM1; i++) {
        for(j=0; j < MSPACE_DIM2; j++) printf("%3d  ", matrix_out[i][j]);
        printf("\n");
    }
    

#if 0

    /* for dataset reading */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);

     ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    ret = H5Dread(dataset, H5T_NATIVE_INT, fid, fid,
      xfer_plist, matrix_out);


     /* ret = H5Dread(dataset, H5T_NATIVE_INT, fid, fid,
	H5P_DEFAULT, matrix_out);*/

    if(ret < 0) printf("fail to read the dataset afterwards.\n");
    for (i=0; i < MSPACE_DIM1; i++) {
        for(j=0; j < MSPACE_DIM2; j++) printf("%3d  ", matrix_out[i][j]);
        printf("\n");
    }
#endif 
 
    ret = H5Sclose(mid1);
    ret = H5Sclose(fid); 
 
    /*
     * Close dataset.
     */
    ret = H5Dclose(dataset);

    /*
     * Close the file.
     */
    ret = H5Fclose(file);

    /*
     * Close property list
     */

     ret = H5Pclose(acc_plist);

    ret = H5Pclose(xfer_plist);
    ret = H5Pclose(plist);

#if 0
    /*
     * Open the file.
     */
    file = H5Fopen(filename, H5F_ACC_RDONLY, acc_plist);
    if(file <0) printf("fail to open the file.\n");

    /*
     * Open the dataset.
     */
    dataset = H5Dopen(file,"Matrix in file");
    if(dataset < 0) printf("fail to open the dataset.\n");
    
    /* 
     * Get dataspace of the open dataset.
     */
    fid = H5Dget_space(dataset);
    if(fid < 0) printf("cannot obtain the correct file space.\n");
    H5Sget_simple_extent_dims(fid,testdims,NULL);
    HDfprintf(stdout,"testdims[0] %Hu\n",testdims[0]);
    HDfprintf(stdout, "testdims[1] %Hu\n",testdims[1]);

    /* 
     * Initialize data buffer.
     */
    for (i = 0; i < MSPACE_DIM1; i++) {
       for (j = 0; j < MSPACE_DIM2; j++)
            matrix_out[i][j] = 1;
    }

    ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                  H5P_DEFAULT, matrix_out);

    if(ret < 0) printf("fail to read the dataset.\n");
    for (i=0; i < MSPACE_DIM1; i++) {
        for(j=0; j < MSPACE_DIM2; j++) printf("%3d  ", matrix_out[i][j]);
        printf("\n");
    }

    /*
     * Close memory file and memory dataspaces.
     */  
    ret = H5Sclose(mid);
    ret = H5Sclose(fid);
 
    /*
     * Close dataset.
     */  
    ret = H5Dclose(dataset);

    /*
     * Close property list
     */
    ret = H5Pclose(plist);
     ret = H5Pclose(acc_plist);

    ret = H5Pclose(xfer_plist);
 
    /*
     * Close the file.
     */  
    ret = H5Fclose(file);
#endif
    return ;
}
