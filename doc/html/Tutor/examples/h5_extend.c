/**************************************************************  
 *
 *   This example shows how to work with extendible datasets.
 *   In the current version of the library a dataset MUST be
 *   chunked in order to be extendible.  
 *
 *   This example is derived from the h5_extend_write.c and 
 *   h5_read_chunk.c examples that are in the "Introduction 
 *   to HDF5".
 *   
 *************************************************************/
 
#include "hdf5.h"

#define FILE        "ext.h5"
#define DATASETNAME "ExtendibleArray" 
#define RANK         2

int
main (void)
{
    hid_t       file;                          /* handles */
    hid_t       dataspace, dataset;  
    hid_t       filespace;                   
    hid_t       cparms;                     
    hid_t       memspace;

    hsize_t      dims[2]  = { 3, 3};           /* dataset dimensions			
                                                  at creation time */
    hsize_t      dims1[2] = { 3, 3};           /* data1 dimensions */ 
    hsize_t      dims2[2] = { 7, 1};           /* data2 dimensions */  

    hsize_t      maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t      size[2];
    hsize_t      offset[2];
    hsize_t      i,j;
    herr_t       status, status_n;                             
    int          data1[3][3] = { {1, 1, 1},      /* data to write */
                                 {1, 1, 1},
                                 {1, 1, 1} };      

    int          data2[7]    = { 2, 2, 2, 2, 2, 2, 2};

    /* Variables used in reading data back */
    hsize_t      chunk_dims[2] ={2, 5};
    hsize_t      chunk_dimsr[2];
    hsize_t      dimsr[2];
    int          data_out[10][3];
    int          rank, rank_chunk;

    /* Create the data space with unlimited dimensions. */
    dataspace = H5Screate_simple (RANK, dims, maxdims); 

    /* Create a new file. If file exists its contents will be overwritten. */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Modify dataset creation properties, i.e. enable chunking  */
    cparms = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_chunk ( cparms, RANK, chunk_dims);

    /* Create a new dataset within the file using cparms
       creation properties.  */
    dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, dataspace,
                         cparms);

    /* Extend the dataset. This call assures that dataset is 3 x 3.*/
    size[0]   = 3; 
    size[1]   = 3; 
    status = H5Dextend (dataset, size);

    /* Select a hyperslab  */
    filespace = H5Dget_space (dataset);
    offset[0] = 0;
    offset[1] = 0;
    status = H5Sselect_hyperslab (filespace, H5S_SELECT_SET, offset, NULL,
                                  dims1, NULL);  

    /* Write the data to the hyperslab  */
    status = H5Dwrite (dataset, H5T_NATIVE_INT, dataspace, filespace,
                       H5P_DEFAULT, data1);

    /* Extend the dataset. Dataset becomes 10 x 3  */
    dims[0]   = dims1[0] + dims2[0];
    size[0]   = dims[0];  
    size[1]   = dims[1]; 
    status = H5Dextend (dataset, size);

    /* Select a hyperslab  */
    filespace = H5Dget_space (dataset);
    offset[0] = 3;
    offset[1] = 0;
    status = H5Sselect_hyperslab (filespace, H5S_SELECT_SET, offset, NULL,
                                  dims2, NULL);  

    /* Define memory space */
    dataspace = H5Screate_simple (RANK, dims2, NULL); 

    /* Write the data to the hyperslab  */
    status = H5Dwrite (dataset, H5T_NATIVE_INT, dataspace, filespace,
                       H5P_DEFAULT, data2);

    /* Close resources */
    status = H5Dclose (dataset);
    status = H5Sclose (dataspace);
    status = H5Sclose (filespace);
    status = H5Fclose (file);

/****************************************************************
    Read the data back 
 ***************************************************************/

    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dataset = H5Dopen (file, DATASETNAME);
    filespace = H5Dget_space (dataset);
    rank = H5Sget_simple_extent_ndims (filespace);
    status_n = H5Sget_simple_extent_dims (filespace, dimsr, NULL);

    cparms = H5Dget_create_plist (dataset);
    if (H5D_CHUNKED == H5Pget_layout (cparms))
    {
       rank_chunk = H5Pget_chunk (cparms, 2, chunk_dimsr);
    }

    memspace = H5Screate_simple (rank,dimsr,NULL);
    status = H5Dread (dataset, H5T_NATIVE_INT, memspace, filespace,
                      H5P_DEFAULT, data_out);
    printf("\n");
    printf("Dataset: \n");
    for (j = 0; j < dimsr[0]; j++)
    {
       for (i = 0; i < dimsr[1]; i++)
           printf("%d ", data_out[j][i]);
       printf("\n");
    }

    status = H5Pclose (cparms);
    status = H5Dclose (dataset);
    status = H5Sclose (filespace);
    status = H5Sclose (memspace);
    status = H5Fclose (file);
}     
