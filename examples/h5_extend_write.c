/*  
 *   This example shows how to work with extendable dataset.
 *   In the current version of the library dataset MUST be
 *   chunked.
 *   
 */
 
#include "hdf5.h"

#define FILE        "SDSextendable.h5"
#define DATASETNAME "ExtendableArray" 
#define RANK         2
#define NX     10
#define NY     5 

main ()
{
   hid_t       file;                          /* handles */
   hid_t       datatype, dataspace, dataset;  
   hid_t       filespace;                   
   hid_t       cparms;                     
   size_t      dims[2]  = { 3, 3};            /* dataset dimensions
                                                 at the creation time  */ 
   size_t      dims1[2] = { 3, 3};            /* data1 dimensions */ 
   size_t      dims2[2] = { 7, 1};            /* data2 dimensions */  
   size_t      dims3[2] = { 2, 2};            /* data3 dimensions */ 

   size_t      maxdims[2] = {H5P_UNLIMITED, H5P_UNLIMITED};
   size_t      chunk_dims[2] ={2, 5};
   size_t      size[2];
   int         offset[2];

   herr_t      status;                             

   int         data1[3][3] = { 1, 1, 1,       /* data to write */
                               1, 1, 1,
                               1, 1, 1 };      

   int         data2[7]    = { 2, 2, 2, 2, 2, 2, 2};

   int         data3[2][2] = { 3, 3,
                               3, 3};

/*
 * Create the data space with ulimited dimensions. 
 */
dataspace = H5Pcreate_simple(RANK, dims, maxdims); 

/*
 * Create a new file. If file exists its contents will be overwritten.
 */
file = H5Fcreate(FILE, H5ACC_OVERWRITE, H5C_DEFAULT, H5C_DEFAULT);

/* 
 * Modify dataset creation properties, i.e. enable chunking.
 */
cparms = H5Ccreate (H5C_DATASET_CREATE);
status = H5Cset_chunk( cparms, RANK, chunk_dims);

/*
 * Create a new dataset within the file using cparms
 * creation properties.
 */
dataset = H5Dcreate(file, DATASETNAME, H5T_NATIVE_INT, dataspace,
                 cparms);

/*
 * Extend the dataset. This call assures that dataset is at least 3 x 3.
 */
size[0]   = 3; 
size[1]   = 3; 
status = H5Dextend (dataset, size);

/*
 * Select a hyperslab.
 */
filespace = H5Dget_space (dataset);
offset[0] = 0;
offset[1] = 0;
status = H5Pset_hyperslab(filespace, offset, dims1, NULL);  

/*
 * Write the data to the hyperslab.
 */
status = H5Dwrite(dataset, H5T_NATIVE_INT, dataspace, filespace,
                  H5C_DEFAULT, data1);

/*
 * Extend the dataset. Dataset becomes 7 x 3.
 */
dims[0]   = dims1[0] + dims2[0];
size[0]   = dims[0];  
size[1]   = dims[1]; 
status = H5Dextend (dataset, size);

/*
 * Select a hyperslab.
 */
filespace = H5Dget_space (dataset);
offset[0] = 3;
offset[1] = 0;
status = H5Pset_hyperslab(filespace, offset, dims2, NULL);  

/*
 * Define memory space
 */
dataspace = H5Pcreate_simple(RANK, dims2, NULL); 

/*
 * Write the data to the hyperslab.
 */
status = H5Dwrite(dataset, H5T_NATIVE_INT, dataspace, filespace,
                  H5C_DEFAULT, data2);

/*
 * Extend the dataset. Dataset becomes 7 x 5.
 */
dims[1]   = dims1[1] + dims3[1];
size[0]   = dims[0];  
size[1]   = dims[1]; 
status = H5Dextend (dataset, size);

/*
 * Select a hyperslab
 */
filespace = H5Dget_space (dataset);
offset[0] = 0;
offset[1] = 3;
status = H5Pset_hyperslab(filespace, offset, dims3, NULL);  

/*
 * Define memory space.
 */
dataspace = H5Pcreate_simple(RANK, dims3, NULL); 

/*
 * Write the data to the hyperslab.
 */
status = H5Dwrite(dataset, H5T_NATIVE_INT, dataspace, filespace,
                  H5C_DEFAULT, data3);

/*
 * Close/release resources.
 */
H5Dclose(dataset);
H5Pclose(dataspace);
H5Pclose(filespace);
H5Fclose(file);

}     
