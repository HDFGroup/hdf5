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
   hid_t       file;                        /* handles */
   hid_t       datatype, dataset;  
   hid_t       filespace;                   
   hid_t       memspace;                  
   hid_t       cparms;                   
   H5T_class_t class;                       /* data type class */
   size_t      elem_size;                   /* size of the data element
                                               stored in file */ 
   size_t      dims[2];                     /* dataset and chunk dimensions */ 
   size_t      chunk_dims[2];

   size_t      size[2];
   size_t      count[2];
   int         offset[2];

   herr_t      status, status_n;                             

   int         data_out[NX][NY];  /* buffer for dataset to be read */
   int         chunk_out[2][5];   /* buffer for chunk to be read */
   int         i, j, rank, rank_chunk;

 
/*
 * Open the file and the dataset.
 */
file = H5Fopen(FILE, H5ACC_DEFAULT, H5C_DEFAULT);
dataset = H5Dopen(file, DATASETNAME);
 
/*
 * Get dataset rank and dimension.
 */
 
filespace = H5Dget_space(dataset);    /* Get filespace handle first. */
rank      = H5Pget_ndims(filespace);
status_n  = H5Pget_dims(filespace, dims);
printf("dataset rank %d, dimensions %d x %d \n", rank, dims[0], dims[1]);

/*
 * Get creation properties.
 */
cparms = H5Dget_create_parms(dataset); /* Get properties handle first. */

/*
 * Get chunking information: rank and dimensions
 */
rank_chunk = H5Cget_chunk(cparms, 2, chunk_dims);
printf("chunk rank %d, dimensions %d x %d \n", rank_chunk,
        chunk_dims[0], chunk_dims[1]);
 
 
/*
 * Define the memory filespace to read dataset.
 */
memspace = H5Pcreate_simple(RANK,dims,NULL);
 
/*
 * Read dataset back and display.
 */
status = H5Dread(dataset, H5T_NATIVE_INT, memspace, filespace,
                 H5C_DEFAULT, data_out);
    printf("\n");
    printf("Dataset: \n");
for (j = 0; j < dims[0]; j++) {
    for (i = 0; i < dims[1]; i++) printf("%d ", data_out[j][i]);
    printf("\n");
}     

/*
            dataset rank 2, dimensions 10 x 5 
            chunk rank 2, dimensions 2 x 5 

            Dataset:
            1 1 1 3 3 
            1 1 1 3 3 
            1 1 1 0 0 
            2 0 0 0 0 
            2 0 0 0 0 
            2 0 0 0 0 
            2 0 0 0 0 
            2 0 0 0 0 
            2 0 0 0 0 
            2 0 0 0 0 
*/

/*
 * Define the memory filespace to read a chunk.
 */
memspace = H5Pcreate_simple(rank_chunk,chunk_dims,NULL);

/*
 * Define chunk in the file (hyperslab) to read.
 */
offset[0] = 2;
offset[1] = 0;
count[0]  = chunk_dims[0];
count[1]  = chunk_dims[1];
status = H5Pset_hyperslab(filespace, offset, count, NULL);

/*
 * Read chunk back and display.
 */
status = H5Dread(dataset, H5T_NATIVE_INT, memspace, filespace,
                 H5C_DEFAULT, chunk_out);
    printf("\n");
    printf("Chunk: \n");
for (j = 0; j < chunk_dims[0]; j++) {
    for (i = 0; i < chunk_dims[1]; i++) printf("%d ", chunk_out[j][i]);
    printf("\n");
}     
/*
         Chunk: 
         1 1 1 0 0 
         2 0 0 0 0 
*/

/*
 * Close/release resources.
 */
H5Dclose(dataset);
H5Pclose(filespace);
H5Pclose(memspace);
H5Fclose(file);

}     
