/*  
 *  This example writes data to HDF5 file.
 *  Data conversion is performed during write operation.  
 */
 
#include "hdf5.h"

#define FILE        "SDS.h5"
#define DATASETNAME "IntArray" 
#define NX     5                      /* dataset dimensions */
#define NY     6
#define RANK   2

main ()
{
   hid_t       file, dataset;         /* file and dataset handles */
   hid_t       datatype, dataspace;   /* handles */
   size_t      dimsf[2];              /* dataset dimensions */
   herr_t      status;                             
   int         data[NX][NY];          /* data to write */
   int         i, j;

/* 
 * Data  and output buffer initialization. 
 */

for (j = 0; j < NX; j++) {
    for (i = 0; i < NY; i++)
        data[j][i] = i + j;
}     
                                       /*  0 1 2 3 4 5 
                                           1 2 3 4 5 6
                                           2 3 4 5 6 7
                                           3 4 5 6 7 8
                                           4 5 6 7 8 9   */

/*
 * Create a new file using H5ACC_OVERWRITE access,
 * default file creation properties, and default file
 * access properties.
 */
file = H5Fcreate(FILE, H5ACC_DEFAULT, H5C_DEFAULT, H5C_DEFAULT);

/*
 * Describe the size of the array and create the data space for fixed
 * size dataset. 
 */
dimsf[0] = NX;
dimsf[1] = NY;
dataspace = H5Pcreate_simple(RANK, dimsf, NULL); 

/* 
 * Define datatype for the data in the file.
 * We will store liitle endian INT32 numbers.
 */
datatype = H5Tcopy(H5T_NATIVE_INT32);
status = H5Tset_order(datatype, H5T_ORDER_LE);
/*
 * Create a new dataset within the file using defined dataspace and
 * datatype and default dataset creation properties.
 */
dataset = H5Dcreate(file, DATASETNAME, datatype, dataspace,
                    H5C_DEFAULT);

/*
 * Write the data to the dataset using default transfer properties.
 */
status = H5Dwrite(dataset, H5T_NATIVE_INT, H5P_ALL, H5P_ALL,
                  H5C_DEFAULT, data);

/*
 * Close/release resources.
 */
H5Dclose(dataset);
H5Pclose(dataspace);
H5Fclose(file);
 
}     
