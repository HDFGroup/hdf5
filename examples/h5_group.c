/*
 * This example shows how to create groups within the file and    
 * datasets within the file and groups.
 */ 


#include "hdf5.h"


#define FILE    "DIR.h5"
#define RANK    2

main()
{

   hid_t    file, dir;
   hid_t    dataset, dataspace;

   herr_t   status;
   hsize_t  dims[2];
   hsize_t  size[1];

/*
 * Create a file.
 */
file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

/*
 * Create two groups in a file.
 */
dir = H5Gcreate(file, "/IntData", 0);
status = H5Gclose(dir);

dir = H5Gcreate(file,"/FloatData", 0);
status = H5Gclose(dir);

/* 
 * Create dataspace for the character string
 */
size[0] = 80;
dataspace = H5Screate_simple(1, size, NULL);

/*
 * Create dataset "String" in the root group.  
 */
dataset = H5Dcreate(file, "String", H5T_NATIVE_CHAR, dataspace, H5P_DEFAULT);
H5Dclose(dataset);

/*
 * Create dataset "String" in the /IntData group.  
 */
dataset = H5Dcreate(file, "/IntData/String", H5T_NATIVE_CHAR, dataspace,
                    H5P_DEFAULT);
H5Dclose(dataset);

/*
 * Create dataset "String" in the /FloatData group.  
 */
dataset = H5Dcreate(file, "/FloatData/String", H5T_NATIVE_CHAR, dataspace,
                    H5P_DEFAULT);
H5Sclose(dataspace);
H5Dclose(dataset);

/*
 * Create IntArray dataset in the /IntData group by specifying full path.
 */
dims[0] = 2;
dims[1] = 3;
dataspace = H5Screate_simple(RANK, dims, NULL);
dataset = H5Dcreate(file, "/IntData/IntArray", H5T_NATIVE_INT, dataspace,
                    H5P_DEFAULT); 
H5Sclose(dataspace);
H5Dclose(dataset);

/*
 * Set current group to /IntData and attach to the dataset String.
 */

status = H5Gset (file, "/IntData");
dataset = H5Dopen(file, "String");
if (dataset > 0) printf("String dataset in /IntData group is found\n"); 
H5Dclose(dataset);

/*
 * Set current group to /FloatData.
 */
status = H5Gset (file, "/FloatData");

/* 
 * Create two datasets FlatArray and DoubleArray.
 */

dims[0] = 5;
dims[1] = 10;
dataspace = H5Screate_simple(RANK, dims, NULL);
dataset = H5Dcreate(file, "FloatArray", H5T_NATIVE_FLOAT, dataspace, H5P_DEFAULT); 
H5Sclose(dataspace);
H5Dclose(dataset);

dims[0] = 4;
dims[1] = 6;
dataspace = H5Screate_simple(RANK, dims, NULL);
dataset = H5Dcreate(file, "DoubleArray", H5T_NATIVE_DOUBLE, dataspace,
                    H5P_DEFAULT); 
H5Sclose(dataspace);
H5Dclose(dataset);

/* 
 * Attach to /FloatData/String dataset.
 */

dataset = H5Dopen(file, "/FloatData/String");
if (dataset > 0) printf("/FloatData/String dataset is found\n"); 
H5Dclose(dataset);
H5Fclose(file);

}
