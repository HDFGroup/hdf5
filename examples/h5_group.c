#include "hdf5.h"


#define FILE    "DIR.h5"
#define RANK    2

main()
{

   hid_t    file, dir;
   hid_t    dataset, dataspace;

   herr_t   status;
   size_t   dims[2];

/*
 * Create a file.
 */
file = H5Fcreate(FILE, H5ACC_OVERWRITE, H5C_DEFAULT, H5C_DEFAULT);

/*
 * Create two groups in a file.
 */
dir = H5Gcreate(file, "/IntData", 0);
status = H5Gclose(dir);

dir = H5Gcreate(file,"/FloatData", 0);
status = H5Gclose(dir);

/*
 * Create dataset in the /IntData group by specifying full path.
 */
dims[0] = 2;
dims[1] = 3;
dataspace = H5Pcreate_simple(RANK, dims, NULL);
dataset = H5Dcreate(file, "/IntData/IntArray", H5T_NATIVE_INT, dataspace, H5C_DEFAULT); 

H5Pclose(dataspace);
H5Dclose(dataset);

/*
 * Set current group to /IntData and attach to the dataset IntArray.
 */

status = H5Gset (file, "/IntData");
dataset = H5Dopen(file, "IntArray");
if (dataset > 0) printf("IntArray dataset in /IntData group is found\n"); 
H5Dclose(dataset);

/*
 * Set current group to /FloatData.
 */
status = H5Gset (file, "/FloatData");

/* 
 * Create two datasets
 */

dims[0] = 5;
dims[1] = 10;
dataspace = H5Pcreate_simple(RANK, dims, NULL);
dataset = H5Dcreate(file, "FloatArray", H5T_NATIVE_FLOAT, dataspace, H5C_DEFAULT); 

H5Pclose(dataspace);
H5Dclose(dataset);

dims[0] = 4;
dims[1] = 6;
dataspace = H5Pcreate_simple(RANK, dims, NULL);
dataset = H5Dcreate(file, "DoubleArray", H5T_NATIVE_DOUBLE, dataspace, H5C_DEFAULT); 

H5Pclose(dataspace);
H5Dclose(dataset);

/* 
 * Attach to /FloatData/DoubleArray dataset
 */

dataset = H5Dopen(file, "/FloatData/DoubleArray");
if (dataset > 0) printf("/FloatData/DoubleArray dataset is found\n"); 
H5Dclose(dataset);
H5Fclose(file);

}
