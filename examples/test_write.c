
/* This program creates two datasets and stores them in HDF5 file */

#include "hdf5.h"    

/* Define data type, dimensionlity, architecture and name for the first
   dataset  */


#define DATA1_BASE  H5T_INT
#define DATA1_LEN   2
#define DATA1_RANK  2
#define DATA1_DIM1  10
#define DATA1_DIM2  5
#define DATA1_ARCH  H5T_BIGENDIAN
#define DATA1_NAME  "ShortIntegers" 

/* Define data type, dimensionality, architecture and name for the second 
   dataset  */


#define DATA2_BASE  H5T_FLOAT
#define DATA2_LEN   8 
#define DATA2_RANK  1 
#define DATA2_DIM1  8 
#define DATA2_ARCH  H5T_LITTLEENDIAN
#define DATA2_NAME  "DoubleFloats"

main(void)

{

     hid_t   file1_id, file2_id;        /* HDF5 file IDs */

     hid_t   t1_id;          /* Type ID for the first dataset */
     hid_t   d1_id;          /* Dimensionality ID for the first dataset */
     hid_t   data1_id;       /* Data ID for the first dataset */

     uint32  dims1[DATA1_RANK] = {DATA1_DIM1, DATA1_DIM2};
     int16   data1[DATA1_DIM1][DATA1_DIM2];

 
     hid_t   t2_id;          /* Type ID for the second dataset */
     hid_t   d2_id;          /* Dimensionality ID for the second dataset */
     hid_t   data2_id;       /* Data ID for the second dataset */

     uint32  dims2[DATA2_RANK] = {DATA2_DIM1};
     float64 data2[DATA2_DIM1];

     herr_t   ret;

     intn    k,l;

/* Initialize datasets  */

     for (k=0; k<DATA1_DIM1; k++) {
          for (l=0; l<DATA1_DIM2; l++) {
               data1[k][l] = (k+l);
          }
     } 
        
 
     for (k=0; k<DATA2_DIM1; k++) {
          data2[k] = (double) (k);
     } 

/* Create HDF5 file */

     file1_id = H5Fcreate("HDF5_file", H5ACC_OVERWRITE,0,0);

/* Create and initialize datatype object for the first dataset */

     t1_id = H5Mcreate(file1_id,H5_DATATYPE,"Short Integer data type");
     ret = H5Tset_type(t1_id, DATA1_BASE, DATA1_LEN, DATA1_ARCH);

/* Create and initialize dimensionality object for the first dataset */

     d1_id = H5Mcreate(file1_id, H5_DATASPACE, "2D");
     ret = H5Pset_space(d1_id,DATA1_RANK, dims1);

/* Create and initialize first dataset object */

     data1_id = H5Mcreate(file1_id, H5_DATASET, DATA1_NAME);
     ret = H5Dset_info (data1_id, t1_id, d1_id);

/* Write first dataset into HDF5_file */

     ret = H5Dwrite(data1_id, H5P_ALL, data1);

/* Release all ID's and close the file */

     ret = H5Mrelease(t1_id);
     ret = H5Mrelease(d1_id);
     ret = H5Mrelease(data1_id);

     H5Fclose(file1_id); 

/* Reopen the file  to add second dataset */

     file2_id = H5Fopen("HDF5_file", H5ACC_WRITE, 0);  

 
/* Create and initialize datatype object for the second dataset */

     t2_id = H5Mcreate(file2_id,H5_DATATYPE,"Double float data type");
     ret = H5Tset_type(t2_id, DATA2_BASE, DATA2_LEN, DATA2_ARCH);

/* Create and initialize dimensionality object for the second dataset */

     d2_id = H5Mcreate(file2_id, H5_DATASPACE, "1D");
     ret = H5Pset_space(d2_id,DATA2_RANK, dims2);

/* Create and initialize second dataset object */

     data2_id = H5Mcreate(file2_id, H5_DATASET, DATA2_NAME);
     ret = H5Dset_info (data2_id, t2_id, d2_id);

/* Write second dataset into HDF5_file */

     ret = H5Dwrite(data2_id, H5P_ALL, data2);

/* Release all ID's and close the file */

     ret = H5Mrelease(t2_id);
     ret = H5Mrelease(d2_id);
     ret = H5Mrelease(data2_id);

     ret = H5Fclose(file2_id);

}
