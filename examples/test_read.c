
/* This program finds two datasets in HDF5 file created by test_write.c
   program  */

#include "hdf5.h"    

/* Define datasets names */ 


#define DATA1_NAME  "ShortIntegers" 
#define DATA2_NAME  "DoubleFloats"

main(void)

{

     hid_t   file_id;        /* HDF5 file ID */

     hid_t   t1_id;          /* Type ID for the first dataset */
     hid_t   d1_id;          /* Dimensionality ID for the first dataset */
     hid_t   data1_id;       /* Data ID for the first dataset */
     hid_t   data1_type;     /* Datatype of the first dataset */
     uint8   data1_len;      /* Length in bytes for data1_type */
     uint8   data1_arch;     /* Architecture format data1_type is in */
     uint32  data1_rank;     /* Rank of the first dataset */
 
     hid_t   t2_id;          /* Type ID for the second dataset */
     hid_t   d2_id;          /* Dimensionality ID for the second dataset */
     hid_t   data2_id;       /* Data ID for the second dataset */
     hid_t   data2_type;     /* Datatype of the first dataset */
     uint8   data2_len;      /* Length in bytes for data2_type */
     uint8   data2_arch;     /* Architecture format data2_type is in */
     uint32  data2_rank;     /* Rank of the second dataset */


     herr_t   ret;
     hbool_t  ret_file;

/* Check if we have HDF5 file */

    ret_file = H5Fis_hdf5("HDF5_file");
    if(ret_file == BTRUE) 
    printf("Is HDF5? yes\n");

/* Open HDF5 file */

     file_id = H5Fopen("HDF5_file", 0, 0);
     

/* Find first dataset and return its dataset ID*/

     data1_id = H5Mfind_name(file_id,H5_DATASET,DATA1_NAME);

/* Return datatype and dimensionality IDs for the found dataset */

     ret = H5Dget_info(data1_id, &t1_id, &d1_id);

/* Get type, base length and architecture for the first data set */

     ret = H5Tget_type(t1_id, &data1_type, &data1_len, &data1_arch);
     printf("Base length and architecture for the first data set\n");
     printf("%u %u \n",  data1_len, data1_arch);
 
/*  Get rank for the first data set */

     data1_rank = H5Pget_lrank(d1_id);
     printf("data1_rank  %u\n", data1_rank);

/* Find second dataset and return its dataset ID*/

     data2_id = H5Mfind_name(file_id,H5_DATASET,DATA2_NAME);  

/* Return datatype and dimensionality IDs for the found dataset */

     ret = H5Dget_info(data2_id, &t2_id, &d2_id);
    
/* Get type, base length and architecture for the second data set */

     ret = H5Tget_type(t2_id, &data2_type, &data2_len, &data2_arch);
     printf(" Base length and architecture for the second data set\n");
     printf("%i %i \n", data2_len, data2_arch);

/*  Get rank for the second data set */

     data2_rank = H5Pget_lrank(d2_id);
     printf("data2_rank  %u\n", data2_rank);
     ret = H5Mrelease(t1_id);
     ret = H5Mrelease(d1_id);
     ret = H5Mrelease(data1_id);

     ret = H5Mrelease(t2_id);
     ret = H5Mrelease(d2_id);
     ret = H5Mrelease(data2_id);

     H5Fclose(file_id);


}
