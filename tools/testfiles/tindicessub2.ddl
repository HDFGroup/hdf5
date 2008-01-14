#############################
Expected output for 'h5dump -d 2d -s 1,2 -c 2,3 -k 2,2 -S 2,1 taindices.h5'
#############################
HDF5 "taindices.h5" {
DATASET "2d" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
   SUBSET {
      START ( 1, 2 );
      STRIDE ( 2, 1 );
      COUNT ( 2, 3 );
      BLOCK ( 2, 2 );
      DATA {
      (1,2): 12, 13, 14, 15, 16, 17,
      (3,2):  32, 33, 34, 35, 36, 37,
      (5,2):  52, 53, 54, 55, 56, 57,
      (7,2):  72, 73, 74, 75, 76, 77
      }
   }
}
}
