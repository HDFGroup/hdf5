#############################
Expected output for 'h5dump -d 1d -s 3 -c 40 taindices.h5'
#############################
HDF5 "taindices.h5" {
DATASET "1d" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 100 ) / ( 100 ) }
   SUBSET {
      START ( 3 );
      STRIDE ( 1 );
      COUNT ( 40 );
      BLOCK ( 1 );
      DATA {
      (3): 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
      (21): 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
      (37): 37, 38, 39, 40, 41, 42
      }
   }
}
}
