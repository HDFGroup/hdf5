#############################
Expected output for 'h5dump -d 2d -s 1,3 -c 6,4 taindices.h5'
#############################
HDF5 "taindices.h5" {
DATASET "2d" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
   SUBSET {
      START ( 1, 3 );
      STRIDE ( 1, 1 );
      COUNT ( 6, 4 );
      BLOCK ( 1, 1 );
      DATA {
      (1,3): 13, 23, 33, 43, 53, 63,
      (1,4):  14, 24, 34, 44, 54, 64,
      (1,5):  15, 25, 35, 45, 55, 65,
      (1,6):  16, 26, 36, 46, 56, 66
      }
   }
}
}
