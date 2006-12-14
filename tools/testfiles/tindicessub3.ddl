#############################
Expected output for 'h5dump -d 3d -s 0,1,3 -c 1,6,4 taindices.h5'
#############################
HDF5 "taindices.h5" {
DATASET "3d" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 2, 10, 10 ) / ( 2, 10, 10 ) }
   SUBSET {
      START ( 0, 1, 3 );
      STRIDE ( 1, 1, 1 );
      COUNT ( 1, 6, 4 );
      BLOCK ( 1, 1, 1 );
      DATA {
      (0,1,3): 13, 23, 33, 43, 53, 63,
      (0,1,4):  14, 24, 34, 44, 54, 64,
      (0,1,5):  15, 25, 35, 45, 55, 65,
      (0,1,6):  16, 26, 36, 46, 56, 66
      }
   }
}
}
