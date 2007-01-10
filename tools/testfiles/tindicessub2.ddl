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
      (1,3): 13, 14, 15, 16,
      (2,3):  23, 24, 25, 26,
      (3,3):  33, 34, 35, 36,
      (4,3):  43, 44, 45, 46,
      (5,3):  53, 54, 55, 56,
      (6,3):  63, 64, 65, 66
      }
   }
}
}
