#############################
Expected output for 'h5dump -d 3d -s 0,1,2 -c 1,2,3 -k 1,1,1 -S 1,2,1 taindices.h5'
#############################
HDF5 "taindices.h5" {
DATASET "3d" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 2, 10, 10 ) / ( 2, 10, 10 ) }
   SUBSET {
      START ( 0, 1, 2 );
      STRIDE ( 1, 2, 1 );
      COUNT ( 1, 2, 3 );
      BLOCK ( 1, 1, 1 );
      DATA {
      (0,1,2): 12, 13, 14,
      (0,3,2):  32, 33, 34
      }
   }
}
}
