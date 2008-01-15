#############################
Expected output for 'h5dump -d 2d -s 1,2 -c 2,3 -k 1,1 -S 2,1 taindices.h5'
#############################
HDF5 "taindices.h5" {
DATASET "2d" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
   SUBSET {
      START ( 1, 2 );
      STRIDE ( 2, 1 );
      COUNT ( 2, 3 );
      BLOCK ( 1, 1 );
      DATA {
      (1,2): 12, 13, 14,
      (3,2):  32, 33, 34
      }
   }
}
}
