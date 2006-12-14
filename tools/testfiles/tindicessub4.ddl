#############################
Expected output for 'h5dump -d 4d -s 0,0,1,3 -c 1,1,1,1 taindices.h5'
#############################
HDF5 "taindices.h5" {
DATASET "4d" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 2, 2, 10, 10 ) / ( 2, 2, 10, 10 ) }
   SUBSET {
      START ( 0, 0, 1, 3 );
      STRIDE ( 1, 1, 1, 1 );
      COUNT ( 1, 1, 1, 1 );
      BLOCK ( 1, 1, 1, 1 );
      DATA {
      (0,0,1,3): 13
      }
   }
}
}
