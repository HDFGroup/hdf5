HDF5 "taindices.h5" {
DATASET "1d" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 100 ) / ( 100 ) }
   SUBSET {
      START ( 1 );
      STRIDE ( 10 );
      COUNT ( 2 );
      BLOCK ( 3 );
      DATA {
      (1): 1, 2, 3, 11, 12, 13
      }
   }
}
}
