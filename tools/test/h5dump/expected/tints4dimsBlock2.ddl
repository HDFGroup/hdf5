HDF5 "tints4dims.h5" {
DATASET "FourDimInts" {
   DATATYPE  H5T_STD_U32LE
   DATASPACE  SIMPLE { ( 4, 6, 8, 10 ) / ( 4, 6, 8, 10 ) }
   SUBSET {
      START ( 0, 0, 0, 0 );
      STRIDE ( 1, 1, 1, 1 );
      COUNT ( 1, 1, 1, 1 );
      BLOCK ( 2, 2, 2, 2 );
      DATA {
      (0,0,0,0): 0, 1, 80, 81, 480, 481, 560, 561,
      (0,0,1,0): 10, 11, 90, 91, 490, 491, 570, 571
      }
   }
}
}
