HDF5 "tints4dims.h5" {
DATASET "FourDimInts" {
   DATATYPE  H5T_STD_U32LE
   DATASPACE  SIMPLE { ( 4, 6, 8, 10 ) / ( 4, 6, 8, 10 ) }
   SUBSET {
      START ( 0, 0, 0, 0 );
      STRIDE ( 1, 1, 1, 1 );
      COUNT ( 2, 2, 2, 2 );
      BLOCK ( 1, 1, 1, 1 );
      DATA {
      (0,0,0,0): 0, 1,
      (0,0,1,0): 10, 11
      (0,1,0,0): 80, 81,
      (0,1,1,0): 90, 91
      (1,0,0,0): 480, 481,
      (1,0,1,0): 490, 491
      (1,1,0,0): 560, 561,
      (1,1,1,0): 570, 571
      }
   }
}
}
