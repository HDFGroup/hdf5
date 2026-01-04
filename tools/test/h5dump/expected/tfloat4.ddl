HDF5 "tfloat4.h5" {
DATASET "/DS4BITSE2M1" {
   DATATYPE  H5T_FLOAT_F4E2M1
   DATASPACE  SIMPLE { ( 8, 16 ) / ( 8, 16 ) }
   DATA {
   (0,0): inf, 0.5, 1, 1.5, 2, 3, 3, 3, inf, inf, inf, inf, inf, inf, inf,
   (0,15): inf,
   (1,0): inf, 0.5, 1, 1.5, 2, 3, 3, 3, inf, inf, inf, inf, inf, inf, inf,
   (1,15): inf,
   (2,0): inf, 0.5, 1, 1.5, 2, 3, 3, 3, inf, inf, inf, inf, inf, inf, inf,
   (2,15): inf,
   (3,0): inf, 0.5, 1, 1.5, 2, 3, 3, 3, inf, inf, inf, inf, inf, inf, inf,
   (3,15): inf,
   (4,0): inf, 0.5, 1.5, 2, 2, 3, 3, 3, inf, inf, inf, inf, inf, inf, inf,
   (4,15): inf,
   (5,0): inf, 0.5, 1.5, 2, 2, 3, 3, 3, inf, inf, inf, inf, inf, inf, inf,
   (5,15): inf,
   (6,0): inf, 0.5, 1.5, 2, 2, 3, 3, 3, inf, inf, inf, inf, inf, inf, inf,
   (6,15): inf,
   (7,0): inf, 0.5, 1.5, 2, 2, 3, 3, 3, inf, inf, inf, inf, inf, inf, inf,
   (7,15): inf
   }
}
DATASET "/DS4BITSE2M1_ALLVALS" {
   DATATYPE  H5T_FLOAT_F4E2M1
   DATASPACE  SIMPLE { ( 2, 8 ) / ( 2, 8 ) }
   SUBSET {
      START ( 0, 0 );
      STRIDE ( 1, 1 );
      COUNT ( 1, 1 );
      BLOCK ( 2, 6 );
      DATA {
      (0,0): 0, 0.5, 1, 1.5, 2, 3,
      (1,0): -0, -0.5, -1, -1.5, -2, -3
      }
   }
}
DATASET "/DS4BITSE2M1_ALLVALS_CONVERT" {
   DATATYPE  H5T_FLOAT_F4E2M1
   DATASPACE  SIMPLE { ( 2, 8 ) / ( 2, 8 ) }
   DATA {
   (0,0): 0, 0.5, 1, 1.5, 2, 3, inf, inf,
   (1,0): -0, -0.5, -1, -1.5, -2, -3, -inf, -inf
   }
}
}
