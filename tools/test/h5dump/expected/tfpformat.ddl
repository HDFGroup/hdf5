HDF5 "tfpformat.h5" {
GROUP "/" {
   DATASET "double" {
      DATATYPE  H5T_IEEE_F64LE
      DATASPACE  SIMPLE { ( 6 ) / ( 6 ) }
      DATA {
      (0): -0.1234567,
      (1): 0.1234567,
      (2): 0.0000000,
      (3): 0.0000000,
      (4): 0.0000000,
      (5): 0.0000000
      }
   }
   DATASET "float" {
      DATATYPE  H5T_IEEE_F32LE
      DATASPACE  SIMPLE { ( 6 ) / ( 6 ) }
      DATA {
      (0): -0.1234567,
      (1): 0.1234567,
      (2): 0.0000000,
      (3): 0.0000000,
      (4): 0.0000000,
      (5): 0.0000000
      }
   }
}
}
