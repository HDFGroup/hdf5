HDF5 "h5ex_vds.h5" {
GROUP "/" {
   DATASET "VDS" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 4, 6 ) / ( 4, 6 ) }
      DATA {
      (0,0): 1, 1, 1, 1, 1, 1,
      (1,0): 2, 2, 2, 2, 2, 2,
      (2,0): 3, 3, 3, 3, 3, 3,
      (3,0): -1, -1, -1, -1, -1, -1
      }
   }
}
}
