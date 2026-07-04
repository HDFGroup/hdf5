HDF5 "exlite.h5" {
GROUP "/" {
   DATASET "dset1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): 1, 2, 3, 4
      }
   }
}
}
