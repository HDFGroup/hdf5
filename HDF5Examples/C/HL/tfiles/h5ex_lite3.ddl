HDF5 "h5ex_lite3.h5" {
GROUP "/" {
   DATASET "dset" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 5 ) / ( 5 ) }
      DATA {
      (0): 0, 0, 0, 0, 0
      }
      ATTRIBUTE "attr1" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 5 ) / ( 5 ) }
         DATA {
         (0): 1, 2, 3, 4, 5
         }
      }
   }
}
}
