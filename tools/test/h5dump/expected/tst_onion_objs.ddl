Using revision 3
HDF5 "tst_onion_objs.h5" {
GROUP "/" {
   ATTRIBUTE "file_attribute" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): 0, 0, 0, 0
      }
   }
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 4, 4 ) / ( H5S_UNLIMITED, H5S_UNLIMITED ) }
      DATA {
      (0,0): 0, 1, 2, 3,
      (1,0): 1, 2, 3, 4,
      (2,0): 2, 3, 4, 5,
      (3,0): 3, 4, 5, 6
      }
   }
}
}
