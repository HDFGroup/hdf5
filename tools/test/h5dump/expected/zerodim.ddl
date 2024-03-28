HDF5 "zerodim.h5" {
GROUP "/" {
   ATTRIBUTE "attr of 0 dimension size" {
      DATATYPE  H5T_STD_U32LE
      DATASPACE  SIMPLE { ( 0, 0 ) / ( 0, 0 ) }
      DATA {
      }
   }
   DATASET "dset of 0 dimension size" {
      DATATYPE  H5T_STD_I32BE
      DATASPACE  SIMPLE { ( 0, 0 ) / ( 0, 0 ) }
      DATA {
      }
   }
}
}
