HDF5 "tintascii.h5" {
GROUP "/" {
   DATASET "dset" {
      DATATYPE  H5T_STD_U8LE
      DATASPACE  SIMPLE { ( 81 ) / ( 81 ) }
      DATA {
         "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123"
         "45678901234567\000"
      }
      ATTRIBUTE "attr" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SIMPLE { ( 81 ) / ( 81 ) }
         DATA {
            "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0"
            "12345678901234567\000"
         }
      }
   }
}
}
