#############################
Expected output for '../h5dump tattr.h5'
#############################
HDF5 "tattr.h5" {
GROUP "/" {
   ATTRIBUTE "attr1" {
      DATATYPE { "H5T_STD_I8LE" }
      DATASPACE { ARRAY ( 24 ) ( 24 ) }
      DATA {
         "attribute of root group\000"
      }
   }
   ATTRIBUTE "attr2" {
      DATATYPE { "H5T_STD_I32BE" }
      DATASPACE { ARRAY ( 10 ) ( 10 ) }
      DATA {
         1, 2, 3, 4, 5, 6, 7, 8, 9, 10
      }
   }
   ATTRIBUTE "attr3" {
      DATATYPE { "H5T_IEEE_F64BE" }
      DATASPACE { ARRAY ( 10 ) ( 10 ) }
      DATA {
         0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
      }
   }
}
}
