#############################
Expected output for './h5dump tattr.h5'
#############################
HDF5 "tattr.h5" {
GROUP "/" {
   ATTRIBUTE "attr1" {
      DATATYPE { "H5T_NATIVE_CHAR" }
      DATASPACE { ARRAY ( 24 ) ( 24 ) }
      DATA {"attribute of root group\000"}
   }
   ATTRIBUTE "attr2" {
      DATATYPE { "H5T_NATIVE_INT" }
      DATASPACE { ARRAY ( 20 ) ( 20 ) }
      DATA {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20}
   }
   ATTRIBUTE "attr3" {
      DATATYPE { "H5T_NATIVE_DOUBLE" }
      DATASPACE { ARRAY ( 10 ) ( 10 ) }
      DATA {0.0001,0.0002,0.0003,0.0004,0.0005,0.0006,0.0007,0.0008,0.0009,0.001}
   }
}
}
