#############################
Expected output for 'h5dump -a attr1 attr3 tattr.h5'
#############################
HDF5 "tattr.h5" {
ATTRIBUTE "attr1" {
   DATATYPE { "H5T_STD_I8LE" }
   DATASPACE { ARRAY ( 24 ) ( 24 ) }
   DATA {
      "attribute of root group\000"
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
