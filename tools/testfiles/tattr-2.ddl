#############################
Expected output for '../h5dump -a attr1 attr3 tattr.h5'
#############################
HDF5 "tattr.h5" {
ATTRIBUTE "attr1" {
   DATATYPE { "H5T_STD_I8LE" }
   DATASPACE { ARRAY ( 24 ) ( 24 ) }
   DATA {"attribute of root group\000"}
}
ATTRIBUTE "attr3" {
   DATATYPE { "H5T_IEEE_F64BE" }
   DATASPACE { ARRAY ( 10 ) ( 10 ) }
   DATA {0.0001,0.0002,0.0003,0.0004,0.0005,0.0006,0.0007,0.0008,0.0009,0.001}
}
}
