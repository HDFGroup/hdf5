#############################
Expected output for '../h5dump -header -g /g1/g1.1 -a attr2 tall.h5'
#############################
HDF5 "tall.h5" {
GROUP "/g1/g1.1" {
   DATASET "dset1.1.1" {
      DATATYPE { "H5T_STD_I32BE" }
      DATASPACE { ARRAY ( 10, 10 ) ( 10, 10 ) }
      ATTRIBUTE "attr1" {
         DATATYPE { "H5T_STD_I8LE" }
         DATASPACE { ARRAY ( 27 ) ( 27 ) }
      }
      ATTRIBUTE "attr2" {
         DATATYPE { "H5T_STD_I8LE" }
         DATASPACE { ARRAY ( 27 ) ( 27 ) }
      }
   }
   DATASET "dset1.1.2" {
      DATATYPE { "H5T_STD_I32BE" }
      DATASPACE { ARRAY ( 20 ) ( 20 ) }
   }
}
ATTRIBUTE "attr2" {
   DATATYPE { "H5T_STD_I32BE" }
   DATASPACE { ARRAY ( 2, 2 ) ( 2, 2 ) }
}
}
