HDF5 "tarray1.h5" {
DATASET "Dataset1" {
   DATATYPE  H5T_ARRAY { [4] H5T_STD_I32LE }
   DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
}
}
HDF5 "tarray2.h5" {
DATASET "Dataset1" {
   DATATYPE  H5T_ARRAY { [3][4][5] H5T_STD_I32LE }
   DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
}
}
HDF5 "tarray3.h5" {
DATASET "Dataset1" {
   DATATYPE  H5T_ARRAY { [4] H5T_ARRAY { [6][3] H5T_STD_I32LE } }
   DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
}
}
HDF5 "tarray4.h5" {
DATASET "Dataset1" {
   DATATYPE  H5T_ARRAY { [4] H5T_COMPOUND {
      H5T_STD_I32LE "i";
      H5T_IEEE_F32LE "f";
   } }
   DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
}
}
HDF5 "tarray5.h5" {
DATASET "Dataset1" {
   DATATYPE  H5T_ARRAY { [4] H5T_COMPOUND {
      H5T_STD_I32LE "i";
      H5T_ARRAY { [4] H5T_IEEE_F32LE } "f";
   } }
   DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
}
}
HDF5 "tarray6.h5" {
DATASET "Dataset1" {
   DATATYPE  H5T_ARRAY { [4] H5T_VLEN { H5T_STD_U32LE} }
   DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
}
}
HDF5 "tarray7.h5" {
DATASET "Dataset1" {
   DATATYPE  H5T_ARRAY { [4] H5T_VLEN { H5T_ARRAY { [4] H5T_STD_U32LE }} }
   DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
}
}
HDF5 "tarray8.h5" {
}
