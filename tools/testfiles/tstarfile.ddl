HDF5 "tarray1_big.h5" {
DATASET "Dataset1" {
   DATATYPE  H5T_ARRAY { [1000] H5T_STD_I32LE }
   DATASPACE  SIMPLE { ( 2000 ) / ( 2000 ) }
}
}
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
HDF5-DIAG: Error detected in HDF5 (version (number)) thread (IDs):
  #000: (file name) line (number) in H5Dopen2(): not found
    major: Dataset
    minor: Object not found
  #001: (file name) line (number) in H5G_loc_find(): can't find object
    major: Symbol table
    minor: Object not found
  #002: (file name) line (number) in H5G_traverse(): internal path traversal failed
    major: Symbol table
    minor: Object not found
  #003: (file name) line (number) in H5G_traverse_real(): traversal operator failed
    major: Symbol table
    minor: Callback failed
  #004: (file name) line (number) in H5G_loc_find_cb(): object 'Dataset1' doesn't exist
    major: Symbol table
    minor: Object not found
HDF5-DIAG: Error detected in HDF5 (version (number)) thread (IDs):
  #000: (file name) line (number) in H5Lget_info(): unable to get link info
    major: Symbol table
    minor: Object not found
  #001: (file name) line (number) in H5L_get_info(): name doesn't exist
    major: Symbol table
    minor: Object already exists
  #002: (file name) line (number) in H5G_traverse(): internal path traversal failed
    major: Symbol table
    minor: Object not found
  #003: (file name) line (number) in H5G_traverse_real(): traversal operator failed
    major: Symbol table
    minor: Callback failed
  #004: (file name) line (number) in H5L_get_info_cb(): name doesn't exist
    major: Symbol table
    minor: Object not found
h5dump error: unable to get link info from "Dataset1"
