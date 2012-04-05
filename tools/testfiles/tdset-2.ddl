HDF5 "tdset.h5" {
DATASET "dset1" {
   DATATYPE  H5T_STD_I32BE
   DATASPACE  SIMPLE { ( 10, 20 ) / ( 10, 20 ) }
}
DATASET "/dset2" {
   DATATYPE  H5T_IEEE_F64BE
   DATASPACE  SIMPLE { ( 30, 20 ) / ( 30, 20 ) }
}
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
  #004: (file name) line (number) in H5G_loc_find_cb(): object 'dset3' doesn't exist
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
h5dump error: unable to get link info from "dset3"
