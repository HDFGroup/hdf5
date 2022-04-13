HDF5 "trefer_grp.h5" {
GROUP "/" {
   DATASET "dset" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF }
      DATASPACE  SCALAR
      DATA {
         GROUP "trefer_grp.h5/group"
      }
   }
   GROUP "group" {
      GROUP "group3" {
      }
   }
}
}
