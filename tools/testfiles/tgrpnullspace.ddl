HDF5 "tgrpnullspace.h5" {
GROUP "/" {
   GROUP "g1" {
      ATTRIBUTE "attr" {
         DATATYPE  H5T_STD_U32LE
         DATASPACE  NULL
         DATA {
         }
      }
      DATASET "dset" {
         DATATYPE  H5T_STD_I32BE
         DATASPACE  NULL
         STORAGE_LAYOUT {
            CONTIGUOUS
            SIZE 0
            OFFSET 18446744073709551615
         }
         FILTERS {
            NONE
         }
         FILLVALUE {
            FILL_TIME H5D_FILL_TIME_IFSET
            VALUE  H5D_FILL_VALUE_DEFAULT
         }
         ALLOCATION_TIME {
            H5D_ALLOC_TIME_LATE
         }
         DATA {
         }
      }
   }
}
}
