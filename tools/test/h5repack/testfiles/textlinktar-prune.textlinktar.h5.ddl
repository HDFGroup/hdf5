HDF5 "out-textlinktar-prune.textlinktar.h5" {
GROUP "/" {
   DATASET "dset" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 6 ) / ( 6 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 24
         OFFSET 2048
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
   }
   GROUP "empty_group" {
   }
   GROUP "group" {
      DATASET "dset" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 6 ) / ( 6 ) }
         STORAGE_LAYOUT {
            CONTIGUOUS
            SIZE 24
            OFFSET 2072
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
      }
      GROUP "subgroup" {
         GROUP "link_to_group" {
            HARDLINK "/group"
         }
      }
   }
   DATATYPE "type" H5T_STD_I32LE;
}
}
