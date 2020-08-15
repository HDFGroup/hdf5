HDF5 "out-h5copy_extlinks_src-mergeprune.h5copy_extlinks_src.h5" {
GROUP "/" {
   GROUP "group_ext" {
      DATATYPE "extlink_datatype" H5T_STD_I32LE;
      DATASET "extlink_dset" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 6 ) / ( 6 ) }
         STORAGE_LAYOUT {
            CONTIGUOUS
            SIZE 24
            OFFSET 2200
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
      GROUP "extlink_grp" {
      }
   }
}
}
