HDF5 "out-tsoftlinks-prune.tsoftlinks.h5" {
GROUP "/" {
   DATASET "dset1" {
      DATATYPE  H5T_STD_I32BE
      DATASPACE  SIMPLE { ( 4, 2 ) / ( 4, 2 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 32
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
   DATASET "dset2" {
      DATATYPE  H5T_STD_I32BE
      DATASPACE  SIMPLE { ( 4, 2 ) / ( 4, 2 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 32
         OFFSET 2080
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
   DATATYPE "dtype" H5T_STD_I32BE;
   GROUP "group1" {
   }
   GROUP "group_empty" {
   }
}
}
