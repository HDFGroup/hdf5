HDF5 "out-tsoftlinks-mergeprune.tsoftlinks.h5" {
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
      DATASET "soft_dset1" {
         DATATYPE  H5T_STD_I32BE
         DATASPACE  SIMPLE { ( 4, 2 ) / ( 4, 2 ) }
         STORAGE_LAYOUT {
            CONTIGUOUS
            SIZE 32
            OFFSET 2112
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
      DATASET "soft_dset2" {
         DATATYPE  H5T_STD_I32BE
         DATASPACE  SIMPLE { ( 4, 2 ) / ( 4, 2 ) }
         STORAGE_LAYOUT {
            CONTIGUOUS
            SIZE 32
            OFFSET 2144
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
      DATATYPE "soft_dtype" H5T_STD_I32BE;
      GROUP "soft_empty_grp" {
      }
   }
   GROUP "group_empty" {
   }
   DATASET "soft_dset1" {
      DATATYPE  H5T_STD_I32BE
      DATASPACE  SIMPLE { ( 4, 2 ) / ( 4, 2 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 32
         OFFSET 2176
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
   DATATYPE "soft_dtype" H5T_STD_I32BE;
   GROUP "soft_empty_grp" {
   }
   GROUP "soft_group1" {
      SOFTLINK "soft_dangle" {
         LINKTARGET "not_yet"
      }
      SOFTLINK "soft_dset1" {
         LINKTARGET "/dset1"
      }
      SOFTLINK "soft_dset2" {
         LINKTARGET "/dset2"
      }
      SOFTLINK "soft_dtype" {
         LINKTARGET "/dtype"
      }
      SOFTLINK "soft_empty_grp" {
         LINKTARGET "/group_empty"
      }
   }
}
}
