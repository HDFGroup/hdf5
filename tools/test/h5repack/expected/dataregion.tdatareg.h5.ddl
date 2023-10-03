HDF5 "out-dataregion.tdatareg.h5" {
GROUP "/" {
   DATASET "Dataset1" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF_DSETREG }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 48
         OFFSET 6244
      }
      FILTERS {
         NONE
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_IFSET
         VALUE  H5D_FILL_VALUE_UNDEFINED
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_LATE
      }
   }
   DATASET "Dataset2" {
      DATATYPE  H5T_STD_U8BE
      DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 100
         OFFSET 2048
      }
      FILTERS {
         NONE
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_IFSET
         VALUE  H5D_FILL_VALUE_UNDEFINED
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_LATE
      }
   }
}
}
