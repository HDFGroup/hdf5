HDF5 "out-attrregion.tattrreg.h5" {
GROUP "/" {
   DATASET "Dataset1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  NULL
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 0
         OFFSET HADDR_UNDEF
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
      ATTRIBUTE "Attribute1" {
         DATATYPE  H5T_REFERENCE { H5T_STD_REF_DSETREG }
         DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
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
         VALUE  H5D_FILL_VALUE_DEFAULT
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_LATE
      }
   }
}
}
