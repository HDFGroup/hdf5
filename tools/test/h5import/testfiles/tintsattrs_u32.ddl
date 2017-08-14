HDF5 "d-tintsattrs_u32.h5" {
GROUP "/" {
   DATASET "DU32BITS" {
      DATATYPE  H5T_STD_U32LE
      DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
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
      DATA {
      (0,0): 4294967292, 4294967264,
      (1,0): 4294967280, 4294967168,
      (2,0): 4294967232, 4294966784
      }
   }
}
}
