HDF5 "tldouble_scalar.h5" {
GROUP "/" {
   DATASET "dset" {
      DATATYPE  H5T_ARRAY { [6] H5T_NATIVE_LDOUBLE }
      DATASPACE  SCALAR
      STORAGE_LAYOUT {
         CONTIGUOUS
         SIZE 96
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
      (0): [ 0, 1, 2, 3, 4, 5 ]
      }
   }
}
}
