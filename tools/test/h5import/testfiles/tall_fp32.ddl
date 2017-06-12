HDF5 "d-tall_fp32.h5" {
GROUP "/" {
   GROUP "g2" {
      DATASET "dset2.2" {
         DATATYPE  H5T_IEEE_F32BE
         DATASPACE  SIMPLE { ( 1, 2 ) / ( 1, 2 ) }
         STORAGE_LAYOUT {
            CONTIGUOUS
            SIZE 8
            OFFSET 2432
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
         (0,0): 0.2, 0.8
         }
      }
   }
}
}
