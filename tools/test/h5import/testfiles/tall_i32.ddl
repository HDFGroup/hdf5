HDF5 "d-tall_i32.h5" {
GROUP "/" {
   GROUP "g1" {
      GROUP "g1.1" {
         DATASET "dset1.1.1" {
            DATATYPE  H5T_STD_I32BE
            DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
            STORAGE_LAYOUT {
               CONTIGUOUS
               SIZE 24
               OFFSET 3464
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
            (0,0): 1, 4,
            (1,0): 3, 12,
            (2,0): 5, 20
            }
         }
      }
   }
}
}
