HDF5 "h5ex_d_granularbr.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_IEEE_F32LE
      DATASPACE  SIMPLE { ( 32, 64 ) / ( 32, 64 ) }
      STORAGE_LAYOUT {
         CHUNKED ( 4, 8 )
         SIZE 8192 (1.000:1 COMPRESSION)
      }
      FILTERS {
         USER_DEFINED_FILTER {
            FILTER_ID 32023
            COMMENT Granular BitRound filter
            PARAMS { 3 4 0 0 0 }
         }
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_IFSET
         VALUE  H5D_FILL_VALUE_DEFAULT
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_INCR
      }
   }
}
}
