HDF5 "tfilters.h5" {
DATASET "myfilter" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE 800 (1.000:1 COMPRESSION)
   }
   FILTERS {
      USER_DEFINED_FILTER {
         FILTER_ID 405
         COMMENT myfilter
         PARAMS { 5 6 }
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
