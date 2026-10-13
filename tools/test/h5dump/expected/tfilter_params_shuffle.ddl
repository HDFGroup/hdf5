HDF5 "tfilters.h5" {
DATASET "shuffle" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE 800 (1.000:1 COMPRESSION)
   }
   FILTERS {
      PREPROCESSING SHUFFLE {
         PARAMS_STRING 'cd_values=4'
         DESCRIPTION "Byte shuffle preprocessing to improve downstream compression"
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
