#############################
Expected output for 'h5dump -H -p -d myfilter tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "myfilter" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE 800
    }
   FILTERS {
      UNKNOWN_FILTER {
         FILTER_ID 405
         COMMENT myfilter
         PARAMS { 5 6 }
      }
      FILLVALUE {
         FILL_TIME IFSET
         ALLOC_TIME INCR
         VALUE  -99      
      }
   }
}
