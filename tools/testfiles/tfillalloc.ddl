#############################
Expected output for 'h5dump -H -p -d fill_time_alloc tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "fill_time_alloc" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE 800
    }
   FILLVALUE {
      FILL_TIME ALLOC
      ALLOC_TIME INCR
      VALUE  -99   
   }
}
}
