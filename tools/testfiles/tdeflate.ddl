#############################
Expected output for 'h5dump -H -p -d deflate tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "deflate" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE 385
    }
   FILTERS {
      COMPRESSION DEFLATE { LEVEL 9 }
   }
   FILLVALUE {
      FILL_TIME IFSET
      ALLOC_TIME INCR
      VALUE  -99   
   }
}
}
