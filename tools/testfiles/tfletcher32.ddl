#############################
Expected output for 'h5dump -H -p -d fletcher32 tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "fletcher32" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT CHUNKED {
      SIZE 816 ( 10, 5 )
   }
   FILTERS {
      CHECKSUM FLETCHER32
   }
   FILLVALUE {
      FILL_TIME IFSET
      ALLOC_TIME INCR
      VALUE  -99   
   }
}
}
