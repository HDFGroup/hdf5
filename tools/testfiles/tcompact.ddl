#############################
Expected output for 'h5dump -H -p -d compact tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "compact" {
COMMENT "This is a dataset with compact storage"
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT COMPACT {
      SIZE 800
   }
   FILLVALUE {
      FILL_TIME IFSET
      ALLOC_TIME EARLY
      VALUE  -99   
   }
}
}
