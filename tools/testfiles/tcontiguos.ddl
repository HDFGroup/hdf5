#############################
Expected output for 'h5dump -H -p -d contiguous tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "contiguous" {
COMMENT "This is a dataset with contiguous storage"
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CONTIGUOUS
      SIZE 800
      OFFSET 4096
   }
   FILLVALUE {
      FILL_TIME IFSET
      ALLOC_TIME LATE
      VALUE  -99   
   }
}
}
