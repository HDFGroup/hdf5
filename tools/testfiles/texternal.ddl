#############################
Expected output for 'h5dump -H -p -d external tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "external" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 100 ) / ( 100 ) }
   STORAGE_LAYOUT {
      CONTIGUOUS
      EXTERNAL {
         FILENAME ext1.bin SIZE 200 OFFSET 0
         FILENAME ext2.bin SIZE 200 OFFSET 0
      }
   }
   FILLVALUE {
      FILL_TIME IFSET
      ALLOC_TIME LATE
      VALUE  0   
   }
}
}
