#############################
Expected output for 'h5dump -d reference tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "reference" {
   DATATYPE  H5T_REFERENCE
   DATASPACE  SIMPLE { ( 5 ) / ( 5 ) }
   DATA {
        (0) DATASET 976 /compact , DATATYPE 4008 /myvlen ,
        (2) DATASET 976 /compact , DATATYPE 4008 /myvlen ,
        (4) DATASET 2376 /contiguous 
   }
}
}
