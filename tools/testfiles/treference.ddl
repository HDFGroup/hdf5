#############################
Expected output for 'h5dump -d reference tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "reference" {
   DATATYPE  H5T_REFERENCE
   DATASPACE  SIMPLE { ( 5 ) / ( 5 ) }
   DATA {
        (0) DATASET 0:976 /compact , DATATYPE 0:15955 /myvlen ,
        (2) DATASET 0:976 /compact , DATATYPE 0:15955 /myvlen ,
        (4) DATASET 0:2376 /contiguous 
   }
}
}
