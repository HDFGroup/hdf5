#############################
Expected output for 'h5dump -e -d string tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "string" {
   DATATYPE  H5T_STRING {
         STRSIZE 57;
         STRPAD H5T_STR_NULLTERM;
         CSET H5T_CSET_ASCII;
         CTYPE H5T_C_S1;
      }
   DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
   DATA {
        (0) "this is
            a string with three
            newline
            escape characters"
   }
}
}
