#############################
Expected output for 'h5dump -d string tfilters.h5'
#############################
HDF5 "tfilters.h5" {
DATASET "string" {
   DATATYPE  H5T_STRING {
         STRSIZE 73;
         STRPAD H5T_STR_NULLTERM;
         CSET H5T_CSET_ASCII;
         CTYPE H5T_C_S1;
      }
   DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
   DATA {
   (0): "quote "  backspace form feed new line
            tab	 new line
            carriage return
           "
   }
}
}
