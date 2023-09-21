HDF5 "charsets.h5" {
GROUP "/" {
   DATASET "CharSets" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE H5T_VARIABLE;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "ascii";
         H5T_STRING {
            STRSIZE H5T_VARIABLE;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_UTF8;
            CTYPE H5T_C_S1;
         } "utf8";
      }
      DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
      DATA {
      (0): {
            "ascii",
            "utf8"
         }
      }
   }
}
}
