#############################
Expected output for 'h5dump -a attr1 attr4 attr5 tattr.h5'
#############################
HDF5 "tattr.h5" {
ATTRIBUTE "attr1" {
   DATATYPE { H5T_STD_I8LE }
   DATASPACE { SIMPLE ( 24 ) / ( 24 ) }
   DATA {
      "attribute of root group\000"
   }
}
ATTRIBUTE "attr4" {
   DATATYPE { H5T_STD_I32BE }
   DATASPACE { SCALAR }
   DATA {
      100
   }
}
ATTRIBUTE "attr5" {
   DATATYPE {
      { STRSIZE 17;
        STRPAD H5T_STR_NULLTERM;
        CSET H5T_CSET_ASCII;
        CTYPE H5T_C_S1;
      }
   }
   DATASPACE { SCALAR }
   DATA {
      "string attribute\000"
   }
}
}
