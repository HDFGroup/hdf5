HDF5 "tattr.h5" {
GROUP "/" {
   ATTRIBUTE "/attr1" {
      DATATYPE  H5T_STD_I8BE
      DATASPACE  SIMPLE { ( 24 ) / ( 24 ) }
      DATA {
      }
   }
   ATTRIBUTE "attr2" {
      DATATYPE  H5T_STD_I32BE
      DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
      DATA {
      }
   }
   ATTRIBUTE "attr3" {
      DATATYPE  H5T_IEEE_F64BE
      DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
      DATA {
      }
   }
   ATTRIBUTE "attr4" {
      DATATYPE  H5T_STD_I32BE
      DATASPACE  SCALAR
      DATA {
      }
   }
   ATTRIBUTE "attr5" {
      DATATYPE  H5T_STRING {
         STRSIZE 17;
         STRPAD H5T_STR_NULLTERM;
         CSET H5T_CSET_ASCII;
         CTYPE H5T_C_S1;
      }
      DATASPACE  SCALAR
      DATA {
      }
   }
}
}
