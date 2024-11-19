HDF5 "h5ex_ds1.h5" {
GROUP "/" {
   DATASET "Mydata" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 3, 4 ) / ( 3, 4 ) }
      DATA {
      (0,0): 1, 2, 3, 4,
      (1,0): 5, 6, 7, 8,
      (2,0): 9, 10, 11, 12
      }
      ATTRIBUTE "DIMENSION_LIST" {
         DATATYPE  H5T_VLEN { H5T_REFERENCE { H5T_STD_REF_OBJECT } }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): (DATASET 228467808 "/Yaxis"), (DATASET 228463248 "/Xaxis")
         }
      }
   }
   DATASET "Xaxis" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): 10, 20, 50, 100
      }
      ATTRIBUTE "CLASS" {
         DATATYPE  H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "DIMENSION_SCALE"
         }
      }
      ATTRIBUTE "REFERENCE_LIST" {
         DATATYPE  H5T_COMPOUND {
            H5T_REFERENCE { H5T_STD_REF_OBJECT } "dataset";
            H5T_STD_U32LE "dimension";
         }
         DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
         DATA {
         (0): {
               DATASET 228478416 "/Mydata",
               1
            }
         }
      }
   }
   DATASET "Yaxis" {
      DATATYPE  H5T_IEEE_F32LE
      DATASPACE  SIMPLE { ( 3 ) / ( 3 ) }
      DATA {
      (0): 10, 20, 30
      }
      ATTRIBUTE "CLASS" {
         DATATYPE  H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "DIMENSION_SCALE"
         }
      }
      ATTRIBUTE "REFERENCE_LIST" {
         DATATYPE  H5T_COMPOUND {
            H5T_REFERENCE { H5T_STD_REF_OBJECT } "dataset";
            H5T_STD_U32LE "dimension";
         }
         DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
         DATA {
         (0): {
               DATASET 228464864 "/Mydata",
               0
            }
         }
      }
   }
}
}
