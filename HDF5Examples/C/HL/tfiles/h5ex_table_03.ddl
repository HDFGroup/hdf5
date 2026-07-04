HDF5 "h5ex_table_03.h5" {
GROUP "/" {
   DATASET "table" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I32LE "Latitude";
         H5T_STD_I32LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            1,
            0.2,
            0.3
         },
      (1): {
            "one",
            10,
            11,
            1.2,
            10.3
         },
      (2): {
            "no data",
            -1,
            -2,
            -99,
            -98
         },
      (3): {
            "no data",
            -1,
            -2,
            -99,
            -98
         },
      (4): {
            "no data",
            -1,
            -2,
            -99,
            -98
         },
      (5): {
            "no data",
            -1,
            -2,
            -99,
            -98
         },
      (6): {
            "no data",
            -1,
            -2,
            -99,
            -98
         },
      (7): {
            "no data",
            -1,
            -2,
            -99,
            -98
         }
      }
      ATTRIBUTE "CLASS" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "TABLE"
         }
      }
      ATTRIBUTE "FIELD_0_FILL" {
         DATATYPE  H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "no data"
         }
      }
      ATTRIBUTE "FIELD_0_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 5;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Name"
         }
      }
      ATTRIBUTE "FIELD_1_FILL" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SCALAR
         DATA {
         (0): -1
         }
      }
      ATTRIBUTE "FIELD_1_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 9;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Latitude"
         }
      }
      ATTRIBUTE "FIELD_2_FILL" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SCALAR
         DATA {
         (0): -2
         }
      }
      ATTRIBUTE "FIELD_2_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 10;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Longitude"
         }
      }
      ATTRIBUTE "FIELD_3_FILL" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SCALAR
         DATA {
         (0): -99
         }
      }
      ATTRIBUTE "FIELD_3_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 9;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Pressure"
         }
      }
      ATTRIBUTE "FIELD_4_FILL" {
         DATATYPE  H5T_IEEE_F64LE
         DATASPACE  SCALAR
         DATA {
         (0): -98
         }
      }
      ATTRIBUTE "FIELD_4_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 12;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Temperature"
         }
      }
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 12;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Table Title"
         }
      }
      ATTRIBUTE "VERSION" {
         DATATYPE  H5T_STRING {
            STRSIZE 4;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "3.0"
         }
      }
   }
}
}
