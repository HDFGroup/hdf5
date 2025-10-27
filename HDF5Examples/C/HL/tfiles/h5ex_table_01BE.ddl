HDF5 "h5ex_table_01.h5" {
GROUP "/" {
   DATASET "table" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I32BE "Latitude";
         H5T_STD_I32BE "Longitude";
         H5T_IEEE_F32BE "Pressure";
         H5T_IEEE_F64BE "Temperature";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            1,
            0.2,
            3
         },
      (1): {
            "one",
            10,
            11,
            1.2,
            13
         },
      (2): {
            "two",
            20,
            21,
            2.2,
            23
         },
      (3): {
            "three",
            30,
            31,
            3.2,
            33
         },
      (4): {
            "four",
            40,
            41,
            4.2,
            43
         },
      (5): {
            "five",
            50,
            51,
            5.2,
            53
         },
      (6): {
            "six",
            60,
            61,
            6.2,
            63
         },
      (7): {
            "seven",
            70,
            71,
            7.2,
            73
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
