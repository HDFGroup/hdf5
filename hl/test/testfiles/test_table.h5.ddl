HDF5 "test_table.h5" {
GROUP "/" {
   DATASET "table" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_U32LE "F1";
         H5T_IEEE_F64LE "F2";
         H5T_IEEE_F64LE "F3";
         H5T_ARRAY { [3] H5T_IEEE_F32LE } "F4";
         H5T_ARRAY { [3] H5T_IEEE_F32LE } "F5";
         H5T_ARRAY { [2] H5T_IEEE_F32LE } "F6";
      }
      DATASPACE  SIMPLE { ( 5 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            12112,
            1.4,
            2.5,
            [ 1, 2, 3 ],
            [ 4, 5, 6 ],
            [ 99, 100 ]
         },
      (1): {
            12113,
            1.4,
            2.5,
            [ 1, 2, 3 ],
            [ 4, 5, 6 ],
            [ 99, 100 ]
         },
      (2): {
            12114,
            1.4,
            2.5,
            [ 1, 2, 3 ],
            [ 4, 5, 6 ],
            [ 99, 100 ]
         },
      (3): {
            12118,
            1.4,
            2.5,
            [ 1, 2, 3 ],
            [ 4, 5, 6 ],
            [ 99, 100 ]
         },
      (4): {
            12119,
            1.4,
            2.5,
            [ 1, 2, 3 ],
            [ 4, 5, 6 ],
            [ 99, 100 ]
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
         DATATYPE  H5T_STD_U32LE
         DATASPACE  SCALAR
         DATA {
         (0): 9999999
         }
      }
      ATTRIBUTE "FIELD_0_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 3;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "F1"
         }
      }
      ATTRIBUTE "FIELD_1_FILL" {
         DATATYPE  H5T_IEEE_F64LE
         DATASPACE  SCALAR
         DATA {
         (0): -1e+07
         }
      }
      ATTRIBUTE "FIELD_1_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 3;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "F2"
         }
      }
      ATTRIBUTE "FIELD_2_FILL" {
         DATATYPE  H5T_IEEE_F64LE
         DATASPACE  SCALAR
         DATA {
         (0): 999999
         }
      }
      ATTRIBUTE "FIELD_2_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 3;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "F3"
         }
      }
      ATTRIBUTE "FIELD_3_FILL" {
         DATATYPE  H5T_ARRAY { [3] H5T_IEEE_F32LE }
         DATASPACE  SCALAR
         DATA {
         (0): [ 999, 999, 999 ]
         }
      }
      ATTRIBUTE "FIELD_3_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 3;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "F4"
         }
      }
      ATTRIBUTE "FIELD_4_FILL" {
         DATATYPE  H5T_ARRAY { [3] H5T_IEEE_F32LE }
         DATASPACE  SCALAR
         DATA {
         (0): [ 999, 999, 999 ]
         }
      }
      ATTRIBUTE "FIELD_4_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 3;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "F5"
         }
      }
      ATTRIBUTE "FIELD_5_FILL" {
         DATATYPE  H5T_ARRAY { [2] H5T_IEEE_F32LE }
         DATASPACE  SCALAR
         DATA {
         (0): [ 999, 999 ]
         }
      }
      ATTRIBUTE "FIELD_5_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 3;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "F6"
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
   DATASET "table1" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            0,
            0,
            0
         },
      (1): {
            "one",
            10,
            1,
            10,
            10
         },
      (2): {
            "two",
            20,
            2,
            20,
            20
         },
      (3): {
            "three",
            30,
            3,
            30,
            30
         },
      (4): {
            "four",
            40,
            4,
            40,
            40
         },
      (5): {
            "five",
            50,
            5,
            50,
            50
         },
      (6): {
            "six",
            60,
            6,
            60,
            60
         },
      (7): {
            "seven",
            70,
            7,
            70,
            70
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
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table10" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            0,
            -99,
            0
         },
      (1): {
            "one",
            10,
            1,
            -99,
            10
         },
      (2): {
            "two",
            20,
            2,
            -99,
            20
         },
      (3): {
            "three",
            -1,
            3,
            -99,
            -1
         },
      (4): {
            "four",
            -1,
            4,
            -99,
            -1
         },
      (5): {
            "five",
            -1,
            5,
            -99,
            -1
         },
      (6): {
            "six",
            -1,
            6,
            -99,
            -1
         },
      (7): {
            "seven",
            -1,
            7,
            -99,
            -1
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
         DATATYPE  H5T_STD_I64LE
         DATASPACE  SCALAR
         DATA {
         (0): -1
         }
      }
      ATTRIBUTE "FIELD_1_NAME" {
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
      ATTRIBUTE "FIELD_2_FILL" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SCALAR
         DATA {
         (0): -99
         }
      }
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_FILL" {
         DATATYPE  H5T_IEEE_F64LE
         DATASPACE  SCALAR
         DATA {
         (0): -99
         }
      }
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_FILL" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SCALAR
         DATA {
         (0): -1
         }
      }
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table11" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "",
            0,
            0,
            0,
            0
         },
      (1): {
            "",
            0,
            0,
            0,
            0
         },
      (2): {
            "",
            0,
            0,
            0,
            0
         },
      (3): {
            "",
            10,
            1,
            0,
            10
         },
      (4): {
            "",
            20,
            2,
            0,
            20
         },
      (5): {
            "",
            0,
            0,
            0,
            0
         },
      (6): {
            "",
            0,
            0,
            0,
            0
         },
      (7): {
            "",
            0,
            0,
            0,
            0
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
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table12" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            0,
            0,
            0
         },
      (1): {
            "one",
            10,
            1,
            0,
            10
         },
      (2): {
            "two",
            20,
            2,
            0,
            20
         },
      (3): {
            "three",
            0,
            3,
            0,
            0
         },
      (4): {
            "four",
            0,
            4,
            0,
            0
         },
      (5): {
            "five",
            0,
            5,
            0,
            0
         },
      (6): {
            "six",
            0,
            6,
            0,
            0
         },
      (7): {
            "seven",
            0,
            7,
            0,
            0
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
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table13" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
         H5T_STD_I32LE "New Field";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            0,
            0,
            0,
            0
         },
      (1): {
            "one",
            10,
            1,
            10,
            10,
            1
         },
      (2): {
            "two",
            20,
            2,
            20,
            20,
            2
         },
      (3): {
            "three",
            30,
            3,
            30,
            30,
            3
         },
      (4): {
            "four",
            40,
            4,
            40,
            40,
            4
         },
      (5): {
            "five",
            50,
            5,
            50,
            50,
            5
         },
      (6): {
            "six",
            60,
            6,
            60,
            60,
            6
         },
      (7): {
            "seven",
            70,
            7,
            70,
            70,
            7
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
         DATATYPE  H5T_STD_I64LE
         DATASPACE  SCALAR
         DATA {
         (0): -1
         }
      }
      ATTRIBUTE "FIELD_1_NAME" {
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
      ATTRIBUTE "FIELD_2_FILL" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SCALAR
         DATA {
         (0): -99
         }
      }
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_FILL" {
         DATATYPE  H5T_IEEE_F64LE
         DATASPACE  SCALAR
         DATA {
         (0): -99
         }
      }
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_FILL" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SCALAR
         DATA {
         (0): -1
         }
      }
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "FIELD_5_FILL" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SCALAR
         DATA {
         (0): -100
         }
      }
      ATTRIBUTE "FIELD_5_NAME" {
         DATATYPE  H5T_STRING {
            STRSIZE 10;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "New Field"
         }
      }
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table14" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            0,
            0
         },
      (1): {
            "one",
            10,
            10,
            10
         },
      (2): {
            "two",
            20,
            20,
            20
         },
      (3): {
            "three",
            30,
            30,
            30
         },
      (4): {
            "four",
            40,
            40,
            40
         },
      (5): {
            "five",
            50,
            50,
            50
         },
      (6): {
            "six",
            60,
            60,
            60
         },
      (7): {
            "seven",
            70,
            70,
            70
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
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table2" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 12 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            0,
            0,
            0
         },
      (1): {
            "zero",
            0,
            0,
            0,
            0
         },
      (2): {
            "zero",
            0,
            0,
            0,
            0
         },
      (3): {
            "one",
            10,
            1,
            10,
            10
         },
      (4): {
            "two",
            20,
            2,
            20,
            20
         },
      (5): {
            "three",
            30,
            3,
            30,
            30
         },
      (6): {
            "four",
            40,
            4,
            40,
            40
         },
      (7): {
            "five",
            50,
            5,
            50,
            50
         },
      (8): {
            "six",
            60,
            6,
            60,
            60
         },
      (9): {
            "seven",
            70,
            7,
            70,
            70
         },
      (10): {
            "eight",
            80,
            8,
            80,
            80
         },
      (11): {
            "nine",
            90,
            9,
            90,
            90
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
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table3" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 0 ) / ( H5S_UNLIMITED ) }
      DATA {
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
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table4" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            0,
            0,
            0
         },
      (1): {
            "one",
            10,
            1,
            10,
            10
         },
      (2): {
            "two",
            20,
            2,
            20,
            20
         },
      (3): {
            "three",
            30,
            3,
            30,
            30
         },
      (4): {
            "four",
            40,
            4,
            40,
            40
         },
      (5): {
            "five",
            50,
            5,
            50,
            50
         },
      (6): {
            "six",
            60,
            6,
            60,
            60
         },
      (7): {
            "seven",
            70,
            7,
            70,
            70
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
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table5" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 10 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            0,
            0,
            0
         },
      (1): {
            "one",
            10,
            1,
            10,
            10
         },
      (2): {
            "two",
            20,
            2,
            20,
            20
         },
      (3): {
            "three",
            30,
            3,
            30,
            30
         },
      (4): {
            "four",
            40,
            4,
            40,
            40
         },
      (5): {
            "five",
            50,
            5,
            50,
            50
         },
      (6): {
            "three",
            30,
            3,
            30,
            30
         },
      (7): {
            "four",
            40,
            4,
            40,
            40
         },
      (8): {
            "six",
            60,
            6,
            60,
            60
         },
      (9): {
            "seven",
            70,
            7,
            70,
            70
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
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table6" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            0,
            0,
            0
         },
      (1): {
            "one",
            10,
            1,
            10,
            10
         },
      (2): {
            "two",
            20,
            2,
            20,
            20
         },
      (3): {
            "three",
            30,
            3,
            30,
            30
         },
      (4): {
            "four",
            40,
            4,
            40,
            40
         },
      (5): {
            "five",
            50,
            5,
            50,
            50
         },
      (6): {
            "six",
            60,
            6,
            60,
            60
         },
      (7): {
            "seven",
            70,
            7,
            70,
            70
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
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table7" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            0,
            0,
            0
         },
      (1): {
            "one",
            10,
            1,
            10,
            10
         },
      (2): {
            "two",
            20,
            2,
            20,
            20
         },
      (3): {
            "three",
            30,
            3,
            30,
            30
         },
      (4): {
            "four",
            40,
            4,
            40,
            40
         },
      (5): {
            "five",
            50,
            5,
            50,
            50
         },
      (6): {
            "six",
            60,
            6,
            60,
            60
         },
      (7): {
            "seven",
            70,
            7,
            70,
            70
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
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
   DATASET "table8" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 16 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "zero",
            0,
            0,
            0,
            0
         },
      (1): {
            "one",
            10,
            1,
            10,
            10
         },
      (2): {
            "two",
            20,
            2,
            20,
            20
         },
      (3): {
            "three",
            30,
            3,
            30,
            30
         },
      (4): {
            "four",
            40,
            4,
            40,
            40
         },
      (5): {
            "five",
            50,
            5,
            50,
            50
         },
      (6): {
            "six",
            60,
            6,
            60,
            60
         },
      (7): {
            "seven",
            70,
            7,
            70,
            70
         },
      (8): {
            "zero",
            0,
            0,
            0,
            0
         },
      (9): {
            "one",
            10,
            1,
            10,
            10
         },
      (10): {
            "two",
            20,
            2,
            20,
            20
         },
      (11): {
            "three",
            30,
            3,
            30,
            30
         },
      (12): {
            "four",
            40,
            4,
            40,
            40
         },
      (13): {
            "five",
            50,
            5,
            50,
            50
         },
      (14): {
            "six",
            60,
            6,
            60,
            60
         },
      (15): {
            "seven",
            70,
            7,
            70,
            70
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
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 12;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Merge table"
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
   DATASET "table9" {
      DATATYPE  H5T_COMPOUND {
         H5T_STRING {
            STRSIZE 16;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "Name";
         H5T_STD_I64LE "Longitude";
         H5T_IEEE_F32LE "Pressure";
         H5T_IEEE_F64LE "Temperature";
         H5T_STD_I32LE "Latitude";
      }
      DATASPACE  SIMPLE { ( 8 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            "no data",
            -1,
            -99,
            -99,
            -1
         },
      (1): {
            "no data",
            -1,
            -99,
            -99,
            -1
         },
      (2): {
            "no data",
            0,
            0,
            -99,
            0
         },
      (3): {
            "no data",
            10,
            1,
            -99,
            10
         },
      (4): {
            "no data",
            20,
            2,
            -99,
            20
         },
      (5): {
            "no data",
            -1,
            -99,
            -99,
            -1
         },
      (6): {
            "no data",
            -1,
            -99,
            -99,
            -1
         },
      (7): {
            "no data",
            -1,
            -99,
            -99,
            -1
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
         DATATYPE  H5T_STD_I64LE
         DATASPACE  SCALAR
         DATA {
         (0): -1
         }
      }
      ATTRIBUTE "FIELD_1_NAME" {
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
      ATTRIBUTE "FIELD_2_FILL" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SCALAR
         DATA {
         (0): -99
         }
      }
      ATTRIBUTE "FIELD_2_NAME" {
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
      ATTRIBUTE "FIELD_3_FILL" {
         DATATYPE  H5T_IEEE_F64LE
         DATASPACE  SCALAR
         DATA {
         (0): -99
         }
      }
      ATTRIBUTE "FIELD_3_NAME" {
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
      ATTRIBUTE "FIELD_4_FILL" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SCALAR
         DATA {
         (0): -1
         }
      }
      ATTRIBUTE "FIELD_4_NAME" {
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
      ATTRIBUTE "TITLE" {
         DATATYPE  H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
         DATASPACE  SCALAR
         DATA {
         (0): "Title"
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
