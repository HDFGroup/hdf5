HDF5 "tvlenstr_array.h5" {
GROUP "/" {
   DATASET "CompoundArrayOfVlenStr" {
      DATATYPE  H5T_COMPOUND {
         H5T_ARRAY { [3] H5T_STRING {
            STRSIZE H5T_VARIABLE;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } } "vlen_str_array";
      }
      DATASPACE  SIMPLE { ( 5 ) / ( 5 ) }
      DATA {
      (0): {
            [ "This is a variable-length test string.", "This test string is also variable-length.", "A final test of variable-length strings. This string is longer than the others." ]
         },
      (1): {
            [ "This is a variable-length test string.", "This test string is also variable-length.", "A final test of variable-length strings. This string is longer than the others." ]
         },
      (2): {
            [ "This is a variable-length test string.", "This test string is also variable-length.", "A final test of variable-length strings. This string is longer than the others." ]
         },
      (3): {
            [ "This is a variable-length test string.", "This test string is also variable-length.", "A final test of variable-length strings. This string is longer than the others." ]
         },
      (4): {
            [ "This is a variable-length test string.", "This test string is also variable-length.", "A final test of variable-length strings. This string is longer than the others." ]
         }
      }
   }
   DATASET "ScalarArrayOfVlenStr" {
      DATATYPE  H5T_ARRAY { [3] H5T_STRING {
         STRSIZE H5T_VARIABLE;
         STRPAD H5T_STR_NULLTERM;
         CSET H5T_CSET_ASCII;
         CTYPE H5T_C_S1;
      } }
      DATASPACE  SIMPLE { ( 5 ) / ( 5 ) }
      DATA {
      (0): [ "This is a variable-length test string.", "This test string is also variable-length.", "A final test of variable-length strings. This string is longer than the others." ],
      (1): [ "This is a variable-length test string.", "This test string is also variable-length.", "A final test of variable-length strings. This string is longer than the others." ],
      (2): [ "This is a variable-length test string.", "This test string is also variable-length.", "A final test of variable-length strings. This string is longer than the others." ],
      (3): [ "This is a variable-length test string.", "This test string is also variable-length.", "A final test of variable-length strings. This string is longer than the others." ],
      (4): [ "This is a variable-length test string.", "This test string is also variable-length.", "A final test of variable-length strings. This string is longer than the others." ]
      }
   }
}
}
