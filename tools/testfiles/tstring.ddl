#############################
Expected output for 'h5dump tstr3.h5'
#############################
HDF5 "tstr3.h5" {
GROUP "/" {
   DATASET "str1" {
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
   DATASET "str2" {
      DATATYPE  H5T_STRING {
            STRSIZE H5T_VARIABLE;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): "Four score and seven
            years ago our forefathers brought forth on this continent a new nation,",
      (1): "conceived in liberty
            and dedicated to the proposition that all men are created equal.",
      (2): "Now we are engaged
            in a great civil war,",
      (3): "testing whether that
            nation or any nation so conceived and so dedicated can long endure."
      }
   }
   DATASET "str3" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I32LE "a";
         H5T_STRING {
            STRSIZE 255;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "str";
      }
      DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
      DATA {
      (0): {
            24,
            "Four score and seven
            years ago our forefathers brought forth on this continent a new nation"
         }
      }
   }
   DATASET "str4" {
      DATATYPE  H5T_STD_I8LE
      DATASPACE  SIMPLE { ( 93 ) / ( 93 ) }
      DATA {
      (0): 70, 111, 117, 114, 32, 115, 99, 111, 114, 101, 32, 97, 110,
      (13): 100, 32, 115, 101, 118, 101, 110, 10, 32, 121, 101, 97,
      (25): 114, 115, 32, 97, 103, 111, 32, 111, 117, 114, 32, 102,
      (37): 111, 114, 101, 102, 97, 116, 104, 101, 114, 115, 32, 98,
      (49): 114, 111, 117, 103, 104, 116, 32, 102, 111, 114, 116,
      (60): 104, 32, 111, 110, 32, 116, 104, 105, 115, 32, 99, 111,
      (72): 110, 116, 105, 110, 101, 110, 116, 32, 97, 32, 110, 101,
      (84): 119, 32, 110, 97, 116, 105, 111, 110, 0
      }
   }
}
}
