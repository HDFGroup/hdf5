#############################
Expected output for 'h5dump tenum.h5'
#############################
HDF5 "tenum.h5" {
GROUP "/" {
   DATATYPE "enum normal" {
      H5T_ENUM
      { H5T_STD_I32BE;
       "RED"              0;
       "GREEN"            1;
       "BLUE"             2;
       "WHITE"            3;
       "BLACK"            4;
      };
   }
   DATASET "table" {
      DATATYPE { H5T_ENUM
         { H5T_STD_I32BE;
          "RED"              0;
          "GREEN"            1;
          "BLUE"             2;
          "WHITE"            3;
          "BLACK"            4;
         }
 }
      DATASPACE { SIMPLE ( 20 ) / ( 20 ) }
      DATA {
         RED, GREEN, BLUE, GREEN, WHITE, WHITE, BLACK, GREEN, BLUE, RED, RED, 
         BLUE, GREEN, BLACK, WHITE, RED, WHITE, GREEN, GREEN, BLUE
      }
   }
}
}
