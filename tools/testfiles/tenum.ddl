#############################
Expected output for 'h5dump tenum.h5'
#############################
HDF5 "tenum.h5" {
GROUP "/" {
   DATATYPE "enum normal"       H5T_ENUM {
         H5T_STD_I32LE;
         "RED"              0;
         "GREEN
green"      1;
         "BLUE blue"        2;
         "WHITE "white""    3;
         "BLACK 'black'"    4;
      };

   DATASET "table" {
      DATATYPE  "/enum normal"
      DATASPACE  SIMPLE { ( 20 ) / ( 20 ) }
      DATA {
         RED, GREEN\ngreen, BLUE blue, GREEN\ngreen, WHITE \"white\",
         WHITE \"white\", BLACK \'black\', GREEN\ngreen, BLUE blue, RED, RED,
         BLUE blue, GREEN\ngreen, BLACK \'black\', WHITE \"white\", RED,
         WHITE \"white\", GREEN\ngreen, GREEN\ngreen, BLUE blue
      }
   }
}
}
