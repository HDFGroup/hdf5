HDF5 "tdatareg.h5" {
DATASET "/Dataset1" {
   DATATYPE  H5T_REFERENCE { H5T_STD_REF_DSETREG }
   DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
   SUBSET {
      START ( 0 );
      STRIDE ( 1 );
      COUNT ( 1 );
      BLOCK ( 1 );
      DATA {
         DATASET "tdatareg.h5/Dataset2" {
            REGION_TYPE BLOCK  (2,2)-(7,7)
            DATATYPE  H5T_STD_U8BE
            DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
            DATA {
            }
         }
         DATASET "tdatareg.h5/Dataset2" {
            REGION_TYPE POINT  (6,9), (2,2), (8,4), (1,6), (2,8), (3,2),
             (0,4), (9,0), (7,1), (3,3)
            DATATYPE  H5T_STD_U8BE
            DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
            DATA {
            }
         }
         NULL
         NULL
      }
   }
}
}
