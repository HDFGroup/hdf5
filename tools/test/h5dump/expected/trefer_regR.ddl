HDF5 "trefer_reg.h5" {
GROUP "/" {
   DATASET "DS_NA" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF }
      DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
      DATA {
         NULL
      }
   }
   DATASET "Dataset1" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
         DATASET "trefer_reg.h5/Dataset2" {
            REGION_TYPE BLOCK  (2,2)-(7,7)
            DATATYPE  H5T_STD_U8LE
            DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
            DATA {
               66, 69, 72, 75, 78, 81,
               96, 99, 102, 105, 108, 111,
               126, 129, 132, 135, 138, 141,
               156, 159, 162, 165, 168, 171,
               186, 189, 192, 195, 198, 201,
               216, 219, 222, 225, 228, 231
            }
         }
         DATASET "trefer_reg.h5/Dataset2" {
            REGION_TYPE POINT  (6,9), (2,2), (8,4), (1,6), (2,8), (3,2),
             (0,4), (9,0), (7,1), (3,3)
            DATATYPE  H5T_STD_U8LE
            DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
            DATA {
               207,
               66,
               252,
               48,
               84,
               96,
               12,
               14,
               213,
               99
            }
         }
         DATASET "trefer_reg.h5/Dataset2"
         DATASET "trefer_reg.h5/Dataset2"
      }
   }
   DATASET "Dataset2" {
      DATATYPE  H5T_STD_U8LE
      DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
      DATA {
      (0,0): 0, 3, 6, 9, 12, 15, 18, 21, 24, 27,
      (1,0): 30, 33, 36, 39, 42, 45, 48, 51, 54, 57,
      (2,0): 60, 63, 66, 69, 72, 75, 78, 81, 84, 87,
      (3,0): 90, 93, 96, 99, 102, 105, 108, 111, 114, 117,
      (4,0): 120, 123, 126, 129, 132, 135, 138, 141, 144, 147,
      (5,0): 150, 153, 156, 159, 162, 165, 168, 171, 174, 177,
      (6,0): 180, 183, 186, 189, 192, 195, 198, 201, 204, 207,
      (7,0): 210, 213, 216, 219, 222, 225, 228, 231, 234, 237,
      (8,0): 240, 243, 246, 249, 252, 255, 2, 5, 8, 11,
      (9,0): 14, 17, 20, 23, 26, 29, 32, 35, 38, 41
      }
   }
}
}
