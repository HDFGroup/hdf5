#############################
Expected output for './h5dump tall.h5'
#############################
HDF5 "tall.h5" {
GROUP "/" {
   ATTRIBUTE "attr1" {
      DATATYPE { "H5T_NATIVE_CHAR" }
      DATASPACE { ARRAY ( 10 ) ( 10 ) }
      DATA {"abcdefghi\000"}
   }
   ATTRIBUTE "attr2" {
      DATATYPE { "H5T_NATIVE_INT" }
      DATASPACE { ARRAY ( 2, 2 ) ( 2, 2 ) }
      DATA {0,1,2,3}
   }
   GROUP "g1" {
      GROUP "g1.1" {
         DATASET "dset1.1.1" {
            DATATYPE { "H5T_NATIVE_INT" }
            DATASPACE { ARRAY ( 10, 10 ) ( 10, 10 ) }
            DATA {0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9}
            ATTRIBUTE "attr1" {
               DATATYPE { "H5T_NATIVE_CHAR" }
               DATASPACE { ARRAY ( 27 ) ( 27 ) }
               DATA {"1st attribute of dset1.1.1\000"}
            }
            ATTRIBUTE "attr2" {
               DATATYPE { "H5T_NATIVE_CHAR" }
               DATASPACE { ARRAY ( 27 ) ( 27 ) }
               DATA {"2nd attribute of dset1.1.1\000"}
            }
         }
         DATASET "dset1.1.2" {
            DATATYPE { "H5T_NATIVE_INT" }
            DATASPACE { ARRAY ( 20 ) ( 20 ) }
            DATA {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19}
         }
      }
      GROUP "g1.2" {
         GROUP "g1.2.1" {
            SOFTLINK "slink" {
               linktarget "../somevalue/."
            }
         }
      }
   }
   GROUP "g2" {
      DATASET "dset2.1" {
         DATATYPE { "H5T_NATIVE_FLOAT" }
         DATASPACE { ARRAY ( 10 ) ( 10 ) }
         DATA {1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9}
      }
      DATASET "dset2.2" {
         DATATYPE { "H5T_NATIVE_FLOAT" }
         DATASPACE { ARRAY ( 3, 5 ) ( 3, 5 ) }
         DATA {0,0,0,0,0,0.1,0.1,0.1,0.1,0.1,0.2,0.2,0.2,0.2,0.2}
      }
   }
}
}
