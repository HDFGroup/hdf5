HDF5 "tobjref.h5" {
GROUP "/" {
   DATASET "Dataset3" {
      DATATYPE { H5T_REFERENCE }
      DATASPACE { SIMPLE ( 4 ) / ( 4 ) }
      DATA {
         DATASET 0:1696, DATASET 0:2152, GROUP 0:1320, DATATYPE 0:2268
      }
   }
   GROUP "Group1" {
      DATASET "Dataset1" {
         DATATYPE { H5T_STD_U32BE }
         DATASPACE { SIMPLE ( 4 ) / ( 4 ) }
         DATA {
            0, 3, 6, 9
         }
      }
      DATASET "Dataset2" {
         DATATYPE { H5T_STD_U8BE }
         DATASPACE { SIMPLE ( 4 ) / ( 4 ) }
         DATA {
            0, 0, 0, 0
         }
      }
      DATATYPE "Datatype1" {
         H5T_STD_I32BE "a";
         H5T_IEEE_F32BE "b";
         H5T_IEEE_F32BE "c";
      }
   }
}
}
