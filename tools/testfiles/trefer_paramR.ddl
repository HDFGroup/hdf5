HDF5 "trefer_param.h5" {
GROUP "/" {
   DATASET "Dataset3" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
         NULL
         NULL
         NULL
         NULL
      }
   }
   GROUP "Group1" {
      COMMENT "Foo!"
      DATASET "Dataset1" {
         DATATYPE  H5T_STD_U32LE
         DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
         DATA {
         (0): 0, 3, 6, 9
         }
      }
      DATASET "Dataset2" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
         DATA {
         (0): 0, 0, 0, 0
         }
         ATTRIBUTE "Attr" {
            DATATYPE  H5T_STD_U32LE
            DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
            DATA {
            (0): 0, 3, 6, 9
            }
         }
      }
      DATATYPE "Datatype1" H5T_COMPOUND {
         H5T_STD_I32LE "a";
         H5T_STD_I32LE "b";
         H5T_IEEE_F32LE "c";
      }
   }
}
}
