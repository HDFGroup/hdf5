HDF5 "trefer_attr.h5" {
GROUP "/" {
   DATASET "Dataset3" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
         ATTRIBUTE "trefer_attr.h5/Group1/Dataset1/Attr1" {
            DATATYPE  H5T_STD_U32LE
            DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
            DATA {
            (0): 0, 3, 6, 9
            }
         }
         ATTRIBUTE "trefer_attr.h5/Group1/Dataset2/Attr1" {
            NULL
         }
         ATTRIBUTE "trefer_attr.h5/Group1/Attr2" {
            DATATYPE  H5T_STD_U32LE
            DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
            DATA {
            (0): 1, 4, 7, 10
            }
         }
         ATTRIBUTE "trefer_attr.h5/Group1/Datatype1/Attr3" {
            DATATYPE  H5T_STD_U32LE
            DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
            DATA {
            (0): 2, 5, 8, 11
            }
         }
      }
   }
   GROUP "Group1" {
      ATTRIBUTE "Attr2" {
         DATATYPE  H5T_STD_U32LE
         DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
         DATA {
         (0): 1, 4, 7, 10
         }
      }
      DATASET "Dataset1" {
         DATATYPE  H5T_STD_U32LE
         DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
         DATA {
         (0): 0, 0, 0, 0
         }
         ATTRIBUTE "Attr1" {
            DATATYPE  H5T_STD_U32LE
            DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
            DATA {
            (0): 0, 3, 6, 9
            }
         }
      }
      DATASET "Dataset2" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
         DATA {
         (0): 0, 0, 0, 0
         }
      }
      DATATYPE "Datatype1" H5T_COMPOUND {
         H5T_STD_I32LE "a";
         H5T_STD_I32LE "b";
         H5T_IEEE_F32LE "c";
      }
         ATTRIBUTE "Attr3" {
            DATATYPE  H5T_STD_U32LE
            DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
            DATA {
            (0): 2, 5, 8, 11
            }
         }
   }
}
}
