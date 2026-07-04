HDF5 "trefer_ext2.h5" {
GROUP "/" {
   DATASET "Dataset3" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
         ATTRIBUTE "trefer_ext1.h5/Group1/Dataset1/Attr1" {
            DATATYPE  H5T_STD_U32LE
            DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
            DATA {
            (0): 0, 3, 6, 9
            }
         }
         ATTRIBUTE "trefer_ext1.h5/Group1/Dataset2/Attr1" {
            NULL
         }
         ATTRIBUTE "trefer_ext1.h5/Group1/Attr2" {
            DATATYPE  H5T_STD_U32LE
            DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
            DATA {
            (0): 1, 4, 7, 10
            }
         }
         ATTRIBUTE "trefer_ext1.h5/Group1/Datatype1/Attr3" {
            DATATYPE  H5T_STD_U32LE
            DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
            DATA {
            (0): 2, 5, 8, 11
            }
         }
      }
   }
}
}
