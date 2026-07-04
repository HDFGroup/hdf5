HDF5 "trefer_obj.h5" {
GROUP "/" {
   DATASET "Dataset3" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF }
      DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
      DATA {
         DATASET "trefer_obj.h5/Group1/Dataset1"
            DATA {
            (0): 0
            }
      }
   }
   DATASET "Dataset4" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF_OBJECT }
      DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
      DATA {
         DATASET "trefer_obj.h5/Group1/Dataset1"
            DATA {
            (0): 0
            }
      }
   }
   DATASET "Dataset5" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF }
      DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
      DATA {
         DATASET "trefer_obj.h5/Group1/Dataset1"
      }
   }
   DATASET "Dataset6" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF_DSETREG }
      DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
      DATA {
         DATASET "trefer_obj.h5/Group1/Dataset1"
      }
   }
   GROUP "Group1" {
      DATASET "Dataset1" {
         DATATYPE  H5T_STD_U32LE
         DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
         DATA {
         (0): 0
         }
      }
      DATASET "Dataset2" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
         DATA {
         (0): 0
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
