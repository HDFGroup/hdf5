#############################
Expected output for 'h5dump -A tnamed_dtype_attr.h5'
#############################
HDF5 "tnamed_dtype_attr.h5" {
GROUP "/" {
   DATASET "Dataset" {
      DATATYPE  "/Datatype"
      DATASPACE  SCALAR
      ATTRIBUTE "Attribute" {
         DATATYPE  "/Datatype"
         DATASPACE  SCALAR
         DATA {
         (0): 8
         }
      }
   }
   DATATYPE "Datatype"       H5T_STD_I32LE;

}
}
