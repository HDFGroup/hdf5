#############################
Expected output for '../h5dump -t /#3432:0 -g /group2 tcompound.h5'
#############################
HDF5 "tcompound.h5" {
DATATYPE "/#3432:0" {
   H5T_STD_I32BE int;
   H5T_IEEE_F32BE float;
}
GROUP "/group2" {
   DATASET "dset5" {
      DATATYPE {
         HARDLINK { "#3432:0" }
      }
      DATASPACE { ARRAY ( 5 ) ( 5 ) }
      DATA {
         {0,0}, {1,1}, {2,2}, {3,3}, {4,4}
      }
   }
}
}
