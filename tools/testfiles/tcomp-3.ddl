#############################
Expected output for 'h5dump -d /group2/dset5 -g /group1 tcompound.h5'
#############################
HDF5 "tcompound.h5" {
DATASET "/group2/dset5" {
   DATATYPE {
      HARDLINK { "#3432:0" }
   }
   DATASPACE { ARRAY ( 5 ) ( 5 ) }
   DATA {
      {0,0}, {1,1}, {2,2}, {3,3}, {4,4}
   }
}
GROUP "/group1" {
   DATASET "dset2" {
      DATATYPE {
         HARDLINK { "/type1" }
      }
      DATASPACE { ARRAY ( 5 ) ( 5 ) }
      DATA {
         {0,0}, {1,1.1}, {2,2.2}, {3,3.3}, {4,4.4}
      }
   }
   DATASET "dset4" {
      DATATYPE {
         HARDLINK { "/group1/type3" }
      }
      DATASPACE { ARRAY ( 5 ) ( 5 ) }
      DATA {
         {0,0}, {1,1}, {2,2}, {3,3}, {4,4}
      }
   }
   DATATYPE "type3" {
      H5T_STD_I32BE int;
      H5T_IEEE_F32BE float;
   }
}
}
