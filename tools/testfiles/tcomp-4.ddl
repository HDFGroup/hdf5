#############################
Expected output for 'h5dump -t /#3432:0 -g /group2 tcompound.h5'
#############################
HDF5 "tcompound.h5" {
DATATYPE "/#3432:0" {
   h5dump error: unable to open /#3432:0
}
GROUP "/group2" {
   DATASET "dset5" {
      DATATYPE {
         HARDLINK { "#5992:0" }
      }
      DATASPACE { ARRAY ( 5 ) ( 5 ) }
      DATA {
         {0,0}, {1,0.1}, {2,0.2}, {3,0.3}, {4,0.4}
      }
   }
}
}
