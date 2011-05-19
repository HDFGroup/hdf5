HDF5 "thlink.h5" {
DATASET "/dset1" {
   DATATYPE  H5T_STD_I32BE
   DATASPACE  SIMPLE { ( 5 ) / ( 5 ) }
   DATA {
   (0): 0, 1, 2, 3, 4
   }
}
GROUP "/g2" {
   DATASET "dset3" {
      HARDLINK "/dset1"
   }
}
DATASET "/g1/dset2" {
   HARDLINK "/dset1"
}
}
