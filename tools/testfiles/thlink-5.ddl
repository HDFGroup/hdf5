#############################
Expected output for '../h5dump -d /dset -g /g2 -d /g1/link2 thlink.h5'
#############################
HDF5 "thlink.h5" {
DATASET "/dset" {
   DATATYPE { "H5T_STD_I32BE" }
   DATASPACE { ARRAY ( 5 ) ( 5 ) }
   DATA {
      0, 1, 2, 3, 4
   }
}
GROUP "/g2" {
   DATASET "link3" {
      HARDLINK { "/dset" }
   }
}
DATASET "/g1/link2" {
   HARDLINK { "/dset" }
}
}
