#############################
Expected output for 'h5dump -d /g1/link2 /dset /g1/link1/link3 thlink.h5'
#############################
HDF5 "thlink.h5" {
DATASET "/g1/link2" {
   DATATYPE { "H5T_STD_I32BE" }
   DATASPACE { ARRAY ( 5 ) ( 5 ) }
   DATA {
      0, 1, 2, 3, 4
   }
}
DATASET "/dset" {
   HARDLINK { "/g1/link2" }
}
DATASET "/g1/link1/link3" {
   HARDLINK { "/g1/link2" }
}
}
