#############################
Expected output for 'h5dump -d /dset -g /g2 -d /g1/link2 thlink.h5'
#############################
HDF5 "thlink.h5" {
DATASET "/dset" {
   h5dump error: unable to open /dset
}
GROUP "/g2" {
   DATASET "dset3" {
      DATATYPE { "H5T_STD_I32BE" }
      DATASPACE { ARRAY ( 5 ) ( 5 ) }
      DATA {
         0, 1, 2, 3, 4
      }
   }
}
DATASET "/g1/link2" {
   h5dump error: unable to open /g1/link2
}
}
