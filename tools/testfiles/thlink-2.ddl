#############################
Expected output for 'h5dump -d /g1/dset2 /dset1 /g1/g1.1/dset3 thlink.h5'
#############################
HDF5 "thlink.h5" {
DATASET "/g1/dset2" {
   DATATYPE { H5T_STD_I32BE }
   DATASPACE { SIMPLE ( 5 ) / ( 5 ) }
   DATA {
      0, 1, 2, 3, 4
   }
}
DATASET "/dset1" {
   HARDLINK "/g1/dset2"
}
DATASET "/g1/g1.1/dset3" {
   HARDLINK "/g1/dset2"
}
}
