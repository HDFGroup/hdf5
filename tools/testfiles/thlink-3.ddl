#############################
Expected output for 'h5dump -d /g1/g1.1/dset3 /g1/dset2 /dset1 thlink.h5'
#############################
HDF5 "thlink.h5" {
DATASET "/g1/g1.1/dset3" {
   DATATYPE { H5T_STD_I32BE }
   DATASPACE { SIMPLE ( 5 ) / ( 5 ) }
   DATA {
      0, 1, 2, 3, 4
   }
}
DATASET "/g1/dset2" {
   HARDLINK "/g1/g1.1/dset3"
}
DATASET "/dset1" {
   HARDLINK "/g1/g1.1/dset3"
}
}
