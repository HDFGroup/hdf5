#############################
Expected output for 'h5dump -d /g1/link2 /dset /g1/link1/link3 thlink.h5'
#############################
HDF5 "thlink.h5" {
DATASET "/g1/link2" {
   h5dump error: unable to open /g1/link2
}
DATASET "/dset" {
   h5dump error: unable to open /dset
}
DATASET "/g1/link1/link3" {
   h5dump error: unable to open /g1/link1/link3
}
}
