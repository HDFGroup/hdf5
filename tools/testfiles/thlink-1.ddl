#############################
Expected output for 'h5dump thlink.h5'
#############################
HDF5 "thlink.h5" {
GROUP "/" {
   DATASET "dset" {
      DATATYPE { "H5T_STD_I32BE" }
      DATASPACE { ARRAY ( 5 ) ( 5 ) }
      DATA {
         0, 1, 2, 3, 4
      }
   }
   GROUP "g1" {
      GROUP "link1" {
         DATASET "link3" {
            HARDLINK { "/dset" }
         }
      }
      DATASET "link2" {
         HARDLINK { "/dset" }
      }
   }
   GROUP "g2" {
      HARDLINK { "/g1/link1" }
   }
}
}
