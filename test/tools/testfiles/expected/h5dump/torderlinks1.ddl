HDF5 "tfcontents1.h5" {
GROUP "/" {
   DATATYPE "#5616" H5T_STD_I32BE;
   DATASET "dset" {
      DATATYPE  H5T_STD_I32BE
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): 1, 2, 3, 4
      }
   }
   DATASET "dset3" {
      HARDLINK "/dset"
   }
   DATASET "dset4" {
      HARDLINK "/dset"
   }
   DATASET "dsetmytype2" {
      DATATYPE  "/#5616"
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): 1, 2, 3, 4
      }
   }
   EXTERNAL_LINK "extlink" {
      TARGETFILE "fname"
      TARGETPATH "oname"
   }
   GROUP "g1" {
      DATASET "dset1" {
         HARDLINK "/dset"
      }
      GROUP "g1.1" {
         DATASET "dset2" {
            HARDLINK "/dset"
         }
      }
   }
   GROUP "g2" {
      HARDLINK "/g1/g1.1"
   }
   SOFTLINK "mylink" {
      LINKTARGET "mylink"
   }
   DATATYPE "mytype" H5T_STD_I32BE;
   SOFTLINK "softlink" {
      LINKTARGET "/dset"
   }
   USERDEFINED_LINK "udlink" {
      LINKCLASS 187
   }
}
}
