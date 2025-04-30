HDF5 "tfcontents1.h5" {
GROUP "/" {
   DATATYPE "#5616" H5T_STD_I32BE;
   USERDEFINED_LINK "udlink" {
      LINKCLASS 187
   }
   SOFTLINK "softlink" {
      LINKTARGET "/dset"
   }
   DATATYPE "mytype" H5T_STD_I32BE;
   SOFTLINK "mylink" {
      LINKTARGET "mylink"
   }
   GROUP "g2" {
      DATASET "dset2" {
         DATATYPE  H5T_STD_I32BE
         DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
         DATA {
         (0): 1, 2, 3, 4
         }
      }
   }
   GROUP "g1" {
      GROUP "g1.1" {
         HARDLINK "/g2"
      }
      DATASET "dset1" {
         HARDLINK "/g2/dset2"
      }
   }
   EXTERNAL_LINK "extlink" {
      TARGETFILE "fname"
      TARGETPATH "oname"
   }
   DATASET "dsetmytype2" {
      DATATYPE  "/#5616"
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): 1, 2, 3, 4
      }
   }
   DATASET "dset4" {
      HARDLINK "/g2/dset2"
   }
   DATASET "dset3" {
      HARDLINK "/g2/dset2"
   }
   DATASET "dset" {
      HARDLINK "/g2/dset2"
   }
}
}
