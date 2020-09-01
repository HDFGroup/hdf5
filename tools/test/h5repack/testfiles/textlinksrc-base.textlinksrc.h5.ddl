HDF5 "out-textlinksrc-base.textlinksrc.h5" {
GROUP "/" {
   EXTERNAL_LINK "ext2soft_link1" {
      TARGETFILE "tsoftlinks.h5"
      TARGETPATH "/soft_dset1"
         DATASET "/soft_dset1" {
            DATATYPE  H5T_STD_I32BE
            DATASPACE  SIMPLE { ( 4, 2 ) / ( 4, 2 ) }
            STORAGE_LAYOUT {
               CONTIGUOUS
               SIZE 32
               OFFSET 2848
            }
            FILTERS {
               NONE
            }
            FILLVALUE {
               FILL_TIME H5D_FILL_TIME_IFSET
               VALUE  H5D_FILL_VALUE_DEFAULT
            }
            ALLOCATION_TIME {
               H5D_ALLOC_TIME_LATE
            }
         }
   }
   EXTERNAL_LINK "ext2softdangle_link1" {
      TARGETFILE "tsoftlinks.h5"
      TARGETPATH "/soft_dangle"
   }
   EXTERNAL_LINK "ext_link1" {
      TARGETFILE "textlinktar.h5"
      TARGETPATH "group"
         GROUP "group" {
            DATASET "dset" {
               DATATYPE  H5T_STD_I32LE
               DATASPACE  SIMPLE { ( 6 ) / ( 6 ) }
               STORAGE_LAYOUT {
                  CONTIGUOUS
                  SIZE 24
                  OFFSET 3136
               }
               FILTERS {
                  NONE
               }
               FILLVALUE {
                  FILL_TIME H5D_FILL_TIME_IFSET
                  VALUE  H5D_FILL_VALUE_DEFAULT
               }
               ALLOCATION_TIME {
                  H5D_ALLOC_TIME_LATE
               }
            }
            EXTERNAL_LINK "elink_t1" {
               TARGETFILE "textlinksrc.h5"
               TARGETPATH "/"
                  GROUP "/" {
                     EXTERNAL_LINK "ext2soft_link1" {
                        TARGETFILE "tsoftlinks.h5"
                        TARGETPATH "/soft_dset1"
                           DATASET "/soft_dset1" {
                              HARDLINK "/dset1"
                           }
                     }
                     EXTERNAL_LINK "ext2softdangle_link1" {
                        TARGETFILE "tsoftlinks.h5"
                        TARGETPATH "/soft_dangle"
                     }
                     EXTERNAL_LINK "ext_link1" {
                        TARGETFILE "textlinktar.h5"
                        TARGETPATH "group"
                           GROUP "group" {
                              HARDLINK "/group"
                           }
                     }
                     EXTERNAL_LINK "ext_link2" {
                        TARGETFILE "textlinktar.h5"
                        TARGETPATH "dset"
                           DATASET "dset" {
                              DATATYPE  H5T_STD_I32LE
                              DATASPACE  SIMPLE { ( 6 ) / ( 6 ) }
                              STORAGE_LAYOUT {
                                 CONTIGUOUS
                                 SIZE 24
                                 OFFSET 3160
                              }
                              FILTERS {
                                 NONE
                              }
                              FILLVALUE {
                                 FILL_TIME H5D_FILL_TIME_IFSET
                                 VALUE  H5D_FILL_VALUE_DEFAULT
                              }
                              ALLOCATION_TIME {
                                 H5D_ALLOC_TIME_LATE
                              }
                           }
                     }
                     EXTERNAL_LINK "ext_link3" {
                        TARGETFILE "textlinktar.h5"
                        TARGETPATH "type"
                           DATATYPE "type" H5T_STD_I32LE;
                     }
                     EXTERNAL_LINK "ext_link4" {
                        TARGETFILE "textlinktar.h5"
                        TARGETPATH "group/elink_t2"
                     }
                     EXTERNAL_LINK "ext_link5" {
                        TARGETFILE "textlinktar.h5"
                        TARGETPATH "empty_group"
                           GROUP "empty_group" {
                           }
                     }
                  }
            }
            EXTERNAL_LINK "elink_t2" {
               TARGETFILE "textlinksrc.h5"
               TARGETPATH "/ext_link4"
            }
            GROUP "subgroup" {
               GROUP "link_to_group" {
                  HARDLINK "/group"
               }
            }
         }
   }
   EXTERNAL_LINK "ext_link2" {
      TARGETFILE "textlinktar.h5"
      TARGETPATH "dset"
         DATASET "dset" {
            HARDLINK "/dset"
         }
   }
   EXTERNAL_LINK "ext_link3" {
      TARGETFILE "textlinktar.h5"
      TARGETPATH "type"
         DATATYPE "type" HARDLINK "/type"
   }
   EXTERNAL_LINK "ext_link4" {
      TARGETFILE "textlinktar.h5"
      TARGETPATH "group/elink_t2"
   }
   EXTERNAL_LINK "ext_link5" {
      TARGETFILE "textlinktar.h5"
      TARGETPATH "empty_group"
         GROUP "empty_group" {
            HARDLINK "/empty_group"
         }
   }
}
}
