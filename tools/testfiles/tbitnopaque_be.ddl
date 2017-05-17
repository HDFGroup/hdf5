HDF5 "tbitnopaque.h5" {
GROUP "/" {
   GROUP "bittypetests" {
      DATASET "bitfield_1" {
         DATATYPE  H5T_STD_B8LE
         DATASPACE  SIMPLE { ( 32 ) / ( 32 ) }
         DATA {
         (0): 0xff, 0xfe, 0xfd, 0xfc, 0xfb, 0xfa, 0xf9, 0xf8, 0xf7, 0xf6,
         (10): 0xf5, 0xf4, 0xf3, 0xf2, 0xf1, 0xf0, 0xef, 0xee, 0xed, 0xec,
         (20): 0xeb, 0xea, 0xe9, 0xe8, 0xe7, 0xe6, 0xe5, 0xe4, 0xe3, 0xe2,
         (30): 0xe1, 0xe0
         }
      }
      DATASET "bitfield_2" {
         DATATYPE  H5T_STD_B16LE
         DATASPACE  SIMPLE { ( 32 ) / ( 32 ) }
         DATA {
         (0): ff:ff, ff:ef, ff:df, ff:cf, ff:bf, ff:af, ff:9f, ff:8f, ff:7f,
         (9): ff:6f, ff:5f, ff:4f, ff:3f, ff:2f, ff:1f, ff:0f, fe:ff, fe:ef,
         (18): fe:df, fe:cf, fe:bf, fe:af, fe:9f, fe:8f, fe:7f, fe:6f, fe:5f,
         (27): fe:4f, fe:3f, fe:2f, fe:1f, fe:0f
         }
      }
      DATASET "bitfield_3" {
         DATATYPE  H5T_STD_B32LE
         DATASPACE  SIMPLE { ( 32 ) / ( 32 ) }
         DATA {
         (0): ff:ff:ff:ff, ff:ff:ff:df, ff:ff:ff:bf, ff:ff:ff:9f,
         (4): ff:ff:ff:7f, ff:ff:ff:5f, ff:ff:ff:3f, ff:ff:ff:1f,
         (8): ff:ff:fe:ff, ff:ff:fe:df, ff:ff:fe:bf, ff:ff:fe:9f,
         (12): ff:ff:fe:7f, ff:ff:fe:5f, ff:ff:fe:3f, ff:ff:fe:1f,
         (16): ff:ff:fd:ff, ff:ff:fd:df, ff:ff:fd:bf, ff:ff:fd:9f,
         (20): ff:ff:fd:7f, ff:ff:fd:5f, ff:ff:fd:3f, ff:ff:fd:1f,
         (24): ff:ff:fc:ff, ff:ff:fc:df, ff:ff:fc:bf, ff:ff:fc:9f,
         (28): ff:ff:fc:7f, ff:ff:fc:5f, ff:ff:fc:3f, ff:ff:fc:1f
         }
      }
      DATASET "bitfield_4" {
         DATATYPE  H5T_STD_B64LE
         DATASPACE  SIMPLE { ( 32 ) / ( 32 ) }
         DATA {
         (0): ff:ff:ff:ff:ff:ff:ff:ff, ff:ff:ff:ff:ff:ff:ff:bf,
         (2): ff:ff:ff:ff:ff:ff:ff:7f, ff:ff:ff:ff:ff:ff:ff:3f,
         (4): ff:ff:ff:ff:ff:ff:fe:ff, ff:ff:ff:ff:ff:ff:fe:bf,
         (6): ff:ff:ff:ff:ff:ff:fe:7f, ff:ff:ff:ff:ff:ff:fe:3f,
         (8): ff:ff:ff:ff:ff:ff:fd:ff, ff:ff:ff:ff:ff:ff:fd:bf,
         (10): ff:ff:ff:ff:ff:ff:fd:7f, ff:ff:ff:ff:ff:ff:fd:3f,
         (12): ff:ff:ff:ff:ff:ff:fc:ff, ff:ff:ff:ff:ff:ff:fc:bf,
         (14): ff:ff:ff:ff:ff:ff:fc:7f, ff:ff:ff:ff:ff:ff:fc:3f,
         (16): ff:ff:ff:ff:ff:ff:fb:ff, ff:ff:ff:ff:ff:ff:fb:bf,
         (18): ff:ff:ff:ff:ff:ff:fb:7f, ff:ff:ff:ff:ff:ff:fb:3f,
         (20): ff:ff:ff:ff:ff:ff:fa:ff, ff:ff:ff:ff:ff:ff:fa:bf,
         (22): ff:ff:ff:ff:ff:ff:fa:7f, ff:ff:ff:ff:ff:ff:fa:3f,
         (24): ff:ff:ff:ff:ff:ff:f9:ff, ff:ff:ff:ff:ff:ff:f9:bf,
         (26): ff:ff:ff:ff:ff:ff:f9:7f, ff:ff:ff:ff:ff:ff:f9:3f,
         (28): ff:ff:ff:ff:ff:ff:f8:ff, ff:ff:ff:ff:ff:ff:f8:bf,
         (30): ff:ff:ff:ff:ff:ff:f8:7f, ff:ff:ff:ff:ff:ff:f8:3f
         }
      }
   }
   GROUP "cmpdtypetests" {
      DATASET "compound_1" {
         DATATYPE  H5T_COMPOUND {
            H5T_STD_B8LE "a";
            H5T_STD_B16LE "b";
            H5T_STD_B32LE "c";
            H5T_STD_B64LE "d";
         }
         DATASPACE  SIMPLE { ( 32 ) / ( 32 ) }
         DATA {
         (0): {
               0xff,
               ff:ff,
               ff:ff:ff:ff,
               ff:ff:ff:ff:ff:ff:ff:ff
            },
         (1): {
               0xfe,
               ff:ef,
               ff:ff:ff:df,
               ff:ff:ff:ff:ff:ff:ff:bf
            },
         (2): {
               0xfd,
               ff:df,
               ff:ff:ff:bf,
               ff:ff:ff:ff:ff:ff:ff:7f
            },
         (3): {
               0xfc,
               ff:cf,
               ff:ff:ff:9f,
               ff:ff:ff:ff:ff:ff:ff:3f
            },
         (4): {
               0xfb,
               ff:bf,
               ff:ff:ff:7f,
               ff:ff:ff:ff:ff:ff:fe:ff
            },
         (5): {
               0xfa,
               ff:af,
               ff:ff:ff:5f,
               ff:ff:ff:ff:ff:ff:fe:bf
            },
         (6): {
               0xf9,
               ff:9f,
               ff:ff:ff:3f,
               ff:ff:ff:ff:ff:ff:fe:7f
            },
         (7): {
               0xf8,
               ff:8f,
               ff:ff:ff:1f,
               ff:ff:ff:ff:ff:ff:fe:3f
            },
         (8): {
               0xf7,
               ff:7f,
               ff:ff:fe:ff,
               ff:ff:ff:ff:ff:ff:fd:ff
            },
         (9): {
               0xf6,
               ff:6f,
               ff:ff:fe:df,
               ff:ff:ff:ff:ff:ff:fd:bf
            },
         (10): {
               0xf5,
               ff:5f,
               ff:ff:fe:bf,
               ff:ff:ff:ff:ff:ff:fd:7f
            },
         (11): {
               0xf4,
               ff:4f,
               ff:ff:fe:9f,
               ff:ff:ff:ff:ff:ff:fd:3f
            },
         (12): {
               0xf3,
               ff:3f,
               ff:ff:fe:7f,
               ff:ff:ff:ff:ff:ff:fc:ff
            },
         (13): {
               0xf2,
               ff:2f,
               ff:ff:fe:5f,
               ff:ff:ff:ff:ff:ff:fc:bf
            },
         (14): {
               0xf1,
               ff:1f,
               ff:ff:fe:3f,
               ff:ff:ff:ff:ff:ff:fc:7f
            },
         (15): {
               0xf0,
               ff:0f,
               ff:ff:fe:1f,
               ff:ff:ff:ff:ff:ff:fc:3f
            },
         (16): {
               0xef,
               fe:ff,
               ff:ff:fd:ff,
               ff:ff:ff:ff:ff:ff:fb:ff
            },
         (17): {
               0xee,
               fe:ef,
               ff:ff:fd:df,
               ff:ff:ff:ff:ff:ff:fb:bf
            },
         (18): {
               0xed,
               fe:df,
               ff:ff:fd:bf,
               ff:ff:ff:ff:ff:ff:fb:7f
            },
         (19): {
               0xec,
               fe:cf,
               ff:ff:fd:9f,
               ff:ff:ff:ff:ff:ff:fb:3f
            },
         (20): {
               0xeb,
               fe:bf,
               ff:ff:fd:7f,
               ff:ff:ff:ff:ff:ff:fa:ff
            },
         (21): {
               0xea,
               fe:af,
               ff:ff:fd:5f,
               ff:ff:ff:ff:ff:ff:fa:bf
            },
         (22): {
               0xe9,
               fe:9f,
               ff:ff:fd:3f,
               ff:ff:ff:ff:ff:ff:fa:7f
            },
         (23): {
               0xe8,
               fe:8f,
               ff:ff:fd:1f,
               ff:ff:ff:ff:ff:ff:fa:3f
            },
         (24): {
               0xe7,
               fe:7f,
               ff:ff:fc:ff,
               ff:ff:ff:ff:ff:ff:f9:ff
            },
         (25): {
               0xe6,
               fe:6f,
               ff:ff:fc:df,
               ff:ff:ff:ff:ff:ff:f9:bf
            },
         (26): {
               0xe5,
               fe:5f,
               ff:ff:fc:bf,
               ff:ff:ff:ff:ff:ff:f9:7f
            },
         (27): {
               0xe4,
               fe:4f,
               ff:ff:fc:9f,
               ff:ff:ff:ff:ff:ff:f9:3f
            },
         (28): {
               0xe3,
               fe:3f,
               ff:ff:fc:7f,
               ff:ff:ff:ff:ff:ff:f8:ff
            },
         (29): {
               0xe2,
               fe:2f,
               ff:ff:fc:5f,
               ff:ff:ff:ff:ff:ff:f8:bf
            },
         (30): {
               0xe1,
               fe:1f,
               ff:ff:fc:3f,
               ff:ff:ff:ff:ff:ff:f8:7f
            },
         (31): {
               0xe0,
               fe:0f,
               ff:ff:fc:1f,
               ff:ff:ff:ff:ff:ff:f8:3f
            }
         }
      }
   }
   GROUP "opaquetypetests" {
      DATASET "opaque_1" {
         DATATYPE  H5T_OPAQUE {
            OPAQUE_TAG "1-byte opaque type";
         }
         DATASPACE  SIMPLE { ( 32 ) / ( 32 ) }
         DATA {
         (0): 0xff, 0xfe, 0xfd, 0xfc, 0xfb, 0xfa, 0xf9, 0xf8, 0xf7, 0xf6,
         (10): 0xf5, 0xf4, 0xf3, 0xf2, 0xf1, 0xf0, 0xef, 0xee, 0xed, 0xec,
         (20): 0xeb, 0xea, 0xe9, 0xe8, 0xe7, 0xe6, 0xe5, 0xe4, 0xe3, 0xe2,
         (30): 0xe1, 0xe0
         }
      }
      DATASET "opaque_2" {
         DATATYPE  H5T_OPAQUE {
            OPAQUE_TAG "2-byte opaque type";
         }
         DATASPACE  SIMPLE { ( 32 ) / ( 32 ) }
         DATA {
         (0): ff:ff, ef:ff, df:ff, cf:ff, bf:ff, af:ff, 9f:ff, 8f:ff, 7f:ff,
         (9): 6f:ff, 5f:ff, 4f:ff, 3f:ff, 2f:ff, 1f:ff, 0f:ff, ff:fe, ef:fe,
         (18): df:fe, cf:fe, bf:fe, af:fe, 9f:fe, 8f:fe, 7f:fe, 6f:fe, 5f:fe,
         (27): 4f:fe, 3f:fe, 2f:fe, 1f:fe, 0f:fe
         }
      }
   }
}
}
