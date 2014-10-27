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
         (0): ff:ff, 00:00, ef:ff, 00:00, df:ff, 00:00, cf:ff, 00:00, bf:ff,
         (9): 00:00, af:ff, 00:00, 9f:ff, 00:00, 8f:ff, 00:00, 7f:ff, 00:00,
         (18): 6f:ff, 00:00, 5f:ff, 00:00, 4f:ff, 00:00, 3f:ff, 00:00, 2f:ff,
         (27): 00:00, 1f:ff, 00:00, 0f:ff, 00:00
         }
      }
      DATASET "bitfield_3" {
         DATATYPE  H5T_STD_B32LE
         DATASPACE  SIMPLE { ( 32 ) / ( 32 ) }
         DATA {
         (0): ff:ff:ff:ff, 00:00:00:00, df:ff:ff:ff, 00:00:00:00,
         (4): bf:ff:ff:ff, 00:00:00:00, 9f:ff:ff:ff, 00:00:00:00,
         (8): 7f:ff:ff:ff, 00:00:00:00, 5f:ff:ff:ff, 00:00:00:00,
         (12): 3f:ff:ff:ff, 00:00:00:00, 1f:ff:ff:ff, 00:00:00:00,
         (16): ff:fe:ff:ff, 00:00:00:00, df:fe:ff:ff, 00:00:00:00,
         (20): bf:fe:ff:ff, 00:00:00:00, 9f:fe:ff:ff, 00:00:00:00,
         (24): 7f:fe:ff:ff, 00:00:00:00, 5f:fe:ff:ff, 00:00:00:00,
         (28): 3f:fe:ff:ff, 00:00:00:00, 1f:fe:ff:ff, 00:00:00:00
         }
      }
      DATASET "bitfield_4" {
         DATATYPE  H5T_STD_B64LE
         DATASPACE  SIMPLE { ( 32 ) / ( 32 ) }
         DATA {
         (0): ff:ff:ff:ff:ff:ff:ff:ff, bf:ff:ff:ff:ff:ff:ff:ff,
         (2): 7f:ff:ff:ff:ff:ff:ff:ff, 3f:ff:ff:ff:ff:ff:ff:ff,
         (4): ff:fe:ff:ff:ff:ff:ff:ff, bf:fe:ff:ff:ff:ff:ff:ff,
         (6): 7f:fe:ff:ff:ff:ff:ff:ff, 3f:fe:ff:ff:ff:ff:ff:ff,
         (8): ff:fd:ff:ff:ff:ff:ff:ff, bf:fd:ff:ff:ff:ff:ff:ff,
         (10): 7f:fd:ff:ff:ff:ff:ff:ff, 3f:fd:ff:ff:ff:ff:ff:ff,
         (12): ff:fc:ff:ff:ff:ff:ff:ff, bf:fc:ff:ff:ff:ff:ff:ff,
         (14): 7f:fc:ff:ff:ff:ff:ff:ff, 3f:fc:ff:ff:ff:ff:ff:ff,
         (16): ff:fb:ff:ff:ff:ff:ff:ff, bf:fb:ff:ff:ff:ff:ff:ff,
         (18): 7f:fb:ff:ff:ff:ff:ff:ff, 3f:fb:ff:ff:ff:ff:ff:ff,
         (20): ff:fa:ff:ff:ff:ff:ff:ff, bf:fa:ff:ff:ff:ff:ff:ff,
         (22): 7f:fa:ff:ff:ff:ff:ff:ff, 3f:fa:ff:ff:ff:ff:ff:ff,
         (24): ff:f9:ff:ff:ff:ff:ff:ff, bf:f9:ff:ff:ff:ff:ff:ff,
         (26): 7f:f9:ff:ff:ff:ff:ff:ff, 3f:f9:ff:ff:ff:ff:ff:ff,
         (28): ff:f8:ff:ff:ff:ff:ff:ff, bf:f8:ff:ff:ff:ff:ff:ff,
         (30): 7f:f8:ff:ff:ff:ff:ff:ff, 3f:f8:ff:ff:ff:ff:ff:ff
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
               ef:ff,
               df:ff:ff:ff,
               bf:ff:ff:ff:ff:ff:ff:ff
            },
         (2): {
               0xfd,
               df:ff,
               bf:ff:ff:ff,
               7f:ff:ff:ff:ff:ff:ff:ff
            },
         (3): {
               0xfc,
               cf:ff,
               9f:ff:ff:ff,
               3f:ff:ff:ff:ff:ff:ff:ff
            },
         (4): {
               0xfb,
               bf:ff,
               7f:ff:ff:ff,
               ff:fe:ff:ff:ff:ff:ff:ff
            },
         (5): {
               0xfa,
               af:ff,
               5f:ff:ff:ff,
               bf:fe:ff:ff:ff:ff:ff:ff
            },
         (6): {
               0xf9,
               9f:ff,
               3f:ff:ff:ff,
               7f:fe:ff:ff:ff:ff:ff:ff
            },
         (7): {
               0xf8,
               8f:ff,
               1f:ff:ff:ff,
               3f:fe:ff:ff:ff:ff:ff:ff
            },
         (8): {
               0xf7,
               7f:ff,
               ff:fe:ff:ff,
               ff:fd:ff:ff:ff:ff:ff:ff
            },
         (9): {
               0xf6,
               6f:ff,
               df:fe:ff:ff,
               bf:fd:ff:ff:ff:ff:ff:ff
            },
         (10): {
               0xf5,
               5f:ff,
               bf:fe:ff:ff,
               7f:fd:ff:ff:ff:ff:ff:ff
            },
         (11): {
               0xf4,
               4f:ff,
               9f:fe:ff:ff,
               3f:fd:ff:ff:ff:ff:ff:ff
            },
         (12): {
               0xf3,
               3f:ff,
               7f:fe:ff:ff,
               ff:fc:ff:ff:ff:ff:ff:ff
            },
         (13): {
               0xf2,
               2f:ff,
               5f:fe:ff:ff,
               bf:fc:ff:ff:ff:ff:ff:ff
            },
         (14): {
               0xf1,
               1f:ff,
               3f:fe:ff:ff,
               7f:fc:ff:ff:ff:ff:ff:ff
            },
         (15): {
               0xf0,
               0f:ff,
               1f:fe:ff:ff,
               3f:fc:ff:ff:ff:ff:ff:ff
            },
         (16): {
               0xef,
               ff:fe,
               ff:fd:ff:ff,
               ff:fb:ff:ff:ff:ff:ff:ff
            },
         (17): {
               0xee,
               ef:fe,
               df:fd:ff:ff,
               bf:fb:ff:ff:ff:ff:ff:ff
            },
         (18): {
               0xed,
               df:fe,
               bf:fd:ff:ff,
               7f:fb:ff:ff:ff:ff:ff:ff
            },
         (19): {
               0xec,
               cf:fe,
               9f:fd:ff:ff,
               3f:fb:ff:ff:ff:ff:ff:ff
            },
         (20): {
               0xeb,
               bf:fe,
               7f:fd:ff:ff,
               ff:fa:ff:ff:ff:ff:ff:ff
            },
         (21): {
               0xea,
               af:fe,
               5f:fd:ff:ff,
               bf:fa:ff:ff:ff:ff:ff:ff
            },
         (22): {
               0xe9,
               9f:fe,
               3f:fd:ff:ff,
               7f:fa:ff:ff:ff:ff:ff:ff
            },
         (23): {
               0xe8,
               8f:fe,
               1f:fd:ff:ff,
               3f:fa:ff:ff:ff:ff:ff:ff
            },
         (24): {
               0xe7,
               7f:fe,
               ff:fc:ff:ff,
               ff:f9:ff:ff:ff:ff:ff:ff
            },
         (25): {
               0xe6,
               6f:fe,
               df:fc:ff:ff,
               bf:f9:ff:ff:ff:ff:ff:ff
            },
         (26): {
               0xe5,
               5f:fe,
               bf:fc:ff:ff,
               7f:f9:ff:ff:ff:ff:ff:ff
            },
         (27): {
               0xe4,
               4f:fe,
               9f:fc:ff:ff,
               3f:f9:ff:ff:ff:ff:ff:ff
            },
         (28): {
               0xe3,
               3f:fe,
               7f:fc:ff:ff,
               ff:f8:ff:ff:ff:ff:ff:ff
            },
         (29): {
               0xe2,
               2f:fe,
               5f:fc:ff:ff,
               bf:f8:ff:ff:ff:ff:ff:ff
            },
         (30): {
               0xe1,
               1f:fe,
               3f:fc:ff:ff,
               7f:f8:ff:ff:ff:ff:ff:ff
            },
         (31): {
               0xe0,
               0f:fe,
               1f:fc:ff:ff,
               3f:f8:ff:ff:ff:ff:ff:ff
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
         (0): ff:ff, 00:00, ef:ff, 00:00, df:ff, 00:00, cf:ff, 00:00, bf:ff,
         (9): 00:00, af:ff, 00:00, 9f:ff, 00:00, 8f:ff, 00:00, 7f:ff, 00:00,
         (18): 6f:ff, 00:00, 5f:ff, 00:00, 4f:ff, 00:00, 3f:ff, 00:00, 2f:ff,
         (27): 00:00, 1f:ff, 00:00, 0f:ff, 00:00
         }
      }
   }
}
}
