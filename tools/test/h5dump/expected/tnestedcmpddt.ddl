HDF5 "tnestedcmpddt.h5" {
GROUP "/" {
   DATASET "dset1" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I32LE "a_name";
         H5T_IEEE_F32LE "b_name";
      }
      DATASPACE  SIMPLE { ( 6 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            0,
            0
         },
      (1): {
            1,
            1
         },
      (2): {
            2,
            4
         },
      (3): {
            3,
            9
         },
      (4): {
            4,
            16
         },
      (5): {
            5,
            25
         }
      }
   }
   DATASET "dset2" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I32LE "a_name";
         H5T_IEEE_F32LE "b_name";
         H5T_ENUM {
            H5T_STD_I32LE;
            "Red"              0;
            "Green"            1;
            "Blue"             2;
            "White"            3;
            "Black"            4;
         } "c_name";
      }
      DATASPACE  SIMPLE { ( 6 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            0,
            0,
            Green
         },
      (1): {
            1,
            1.1,
            Green
         },
      (2): {
            2,
            2.2,
            Green
         },
      (3): {
            3,
            3.3,
            Green
         },
      (4): {
            4,
            4.4,
            Green
         },
      (5): {
            5,
            5.5,
            Green
         }
      }
   }
   DATASET "dset4" {
      DATATYPE  "/enumtype"
      DATASPACE  SIMPLE { ( 6 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): Red, Green, Blue, Green, White, Blue
      }
   }
   DATASET "dset5" {
      DATATYPE  "/type1"
      DATASPACE  SIMPLE { ( 6 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            0,
            0
         },
      (1): {
            1,
            1
         },
      (2): {
            2,
            4
         },
      (3): {
            3,
            9
         },
      (4): {
            4,
            16
         },
      (5): {
            5,
            25
         }
      }
   }
   DATATYPE "enumtype" H5T_ENUM {
      H5T_STD_I32LE;
      "Red"              0;
      "Green"            1;
      "Blue"             2;
      "White"            3;
      "Black"            4;
   };
   GROUP "group1" {
      DATASET "dset3" {
         DATATYPE  H5T_COMPOUND {
            H5T_ARRAY { [5] H5T_STD_I32LE } "int_name";
            H5T_ARRAY { [5][6] H5T_IEEE_F32LE } "float_name";
            H5T_COMPOUND {
               H5T_STD_I32LE "int_name";
               H5T_IEEE_F32LE "float_name";
            } "cmpd_name";
         }
         DATASPACE  SIMPLE { ( 6 ) / ( H5S_UNLIMITED ) }
         DATA {
         (0): {
               [ 0, 0, 0, 0, 0 ],
               [ 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0 ],
               {
                  0,
                  0
               }
            },
         (1): {
               [ 0, 1, 2, 3, 4 ],
               [ 0, 0, 0, 0, 0, 0,
                  0, 1, 2, 3, 4, 5,
                  0, 2, 4, 6, 8, 10,
                  0, 3, 6, 9, 12, 15,
                  0, 4, 8, 12, 16, 20 ],
               {
                  1,
                  1
               }
            },
         (2): {
               [ 0, 2, 4, 6, 8 ],
               [ 0, 0, 0, 0, 0, 0,
                  0, 2, 4, 6, 8, 10,
                  0, 4, 8, 12, 16, 20,
                  0, 6, 12, 18, 24, 30,
                  0, 8, 16, 24, 32, 40 ],
               {
                  2,
                  2
               }
            },
         (3): {
               [ 0, 3, 6, 9, 12 ],
               [ 0, 0, 0, 0, 0, 0,
                  0, 3, 6, 9, 12, 15,
                  0, 6, 12, 18, 24, 30,
                  0, 9, 18, 27, 36, 45,
                  0, 12, 24, 36, 48, 60 ],
               {
                  3,
                  3
               }
            },
         (4): {
               [ 0, 4, 8, 12, 16 ],
               [ 0, 0, 0, 0, 0, 0,
                  0, 4, 8, 12, 16, 20,
                  0, 8, 16, 24, 32, 40,
                  0, 12, 24, 36, 48, 60,
                  0, 16, 32, 48, 64, 80 ],
               {
                  4,
                  4
               }
            },
         (5): {
               [ 0, 5, 10, 15, 20 ],
               [ 0, 0, 0, 0, 0, 0,
                  0, 5, 10, 15, 20, 25,
                  0, 10, 20, 30, 40, 50,
                  0, 15, 30, 45, 60, 75,
                  0, 20, 40, 60, 80, 100 ],
               {
                  5,
                  5
               }
            }
         }
      }
   }
   DATATYPE "type1" H5T_COMPOUND {
      H5T_STD_I32LE "int_name";
      H5T_IEEE_F32LE "float_name";
   }
}
}
