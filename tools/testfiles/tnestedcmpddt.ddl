HDF5 "tnestedcmpddt.h5" {
GROUP "/" {
   DATASET "dset1" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I32BE "a_name";
         H5T_IEEE_F32BE "b_name";
      }
      DATASPACE  SIMPLE { ( 6 ) / ( H5S_UNLIMITED ) }
      DATA {
      (0): {
            0,
            0
         },
      (1): {
            16777216,
            4.6006e-41
         },
      (2): {
            33554432,
            4.60074e-41
         },
      (3): {
            50331648,
            5.8308e-42
         },
      (4): {
            67108864,
            4.60088e-41
         },
      (5): {
            83886080,
            7.18376e-41
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
            -1434300087,
            -1.69532e-21
         },
      (1): {
            -965921664,
            -7.63504e-06
         },
      (2): {
            0,
            1.01342e-41
         },
      (3): {
            485220918,
            2.64926e-23
         },
      (4): {
            970376557,
            1.19312e-07
         },
      (5): {
            1438754980,
            5.37334e+08
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
            H5T_ARRAY { [4] H5T_STD_I32BE } "int_name";
            H5T_ARRAY { [5][6] H5T_IEEE_F32BE } "float_name";
            H5T_COMPOUND {
               H5T_STD_I32BE "int_name";
               H5T_IEEE_F32BE "float_name";
            } "cmpd_name";
         }
         DATASPACE  SIMPLE { ( 6 ) / ( H5S_UNLIMITED ) }
         DATA {
         (0): {
               [ 0, 14745599, 0, 12648447 ],
               [ 0, 1.18468e-38, 0, 9.18341e-41, 0, 9.14754e-41,
                  0, 9.07579e-41, 0, 8.9323e-41, 0, 8.64531e-41,
                  0, 8.07134e-41, 0, 6.9234e-41, 0, 4.62751e-41,
                  0, 3.57331e-43, 0, 3.5593e-43, 0, 3.53127e-43,
                  0, 3.47522e-43, 0, 3.36312e-43, 0, 3.13891e-43 ],
               {
                  0,
                  0
               }
            },
         (1): {
               [ 16777216, 128, 0, 0 ],
               [ 4.6006e-41, 0, 0, 0, 0, 0,
                  0, 0, -8, -nan, -2.35099e-38, -nan,
                  2.35099e-38, -nan, 2.34181e-38, -nan, 2.32344e-38, -nan,
                  2.2867e-38, -nan, 2.21324e-38, -nan, 2.0663e-38, -nan,
                  1.77242e-38, -nan, 1.18468e-38, -nan, 9.18341e-41, -nan ],
               {
                  16777216,
                  4.6006e-41
               }
            },
         (2): {
               [ 33554432, -1, 63743, -1 ],
               [ 8.96831e-44, -nan, 8.07134e-41, -nan, 6.9234e-41, -nan,
                  4.62751e-41, -nan, 3.57331e-43, -nan, 3.5593e-43, -nan,
                  3.53127e-43, -nan, 3.47522e-43, -nan, 3.36312e-43, -nan,
                  3.13891e-43, -nan, 2.69049e-43, -nan, 1.79366e-43, -nan,
                  0, -nan, 0, -1.70141e+38, 0, -1.06338e+37 ],
               {
                  33554432,
                  8.96831e-44
               }
            },
         (3): {
               [ 50331648, -251658241, 0, -520093697 ],
               [ 2.30486e-41, -8, 0, -2.35099e-38, 0, 2.35099e-38,
                  0, 2.34181e-38, 0, 2.32344e-38, 0, 2.2867e-38,
                  0, 2.21324e-38, 0, 2.0663e-38, 0, 1.77242e-38,
                  0, 1.18468e-38, 0, 9.18341e-41, 0, 9.14754e-41,
                  0, 9.07579e-41, 0, 8.9323e-41, 0, 8.64531e-41 ],
               {
                  50331648,
                  2.30486e-41
               }
            },
         (4): {
               [ 67108864, 49407, 0, 33023 ],
               [ 4.60074e-41, 3.57331e-43, 0, 3.5593e-43, 0, 3.53127e-43,
                  0, 3.47522e-43, 0, 3.36312e-43, 0, 3.13891e-43,
                  0, 2.69049e-43, 0, 1.79366e-43, 0, 0,
                  0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, -2.35099e-38, -nan ],
               {
                  67108864,
                  4.60074e-41
               }
            },
         (5): {
               [ 83886080, -1, 16580607, -1 ],
               [ 5.74869e-41, -nan, 2.21324e-38, -nan, 2.0663e-38, -nan,
                  1.77242e-38, -nan, 1.18468e-38, -nan, 9.18341e-41, -nan,
                  9.14754e-41, -nan, 9.07579e-41, -nan, 8.9323e-41, -nan,
                  8.64531e-41, -nan, 8.07134e-41, -nan, 6.9234e-41, -nan,
                  4.62751e-41, -nan, 3.57331e-43, -nan, 3.5593e-43, -nan ],
               {
                  83886080,
                  5.74869e-41
               }
            }
         }
      }
   }
   DATATYPE "type1" H5T_COMPOUND {
      H5T_STD_I32BE "int_name";
      H5T_IEEE_F32BE "float_name";
   }
}
}
