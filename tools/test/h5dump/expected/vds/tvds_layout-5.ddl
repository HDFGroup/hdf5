HDF5 "5_vds.h5" {
GROUP "/" {
   DATASET "vds_dset" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 9, 4, 4 ) / ( H5S_UNLIMITED, 4, 4 ) }
      STORAGE_LAYOUT {
         MAPPING 0 { 
            VIRTUAL {
               SELECTION REGULAR_HYPERSLAB { 
                  START (0,0,0)
                  STRIDE (3,1,1)
                  COUNT (H5S_UNLIMITED,1,1)
                  BLOCK (1,4,4)
               }
            }
            SOURCE {
               FILE "5_a.h5"
               DATASET "/source_dset"
               SELECTION REGULAR_HYPERSLAB { 
                  START (0,0,0)
                  STRIDE (1,1,1)
                  COUNT (1,1,1)
                  BLOCK (H5S_UNLIMITED,4,4)
               }
            }
         }
         MAPPING 1 { 
            VIRTUAL {
               SELECTION REGULAR_HYPERSLAB { 
                  START (1,0,0)
                  STRIDE (3,1,1)
                  COUNT (H5S_UNLIMITED,1,1)
                  BLOCK (1,4,4)
               }
            }
            SOURCE {
               FILE "5_b.h5"
               DATASET "/source_dset"
               SELECTION REGULAR_HYPERSLAB { 
                  START (0,0,0)
                  STRIDE (1,1,1)
                  COUNT (1,1,1)
                  BLOCK (H5S_UNLIMITED,4,4)
               }
            }
         }
         MAPPING 2 { 
            VIRTUAL {
               SELECTION REGULAR_HYPERSLAB { 
                  START (2,0,0)
                  STRIDE (3,1,1)
                  COUNT (H5S_UNLIMITED,1,1)
                  BLOCK (1,4,4)
               }
            }
            SOURCE {
               FILE "5_c.h5"
               DATASET "/source_dset"
               SELECTION REGULAR_HYPERSLAB { 
                  START (0,0,0)
                  STRIDE (1,1,1)
                  COUNT (1,1,1)
                  BLOCK (H5S_UNLIMITED,4,4)
               }
            }
         }
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_IFSET
         VALUE  -9
      }
      DATA {
      (0,0,0): 10, 10, 10, 10,
      (0,1,0): 10, 10, 10, 10,
      (0,2,0): 10, 10, 10, 10,
      (0,3,0): 10, 10, 10, 10,
      (1,0,0): 20, 20, 20, 20,
      (1,1,0): 20, 20, 20, 20,
      (1,2,0): 20, 20, 20, 20,
      (1,3,0): 20, 20, 20, 20,
      (2,0,0): 30, 30, 30, 30,
      (2,1,0): 30, 30, 30, 30,
      (2,2,0): 30, 30, 30, 30,
      (2,3,0): 30, 30, 30, 30,
      (3,0,0): 11, 11, 11, 11,
      (3,1,0): 11, 11, 11, 11,
      (3,2,0): 11, 11, 11, 11,
      (3,3,0): 11, 11, 11, 11,
      (4,0,0): 21, 21, 21, 21,
      (4,1,0): 21, 21, 21, 21,
      (4,2,0): 21, 21, 21, 21,
      (4,3,0): 21, 21, 21, 21,
      (5,0,0): 31, 31, 31, 31,
      (5,1,0): 31, 31, 31, 31,
      (5,2,0): 31, 31, 31, 31,
      (5,3,0): 31, 31, 31, 31,
      (6,0,0): 12, 12, 12, 12,
      (6,1,0): 12, 12, 12, 12,
      (6,2,0): 12, 12, 12, 12,
      (6,3,0): 12, 12, 12, 12,
      (7,0,0): 22, 22, 22, 22,
      (7,1,0): 22, 22, 22, 22,
      (7,2,0): 22, 22, 22, 22,
      (7,3,0): 22, 22, 22, 22,
      (8,0,0): 32, 32, 32, 32,
      (8,1,0): 32, 32, 32, 32,
      (8,2,0): 32, 32, 32, 32,
      (8,3,0): 32, 32, 32, 32
      }
   }
}
}
