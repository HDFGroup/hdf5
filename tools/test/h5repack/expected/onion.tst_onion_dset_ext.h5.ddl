HDF5 "out-onion.tst_onion_dset_ext.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 8, 4 ) / ( H5S_UNLIMITED, H5S_UNLIMITED ) }
      DATA {
      (0,0): 0, 1, 2, 3,
      (1,0): 1, 2, 3, 4,
      (2,0): 2, 3, 4, 5,
      (3,0): 3, 4, 5, 6,
      (4,0): 0, 1, 2, 3,
      (5,0): 1, 2, 3, 4,
      (6,0): 2, 3, 4, 5,
      (7,0): 3, 4, 5, 6
      }
   }
}
}
