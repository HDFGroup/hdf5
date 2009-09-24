#############################
Expected output for 'h5dump -p -H tdset_idx.h5'
#############################
HDF5 "tdset_idx.h5" {
GROUP "/" {
   DATASET "dset_btree" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 20, 10 ) / ( 200, 100 ) }
      STORAGE_LAYOUT {
         CHUNKED ( 5, 5 )
         SIZE 800
       }
      FILTERS {
         NONE
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_IFSET
         VALUE  0      
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_INCR
      }
   }
   DATASET "dset_filter" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
      STORAGE_LAYOUT {
         CHUNKED ( 5, 5 )
         SIZE 200 (4.000:1 COMPRESSION)
       }
      FILTERS {
         COMPRESSION DEFLATE { LEVEL 9 }
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_IFSET
         VALUE  0      
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_INCR
      }
   }
   DATASET "dset_fixed" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
      STORAGE_LAYOUT {
         CHUNKED ( 5, 5 )
         SIZE 800
       }
      FILTERS {
         NONE
      }
      FILLVALUE {
         FILL_TIME H5D_FILL_TIME_IFSET
         VALUE  0      
      }
      ALLOCATION_TIME {
         H5D_ALLOC_TIME_INCR
      }
   }
}
}
