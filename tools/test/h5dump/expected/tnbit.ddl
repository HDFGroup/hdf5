HDF5 "tfilters.h5" {
DATASET "nbit" {
   DATATYPE  32-bit little-endian integer 17-bit precision
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE XXXX (1.XXX:1 COMPRESSION)
   }
   FILTERS {
      COMPRESSION NBIT {
         PARAMS_STRING "cd_values=8:0:50:1:4:0:17:0"
         DESCRIPTION "N-bit packing for non-byte-aligned integer/float storage"
      }
   }
   FILLVALUE {
      FILL_TIME H5D_FILL_TIME_IFSET
      VALUE  H5D_FILL_VALUE_DEFAULT
   }
   ALLOCATION_TIME {
      H5D_ALLOC_TIME_INCR
   }
}
}
