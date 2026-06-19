HDF5 "tfilters.h5" {
DATASET "all" {
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
   STORAGE_LAYOUT {
      CHUNKED ( 10, 5 )
      SIZE XXXX (1.XXX:1 COMPRESSION)
   }
   FILTERS {
      PREPROCESSING SHUFFLE {
         PARAMS_STRING "cd_values=4"
         DESCRIPTION "Byte shuffle preprocessing to improve downstream compression"
      }
      COMPRESSION SZIP {
         PIXELS_PER_BLOCK 4
         MODE K13
         CODING ENTROPY
         BYTE_ORDER LSB
         HEADER RAW
         PARAMS_STRING "coding = \"entropy\", pixels_per_block = 4"
         DESCRIPTION "SZIP lossless compression for scientific data"
      }
      COMPRESSION DEFLATE {
         LEVEL 5
         PARAMS_STRING "level = 5"
         DESCRIPTION "Deflate (zlib) general-purpose lossless compression"
      }
      CHECKSUM FLETCHER32 {
         DESCRIPTION "Fletcher32 checksum for end-to-end data integrity"
      }
      COMPRESSION NBIT {
         PARAMS_STRING "cd_values=8:1:50:1:4:0:32:0"
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
