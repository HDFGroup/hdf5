#############################
Expected output for 'h5dump -H -B -d compact tfilters.h5'
#############################
HDF5 "tfilters.h5" {
SUPER_BLOCK {
   SUPERBLOCK_VERSION 0
   FREELIST_VERSION 0
   SYMBOLTABLE_VERSION 0
   OBJECTHEADER_VERSION 0
   USERBLOCK_VERSION 0
   OFFSET_SIZE 8
   LENGTH_SIZE 8
   BTREE_RANK 16
   BTREE_LEAF 4
   FILE_DRIVER H5FD_SEC2
   ISTORE_K 32
}
DATASET "compact" {
COMMENT "This is a dataset with compact storage"
   DATATYPE  H5T_STD_I32LE
   DATASPACE  SIMPLE { ( 20, 10 ) / ( 20, 10 ) }
}
}
