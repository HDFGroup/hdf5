#############################
Expected output for 'h5dump -r tchar.h5'
#############################
HDF5 "tchar.h5" {
GROUP "/" {
   DATASET "Dataset1" {
      DATATYPE  H5T_STD_I8LE
      DATASPACE  SIMPLE { ( 308 ) / ( 308 ) }
      DATA {
        (0) Four score and seven years ago our forefathers brought forth on thi"
        (67) s continent a new nation, conceived in liberty and dedicated to th"
        (133) e proposition that all men are created equal. Now we are engaged "
        (198) in a great civil war, testing whether that nation or any nation s"
        (263) o conceived and so dedicated can long endure."
      }
   }
}
}
