#############################
Expected output for 'h5dump -r tchar.h5'
#############################
HDF5 "tchar.h5" {
GROUP "/" {
   DATASET "Dataset1" {
      DATATYPE  H5T_STD_I8LE
      DATASPACE  SIMPLE { ( 308 ) / ( 308 ) }
      DATA {
        (0): Four score and seven years ago our forefathers brought forth on th"
        (66): is continent a new nation, conceived in liberty and dedicated to "
        (131): the proposition that all men are created equal. Now we are engag"
        (195): ed in a great civil war, testing whether that nation or any nati"
        (259): on so conceived and so dedicated can long endure."
      }
   }
}
}
