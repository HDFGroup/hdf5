#############################
Expected output for 'h5dump -r tchar.h5'
#############################
HDF5 "tchar.h5" {
GROUP "/" {
   DATASET "Dataset1" {
      DATATYPE  H5T_STD_I8LE
      DATASPACE  SIMPLE { ( 308 ) / ( 308 ) }
      DATA {
         "Four score and seven years ago our fore"
         "fathers brought forth on this continent"
         " a new nation, conceived in liberty and"
         " dedicated to the proposition that all "
         "men are created equal. Now we are engag"
         "ed in a great civil war, testing whethe"
         "r that nation or any nation so conceive"
         "d and so dedicated can long endure."
      }
   }
}
}
