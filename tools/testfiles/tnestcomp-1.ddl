#############################
Expected output for 'h5dump tnestedcomp.h5'
#############################
HDF5 "tnestedcomp.h5" {
GROUP "/" {
   DATASET "nested compound" {
      DATATYPE {
         {
            H5T_IEEE_F64BE "re";
            H5T_IEEE_F64BE "im";
         } "x";
         {
            H5T_IEEE_F64BE "re";
            H5T_IEEE_F64BE "im";
         } "y";
      } 
      DATASPACE { SIMPLE ( 5 ) / ( 5 ) } 
      DATA {
         {
            {
            0,
            0
         },
            {
            0,
            0
         }
         },
         {
            {
            1,
            1
         },
            {
            1,
            1
         }
         },
         {
            {
            2,
            2
         },
            {
            2,
            2
         }
         },
         {
            {
            3,
            3
         },
            {
            3,
            3
         }
         },
         {
            {
            4,
            4
         },
            {
            4,
            4
         }
         }
      } 
   } 
} 
} 
