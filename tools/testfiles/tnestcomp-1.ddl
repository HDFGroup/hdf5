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
            1
         },
            {
            2,
            4
         }
         },
         {
            {
            1,
            3
         },
            {
            4,
            7
         }
         },
         {
            {
            2,
            5
         },
            {
            6,
            10
         }
         },
         {
            {
            3,
            7
         },
            {
            8,
            13
         }
         },
         {
            {
            4,
            9
         },
            {
            10,
            16
         }
         }
      } 
   } 
} 
} 
