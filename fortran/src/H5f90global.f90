    MODULE H5GLOBAL
      USE H5FORTRAN_TYPES
      INTEGER, PARAMETER :: PREDEF_TYPES_LEN = 6 ! Do not forget to change this
                                                 ! value when new predefined
                                                 ! datatypes are added
      ! Do not forget to change the following line when new predefined 
      ! floating data types are added
      INTEGER, PARAMETER :: FLOATING_TYPES_LEN = 4

      ! Do not forget to change the following line when new predefined 
      ! integer data types are added
      INTEGER, PARAMETER :: INTEGER_TYPES_LEN = 16

      INTEGER(HID_T) H5T_NATIVE_INTEGER, &
                     H5T_NATIVE_REAL, &
                     H5T_NATIVE_DOUBLE, &
                     H5T_NATIVE_CHARACTER , &
                     H5T_STD_REF_OBJ,      &
                     H5T_STD_REF_DSETREG, &
                     H5T_IEEE_F32BE,  &
                     H5T_IEEE_F32LE,  &
                     H5T_IEEE_F64BE,  &
                     H5T_IEEE_F64LE,  &
                     H5T_STD_I8BE,    &
                     H5T_STD_I8LE,    &
                     H5T_STD_I16BE,   &
                     H5T_STD_I16LE,   &
                     H5T_STD_I32BE,   &
                     H5T_STD_I32LE,   &
                     H5T_STD_I64BE,   &
                     H5T_STD_I64LE,   &
                     H5T_STD_U8BE,    &
                     H5T_STD_U8LE,    &
                     H5T_STD_U16BE,   &
                     H5T_STD_U16LE,   &
                     H5T_STD_U32BE,   &
                     H5T_STD_U32LE,   &
                     H5T_STD_U64BE,   &
                     H5T_STD_U64LE


      INTEGER(HID_T), DIMENSION(PREDEF_TYPES_LEN) :: predef_types
      EQUIVALENCE (predef_types(1), H5T_NATIVE_INTEGER)
      EQUIVALENCE (predef_types(2), H5T_NATIVE_REAL)
      EQUIVALENCE (predef_types(3), H5T_NATIVE_DOUBLE)
      EQUIVALENCE (predef_types(4), H5T_NATIVE_CHARACTER)
      EQUIVALENCE (predef_types(5), H5T_STD_REF_OBJ)
      EQUIVALENCE (predef_types(6), H5T_STD_REF_DSETREG)

      INTEGER(HID_T), DIMENSION(FLOATING_TYPES_LEN) :: floating_types
      EQUIVALENCE (floating_types(1), H5T_IEEE_F32BE )
      EQUIVALENCE (floating_types(2), H5T_IEEE_F32LE)
      EQUIVALENCE (floating_types(3), H5T_IEEE_F64BE)
      EQUIVALENCE (floating_types(4), H5T_IEEE_F64LE)

      INTEGER(HID_T), DIMENSION(INTEGER_TYPES_LEN) :: integer_types
      EQUIVALENCE (integer_types(1), H5T_STD_I8BE )
      EQUIVALENCE (integer_types(2), H5T_STD_I8LE)
      EQUIVALENCE (integer_types(3), H5T_STD_I16BE)
      EQUIVALENCE (integer_types(4), H5T_STD_I16LE)
      EQUIVALENCE (integer_types(5), H5T_STD_I32BE)
      EQUIVALENCE (integer_types(6), H5T_STD_I32LE)
      EQUIVALENCE (integer_types(7), H5T_STD_I64BE)
      EQUIVALENCE (integer_types(8), H5T_STD_I64LE)
      EQUIVALENCE (integer_types(9), H5T_STD_U8BE)
      EQUIVALENCE (integer_types(10), H5T_STD_U8LE)
      EQUIVALENCE (integer_types(11), H5T_STD_U16BE)
      EQUIVALENCE (integer_types(12), H5T_STD_U16LE)
      EQUIVALENCE (integer_types(13), H5T_STD_U32BE)
      EQUIVALENCE (integer_types(14), H5T_STD_U32LE)
      EQUIVALENCE (integer_types(15), H5T_STD_U64BE)
      EQUIVALENCE (integer_types(16), H5T_STD_U64LE)


      COMMON /PREDEFINED_TYPES/ H5T_NATIVE_INTEGER, &
                                H5T_NATIVE_REAL, &
                                H5T_NATIVE_DOUBLE, &
                                H5T_NATIVE_CHARACTER, &
                                H5T_STD_REF_OBJ, &
                                H5T_STD_REF_DSETREG

      COMMON /FLOATING_TYPES/ H5T_IEEE_F32BE,  &
                              H5T_IEEE_F32LE,  &
                              H5T_IEEE_F64BE,  &
                              H5T_IEEE_F64LE

      COMMON /INTEGER_TYPES/ H5T_STD_I8BE,  &
                             H5T_STD_I8LE,    &
                             H5T_STD_I16BE,   &
                             H5T_STD_I16LE,   &
                             H5T_STD_I32BE,   &
                             H5T_STD_I32LE,   &
                             H5T_STD_I64BE,   &
                             H5T_STD_I64LE,   &
                             H5T_STD_U8BE,    &
                             H5T_STD_U8LE,    &
                             H5T_STD_U16BE,   &
                             H5T_STD_U16LE,   &
                             H5T_STD_U32BE,   &
                             H5T_STD_U32LE,   &
                             H5T_STD_U64BE,   &
                             H5T_STD_U64LE

    END MODULE H5GLOBAL
      
