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


!      COMMON /PREDEFINED_TYPES/ H5T_NATIVE_INTEGER, &
!                                H5T_NATIVE_REAL, &
!                                H5T_NATIVE_DOUBLE, &
!                                H5T_NATIVE_CHARACTER, &
!                                H5T_STD_REF_OBJ, &
!                                H5T_STD_REF_DSETREG
      COMMON /PREDEFINED_TYPES/  predef_types

!      COMMON /FLOATING_TYPES/ H5T_IEEE_F32BE,  &
!                              H5T_IEEE_F32LE,  &
!                              H5T_IEEE_F64BE,  &
!                              H5T_IEEE_F64LE
      COMMON /FLOATING_TYPES/ floating_types 
!
!      COMMON /INTEGER_TYPES/ H5T_STD_I8BE,  &
!                             H5T_STD_I8LE,    &
!                             H5T_STD_I16BE,   &
!                             H5T_STD_I16LE,   &
!                             H5T_STD_I32BE,   &
!                             H5T_STD_I32LE,   &
!                             H5T_STD_I64BE,   &
!                             H5T_STD_I64LE,   &
!                             H5T_STD_U8BE,    &
!                             H5T_STD_U8LE,    &
!                             H5T_STD_U16BE,   &
!                             H5T_STD_U16LE,   &
!                             H5T_STD_U32BE,   &
!                             H5T_STD_U32LE,   &
!                             H5T_STD_U64BE,   &
!                             H5T_STD_U64LE
      COMMON /INTEGER_TYPES/ integer_types
!
! Fortran flags
!
!
! H5F flags (DO NOT FORGET TO UPDATE WHEN NEW FLAGS ARE ADDEDD !)
!
! H5F flags declaration
!
      INTEGER, PARAMETER :: H5F_FLAGS_LEN = 7
      INTEGER H5F_flags(H5F_FLAGS_LEN)
      COMMON /H5F_FLAGS/ H5F_flags

      INTEGER :: H5F_ACC_RDWR_F 
      INTEGER :: H5F_ACC_RDONLY_F
      INTEGER :: H5F_ACC_TRUNC_F
      INTEGER :: H5F_ACC_EXCL_F
      INTEGER :: H5F_ACC_DEBUG_F
      INTEGER :: H5F_SCOPE_LOCAL_F
      INTEGER :: H5F_SCOPE_GLOBAL_F

      EQUIVALENCE(H5F_flags(1), H5F_ACC_RDWR_F)
      EQUIVALENCE(H5F_flags(2), H5F_ACC_RDONLY_F)
      EQUIVALENCE(H5F_flags(3), H5F_ACC_TRUNC_F)
      EQUIVALENCE(H5F_flags(4), H5F_ACC_EXCL_F)
      EQUIVALENCE(H5F_flags(5), H5F_ACC_DEBUG_F)
      EQUIVALENCE(H5F_flags(6), H5F_SCOPE_LOCAL_F)
      EQUIVALENCE(H5F_flags(7), H5F_SCOPE_GLOBAL_F)
!
! H5G flags declaration
!
      INTEGER, PARAMETER :: H5G_FLAGS_LEN = 8 
      INTEGER H5G_flags(H5G_FLAGS_LEN)
      COMMON /H5G_FLAGS/ H5G_flags

      INTEGER :: H5G_UNKNOWN_F
      INTEGER :: H5G_LINK_F
      INTEGER :: H5G_GROUP_F
      INTEGER :: H5G_DATASET_F
      INTEGER :: H5G_TYPE_F
      INTEGER :: H5G_LINK_ERROR_F
      INTEGER :: H5G_LINK_HARD_F
      INTEGER :: H5G_LINK_SOFT_F

      EQUIVALENCE(H5G_flags(1), H5G_UNKNOWN_F)
      EQUIVALENCE(H5G_flags(2), H5G_LINK_F)
      EQUIVALENCE(H5G_flags(3), H5G_GROUP_F)
      EQUIVALENCE(H5G_flags(4), H5G_DATASET_F)
      EQUIVALENCE(H5G_flags(5), H5G_TYPE_F) 
      EQUIVALENCE(H5G_flags(6), H5G_LINK_ERROR_F) 
      EQUIVALENCE(H5G_flags(7), H5G_LINK_HARD_F) 
      EQUIVALENCE(H5G_flags(8), H5G_LINK_SOFT_F) 
!
! H5D flags declaration
!

      INTEGER, PARAMETER :: H5D_FLAGS_LEN = 3 
      INTEGER H5D_flags(H5D_FLAGS_LEN)
      COMMON /H5D_FLAGS/ H5D_flags

      INTEGER :: H5D_COMPACT_F  
      INTEGER :: H5D_CONTIGUOUS_F
      INTEGER :: H5D_CHUNKED_F

      EQUIVALENCE(H5D_flags(1), H5D_COMPACT_F)
      EQUIVALENCE(H5D_flags(2), H5D_CONTIGUOUS_F)
      EQUIVALENCE(H5D_flags(3), H5D_CHUNKED_F)

!
! H5FD flags declaration
!
      INTEGER, PARAMETER :: H5FD_FLAGS_LEN = 2
      INTEGER H5FD_flags(H5FD_FLAGS_LEN)
      COMMON /H5FD_FLAGS/ H5FD_flags
      
      INTEGER :: H5FD_MPIO_INDEPENDENT_F 
      INTEGER :: H5FD_MPIO_COLLECTIVE_F
 
      EQUIVALENCE(H5FD_flags(1), H5FD_MPIO_INDEPENDENT_F)
      EQUIVALENCE(H5FD_flags(2), H5FD_MPIO_COLLECTIVE_F)

!
! H5E flags declaration
!
      INTEGER, PARAMETER :: H5E_FLAGS_LEN = 23
      INTEGER H5E_flags(H5E_FLAGS_LEN)
      COMMON /H5E_FLAGS/ H5E_flags

      INTEGER ::  H5E_NONE_MAJOR_F 
      INTEGER ::  H5E_ARGS_F 
      INTEGER ::  H5E_RESOURCE_F 
      INTEGER ::  H5E_INTERNAL_F 
      INTEGER ::  H5E_FILE_F 
      INTEGER ::  H5E_IO_F 
      INTEGER ::  H5E_FUNC_F 
      INTEGER ::  H5E_ATOM_F 
      INTEGER ::  H5E_CACHE_F 
      INTEGER ::  H5E_BTREE_F 
      INTEGER ::  H5E_SYM_F 
      INTEGER ::  H5E_HEAP_F 
      INTEGER ::  H5E_OHDR_F 
      INTEGER ::  H5E_DATATYPE_F 
      INTEGER ::  H5E_DATASPACE_F 
      INTEGER ::  H5E_DATASET_F 
      INTEGER ::  H5E_STORAGE_F 
      INTEGER ::  H5E_PLIST_F 
      INTEGER ::  H5E_ATTR_F 
      INTEGER ::  H5E_PLINE_F 
      INTEGER ::  H5E_EFL_F 
      INTEGER ::  H5E_RAGGED_F  
      INTEGER ::  H5E_REFERENCE_F

      EQUIVALENCE(H5E_flags(1), H5E_NONE_MAJOR_F)
      EQUIVALENCE(H5E_flags(2), H5E_ARGS_F)
      EQUIVALENCE(H5E_flags(3), H5E_RESOURCE_F)
      EQUIVALENCE(H5E_flags(4), H5E_INTERNAL_F)
      EQUIVALENCE(H5E_flags(5), H5E_FILE_F)
      EQUIVALENCE(H5E_flags(6), H5E_IO_F)
      EQUIVALENCE(H5E_flags(7), H5E_FUNC_F)
      EQUIVALENCE(H5E_flags(8), H5E_ATOM_F)
      EQUIVALENCE(H5E_flags(9), H5E_CACHE_F)
      EQUIVALENCE(H5E_flags(10), H5E_BTREE_F)
      EQUIVALENCE(H5E_flags(11), H5E_SYM_F)
      EQUIVALENCE(H5E_flags(12), H5E_HEAP_F)
      EQUIVALENCE(H5E_flags(13), H5E_OHDR_F)
      EQUIVALENCE(H5E_flags(14), H5E_DATATYPE_F)
      EQUIVALENCE(H5E_flags(15), H5E_DATASPACE_F)
      EQUIVALENCE(H5E_flags(16), H5E_DATASET_F)
      EQUIVALENCE(H5E_flags(17), H5E_STORAGE_F)
      EQUIVALENCE(H5E_flags(18), H5E_PLIST_F)
      EQUIVALENCE(H5E_flags(19), H5E_ATTR_F)
      EQUIVALENCE(H5E_flags(20), H5E_PLINE_F)
      EQUIVALENCE(H5E_flags(21), H5E_EFL_F)
      EQUIVALENCE(H5E_flags(22), H5E_RAGGED_F)
      EQUIVALENCE(H5E_flags(23), H5E_REFERENCE_F)

!
! H5E flags declaration
!
      INTEGER, PARAMETER :: H5I_FLAGS_LEN = 7
      INTEGER H5I_flags(H5I_FLAGS_LEN)
      COMMON /H5I_FLAGS/ H5I_flags

      INTEGER ::  H5I_FILE_F
      INTEGER ::  H5I_GROUP_F
      INTEGER ::  H5I_DATATYPE_F
      INTEGER ::  H5I_DATASPACE_F
      INTEGER ::  H5I_DATASET_F
      INTEGER ::  H5I_ATTR_F
      INTEGER ::  H5I_BADID_F

      EQUIVALENCE(H5I_flags(1), H5I_FILE_F)
      EQUIVALENCE(H5I_flags(2), H5I_GROUP_F)
      EQUIVALENCE(H5I_flags(3), H5I_DATATYPE_F)
      EQUIVALENCE(H5I_flags(4), H5I_DATASPACE_F)
      EQUIVALENCE(H5I_flags(5), H5I_DATASET_F)
      EQUIVALENCE(H5I_flags(6), H5I_ATTR_F)
      EQUIVALENCE(H5I_flags(7), H5I_BADID_F)

!
! H5P flags declaration
!
      INTEGER, PARAMETER :: H5P_FLAGS_LEN = 6
      INTEGER H5P_flags(H5P_FLAGS_LEN)
      COMMON /H5P_FLAGS/ H5P_flags

      INTEGER ::  H5P_FILE_CREATE_F 
      INTEGER ::  H5P_FILE_ACCESS_F 
      INTEGER ::  H5P_DATASET_CREATE_F
      INTEGER ::  H5P_DATASET_XFER_F 
      INTEGER ::  H5P_MOUNT_F 
      INTEGER ::  H5P_DEFAULT_F 

      EQUIVALENCE(H5P_flags(1), H5P_FILE_CREATE_F)
      EQUIVALENCE(H5P_flags(2), H5P_FILE_ACCESS_F)
      EQUIVALENCE(H5P_flags(3), H5P_DATASET_CREATE_F)
      EQUIVALENCE(H5P_flags(4), H5P_DATASET_XFER_F)
      EQUIVALENCE(H5P_flags(5), H5P_MOUNT_F)
      EQUIVALENCE(H5P_flags(6), H5P_DEFAULT_F)

!
! H5P flags declaration
!
      INTEGER, PARAMETER :: H5R_FLAGS_LEN = 2
      INTEGER H5R_flags(H5R_FLAGS_LEN)
      COMMON /H5R_FLAGS/ H5R_flags
      
      INTEGER :: H5R_OBJECT_F
      INTEGER :: H5R_DATASET_REGION_F

      EQUIVALENCE(H5R_flags(1), H5R_OBJECT_F)
      EQUIVALENCE(H5R_flags(2), H5R_DATASET_REGION_F)

!
! H5S flags declaration
!
      INTEGER, PARAMETER :: H5S_FLAGS_LEN = 6
      INTEGER H5S_flags(H5S_FLAGS_LEN)
      COMMON /H5S_FLAGS/ H5S_flags

      INTEGER :: H5S_SCALAR_F 
      INTEGER :: H5S_SIMPLE_F 
      INTEGER :: H5S_SELECT_SET_F
      INTEGER :: H5S_SELECT_OR_F
      INTEGER :: H5S_UNLIMITED_F
      INTEGER :: H5S_ALL_F

      EQUIVALENCE(H5S_flags(1), H5S_SCALAR_F)
      EQUIVALENCE(H5S_flags(2), H5S_SIMPLE_F)
      EQUIVALENCE(H5S_flags(3), H5S_SELECT_SET_F)
      EQUIVALENCE(H5S_flags(4), H5S_SELECT_OR_F)
      EQUIVALENCE(H5S_flags(5), H5S_UNLIMITED_F)
      EQUIVALENCE(H5S_flags(6), H5S_ALL_F)

!
! H5T flags declaration
!
      INTEGER, PARAMETER :: H5T_FLAGS_LEN = 28
      INTEGER H5T_flags(H5T_FLAGS_LEN)
      COMMON /H5T_FLAGS/ H5T_flags

      INTEGER ::  H5T_NO_CLASS_F 
      INTEGER ::  H5T_INTEGER_F 
      INTEGER ::  H5T_FLOAT_F  
      INTEGER ::  H5T_TIME_F 
      INTEGER ::  H5T_STRING_F 
      INTEGER ::  H5T_BITFIELD_F
      INTEGER ::  H5T_OPAQUE_F 
      INTEGER ::  H5T_COMPOUND_F 
      INTEGER ::  H5T_REFERENCE_F
      INTEGER ::  H5T_ENUM_F 
      INTEGER ::  H5T_ORDER_LE_F 
      INTEGER ::  H5T_ORDER_BE_F
      INTEGER ::  H5T_ORDER_VAX_F
      INTEGER ::  H5T_PAD_ZERO_F
      INTEGER ::  H5T_PAD_ONE_F
      INTEGER ::  H5T_PAD_BACKGROUND_F
      INTEGER ::  H5T_PAD_ERROR_F    
      INTEGER ::  H5T_SGN_NONE_F   
      INTEGER ::  H5T_SGN_2_F     
      INTEGER ::  H5T_SGN_ERROR_F
      INTEGER ::  H5T_NORM_IMPLIED_F
      INTEGER ::  H5T_NORM_MSBSET_F
      INTEGER ::  H5T_NORM_NONE_F 
      INTEGER ::  H5T_CSET_ASCII_F
      INTEGER ::  H5T_STR_NULLTERM_F 
      INTEGER ::  H5T_STR_NULLPAD_F 
      INTEGER ::  H5T_STR_SPACEPAD_F
      INTEGER ::  H5T_STR_ERROR_F
       
      EQUIVALENCE(H5T_flags(1), H5T_NO_CLASS_F)
      EQUIVALENCE(H5T_flags(2), H5T_INTEGER_F)
      EQUIVALENCE(H5T_flags(3), H5T_FLOAT_F)
      EQUIVALENCE(H5T_flags(4), H5T_TIME_F)
      EQUIVALENCE(H5T_flags(5), H5T_STRING_F)
      EQUIVALENCE(H5T_flags(6), H5T_BITFIELD_F)
      EQUIVALENCE(H5T_flags(7), H5T_OPAQUE_F)
      EQUIVALENCE(H5T_flags(8), H5T_COMPOUND_F)
      EQUIVALENCE(H5T_flags(9), H5T_REFERENCE_F)
      EQUIVALENCE(H5T_flags(10), H5T_ENUM_F)
      EQUIVALENCE(H5T_flags(11), H5T_ORDER_LE_F)
      EQUIVALENCE(H5T_flags(12), H5T_ORDER_BE_F)
      EQUIVALENCE(H5T_flags(13), H5T_ORDER_VAX_F)
      EQUIVALENCE(H5T_flags(14), H5T_PAD_ZERO_F)
      EQUIVALENCE(H5T_flags(15), H5T_PAD_ONE_F)
      EQUIVALENCE(H5T_flags(16), H5T_PAD_BACKGROUND_F)
      EQUIVALENCE(H5T_flags(17), H5T_PAD_ERROR_F)
      EQUIVALENCE(H5T_flags(18), H5T_SGN_NONE_F)
      EQUIVALENCE(H5T_flags(19), H5T_SGN_2_F)
      EQUIVALENCE(H5T_flags(20), H5T_SGN_ERROR_F)
      EQUIVALENCE(H5T_flags(21), H5T_NORM_IMPLIED_F)
      EQUIVALENCE(H5T_flags(22), H5T_NORM_MSBSET_F)
      EQUIVALENCE(H5T_flags(23), H5T_NORM_NONE_F)
      EQUIVALENCE(H5T_flags(24), H5T_CSET_ASCII_F)
      EQUIVALENCE(H5T_flags(25), H5T_STR_NULLTERM_F)
      EQUIVALENCE(H5T_flags(26), H5T_STR_NULLPAD_F)
      EQUIVALENCE(H5T_flags(27), H5T_STR_SPACEPAD_F)
      EQUIVALENCE(H5T_flags(28), H5T_STR_ERROR_F)

    END MODULE H5GLOBAL
      
