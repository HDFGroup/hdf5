!****h* ROBODoc/H5LIB
!
! NAME
!  MODULE H5LIB
!
! PURPOSE
!  This module provides fortran specific helper functions for the HDF library
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

#include <H5config_f.inc>

MODULE H5LIB

  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_INTPTR_T
  USE H5GLOBAL
  IMPLICIT NONE

  PRIVATE
  !
  ! H5F flags declaration
  !
  INTEGER, PARAMETER :: H5F_FLAGS_LEN = 19
  INTEGER, DIMENSION(1:H5F_FLAGS_LEN) :: H5F_flags
  !
  ! H5generic flags declaration
  !
  INTEGER, PARAMETER :: H5generic_FLAGS_LEN = 9
  INTEGER, DIMENSION(1:H5generic_FLAGS_LEN) :: H5generic_flags
  
  INTEGER, PARAMETER :: H5generic_haddr_FLAGS_LEN = 1
  INTEGER(HADDR_T), DIMENSION(1:H5generic_haddr_FLAGS_LEN) :: H5generic_haddr_flags
  !
  ! H5G flags declaration
  !
  INTEGER, PARAMETER :: H5G_FLAGS_LEN = 12
  INTEGER, DIMENSION(1:H5G_FLAGS_LEN) :: H5G_flags
  !
  ! H5D flags declaration
  !
  INTEGER, PARAMETER :: H5D_FLAGS_LEN = 29
  INTEGER, DIMENSION(1:H5D_FLAGS_LEN) :: H5D_flags
  INTEGER, PARAMETER :: H5D_SIZE_FLAGS_LEN = 2
  INTEGER(SIZE_T), DIMENSION(1:H5D_SIZE_FLAGS_LEN) :: H5D_size_flags
  !
  ! H5E flags declaration
  !
  INTEGER, PARAMETER :: H5E_FLAGS_LEN = 4
  INTEGER, DIMENSION(1:H5E_FLAGS_LEN) :: H5E_flags
  INTEGER, PARAMETER :: H5E_HID_FLAGS_LEN = 1
  INTEGER(HID_T), DIMENSION(1:H5E_HID_FLAGS_LEN) :: H5E_hid_flags
  !
  ! H5FD flags declaration
  !
  INTEGER, PARAMETER :: H5FD_FLAGS_LEN = 11
  INTEGER, DIMENSION(1:H5FD_FLAGS_LEN) :: H5FD_flags
  !
  ! H5FD file drivers flags declaration 
  !
  INTEGER, PARAMETER :: H5FD_HID_FLAGS_LEN = 7
  INTEGER(HID_T), DIMENSION(1:H5FD_HID_FLAGS_LEN) :: H5FD_hid_flags
  !
  ! H5I flags declaration
  !
  INTEGER, PARAMETER :: H5I_FLAGS_LEN = 7
  INTEGER, DIMENSION(1:H5I_FLAGS_LEN) :: H5I_flags
  !
  ! H5L flags declaration
  !
  INTEGER, PARAMETER :: H5L_FLAGS_LEN = 6
  INTEGER, DIMENSION(1:H5L_FLAGS_LEN) :: H5L_flags
  !
  ! H5O flags declaration
  !
  INTEGER, PARAMETER :: H5O_FLAGS_LEN = 33
  INTEGER, DIMENSION(1:H5O_FLAGS_LEN) :: H5O_flags
  !
  ! H5P flags declaration
  !
  INTEGER, PARAMETER :: H5P_FLAGS_LEN = 18
  INTEGER(HID_T), DIMENSION(1:H5P_FLAGS_LEN) :: H5P_flags
  !
  ! H5P integers flags declaration
  !
  INTEGER, PARAMETER :: H5P_FLAGS_INT_LEN = 2
  INTEGER, DIMENSION(1:H5P_FLAGS_INT_LEN) :: H5P_flags_int
  !
  ! H5R flags declaration
  !
  INTEGER, PARAMETER :: H5R_FLAGS_LEN = 2
  INTEGER, DIMENSION(1:H5R_FLAGS_LEN) :: H5R_flags
  !
  ! H5S flags declaration
  !
  INTEGER, PARAMETER :: H5S_FLAGS_LEN = 18
  INTEGER, DIMENSION(1:H5S_FLAGS_LEN) :: H5S_flags
  INTEGER, PARAMETER :: H5S_HSIZE_FLAGS_LEN = 1
  INTEGER(HSIZE_T), DIMENSION(1:H5S_HSIZE_FLAGS_LEN) :: H5S_hsize_flags
  INTEGER, PARAMETER :: H5S_HID_FLAGS_LEN = 1
  INTEGER(HSIZE_T), DIMENSION(1:H5S_HID_FLAGS_LEN)   :: H5S_hid_flags
  !
  ! H5T flags declaration
  !
  INTEGER, PARAMETER :: H5T_FLAGS_LEN = 35
  INTEGER, DIMENSION(1:H5T_FLAGS_LEN) :: H5T_flags

  !
  ! H5Z flags declaration
  !
  INTEGER, PARAMETER :: H5Z_FLAGS_LEN = 20
  INTEGER, DIMENSION(1:H5Z_FLAGS_LEN) :: H5Z_flags
  !
  ! H5 Library flags declaration
  !
  INTEGER, PARAMETER :: H5LIB_FLAGS_LEN =  2
  INTEGER, DIMENSION(1:H5LIB_FLAGS_LEN) :: H5LIB_flags

  PUBLIC :: h5open_f, h5close_f, h5get_libversion_f, h5dont_atexit_f, h5kind_to_type, h5offsetof, h5gmtime
  PUBLIC :: h5garbage_collect_f, h5check_version_f

CONTAINS
!****s* H5LIB/h5open_f
!
! NAME
!  h5open_f
!
! PURPOSE
!  Initializes HDF5 Fortran interface.
!
! Outputs:
!  error - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! Removed call to h5open_c since this may cause a problem for an
! application that uses HDF5 library outside HDF5 Fortran APIs.
! October 13, 2011
! Fortran90 Interface:
  SUBROUTINE h5open_f(error)
    USE H5F, ONLY : h5fget_obj_count_f, H5OPEN_NUM_OBJ 
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: error
    INTEGER(SIZE_T) :: H5OPEN_NUM_OBJ_LOC = 0
!*****
    INTERFACE

       INTEGER FUNCTION h5init_types_c(p_types, f_types, i_types) &
            BIND(C,NAME='h5init_types_c')
         IMPORT :: HID_T
         IMPORT :: PREDEF_TYPES_LEN, FLOATING_TYPES_LEN, INTEGER_TYPES_LEN
         IMPLICIT NONE
         INTEGER(HID_T), DIMENSION(1:PREDEF_TYPES_LEN)   :: p_types
         INTEGER(HID_T), DIMENSION(1:FLOATING_TYPES_LEN) :: f_types
         INTEGER(HID_T), DIMENSION(1:INTEGER_TYPES_LEN)  :: i_types
       END FUNCTION h5init_types_c

       INTEGER FUNCTION h5init_flags_c(i_H5D_flags, &
            i_H5D_size_flags,&
            i_H5E_flags, &
            i_H5E_hid_flags, &
            i_H5F_flags, &
            i_H5FD_flags, &
            i_H5FD_hid_flags, &
            i_H5G_flags, &
            i_H5I_flags, &
            i_H5L_flags, &
            i_H5O_flags, &
            i_H5P_flags, &
            i_H5P_flags_int, &
            i_H5R_flags, &
            i_H5S_flags, &
            i_H5S_hid_flags, &
            i_H5S_hsize_flags, &
            i_H5T_flags, &
            i_H5Z_flags, &
            i_H5generic_flags, &
            i_H5generic_haddr_flags) &
            BIND(C,NAME='h5init_flags_c')
         IMPORT :: HID_T, SIZE_T, HSIZE_T, HADDR_T
         IMPORT :: H5D_FLAGS_LEN, H5D_SIZE_FLAGS_LEN, &
              H5E_FLAGS_LEN, H5E_HID_FLAGS_LEN, &
              H5F_FLAGS_LEN, H5G_FLAGS_LEN, H5FD_FLAGS_LEN, &
              H5FD_HID_FLAGS_LEN, H5I_FLAGS_LEN, H5L_FLAGS_LEN, &
              H5O_FLAGS_LEN, H5P_FLAGS_LEN, H5P_FLAGS_INT_LEN, &
              H5R_FLAGS_LEN, H5S_FLAGS_LEN, H5S_HID_FLAGS_LEN, H5S_HSIZE_FLAGS_LEN, &
              H5T_FLAGS_LEN, H5Z_FLAGS_LEN, H5generic_FLAGS_LEN, H5generic_haddr_FLAGS_LEN
         IMPLICIT NONE
         INTEGER         , DIMENSION(1:H5D_FLAGS_LEN)             :: i_H5D_flags
         INTEGER(SIZE_T) , DIMENSION(1:H5D_SIZE_FLAGS_LEN)        :: i_H5D_size_flags
         INTEGER         , DIMENSION(1:H5E_FLAGS_LEN)             :: i_H5E_flags
         INTEGER(HID_T)  , DIMENSION(1:H5E_HID_FLAGS_LEN)         :: i_H5E_hid_flags
         INTEGER         , DIMENSION(1:H5F_FLAGS_LEN)             :: i_H5F_flags
         INTEGER         , DIMENSION(1:H5G_FLAGS_LEN)             :: i_H5G_flags
         INTEGER         , DIMENSION(1:H5FD_FLAGS_LEN)            :: i_H5FD_flags
         INTEGER(HID_T)  , DIMENSION(1:H5FD_HID_FLAGS_LEN)        :: i_H5FD_hid_flags
         INTEGER         , DIMENSION(1:H5I_FLAGS_LEN)             :: i_H5I_flags
         INTEGER         , DIMENSION(1:H5L_FLAGS_LEN)             :: i_H5L_flags
         INTEGER         , DIMENSION(1:H5O_FLAGS_LEN)             :: i_H5O_flags
         INTEGER(HID_T)  , DIMENSION(1:H5P_FLAGS_LEN)             :: i_H5P_flags
         INTEGER         , DIMENSION(1:H5P_FLAGS_INT_LEN)         :: i_H5P_flags_int
         INTEGER         , DIMENSION(1:H5R_FLAGS_LEN)             :: i_H5R_flags
         INTEGER         , DIMENSION(1:H5S_FLAGS_LEN)             :: i_H5S_flags
         INTEGER(HID_T)  , DIMENSION(1:H5S_HID_FLAGS_LEN)         :: i_H5S_hid_flags
         INTEGER(HSIZE_T), DIMENSION(1:H5S_HSIZE_FLAGS_LEN)       :: i_H5S_hsize_flags
         INTEGER         , DIMENSION(1:H5T_FLAGS_LEN)             :: i_H5T_flags
         INTEGER         , DIMENSION(1:H5Z_FLAGS_LEN)             :: i_H5Z_flags
         INTEGER         , DIMENSION(1:H5generic_FLAGS_LEN)       :: i_H5generic_flags
         INTEGER(HADDR_T), DIMENSION(1:H5generic_haddr_FLAGS_LEN) :: i_H5generic_haddr_flags
       END FUNCTION h5init_flags_c

       INTEGER FUNCTION h5init1_flags_c( i_H5LIB_flags ) &
            BIND(C,NAME='h5init1_flags_c')
         IMPORT :: H5LIB_FLAGS_LEN
         IMPLICIT NONE
         INTEGER, DIMENSION(1:H5LIB_FLAGS_LEN) :: i_H5LIB_flags
       END FUNCTION h5init1_flags_c

    END INTERFACE
    
    error = h5init_types_c(predef_types, floating_types, integer_types)

    H5T_NATIVE_INTEGER_KIND(1:5)  = predef_types(1:5) 
    H5T_NATIVE_INTEGER            = predef_types(6)
    H5T_NATIVE_REAL               = predef_types(7)
    H5T_NATIVE_DOUBLE             = predef_types(8) 
    H5T_NATIVE_CHARACTER          = predef_types(9)
    H5T_STD_REF_OBJ               = predef_types(10)
    H5T_STD_REF_DSETREG           = predef_types(11)
    H5T_NATIVE_REAL_C_FLOAT       = predef_types(12)
    H5T_NATIVE_REAL_C_DOUBLE      = predef_types(13)
    H5T_NATIVE_REAL_C_LONG_DOUBLE = predef_types(14)
    H5T_NATIVE_B8                 = predef_types(15)
    H5T_NATIVE_B16                = predef_types(16)
    H5T_NATIVE_B32                = predef_types(17)
    H5T_NATIVE_B64                = predef_types(18)
    H5T_NATIVE_FLOAT_128          = predef_types(19)
    
    H5T_IEEE_F32BE = floating_types(1)
    H5T_IEEE_F32LE = floating_types(2)
    H5T_IEEE_F64BE = floating_types(3)
    H5T_IEEE_F64LE = floating_types(4) 
    
    H5T_STD_I8BE   = integer_types(1)
    H5T_STD_I8LE   = integer_types(2)
    H5T_STD_I16BE  = integer_types(3)
    H5T_STD_I16LE  = integer_types(4)
    H5T_STD_I32BE  = integer_types(5)
    H5T_STD_I32LE  = integer_types(6)
    H5T_STD_I64BE  = integer_types(7)
    H5T_STD_I64LE  = integer_types(8)
    H5T_STD_U8BE   = integer_types(9)
    H5T_STD_U8LE   = integer_types(10)
    H5T_STD_U16BE  = integer_types(11)
    H5T_STD_U16LE  = integer_types(12)
    H5T_STD_U32BE  = integer_types(13)
    H5T_STD_U32LE  = integer_types(14)
    H5T_STD_U64BE  = integer_types(15)
    H5T_STD_U64LE  = integer_types(16)
    H5T_STRING     = integer_types(17)
    H5T_STD_B8BE   = integer_types(18)
    H5T_STD_B8LE   = integer_types(19)
    H5T_STD_B16BE  = integer_types(20)
    H5T_STD_B16LE  = integer_types(21)
    H5T_STD_B32BE  = integer_types(22)
    H5T_STD_B32LE  = integer_types(23)
    H5T_STD_B64BE  = integer_types(24)
    H5T_STD_B64LE  = integer_types(25)
    H5T_FORTRAN_S1 = integer_types(26)
    H5T_C_S1       = integer_types(27)

    error = error + h5init_flags_c(H5D_flags, &
         H5D_size_flags, &
         H5E_flags, &
         H5E_hid_flags, &
         H5F_flags, &
         H5FD_flags, &
         H5FD_hid_flags, &
         H5G_flags, &
         H5I_flags, &
         H5L_flags, &
         H5O_flags, &
         H5P_flags, &
         H5P_flags_int, &
         H5R_flags, &
         H5S_flags, &
         H5S_hid_flags, &
         H5S_hsize_flags, &
         H5T_flags, &
         H5Z_flags, &
         H5generic_flags,&
         H5generic_haddr_flags)
    !
    ! H5F flags
    !
    H5F_ACC_RDWR_F        = H5F_flags(1)
    H5F_ACC_RDONLY_F      = H5F_flags(2)
    H5F_ACC_TRUNC_F       = H5F_flags(3)
    H5F_ACC_EXCL_F        = H5F_flags(4)
    H5F_ACC_DEBUG_F       = H5F_flags(5)
    H5F_SCOPE_LOCAL_F     = H5F_flags(6)
    H5F_SCOPE_GLOBAL_F    = H5F_flags(7)
    H5F_CLOSE_DEFAULT_F   = H5F_flags(8)
    H5F_CLOSE_WEAK_F      = H5F_flags(9)
    H5F_CLOSE_SEMI_F      = H5F_flags(10)
    H5F_CLOSE_STRONG_F    = H5F_flags(11)
    H5F_OBJ_FILE_F        = H5F_flags(12)
    H5F_OBJ_DATASET_F     = H5F_flags(13)
    H5F_OBJ_GROUP_F       = H5F_flags(14)
    H5F_OBJ_DATATYPE_F    = H5F_flags(15)
    H5F_OBJ_ALL_F         = H5F_flags(16)
    H5F_LIBVER_EARLIEST_F = H5F_flags(17)
    H5F_LIBVER_LATEST_F   = H5F_flags(18)
    H5F_UNLIMITED_F       = H5F_flags(19)
    !
    ! H5generic flags
    !
    H5_INDEX_UNKNOWN_F   = H5generic_flags(1)
    H5_INDEX_NAME_F      = H5generic_flags(2)
    H5_INDEX_CRT_ORDER_F = H5generic_flags(3)
    H5_INDEX_N_F         = H5generic_flags(4)
    H5_ITER_UNKNOWN_F    = H5generic_flags(5)
    H5_ITER_INC_F        = H5generic_flags(6)
    H5_ITER_DEC_F        = H5generic_flags(7)
    H5_ITER_NATIVE_F     = H5generic_flags(8)
    H5_ITER_N_F          = H5generic_flags(9)
    
    HADDR_UNDEF_F = H5generic_haddr_flags(1)
    !
    ! H5G flags
    !
    H5G_UNKNOWN_F                   = H5G_flags(1) 
    H5G_GROUP_F                     = H5G_flags(2)
    H5G_DATASET_F                   = H5G_flags(3)
    H5G_TYPE_F                      = H5G_flags(4)
    H5G_SAME_LOC_F                  = H5G_flags(5)
    H5G_LINK_ERROR_F                = H5G_flags(6)
    H5G_LINK_HARD_F                 = H5G_flags(7)
    H5G_LINK_SOFT_F                 = H5G_flags(8)
    H5G_STORAGE_TYPE_UNKNOWN_F      = H5G_flags(9)
    H5G_STORAGE_TYPE_SYMBOL_TABLE_F = H5G_flags(10)
    H5G_STORAGE_TYPE_COMPACT_F      = H5G_flags(11)
    H5G_STORAGE_TYPE_DENSE_F        = H5G_flags(12)
    !
    ! H5D flags
    !
    H5D_COMPACT_F                  = H5D_flags(1)
    H5D_CONTIGUOUS_F               = H5D_flags(2)
    H5D_CHUNKED_F                  = H5D_flags(3)
    H5D_ALLOC_TIME_ERROR_F         = H5D_flags(4)
    H5D_ALLOC_TIME_DEFAULT_F       = H5D_flags(5)
    H5D_ALLOC_TIME_EARLY_F         = H5D_flags(6)
    H5D_ALLOC_TIME_LATE_F          = H5D_flags(7)
    H5D_ALLOC_TIME_INCR_F          = H5D_flags(8)
    H5D_SPACE_STS_ERROR_F          = H5D_flags(9)
    H5D_SPACE_STS_NOT_ALLOCATED_F  = H5D_flags(10)
    H5D_SPACE_STS_PART_ALLOCATED_F = H5D_flags(11)
    H5D_SPACE_STS_ALLOCATED_F      = H5D_flags(12)
    H5D_FILL_TIME_ERROR_F          = H5D_flags(13)
    H5D_FILL_TIME_ALLOC_F          = H5D_flags(14)
    H5D_FILL_TIME_NEVER_F          = H5D_flags(15)
    H5D_FILL_VALUE_ERROR_F         = H5D_flags(16)
    H5D_FILL_VALUE_UNDEFINED_F     = H5D_flags(17)
    H5D_FILL_VALUE_DEFAULT_F       = H5D_flags(18)
    H5D_FILL_VALUE_USER_DEFINED_F  = H5D_flags(19)
    H5D_CHUNK_CACHE_W0_DFLT_F      = H5D_flags(20)
    H5D_MPIO_NO_COLLECTIVE_F       = H5D_flags(21)
    H5D_MPIO_CHUNK_INDEPENDENT_F   = H5D_flags(22)
    H5D_MPIO_CHUNK_COLLECTIVE_F    = H5D_flags(23)
    H5D_MPIO_CHUNK_MIXED_F         = H5D_flags(24)
    H5D_MPIO_CONTIG_COLLECTIVE_F   = H5D_flags(25)
    H5D_VDS_ERROR_F                = H5D_flags(26)
    H5D_VDS_FIRST_MISSING_F        = H5D_flags(27)
    H5D_VDS_LAST_AVAILABLE_F       = H5D_flags(28)
    H5D_VIRTUAL_F                  = H5D_flags(29)
    
    H5D_CHUNK_CACHE_NSLOTS_DFLT_F = H5D_size_flags(1)
    H5D_CHUNK_CACHE_NBYTES_DFLT_F = H5D_size_flags(2)
    !
    ! H5E flags
    !
    H5E_DEFAULT_F = H5E_hid_flags(1)

    H5E_MAJOR_F         = H5E_flags(1)
    H5E_MINOR_F         = H5E_flags(2) 
    H5E_WALK_UPWARD_F   = H5E_flags(3)
    H5E_WALK_DOWNWARD_F = H5E_flags(4)
    !
    ! H5FD flags
    !
    H5FD_MPIO_INDEPENDENT_F = H5FD_flags(1)
    H5FD_MPIO_COLLECTIVE_F  = H5FD_flags(2)
    H5FD_MEM_NOLIST_F       = H5FD_flags(3)
    H5FD_MEM_DEFAULT_F      = H5FD_flags(4)
    H5FD_MEM_SUPER_F        = H5FD_flags(5)
    H5FD_MEM_BTREE_F        = H5FD_flags(6)
    H5FD_MEM_DRAW_F         = H5FD_flags(7)
    H5FD_MEM_GHEAP_F        = H5FD_flags(8)
    H5FD_MEM_LHEAP_F        = H5FD_flags(9)
    H5FD_MEM_OHDR_F         = H5FD_flags(10)
    H5FD_MEM_NTYPES_F       = H5FD_flags(11)
    !
    ! H5FD file driver flags
    !    
    H5FD_CORE_F   = H5FD_hid_flags(1)
    H5FD_FAMILY_F = H5FD_hid_flags(2)
    H5FD_LOG_F    = H5FD_hid_flags(3)
    H5FD_MPIO_F   = H5FD_hid_flags(4)
    H5FD_MULTI_F  = H5FD_hid_flags(5)
    H5FD_SEC2_F   = H5FD_hid_flags(6)
    H5FD_STDIO_F  = H5FD_hid_flags(7)
    !
    ! H5I flags declaration
    !
    H5I_FILE_F      = H5I_flags(1)
    H5I_GROUP_F     = H5I_flags(2)
    H5I_DATATYPE_F  = H5I_flags(3)
    H5I_DATASPACE_F = H5I_flags(4)
    H5I_DATASET_F   = H5I_flags(5)
    H5I_ATTR_F      = H5I_flags(6)
    H5I_BADID_F     = H5I_flags(7)
    !
    ! H5L flags
    !    
    H5L_TYPE_ERROR_F        = H5L_flags(1)
    H5L_TYPE_HARD_F         = H5L_flags(2)
    H5L_TYPE_SOFT_F         = H5L_flags(3)
    H5L_TYPE_EXTERNAL_F     = H5L_flags(4)
    H5L_SAME_LOC_F          = H5L_flags(5)
    H5L_LINK_CLASS_T_VERS_F = H5L_flags(6)
    !
    ! H5O flags
    !
    H5O_COPY_SHALLOW_HIERARCHY_F   = h5o_flags(1)
    H5O_COPY_EXPAND_SOFT_LINK_F    = h5o_flags(2)
    H5O_COPY_EXPAND_EXT_LINK_F     = h5o_flags(3)
    H5O_COPY_EXPAND_REFERENCE_F    = h5o_flags(4)
    H5O_COPY_WITHOUT_ATTR_FLAG_F   = h5o_flags(5)
    H5O_COPY_PRESERVE_NULL_FLAG_F  = h5o_flags(6)
    H5O_COPY_ALL_F                 = h5o_flags(7)
    H5O_SHMESG_NONE_FLAG_F         = h5o_flags(8) 
    H5O_SHMESG_SDSPACE_FLAG_F      = h5o_flags(9)
    H5O_SHMESG_DTYPE_FLAG_F        = h5o_flags(10)
    H5O_SHMESG_FILL_FLAG_F         = h5o_flags(11) 
    H5O_SHMESG_PLINE_FLAG_F        = h5o_flags(12) 
    H5O_SHMESG_ATTR_FLAG_F         = h5o_flags(13) 
    H5O_SHMESG_ALL_FLAG_F          = h5o_flags(14) 
    H5O_HDR_CHUNK0_SIZE_F          = h5o_flags(15) 
    H5O_HDR_ATTR_CRT_ORDER_TRACK_F = h5o_flags(16) 
    H5O_HDR_ATTR_CRT_ORDER_INDEX_F = h5o_flags(17) 
    H5O_HDR_ATTR_STORE_PHASE_CHA_F = h5o_flags(18) 
    H5O_HDR_STORE_TIMES_F          = h5o_flags(19) 
    H5O_HDR_ALL_FLAGS_F            = h5o_flags(20)
    H5O_SHMESG_MAX_NINDEXES_F      = h5o_flags(21) 
    H5O_SHMESG_MAX_LIST_SIZE_F     = h5o_flags(22) 
    H5O_TYPE_UNKNOWN_F             = h5o_flags(23) 
    H5O_TYPE_GROUP_F               = h5o_flags(24) 
    H5O_TYPE_DATASET_F             = h5o_flags(25) 
    H5O_TYPE_NAMED_DATATYPE_F      = h5o_flags(26) 
    H5O_TYPE_NTYPES_F              = h5o_flags(27)
    H5O_INFO_ALL_F                 = h5o_flags(28)
    H5O_INFO_BASIC_F               = h5o_flags(29)
    H5O_INFO_TIME_F                = h5o_flags(30)
    H5O_INFO_NUM_ATTRS_F           = h5o_flags(31)
    H5O_INFO_HDR_F                 = h5o_flags(32)
    H5O_INFO_META_SIZE_F           = h5o_flags(33)
    !
    ! H5P flags
    !    
    H5P_FILE_CREATE_F      = H5P_flags(1)
    H5P_FILE_ACCESS_F      = H5P_flags(2)
    H5P_DATASET_CREATE_F   = H5P_flags(3)
    H5P_DATASET_XFER_F     = H5P_flags(4)
    H5P_FILE_MOUNT_F       = H5P_flags(5)
    H5P_DEFAULT_F          = H5P_flags(6)
    H5P_ROOT_F             = H5P_flags(7)
    H5P_OBJECT_CREATE_F    = H5P_flags(8)
    H5P_DATASET_ACCESS_F   = H5P_flags(9)
    H5P_GROUP_CREATE_F     = H5P_flags(10)
    H5P_GROUP_ACCESS_F     = H5P_flags(11)
    H5P_DATATYPE_CREATE_F  = H5P_flags(12)
    H5P_DATATYPE_ACCESS_F  = H5P_flags(13)
    H5P_STRING_CREATE_F    = H5P_flags(14)
    H5P_ATTRIBUTE_CREATE_F = H5P_flags(15)
    H5P_OBJECT_COPY_F      = H5P_flags(16)
    H5P_LINK_CREATE_F      = H5P_flags(17)
    H5P_LINK_ACCESS_F      = H5P_flags(18)
    !
    ! H5P integers flags
    !    
    H5P_CRT_ORDER_INDEXED_F = H5P_flags_int(1)
    H5P_CRT_ORDER_TRACKED_F = H5P_flags_int(2)
    !
    ! H5R flags
    !    
    H5R_OBJECT_F         = H5R_flags(1)
    H5R_DATASET_REGION_F = H5R_flags(2)
    !
    ! H5S flags
    !    
    H5S_ALL_F = H5S_hid_flags(1)
    
    H5S_UNLIMITED_F = H5S_hsize_flags(1)
    
    H5S_SCALAR_F         = H5S_flags(1)
    H5S_SIMPLE_F         = H5S_flags(2)
    H5S_NULL_F           = H5S_flags(3)
    H5S_SELECT_SET_F     = H5S_flags(4)
    H5S_SELECT_OR_F      = H5S_flags(5)
    H5S_SELECT_NOOP_F    = H5S_flags(6)
    H5S_SELECT_AND_F     = H5S_flags(7)
    H5S_SELECT_XOR_F     = H5S_flags(8)
    H5S_SELECT_NOTB_F    = H5S_flags(9)
    H5S_SELECT_NOTA_F    = H5S_flags(10)
    H5S_SELECT_APPEND_F  = H5S_flags(11)
    H5S_SELECT_PREPEND_F = H5S_flags(12)
    H5S_SELECT_INVALID_F = H5S_flags(13)
    H5S_SEL_ERROR_F      = H5S_flags(14)
    H5S_SEL_NONE_F       = H5S_flags(15)
    H5S_SEL_POINTS_F     = H5S_flags(16)
    H5S_SEL_HYPERSLABS_F = H5S_flags(17)
    H5S_SEL_ALL_F        = H5S_flags(18)
    !
    ! H5T flags declaration
    !    
    H5T_NO_CLASS_F       = H5T_flags(1)
    H5T_INTEGER_F        = H5T_flags(2)
    H5T_FLOAT_F          = H5T_flags(3)
    H5T_TIME_F           = H5T_flags(4)
    H5T_STRING_F         = H5T_flags(5)
    H5T_BITFIELD_F       = H5T_flags(6)
    H5T_OPAQUE_F         = H5T_flags(7)
    H5T_COMPOUND_F       = H5T_flags(8)
    H5T_REFERENCE_F      = H5T_flags(9)
    H5T_ENUM_F           = H5T_flags(10)
    H5T_ORDER_LE_F       = H5T_flags(11)
    H5T_ORDER_BE_F       = H5T_flags(12)
    H5T_ORDER_MIXED_F    = H5T_flags(13)
    H5T_ORDER_VAX_F      = H5T_flags(14)
    H5T_ORDER_NONE_F     = H5T_flags(15)
    H5T_PAD_ZERO_F       = H5T_flags(16)
    H5T_PAD_ONE_F        = H5T_flags(17)
    H5T_PAD_BACKGROUND_F = H5T_flags(18)
    H5T_PAD_ERROR_F      = H5T_flags(19)
    H5T_SGN_NONE_F       = H5T_flags(20)
    H5T_SGN_2_F          = H5T_flags(21)
    H5T_SGN_ERROR_F      = H5T_flags(22)
    H5T_NORM_IMPLIED_F   = H5T_flags(23)
    H5T_NORM_MSBSET_F    = H5T_flags(24)
    H5T_NORM_NONE_F      = H5T_flags(25)
    H5T_CSET_ASCII_F     = H5T_flags(26)
    H5T_CSET_UTF8_F      = H5T_flags(27)
    H5T_STR_NULLTERM_F   = H5T_flags(28)
    H5T_STR_NULLPAD_F    = H5T_flags(29)
    H5T_STR_SPACEPAD_F   = H5T_flags(30)
    H5T_STR_ERROR_F      = H5T_flags(31)
    H5T_VLEN_F           = H5T_flags(32)
    H5T_ARRAY_F          = H5T_flags(33)
    H5T_DIR_ASCEND_F     = H5T_flags(34)
    H5T_DIR_DESCEND_F    = H5T_flags(35)
    !
    ! H5Z flags
    !
    H5Z_FILTER_ERROR_F           = H5Z_flags(1)
    H5Z_FILTER_NONE_F            = H5Z_flags(2)
    H5Z_FILTER_DEFLATE_F         = H5Z_flags(3)
    H5Z_FILTER_SHUFFLE_F         = H5Z_flags(4)
    H5Z_FILTER_FLETCHER32_F      = H5Z_flags(5)
    H5Z_ERROR_EDC_F              = H5Z_flags(6)
    H5Z_DISABLE_EDC_F            = H5Z_flags(7)
    H5Z_ENABLE_EDC_F             = H5Z_flags(8)
    H5Z_NO_EDC_F                 = H5Z_flags(9)
    H5Z_FILTER_SZIP_F            = H5Z_flags(10)
    H5Z_FLAG_OPTIONAL_F          = H5Z_flags(11)
    H5Z_FILTER_ENCODE_ENABLED_F  = H5Z_flags(12)
    H5Z_FILTER_DECODE_ENABLED_F  = H5Z_flags(13)
    H5Z_FILTER_ALL_F             = H5Z_flags(14)
    H5Z_FILTER_NBIT_F            = H5Z_flags(15)
    H5Z_FILTER_SCALEOFFSET_F     = H5Z_flags(16)
    H5Z_SO_FLOAT_DSCALE_F        = H5Z_flags(17)
    H5Z_SO_FLOAT_ESCALE_F        = H5Z_flags(18)
    H5Z_SO_INT_F                 = H5Z_flags(19)
    H5Z_SO_INT_MINBITS_DEFAULT_F = H5Z_flags(20)

    error = error + h5init1_flags_c(H5LIB_flags)
    !
    ! H5 Library flags
    !
    H5_SZIP_EC_OM_F = H5LIB_flags(1)
    H5_SZIP_NN_OM_F = H5LIB_flags(2)

    CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), H5F_OBJ_ALL_F, H5OPEN_NUM_OBJ_LOC,  error)

    H5OPEN_NUM_OBJ = H5OPEN_NUM_OBJ_LOC

  END SUBROUTINE h5open_f

!****s* H5LIB/h5close_f
!
! NAME
!  h5close_f
!
! PURPOSE
!  Closes HDF5 Fortran interface.
!
! Outputs:
!  error - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! Removed call to h5close_c since this may cause a problem for an
! application that uses HDF5 library outside HDF5 Fortran APIs.
!          October 13, 2011
! Fortran90 Interface:
  SUBROUTINE h5close_f(error)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: error
!*****
    INTERFACE
       INTEGER FUNCTION h5close_types_c(p_types, P_TYPES_LEN, &
            f_types, F_TYPES_LEN, &
            i_types, I_TYPES_LEN ) &
            BIND(C,NAME='h5close_types_c')
         IMPORT :: HID_T
         INTEGER :: P_TYPES_LEN
         INTEGER :: F_TYPES_LEN
         INTEGER :: I_TYPES_LEN
         INTEGER(HID_T), DIMENSION(1:P_TYPES_LEN) :: p_types
         INTEGER(HID_T), DIMENSION(1:F_TYPES_LEN) :: f_types
         INTEGER(HID_T), DIMENSION(1:I_TYPES_LEN) :: i_types
       END FUNCTION h5close_types_c
    END INTERFACE
    error = h5close_types_c(predef_types, PREDEF_TYPES_LEN, &
         floating_types, FLOATING_TYPES_LEN, &
         integer_types, INTEGER_TYPES_LEN )

  END SUBROUTINE h5close_f

!****s* H5LIB/h5get_libversion_f
!
! NAME
!  h5get_libversion_f
!
! PURPOSE
!  Returns the HDF5 LIbrary release number
!
! Outputs:
!  majnum - major version of the library
!  minum  - minor version of the library
!  relnum - release version of the library
!  error  - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  September 24, 2002
!
! Fortran90 Interface:
  SUBROUTINE h5get_libversion_f(majnum, minnum, relnum, error)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: majnum, minnum, relnum, error
!*****
    INTERFACE
       INTEGER FUNCTION h5get_libversion_c(majnum, minnum, relnum) &
            BIND(C,NAME='h5get_libversion_c')
         IMPLICIT NONE
         INTEGER, INTENT(OUT) :: majnum, minnum, relnum
       END FUNCTION h5get_libversion_c
    END INTERFACE

    error = h5get_libversion_c(majnum, minnum, relnum)

  END SUBROUTINE h5get_libversion_f

!****s* H5LIB/h5check_version_f
!
! NAME
!  h5check_version_f
!
! PURPOSE
!  Verifies that library versions are consistent.
!
! Inputs:
!  majnum - major version of the library
!  minum  - minor version of the library
!  relnum - release version of the library
!
! Outputs:
!  error - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  September 24, 2002
!
! Fortran90 Interface:
  SUBROUTINE h5check_version_f(majnum, minnum, relnum, error)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: majnum, minnum, relnum
    INTEGER, INTENT(OUT) :: error
!*****
    INTERFACE
       INTEGER FUNCTION h5check_version_c(majnum, minnum, relnum) &
            BIND(C,NAME='h5check_version_c')
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: majnum, minnum, relnum
       END FUNCTION h5check_version_c
    END INTERFACE

    error = h5check_version_c(majnum, minnum, relnum)

  END SUBROUTINE h5check_version_f
!****s* H5LIB/h5garbage_collect_f
!
! NAME
!  h5garbage_collect_f
!
! PURPOSE
!  Garbage collects on all free-lists of all types.
!
! Outputs:
!  error - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  September 24, 2002
!
! Fortran90 Interface:
  SUBROUTINE h5garbage_collect_f(error)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: error
!*****
    INTERFACE
       INTEGER FUNCTION h5garbage_collect_c() &
            BIND(C,NAME='h5garbage_collect_c')
       END FUNCTION h5garbage_collect_c
    END INTERFACE

    error = h5garbage_collect_c()

  END SUBROUTINE h5garbage_collect_f
!****s* H5LIB/h5dont_atexit_f
!
! NAME
!  h5dont_atexit_f
!
! PURPOSE
!  Instructs library not to install atexit cleanup routine.
!
! Outputs:
!  error - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  September 24, 2002
!
! Fortran90 Interface:
  SUBROUTINE h5dont_atexit_f(error)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: error
!*****
    INTERFACE
       INTEGER FUNCTION h5dont_atexit_c() &
            BIND(C,NAME='h5dont_atexit_c')
       END FUNCTION h5dont_atexit_c
    END INTERFACE

    error = h5dont_atexit_c()

  END SUBROUTINE h5dont_atexit_f

!****f* H5LIB/h5kind_to_type
!
! NAME
!  h5kind_to_type
!
! PURPOSE
!  Converts the KIND to the correct HDF type
!
! Inputs:
!  kind    - Fortran KIND parameter
!  flag    - Whether KIND is of type INTEGER or REAL:
!              H5_INTEGER_KIND - integer
!              H5_REAL_KIND    - real
! Outputs:
!  h5_type - Returns the type
!
! AUTHOR
!  M. Scot Breitenfeld
!  August 25, 2008
!
! Fortran90 Interface:
  INTEGER(HID_T) FUNCTION h5kind_to_type(ikind, flag) RESULT(h5_type)
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ikind
    INTEGER, INTENT(IN) :: flag
    INTEGER :: i
!*****

!#if H5_HAVE_Fortran_INTEGER_SIZEOF_16!=0
!    ! (1) The array index assumes INTEGER*16 the last integer in the series, and
!    ! (2) it should map to INTEGER*16 on most modern processors
!    H5T_NATIVE_INTEGER_KIND(H5_FORTRAN_NUM_INTEGER_KINDS)=SELECTED_INT_KIND(36)
!#endif

    h5_type = -1
    IF(flag.EQ.H5_INTEGER_KIND)THEN
       do_kind: DO i = 1, H5_FORTRAN_NUM_INTEGER_KINDS
          IF(ikind.EQ.Fortran_INTEGER_AVAIL_KINDS(i))THEN
             h5_type = H5T_NATIVE_INTEGER_KIND(i)
             EXIT do_kind
          ENDIF
       END DO do_kind
    ELSE IF(flag.EQ.H5_REAL_KIND)THEN
       IF(ikind.EQ.KIND(1.0_C_FLOAT))THEN
          h5_type = H5T_NATIVE_REAL_C_FLOAT
       ELSE IF(ikind.EQ.KIND(1.0_C_DOUBLE))THEN
          h5_type = H5T_NATIVE_REAL_C_DOUBLE
#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
       ELSE IF(ikind.EQ.KIND(1.0_C_LONG_DOUBLE))THEN
          h5_type = H5T_NATIVE_REAL_C_LONG_DOUBLE
#endif
#if H5_PAC_FC_MAX_REAL_PRECISION > 28
#if H5_HAVE_FLOAT128 == 1
       ELSE
          h5_type = H5T_NATIVE_FLOAT_128
#endif
#endif
       ENDIF
    ENDIF

  END FUNCTION h5kind_to_type

!****f* H5LIB_PROVISIONAL/h5offsetof
!
! NAME
!  h5offsetof
!
! PURPOSE
!  Computes the offset in memory
!
! Inputs:
!  start - starting pointer address
!  end 	 - ending pointer address
!
! Outputs:
!  offset - offset of a member within the derived type
!
! AUTHOR
!  M. Scot Breitenfeld
!  Augest 25, 2008
!
! ACKNOWLEDGEMENTS
!  Joe Krahn
!
! Fortran2003 Interface:
  FUNCTION h5offsetof(start,end) RESULT(offset)
    IMPLICIT NONE
    INTEGER(SIZE_T) :: offset
    TYPE(C_PTR), VALUE, INTENT(IN) :: start, end
!*****
    INTEGER(C_INTPTR_T) :: int_address_start, int_address_end
    int_address_start = TRANSFER(start, int_address_start)
    int_address_end   = TRANSFER(end  , int_address_end  )

    offset = int_address_end - int_address_start

  END FUNCTION h5offsetof

!****f* H5LIB_PROVISIONAL/h5gmtime
!
! NAME
!  h5gmtime
!
! PURPOSE
!  Convert time_t structure (C) to Fortran DATE AND TIME storage format.
!
! Inputs:
!  stdtime_t - Object of type time_t that contains a time value
!
! Outputs:
!   datetime - A date/time array using Fortran conventions:
!   datetime(1)     = year
!   datetime(2)     = month
!   datetime(3)     = day
!   datetime(4)     = 0 ! time is expressed as UTC (or GMT timezone) */
!   datetime(5)     = hour
!   datetime(6)     = minute
!   datetime(7)     = second
!   datetime(8)     = millisecond -- not available, assigned - HUGE(0)
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2019
!
! Fortran Interface:
  FUNCTION h5gmtime(stdtime_t)
    IMPLICIT NONE
    INTEGER(KIND=TIME_T), INTENT(IN) :: stdtime_t
    INTEGER, DIMENSION(1:8) :: h5gmtime
!*****
    TYPE(C_PTR) :: cptr
    INTEGER(C_INT), DIMENSION(:), POINTER :: c_time

    INTERFACE
       FUNCTION gmtime(stdtime_t) BIND(C, NAME='gmtime')
         IMPORT :: TIME_T, C_PTR
         IMPLICIT NONE
         INTEGER(KIND=TIME_T) :: stdtime_t
         TYPE(C_PTR) :: gmtime
       END FUNCTION gmtime
    END INTERFACE

    cptr = gmtime(stdtime_t)
    CALL C_F_POINTER(cptr, c_time, [9])

    h5gmtime(1) = INT(c_time(6)+1900) ! year starts at 1900
    h5gmtime(2) = INT(c_time(5)+1)    ! month starts at 0 in C
    h5gmtime(3) = INT(c_time(4))      ! day
    h5gmtime(4) = 0                   ! time is expressed as UTC (or GMT timezone)
    h5gmtime(5) = INT(c_time(3))      ! hour
    h5gmtime(6) = INT(c_time(2))      ! minute
    h5gmtime(7) = INT(c_time(1))      ! second
    h5gmtime(8) = -32767              ! millisecond is not available, assign it -HUGE(0)

  END FUNCTION h5gmtime

END MODULE H5LIB
