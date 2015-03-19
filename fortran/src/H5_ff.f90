!****h* ROBODoc/H5LIB
!
! NAME
!  MODULE H5LIB
!
! PURPOSE
!  This module provides fortran specific helper functions for the HDF library
!
! USES
!  H5LIB_PROVISIONAL - This module provides helper functions for Fortran 2003
!                      only features. If Fortran 2003 functions are enabled then
!                      H5_ff_F03.f90 is compiled, else H5_ff_F90.f90,
!                      which is just a place holder blank module, is compiled.
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!   access to either file, you may request a copy from help@hdfgroup.org.     *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES
!                         *** IMPORTANT ***
!  If you add a new function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5LIB

  USE H5LIB_PROVISIONAL  ! helper functions for Fortran 2003 features:
                         !       pre-Fortran 2003 - empty module
                         !       Forttran 2003    - contains functions
  USE H5GLOBAL

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
! NOTES
!  The size of the C arrays in H5_f.c has to match the values of the variables
!  declared as PARAMETER, hence if the size of an array in H5_f.c is changed
!  then the PARAMETER of that corresponding array in Fortran must also be changed.
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! Removed call to h5open_c since this may cause a problem for an
! application that uses HDF5 library outside HDF5 Fortran APIs.
!          October 13, 2011
! Fortran90 Interface:
  SUBROUTINE h5open_f(error)
    USE H5GLOBAL
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: error
    !*****
    !
    ! H5F flags
    !
    INTEGER, PARAMETER :: H5F_FLAGS_LEN = 19
    INTEGER :: H5F_flags(H5F_FLAGS_LEN)
    !
    ! H5generic flags declaration
    !
    INTEGER, PARAMETER :: H5generic_FLAGS_LEN = 9
    INTEGER :: H5generic_flags(H5generic_FLAGS_LEN)
    !
    ! H5G flags declaration
    !
    INTEGER, PARAMETER :: H5G_FLAGS_LEN = 12
    INTEGER :: H5G_flags(H5G_FLAGS_LEN)
    !
    ! H5D flags declaration
    !
    INTEGER, PARAMETER :: H5D_FLAGS_LEN = 25
    INTEGER :: H5D_flags(H5D_FLAGS_LEN)
    INTEGER, PARAMETER :: H5D_SIZE_FLAGS_LEN = 2
    INTEGER(SIZE_T) :: H5D_size_flags(H5D_SIZE_FLAGS_LEN)
    !
    ! H5E flags declaration
    !
    INTEGER, PARAMETER :: H5E_FLAGS_LEN = 4
    INTEGER :: H5E_flags(H5E_FLAGS_LEN)
    INTEGER, PARAMETER :: H5E_HID_FLAGS_LEN = 1
    INTEGER(HID_T) :: H5E_hid_flags(H5E_HID_FLAGS_LEN)
    !
    ! H5FD flags declaration
    !
    INTEGER, PARAMETER :: H5FD_FLAGS_LEN = 11
    INTEGER :: H5FD_flags(H5FD_FLAGS_LEN)
    !
    ! H5FD file drivers flags declaration
    !
    INTEGER, PARAMETER :: H5FD_HID_FLAGS_LEN = 7
    INTEGER(HID_T) :: H5FD_hid_flags(H5FD_HID_FLAGS_LEN)
    !
    ! H5I flags declaration
    !
    INTEGER, PARAMETER :: H5I_FLAGS_LEN = 7
    INTEGER :: H5I_flags(H5I_FLAGS_LEN)
    !
    ! H5L flags declaration
    !
    INTEGER, PARAMETER :: H5L_FLAGS_LEN = 6
    INTEGER :: H5L_flags(H5L_FLAGS_LEN)
    !
    ! H5O flags declaration
    !
    INTEGER, PARAMETER :: H5O_FLAGS_LEN = 27
    INTEGER :: H5o_flags(H5O_FLAGS_LEN)
    !
    ! H5P flags declaration
    !
    INTEGER, PARAMETER :: H5P_FLAGS_LEN = 18
    INTEGER(HID_T) H5P_flags(H5P_FLAGS_LEN)
    !
    ! H5P integers flags declaration
    !
    INTEGER, PARAMETER :: H5P_FLAGS_INT_LEN = 2
    INTEGER :: H5P_flags_int(H5P_FLAGS_INT_LEN)
    !
    ! H5R flags declaration
    !
    INTEGER, PARAMETER :: H5R_FLAGS_LEN = 2
    INTEGER :: H5R_flags(H5R_FLAGS_LEN)
    !
    ! H5S flags declaration
    !
    INTEGER, PARAMETER :: H5S_FLAGS_LEN = 19
    INTEGER :: H5S_flags(H5S_FLAGS_LEN)
    INTEGER, PARAMETER :: H5S_HSIZE_FLAGS_LEN = 1
    INTEGER(HSIZE_T) :: H5S_hsize_flags(H5S_HSIZE_FLAGS_LEN)
    !
    ! H5T flags declaration
    !
    INTEGER, PARAMETER :: H5T_FLAGS_LEN = 35
    INTEGER :: H5T_flags(H5T_FLAGS_LEN)
    !
    ! H5Z flags declaration
    !
    INTEGER, PARAMETER :: H5Z_FLAGS_LEN = 20
    INTEGER :: H5Z_flags(H5Z_FLAGS_LEN)
    !
    ! H5 Library flags declaration
    !
    INTEGER, PARAMETER :: H5LIB_FLAGS_LEN =  2
    INTEGER :: H5LIB_flags(H5LIB_FLAGS_LEN)

    INTEGER ::  error_1, error_2, error_3

    INTERFACE
       INTEGER FUNCTION h5init_types_c(p_types, f_types, i_types)
         USE H5GLOBAL
         IMPLICIT NONE
         INTEGER(HID_T), DIMENSION(*) :: p_types
         INTEGER(HID_T), DIMENSION(*) :: f_types
         INTEGER(HID_T), DIMENSION(*) :: i_types
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5INIT_TYPES_C'::h5init_types_c
         !DEC$ENDIF
       END FUNCTION h5init_types_c
    END INTERFACE
    INTERFACE
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
            i_H5S_hsize_flags, &
            i_H5T_flags, &
            i_H5Z_flags, &
            i_H5generic_flags)
         USE H5GLOBAL
         IMPLICIT NONE
         INTEGER, DIMENSION(*) :: i_H5D_flags
         INTEGER(SIZE_T), DIMENSION(*) :: i_H5D_size_flags
         INTEGER, DIMENSION(*) :: i_H5E_flags
         INTEGER(HID_T), DIMENSION(*) :: i_H5E_hid_flags
         INTEGER, DIMENSION(*) :: i_H5F_flags
         INTEGER, DIMENSION(*) :: i_H5G_flags
         INTEGER, DIMENSION(*) :: i_H5FD_flags
         INTEGER(HID_T), DIMENSION(*) :: i_H5FD_hid_flags
         INTEGER, DIMENSION(*) :: i_H5I_flags
         INTEGER, DIMENSION(*) :: i_H5L_flags
         INTEGER, DIMENSION(*) :: i_H5O_flags
         INTEGER(HID_T), DIMENSION(*) :: i_H5P_flags
         INTEGER, DIMENSION(*) :: i_H5P_flags_int
         INTEGER, DIMENSION(*) :: i_H5R_flags
         INTEGER, DIMENSION(*) :: i_H5S_flags
         INTEGER(HSIZE_T), DIMENSION(*) :: i_H5S_hsize_flags
         INTEGER, DIMENSION(*) :: i_H5T_flags
         INTEGER, DIMENSION(*) :: i_H5Z_flags
         INTEGER, DIMENSION(*) :: i_H5generic_flags
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5INIT_FLAGS_C'::h5init_flags_c
         !DEC$ENDIF
       END FUNCTION h5init_flags_c
    END INTERFACE
    INTERFACE
       INTEGER FUNCTION h5init1_flags_c( i_H5LIB_flags )
         IMPLICIT NONE
         INTEGER, DIMENSION(*) :: i_H5LIB_flags
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5INIT1_FLAGS_C'::h5init1_flags_c
         !DEC$ENDIF
       END FUNCTION h5init1_flags_c
    END INTERFACE

    error_1 = h5init_types_c(predef_types, floating_types, integer_types)

    H5T_NATIVE_INTEGER   = predef_types(1)
    H5T_NATIVE_REAL      = predef_types(2)
    H5T_NATIVE_DOUBLE    = predef_types(3)
    H5T_NATIVE_CHARACTER = predef_types(4)
    H5T_STD_REF_OBJ      = predef_types(5)
    H5T_STD_REF_DSETREG  = predef_types(6)
    H5T_NATIVE_INTEGER_1 = predef_types(7)
    H5T_NATIVE_INTEGER_2 = predef_types(8)
    H5T_NATIVE_INTEGER_4 = predef_types(9)
    H5T_NATIVE_INTEGER_8 = predef_types(10)
    H5T_NATIVE_REAL_4    = predef_types(11)
    H5T_NATIVE_REAL_8    = predef_types(12)
    H5T_NATIVE_REAL_16   = predef_types(13)
    H5T_NATIVE_B8        = predef_types(14)
    H5T_NATIVE_B16       = predef_types(15)
    H5T_NATIVE_B32       = predef_types(16)
    H5T_NATIVE_B64       = predef_types(17)

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

    error_2 = h5init_flags_c(H5D_flags, &
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
         H5S_hsize_flags, &
         H5T_flags, &
         H5Z_flags, &
         H5generic_flags)

    !
    ! H5F flags declaration
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
    ! H5generic flags declaration
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
    !
    ! H5G flags declaration
    !
    H5G_UNKNOWN_F                   = H5G_flags(1) ! Unknown object type
    H5G_GROUP_F                     = H5G_flags(2) ! Object is a group
    H5G_DATASET_F                   = H5G_flags(3) ! Object is a dataset
    H5G_TYPE_F                      = H5G_flags(4) ! Object is a named data type
    H5G_SAME_LOC_F                  = H5G_flags(5)
    H5G_LINK_ERROR_F                = H5G_flags(6)
    H5G_LINK_HARD_F                 = H5G_flags(7)
    H5G_LINK_SOFT_F                 = H5G_flags(8)
    H5G_STORAGE_TYPE_UNKNOWN_F      = H5G_flags(9)
    H5G_STORAGE_TYPE_SYMBOL_TABLE_F = H5G_flags(10)
    H5G_STORAGE_TYPE_COMPACT_F      = H5G_flags(11)
    H5G_STORAGE_TYPE_DENSE_F        = H5G_flags(12)
    !
    ! H5D flags declaration
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
    H5D_CHUNK_CACHE_NSLOTS_DFLT_F  = H5D_size_flags(1)
    H5D_CHUNK_CACHE_NBYTES_DFLT_F  = H5D_size_flags(2)
    !
    ! H5E flags declaration
    !
    H5E_DEFAULT_F       = H5E_hid_flags(1)
    H5E_MAJOR_F         = H5E_flags(1)    
    H5E_MINOR_F         = H5E_flags(2)    
    H5E_WALK_UPWARD_F   = H5E_flags(3)    
    H5E_WALK_DOWNWARD_F = H5E_flags(4)    
    !
    ! H5FD flags declaration
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
    ! H5FD file drivers flags declaration
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
    ! H5L flags declaration
    !
    H5L_TYPE_ERROR_F        = H5L_flags(1)
    H5L_TYPE_HARD_F         = H5L_flags(2)
    H5L_TYPE_SOFT_F         = H5L_flags(3)
    H5L_TYPE_EXTERNAL_F     = H5L_flags(4)
    H5L_SAME_LOC_F          = H5L_flags(5)
    H5L_LINK_CLASS_T_VERS_F = H5L_flags(6)
    !
    ! H5O flags declaration
    !
    H5O_COPY_SHALLOW_HIERARCHY_F   = H5O_flags(1) 
    H5O_COPY_EXPAND_SOFT_LINK_F    = H5O_flags(2) 
    H5O_COPY_EXPAND_EXT_LINK_F     = H5O_flags(3) 
    H5O_COPY_EXPAND_REFERENCE_F    = H5O_flags(4) 
    H5O_COPY_WITHOUT_ATTR_FLAG_F   = H5O_flags(5) 
    H5O_COPY_PRESERVE_NULL_FLAG_F  = H5O_flags(6) 
    H5O_COPY_ALL_F                 = H5O_flags(7) 
    H5O_SHMESG_NONE_FLAG_F         = H5O_flags(8) 
    H5O_SHMESG_SDSPACE_FLAG_F      = H5O_flags(9) 
    H5O_SHMESG_DTYPE_FLAG_F        = H5O_flags(10) 
    H5O_SHMESG_FILL_FLAG_F         = H5O_flags(11) 
    H5O_SHMESG_PLINE_FLAG_F        = H5O_flags(12) 
    H5O_SHMESG_ATTR_FLAG_F         = H5O_flags(13) 
    H5O_SHMESG_ALL_FLAG_F          = H5O_flags(14) 
    H5O_HDR_CHUNK0_SIZE_F          = H5O_flags(15) 
    H5O_HDR_ATTR_CRT_ORDER_TRACK_F = H5O_flags(16) 
    H5O_HDR_ATTR_CRT_ORDER_INDEX_F = H5O_flags(17) 
    H5O_HDR_ATTR_STORE_PHASE_CHA_F = H5O_flags(18) 
    H5O_HDR_STORE_TIMES_F          = H5O_flags(19) 
    H5O_HDR_ALL_FLAGS_F            = H5O_flags(20) 
    H5O_SHMESG_MAX_NINDEXES_F      = H5O_flags(21) 
    H5O_SHMESG_MAX_LIST_SIZE_F     = H5O_flags(22) 
    H5O_TYPE_UNKNOWN_F             = H5O_flags(23) 
    H5O_TYPE_GROUP_F               = H5O_flags(24) 
    H5O_TYPE_DATASET_F             = H5O_flags(25) 
    H5O_TYPE_NAMED_DATATYPE_F      = H5O_flags(26) 
    H5O_TYPE_NTYPES_F              = H5O_flags(27) 
    !
    ! H5P integers flags declaration
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
    ! H5P integers flags declaration
    !
    H5P_CRT_ORDER_INDEXED_F = H5P_flags_int(1)
    H5P_CRT_ORDER_TRACKED_F = H5P_flags_int(2)
    !
    ! H5R flags declaration
    !
    H5R_OBJECT_F         = H5R_flags(1)
    H5R_DATASET_REGION_F = H5R_flags(2)
    !
    ! H5S flags declaration
    !
    H5S_UNLIMITED_F      = H5S_hsize_flags(1)
    H5S_SCALAR_F         = H5S_flags(1)
    H5S_SIMPLE_F         = H5S_flags(2)
    H5S_NULL_F           = H5S_flags(3)
    H5S_SELECT_SET_F     = H5S_flags(4)
    H5S_SELECT_OR_F      = H5S_flags(5)
    H5S_ALL_F            = H5S_flags(6)
    H5S_SELECT_NOOP_F    = H5S_flags(7)
    H5S_SELECT_AND_F     = H5S_flags(8)
    H5S_SELECT_XOR_F     = H5S_flags(9)
    H5S_SELECT_NOTB_F    = H5S_flags(10)
    H5S_SELECT_NOTA_F    = H5S_flags(11)
    H5S_SELECT_APPEND_F  = H5S_flags(12)
    H5S_SELECT_PREPEND_F = H5S_flags(13)
    H5S_SELECT_INVALID_F = H5S_flags(14)
    H5S_SEL_ERROR_F      = H5S_flags(15)
    H5S_SEL_NONE_F       = H5S_flags(16)
    H5S_SEL_POINTS_F     = H5S_flags(17)
    H5S_SEL_HYPERSLABS_F = H5S_flags(18)
    H5S_SEL_ALL_F        = H5S_flags(19)
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
    ! H5Z flags declaration
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

    error_3 = h5init1_flags_c(H5LIB_flags )
    !
    ! H5 Library flags declaration
    !
    H5_SZIP_EC_OM_F = H5LIB_flags(1)
    H5_SZIP_NN_OM_F = H5LIB_flags(2)

    error = error_1 + error_2 + error_3

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
    USE H5GLOBAL
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: error
!*****
    INTEGER :: error_1
    INTERFACE
       INTEGER FUNCTION h5close_types_c(p_types, P_TYPES_LEN, &
            f_types, F_TYPES_LEN, &
            i_types, I_TYPES_LEN )
         USE H5GLOBAL
         INTEGER P_TYPES_LEN
         INTEGER F_TYPES_LEN
         INTEGER I_TYPES_LEN
         INTEGER(HID_T), DIMENSION(*) :: p_types
         INTEGER(HID_T), DIMENSION(*) :: f_types
         INTEGER(HID_T), DIMENSION(*) :: i_types
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5CLOSE_TYPES_C'::h5close_types_c
         !DEC$ENDIF
       END FUNCTION h5close_types_c
    END INTERFACE
    error_1 = h5close_types_c(predef_types, PREDEF_TYPES_LEN, &
         floating_types, FLOATING_TYPES_LEN, &
         integer_types, INTEGER_TYPES_LEN )
    error = error_1

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
    USE H5GLOBAL
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: majnum, minnum, relnum, error
!*****
    INTERFACE
       INTEGER FUNCTION h5get_libversion_c(majnum, minnum, relnum)
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5GET_LIBVERSION_C'::h5get_libversion_c
         !DEC$ENDIF
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
    USE H5GLOBAL
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: majnum, minnum, relnum
    INTEGER, INTENT(OUT) :: error
!*****
    INTERFACE
       INTEGER FUNCTION h5check_version_c(majnum, minnum, relnum)
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5CHECK_VERSION_C'::h5check_version_c
         !DEC$ENDIF
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
    USE H5GLOBAL
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: error
!*****
    INTERFACE
       INTEGER FUNCTION h5garbage_collect_c()
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5GARBAGE_COLLECT_C'::h5garbage_collect_c
         !DEC$ENDIF
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
    USE H5GLOBAL
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: error
!*****
    INTERFACE
       INTEGER FUNCTION h5dont_atexit_c()
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DONT_ATEXIT_C'::h5dont_atexit_c
         !DEC$ENDIF
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
  INTEGER(HID_T) FUNCTION h5kind_to_type(kind, flag) RESULT(h5_type)
    USE H5GLOBAL
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: kind
    INTEGER, INTENT(IN) :: flag
!*****
    IF(flag.EQ.H5_INTEGER_KIND)THEN
       IF(kind.EQ.Fortran_INTEGER_1)THEN
          h5_type = H5T_NATIVE_INTEGER_1
       ELSE IF(kind.EQ.Fortran_INTEGER_2)THEN
          h5_type = H5T_NATIVE_INTEGER_2
       ELSE IF(kind.EQ.Fortran_INTEGER_4)THEN
          h5_type = H5T_NATIVE_INTEGER_4
       ELSE IF(kind.EQ.Fortran_INTEGER_8)THEN
          h5_type = H5T_NATIVE_INTEGER_8
       ENDIF
    ELSE IF(flag.EQ.H5_REAL_KIND)THEN
       IF(kind.EQ.Fortran_REAL_4)THEN
          h5_type = H5T_NATIVE_REAL_4
       ELSE IF(kind.EQ.Fortran_REAL_8)THEN
          h5_type = H5T_NATIVE_REAL_8
       ELSE IF(kind.EQ.Fortran_REAL_16)THEN
          h5_type = H5T_NATIVE_REAL_16
       ENDIF
    ENDIF

  END FUNCTION h5kind_to_type

END MODULE H5LIB
