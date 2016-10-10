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
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!   access to either file, you may request a copy from help@hdfgroup.org.     *
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

  USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_ptr, C_INTPTR_T
  USE H5GLOBAL
  IMPLICIT NONE

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
!          October 13, 2011
! Fortran90 Interface:
  SUBROUTINE h5open_f(error)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: error
!*****
    INTEGER ::  error_1, error_2, error_3

    INTERFACE
       INTEGER FUNCTION h5init_types_c(p_types, f_types, i_types) &
            BIND(C,NAME='h5init_types_c')
         IMPORT :: HID_T
         IMPORT :: PREDEF_TYPES_LEN, FLOATING_TYPES_LEN, INTEGER_TYPES_LEN
         IMPLICIT NONE
         INTEGER(HID_T), DIMENSION(PREDEF_TYPES_LEN) :: p_types
         INTEGER(HID_T), DIMENSION(FLOATING_TYPES_LEN) :: f_types
         INTEGER(HID_T), DIMENSION(INTEGER_TYPES_LEN) :: i_types
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
         INTEGER i_H5D_flags(H5D_FLAGS_LEN)
         INTEGER(SIZE_T) i_H5D_size_flags(H5D_SIZE_FLAGS_LEN)
         INTEGER i_H5E_flags(H5E_FLAGS_LEN)
         INTEGER(HID_T) i_H5E_hid_flags(H5E_HID_FLAGS_LEN)
         INTEGER i_H5F_flags(H5F_FLAGS_LEN)
         INTEGER i_H5G_flags(H5G_FLAGS_LEN)
         INTEGER i_H5FD_flags(H5FD_FLAGS_LEN)
         INTEGER(HID_T) i_H5FD_hid_flags(H5FD_HID_FLAGS_LEN)
         INTEGER i_H5I_flags(H5I_FLAGS_LEN)
         INTEGER i_H5L_flags(H5L_FLAGS_LEN)
         INTEGER i_H5O_flags(H5O_FLAGS_LEN)
         INTEGER(HID_T) i_H5P_flags(H5P_FLAGS_LEN)
         INTEGER i_H5P_flags_int(H5P_FLAGS_INT_LEN)
         INTEGER i_H5R_flags(H5R_FLAGS_LEN)
         INTEGER i_H5S_flags(H5S_FLAGS_LEN)
         INTEGER(HID_T) i_H5S_hid_flags(H5S_HID_FLAGS_LEN)
         INTEGER(HSIZE_T) i_H5S_hsize_flags(H5S_HSIZE_FLAGS_LEN)
         INTEGER i_H5T_flags(H5T_FLAGS_LEN)
         INTEGER i_H5Z_flags(H5Z_FLAGS_LEN)
         INTEGER i_H5generic_flags(H5generic_FLAGS_LEN)
         INTEGER(HADDR_T) i_H5generic_haddr_flags(H5generic_haddr_FLAGS_LEN)
       END FUNCTION h5init_flags_c
    END INTERFACE
    INTERFACE
       INTEGER FUNCTION h5init1_flags_c( i_H5LIB_flags ) &
            BIND(C,NAME='h5init1_flags_c')
         IMPORT :: H5LIB_FLAGS_LEN
         IMPLICIT NONE
         INTEGER i_H5LIB_flags(H5LIB_FLAGS_LEN)
       END FUNCTION h5init1_flags_c
    END INTERFACE
    error_1 = h5init_types_c(predef_types, floating_types, integer_types)
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
         H5S_hid_flags, &
         H5S_hsize_flags, &
         H5T_flags, &
         H5Z_flags, &
         H5generic_flags,&
         H5generic_haddr_flags)
    error_3 = h5init1_flags_c(H5LIB_flags )
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
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: error
!*****
    INTEGER :: error_1
    INTERFACE
       INTEGER FUNCTION h5close_types_c(p_types, P_TYPES_LEN, &
            f_types, F_TYPES_LEN, &
            i_types, I_TYPES_LEN ) &
            BIND(C,NAME='h5close_types_c')
         IMPORT :: HID_T
         INTEGER P_TYPES_LEN
         INTEGER F_TYPES_LEN
         INTEGER I_TYPES_LEN
         INTEGER(HID_T), DIMENSION(P_TYPES_LEN) :: p_types
         INTEGER(HID_T), DIMENSION(F_TYPES_LEN) :: f_types
         INTEGER(HID_T), DIMENSION(I_TYPES_LEN) :: i_types
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

END MODULE H5LIB
