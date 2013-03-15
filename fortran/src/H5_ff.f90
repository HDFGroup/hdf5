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
    INTEGER :: error_1, error_2, error_3

    INTERFACE
       INTEGER FUNCTION h5init_types_c(p_types, f_types, i_types)
         USE H5GLOBAL
         INTEGER(HID_T), DIMENSION(PREDEF_TYPES_LEN) :: p_types
         INTEGER(HID_T), DIMENSION(FLOATING_TYPES_LEN) :: f_types
         INTEGER(HID_T), DIMENSION(INTEGER_TYPES_LEN) :: i_types
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5INIT_TYPES_C'::h5init_types_c
         !DEC$ENDIF
       END FUNCTION h5init_types_c
    END INTERFACE
    INTERFACE
       INTEGER FUNCTION h5init_flags_c(i_H5D_flags, &
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
         INTEGER i_H5D_flags(H5D_FLAGS_LEN)
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
         INTEGER(HSIZE_T) i_H5S_hsize_flags(H5S_HSIZE_FLAGS_LEN)
         INTEGER i_H5T_flags(H5T_FLAGS_LEN)
         INTEGER i_H5Z_flags(H5Z_FLAGS_LEN)
         INTEGER i_H5generic_flags(H5generic_FLAGS_LEN)
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5INIT_FLAGS_C'::h5init_flags_c
         !DEC$ENDIF
       END FUNCTION h5init_flags_c
    END INTERFACE
    INTERFACE
       INTEGER FUNCTION h5init1_flags_c( i_H5LIB_flags )
         USE H5GLOBAL
         INTEGER i_H5LIB_flags(H5LIB_FLAGS_LEN)
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5INIT1_FLAGS_C'::h5init1_flags_c
         !DEC$ENDIF
       END FUNCTION h5init1_flags_c
    END INTERFACE
    error_1 = h5init_types_c(predef_types, floating_types, integer_types)
    error_2 = h5init_flags_c(H5D_flags, &
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
         INTEGER(HID_T), DIMENSION(P_TYPES_LEN) :: p_types
         INTEGER(HID_T), DIMENSION(F_TYPES_LEN) :: f_types
         INTEGER(HID_T), DIMENSION(I_TYPES_LEN) :: i_types
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
!  flag    - whether KIND is of type INTEGER or REAL:
!              H5_INTEGER_KIND - integer
!              H5_REAL_KIND    - real
! Outputs:
!  h5_type - returns the type
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
