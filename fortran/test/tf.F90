!****h* root/fortran/test/tf.f90
!
! NAME
!  tf.f90
!
! FUNCTION
!  Contains subroutines which are needed in all the hdf5 fortran tests
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
! CONTAINS SUBROUTINES
!  write_test_status, check, verify, verifyLogical, verifyString, h5_fixname_f,
!  h5_cleanup_f, h5_exit_f, h5_env_nocleanup_f,dreal_eqv
!
!*****

#include "H5config_f.inc"

MODULE TH5_MISC

  USE, INTRINSIC :: ISO_C_BINDING

  IMPLICIT NONE

  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(5)  ! This should map to REAL*4 on most modern processors
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(10) ! This should map to REAL*8 on most modern processors

  ! generic compound datatype
  TYPE :: comp_datatype
    SEQUENCE
    REAL :: a
    INTEGER :: x
    DOUBLE PRECISION :: y
    CHARACTER(KIND=C_CHAR) :: z
  END TYPE comp_datatype

  PUBLIC :: H5_SIZEOF
  INTERFACE H5_SIZEOF
     MODULE PROCEDURE H5_SIZEOF_CMPD
     MODULE PROCEDURE H5_SIZEOF_CHR
     MODULE PROCEDURE H5_SIZEOF_I
     MODULE PROCEDURE H5_SIZEOF_SP,H5_SIZEOF_DP
  END INTERFACE

CONTAINS

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: write_test_status
!DEC$endif
  SUBROUTINE write_test_status( test_result, test_title, total_error)

    ! Writes the results of the tests

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: test_result  ! negative,  --skip --
                                        ! 0       ,   passed
                                        ! positive,   failed

    CHARACTER(LEN=*), INTENT(IN) :: test_title ! Short description of test
    INTEGER, INTENT(INOUT) :: total_error ! Accumulated error

! Controls the output style for reporting test results

    CHARACTER(LEN=8) :: error_string
    CHARACTER(LEN=8), PARAMETER :: success = ' PASSED '
    CHARACTER(LEN=8), PARAMETER :: failure = '*FAILED*'
    CHARACTER(LEN=8), PARAMETER :: skip    = '--SKIP--'
    

    error_string = failure
    IF (test_result ==  0) THEN
       error_string = success
    ELSE IF (test_result == -1) THEN
       error_string = skip
    ENDIF

    WRITE(*, fmt = '(A, T80, A)') test_title, error_string

    IF(test_result.GT.0) total_error = total_error + test_result

  END SUBROUTINE write_test_status


!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: check
!DEC$endif
  SUBROUTINE check(string,error,total_error)
    CHARACTER(LEN=*) :: string
    INTEGER :: error, total_error
    IF (error .LT. 0) THEN
       total_error=total_error+1
       WRITE(*,*) string, " FAILED"
    ENDIF
    RETURN
  END SUBROUTINE check

!----------------------------------------------------------------------
! Name:		h5_fixname_f
!
! Purpose:	Create a file name from the a file base name.
!               It is a fortran counterpart for the h5_fixname in ../../test/h5test.c
!
! Inputs:
!		base_name	- base name of the file
!               fapl		- file access property list
! Outputs:
!		full_name	- full file name
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! Programmer:	Elena Pourmal
!		September 13, 2002
!
!
!----------------------------------------------------------------------
  SUBROUTINE h5_fixname_f(base_name, full_name, fapl, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_fixname_f
!DEC$endif
    USE H5GLOBAL
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: base_name   ! base name
    CHARACTER(LEN=*), INTENT(IN) :: full_name   ! full name
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    INTEGER(HID_T), INTENT(IN) :: fapl ! file access property list

    INTEGER(SIZE_T) :: base_namelen ! Length of the base name character string
    INTEGER(SIZE_T) :: full_namelen ! Length of the full name character string
!            INTEGER(HID_T) :: fapl_default

    INTERFACE
       INTEGER FUNCTION h5_fixname_c(base_name, base_namelen, fapl, &
            full_name, full_namelen)
         USE H5GLOBAL
         !DEC$ IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5_FIXNAME_C':: h5_fixname_c
         !DEC$ ENDIF
         !DEC$ATTRIBUTES reference :: base_name
         !DEC$ATTRIBUTES reference :: full_name
         CHARACTER(LEN=*), INTENT(IN) :: base_name
         INTEGER(SIZE_T) :: base_namelen
         INTEGER(HID_T), INTENT(IN) :: fapl
         CHARACTER(LEN=*), INTENT(IN) :: full_name
         INTEGER(SIZE_T) :: full_namelen
       END FUNCTION h5_fixname_c
    END INTERFACE

    base_namelen = LEN(base_name)
    full_namelen = LEN(full_name)
    hdferr = h5_fixname_c(base_name, base_namelen, fapl, &
         full_name, full_namelen)
  
  END SUBROUTINE h5_fixname_f

!----------------------------------------------------------------------
! Name:		h5_cleanup_f
!
! Purpose:	Cleanups tests files
!               It is a fortran counterpart for the h5_cleanup in ../../test/h5test.c
!
! Inputs:
!		base_name	- base name of the file
!               fapl		- file access property list
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! Programmer:	Elena Pourmal
!		September 19, 2002
!
!
!----------------------------------------------------------------------
  SUBROUTINE h5_cleanup_f(base_name, fapl, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_cleanup_f
!DEC$endif
    USE H5GLOBAL
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: base_name   ! base name
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    INTEGER(HID_T), INTENT(IN) :: fapl ! file access property list
    
    INTEGER(SIZE_T) :: base_namelen ! Length of the base name character string

    INTERFACE
       INTEGER FUNCTION h5_cleanup_c(base_name, base_namelen, fapl)
         USE H5GLOBAL
         !DEC$ IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5_CLEANUP_C':: h5_cleanup_c
         !DEC$ ENDIF
         !DEC$ATTRIBUTES reference :: base_name
         CHARACTER(LEN=*), INTENT(IN) :: base_name
         INTEGER(SIZE_T) :: base_namelen
         INTEGER(HID_T), INTENT(IN) :: fapl
       END FUNCTION h5_cleanup_c
    END INTERFACE
    
    base_namelen = LEN(base_name)
    hdferr = h5_cleanup_c(base_name, base_namelen, fapl)
    
  END SUBROUTINE h5_cleanup_f

!----------------------------------------------------------------------
! Name:		h5_exit_f
!
! Purpose:	Exit application
!               It is a fortran counterpart for the standard C 'exit()' routine
! 		Be careful not to overflow the exit value range since
! 		UNIX supports a very small range such as 1 byte.
!		Therefore, exit(256) may end up as exit(0).
!
! Inputs:
!		status	- Status to return from application
!
! Outputs:
!		none
!
! Programmer:	Quincey Koziol
!		December 14, 2004
!
!
!----------------------------------------------------------------------
  SUBROUTINE h5_exit_f(status)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_exit_f
!DEC$endif
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: status         ! Return code

    INTERFACE
       SUBROUTINE h5_exit_c(status)
         !DEC$ IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5_EXIT_C':: h5_exit_c
         !DEC$ ENDIF
         INTEGER, INTENT(IN) :: status
       END SUBROUTINE h5_exit_c
    END INTERFACE
    
    CALL h5_exit_c(status)

  END SUBROUTINE h5_exit_f

!----------------------------------------------------------------------
! Name:		h5_env_nocleanup_f
!
! Purpose:	Uses the HDF5_NOCLEANUP environment variable in Fortran
!               tests to determine if the output files should be removed
!
! Inputs:
!
! Outputs:      HDF5_NOCLEANUP:  .true. - don't remove test files
!		                .false. - remove test files
!
! Programmer:	M.S. Breitenfeld
!               September 30, 2008
!
!----------------------------------------------------------------------
  SUBROUTINE h5_env_nocleanup_f(HDF5_NOCLEANUP)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_env_nocleanup_f
!DEC$endif
    IMPLICIT NONE
    LOGICAL, INTENT(OUT) :: HDF5_NOCLEANUP ! Return code
    INTEGER :: status
    
    INTERFACE
       SUBROUTINE h5_env_nocleanup_c(status)
         !DEC$ IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ ATTRIBUTES C,reference,decorate,alias:'H5_ENV_NOCLEANUP_C':: h5_env_nocleanup_c
         !DEC$ ENDIF
         INTEGER :: status
       END SUBROUTINE h5_env_nocleanup_c
    END INTERFACE
    
    CALL h5_env_nocleanup_c(status)
    
    HDF5_NOCLEANUP = .FALSE.
    IF(status.EQ.1) HDF5_NOCLEANUP = .TRUE.
    
  END SUBROUTINE h5_env_nocleanup_f

! ---------------------------------------------------------------------------------------------------
! H5_SIZEOF routines
!
! NOTES
!   (1) The Sun/Oracle compiler has the following restrictions on the SIZEOF intrinsic function:
!
!     "The SIZEOF intrinsic cannot be applied to arrays of an assumed size, characters of a 
!      length that is passed, or subroutine calls or names. SIZEOF returns default INTEGER*4 data. 
!      If compiling for a 64-bit environment, the compiler will issue a warning if the result overflows 
!      the INTEGER*4 data range. To use SIZEOF in a 64-bit environment with arrays larger 
!      than the INTEGER*4 limit (2 Gbytes), the SIZEOF function and 
!      the variables receiving the result must be declared INTEGER*8."
!
!    Thus, we can not overload the H5_SIZEOF function to handle arrays (as used in tH5P_F03.f90), or
!    characters that do not have a set length (as used in tH5P_F03.f90), sigh...
!
!   (2) F08+TS29113 requires C interoperable variable as argument for C_SIZEOF.
!
!   (3) Unfortunately we need to wrap the C_SIZEOF/STORAGE_SIZE functions to handle different
!       data types from the various tests.
!
! ---------------------------------------------------------------------------------------------------

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_cmpd
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_CMPD(a)
    IMPLICIT NONE
    TYPE(comp_datatype), INTENT(in) :: a

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    H5_SIZEOF_CMPD = storage_size(a, c_size_t)/storage_size(c_char_'a',c_size_t)
#else
    H5_SIZEOF_CMPD = SIZEOF(a)
#endif

  END FUNCTION H5_SIZEOF_CMPD

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_chr
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_CHR(a)
    IMPLICIT NONE
    CHARACTER(LEN=1), INTENT(in) :: a

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    H5_SIZEOF_CHR = storage_size(a, c_size_t)/storage_size(c_char_'a',c_size_t)
#else
    H5_SIZEOF_CHR = SIZEOF(a)
#endif

  END FUNCTION H5_SIZEOF_CHR

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_i
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_I(a)
    IMPLICIT NONE
    INTEGER, INTENT(in):: a

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    H5_SIZEOF_I = storage_size(a, c_size_t)/storage_size(c_char_'a',c_size_t)
#else
    H5_SIZEOF_I = SIZEOF(a)
#endif

  END FUNCTION H5_SIZEOF_I


!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_sp
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_SP(a)
    IMPLICIT NONE
    REAL(sp), INTENT(in):: a

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    H5_SIZEOF_SP = storage_size(a, c_size_t)/storage_size(c_char_'a',c_size_t)
#else
    H5_SIZEOF_SP = SIZEOF(a)
#endif

  END FUNCTION H5_SIZEOF_SP

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_dp
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_DP(a)
    IMPLICIT NONE
    REAL(dp), INTENT(in):: a

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    H5_SIZEOF_DP = storage_size(a, c_size_t)/storage_size(c_char_'a',c_size_t)
#else
    H5_SIZEOF_DP = SIZEOF(a)
#endif

  END FUNCTION H5_SIZEOF_DP

END MODULE TH5_MISC
