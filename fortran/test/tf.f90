!****h* root/fortran/test/tf.f90
!
! NAME
!  tf.f90
!
! FUNCTION
!  Contains subroutines which are needed in all the hdf5 fortran tests
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  Copyright by The HDF Group.                                                 *
!  Copyright by the Board of Trustees of the University of Illinois.           *
!  All rights reserved.                                                        *
!                                                                              *
!  This file is part of HDF5.  The full HDF5 copyright notice, including       *
!  terms governing use, modification, and redistribution, is contained in      *
!  the COPYING file, which can be found at the root of the source code         *
!  distribution tree, or in https://www.hdfgroup.org/licenses.                 *
!  If you do not have access to either file, you may request a copy from       *
!  help@hdfgroup.org.                                                          *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! CONTAINS SUBROUTINES
!  write_test_status, check, verify, verifyLogical, verifyString, h5_fixname_f,
!  h5_cleanup_f, h5_exit_f, h5_env_nocleanup_f,dreal_eqv
!
!*****
MODULE TH5_MISC

  USE TH5_MISC_PROVISIONAL

  IMPLICIT NONE

CONTAINS

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: dreal_eq
!DEC$endif
  LOGICAL FUNCTION dreal_eq(a,b)

    ! Check if two double precision reals are equivalent
    REAL(dp), INTENT (in):: a,b
    REAL(dp), PARAMETER :: eps = 1.e-8
    dreal_eq = ABS(a-b) .LT. eps

  END FUNCTION dreal_eq

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_real_kind_7
!DEC$endif
  SUBROUTINE verify_real_kind_7(string,value,correct_value,total_error)
    USE HDF5
    INTEGER, PARAMETER :: real_kind_7 = SELECTED_REAL_KIND(Fortran_REAL_4) !should map to REAL*4 on most modern processors
    CHARACTER(LEN=*) :: string
    REAL(real_kind_7) :: value, correct_value
    INTEGER :: total_error
    IF (.NOT.dreal_eq( REAL(value,dp), REAL(correct_value, dp)) ) THEN
       total_error=total_error+1
       WRITE(*,*) "ERROR: INCORRECT REAL VALIDATION ", string
    ENDIF
    RETURN
  END SUBROUTINE verify_real_kind_7

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

    WRITE(*, fmt = '(A, T72, A)') test_title, error_string

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

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify
!DEC$endif
  SUBROUTINE VERIFY(string,value,correct_value,total_error)
    CHARACTER(LEN=*) :: string
    INTEGER :: value, correct_value, total_error
    IF (value .NE. correct_value) THEN
       total_error=total_error+1
       WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string
    ENDIF
    RETURN
  END SUBROUTINE verify

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_INTEGER_HID_T
!DEC$endif
  SUBROUTINE verify_INTEGER_HID_T(string,value,correct_value,total_error)
    USE HDF5	
    CHARACTER(LEN=*) :: string
    INTEGER(HID_T) :: value, correct_value
    INTEGER :: total_error
    IF (value .NE. correct_value) THEN
       total_error=total_error+1
       WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string
    ENDIF
    RETURN
  END SUBROUTINE verify_INTEGER_HID_T

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_Fortran_INTEGER_4
!DEC$endif
  SUBROUTINE verify_Fortran_INTEGER_4(string,value,correct_value,total_error)
    USE HDF5
    INTEGER, PARAMETER :: int_kind_8 = SELECTED_INT_KIND(Fortran_INTEGER_4)  ! should map to INTEGER*4 on most modern processors	
    CHARACTER(LEN=*) :: string
    INTEGER(int_kind_8) :: value, correct_value
    INTEGER :: total_error
    IF (value .NE. correct_value) THEN
       total_error=total_error+1
       WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string
    ENDIF
    RETURN
  END SUBROUTINE verify_Fortran_INTEGER_4

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verifyLogical
!DEC$endif
  SUBROUTINE verifyLogical(string,value,correct_value,total_error)
    CHARACTER(LEN=*) :: string
    LOGICAL :: value, correct_value
    INTEGER :: total_error
    IF (value .NEQV. correct_value) THEN
       total_error = total_error + 1
       WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string
    ENDIF
    RETURN
  END SUBROUTINE verifyLogical
  
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verifyString
!DEC$endif
  SUBROUTINE verifyString(string, value,correct_value,total_error)
    CHARACTER*(*) :: string
    CHARACTER*(*) :: value, correct_value
    INTEGER :: total_error
    IF (TRIM(value) .NE. TRIM(correct_value)) THEN
       total_error = total_error + 1
       WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string
    ENDIF
    RETURN
  END SUBROUTINE verifyString


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
END MODULE TH5_MISC
