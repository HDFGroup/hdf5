!****h* ROBODoc/TH5_MISC_gen.F90
!
! NAME
!  TH5_MISC_gen
! 
! PURPOSE
!  This module is generated at build by H5_test_buildiface.F90 to handle checking 
!  in the tests all the detected KINDs.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the LICENSE file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! AUTHOR
!  H5_test_buildiface.F90
!
!*****

MODULE TH5_MISC_gen
  USE, INTRINSIC :: ISO_C_BINDING
  INTERFACE verify
     MODULE PROCEDURE verify_real_kind_4
     MODULE PROCEDURE verify_real_kind_8
     MODULE PROCEDURE verify_real_kind_10
     MODULE PROCEDURE verify_real_kind_16
     MODULE PROCEDURE verify_integer_kind_1
     MODULE PROCEDURE verify_integer_kind_2
     MODULE PROCEDURE verify_integer_kind_4
     MODULE PROCEDURE verify_integer_kind_8
     MODULE PROCEDURE verify_integer_kind_16
     MODULE PROCEDURE verify_character
     MODULE PROCEDURE verify_logical
     MODULE PROCEDURE verify_c_bool
  END INTERFACE
  INTERFACE check_real_eq
     MODULE PROCEDURE real_eq_kind_4
     MODULE PROCEDURE real_eq_kind_8
     MODULE PROCEDURE real_eq_kind_10
     MODULE PROCEDURE real_eq_kind_16
  END INTERFACE
CONTAINS
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_integer_kind_1
!DEC$endif
  SUBROUTINE verify_integer_kind_1(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: string
    INTEGER(KIND=1) :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (value .NE. correct_value) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT INTEGER VALIDATION ", string
      ENDIF
    ELSE
      IF (value .EQ. correct_value) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT INTEGER VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_integer_kind_1
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_integer_kind_2
!DEC$endif
  SUBROUTINE verify_integer_kind_2(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: string
    INTEGER(KIND=2) :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (value .NE. correct_value) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT INTEGER VALIDATION ", string
      ENDIF
    ELSE
      IF (value .EQ. correct_value) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT INTEGER VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_integer_kind_2
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_integer_kind_4
!DEC$endif
  SUBROUTINE verify_integer_kind_4(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: string
    INTEGER(KIND=4) :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (value .NE. correct_value) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT INTEGER VALIDATION ", string
      ENDIF
    ELSE
      IF (value .EQ. correct_value) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT INTEGER VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_integer_kind_4
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_integer_kind_8
!DEC$endif
  SUBROUTINE verify_integer_kind_8(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: string
    INTEGER(KIND=8) :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (value .NE. correct_value) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT INTEGER VALIDATION ", string
      ENDIF
    ELSE
      IF (value .EQ. correct_value) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT INTEGER VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_integer_kind_8
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_integer_kind_16
!DEC$endif
  SUBROUTINE verify_integer_kind_16(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: string
    INTEGER(KIND=16) :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (value .NE. correct_value) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT INTEGER VALIDATION ", string
      ENDIF
    ELSE
      IF (value .EQ. correct_value) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT INTEGER VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_integer_kind_16
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_real_kind_4
!DEC$endif
  SUBROUTINE verify_real_kind_4(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: string
    REAL(KIND=4) :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (.NOT.real_eq_kind_4( value, correct_value) ) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT REAL VALIDATION ", string
      ENDIF
    ELSE
      IF (real_eq_kind_4( value, correct_value) ) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT REAL VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_real_kind_4
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: real_eq_kind_4
!DEC$endif
  LOGICAL FUNCTION real_eq_kind_4(a,b,ulp)
    IMPLICIT NONE
    REAL(KIND=4), INTENT (in):: a,b
    REAL(KIND=4) :: Rel
    INTEGER,        OPTIONAL, INTENT( IN )  :: ulp
    IF ( PRESENT( ulp ) )  Rel = REAL( ABS(ulp), 4)
    Rel = 1.0_4
    real_eq_kind_4 = ABS( a - b ) < ( Rel * SPACING( MAX(ABS(a),ABS(b)) ) )
  END FUNCTION real_eq_kind_4
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_real_kind_8
!DEC$endif
  SUBROUTINE verify_real_kind_8(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: string
    REAL(KIND=8) :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (.NOT.real_eq_kind_8( value, correct_value) ) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT REAL VALIDATION ", string
      ENDIF
    ELSE
      IF (real_eq_kind_8( value, correct_value) ) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT REAL VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_real_kind_8
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: real_eq_kind_8
!DEC$endif
  LOGICAL FUNCTION real_eq_kind_8(a,b,ulp)
    IMPLICIT NONE
    REAL(KIND=8), INTENT (in):: a,b
    REAL(KIND=8) :: Rel
    INTEGER,        OPTIONAL, INTENT( IN )  :: ulp
    IF ( PRESENT( ulp ) )  Rel = REAL( ABS(ulp), 8)
    Rel = 1.0_8
    real_eq_kind_8 = ABS( a - b ) < ( Rel * SPACING( MAX(ABS(a),ABS(b)) ) )
  END FUNCTION real_eq_kind_8
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_real_kind_10
!DEC$endif
  SUBROUTINE verify_real_kind_10(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: string
    REAL(KIND=10) :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (.NOT.real_eq_kind_10( value, correct_value) ) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT REAL VALIDATION ", string
      ENDIF
    ELSE
      IF (real_eq_kind_10( value, correct_value) ) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT REAL VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_real_kind_10
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: real_eq_kind_10
!DEC$endif
  LOGICAL FUNCTION real_eq_kind_10(a,b,ulp)
    IMPLICIT NONE
    REAL(KIND=10), INTENT (in):: a,b
    REAL(KIND=10) :: Rel
    INTEGER,        OPTIONAL, INTENT( IN )  :: ulp
    IF ( PRESENT( ulp ) )  Rel = REAL( ABS(ulp), 10)
    Rel = 1.0_10
    real_eq_kind_10 = ABS( a - b ) < ( Rel * SPACING( MAX(ABS(a),ABS(b)) ) )
  END FUNCTION real_eq_kind_10
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_real_kind_16
!DEC$endif
  SUBROUTINE verify_real_kind_16(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: string
    REAL(KIND=16) :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (.NOT.real_eq_kind_16( value, correct_value) ) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT REAL VALIDATION ", string
      ENDIF
    ELSE
      IF (real_eq_kind_16( value, correct_value) ) THEN
         total_error=total_error+1
         WRITE(*,*) "ERROR: INCORRECT REAL VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_real_kind_16
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: real_eq_kind_16
!DEC$endif
  LOGICAL FUNCTION real_eq_kind_16(a,b,ulp)
    IMPLICIT NONE
    REAL(KIND=16), INTENT (in):: a,b
    REAL(KIND=16) :: Rel
    INTEGER,        OPTIONAL, INTENT( IN )  :: ulp
    IF ( PRESENT( ulp ) )  Rel = REAL( ABS(ulp), 16)
    Rel = 1.0_16
    real_eq_kind_16 = ABS( a - b ) < ( Rel * SPACING( MAX(ABS(a),ABS(b)) ) )
  END FUNCTION real_eq_kind_16
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_character
!DEC$endif
  SUBROUTINE verify_character(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER*(*) :: string
    CHARACTER*(*) :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (TRIM(value) .NE. TRIM(correct_value)) THEN
         total_error = total_error + 1
         WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string
      ENDIF
    ELSE
      IF (TRIM(value) .EQ. TRIM(correct_value)) THEN
         total_error = total_error + 1
         WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_character
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_logical
!DEC$endif
  SUBROUTINE verify_logical(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: string
    LOGICAL :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (value .NEQV. correct_value) THEN
         total_error = total_error + 1
         WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string
      ENDIF
    ELSE
      IF (value .EQV. correct_value) THEN
         total_error = total_error + 1
         WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_logical
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: verify_c_bool
!DEC$endif
  SUBROUTINE verify_c_bool(string,value,correct_value,total_error,chck_eq)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: string
    LOGICAL(C_BOOL) :: value, correct_value
    INTEGER :: total_error
    LOGICAL, OPTIONAL :: chck_eq
    LOGICAL :: chck_eq_opt
    chck_eq_opt = .TRUE.
    IF(PRESENT(chck_eq)) chck_eq_opt = chck_eq
    IF(chck_eq_opt .EQV. .TRUE.)THEN
      IF (value .NEQV. correct_value) THEN
         total_error = total_error + 1
         WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string
      ENDIF
    ELSE
      IF (value .EQV. correct_value) THEN
         total_error = total_error + 1
         WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string
      ENDIF
    ENDIF
  END SUBROUTINE verify_c_bool
END MODULE TH5_MISC_gen
