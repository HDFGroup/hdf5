!****p* Program/H5_test_buildiface
!
! NAME
!  Executable: H5_test_buildiface
!
! FILE
!  fortran/src/H5_test_buildiface.F90
!
! PURPOSE
!  This stand alone program is used at build time to generate the program
!  H5fortran_detect.f90. It cycles through all the available KIND parameters for
!  integers and reals. The appropriate program and subroutines are then generated
!  depending on which of the KIND values are found.
!
! NOTES
!  This program uses the Fortran 2008 intrinsic function STORAGE_SIZE or SIZEOF 
!  depending on availablity.It generates code that makes use of 
!  STORAGE_SIZE/SIZEOF in H5fortran_detect.f90. STORAGE_SIZE is standard
!  compliant and should always be chosen over SIZEOF.
!
!  The availability of STORAGE_SIZE/SIZEOF is checked at configure time and the TRUE/FALSE
!  condition is set in the configure variable "FORTRAN_HAVE_STORAGE_SIZE" or
!  "FORTRAN_HAVE_SIZEOF".
!
!  The use of C_SIZOF(X) is not used since the argument X must be an interoperable
!  data entity.
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
!  distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.    *
!  If you do not have access to either file, you may request a copy from       *
!  help@hdfgroup.org.                                                          *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! AUTHOR
!  M. Scot Breitenfeld
!
!*****

#include <H5config_f.inc>

PROGRAM H5_test_buildiface
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

! These values are valid REAL KINDs (with corresponding C float) found during configure
  H5_H5CONFIG_F_NUM_RKIND
  H5_H5CONFIG_F_RKIND
! These values are valid INTEGER KINDs (with corresponding C integer) found during configure
  H5_H5CONFIG_F_NUM_IKIND
  H5_H5CONFIG_F_IKIND

  INTEGER :: i, j, k
  CHARACTER(LEN=2) :: chr2

! Generate Fortran Check routines for the tests KIND interfaces.

  OPEN(11,FILE='tf_gen.F90')
  WRITE(11,'(40(A,/))') &
'!****h* ROBODoc/TH5_MISC_gen.F90',&
'!',&
'! NAME',&
'!  TH5_MISC_gen',&
'! ',&
'! PURPOSE',&
'!  This module is generated at build by H5_test_buildiface.F90 to handle checking ',&
'!  in the tests all the detected KINDs.',&
'!',&
'! COPYRIGHT',&
'! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *',&
'!   Copyright by The HDF Group.                                               *',&
'!   All rights reserved.                                                      *',&
'!                                                                             *',&
'!   This file is part of HDF5.  The full HDF5 copyright notice, including     *',&
'!   terms governing use, modification, and redistribution, is contained in    *',&
'!   the COPYING file, which can be found at the root of the source code       *',&
'!   distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *',&
'!   If you do not have access to either file, you may request a copy from     *',&
'!   help@hdfgroup.org.                                                        *',&
'! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *',&
'!',&
'! AUTHOR',&
'!  H5_test_buildiface.F90',&
'!',&
'!*****'

  WRITE(11,'(a)') "MODULE TH5_MISC_gen"

  WRITE(11,'(A)') '  USE, INTRINSIC :: ISO_C_BINDING'
  
! Interfaces for validating REALs, INTEGERs, CHARACTERs, LOGICALs

  WRITE(11,'(A)') '  INTERFACE verify'
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(A)') "     MODULE PROCEDURE verify_real_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(A)') "     MODULE PROCEDURE verify_integer_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  WRITE(11,'(A)') "     MODULE PROCEDURE verify_character"
  WRITE(11,'(A)') "     MODULE PROCEDURE verify_logical"
  WRITE(11,'(A)') "  END INTERFACE"

  WRITE(11,'(A)') '  INTERFACE check_real_eq'
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(A)') "     MODULE PROCEDURE real_eq_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

  WRITE(11,'(A)') 'CONTAINS'

! ***************************
! VALIDATE INTEGERS
! ***************************
  DO i = 1, num_ikinds
     k = ikind(i)
     WRITE(chr2,'(I2)') k
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_TEST_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: verify_integer_kind_'//TRIM(ADJUSTL(chr2))
     WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE verify_integer_kind_'//TRIM(ADJUSTL(chr2))//'(string,value,correct_value,total_error)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    CHARACTER(LEN=*) :: string'
     WRITE(11,'(A)') '    INTEGER(KIND='//TRIM(ADJUSTL(chr2))//') :: value, correct_value'
     WRITE(11,'(A)') '    INTEGER :: total_error'
     WRITE(11,'(A)') '    IF (value .NE. correct_value) THEN'
     WRITE(11,'(A)') '       total_error=total_error+1'
     WRITE(11,'(A)') '       WRITE(*,*) "ERROR: INCORRECT INTEGER VALIDATION ", string'
     WRITE(11,'(A)') '    ENDIF'
     WRITE(11,'(A)') '  END SUBROUTINE verify_integer_kind_'//TRIM(ADJUSTL(chr2))
 ENDDO

! ***************************
! VALIDATE REALS
! ***************************
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_TEST_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: verify_real_kind_'//TRIM(ADJUSTL(chr2))
     WRITE(11,'(A)') '!DEC$endif'
    
! Subroutine API 
     WRITE(11,'(A)') '  SUBROUTINE verify_real_kind_'//TRIM(ADJUSTL(chr2))//'(string,value,correct_value,total_error)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    CHARACTER(LEN=*) :: string'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//') :: value, correct_value'
     WRITE(11,'(A)') '    INTEGER :: total_error'
     WRITE(11,'(A)') '    IF (.NOT.real_eq_kind_'//TRIM(ADJUSTL(chr2))//'( value, correct_value) ) THEN'
     WRITE(11,'(A)') '       total_error=total_error+1'
     WRITE(11,'(A)') '       WRITE(*,*) "ERROR: INCORRECT REAL VALIDATION ", string'
     WRITE(11,'(A)') '    ENDIF'
     WRITE(11,'(A)') '  END SUBROUTINE verify_real_kind_'//TRIM(ADJUSTL(chr2))


! ***********************************
! TEST IF TWO REAL NUMBERS ARE EQUAL
! ***********************************

!   [1] The test performed is
!
!         ABS( x - y ) < ( ULP * SPACING( MAX(ABS(x),ABS(y)) ) )
!
!       The numbers are considered equal if true
!
!       The intrinsic function SPACING(x) returns the absolute spacing of numbers
!       near the value of x,
!
!                      {     EXPONENT(x)-DIGITS(x)
!                      {  2.0                        for x /= 0
!         SPACING(x) = {
!                      {  
!                      {  TINY(x)                    for x == 0
!
!       The ULP optional argument scales the comparison:
!
!         Unit of data precision. The acronym stands for "unit in
!         the last place," the smallest possible increment or decrement
!         that can be made using a machine's floating point arithmetic.
!         A 0.5 ulp maximum error is the best you could hope for, since
!         this corresponds to always rounding to the nearest representable
!         floating-point number. Value must be positive - if a negative
!         value is supplied, the absolute value is used.
!         If not specified, the default value is 1.
!
!       James Van Buskirk and James Giles suggested this method for floating
!       point comparisons in the comp.lang.fortran newsgroup.
!
!       Reference: [1] Paul van Delst,  paul.vandelst@ssec.wisc.edu

     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_TEST_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: real_eq_kind_'//TRIM(ADJUSTL(chr2))
     WRITE(11,'(A)') '!DEC$endif'
     WRITE(11,'(A)') '  LOGICAL FUNCTION real_eq_kind_'//TRIM(ADJUSTL(chr2))//'(a,b,ulp)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'), INTENT (in):: a,b'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//') :: Rel'
     WRITE(11,'(A)') '    INTEGER,        OPTIONAL, INTENT( IN )  :: ulp'
     WRITE(11,'(A)') '    IF ( PRESENT( ulp ) )  Rel = REAL( ABS(ulp), '//TRIM(ADJUSTL(chr2))//')'
     WRITE(11,'(A)') '    Rel = 1.0_'//TRIM(ADJUSTL(chr2))
     WRITE(11,'(A)') '    real_eq_kind_'//TRIM(ADJUSTL(chr2))//' = ABS( a - b ) < ( Rel * SPACING( MAX(ABS(a),ABS(b)) ) )'
     WRITE(11,'(A)') '  END FUNCTION real_eq_kind_'//TRIM(ADJUSTL(chr2))
  ENDDO

! ***************************
! VALIDATE CHARACTER STRINGS
! ***************************

! DLL definitions for windows
  WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_TEST_DLL)'
  WRITE(11,'(A)') '!DEC$attributes dllexport :: verify_character'
  WRITE(11,'(A)') '!DEC$endif'

! Subroutine API 
  WRITE(11,'(A)') '  SUBROUTINE verify_character(string,value,correct_value,total_error)'
  WRITE(11,'(A)') '    IMPLICIT NONE'
  WRITE(11,'(A)') '    CHARACTER*(*) :: string'
  WRITE(11,'(A)') '    CHARACTER*(*) :: value, correct_value'
  WRITE(11,'(A)') '    INTEGER :: total_error'
  WRITE(11,'(A)') '    IF (TRIM(value) .NE. TRIM(correct_value)) THEN'
  WRITE(11,'(A)') '       total_error = total_error + 1'
  WRITE(11,'(A)') '       WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string'
  WRITE(11,'(A)') '    ENDIF'
  WRITE(11,'(A)') '  END SUBROUTINE verify_character'

! ***************************
! VALIDATE LOGICAL
! ***************************

! DLL definitions for windows
  WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_TEST_DLL)'
  WRITE(11,'(A)') '!DEC$attributes dllexport :: verify_logical'
  WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
  WRITE(11,'(A)') '  SUBROUTINE verify_logical(string,value,correct_value,total_error)'
  WRITE(11,'(A)') '    IMPLICIT NONE'
  WRITE(11,'(A)') '    CHARACTER(LEN=*) :: string'
  WRITE(11,'(A)') '    LOGICAL :: value, correct_value'
  WRITE(11,'(A)') '    INTEGER :: total_error'
  WRITE(11,'(A)') '    IF (value .NEQV. correct_value) THEN'
  WRITE(11,'(A)') '       total_error = total_error + 1'
  WRITE(11,'(A)') '       WRITE(*,*) "ERROR: INCORRECT VALIDATION ", string'
  WRITE(11,'(A)') '    ENDIF'
  
  WRITE(11,'(A)') '  END SUBROUTINE verify_logical'

  WRITE(11,'(A)') "END MODULE TH5_MISC_gen"

  CLOSE(11)

END PROGRAM H5_test_buildiface



