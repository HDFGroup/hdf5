!****h* ROBODoc/H5P (F03)
!
! NAME
!  H5P_PROVISIONAL
!
! PURPOSE
!  This file contains Fortran 90 and Fortran 2003 interfaces for H5P functions.
!  It contains the same functions as H5Pff_DEPRECIATE.f90 but includes the
!  Fortran 2003 functions and the interface listings. This file will be compiled
!  instead of H5Pff_DEPRECIATE.f90 if Fortran 2003 functions are enabled.
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  Copyright by The HDF Group.                                               *
!  Copyright by the Board of Trustees of the University of Illinois.         *
!  All rights reserved.                                                      *
!  *
!  This file is part of HDF5.  The full HDF5 copyright notice, including     *
!  terms governing use, modification, and redistribution, is contained in    *
!  the files COPYING and Copyright.html.  COPYING can be found at the root   *
!  of the source code distribution tree; Copyright.html can be found at the  *
!  root level of an installed copy of the electronic HDF5 document set and   *
!  is linked from the top-level documents page.  It can also be found at     *
!  http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!  access to either file, you may request a copy from help@hdfgroup.org.     *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES
!                         *** IMPORTANT ***
!  If you add a new H5P function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5P_PROVISIONAL

  USE H5GLOBAL

  INTERFACE h5pset_fill_value_f
     MODULE PROCEDURE h5pset_fill_value_integer
     MODULE PROCEDURE h5pset_fill_value_real
     MODULE PROCEDURE h5pset_fill_value_char

     ! Recommended procedure:
     MODULE PROCEDURE h5pset_fill_value_ptr

  END INTERFACE

  INTERFACE h5pget_fill_value_f
     MODULE PROCEDURE h5pget_fill_value_integer
     MODULE PROCEDURE h5pget_fill_value_real
     MODULE PROCEDURE h5pget_fill_value_char

     ! Recommended procedure:
     MODULE PROCEDURE h5pget_fill_value_ptr

  END INTERFACE

  INTERFACE h5pset_f
     MODULE PROCEDURE h5pset_integer
     MODULE PROCEDURE h5pset_real
     MODULE PROCEDURE h5pset_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pset_ptr

  END INTERFACE

  INTERFACE h5pget_f
     MODULE PROCEDURE h5pget_integer
     MODULE PROCEDURE h5pget_real
     MODULE PROCEDURE h5pget_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pget_ptr
  END INTERFACE

  INTERFACE h5pregister_f
     MODULE PROCEDURE h5pregister_integer
     MODULE PROCEDURE h5pregister_real
     MODULE PROCEDURE h5pregister_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pregister_ptr
  END INTERFACE

  INTERFACE h5pinsert_f
     MODULE PROCEDURE h5pinsert_integer
     MODULE PROCEDURE h5pinsert_real
     MODULE PROCEDURE h5pinsert_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pinsert_ptr
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pget_fill_value_c(prp_id, type_id, fillvalue)
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FILL_VALUE_C'::h5pget_fill_value_c
       !DEC$ENDIF
       INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
       INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                             ! of fillvalue datatype
                                             ! (in memory)
       TYPE(C_PTR), VALUE :: fillvalue       ! Fillvalue
     END FUNCTION h5pget_fill_value_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pset_fill_value_c(prp_id, type_id, fillvalue)
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FILL_VALUE_C'::h5pset_fill_value_c
       !DEC$ENDIF
       INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
       INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                             ! of fillvalue datatype
                                             ! (in memory)
       TYPE(C_PTR), VALUE :: fillvalue       ! Fillvalue
     END FUNCTION h5pset_fill_value_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pset_c(prp_id, name, name_len, value)
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_C'::h5pset_c
       !DEC$ENDIF
       INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
       CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
       INTEGER :: name_len
       TYPE(C_PTR), VALUE :: value ! Property value
     END FUNCTION h5pset_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pget_c(prp_id, name, name_len, value)
       USE ISO_C_BINDING
       USE H5GLOBAL
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_C'::h5pget_c
       !DEC$ENDIF
       INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
       CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
       INTEGER :: name_len
       TYPE(C_PTR), VALUE :: value ! Property value
     END FUNCTION h5pget_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pregister_c(class, name, name_len, size, value)
       USE iso_c_binding
       USE H5GLOBAL
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PREGISTER_C'::h5pregister_c
       !DEC$ENDIF
       !DEC$ATTRIBUTES reference :: name
       INTEGER(HID_T), INTENT(IN) :: class
       CHARACTER(LEN=*), INTENT(IN) :: name
       INTEGER, INTENT(IN)         :: name_len
       INTEGER(SIZE_T), INTENT(IN) :: size
       TYPE(C_PTR), INTENT(IN), VALUE :: value
     END FUNCTION h5pregister_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pinsert_c(plist, name, name_len, size, value)
       USE iso_c_binding
       USE H5GLOBAL
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PINSERT_C'::h5pinsert_c
       !DEC$ENDIF
       !DEC$ATTRIBUTES reference :: name
       INTEGER(HID_T), INTENT(IN) :: plist
       CHARACTER(LEN=*), INTENT(IN) :: name
       INTEGER, INTENT(IN)         :: name_len
       INTEGER(SIZE_T), INTENT(IN) :: size
       TYPE(c_ptr), INTENT(IN), value :: value
     END FUNCTION h5pinsert_c
  END INTERFACE

CONTAINS

!
!****s* H5P (F03)/h5pset(get)fill_value_f
!
! NAME
!  h5pset(get)fill_value_f
!
! PURPOSE
!  Sets(gets) fill value for a dataset creation property list
!
! INPUTS
!  prp_id 	 - dataset creation property list identifier
!  type_id 	 - datatype identifier for fill value
!  fillvalue 	 - fill value
! OUTPUTS
!  type_id       - datatype identifier for fill value
!  fillvalue	 - fill value
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 14, 2001
!
!  Added the recommended way of passing fillvalue
!  and that is by passing the C address, all other
!  ways are obsolete and should be avoided. June, 2008 MSB
!
! NOTES
!  h5pset(get)fill_value_f function is overloaded to support
!  INTEGER, REAL, DOUBLE PRECISION and CHARACTER dtatypes.
!
! SOURCE
  SUBROUTINE h5pset_fill_value_integer(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    INTEGER, INTENT(IN), TARGET :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
!*****
    TYPE(C_PTR) :: f_ptr ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pset_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pset_fill_value_integer

  SUBROUTINE h5pget_fill_value_integer(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    INTEGER, INTENT(OUT), TARGET :: fillvalue ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr            ! Error code
    TYPE(C_PTR) :: f_ptr                      ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pget_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pget_fill_value_integer


  SUBROUTINE h5pset_fill_value_real(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    REAL, INTENT(IN), TARGET :: fillvalue ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    TYPE(C_PTR) :: f_ptr                  ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pset_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pset_fill_value_real


  SUBROUTINE h5pget_fill_value_real(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    REAL, INTENT(OUT), TARGET :: fillvalue  ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr                    ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pget_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pget_fill_value_real


  SUBROUTINE h5pset_fill_value_double(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    DOUBLE PRECISION, INTENT(IN), TARGET :: fillvalue ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr ! Error code
    TYPE(C_PTR) :: f_ptr           ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pset_fill_value_c(prp_id, type_id, f_ptr)
  END SUBROUTINE h5pset_fill_value_double


  SUBROUTINE h5pget_fill_value_double(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    DOUBLE PRECISION, INTENT(OUT), TARGET :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    TYPE(C_PTR) :: f_ptr                  ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pget_fill_value_c(prp_id, type_id, f_ptr)
  END SUBROUTINE h5pget_fill_value_double

  SUBROUTINE h5pset_fill_value_char(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    CHARACTER, INTENT(IN), TARGET :: fillvalue ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
    TYPE(C_PTR) :: f_ptr                       ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pset_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pset_fill_value_char

  SUBROUTINE h5pget_fill_value_char(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    CHARACTER, INTENT(OUT) :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr        ! Error code

    INTEGER :: i
    CHARACTER(LEN=1), ALLOCATABLE, DIMENSION(:), TARGET :: chr
    INTEGER :: chr_len

    TYPE(C_PTR) :: f_ptr ! C address
    ! To resolve Issue #1 outlined in the preamble of this file we
    ! need to pack the character string into an array.

    chr_len = LEN(fillvalue)
    ALLOCATE(chr(1:chr_len), STAT=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF

    f_ptr = C_LOC(chr)

    hdferr = h5pget_fill_value_c(prp_id, type_id, f_ptr)

    DO i = 1, chr_len
       fillvalue(i:i) = chr(i)
    ENDDO
    DEALLOCATE(chr)


  END SUBROUTINE h5pget_fill_value_char

  SUBROUTINE h5pset_fill_value_ptr(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    TYPE(C_PTR), VALUE :: fillvalue       ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr       ! Error code

    hdferr = h5pset_fill_value_c(prp_id, type_id, fillvalue)

  END SUBROUTINE h5pset_fill_value_ptr

  SUBROUTINE h5pget_fill_value_ptr(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    TYPE(C_PTR), INTENT(OUT) :: fillvalue ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr        ! Error code

    hdferr = h5pget_fill_value_c(prp_id, type_id, fillvalue)

  END SUBROUTINE h5pget_fill_value_ptr

!
!****s* H5P (F03)/h5pset_integer
!
! NAME
!  h5pset_integer
!
! PURPOSE
!  Sets a property list value
!
! INPUTS
!  prp_id 	 - iproperty list identifier to modify
!  name 	 - name of property to modify
!  value 	 - value to set property to
! OUTPUTS
!  hdferr:       - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
! SOURCE
  SUBROUTINE h5pset_integer(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id   ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of property to modify
    INTEGER,   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pset_integer

!
!****s* H5P (F03)/h5pset_real
!
! NAME
!  h5pset_real
!
! PURPOSE
!  Sets a property list value
!
! INPUTS
!  prp_id 	 - iproperty list identifier to modify
!  name 	 - name of property to modify
!  value 	 - value to set property to
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                    Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
! SOURCE
  SUBROUTINE h5pset_real(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name ! Name of property to modify
    REAL,   INTENT(IN), TARGET :: value  ! Property value
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
!*****
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pset_real

!
!****s* H5P (F03)/h5pset_double
!
! NAME
!  h5pset_double
!
! PURPOSE
!  Sets a property list value
!
! INPUTS
!  prp_id 	 - iproperty list identifier to modify
!  name 	 - name of property to modify
!  value 	 - value to set property to
! OUTPUTS
!  hdferr:	 - error code
!                    Success:  0
!                    Failure: -1
! AUTHOR
!
!  Elena Pourmal
!  October 9, 2002
! SOURCE
  SUBROUTINE h5pset_double(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    DOUBLE PRECISION,   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr                  ! Error code
!*****
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pset_double

!
!****s* H5P (F03)/h5pset_char
!
! NAME
!  h5pset_char
!
! PURPOSE
!  Sets a property list value
!
! INPUTS
!  prp_id 	 - iproperty list identifier to modify
!  name 	 - name of property to modify
!  value 	 - value to set property to
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! OPTIONAL PARAMETERS
!  NONE
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
! SOURCE
  SUBROUTINE h5pset_char(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id    ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Name of property to modify
    CHARACTER(LEN=*),   INTENT(IN) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    INTEGER :: name_len

    INTEGER :: i
    CHARACTER(LEN=1), ALLOCATABLE, DIMENSION(:), TARGET :: chr
    INTEGER :: chr_len

    TYPE(C_PTR) :: f_ptr
    ! To resolve Issue #1 outlined in the preamble of this file we
    ! need to pack the character string into an array.

    chr_len = LEN(value)
    ALLOCATE(chr(1:chr_len), STAT=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF

    DO i = 1, chr_len
       chr(i) = value(i:i)
    ENDDO

    f_ptr = C_LOC(chr)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

    DEALLOCATE(chr)

  END SUBROUTINE h5pset_char

!
!****s* H5P (F03)/h5pget_integer
!
! NAME
!  h5pget_integer
!
! PURPOSE
!  Gets a property list value
!
! INPUTS
!  prp_id 	 - iproperty list identifier to modify
!  name 	 - name of property to modify
! OUTPUTS
!  value 	 - value of property
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
! SOURCE
  SUBROUTINE h5pget_integer(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id    ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Name of property to modify
    INTEGER,   INTENT(OUT), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pget_integer

!
!****s* H5P (F03)/h5pget_real
!
! NAME
!  h5pget_real
!
! PURPOSE
!  Gets a property list value
!
! INPUTS
!  prp_id 	 - iproperty list identifier to modify
!  name 	 - name of property to modify
! OUTPUTS
!  value 	 - value of property
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
! SOURCE
  SUBROUTINE h5pget_real(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name ! Name of property to modify
    REAL,   INTENT(OUT), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
!*****
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)
  END SUBROUTINE h5pget_real

!
!****s* H5P (F03)/ h5pget_double
!
! NAME
!  h5pget_double
!
! PURPOSE
!  Gets a property list value
!
! INPUTS
!  prp_id 	 - iproperty list identifier to modify
!  name 	 - name of property to modify
! OUTPUTS
!  value 	 - value of property
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
!
! SOURCE
  SUBROUTINE h5pget_double(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    DOUBLE PRECISION,   INTENT(OUT), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pget_double

!
!****s* H5P (F03)/h5pget_char
!
! NAME
!  h5pget_char
!
! PURPOSE
!  Gets a property list value
!
! INPUTS
!  prp_id 	 - iproperty list identifier to modify
!  name 	 - name of property to modify
! OUTPUTS
!  value 	 - value of property
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!
!  Elena Pourmal
!  October 9, 2002
! SOURCE
  SUBROUTINE h5pget_char(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id   ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of property to modify
    CHARACTER(LEN=*), INTENT(OUT) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER :: name_len

    INTEGER :: i
    CHARACTER(LEN=1), ALLOCATABLE, DIMENSION(:), TARGET :: chr
    INTEGER :: chr_len
    TYPE(C_PTR) :: f_ptr

    chr_len = LEN(value)
    ALLOCATE(chr(1:chr_len), STAT=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF
    f_ptr = C_LOC(chr)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

    DO i = 1, chr_len
       value(i:i) = chr(i)
    ENDDO

    DEALLOCATE(chr)

  END SUBROUTINE h5pget_char

  SUBROUTINE h5pset_ptr(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    TYPE(C_PTR), INTENT(OUT) :: value     ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pset_c(prp_id, name, name_len, value)

  END SUBROUTINE h5pset_ptr

  SUBROUTINE h5pget_ptr(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    TYPE(C_PTR), INTENT(OUT) :: value     ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, value)

  END SUBROUTINE h5pget_ptr


!
!****s* H5P (F03)/h5pregister_integer
!
! NAME
!  h5pregister_integer
!
! PURPOSE
!  Registers a permanent property with a property list class.
!
! INPUTS
!  class 	 - property list class to register
!                  permanent property within
!  name 	 - name of property to register
!  size 	 - size of property in bytes
!  value 	 - default value for property in newly
!                  created property lists
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
!
! SOURCE
  SUBROUTINE h5pregister_integer(class, name, size, value, hdferr)
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class    ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size    ! Size of the property value
    INTEGER,   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)

  END SUBROUTINE h5pregister_integer

!
!****s* H5P (F03)/h5pregister_real
!
! NAME
!  h5pregister_real
!
! PURPOSE
!  Registers a permanent property with a property list class.
!
! INPUTS
!  class 	 - property list class to register
!                  permanent property within
!  name 	 - name of property to register
!  size 	 - size of property in bytes
!  value 	 - default value for property in newly
!                  created property lists
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
! SOURCE
  SUBROUTINE h5pregister_real(class, name, size, value, hdferr)
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size   ! size of the property value
    REAL,   INTENT(IN), TARGET :: value   ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)

  END SUBROUTINE h5pregister_real

!
!****s* H5P (F03)/h5pregister_double
!
! NAME
!  h5pregister_double
!
! PURPOSE
!  Registers a permanent property with a property list class.
!
! INPUTS
!  class 	 - property list class to register
!                  permanent property within
!  name 	 - name of property to register
!  size 	 - size of property in bytes
!  value 	 - default value for property in newly
!                  created property lists
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!
!  Elena Pourmal
!  October 10, 2002
! SOURCE
  SUBROUTINE h5pregister_double(class, name, size, value, hdferr)
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class  ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size  ! size of the property value
    DOUBLE PRECISION,   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
!*****
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)
  END SUBROUTINE h5pregister_double

!
!****s* H5P (F03)/h5pregister_char
!
! NAME
!  h5pregister_char
!
! PURPOSE
!  Registers a permanent property with a property list class.
!
! INPUTS
!  class 	 - property list class to register
!                  permanent property within
!  name 	 - name of property to register
!  size 	 - size of property in bytes
!  value 	 - default value for property in newly
!                  created property lists
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
!
! SOURCE
  SUBROUTINE h5pregister_char(class, name, size, value, hdferr)
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class     ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size     ! size of the property value
    CHARACTER(LEN=*),   INTENT(IN) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    INTEGER :: name_len

    INTEGER :: i
    CHARACTER(LEN=1), ALLOCATABLE, DIMENSION(:), TARGET :: chr
    INTEGER :: chr_len

    TYPE(C_PTR) :: f_ptr
    ! To resolve Issue #1 outlined in the preamble of this file we
    ! need to pack the character string into an array.

    chr_len = LEN(value)
    ALLOCATE(chr(1:chr_len), STAT=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF

    DO i = 1, chr_len
       chr(i) = value(i:i)
    ENDDO

    f_ptr = C_LOC(chr)

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)
    DEALLOCATE(chr)
  END SUBROUTINE h5pregister_char

!
!****s* H5P (F03)/h5pregister_ptr
!
! NAME
!  h5pregister_ptr
!
! PURPOSE
!  Registers a permanent property with a property list class.
!
! INPUTS
!  class 	 - property list class to register
!                  permanent property within
!  name 	 - name of property to register
!  size 	 - size of property in bytes
!  value 	 - default value for property in newly
!                  created property lists
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  M. Scot Breitenfeld
!  June 24, 2008
! SOURCE
  SUBROUTINE h5pregister_ptr(class, name, size, value, hdferr)
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value
    TYPE(C_PTR), INTENT(IN) :: value      ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, value)
  END SUBROUTINE h5pregister_ptr

!
!****s* H5P (F03)/h5pinsert_integer
!
! NAME
!  h5pinsert_integer
!
! PURPOSE
!  Registers a temporary property with a property list class.
!
! INPUTS
!  plist 	 - property list identifier
!  name 	 - name of property to insert
!  size 	 - size of property in bytes
!  value 	 - initial value for the property
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
!
! SOURCE
  SUBROUTINE h5pinsert_integer(plist, name, size, value, hdferr)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist    ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size    ! Size of the property value
    INTEGER,   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr

    f_ptr = c_loc(value)

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)
  END SUBROUTINE h5pinsert_integer

!
!****s* H5P (F03)/h5pinsert_real
!
! NAME
!  h5pinsert_real
!
! PURPOSE
!  Registers a temporary property with a property list class.
!
! INPUTS
!  plist 	 - property list identifier
!                  permanent property within
!  name 	 - name of property to insert
!  size 	 - size of property in bytes
!  value 	 - initial value for the property
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
! SOURCE
  SUBROUTINE h5pinsert_real(plist, name, size, value, hdferr)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist   ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value
    REAL,   INTENT(IN), TARGET :: value   ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr

    f_ptr = c_loc(value)

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)

  END SUBROUTINE h5pinsert_real

!
!****s* H5P (F03)/h5pinsert_double
!
! NAME
!  h5pinsert_double
!
! PURPOSE
!  Registers a temporary property with a property list class.
!
! INPUTS
!  plist 	 - property list identifier
!                  permanent property within
!  name 	 - name of property to insert
!  size 	 - size of property in bytes
!  value 	 - initial value for the property
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! OPTIONAL PARAMETERS
!  NONE
!
! AUTHOR
!
!  Elena Pourmal
!  October 10, 2002
! SOURCE
  SUBROUTINE h5pinsert_double(plist, name, size, value, hdferr)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist   ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value
    DOUBLE PRECISION, INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr

    f_ptr = c_loc(value)

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)

  END SUBROUTINE h5pinsert_double

!
!****s* H5P (F03)/h5pinsert_char
!
! NAME
!  h5pinsert_char
!
! PURPOSE
!  Registers a temporary property with a property list class.
!
! INPUTS
!  plist 	 - property list identifier
!                  permanent property within
!  name 	 - name of property to insert
!  size 	 - size of property in bytes
!  value 	 - initial value for the property
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
! SOURCE
  SUBROUTINE h5pinsert_char(plist, name, size, value, hdferr)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist      ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name     ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size      ! Size of property value
    CHARACTER(LEN=*),   INTENT(IN) :: value  ! Property value
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
!*****
    INTEGER :: name_len

    INTEGER :: i
    CHARACTER(LEN=1), ALLOCATABLE, DIMENSION(:), TARGET :: chr
    INTEGER :: chr_len

    TYPE(c_ptr) :: f_ptr
    ! To resolve Issue #1 outlined in the preamble of this file we
    ! need to pack the character string into an array.

    chr_len = LEN(value)
    ALLOCATE(chr(1:chr_len), STAT=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF

    DO i = 1, chr_len
       chr(i) = value(i:i)
    ENDDO

    f_ptr = C_LOC(chr)

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)

    DEALLOCATE(chr)

  END SUBROUTINE h5pinsert_char

!
!****s* H5P (F03)/h5pinsert_ptr
!
! NAME
!  h5pinsert_ptr
!
! PURPOSE
!  Registers a temporary property with a property list class.
!
! INPUTS
!  plist 	 - property list identifier
!                  permanent property within
!  name 	 - name of property to insert
!  size 	 - size of property in bytes
!  value 	 - initial value for the property
! OUTPUTS
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  M. Scot Breitenfeld
!  June 24, 2008
! SOURCE
  SUBROUTINE h5pinsert_ptr(plist, name, size, value, hdferr)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size  ! Size of property value
    TYPE(c_ptr),   INTENT(IN) :: value   ! Property value
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
!*****
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, value)
  END SUBROUTINE h5pinsert_ptr

!
!****s* H5P (F03)/h5pcreate_class_f
!
! NAME
!  h5pcreate_class_f
!
! PURPOSE
!  Create a new property list class
!
! INPUTS
!  parent 	 - Property list identifier of the parent class
!                  Possible values include:
!                   H5P_ROOT_F
!                   H5P_FILE_CREATE_F
!                   H5P_FILE_ACCESS_F
!                   H5P_DATASET_CREATE_F
!                   H5P_DATASET_XFER_F
!                   H5P_FILE_MOUNT_F
!  name 	 - name of the class we are creating
! OUTPUTS
!  class 	 - porperty list class identifier
!  hdferr:	 - error code
!
!  Success: 0
!  Failure: -1
! OPTIONAL PARAMETERS
!  H5P_cls_create_func_t (create) - Callback routine called when a property list is created
!  create_data 	                  - User pointer to any class creation information needed
!  H5P_cls_copy_func_t   (copy)   - Callback routine called when a property list is copied
!  copy_data 	                  - User pointer to any class copy information needed
!  H5P_cls_close_func_t  (close)  - Callback routine called when a property list is being closed
!  close_data 	                  - User pointer to any class close information needed
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
!
! HISTORY
!  Added callback arguments
!  M. Scot Breitenfeld, July 3, 2008
!
! SOURCE
  SUBROUTINE h5pcreate_class_f(parent, name, class, hdferr, create, create_data, copy, copy_data, close, close_data)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: parent  ! parent property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! name of property tocreate
    INTEGER(HID_T), INTENT(OUT) :: class  ! property list class identifier
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    TYPE(C_PTR), OPTIONAL :: create_data, copy_data, close_data
    TYPE(C_FUNPTR), OPTIONAL :: create, copy, close
!*****
    INTEGER :: name_len
    TYPE(C_PTR) :: create_data_default, copy_data_default, close_data_default
    TYPE(C_FUNPTR) :: create_default, copy_default, close_default
    INTERFACE
       INTEGER FUNCTION h5pcreate_class_c(parent, name, name_len, class, &
            create, create_data, &
            copy, copy_data, &
            close, close_data)

         USE iso_c_binding
         USE H5GLOBAL
         !DEC$IF DEFINED(HDCLOSEF90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PCREATE_CLASS_C'::h5pcreate_class_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: parent
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(HID_T), INTENT(OUT) :: class
         TYPE(C_PTR), VALUE :: create_data, copy_data, close_data
         TYPE(C_FUNPTR), VALUE :: create, copy, close
       END FUNCTION h5pcreate_class_c
    END INTERFACE
    name_len = LEN(name)

    create_default = c_null_funptr     !fix:scot
    create_data_default = c_null_ptr
    copy_default = c_null_funptr    !fix:scot
    copy_data_default = c_null_ptr
    close_default = c_null_funptr   !fix:scot
    close_data_default = c_null_ptr

    IF(PRESENT(create)) create_default = create
    IF(PRESENT(create_data)) create_data_default = create_data
    IF(PRESENT(copy)) copy_default = copy
    IF(PRESENT(copy_data)) copy_data_default = copy_data
    IF(PRESENT(close)) close_default = close
    IF(PRESENT(close_data)) close_data_default = close_data

    hdferr = h5pcreate_class_c(parent, name , name_len, class, &
         create_default, create_data_default, &
         copy_default, copy_data_default, &
         close_default, close_data_default)


  END SUBROUTINE h5pcreate_class_f

END MODULE H5P_PROVISIONAL

