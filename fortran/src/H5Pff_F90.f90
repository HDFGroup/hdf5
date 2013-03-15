!****h* ROBODoc/H5P (_F90)
!
! NAME
!  H5P_PROVISIONAL
!
! PURPOSE
!
!  This file contains Fortran 90 interfaces for H5P functions. It contains
!  the same functions as H5Pff_F03.f90 but excludes the Fortran 2003 functions
!  and the interface listings. This file will be compiled instead of H5Pff_F03.f90
!  if Fortran 2003 functions are not enabled.
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
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!*****

MODULE H5P_PROVISIONAL

  USE H5GLOBAL

  INTERFACE h5pset_fill_value_f
     MODULE PROCEDURE h5pset_fill_value_integer
     MODULE PROCEDURE h5pset_fill_value_real
     MODULE PROCEDURE h5pset_fill_value_char
  END INTERFACE

  INTERFACE h5pget_fill_value_f
     MODULE PROCEDURE h5pget_fill_value_integer
     MODULE PROCEDURE h5pget_fill_value_real
     MODULE PROCEDURE h5pget_fill_value_char
  END INTERFACE

  INTERFACE h5pset_f
     MODULE PROCEDURE h5pset_integer
     MODULE PROCEDURE h5pset_real
     MODULE PROCEDURE h5pset_char
  END INTERFACE

  INTERFACE h5pget_f
     MODULE PROCEDURE h5pget_integer
     MODULE PROCEDURE h5pget_real
     MODULE PROCEDURE h5pget_char
  END INTERFACE

  INTERFACE h5pregister_f
     MODULE PROCEDURE h5pregister_integer
     MODULE PROCEDURE h5pregister_real
     MODULE PROCEDURE h5pregister_char
  END INTERFACE

  INTERFACE h5pinsert_f
     MODULE PROCEDURE h5pinsert_integer
     MODULE PROCEDURE h5pinsert_real
     MODULE PROCEDURE h5pinsert_char
  END INTERFACE

CONTAINS
!
!****s* H5P (F90)/h5pset(get)fill_value_f
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
!  type_id	- datatype identifier for fill value
!  fillvalue	- fill value
!  hdferr 	- error code
!                  Success:  0
!                  Failure: -1
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 14, 2001
!
! NOTES
!  h5pset(get)fill_value_f function is overloaded to support
!  INTEGER, REAL, DOUBLE PRECISION and CHARACTER dtatypes.
!
! SOURCE
  SUBROUTINE h5pset_fill_value_integer(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    INTEGER, INTENT(IN) :: fillvalue      ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5pset_fill_value_integer_c(prp_id, type_id, fillvalue)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FILL_VALUE_INTEGER_C'::h5pset_fill_value_integer_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: fillvalue
       END FUNCTION h5pset_fill_value_integer_c
    END INTERFACE

    hdferr = h5pset_fill_value_integer_c(prp_id, type_id, fillvalue)
  END SUBROUTINE h5pset_fill_value_integer


  SUBROUTINE h5pget_fill_value_integer(prp_id, type_id, fillvalue, &
       hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    INTEGER, INTENT(IN) :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code

    INTERFACE
       INTEGER FUNCTION h5pget_fill_value_integer_c(prp_id, type_id, fillvalue)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FILL_VALUE_INTEGER_C'::h5pget_fill_value_integer_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER :: fillvalue
       END FUNCTION h5pget_fill_value_integer_c
    END INTERFACE

    hdferr = h5pget_fill_value_integer_c(prp_id, type_id, fillvalue)
  END SUBROUTINE h5pget_fill_value_integer


  SUBROUTINE h5pset_fill_value_real(prp_id, type_id, fillvalue, &
       hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    REAL, INTENT(IN) :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code

    INTERFACE
       INTEGER FUNCTION h5pset_fill_value_real_c(prp_id, type_id, fillvalue)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FILL_VALUE_REAL_C'::h5pset_fill_value_real_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         REAL, INTENT(IN) :: fillvalue
       END FUNCTION h5pset_fill_value_real_c
    END INTERFACE

    hdferr = h5pset_fill_value_real_c(prp_id, type_id, fillvalue)
  END SUBROUTINE h5pset_fill_value_real


  SUBROUTINE h5pget_fill_value_real(prp_id, type_id, fillvalue, &
       hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    REAL, INTENT(IN) :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code

    INTERFACE
       INTEGER FUNCTION h5pget_fill_value_real_c(prp_id, type_id, fillvalue)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FILL_VALUE_REAL_C'::h5pget_fill_value_real_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         REAL :: fillvalue
       END FUNCTION h5pget_fill_value_real_c
    END INTERFACE

    hdferr = h5pget_fill_value_real_c(prp_id, type_id, fillvalue)
  END SUBROUTINE h5pget_fill_value_real

  SUBROUTINE h5pset_fill_value_char(prp_id, type_id, fillvalue, &
       hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    CHARACTER, INTENT(IN) :: fillvalue    ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr        ! Error code

    INTERFACE
       INTEGER FUNCTION h5pset_fill_valuec_c(prp_id, type_id, fillvalue)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FILL_VALUEC_C'::h5pset_fill_valuec_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: fillvalue
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         CHARACTER, INTENT(IN) :: fillvalue
       END FUNCTION h5pset_fill_valuec_c
    END INTERFACE

    hdferr = h5pset_fill_valuec_c(prp_id, type_id, fillvalue)
  END SUBROUTINE h5pset_fill_value_char

  SUBROUTINE h5pget_fill_value_char(prp_id, type_id, fillvalue, &
       hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    CHARACTER, INTENT(IN) :: fillvalue    ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr        ! Error code

    INTERFACE
       INTEGER FUNCTION h5pget_fill_valuec_c(prp_id, type_id, fillvalue)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FILL_VALUEC_C'::h5pget_fill_valuec_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: fillvalue
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         CHARACTER :: fillvalue
       END FUNCTION h5pget_fill_valuec_c
    END INTERFACE

    hdferr = h5pget_fill_valuec_c(prp_id, type_id, fillvalue)
  END SUBROUTINE h5pget_fill_value_char
!
!****s* H5P (F90)/h5pset_integer
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
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
!
! SOURCE
  SUBROUTINE h5pset_integer(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    INTEGER, INTENT(IN) :: value          ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pset_integer_c(prp_id, name, name_len, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_INTEGER_C'::h5pset_integer_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER, INTENT(IN) :: value
       END FUNCTION h5pset_integer_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pset_integer_c(prp_id, name , name_len, value)
  END SUBROUTINE h5pset_integer

!
!****s* H5P (F90)/h5pset_real
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
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
!
! SOURCE
  SUBROUTINE h5pset_real(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    REAL, INTENT(IN) :: value             ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pset_real_c(prp_id, name, name_len, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_REAL_C'::h5pset_real_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         REAL, INTENT(IN) :: value
       END FUNCTION h5pset_real_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pset_real_c(prp_id, name , name_len, value)
  END SUBROUTINE h5pset_real

!****s* H5P (F90)/h5pset_char
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
!  hdferr	 - error code
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
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id    ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Name of property to modify
    CHARACTER(LEN=*),   INTENT(IN) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    INTEGER :: name_len
    INTEGER :: value_len

    INTERFACE
       INTEGER FUNCTION h5psetc_c(prp_id, name, name_len, value, value_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSETC_C'::h5psetc_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         !DEC$ATTRIBUTES reference :: value
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         CHARACTER(LEN=*), INTENT(IN) :: value
         INTEGER, INTENT(IN)         :: value_len
       END FUNCTION h5psetc_c
    END INTERFACE

    name_len = LEN(name)
    value_len = LEN(value)
    hdferr = h5psetc_c(prp_id, name , name_len, value, value_len)
  END SUBROUTINE h5pset_char

!****s* H5P (F90)/h5pget_integer
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
!  hdferr	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
!
! SOURCE
  SUBROUTINE h5pget_integer(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    INTEGER,   INTENT(OUT) :: value       ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pget_integer_c(prp_id, name, name_len, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_INTEGER_C'::h5pget_integer_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER, INTENT(OUT) :: value
       END FUNCTION h5pget_integer_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pget_integer_c(prp_id, name , name_len, value)
  END SUBROUTINE h5pget_integer

!
!****s* H5P (F90)/h5pget_real
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
!  hdferr	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
! SOURCE
  SUBROUTINE h5pget_real(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    REAL,   INTENT(OUT) :: value          ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pget_real_c(prp_id, name, name_len, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_REAL_C'::h5pget_real_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         REAL, INTENT(OUT) :: value
       END FUNCTION h5pget_real_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pget_real_c(prp_id, name , name_len, value)
  END SUBROUTINE h5pget_real

!
!****s* H5P (F90)/h5pget_char
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
!  hdferr	 - error code
!                   Success:  0
!                   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
!
! SOURCE
  SUBROUTINE h5pget_char(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id     ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name     ! Name of property to modify
    CHARACTER(LEN=*),   INTENT(OUT) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
!*****
    INTEGER :: name_len
    INTEGER :: value_len

    INTERFACE
       INTEGER FUNCTION h5pgetc_c(prp_id, name, name_len, value, value_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGETC_C'::h5pgetc_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         !DEC$ATTRIBUTES reference :: value
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         CHARACTER(LEN=*), INTENT(OUT) :: value
         INTEGER, INTENT(IN)         :: value_len
       END FUNCTION h5pgetc_c
    END INTERFACE

    name_len = LEN(name)
    value_len = LEN(value)
    hdferr = h5pgetc_c(prp_id, name , name_len, value, value_len)
  END SUBROUTINE h5pget_char

!
!****s* H5P (F90)/h5pregister_integer
!
! NAME
!  h5pregister_integer
!
! PURPOSE
!  Registers a permanent property with a property list class.
!
! INPUTS
!  class 	 - property list class to register
!  permanent property within
!  name 	 - name of property to register
!  size 	 - size of property in bytes
!  value 	 - default value for property in newly
!  created property lists
! OUTPUTS
!  hdferr	 - error code
!                   Success:  0
!                   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
!
! SOURCE
  SUBROUTINE h5pregister_integer(class, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value
    INTEGER,   INTENT(IN) :: value        ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pregister_integer_c(class, name, name_len, size, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PREGISTER_INTEGER_C'::h5pregister_integer_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: class
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(SIZE_T), INTENT(IN) :: size
         INTEGER, INTENT(IN) :: value
       END FUNCTION h5pregister_integer_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pregister_integer_c(class, name , name_len, size, value)
  END SUBROUTINE h5pregister_integer

!****s* H5P (F90)/h5pregister_real
!
! NAME
!
!  h5pregister_real
!
! PURPOSE 	Registers a permanent property with a property list class.
!
! INPUTS
!  class 	 - property list class to register
!                  permanent property within
!  name 	 - name of property to register
!  size 	 - size of property in bytes
!  value 	 - default value for property in newly
!                  created property lists
! OUTPUTS
!  hdferr	 - error code
!                   Success:  0
!                   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
! SOURCE
  SUBROUTINE h5pregister_real(class, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size   ! size of the property value
    REAL,   INTENT(IN) :: value           ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pregister_real_c(class, name, name_len, size, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PREGISTER_REAL_C'::h5pregister_real_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: class
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(SIZE_T), INTENT(IN) :: size
         REAL, INTENT(IN) :: value
       END FUNCTION h5pregister_real_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pregister_real_c(class, name , name_len, size, value)
  END SUBROUTINE h5pregister_real

!
!****s* H5P (F90)/h5pregister_char
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
!  hdferr	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
! SOURCE
  SUBROUTINE h5pregister_char(class, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class     ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size     ! size of the property value
    CHARACTER(LEN=*),   INTENT(IN) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    INTEGER :: name_len
    INTEGER :: value_len

    INTERFACE
       INTEGER FUNCTION h5pregisterc_c(class, name, name_len, size, value, &
            value_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PREGISTERC_C'::h5pregisterc_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         !DEC$ATTRIBUTES reference :: value
         INTEGER(HID_T), INTENT(IN) :: class
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(SIZE_T), INTENT(IN) :: size
         CHARACTER(LEN=*), INTENT(IN) :: value
         INTEGER, INTENT(IN)          :: value_len
       END FUNCTION h5pregisterc_c
    END INTERFACE

    name_len = LEN(name)
    value_len = LEN(value)
    hdferr = h5pregisterc_c(class, name , name_len, size, value, value_len)
  END SUBROUTINE h5pregister_char
!
!****s* H5P (F90)/h5pinsert_integer
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
!  hdferr	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
! SOURCE
  SUBROUTINE h5pinsert_integer(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist   ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value
    INTEGER,   INTENT(IN) :: value        ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pinsert_integer_c(plist, name, name_len, size, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PINSERT_INTEGER_C'::h5pinsert_integer_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: plist
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(SIZE_T), INTENT(IN) :: size
         INTEGER, INTENT(IN) :: value
       END FUNCTION h5pinsert_integer_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pinsert_integer_c(plist, name , name_len, size, value)
  END SUBROUTINE h5pinsert_integer

!
!****s* H5P (F90)/h5pinsert_real
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
!  hdferr	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
! SOURCE
  SUBROUTINE h5pinsert_real(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist   ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value
    REAL,   INTENT(IN) :: value           ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pinsert_real_c(plist, name, name_len, size, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PINSERT_REAL_C'::h5pinsert_real_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: plist
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(SIZE_T), INTENT(IN) :: size
         REAL, INTENT(IN) :: value
       END FUNCTION h5pinsert_real_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pinsert_real_c(plist, name , name_len, size, value)
  END SUBROUTINE h5pinsert_real


!
!****s* H5P (F90)/h5pinsert_char
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
!  hdferr	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
! SOURCE
SUBROUTINE h5pinsert_char(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist      ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name     ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size      ! Size of property value
    CHARACTER(LEN=*),   INTENT(IN) :: value  ! Property value
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
!*****
    INTEGER :: name_len
    INTEGER :: value_len

    INTERFACE
       INTEGER FUNCTION h5pinsertc_c(plist, name, name_len, size, value, value_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PINSERTC_C'::h5pinsertc_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         !DEC$ATTRIBUTES reference :: value
         INTEGER(HID_T), INTENT(IN) :: plist
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(SIZE_T), INTENT(IN) :: size
         CHARACTER(LEN=*), INTENT(IN) :: value
         INTEGER, INTENT(IN)         :: value_len
       END FUNCTION h5pinsertc_c
    END INTERFACE

    name_len = LEN(name)
    value_len = LEN(value)
    hdferr = h5pinsertc_c(plist, name , name_len, size, value, value_len)
  END SUBROUTINE h5pinsert_char

!
!****s* H5P (F90)/h5pcreate_class_f
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
!                    H5P_ROOT_F
!                    H5P_FILE_CREATE_F
!                    H5P_FILE_ACCESS_F
!                    H5P_DATASET_CREATE_F
!                    H5P_DATASET_XFER_F
!                    H5P_FILE_MOUNT_F
!  name 	 - name of the class we are creating
! OUTPUTS
!  class 	 - property list class identifier
!  hdferr	 - error code
!                   Success: 0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
!
! SOURCE
  SUBROUTINE h5pcreate_class_f(parent, name, class, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: parent  ! parent property list class
                                          ! identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! name of property tocreate
    INTEGER(HID_T), INTENT(OUT) :: class  ! property list class identifier
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pcreate_class_c(parent, name, name_len,&
            class)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PCREATE_CLASS_C'::h5pcreate_class_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: parent
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(HID_T), INTENT(OUT) :: class
       END FUNCTION h5pcreate_class_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pcreate_class_c(parent, name, name_len, class)

  END SUBROUTINE h5pcreate_class_f

END MODULE H5P_PROVISIONAL

