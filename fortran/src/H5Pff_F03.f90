!****h* ROBODoc/H5P (F03)
!
! NAME
!  H5P_PROVISIONAL
!
! PURPOSE
!  This file contains Fortran 90 and Fortran 2003 interfaces for H5P functions.
!  It contains the same functions as H5Pff_F90.f90 but includes the
!  Fortran 2003 functions and the interface listings. This file will be compiled
!  instead of H5Pff_F90.f90 if Fortran 2003 functions are enabled.
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
       USE, INTRINSIC :: ISO_C_BINDING
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
!****s* H5P (F03)/h5pset_fill_value_f_F90
!
! NAME
!  h5pset_fill_value_f
!
! PURPOSE
!  Sets fill value for a dataset creation property list
!
! Inputs:
!  prp_id    - Property list identifier
!  type_id   - Datatype identifier of fill value datatype (in memory)
!  fillvalue - Fillvalue
!
! Outputs:
!  hdferr    - Returns 0 if successful and -1 if fails
!
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
! Fortran90 Interface:
!!  SUBROUTINE h5pset_fill_value_f(prp_id, type_id, fillvalue, hdferr) 
!!    IMPLICIT NONE
!!    INTEGER(HID_T), INTENT(IN)  :: prp_id
!!    INTEGER(HID_T), INTENT(IN)  :: type_id
!!    TYPE(VOID)    , INTENT(IN)  :: fillvalue
!!    INTEGER       , INTENT(OUT) :: hdferr
!*****


  SUBROUTINE h5pset_fill_value_integer(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
                                          ! of fillvalue datatype
                                          ! (in memory)
    INTEGER, INTENT(IN), TARGET :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    TYPE(C_PTR) :: f_ptr ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = h5pset_fill_value_c(prp_id, type_id, f_ptr)

  END SUBROUTINE h5pset_fill_value_integer
!
!****s* H5P (F03)/h5pget_fill_value_f_F90
!
! NAME
!  h5pget_fill_value_f
!
! PURPOSE
!  Gets fill value for a dataset creation property list
!
! Inputs:
!  prp_id    - Property list identifier
!  type_id   - Datatype identifier of fill value datatype (in memory)
!
! Outputs:
!  fillvalue - Fillvalue
!  hdferr    - Returns 0 if successful and -1 if fails
!
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
!  h5pget(get)fill_value_f function is overloaded to support
!  INTEGER, REAL, DOUBLE PRECISION and CHARACTER dtatypes.
!
! Fortran90 Interface:
!!  SUBROUTINE h5pget_fill_value_f(prp_id, type_id, fillvalue, hdferr)
!!    INTEGER(HID_T), INTENT(IN)  :: prp_id 
!!    INTEGER(HID_T), INTENT(IN)  :: type_id
!!    TYPE(VOID)    , INTENT(OUT) :: fillvalue
!!    INTEGER       , INTENT(OUT) :: hdferr
!*****

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

    f_ptr = C_LOC(chr(1)(1:1))

    hdferr = h5pget_fill_value_c(prp_id, type_id, f_ptr)

    DO i = 1, chr_len
       fillvalue(i:i) = chr(i)
    ENDDO
    DEALLOCATE(chr)

  END SUBROUTINE h5pget_fill_value_char
!
!****s* H5P (F03)/h5pset_fill_value_f_F03
!
! NAME
!  h5pset_fill_value_f
!
! PURPOSE
!  Sets fill value for a dataset creation property list
!
! Inputs:
!  prp_id    - Property list identifier
!  type_id   - Datatype identifier of fill value datatype (in memory)
!  fillvalue - Fillvalue
!
! Outputs:
!  hdferr    - Returns 0 if successful and -1 if fails
!
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
! Fortran2003 Interface:
!!  SUBROUTINE h5pset_fill_value_f(prp_id, type_id, fillvalue, hdferr)
!!    INTEGER(HID_T), INTENT(IN)  :: prp_id 
!!    INTEGER(HID_T), INTENT(IN)  :: type_id
!!    TYPE(C_PTR)   , INTENT(IN)  :: fillvalue
!!    INTEGER       , INTENT(OUT) :: hdferr
!*****

  SUBROUTINE h5pset_fill_value_ptr(prp_id, type_id, fillvalue, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id    ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id   ! Datatype identifier of
                                            ! of fillvalue datatype
                                            ! (in memory)
    TYPE(C_PTR), INTENT(IN)    :: fillvalue ! Fillvalue
    INTEGER, INTENT(OUT)       :: hdferr    ! Error code

    hdferr = h5pset_fill_value_c(prp_id, type_id, fillvalue)

  END SUBROUTINE h5pset_fill_value_ptr

!
!****s* H5P (F03)/h5pget_fill_value_f_F03
!
! NAME
!  h5pget_fill_value_f
!
! PURPOSE
!  Gets fill value for a dataset creation property list
!
! Inputs:
!  prp_id    - Property list identifier
!  type_id   - Datatype identifier of fill value datatype (in memory)
!
! Outputs:
!  fillvalue - Fillvalue
!  hdferr    - Returns 0 if successful and -1 if fails
!
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
!  h5pget(get)fill_value_f function is overloaded to support
!  INTEGER, REAL, DOUBLE PRECISION and CHARACTER dtatypes.
!
! Fortran2003 Interface:
!!  SUBROUTINE h5pget_fill_value_f(prp_id, type_id, fillvalue, hdferr)
!!    INTEGER(HID_T), INTENT(IN)  :: prp_id 
!!    INTEGER(HID_T), INTENT(IN)  :: type_id
!!    TYPE(C_PTR)   , INTENT(OUT) :: fillvalue
!!    INTEGER       , INTENT(OUT) :: hdferr
!*****

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
!****s* H5P (F03)/h5pset_f_F90
!
! NAME
!  h5pset_f
!
! PURPOSE
!  Sets a property list value
!
! Inputs:
!  prp_id  - Property list identifier to modify
!  name    - Name of property to modify
!  value   - Property value, supported types are:
!             INTEGER
!             REAL
!             DOUBLE PRECISION
!             CHARACTER(LEN=*)
! Outputs:
!  hdferr  - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
!
! Fortran90 Interface:
!!  SUBROUTINE h5pset_f(plid, name, value, hdferr)
!!    INTEGER(HID_T)  , INTENT(IN)  :: plid
!!    CHARACTER(LEN=*), INTENT(IN)  :: name
!!    TYPE            , INTENT(IN)  :: value
!!    INTEGER         , INTENT(OUT) :: hdferr
!*****
  SUBROUTINE h5pset_integer(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id   ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of property to modify
    INTEGER,   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr         ! Error code

    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pset_integer

  SUBROUTINE h5pset_real(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name ! Name of property to modify
    REAL,   INTENT(IN), TARGET :: value  ! Property value
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pset_real

  SUBROUTINE h5pset_char(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id    ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Name of property to modify
    CHARACTER(LEN=*),   INTENT(IN) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
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

    f_ptr = C_LOC(chr(1)(1:1))

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

    DEALLOCATE(chr)

  END SUBROUTINE h5pset_char
!
!****s* H5P (F03)/h5pget_f_F90
!
! NAME
!  h5pget_f
!
! PURPOSE
!  Queries the value of a property.
!
! Inputs:
!  prp_id  - Property list identifier to modify
!  name    - Name of property to get
!  value   - Property value, supported types are:
!             INTEGER
!             REAL
!             DOUBLE PRECISION
!             CHARACTER(LEN=*)
! Outputs:
!  hdferr  - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
!
! Fortran90 Interface:
!!  SUBROUTINE h5pget_f(plid, name, value, hdferr)
!!    INTEGER(HID_T)  , INTENT(IN)  :: plid
!!    CHARACTER(LEN=*), INTENT(IN)  :: name
!!    TYPE            , INTENT(OUT) :: value
!!    INTEGER         , INTENT(OUT) :: hdferr
!*****

  SUBROUTINE h5pget_integer(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id    ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Name of property to modify
    INTEGER,   INTENT(OUT), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pget_integer

  SUBROUTINE h5pget_real(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name ! Name of property to modify
    REAL,   INTENT(OUT), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)
  END SUBROUTINE h5pget_real

  SUBROUTINE h5pget_char(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id   ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of property to modify
    CHARACTER(LEN=*), INTENT(OUT) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
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
    f_ptr = C_LOC(chr(1)(1:1))

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

    DO i = 1, chr_len
       value(i:i) = chr(i)
    ENDDO

    DEALLOCATE(chr)

  END SUBROUTINE h5pget_char


!
!****s* H5P (F03)/h5pset_f_F03
!
! NAME
!  h5pset_f
!
! PURPOSE
!  Sets a property list value
!
! Inputs:
!  prp_id  - Property list identifier to modify
!  name    - Name of property to modify
!  value   - Pointer to value to set the property to
! Outputs:
!  hdferr  - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
!
! Fortran2003 Interface:
!!  SUBROUTINE h5pset_f(plid, name, value, hdferr)
!!    INTEGER(HID_T)  , INTENT(IN)  :: plid
!!    CHARACTER(LEN=*), INTENT(IN)  :: name
!!    TYPE(C_PTR)     , INTENT(IN)  :: value
!!    INTEGER         , INTENT(OUT) :: hdferr
!*****
  SUBROUTINE h5pset_ptr(prp_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    TYPE(C_PTR), INTENT(IN) :: value      ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pset_c(prp_id, name, name_len, value)

  END SUBROUTINE h5pset_ptr
!
!****s* H5P (F03)/h5pget_f_F03
!
! NAME
!  h5pget_f (F03)
!
! PURPOSE
!  Queries the value of a property.
!
! Inputs:
!  prp_id  - Property list identifier to modify
!  name    - Name of property to get
!  value   - Pointer to a location to which to copy the value of of the property
! Outputs:
!  hdferr  - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002
!
! Fortran2003 Interface:
!!  SUBROUTINE h5pget_f(plid, name, value, hdferr)
!!    INTEGER(HID_T)  , INTENT(IN)  :: plid
!!    CHARACTER(LEN=*), INTENT(IN)  :: name
!!    TYPE(C_PTR)     , INTENT(OUT) :: value
!!    INTEGER         , INTENT(OUT) :: hdferr
!*****
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
!****s* H5P (F03)/h5pregister_f_F90
!
! NAME
!  h5pregister
!
! PURPOSE
!  Registers a permanent property with a property list class.
!
! Inputs:
!  class  - Property list class identifier
!  name   - Name of property to register
!  size   - Size of the property value
!  value  - Property value, supported types are:
!             INTEGER
!             REAL
!             DOUBLE PRECISION
!             CHARACTER(LEN=*)
!
! Outputs:
!  hdferr - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
!
! Fortran90 Interface:
!!  SUBROUTINE h5pregister_f(class, name, size, value, hdferr)
!!    INTEGER(HID_T)  , INTENT(IN)  :: class
!!    CHARACTER(LEN=*), INTENT(IN)  :: name
!!    INTEGER(SIZE_T) , INTENT(IN)  :: size
!!    TYPE            , INTENT(IN)  :: value
!!    INTEGER         , INTENT(OUT) :: hdferr  
!*****
  SUBROUTINE h5pregister_integer(class, name, size, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class    ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size    ! Size of the property value
    INTEGER,   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)

  END SUBROUTINE h5pregister_integer

  SUBROUTINE h5pregister_real(class, name, size, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size   ! size of the property value
    REAL,   INTENT(IN), TARGET :: value   ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)

  END SUBROUTINE h5pregister_real

  SUBROUTINE h5pregister_char(class, name, size, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class     ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size     ! size of the property value
    CHARACTER(LEN=*),   INTENT(IN) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
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

    f_ptr = C_LOC(chr(1)(1:1))

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)
    DEALLOCATE(chr)
  END SUBROUTINE h5pregister_char
!
!****s* H5P (F03)/h5pregister_f_F03
!
! NAME
!  h5pregister (F03)
!
! PURPOSE
!  Registers a permanent property with a property list class.
!
! Inputs:
!  class  - Property list class identifier
!  name   - Name of property to register
!  size   - Size of the property value
!  value  - Pointer to value to set the property to
!
! Outputs:
!  hdferr - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  June 24, 2008
!
! Fortran2003 Interface:
!!  SUBROUTINE h5pregister_f(class, name, size, value, hdferr)
!!    INTEGER(HID_T)  , INTENT(IN)  :: class
!!    CHARACTER(LEN=*), INTENT(IN)  :: name
!!    INTEGER(SIZE_T) , INTENT(IN)  :: size
!!    TYPE(C_PTR)     , INTENT(IN)  :: value
!!    INTEGER         , INTENT(OUT) :: hdferr  
!*****

  SUBROUTINE h5pregister_ptr(class, name, size, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value
    TYPE(C_PTR), INTENT(IN) :: value      ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, value)
  END SUBROUTINE h5pregister_ptr

!
!****s* H5P (F03)/h5pinsert_f_F90
!
! NAME
!  h5pinsert  (f90)
!
! PURPOSE
!  Registers a temporary property with a property list class.
!
! Inputs:
!  plist  - Property list class identifier
!  name   - Name of property to insert
!  size   - Size of the property value
!  value  - Property value, supported types are:
!             INTEGER
!             REAL
!             DOUBLE PRECISION
!             CHARACTER(LEN=*)
! Outputs:
!  hdferr - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  October 10, 2002
!
! Fortran90 Interface:
!!  SUBROUTINE h5pinsert_f
!!    INTEGER(HID_T)  , INTENT(IN)  :: plist
!!    CHARACTER(LEN=*), INTENT(IN)  :: name
!!    INTEGER(SIZE_T) , INTENT(IN)  :: size
!!    TYPE            , INTENT(IN)  :: value
!!    INTEGER         , INTENT(OUT) :: hdferr
!*****
  SUBROUTINE h5pinsert_integer(plist, name, size, value, hdferr)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist    ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size    ! Size of the property value
    INTEGER,   INTENT(IN), TARGET :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr

    f_ptr = c_loc(value)

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)
  END SUBROUTINE h5pinsert_integer

  SUBROUTINE h5pinsert_real(plist, name, size, value, hdferr)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist   ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value
    REAL,   INTENT(IN), TARGET :: value   ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr

    f_ptr = c_loc(value)

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)

  END SUBROUTINE h5pinsert_real

  SUBROUTINE h5pinsert_char(plist, name, size, value, hdferr)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist      ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name     ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size      ! Size of property value
    CHARACTER(LEN=*),   INTENT(IN) :: value  ! Property value
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
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

    f_ptr = C_LOC(chr(1)(1:1))

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)

    DEALLOCATE(chr)

  END SUBROUTINE h5pinsert_char

!
!****s* H5P (F03)/h5pinsert_f_F03
!
! NAME
!  h5pinsert  (f03)
!
! PURPOSE
!  Registers a temporary property with a property list class.
!
! Inputs:
!  plist  - Property list class identifier
!  name   - Name of property to insert
!  size   - Size of the property value
!  value  - Pointer to new value pointer for the property being modified
!
! Outputs:
!  hdferr - Returns 0 if successful and -1 if fails
!
! AUTHOR 
!  M. Scot Breitenfeld
!  June 24, 2008
!
! Fortran90 Interface:
!!  SUBROUTINE h5pinsert_f
!!    INTEGER(HID_T)  , INTENT(IN)  :: plist
!!    CHARACTER(LEN=*), INTENT(IN)  :: name
!!    INTEGER(SIZE_T) , INTENT(IN)  :: size
!!    TYPE(C_PTR)     , INTENT(IN)  :: value
!!    INTEGER         , INTENT(OUT) :: hdferr
!*****
  SUBROUTINE h5pinsert_ptr(plist, name, size, value, hdferr)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size  ! Size of property value
    TYPE(c_ptr),   INTENT(IN) :: value   ! Property value
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, value)
  END SUBROUTINE h5pinsert_ptr
!
!****s* H5P (F03)/h5pcreate_class_f_F03
!
! NAME
!  h5pcreate_class_f
!
! PURPOSE
!  Create a new property list class
!
! Inputs:
!  parent  - Parent property list class identifier
!            Possible values include:
!              H5P_ROOT_F
!              H5P_FILE_CREATE_F
!              H5P_FILE_ACCESS_F
!              H5P_DATASET_CREATE_F
!              H5P_DATASET_XFER_F
!              H5P_FILE_MOUNT_F
!  name    - Name of property to create
!
! Outputs:
!  class   - Property list class identifier
!  hdferr  - Returns 0 if successful and -1 if fails
!
! Optional parameters:
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
! Fortran2003 Interface:
  SUBROUTINE h5pcreate_class_f(parent, name, class, hdferr, create, create_data, copy, copy_data, close, close_data)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: parent
    CHARACTER(LEN=*), INTENT(IN)  :: name
    INTEGER(HID_T)  , INTENT(OUT) :: class
    INTEGER         , INTENT(OUT) :: hdferr
    TYPE(C_PTR)     , OPTIONAL    :: create_data, copy_data, close_data
    TYPE(C_FUNPTR)  , OPTIONAL    :: create, copy, close
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

!
!****s* H5P (F03)/h5pset_file_image_f_F03
!
! NAME
!  h5pset_file_image_f
!
! PURPOSE
!  Sets an initial file image in a memory buffer.
!
! Inputs:
!  fapl_id - File access property list identifier
!  buf_ptr - Pointer to the initial file image, 
!            or C_NULL_PTR if no initial file image is desired
!  buf_len - Size of the supplied buffer, or 0 (zero) if no initial image is desired
!
! Outputs:
!  hdferr  - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  February 19, 2012
!
! Fortran2003 Interface:
  SUBROUTINE h5pset_file_image_f(fapl_id, buf_ptr, buf_len, hdferr)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)  :: fapl_id
    TYPE(C_PTR)    , INTENT(IN)  :: buf_ptr
    INTEGER(SIZE_T), INTENT(IN)  :: buf_len
    INTEGER        , INTENT(OUT) :: hdferr
!*****
    INTERFACE
       INTEGER FUNCTION h5pset_file_image_c(fapl_id, buf_ptr, buf_len)
         USE iso_c_binding
         USE H5GLOBAL
         !DEC$IF DEFINED(HDCLOSEF90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FILE_IMAGE_C'::h5pset_file_image_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: fapl_id
         TYPE(C_PTR), VALUE :: buf_ptr
         INTEGER(SIZE_T), INTENT(IN)  :: buf_len
       END FUNCTION h5pset_file_image_c
    END INTERFACE

    hdferr = h5pset_file_image_c(fapl_id, buf_ptr, buf_len)

  END SUBROUTINE h5pset_file_image_f
!
!****s* H5P (F03)/h5pget_file_image_f_F03
!
! NAME
!  h5pget_file_image_f
!
! PURPOSE
!  Retrieves a copy of the file image designated as the initial content and structure of a file. 
!
! Inputs:
!  fapl_id     - File access property list identifier.
!
! Outputs:
!  buf_ptr     - Will hold either a C_NULL_PTR or a scalar of type
!                c_loc. If buf_ptr is not C_NULL_PTR, on successful
!                return, buf_ptr shall contain a C pointer to a copy
!                of the initial image provided in the last call to
!                H5Pset_file_image_f for the supplied fapl_id, or
!                buf_ptr shall contain a C_NULL_PTR if there is no
!                initial image set.
!
!  buf_len_ptr - Contains the value of the buffer parameter for
!                the initial image in the supplied fapl_id. The value
!                will be 0 if no initial image is set.
!
!
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  February 19, 2012
!
! Fortran2003 Interface:
  SUBROUTINE h5pget_file_image_f(fapl_id, buf_ptr, buf_len_ptr, hdferr)
    USE iso_c_binding
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)                :: fapl_id
    TYPE(C_PTR)    , INTENT(OUT), DIMENSION(*) :: buf_ptr
    INTEGER(SIZE_T), INTENT(OUT)               :: buf_len_ptr
    INTEGER        , INTENT(OUT)               :: hdferr
    
!*****
    INTERFACE
       INTEGER FUNCTION h5pget_file_image_c(fapl_id, buf_ptr, buf_len_ptr)
         USE iso_c_binding
         USE H5GLOBAL
         !DEC$IF DEFINED(HDCLOSEF90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FILE_IMAGE_C'::h5pget_file_image_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: fapl_id
         TYPE(C_PTR), DIMENSION(*), INTENT(OUT)  :: buf_ptr
         INTEGER(SIZE_T), INTENT(OUT)  :: buf_len_ptr
       END FUNCTION h5pget_file_image_c
    END INTERFACE

    hdferr = h5pget_file_image_c(fapl_id, buf_ptr, buf_len_ptr)

  END SUBROUTINE h5pget_file_image_f

END MODULE H5P_PROVISIONAL

