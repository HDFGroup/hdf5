!****h* ROBODoc/H5A (F03)
! NAME
!  H5A_PROVISIONAL
!
! FILE
!  src/fortran/src/H5Aff_F03.f90
!
! PURPOSE
!  This file contains Fortran 90 and Fortran 2003 interfaces for H5A functions.
!  It contains the same functions as H5Aff_F90.f90 but includes the
!  Fortran 2003 functions and the interface listings. This file will be compiled
!  instead of H5Aff_F90.f90 if Fortran 2003 functions are enabled.
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
!
!  (A) C_LOC and character strings according to the Fortran 2003 standard:
!
!  15.1.2.5 C_LOC(X)
!
!  Argument. X shall either
!
!  (1) have interoperable type and type parameters and be
!    (a) a variable that has the TARGET attribute and is interoperable,
!    (b) an allocated allocatable variable that has the TARGET attribute
!        and is not an array of zero size, or
!    (c) an associated scalar pointer, or
!  (2) be a nonpolymorphic scalar, have no length type parameters, and be
!    (a) a nonallocatable, nonpointer variable that has the TARGET attribute,
!    (b) an allocated allocatable variable that has the TARGET attribute, or
!    (c) an associated pointer.
!
!  - When X is a character, for interoperability the standard is:
!
!  15.2.1 Interoperability of intrinsic types
!
!  ...if the type is character, interoperability also requires that the length type parameter
!  be omitted or be specified by an initialization expression whose value is one.
!
!  THEREFORE compilers that have not extended the standard  require
!
!  CHARACTER(LEN=1), TARGET :: chr
!  or
!  CHARACTER, TARGET :: chr
!
!  (B)
!                         *** IMPORTANT ***
!  If you add a new H5A function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5A_PROVISIONAL

  USE H5GLOBAL
!
!  On Windows there are no big (integer*8) integers, so overloading
!  for bug #670 does not work. I have to use DEC compilation directives to make
!  Windows DEC Visual Fortran and OSF compilers happy and do right things.
!  05/01/02 EP
!
  INTERFACE h5awrite_f
     MODULE PROCEDURE h5awrite_integer_scalar
     MODULE PROCEDURE h5awrite_integer_1
     MODULE PROCEDURE h5awrite_integer_2
     MODULE PROCEDURE h5awrite_integer_3
     MODULE PROCEDURE h5awrite_integer_4
     MODULE PROCEDURE h5awrite_integer_5
     MODULE PROCEDURE h5awrite_integer_6
     MODULE PROCEDURE h5awrite_integer_7
     MODULE PROCEDURE h5awrite_char_scalar
     MODULE PROCEDURE h5awrite_char_1
     MODULE PROCEDURE h5awrite_char_2
     MODULE PROCEDURE h5awrite_char_3
     MODULE PROCEDURE h5awrite_char_4
     MODULE PROCEDURE h5awrite_char_5
     MODULE PROCEDURE h5awrite_char_6
     MODULE PROCEDURE h5awrite_char_7
     MODULE PROCEDURE h5awrite_real_scalar
     MODULE PROCEDURE h5awrite_real_1
     MODULE PROCEDURE h5awrite_real_2
     MODULE PROCEDURE h5awrite_real_3
     MODULE PROCEDURE h5awrite_real_4
     MODULE PROCEDURE h5awrite_real_5
     MODULE PROCEDURE h5awrite_real_6
     MODULE PROCEDURE h5awrite_real_7
     ! This is the preferred way to call h5awrite
     ! by passing an address
     MODULE PROCEDURE h5awrite_ptr

  END INTERFACE

  INTERFACE h5aread_f

     MODULE PROCEDURE h5aread_integer_scalar
     MODULE PROCEDURE h5aread_integer_1
     MODULE PROCEDURE h5aread_integer_2
     MODULE PROCEDURE h5aread_integer_3
     MODULE PROCEDURE h5aread_integer_4
     MODULE PROCEDURE h5aread_integer_5
     MODULE PROCEDURE h5aread_integer_6
     MODULE PROCEDURE h5aread_integer_7
     MODULE PROCEDURE h5aread_char_scalar
     MODULE PROCEDURE h5aread_char_1
     MODULE PROCEDURE h5aread_char_2
     MODULE PROCEDURE h5aread_char_3
     MODULE PROCEDURE h5aread_char_4
     MODULE PROCEDURE h5aread_char_5
     MODULE PROCEDURE h5aread_char_6
     MODULE PROCEDURE h5aread_char_7
     MODULE PROCEDURE h5aread_real_scalar
     MODULE PROCEDURE h5aread_real_1
     MODULE PROCEDURE h5aread_real_2
     MODULE PROCEDURE h5aread_real_3
     MODULE PROCEDURE h5aread_real_4
     MODULE PROCEDURE h5aread_real_5
     MODULE PROCEDURE h5aread_real_6
     MODULE PROCEDURE h5aread_real_7

     ! This is the preferred way to call h5aread
     ! by passing an address
     MODULE PROCEDURE h5aread_ptr

  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Awrite routine

  INTERFACE
     INTEGER FUNCTION h5awrite_f_c(attr_id, mem_type_id, buf) BIND(C, NAME='h5awrite_f_c')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_ptr
       USE H5GLOBAL
       INTEGER(HID_T), INTENT(IN) :: attr_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5awrite_f_c
  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Aread routine

  INTERFACE
     INTEGER FUNCTION h5aread_f_c(attr_id, mem_type_id, buf) BIND(C, NAME='h5aread_f_c')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_ptr
       USE H5GLOBAL
       INTEGER(HID_T), INTENT(IN) :: attr_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5aread_f_c
  END INTERFACE

CONTAINS

!****s* H5A (F03)/h5awrite_f_F90
!
! NAME
!  h5awrite_f_F90
!
! PURPOSE
!  Writes an attribute.
!
! Inputs:
!  attr_id     - Attribute identifier
!  memtype_id  - Attribute datatype identifier  (in memory)
!  dims        - Array to hold corresponding dimension sizes of data buffer buf;
!                dim(k) has value of the k-th dimension of buffer buf;
!                values are ignored if buf is a scalar
!  buf 	       - Data buffer; may be a scalar or an array
!
! Outputs:
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
!  dims parameter was added to make code portable;
!  Aprile 4, 2001
!
!  Changed buf intent to INOUT to be consistant
!  with how the C functions handles it. The pg
!  compiler will return 0 if a buf value is not set.
!  February, 2008
!
! NOTES
!  This function is overloaded to write INTEGER,
!  REAL, DOUBLE PRECISION and CHARACTER buffers
!  up to 7 dimensions.
!
! Fortran90 Interface:
!!  SUBROUTINE h5awrite_f(attr_id, memtype_id, buf, dims, hdferr) 
!!    INTEGER(HID_T)  , INTENT(IN)               :: attr_id
!!    INTEGER(HID_T)  , INTENT(IN)               :: memtype_id
!!    TYPE            , INTENT(IN)               :: buf
!!    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
!!    INTEGER         , INTENT(OUT)              :: hdferr
!*****


  SUBROUTINE h5awrite_integer_scalar(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), TARGET :: buf                 ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr                     ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf)

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_integer_scalar

  SUBROUTINE h5awrite_integer_1(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN) , &
         DIMENSION(dims(1)), TARGET :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_integer_1


  SUBROUTINE h5awrite_integer_2(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN) , &
         DIMENSION(dims(1),dims(2)), TARGET :: buf
                                            ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_integer_2

  SUBROUTINE h5awrite_integer_3(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_integer_3


  SUBROUTINE h5awrite_integer_4(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims! Array to story buf dimension sizes
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_integer_4


  SUBROUTINE h5awrite_integer_5(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_integer_5


  SUBROUTINE h5awrite_integer_6(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_integer_6


  SUBROUTINE h5awrite_integer_7(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), &
         TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_integer_7


  SUBROUTINE h5awrite_real_scalar(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), TARGET :: buf          ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf)

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_real_scalar

  SUBROUTINE h5awrite_real_1(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1)), TARGET :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_real_1


  SUBROUTINE h5awrite_real_2(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_real_2


  SUBROUTINE h5awrite_real_3(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_real_3


  SUBROUTINE h5awrite_real_4(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_real_4


  SUBROUTINE h5awrite_real_5(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf)

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_real_5


  SUBROUTINE h5awrite_real_6(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_real_6


  SUBROUTINE h5awrite_real_7(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_real_7

  SUBROUTINE h5awrite_char_scalar(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id               ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id            ! Attribute datatype
                                                        !  identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims  ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN) :: buf                 ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr                      ! Error code

    CALL h5awrite_char_scalar_fix(attr_id, memtype_id, buf, LEN(buf), dims, hdferr)

  END SUBROUTINE h5awrite_char_scalar

  SUBROUTINE h5awrite_char_scalar_fix(attr_id, memtype_id, buf, buf_len, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id               ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id            ! Attribute datatype
                                                        !  identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims  ! Array to story buf dimension sizes
    INTEGER, INTENT(IN)  :: buf_len
    CHARACTER(LEN=buf_len), INTENT(IN), TARGET :: buf   ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr                      ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1:1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_char_scalar_fix

  SUBROUTINE h5awrite_char_1(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1)(1:1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_char_1

  SUBROUTINE h5awrite_char_2(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims  ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1)(1:1))
    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_char_2

  SUBROUTINE h5awrite_char_3(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1)(1:1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_char_3

  SUBROUTINE h5awrite_char_4(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1)(1:1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_char_4

  SUBROUTINE h5awrite_char_5(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1)(1:1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_char_5


  SUBROUTINE h5awrite_char_6(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1)(1:1))

    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_char_6

  SUBROUTINE h5awrite_char_7(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1)(1:1))
    hdferr = h5awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_char_7

!****s* H5A (F03)/h5awrite_f_F03
!
! NAME
!  h5awrite_f_F03
!
! PURPOSE
!  Writes an attribute.
!
! Inputs:
!  attr_id     - Attribute identifier
!  memtype_id  - Attribute datatype identifier  (in memory)
!  buf 	       - Data buffer; may be a scalar or an array
!
! Outputs:
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
! NOTES
!  This function is overloaded to write INTEGER,
!  REAL, DOUBLE PRECISION and CHARACTER buffers
!  up to 7 dimensions.
!
! Fortran2003 Interface:
!!  SUBROUTINE h5awrite_f(attr_id, memtype_id, buf, hdferr) 
!!    INTEGER(HID_T)  , INTENT(IN)  :: attr_id
!!    INTEGER(HID_T)  , INTENT(IN)  :: memtype_id
!!    TYPE(C_PTR)     , INTENT(IN)  :: buf
!!    INTEGER         , INTENT(OUT) :: hdferr
!*****

  SUBROUTINE h5awrite_ptr(attr_id, mem_type_id, buf, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id     ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    TYPE(C_PTR), INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr            ! Error code

    hdferr = h5awrite_f_c(attr_id, mem_type_id, buf)

  END SUBROUTINE h5awrite_ptr

!****s* H5A (F03)/h5aread_f_F90
!
! NAME
!  h5aread_f_F90
!
! PURPOSE
!  Reads an attribute.
!
! Inputs:
!  attr_id     - Attribute identifier
!  memtype_id  - Attribute datatype identifier  (in memory)
!  dims        - Array to hold corresponding dimension sizes of data buffer buf;
!                dim(k) has value of the k-th dimension of buffer buf;
!                values are ignored if buf is a scalar
!
! Outputs:
!  buf 	       - Data buffer; may be a scalar or an array
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
!  dims parameter was added to make code portable;
!  Aprile 4, 2001
!
!  Changed buf intent to INOUT to be consistant
!  with how the C functions handles it. The pg
!  compiler will return 0 if a buf value is not set.
!  February, 2008
!
! NOTES
!  This function is overloaded to write INTEGER,
!  REAL, DOUBLE PRECISION and CHARACTER buffers
!  up to 7 dimensions.
! Fortran90 Interface:
!!  SUBROUTINE h5aread_f(attr_id, memtype_id, buf, dims, hdferr) 
!!    INTEGER(HID_T)  , INTENT(IN)               :: attr_id
!!    INTEGER(HID_T)  , INTENT(IN)               :: memtype_id
!!    TYPE            , INTENT(INOUT)            :: buf
!!    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
!!    INTEGER         , INTENT(OUT)              :: hdferr
!*****
  SUBROUTINE h5aread_integer_scalar(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), TARGET :: buf              ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf)

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_integer_scalar

  SUBROUTINE h5aread_integer_1(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_integer_1


  SUBROUTINE h5aread_integer_2(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT),DIMENSION(dims(1),dims(2)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_integer_2


  SUBROUTINE h5aread_integer_3(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_integer_3


  SUBROUTINE h5aread_integer_4(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_integer_4


  SUBROUTINE h5aread_integer_5(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_integer_5


  SUBROUTINE h5aread_integer_6(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_integer_6


  SUBROUTINE h5aread_integer_7(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_integer_7


  SUBROUTINE h5aread_real_scalar(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), TARGET :: buf                 ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf)

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_real_scalar

  SUBROUTINE h5aread_real_1(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1)), TARGET :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_real_1


  SUBROUTINE h5aread_real_2(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_real_2


  SUBROUTINE h5aread_real_3(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
                                            ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_real_3


  SUBROUTINE h5aread_real_4(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_real_4


  SUBROUTINE h5aread_real_5(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_real_5


  SUBROUTINE h5aread_real_6(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_real_6


  SUBROUTINE h5aread_real_7(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_real_7

  SUBROUTINE h5aread_char_scalar(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr         ! Error code

    CALL h5aread_char_scalar_fix(attr_id, memtype_id, buf, LEN(buf), hdferr)

  END SUBROUTINE h5aread_char_scalar

  SUBROUTINE h5aread_char_scalar_fix(attr_id, memtype_id, buf, buf_len, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER, INTENT(IN)  :: buf_len
    CHARACTER(LEN=buf_len), INTENT(INOUT), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1:1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5aread_char_scalar_fix

  SUBROUTINE h5aread_char_1(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1)), TARGET :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1)(1:1))
    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5aread_char_1


  SUBROUTINE h5aread_char_2(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1)(1:1))
    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5aread_char_2


  SUBROUTINE h5aread_char_3(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1)(1:1))
    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5aread_char_3

  SUBROUTINE h5aread_char_4(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1)(1:1))

    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5aread_char_4

  SUBROUTINE h5aread_char_5(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1)(1:1))
    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5aread_char_5


  SUBROUTINE h5aread_char_6(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1)(1:1))
    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5aread_char_6


  SUBROUTINE h5aread_char_7(attr_id, memtype_id, buf, dims, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1)(1:1))
    hdferr = h5aread_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5aread_char_7


!****s* H5A (F03)/h5aread_f_F03
!
! NAME
!  h5aread_f_F03
!
! PURPOSE
!  Reads an attribute.
!
! Inputs:
!  attr_id     - Attribute identifier
!  memtype_id  - Attribute datatype identifier  (in memory)
!
! Outputs:
!  buf 	       - Data buffer; may be a scalar or an array
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
!  dims parameter was added to make code portable;
!  Aprile 4, 2001
!
!  Changed buf intent to INOUT to be consistant
!  with how the C functions handles it. The pg
!  compiler will return 0 if a buf value is not set.
!  February, 2008
!
! NOTES
!  This function is overloaded to write INTEGER,
!  REAL, DOUBLE PRECISION and CHARACTER buffers
!  up to 7 dimensions.
! Fortran2003 Interface:
!!  SUBROUTINE h5aread_f(attr_id, memtype_id, buf, hdferr) 
!!    INTEGER(HID_T)  , INTENT(IN)    :: attr_id
!!    INTEGER(HID_T)  , INTENT(IN)    :: memtype_id
!!    TYPE(C_PTR)     , INTENT(INOUT) :: buf
!!    INTEGER         , INTENT(OUT)   :: hdferr
!*****

  SUBROUTINE h5aread_ptr(attr_id, mem_type_id, buf, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id     ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    TYPE(C_PTR), INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr            ! Error code

    hdferr = h5aread_f_c(attr_id, mem_type_id, buf)

  END SUBROUTINE h5aread_ptr

END MODULE H5A_PROVISIONAL


