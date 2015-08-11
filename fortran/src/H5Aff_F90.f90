!****h* ROBODoc/H5A (F90)
!
! NAME
!  H5A_PROVISIONAL
!
! FILE
!  fortran/src/H5Aff_F90.f90
!
! PURPOSE
!
!  This file contains Fortran 90 interfaces for H5A functions. It contains
!  the same functions as H5Aff_F03.f90 but excludes the Fortran 2003 functions
!  and the interface listings. This file will be compiled instead of H5Aff_F03.f90
!  if Fortran 2003 functions are not enabled.
!
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
!                         *** IMPORTANT ***
!  If you add a new H5A function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5A_PROVISIONAL

  USE H5GLOBAL
  !
  !On Windows there are no big (integer*8) integers, so overloading
  !for bug #670 does not work. I have to use DEC compilation directives to make
  !Windows DEC Visual Fortran and OSF compilers happy and do right things.
  !						05/01/02 EP
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
  END INTERFACE

CONTAINS

  SUBROUTINE h5awrite_integer_scalar(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims  ! Array to story buf dimension sizes
    INTEGER, INTENT(IN) :: buf              ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    INTERFACE
       INTEGER FUNCTION h5awrite_integer_s_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_S_C'::h5awrite_integer_s_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN)::buf
       END FUNCTION h5awrite_integer_s_c
    END INTERFACE

    hdferr = h5awrite_integer_s_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_integer_scalar

  SUBROUTINE h5awrite_integer_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN) , &
         DIMENSION(dims(1)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr ! Error code
    !            INTEGER, EXTERNAL :: h5awrite_integer_1_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_1_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_1_C'::h5awrite_integer_1_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), DIMENSION(dims(1)) :: buf
       END FUNCTION h5awrite_integer_1_c
    END INTERFACE

    hdferr = h5awrite_integer_1_c(attr_id, memtype_id, buf, dims)

  END SUBROUTINE h5awrite_integer_1


  SUBROUTINE h5awrite_integer_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN) , &
         DIMENSION(dims(1),dims(2)) :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_integer_2_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_2_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_2_C'::h5awrite_integer_2_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5awrite_integer_2_c
    END INTERFACE

    hdferr = h5awrite_integer_2_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_integer_2


  SUBROUTINE h5awrite_integer_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_integer_3_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_3_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_3_C'::h5awrite_integer_3_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5awrite_integer_3_c
    END INTERFACE

    hdferr = h5awrite_integer_3_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_integer_3


  SUBROUTINE h5awrite_integer_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_integer_4_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_4_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_4_C'::h5awrite_integer_4_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5awrite_integer_4_c
    END INTERFACE

    hdferr = h5awrite_integer_4_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_integer_4


  SUBROUTINE h5awrite_integer_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_integer_5_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_5_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_5_C'::h5awrite_integer_5_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5awrite_integer_5_c
    END INTERFACE

    hdferr = h5awrite_integer_5_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_integer_5


  SUBROUTINE h5awrite_integer_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_integer_6_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_6_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_6_C'::h5awrite_integer_6_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5awrite_integer_6_c
    END INTERFACE

    hdferr = h5awrite_integer_6_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_integer_6


  SUBROUTINE h5awrite_integer_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_integer_7_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_7_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_7_C'::h5awrite_integer_7_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5awrite_integer_7_c
    END INTERFACE

    hdferr = h5awrite_integer_7_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_integer_7


  SUBROUTINE h5awrite_real_scalar(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    REAL, INTENT(IN) :: buf                  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_real_s_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_real_s_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_S_C'::h5awrite_real_s_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(IN)::buf
       END FUNCTION h5awrite_real_s_c
    END INTERFACE

    hdferr = h5awrite_real_s_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_real_scalar

  SUBROUTINE h5awrite_real_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1)) :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr  ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_real_1_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_real_1_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_1_C'::h5awrite_real_1_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5awrite_real_1_c
    END INTERFACE

    hdferr = h5awrite_real_1_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_real_1


  SUBROUTINE h5awrite_real_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr         ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_real_2_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_real_2_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_2_C'::h5awrite_real_2_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5awrite_real_2_c
    END INTERFACE

    hdferr = h5awrite_real_2_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_real_2


  SUBROUTINE h5awrite_real_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_real_3_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_real_3_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_3_C'::h5awrite_real_3_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5awrite_real_3_c
    END INTERFACE

    hdferr = h5awrite_real_3_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_real_3


  SUBROUTINE h5awrite_real_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_real_4_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_real_4_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_4_C'::h5awrite_real_4_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5awrite_real_4_c
    END INTERFACE

    hdferr = h5awrite_real_4_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_real_4


  SUBROUTINE h5awrite_real_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_real_5_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_real_5_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_5_C'::h5awrite_real_5_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5awrite_real_5_c
    END INTERFACE

    hdferr = h5awrite_real_5_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_real_5


  SUBROUTINE h5awrite_real_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_real_6_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_real_6_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_6_C'::h5awrite_real_6_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5awrite_real_6_c
    END INTERFACE

    hdferr = h5awrite_real_6_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_real_6


  SUBROUTINE h5awrite_real_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_real_7_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_real_7_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_7_C'::h5awrite_real_7_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5awrite_real_7_c
    END INTERFACE

    hdferr = h5awrite_real_7_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_real_7

  SUBROUTINE h5awrite_char_scalar(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*),INTENT(IN) :: buf
                                             ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    !            INTEGER, EXTERNAL :: h5awritec_s_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awritec_s_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_S_C'::h5awritec_s_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(IN)::buf
       END FUNCTION h5awritec_s_c
    END INTERFACE

    hdferr = h5awritec_s_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_char_scalar

  SUBROUTINE h5awrite_char_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr ! Error code
    !            INTEGER, EXTERNAL :: h5awritec_1_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awritec_1_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_1_C'::h5awritec_1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1))::buf
       END FUNCTION h5awritec_1_c
    END INTERFACE

    hdferr = h5awritec_1_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_char_1


  SUBROUTINE h5awrite_char_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2)) :: buf   ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5awritec_2_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awritec_2_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_2_C'::h5awritec_2_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5awritec_2_c
    END INTERFACE

    hdferr = h5awritec_2_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_char_2


  SUBROUTINE h5awrite_char_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr ! Error code
    !            INTEGER, EXTERNAL :: h5awritec_3_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awritec_3_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_3_C'::h5awritec_3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5awritec_3_c
    END INTERFACE

    hdferr = h5awritec_3_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_char_3


  SUBROUTINE h5awrite_char_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    !            INTEGER, EXTERNAL :: h5awritec_4_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awritec_4_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_4_C'::h5awritec_4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5awritec_4_c
    END INTERFACE

    hdferr = h5awritec_4_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_char_4


  SUBROUTINE h5awrite_char_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    !            INTEGER, EXTERNAL :: h5awritec_5_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awritec_5_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_5_C'::h5awritec_5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5awritec_5_c
    END INTERFACE

    hdferr = h5awritec_5_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_char_5


  SUBROUTINE h5awrite_char_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    !            INTEGER, EXTERNAL :: h5awritec_6_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awritec_6_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_6_C'::h5awritec_6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5awritec_6_c
    END INTERFACE

    hdferr = h5awritec_6_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_char_6


  SUBROUTINE h5awrite_char_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    !            INTEGER, EXTERNAL :: h5awritec_7_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awritec_7_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_7_C'::h5awritec_7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5awritec_7_c
    END INTERFACE

    hdferr = h5awritec_7_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5awrite_char_7

!
! NAME
!  h5aread_f
!
! PURPOSE
!  Reads an attribute.
!
! INPUTS
!		attr_id		- attribute identifier
!		memtype_id	- attribute memory type identifier
!		dims		- 1D array of size 7, stores sizes of the
!				- buf array dimensions.
! OUTPUTS
!		buf		- buffer to read attribute data in
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! OPTIONAL PARAMETERS
!   NONE
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
!  dims parameter was added to make code portable;
!  April 4, 2001
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

  SUBROUTINE h5aread_integer_scalar(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT) :: buf            ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_integer_s_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_integer_s_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_S_C'::h5aread_integer_s_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(INOUT)::buf
       END FUNCTION h5aread_integer_s_c
    END INTERFACE
    hdferr = h5aread_integer_s_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_integer_scalar

  SUBROUTINE h5aread_integer_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), DIMENSION(dims(1)) :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_integer_1_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_integer_1_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_1_C'::h5aread_integer_1_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(INOUT), DIMENSION(dims(1)) :: buf
       END FUNCTION h5aread_integer_1_c
    END INTERFACE

    hdferr = h5aread_integer_1_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_integer_1


  SUBROUTINE h5aread_integer_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT),DIMENSION(dims(1),dims(2)) :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_integer_2_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_integer_2_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_2_C'::h5aread_integer_2_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(INOUT), DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5aread_integer_2_c
    END INTERFACE

    hdferr = h5aread_integer_2_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_integer_2


  SUBROUTINE h5aread_integer_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_integer_3_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_integer_3_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_3_C'::h5aread_integer_3_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5aread_integer_3_c
    END INTERFACE

    hdferr = h5aread_integer_3_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_integer_3


  SUBROUTINE h5aread_integer_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf ! Attribute data
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_integer_4_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_integer_4_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_4_C'::h5aread_integer_4_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5aread_integer_4_c
    END INTERFACE

    hdferr = h5aread_integer_4_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_integer_4


  SUBROUTINE h5aread_integer_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5aread_integer_5_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_integer_5_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_5_C'::h5aread_integer_5_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5aread_integer_5_c
    END INTERFACE

    hdferr = h5aread_integer_5_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_integer_5


  SUBROUTINE h5aread_integer_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_integer_6_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_integer_6_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_6_C'::h5aread_integer_6_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5aread_integer_6_c
    END INTERFACE

    hdferr = h5aread_integer_6_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_integer_6


  SUBROUTINE h5aread_integer_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_integer_7_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_integer_7_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_7_C'::h5aread_integer_7_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5aread_integer_7_c
    END INTERFACE

    hdferr = h5aread_integer_7_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_integer_7


  SUBROUTINE h5aread_real_scalar(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT) :: buf               ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_real_s_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_real_s_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_S_C'::h5aread_real_s_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(INOUT)::buf
       END FUNCTION h5aread_real_s_c
    END INTERFACE

    hdferr = h5aread_real_s_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_real_scalar

  SUBROUTINE h5aread_real_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5aread_real_1_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_real_1_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_1_C'::h5aread_real_1_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5aread_real_1_c
    END INTERFACE

    hdferr = h5aread_real_1_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_real_1


  SUBROUTINE h5aread_real_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5aread_real_2_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_real_2_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_2_C'::h5aread_real_2_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5aread_real_2_c
    END INTERFACE

    hdferr = h5aread_real_2_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_real_2


  SUBROUTINE h5aread_real_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf  ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr                  ! Error code

    !            INTEGER, EXTERNAL :: h5aread_real_3_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_real_3_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_3_C'::h5aread_real_3_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5aread_real_3_c
    END INTERFACE

    hdferr = h5aread_real_3_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_real_3


  SUBROUTINE h5aread_real_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_real_4_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_real_4_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_4_C'::h5aread_real_4_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5aread_real_4_c
    END INTERFACE

    hdferr = h5aread_real_4_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_real_4


  SUBROUTINE h5aread_real_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_real_5_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_real_5_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_5_C'::h5aread_real_5_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5aread_real_5_c
    END INTERFACE

    hdferr = h5aread_real_5_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_real_5


  SUBROUTINE h5aread_real_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_real_6_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_real_6_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_6_C'::h5aread_real_6_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5aread_real_6_c
    END INTERFACE

    hdferr = h5aread_real_6_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_real_6


  SUBROUTINE h5aread_real_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5aread_real_7_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_real_7_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_7_C'::h5aread_real_7_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5aread_real_7_c
    END INTERFACE

    hdferr = h5aread_real_7_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_real_7

  SUBROUTINE h5aread_char_scalar(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5areadc_s_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5areadc_s_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_S_C'::h5areadc_s_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(INOUT) :: buf
       END FUNCTION h5areadc_s_c
    END INTERFACE

    hdferr = h5areadc_s_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_char_scalar

  SUBROUTINE h5aread_char_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr ! Error code

    !            INTEGER, EXTERNAL :: h5areadc_1_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5areadc_1_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_1_C'::h5areadc_1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5areadc_1_c
    END INTERFACE

    hdferr = h5areadc_1_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_char_1


  SUBROUTINE h5aread_char_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr ! Error code

    !            INTEGER, EXTERNAL :: h5areadc_2_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5areadc_2_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_2_C'::h5areadc_2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5areadc_2_c
    END INTERFACE

    hdferr = h5areadc_2_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_char_2


  SUBROUTINE h5aread_char_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5areadc_3_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5areadc_3_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_3_C'::h5areadc_3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5areadc_3_c
    END INTERFACE

    hdferr = h5areadc_3_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_char_3


  SUBROUTINE h5aread_char_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5areadc_4_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5areadc_4_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_4_C'::h5areadc_4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5areadc_4_c
    END INTERFACE

    hdferr = h5areadc_4_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_char_4


  SUBROUTINE h5aread_char_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5areadc_5_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5areadc_5_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_5_C'::h5areadc_5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5areadc_5_c
    END INTERFACE

    hdferr = h5areadc_5_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_char_5


  SUBROUTINE h5aread_char_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5areadc_6_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5areadc_6_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_6_C'::h5areadc_6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5areadc_6_c
    END INTERFACE

    hdferr = h5areadc_6_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_char_6


  SUBROUTINE h5aread_char_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code

    !            INTEGER, EXTERNAL :: h5areadc_7_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5areadc_7_c(attr_id, memtype_id, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_7_C'::h5areadc_7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5areadc_7_c
    END INTERFACE

    hdferr = h5areadc_7_c(attr_id, memtype_id, buf, dims)
  END SUBROUTINE h5aread_char_7

END MODULE H5A_PROVISIONAL


