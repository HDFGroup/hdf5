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
!
! This file contains FORTRAN interfaces for H5LT functions
!
! NOTES
!
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!                             
!  If you add a new function here then you MUST add the function name to the
!  Windows dll file 'hdf5_hl_fortrandll.def.in' in the hl/fortran/src directory.
!  This is needed for Windows based operating systems.
!

#include "H5config_f.inc"
MODULE h5lt
  USE, INTRINSIC :: ISO_C_BINDING
  USE h5fortran_types
  USE hdf5

  INTERFACE h5ltmake_dataset_f
     MODULE PROCEDURE h5ltmake_dataset_f_int1
     MODULE PROCEDURE h5ltmake_dataset_f_int2
     MODULE PROCEDURE h5ltmake_dataset_f_int3
     MODULE PROCEDURE h5ltmake_dataset_f_int4
     MODULE PROCEDURE h5ltmake_dataset_f_int5
     MODULE PROCEDURE h5ltmake_dataset_f_int6
     MODULE PROCEDURE h5ltmake_dataset_f_int7
     MODULE PROCEDURE h5ltmake_dataset_f_c_float1
     MODULE PROCEDURE h5ltmake_dataset_f_c_float2
     MODULE PROCEDURE h5ltmake_dataset_f_c_float3
     MODULE PROCEDURE h5ltmake_dataset_f_c_float4
     MODULE PROCEDURE h5ltmake_dataset_f_c_float5
     MODULE PROCEDURE h5ltmake_dataset_f_c_float6
     MODULE PROCEDURE h5ltmake_dataset_f_c_float7
     MODULE PROCEDURE h5ltmake_dataset_f_c_double1
     MODULE PROCEDURE h5ltmake_dataset_f_c_double2
     MODULE PROCEDURE h5ltmake_dataset_f_c_double3
     MODULE PROCEDURE h5ltmake_dataset_f_c_double4
     MODULE PROCEDURE h5ltmake_dataset_f_c_double5
     MODULE PROCEDURE h5ltmake_dataset_f_c_double6
     MODULE PROCEDURE h5ltmake_dataset_f_c_double7
#ifdef H5_FORTRAN_HAVE_C_LONG_DOUBLE
     MODULE PROCEDURE h5ltmake_dataset_f_c_long_double1
     MODULE PROCEDURE h5ltmake_dataset_f_c_long_double2
     MODULE PROCEDURE h5ltmake_dataset_f_c_long_double3
     MODULE PROCEDURE h5ltmake_dataset_f_c_long_double4
     MODULE PROCEDURE h5ltmake_dataset_f_c_long_double5
     MODULE PROCEDURE h5ltmake_dataset_f_c_long_double6
     MODULE PROCEDURE h5ltmake_dataset_f_c_long_double7
#endif
  END INTERFACE

  INTERFACE h5ltread_dataset_f
     MODULE PROCEDURE h5ltread_dataset_f_int1
     MODULE PROCEDURE h5ltread_dataset_f_int2
     MODULE PROCEDURE h5ltread_dataset_f_int3
     MODULE PROCEDURE h5ltread_dataset_f_int4
     MODULE PROCEDURE h5ltread_dataset_f_int5
     MODULE PROCEDURE h5ltread_dataset_f_int6
     MODULE PROCEDURE h5ltread_dataset_f_int7
     MODULE PROCEDURE h5ltread_dataset_f_c_float1
     MODULE PROCEDURE h5ltread_dataset_f_c_float2
     MODULE PROCEDURE h5ltread_dataset_f_c_float3
     MODULE PROCEDURE h5ltread_dataset_f_c_float4
     MODULE PROCEDURE h5ltread_dataset_f_c_float5
     MODULE PROCEDURE h5ltread_dataset_f_c_float6
     MODULE PROCEDURE h5ltread_dataset_f_c_float7
     MODULE PROCEDURE h5ltread_dataset_f_c_double1
     MODULE PROCEDURE h5ltread_dataset_f_c_double2
     MODULE PROCEDURE h5ltread_dataset_f_c_double3
     MODULE PROCEDURE h5ltread_dataset_f_c_double4
     MODULE PROCEDURE h5ltread_dataset_f_c_double5
     MODULE PROCEDURE h5ltread_dataset_f_c_double6
     MODULE PROCEDURE h5ltread_dataset_f_c_double7
#ifdef H5_FORTRAN_HAVE_C_LONG_DOUBLE
     MODULE PROCEDURE h5ltread_dataset_f_c_long_double1
     MODULE PROCEDURE h5ltread_dataset_f_c_long_double2
     MODULE PROCEDURE h5ltread_dataset_f_c_long_double3
     MODULE PROCEDURE h5ltread_dataset_f_c_long_double4
     MODULE PROCEDURE h5ltread_dataset_f_c_long_double5
     MODULE PROCEDURE h5ltread_dataset_f_c_long_double6
     MODULE PROCEDURE h5ltread_dataset_f_c_long_double7
#endif
  END INTERFACE

  INTERFACE h5ltmake_dataset_int_f
     MODULE PROCEDURE h5ltmake_dataset_int_f_1
     MODULE PROCEDURE h5ltmake_dataset_int_f_2
     MODULE PROCEDURE h5ltmake_dataset_int_f_3
     MODULE PROCEDURE h5ltmake_dataset_int_f_4
     MODULE PROCEDURE h5ltmake_dataset_int_f_5
     MODULE PROCEDURE h5ltmake_dataset_int_f_6
     MODULE PROCEDURE h5ltmake_dataset_int_f_7
  END INTERFACE

  INTERFACE h5ltmake_dataset_float_f
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_1
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_2
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_3
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_4
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_5
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_6
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_7
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_1
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_2
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_3
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_4
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_5
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_6
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_7
#ifdef H5_FORTRAN_HAVE_C_LONG_DOUBLE
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_1
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_2
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_3
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_4
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_5
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_6
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_7
#endif
  END INTERFACE

  INTERFACE h5ltmake_dataset_double_f
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_1
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_2
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_3
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_4
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_5
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_6
     MODULE PROCEDURE h5ltmake_dataset_c_float_f_7
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_1
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_2
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_3
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_4
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_5
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_6
     MODULE PROCEDURE h5ltmake_dataset_c_double_f_7
#ifdef H5_FORTRAN_HAVE_C_LONG_DOUBLE
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_1
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_2
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_3
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_4
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_5
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_6
     MODULE PROCEDURE h5ltmake_dataset_c_long_double_f_7
#endif
  END INTERFACE

  INTERFACE h5ltread_dataset_int_f
     MODULE PROCEDURE h5ltread_dataset_int_f_1
     MODULE PROCEDURE h5ltread_dataset_int_f_2
     MODULE PROCEDURE h5ltread_dataset_int_f_3
     MODULE PROCEDURE h5ltread_dataset_int_f_4
     MODULE PROCEDURE h5ltread_dataset_int_f_5
     MODULE PROCEDURE h5ltread_dataset_int_f_6
     MODULE PROCEDURE h5ltread_dataset_int_f_7
  END INTERFACE

  INTERFACE h5ltread_dataset_float_f
     MODULE PROCEDURE h5ltread_dataset_c_float_f_1
     MODULE PROCEDURE h5ltread_dataset_c_float_f_2
     MODULE PROCEDURE h5ltread_dataset_c_float_f_3
     MODULE PROCEDURE h5ltread_dataset_c_float_f_4
     MODULE PROCEDURE h5ltread_dataset_c_float_f_5
     MODULE PROCEDURE h5ltread_dataset_c_float_f_6
     MODULE PROCEDURE h5ltread_dataset_c_float_f_7
     MODULE PROCEDURE h5ltread_dataset_c_double_f_1
     MODULE PROCEDURE h5ltread_dataset_c_double_f_2
     MODULE PROCEDURE h5ltread_dataset_c_double_f_3
     MODULE PROCEDURE h5ltread_dataset_c_double_f_4
     MODULE PROCEDURE h5ltread_dataset_c_double_f_5
     MODULE PROCEDURE h5ltread_dataset_c_double_f_6
     MODULE PROCEDURE h5ltread_dataset_c_double_f_7
#ifdef H5_FORTRAN_HAVE_C_LONG_DOUBLE
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_1
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_2
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_3
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_4
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_5
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_6
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_7
#endif
  END INTERFACE

  INTERFACE h5ltread_dataset_double_f
     MODULE PROCEDURE h5ltread_dataset_c_float_f_1
     MODULE PROCEDURE h5ltread_dataset_c_float_f_2
     MODULE PROCEDURE h5ltread_dataset_c_float_f_3
     MODULE PROCEDURE h5ltread_dataset_c_float_f_4
     MODULE PROCEDURE h5ltread_dataset_c_float_f_5
     MODULE PROCEDURE h5ltread_dataset_c_float_f_6
     MODULE PROCEDURE h5ltread_dataset_c_float_f_7
     MODULE PROCEDURE h5ltread_dataset_c_double_f_1
     MODULE PROCEDURE h5ltread_dataset_c_double_f_2
     MODULE PROCEDURE h5ltread_dataset_c_double_f_3
     MODULE PROCEDURE h5ltread_dataset_c_double_f_4
     MODULE PROCEDURE h5ltread_dataset_c_double_f_5
     MODULE PROCEDURE h5ltread_dataset_c_double_f_6
     MODULE PROCEDURE h5ltread_dataset_c_double_f_7
#ifdef H5_FORTRAN_HAVE_C_LONG_DOUBLE
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_1
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_2
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_3
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_4
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_5
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_6
     MODULE PROCEDURE h5ltread_dataset_c_long_double_f_7
#endif
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf) &
          BIND(C,NAME='h5ltmake_dataset_c')
       IMPORT :: C_CHAR, C_PTR
       IMPORT :: HID_T, SIZE_T, HSIZE_T
       IMPLICIT NONE
       INTEGER(hid_t),   INTENT(in) :: loc_id                        ! file or group identifier
       INTEGER(hid_t),   INTENT(in) :: type_id                       ! datatype identifier
       INTEGER(size_t) :: namelen                                    ! length of name buffer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name ! name of the dataset
       INTEGER,          INTENT(in) :: rank                          ! rank
       INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims            ! size of the buffer buf
       TYPE(C_PTR), VALUE :: buf                                     ! data buffer
     END FUNCTION h5ltmake_dataset_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims) &
          BIND(C,NAME='h5ltread_dataset_c')
       IMPORT :: C_CHAR, C_PTR
       IMPORT :: HID_T, SIZE_T, HSIZE_T
       IMPLICIT NONE
       INTEGER(hid_t),   INTENT(in) :: loc_id                        ! file or group identifier
       INTEGER(hid_t),   INTENT(in) :: type_id                       ! datatype identifier
       INTEGER(size_t) :: namelen                                    ! length of name buffer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name ! name of the dataset
       INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims            ! size of the buffer buf
       TYPE(C_PTR), VALUE :: buf                                     ! data buffer
     END FUNCTION h5ltread_dataset_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5ltset_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf,dtype) &
          BIND(C,NAME='h5ltset_attribute_c')
       IMPORT :: C_CHAR, C_PTR
       IMPORT :: HID_T, SIZE_T, HSIZE_T
       IMPLICIT NONE
       INTEGER(hid_t),   INTENT(in) :: loc_id                        ! file or group identifier
       INTEGER(size_t) :: namelen                                    ! length of name buffer
       INTEGER(size_t) :: attrlen                                    ! length of attr name buffer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name ! name of the dataset
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: attr_name ! name of the attribute
       INTEGER(size_t),  INTENT(in) :: size                          ! size of attribute array
       TYPE(C_PTR), VALUE :: buf                                     ! data buffer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dtype     ! flag indicating the datatype of the
                                                                     ! the buffer:
                                                                     ! R=Real, D=DOUBLE, I=Interger, C=Character
     END FUNCTION h5ltset_attribute_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5ltget_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,buf,dtype) &
          BIND(C,NAME='h5ltget_attribute_c')
       IMPORT :: C_CHAR, C_PTR
       IMPORT :: HID_T, SIZE_T, HSIZE_T
       IMPLICIT NONE
       INTEGER(hid_t),   INTENT(in) :: loc_id                        ! file or group identifier
       INTEGER(size_t) :: namelen                                    ! length of name buffer
       INTEGER(size_t) :: attrlen                                    ! length of attr name buffer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name ! name of the dataset
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: attr_name ! name of the attribute
       TYPE(C_PTR), VALUE :: buf                                     ! data buffer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dtype     ! flag indicating the datatype of the
                                                                     ! the buffer:
                                                                     ! R=Real, D=DOUBLE, I=Interger, C=Character
     END FUNCTION h5ltget_attribute_c
  END INTERFACE

CONTAINS
  !-------------------------------------------------------------------------
  ! Make/Read dataset functions
  !-------------------------------------------------------------------------

  !-------------------------------------------------------------------------
  ! Function(s): h5ltmake_dataset_f_int(1-7)
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications: Changed to passing C_PTR.
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_int1(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf   ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int1

  SUBROUTINE h5ltmake_dataset_f_int2(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf     ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int2

  SUBROUTINE h5ltmake_dataset_f_int3(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf          ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int3

  SUBROUTINE h5ltmake_dataset_f_int4(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf  ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int4

  SUBROUTINE h5ltmake_dataset_f_int5(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf  ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int5

  SUBROUTINE h5ltmake_dataset_f_int6(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf  ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int6

  SUBROUTINE h5ltmake_dataset_f_int7(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )
    
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int7

  !-------------------------------------------------------------------------
  ! Function(s): h5ltmake_dataset_f_c_float(1-7)
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_c_float1(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    REAL(KIND=C_FLOAT), INTENT(in), DIMENSION(*), TARGET :: buf              ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_float1

  SUBROUTINE h5ltmake_dataset_f_c_float2(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_float2

  SUBROUTINE h5ltmake_dataset_f_c_float3(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_float3

  SUBROUTINE h5ltmake_dataset_f_c_float4(loc_id, dset_name, rank, dims,&
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_float4

  SUBROUTINE h5ltmake_dataset_f_c_float5(loc_id, dset_name, rank, dims,&
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_float5

  SUBROUTINE h5ltmake_dataset_f_c_float6(loc_id, dset_name, rank, dims,&
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_float6

  SUBROUTINE h5ltmake_dataset_f_c_float7(loc_id, dset_name, rank, dims,&
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_float7

  !-------------------------------------------------------------------------
  ! Function(s): h5ltmake_dataset_f_c_double(1-7)
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_c_double1(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    REAL(KIND=C_DOUBLE), INTENT(in), DIMENSION(*), TARGET :: buf ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_double1

  SUBROUTINE h5ltmake_dataset_f_c_double2(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_double2

  SUBROUTINE h5ltmake_dataset_f_c_double3(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_double3

  SUBROUTINE h5ltmake_dataset_f_c_double4(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3), dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_double4

  SUBROUTINE h5ltmake_dataset_f_c_double5(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_double5

  SUBROUTINE h5ltmake_dataset_f_c_double6(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )
    
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_double6

  SUBROUTINE h5ltmake_dataset_f_c_double7(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_double7

#ifdef H5_FORTRAN_HAVE_C_LONG_DOUBLE
  !-------------------------------------------------------------------------
  ! Function(s): h5ltmake_dataset_f_c_long_double(1-7)
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_c_long_double1(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), DIMENSION(*), TARGET :: buf ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length   
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_long_double1

  SUBROUTINE h5ltmake_dataset_f_c_long_double2(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_long_double2

  SUBROUTINE h5ltmake_dataset_f_c_long_double3(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_long_double3

  SUBROUTINE h5ltmake_dataset_f_c_long_double4(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3), dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_long_double4

  SUBROUTINE h5ltmake_dataset_f_c_long_double5(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_long_double5
  SUBROUTINE h5ltmake_dataset_f_c_long_double6(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )
    
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_long_double6

  SUBROUTINE h5ltmake_dataset_f_c_long_double7(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1)) 
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_c_long_double7

#endif

  !-------------------------------------------------------------------------
  ! Function(s): h5ltread_dataset_f_int(1-7)
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_int1(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id              ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name           ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id             ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims  ! size of the buffer buf
    INTEGER, INTENT(inout), DIMENSION(*), TARGET :: buf ! data buffer
    INTEGER :: errcode                                  ! error code
    INTEGER(size_t) :: namelen            
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_int1

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_int2
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_int2(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_int2

  SUBROUTINE h5ltread_dataset_f_int3(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_int3

  SUBROUTINE h5ltread_dataset_f_int4(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_int4

  SUBROUTINE h5ltread_dataset_f_int5(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_int5

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_int6
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 12, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_int6(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_int6

  SUBROUTINE h5ltread_dataset_f_int7(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_int7


  !-------------------------------------------------------------------------
  ! Function(s): h5ltread_dataset_f_c_float(1-7)
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_c_float1(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    REAL(KIND=C_FLOAT), INTENT(inout), DIMENSION(*), TARGET :: buf          ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_float1

  SUBROUTINE h5ltread_dataset_f_c_float2(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_float2

  SUBROUTINE h5ltread_dataset_f_c_float3(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_float3

  SUBROUTINE h5ltread_dataset_f_c_float4(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_float4

  SUBROUTINE h5ltread_dataset_f_c_float5(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_float5

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_c_float6
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_c_float6(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_float6

  SUBROUTINE h5ltread_dataset_f_c_float7(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_float7


  !-------------------------------------------------------------------------
  ! Function(s): h5ltread_dataset_f_c_double(1-7)
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_c_double1(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    REAL(KIND=C_DOUBLE), INTENT(inout), DIMENSION(*), TARGET :: buf! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length                
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_double1

  SUBROUTINE h5ltread_dataset_f_c_double2(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_double2

  SUBROUTINE h5ltread_dataset_f_c_double3(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_double3

  SUBROUTINE h5ltread_dataset_f_c_double4(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_double4

  SUBROUTINE h5ltread_dataset_f_c_double5(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_double5

  SUBROUTINE h5ltread_dataset_f_c_double6(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_double6

  SUBROUTINE h5ltread_dataset_f_c_double7(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_double7

#ifdef H5_FORTRAN_HAVE_C_LONG_DOUBLE
  !-------------------------------------------------------------------------
  ! Function(s): h5ltread_dataset_f_c_long_double(1-7)
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_c_long_double1(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), DIMENSION(*), TARGET :: buf! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length                
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_long_double1

  SUBROUTINE h5ltread_dataset_f_c_long_double2(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_long_double2

  SUBROUTINE h5ltread_dataset_f_c_long_double3(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_long_double3

  SUBROUTINE h5ltread_dataset_f_c_long_double4(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_long_double4

  SUBROUTINE h5ltread_dataset_f_c_long_double5(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_long_double5

  SUBROUTINE h5ltread_dataset_f_c_long_double6(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_long_double6

  SUBROUTINE h5ltread_dataset_f_c_long_double7(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_f_c_long_double7
#endif

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_int_f_1
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_int_f_1 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf   ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,h5t_native_integer,f_ptr)

  END SUBROUTINE h5ltmake_dataset_int_f_1

  SUBROUTINE h5ltmake_dataset_int_f_2 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltmake_dataset_int_f_2

  SUBROUTINE h5ltmake_dataset_int_f_3 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltmake_dataset_int_f_3

  SUBROUTINE h5ltmake_dataset_int_f_4(loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltmake_dataset_int_f_4

  SUBROUTINE h5ltmake_dataset_int_f_5(loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltmake_dataset_int_f_5

  SUBROUTINE h5ltmake_dataset_int_f_6(loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltmake_dataset_int_f_6

  SUBROUTINE h5ltmake_dataset_int_f_7(loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltmake_dataset_int_f_7

  !-------------------------------------------------------------------------
  ! Function(s): h5ltmake_dataset_c_float_f_(1-7)
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_REAL type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_c_float_f_1 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    REAL(KIND=C_FLOAT), INTENT(in), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length             
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_float_f_1

  SUBROUTINE h5ltmake_dataset_c_float_f_2 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_float_f_2

  SUBROUTINE h5ltmake_dataset_c_float_f_3 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_float_f_3

  SUBROUTINE h5ltmake_dataset_c_float_f_4 (loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_float_f_4

  SUBROUTINE h5ltmake_dataset_c_float_f_5 (loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_float_f_5

  SUBROUTINE h5ltmake_dataset_c_float_f_6 (loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_float_f_6

  SUBROUTINE h5ltmake_dataset_c_float_f_7 (loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_float_f_7

  !-------------------------------------------------------------------------
  ! Function9s): h5ltmake_dataset_c_double_f_(1-7)
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_c_double_f_1 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1)), TARGET :: buf                         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_double_f_1

  SUBROUTINE h5ltmake_dataset_c_double_f_2 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_double_f_2

  SUBROUTINE h5ltmake_dataset_c_double_f_3 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_double_f_3

  SUBROUTINE h5ltmake_dataset_c_double_f_4 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_double_f_4

  SUBROUTINE h5ltmake_dataset_c_double_f_5 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_double_f_5

  SUBROUTINE h5ltmake_dataset_c_double_f_6 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_double_f_6

  SUBROUTINE h5ltmake_dataset_c_double_f_7 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_double_f_7

#ifdef H5_FORTRAN_HAVE_C_LONG_DOUBLE
  !-------------------------------------------------------------------------
  ! Function(s): h5ltmake_dataset_c_long_double_f_(1-7)
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_c_long_double_f_1 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1)), TARGET :: buf                         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_long_double_f_1

  SUBROUTINE h5ltmake_dataset_c_long_double_f_2 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_long_double_f_2

  SUBROUTINE h5ltmake_dataset_c_long_double_f_3 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_long_double_f_3

  SUBROUTINE h5ltmake_dataset_c_long_double_f_4 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_long_double_f_4

  SUBROUTINE h5ltmake_dataset_c_long_double_f_5 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_long_double_f_5

  SUBROUTINE h5ltmake_dataset_c_long_double_f_6 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_long_double_f_6

  SUBROUTINE h5ltmake_dataset_c_long_double_f_7 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)

  END SUBROUTINE h5ltmake_dataset_c_long_double_f_7
#endif

  !-------------------------------------------------------------------------
  ! Function(s): h5ltread_dataset_int_f_(1-7)
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_int_f_1(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(HID_T),   INTENT(IN) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1)), TARGET :: buf                         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_int_f_1

  SUBROUTINE h5ltread_dataset_int_f_2(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_int_f_2

  SUBROUTINE h5ltread_dataset_int_f_3(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_int_f_3

  SUBROUTINE h5ltread_dataset_int_f_4(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_int_f_4

  SUBROUTINE h5ltread_dataset_int_f_5(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_int_f_5

  SUBROUTINE h5ltread_dataset_int_f_6(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_int_f_6

  SUBROUTINE h5ltread_dataset_int_f_7(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_int_f_7


  !-------------------------------------------------------------------------
  ! Function(s): h5ltread_dataset_c_float_f_(1-7)
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_c_float_f_1(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1)), TARGET :: buf                         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_float_f_1

  SUBROUTINE h5ltread_dataset_c_float_f_2(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_float_f_2

  SUBROUTINE h5ltread_dataset_c_float_f_3(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_float_f_3

  SUBROUTINE h5ltread_dataset_c_float_f_4(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_float_f_4

  SUBROUTINE h5ltread_dataset_c_float_f_5(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_float_f_5

  SUBROUTINE h5ltread_dataset_c_float_f_6(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_float_f_6

  SUBROUTINE h5ltread_dataset_c_float_f_7(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_FLOAT), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_float_f_7

  !-------------------------------------------------------------------------
  ! Function(s): h5ltread_dataset_c_double_f_(1-7)
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_c_double_f_1(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1)), TARGET :: buf                         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_double_f_1

  SUBROUTINE h5ltread_dataset_c_double_f_2(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_double_f_2

  SUBROUTINE h5ltread_dataset_c_double_f_3(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_double_f_3

  SUBROUTINE h5ltread_dataset_c_double_f_4(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_double_f_4

  SUBROUTINE h5ltread_dataset_c_double_f_5(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_double_f_5

  SUBROUTINE h5ltread_dataset_c_double_f_6(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_double_f_6

  SUBROUTINE h5ltread_dataset_c_double_f_7(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length
    REAL(KIND=C_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_double_f_7

#ifdef H5_FORTRAN_HAVE_C_LONG_DOUBLE
  !-------------------------------------------------------------------------
  ! Function9s): h5ltread_dataset_c_long_double_f_(1-7)
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_c_long_double_f_1(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1)), TARGET :: buf                         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_long_double_f_1

  SUBROUTINE h5ltread_dataset_c_long_double_f_2(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf                 
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_long_double_f_2

  SUBROUTINE h5ltread_dataset_c_long_double_f_3(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_long_double_f_3

  SUBROUTINE h5ltread_dataset_c_long_double_f_4(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_long_double_f_4

  SUBROUTINE h5ltread_dataset_c_long_double_f_5(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_long_double_f_5

  SUBROUTINE h5ltread_dataset_c_long_double_f_6(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_long_double_f_6

  SUBROUTINE h5ltread_dataset_c_long_double_f_7(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the buffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length
    REAL(KIND=C_LONG_DOUBLE), INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf         
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr,dims)

  END SUBROUTINE h5ltread_dataset_c_long_double_f_7
#endif

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_string_f
  !
  ! Purpose: Creates and writes a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_string_f(loc_id,&
       dset_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: buf                ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length
    INTEGER(size_t) :: buflen                          ! buffer length

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_string_c(loc_id,namelen,dset_name,buflen,buf) &
            BIND(C,NAME='h5ltmake_dataset_string_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                          ! file or group identifier
         INTEGER(size_t) :: namelen                                      ! length of name buffer
         INTEGER(size_t) :: buflen                                       ! length of data buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name   ! name of the dataset
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: buf         ! data buffer
       END FUNCTION h5ltmake_dataset_string_c
    END INTERFACE

    namelen = LEN(dset_name)
    buflen = LEN(buf)
    errcode = h5ltmake_dataset_string_c(loc_id,namelen,dset_name,buflen,buf)

  END SUBROUTINE h5ltmake_dataset_string_f

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_string_f
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_string_f(loc_id,&
       dset_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(inout) :: buf             ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_string_c(loc_id,namelen,dset_name,buf) &
            BIND(C,NAME='h5ltread_dataset_string_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
         INTEGER(size_t) :: namelen                                       ! length of name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in)    :: dset_name ! name of the dataset
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(inout) :: buf       ! data buffer
       END FUNCTION h5ltread_dataset_string_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_string_c(loc_id,namelen,dset_name,buf)

  END SUBROUTINE h5ltread_dataset_string_f

  !-------------------------------------------------------------------------
  ! Make/Read attribute functions
  !-------------------------------------------------------------------------


  !-------------------------------------------------------------------------
  ! Function: h5ltset_attribute_int_f
  !
  ! Purpose: Create and write an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltset_attribute_int_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       size,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER(size_t),  INTENT(in) :: size               ! size of attribute array
    INTEGER :: errcode                                 ! error code
    INTEGER, DIMENSION(*), TARGET :: buf   ! data buffer
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER(size_t) :: attrlen                                 ! name length          
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1:1))

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltset_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,size,f_ptr,'I'//C_NULL_CHAR)

  END SUBROUTINE h5ltset_attribute_int_f

  !-------------------------------------------------------------------------
  ! Function: h5ltset_attribute_float_f
  !
  ! Purpose: Create and write an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltset_attribute_float_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       size,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER(size_t),  INTENT(in) :: size               ! size of attribute array
    INTEGER :: errcode                                 ! error code
    REAL(KIND=C_FLOAT), INTENT(in), DIMENSION(*), TARGET :: buf ! data buffer
    INTEGER(size_t) :: namelen                         ! name length
    INTEGER(size_t) :: attrlen                         ! name length             
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltset_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,size,f_ptr,'R'//C_NULL_CHAR)

  END SUBROUTINE h5ltset_attribute_float_f

  !-------------------------------------------------------------------------
  ! Function: h5ltset_attribute_double_f
  !
  ! Purpose: Create and write an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltset_attribute_double_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       size,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER(size_t),  INTENT(in) :: size               ! size of attribute array
    INTEGER :: errcode                                 ! error code
    REAL(KIND=C_DOUBLE), INTENT(in), DIMENSION(*), TARGET :: buf  ! data buffer
    INTEGER(size_t) :: namelen                         ! name length
    INTEGER(size_t) :: attrlen                         ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1)) 

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltset_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,size,f_ptr,'D'//C_NULL_CHAR)

  END SUBROUTINE h5ltset_attribute_double_f


  !-------------------------------------------------------------------------
  ! Function: h5ltset_attribute_string_f
  !
  ! Purpose: Create and write an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltset_attribute_string_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER :: errcode                                 ! error code
    CHARACTER(LEN=*), DIMENSION(*), INTENT(in), TARGET :: buf    ! data buffer
    INTEGER(size_t) :: namelen                         ! name length
    INTEGER(size_t) :: attrlen                         ! name length
    INTEGER(size_t) :: buflen                          ! data buffer length           
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1)(1:1))

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    buflen = LEN(buf)
    errcode = h5ltset_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,buflen,f_ptr,'C'//C_NULL_CHAR)

  END SUBROUTINE h5ltset_attribute_string_f

  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_int_f
  !
  ! Purpose: Reads an attribute named ATTR_NAME
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_int_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER :: errcode                                 ! error code
    INTEGER, INTENT(inout), DIMENSION(*), TARGET :: buf! data buffer
    INTEGER(size_t) :: namelen                         ! name length
    INTEGER(size_t) :: attrlen                         ! name length       
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))    

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,f_ptr,'I'//C_NULL_CHAR)

  END SUBROUTINE h5ltget_attribute_int_f


  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_float_f
  !
  ! Purpose: Reads an attribute named ATTR_NAME
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_float_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER :: errcode                                 ! error code
    REAL(KIND=C_FLOAT), INTENT(inout), DIMENSION(*), TARGET :: buf
    INTEGER(size_t) :: namelen                         ! name length
    INTEGER(size_t) :: attrlen                         ! name length          
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,f_ptr,'R'//C_NULL_CHAR)

  END SUBROUTINE h5ltget_attribute_float_f

  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_c_double_f
  !
  ! Purpose: Reads an attribute named ATTR_NAME
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_double_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER :: errcode                                 ! error code
    REAL(KIND=C_DOUBLE),INTENT(inout),DIMENSION(*), TARGET :: buf
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER(size_t) :: attrlen                                 ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,f_ptr,'D'//C_NULL_CHAR)

  END SUBROUTINE h5ltget_attribute_double_f

  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_string_f
  !
  ! Purpose: Reads an attribute named ATTR_NAME
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_string_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER :: errcode                                 ! error code
    CHARACTER(LEN=*), INTENT(inout) :: buf
    INTEGER(size_t) :: namelen                         ! name length
    INTEGER(size_t) :: attrlen                         ! name length
    INTEGER(size_t) :: buf_size                        ! buf size 

   INTERFACE
       INTEGER FUNCTION h5ltget_attribute_string_c(loc_id,namelen,dset_name,attrlen,attr_name,buf,buf_size) &
            BIND(C,NAME='h5ltget_attribute_string_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                                      ! length of name buffer
         INTEGER(size_t) :: attrlen                                      ! length of attr name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: attr_name               ! name of the attribute
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(inout) :: buf                  ! data buffer
         INTEGER(size_t) :: buf_size                 ! data buffer size
       END FUNCTION h5ltget_attribute_string_c
    END INTERFACE

    namelen  = LEN(dset_name)
    attrlen  = LEN(attr_name)
    buf_size = LEN(buf)

    errcode = h5ltget_attribute_string_c(loc_id,namelen,dset_name,attrlen,attr_name,buf,buf_size)

  END SUBROUTINE h5ltget_attribute_string_f

  !-------------------------------------------------------------------------
  ! Query dataset functions
  !-------------------------------------------------------------------------

  !-------------------------------------------------------------------------
  ! Function: h5ltget_dataset_ndims_f
  !
  ! Purpose: Gets the dimensionality of a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 30, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_dataset_ndims_f(loc_id,&
       dset_name,&
       rank,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(inout) :: rank            ! rank
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_dataset_ndims_c(loc_id,namelen,dset_name,rank) &
            BIND(C,NAME='h5ltget_dataset_ndims_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                                      ! length of name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(inout) :: rank                 ! rank
       END FUNCTION h5ltget_dataset_ndims_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltget_dataset_ndims_c(loc_id,namelen,dset_name,rank)

  END SUBROUTINE h5ltget_dataset_ndims_f


  !-------------------------------------------------------------------------
  ! Function: h5ltfind_dataset_f
  !
  ! Purpose: Inquires if a dataset named dset_name exists attached
  !           to the object loc_id.
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  INTEGER FUNCTION h5ltfind_dataset_f(loc_id,&
       dset_name)

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltfind_dataset_c(loc_id,namelen,dset_name) &
            BIND(C,NAME='h5ltfind_dataset_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                                      ! length of name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
       END FUNCTION h5ltfind_dataset_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltfind_dataset_c(loc_id,namelen,dset_name)
    h5ltfind_dataset_f = errcode

  END FUNCTION h5ltfind_dataset_f

  !-------------------------------------------------------------------------
  ! Function: h5ltget_dataset_info_f
  !
  ! Purpose: Gets information about a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 30, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_dataset_info_f(loc_id,&
       dset_name,&
       dims,&
       type_class,&
       type_size,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims ! dimensions
    INTEGER, INTENT(inout)         :: type_class       ! type class
    INTEGER(size_t), INTENT(inout) :: type_size        ! type size
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_dataset_info_c(loc_id,namelen,dset_name,dims,type_class,type_size) &
            BIND(C,NAME='h5ltget_dataset_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                                      ! length of name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims      ! dimensions
         INTEGER, INTENT(inout)         :: type_class            ! type class
         INTEGER(size_t), INTENT(inout) :: type_size             ! type size
       END FUNCTION h5ltget_dataset_info_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltget_dataset_info_c(loc_id,namelen,dset_name,dims,type_class,type_size)

  END SUBROUTINE h5ltget_dataset_info_f


  !-------------------------------------------------------------------------
  ! Query attribute functions
  !-------------------------------------------------------------------------


  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_ndims_f
  !
  ! Purpose: Create and write an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_ndims_f(loc_id,&
       dset_name,&
       attr_name,&
       rank,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER,          INTENT(inout) :: rank            ! rank
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER(size_t) :: attrlen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_attribute_ndims_c(loc_id,namelen,dset_name,attrlen,attr_name,rank) &
            BIND(C,NAME='h5ltget_attribute_ndims_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                                      ! length of name buffer
         INTEGER(size_t) :: attrlen                                      ! length of attr name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: attr_name               ! name of the attribute
         INTEGER,          INTENT(inout) :: rank                 ! rank
       END FUNCTION h5ltget_attribute_ndims_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_ndims_c(loc_id,namelen,dset_name,attrlen,attr_name,rank)

  END SUBROUTINE h5ltget_attribute_ndims_f


  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_info_f
  !
  ! Purpose: Gets information about an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 30, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_info_f(loc_id,&
       dset_name,&
       attr_name,&
       dims,&
       type_class,&
       type_size,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims ! dimensions
    INTEGER, INTENT(inout)         :: type_class       ! type class
    INTEGER(size_t), INTENT(inout) :: type_size        ! type size
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length
    INTEGER(size_t) :: attrlen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_attribute_info_c(loc_id,namelen,dset_name,attrlen,attr_name,dims,type_class,type_size) &
            BIND(C,NAME='h5ltget_attribute_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                              ! length of name buffer
         INTEGER(size_t) :: attrlen                                      ! length of attr name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: attr_name               ! name of the attribute
         INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims      ! dimensions
         INTEGER, INTENT(inout)         :: type_class            ! type class
         INTEGER(size_t), INTENT(inout) :: type_size             ! type size
       END FUNCTION h5ltget_attribute_info_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_info_c(loc_id,namelen,dset_name,attrlen,attr_name,dims,type_class,type_size)

  END SUBROUTINE h5ltget_attribute_info_f

  !-------------------------------------------------------------------------
  ! Function: h5ltpath_valid_f
  !
  ! Purpose: Validates a path 
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: February 18, 2012
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltpath_valid_f(loc_id, path, check_object_valid, path_valid, errcode)

    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN)  :: loc_id              ! An identifier of an object in the file.
    CHARACTER(LEN=*), INTENT(IN)  :: path                ! Path to the object to check, relative to loc_id.
    LOGICAL         , INTENT(IN)  :: check_object_valid  ! Indicates whether to check if the final component 
                                                         !  of the path resolves to a valid object 
    LOGICAL         , INTENT(OUT) :: path_valid          ! Object status
    INTEGER         , INTENT(OUT) :: errcode             ! Error code: 0 on success and -1 on failure

    INTEGER(size_t) :: pathlen
    INTEGER :: check_object_valid_c
    INTEGER :: status

    INTERFACE
       INTEGER FUNCTION h5ltpath_valid_c(loc_id, path, pathlen, check_object_valid_c) &
            BIND(C,NAME='h5ltpath_valid_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id  
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: path
         INTEGER(size_t) :: pathlen
         INTEGER :: check_object_valid_c
       END FUNCTION h5ltpath_valid_c
    END INTERFACE

    ! Initialize
    path_valid = .FALSE.
    errcode = 0
    
    check_object_valid_c = 0
    IF(check_object_valid) check_object_valid_c = 1

    pathlen = LEN(path)
    status = h5ltpath_valid_c(loc_id, path, pathlen, check_object_valid_c)

    IF(status.EQ.1)THEN
       path_valid = .TRUE.
    ELSE IF(status.LT.0)THEN
       errcode = -1
    ENDIF
    
  END SUBROUTINE h5ltpath_valid_f

END MODULE H5LT






