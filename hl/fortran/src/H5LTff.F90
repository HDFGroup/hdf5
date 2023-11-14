!> @defgroup FH5LT Fortran High Level Lite (H5LT) Interface
!!
!! @see H5LT, C-HL API
!!
!! @see @ref H5LT_UG, User Guide
!!

!> @ingroup H5LT
!!
!! @brief This module contains Fortran interfaces for H5LT.
!
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

#include <H5config_f.inc>

#ifdef H5_DOXYGEN
MODULE H5LT
#else
MODULE H5LT_CONST
#endif

  USE, INTRINSIC :: ISO_C_BINDING
  USE h5fortran_types
  USE hdf5

#ifndef H5_DOXYGEN

  INTERFACE h5ltmake_dataset_f
     MODULE PROCEDURE h5ltmake_dataset_f_ptr
  END INTERFACE

  INTERFACE h5ltread_dataset_f
     MODULE PROCEDURE h5ltread_dataset_f_ptr
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
     INTEGER FUNCTION h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf) &
          BIND(C,NAME='h5ltread_dataset_c')
       IMPORT :: C_CHAR, C_PTR
       IMPORT :: HID_T, SIZE_T, HSIZE_T
       IMPLICIT NONE
       INTEGER(hid_t),   INTENT(in) :: loc_id                        ! file or group identifier
       INTEGER(hid_t),   INTENT(in) :: type_id                       ! datatype identifier
       INTEGER(size_t) :: namelen                                    ! length of name buffer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name ! name of the dataset
       TYPE(C_PTR), VALUE :: buf                                     ! data buffer
     END FUNCTION h5ltread_dataset_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5ltset_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf,dtype, SizeOf_buf) &
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
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dtype     ! flag indicating the datatype of the buffer:
                                                                     ! R=Real, D=DOUBLE, I=Integer, C=Character
       INTEGER(size_t) :: SizeOf_buf                                 ! Sizeof the buf datatype
     END FUNCTION h5ltset_attribute_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5ltget_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,buf,dtype, SizeOf_buf) &
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
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dtype     ! flag indicating the datatype of the buffer:
                                                                     ! R=Real, D=DOUBLE, I=Integer
       INTEGER(size_t), INTENT(in) :: SizeOf_buf                     ! Sizeof the buf data type
     END FUNCTION h5ltget_attribute_c
  END INTERFACE

#endif

CONTAINS

  !-------------------------------------------------------------------------
  ! Make/Read dataset functions
  !-------------------------------------------------------------------------

#ifdef H5_DOXYGEN
  !>
  !! \ingroup FH5LT
  !!
  !! \brief Creates and writes a dataset of a type \p type_id.
  !!
  !! \attention  \fortran_approved
  !!
  !! \param loc_id    Location identifier. The identifier may be that of a file or group.
  !! \param dset_name The name of the dataset to create.
  !! \param rank      Number of dimensions of dataspace.
  !! \param dims      An array of the size of each dimension.
  !! \param type_id   Identifier of the datatype to use when creating the dataset.
  !! \param buf       Buffer with data to be written to the dataset.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTmake_dataset()
  !!
  SUBROUTINE h5ltmake_dataset_f(&
#else
  SUBROUTINE h5ltmake_dataset_f_ptr( &
#endif
       loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode)

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    TYPE(C_PTR) :: buf
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

#ifdef H5_DOXYGEN
  END SUBROUTINE h5ltmake_dataset_f
#else
  END SUBROUTINE h5ltmake_dataset_f_ptr
#endif

#ifdef H5_DOXYGEN
  !>
  !! \ingroup FH5LT
  !!
  !! \brief Creates and writes a dataset of a type \p type_id.
  !!
  !! \attention \fortran_obsolete
  !!
  !! \param loc_id    Location identifier. The identifier may be that of a file or group.
  !! \param dset_name The name of the dataset to create.
  !! \param rank      Number of dimensions of dataspace.
  !! \param dims      An array of the size of each dimension. Limited to seven dimensions.
  !! \param type_id   Identifier of the datatype to use when creating the dataset.
  !! \param buf       Buffer with data to be written to the dataset.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTmake_dataset()
  !!
   SUBROUTINE h5ltmake_dataset_f(&
#else
  SUBROUTINE h5ltmake_dataset_f_int1(&
#endif
       loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
#ifdef H5_DOXYGEN
    TYPE(TYPE), INTENT(in), DIMENSION(*,*,...) :: buf
#else
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf
#endif
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
#ifdef H5_DOXYGEN
  END SUBROUTINE h5ltmake_dataset_f
#else
  END SUBROUTINE h5ltmake_dataset_f_int1

  SUBROUTINE h5ltmake_dataset_f_int2(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf ! data buffer
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
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int3

  SUBROUTINE h5ltmake_dataset_f_int4(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int4

  SUBROUTINE h5ltmake_dataset_f_int5(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int5

  SUBROUTINE h5ltmake_dataset_f_int6(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int6

  SUBROUTINE h5ltmake_dataset_f_int7(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf ! data buffer
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)

  END SUBROUTINE h5ltmake_dataset_f_int7
#endif

#ifdef H5_DOXYGEN
  !>
  !! \ingroup FH5LT
  !!
  !! \brief Reads a dataset of a type \p type_id.
  !!
  !! \attention \fortran_approved
  !!
  !! \param loc_id    Location identifier. The identifier may be that of a file or group.
  !! \param dset_name The name of the dataset to create.
  !! \param type_id   Identifier of the datatype to use when creating the dataset.
  !! \param buf       Buffer with data to be written to the dataset.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTread_dataset()
  !!
  SUBROUTINE h5ltread_dataset_f(&
#else
  SUBROUTINE h5ltread_dataset_f_ptr(&
#endif
       loc_id,&
       dset_name,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    TYPE(C_PTR) :: buf
    INTEGER :: errcode
    INTEGER(size_t) :: namelen

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id, buf)
#ifdef H5_DOXYGEN
  END SUBROUTINE h5ltread_dataset_f
#else
  END SUBROUTINE h5ltread_dataset_f_ptr
#endif

#ifdef H5_DOXYGEN
  !>
  !! \ingroup FH5LT
  !!
  !! \brief Reads a dataset of a type \p type_id.
  !!
  !! \attention \fortran_obsolete
  !!
  !! \param loc_id    Location identifier. The identifier may be that of a file or group.
  !! \param dset_name The name of the dataset to create.
  !! \param type_id   Identifier of the datatype to use when creating the dataset.
  !! \param buf       Buffer with data to be written to the dataset.
  !! \param dims      An array of the size of each dimension. Limited to seven dimensions.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTread_dataset()
  !!
   SUBROUTINE h5ltread_dataset_f(&
#else
  SUBROUTINE h5ltread_dataset_f_int1(&
#endif
       loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
#ifdef H5_DOXYGEN
    TYPE(TYPE), INTENT(inout), DIMENSION(*,*,...) :: buf
#else
    INTEGER, INTENT(inout), DIMENSION(*), TARGET :: buf
#endif
    INTEGER :: errcode
    INTEGER(size_t) :: namelen
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)

#ifdef H5_DOXYGEN
  END SUBROUTINE h5ltread_dataset_f
#else
  END SUBROUTINE h5ltread_dataset_f_int1

  SUBROUTINE h5ltread_dataset_f_int2(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)

  END SUBROUTINE h5ltread_dataset_f_int2

  SUBROUTINE h5ltread_dataset_f_int3(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)

  END SUBROUTINE h5ltread_dataset_f_int3

  SUBROUTINE h5ltread_dataset_f_int4(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)

  END SUBROUTINE h5ltread_dataset_f_int4

  SUBROUTINE h5ltread_dataset_f_int5(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)

  END SUBROUTINE h5ltread_dataset_f_int5

  SUBROUTINE h5ltread_dataset_f_int6(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)

  END SUBROUTINE h5ltread_dataset_f_int6

  SUBROUTINE h5ltread_dataset_f_int7(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)

  END SUBROUTINE h5ltread_dataset_f_int7

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_int_f_1
  !
  !! \brief Creates and writes a dataset of H5T_NATIVE_INT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Comments:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_int_f_1 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
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
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
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
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
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
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
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
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
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
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
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
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(in) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltmake_dataset_int_f_7

  !-------------------------------------------------------------------------
  ! Function(s): h5ltread_dataset_int_f_(1-7)
  !
  !! \brief Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Comments:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_int_f_1(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(HID_T),   INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltread_dataset_int_f_1

  SUBROUTINE h5ltread_dataset_int_f_2(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltread_dataset_int_f_2

  SUBROUTINE h5ltread_dataset_int_f_3(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltread_dataset_int_f_3

  SUBROUTINE h5ltread_dataset_int_f_4(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltread_dataset_int_f_4

  SUBROUTINE h5ltread_dataset_int_f_5(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltread_dataset_int_f_5

  SUBROUTINE h5ltread_dataset_int_f_6(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltread_dataset_int_f_6

  SUBROUTINE h5ltread_dataset_int_f_7(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,f_ptr)

  END SUBROUTINE h5ltread_dataset_int_f_7


  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_string_f
  !
  !! \brief Creates and writes a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Comments:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_string_f(loc_id,&
       dset_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: buf
    INTEGER :: errcode
    INTEGER(size_t) :: namelen                         ! name length
    INTEGER(size_t) :: buflen                          ! buffer length

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_string_c(loc_id,namelen,dset_name,buflen,buf) &
            BIND(C,NAME='h5ltmake_dataset_string_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen                                      ! length of name buffer
         INTEGER(size_t) :: buflen                                       ! length of data buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: buf
       END FUNCTION h5ltmake_dataset_string_c
    END INTERFACE

    namelen = LEN(dset_name)
    buflen = LEN(buf)
    errcode = h5ltmake_dataset_string_c(loc_id,namelen,dset_name,buflen,buf)

  END SUBROUTINE h5ltmake_dataset_string_f

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_string_f
  !
  !! \brief Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Comments:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_string_f(loc_id,&
       dset_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    CHARACTER(LEN=*), INTENT(inout) :: buf
    INTEGER :: errcode
    INTEGER(size_t) :: namelen                         ! name length

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_string_c(loc_id,namelen,dset_name,buf) &
            BIND(C,NAME='h5ltread_dataset_string_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen                                       ! length of name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in)    :: dset_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(inout) :: buf
       END FUNCTION h5ltread_dataset_string_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_string_c(loc_id,namelen,dset_name,buf)

  END SUBROUTINE h5ltread_dataset_string_f

#endif

  !-------------------------------------------------------------------------
  ! Make/Read attribute functions
  !-------------------------------------------------------------------------

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Creates and writes an attribute and is a generic replacement for data type specific
  !!        Fortran h5ltset_attribute_*_f APIs. There is no C equivalent API.
  !!
  !! \attention  \fortran_approved
  !!
  !! \param loc_id          Location identifier. The identifier may be that of a file or group.
  !! \param dset_name       The name of the dataset to create.
  !! \param attr_name       The name of the attribute to create.
  !! \param buf             The data buffer.
  !! \param buf_type        Valid data types are CHARACTER, INTEGER or REAL.
  !!                        NOTE: only the first character matters and is case insensitive.
  !! \param SizeOf_buf_type Size of \p buf&apos;s data type, in bytes.
  !! \param size            Size of attribute array.
  !! \param errcode         \fortran_error
  !!
  SUBROUTINE h5ltset_attribute_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       buf_type,&
       SizeOf_buf_type, &
       size,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    TYPE(C_PTR) :: buf
    CHARACTER(LEN=*), INTENT(in) :: buf_type
    INTEGER(size_t),  INTENT(in) :: size
    INTEGER(size_t),  INTENT(in) :: SizeOf_buf_type
    INTEGER, INTENT(out) :: errcode

    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: attrlen ! name length
    CHARACTER(KIND=C_CHAR) :: buf_type_uppercase

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)

    buf_type_uppercase(1:1) = buf_type(1:1)
    IF(buf_type_uppercase(1:1).EQ.'i')THEN
       buf_type_uppercase(1:1) = 'I'
    ELSE IF(buf_type_uppercase(1:1).EQ.'r')THEN
       buf_type_uppercase(1:1) = 'R'
    ELSE IF(buf_type_uppercase(1:1).EQ.'c')THEN
       buf_type_uppercase(1:1) = 'C'
    ENDIF

    errcode = h5ltset_attribute_c(loc_id,namelen,dset_name,attrlen,attr_name,size,&
         buf,buf_type_uppercase(1:1)//C_NULL_CHAR, SizeOf_buf_type)

  END SUBROUTINE h5ltset_attribute_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Creates and writes an attribute.
  !!
  !! \attention  \fortran_obsolete
  !!
  !! \param loc_id    Identifier of the object (dataset or group) to create the attribute within
  !! \param obj_name  The name of the object to attach the attribute.
  !! \param attr_name The attribute name.
  !! \param buf       Buffer with data to be written to the attribute.
  !! \param size      The size of the 1D array (one in the case of a scalar attribute).
  !!                  This value is used by H5Screate_simple() to create the dataspace.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTset_attribute_int()
  !!
  SUBROUTINE h5ltset_attribute_int_f(loc_id,&
       obj_name,&
       attr_name,&
       buf,&
       size,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: obj_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    INTEGER(size_t),  INTENT(in) :: size
    INTEGER :: errcode
    INTEGER, DIMENSION(*), TARGET :: buf
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: attrlen ! name length
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: SizeOf_buf_type

    f_ptr = C_LOC(buf(1:1))

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    SizeOf_buf_type = STORAGE_SIZE(buf(1), c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)
#else
    SizeOf_buf_type = SIZEOF(buf(1))
#endif

    namelen = LEN(obj_name)
    attrlen = LEN(attr_name)
    errcode = h5ltset_attribute_c(loc_id,namelen,obj_name,attrlen,attr_name,size,&
         f_ptr,'I'//C_NULL_CHAR,SizeOf_buf_type)

  END SUBROUTINE h5ltset_attribute_int_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Creates and writes an attribute.
  !!
  !! \attention  \fortran_obsolete
  !!
  !! \param loc_id    Identifier of the object (dataset or group) to create the attribute within
  !! \param obj_name  The name of the object to attach the attribute.
  !! \param attr_name The attribute name.
  !! \param buf       Buffer with data to be written to the attribute.
  !! \param size      The size of the 1D array (one in the case of a scalar attribute).
  !!                  This value is used by H5Screate_simple() to create the dataspace.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTset_attribute_float()
  !!
  SUBROUTINE h5ltset_attribute_float_f(loc_id,&
       obj_name,&
       attr_name,&
       buf,&
       size,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: obj_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    INTEGER(size_t),  INTENT(in) :: size
    INTEGER :: errcode
    REAL(KIND=C_FLOAT), INTENT(in), DIMENSION(*), TARGET :: buf
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: attrlen ! name length
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: SizeOf_buf_type

    f_ptr = C_LOC(buf(1))
#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    SizeOf_buf_type = STORAGE_SIZE(buf(1), c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)
#else
    SizeOf_buf_type = SIZEOF(buf(1))
#endif

    namelen = LEN(obj_name)
    attrlen = LEN(attr_name)
    errcode = h5ltset_attribute_c(loc_id,namelen,obj_name,attrlen,attr_name,size,&
         f_ptr,'R'//C_NULL_CHAR, SizeOf_buf_type)

  END SUBROUTINE h5ltset_attribute_float_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Creates and writes an attribute.
  !!
  !! \attention  \fortran_obsolete
  !!
  !! \param loc_id    Identifier of the object (dataset or group) to create the attribute within
  !! \param obj_name  The name of the object to attach the attribute.
  !! \param attr_name The attribute name.
  !! \param buf       Buffer with data to be written to the attribute.
  !! \param size      The size of the 1D array (one in the case of a scalar attribute).
  !!                  This value is used by H5Screate_simple() to create the dataspace.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTset_attribute_double()
  !!
  SUBROUTINE h5ltset_attribute_double_f(loc_id,&
       obj_name,&
       attr_name,&
       buf,&
       size,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: obj_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    INTEGER(size_t),  INTENT(in) :: size
    INTEGER :: errcode
    REAL(KIND=C_DOUBLE), INTENT(in), DIMENSION(*), TARGET :: buf
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: attrlen ! name length
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: SizeOf_buf_type

    f_ptr = C_LOC(buf(1))

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    SizeOf_buf_type = STORAGE_SIZE(buf(1), c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)
#else
    SizeOf_buf_type = SIZEOF(buf(1))
#endif

    namelen = LEN(obj_name)
    attrlen = LEN(attr_name)
    errcode = h5ltset_attribute_c(loc_id,namelen,obj_name,attrlen,attr_name,size,&
         f_ptr,'R'//C_NULL_CHAR,SizeOf_buf_type)

  END SUBROUTINE h5ltset_attribute_double_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Creates and writes an attribute.
  !!
  !! \attention  \fortran_obsolete
  !!
  !! \param loc_id    Identifier of the object (dataset or group) to create the attribute within
  !! \param obj_name  The name of the object to attach the attribute.
  !! \param attr_name The attribute name.
  !! \param buf       Buffer with data to be written to the attribute.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTset_attribute_string()
  !!
  SUBROUTINE h5ltset_attribute_string_f(loc_id,&
       obj_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: obj_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    INTEGER :: errcode
    CHARACTER(LEN=*), DIMENSION(*), INTENT(in), TARGET :: buf
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: attrlen ! name length
    INTEGER(size_t) :: buflen  ! data buffer length
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: SizeOf_buf_type

    f_ptr = C_LOC(buf(1)(1:1))

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    SizeOf_buf_type = STORAGE_SIZE(buf(1)(1:1), c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)
#else
    SizeOf_buf_type = SIZEOF(buf(1:1)(1:1))
#endif

    namelen = LEN(obj_name)
    attrlen = LEN(attr_name)
    buflen = LEN(buf)
    errcode = h5ltset_attribute_c(loc_id,namelen,obj_name,attrlen,attr_name,buflen,&
         f_ptr,'C'//C_NULL_CHAR, SizeOf_buf_type)

  END SUBROUTINE h5ltset_attribute_string_f
  !>
  !! \ingroup FH5LT
  !!
  !! \brief Reads an attribute from disk.
  !!
  !! \attention  \fortran_approved
  !!
  !! \param loc_id          Location identifier. The identifier may be that of a file or group.
  !! \param obj_name        The name of the object that the attribute is attached to.
  !! \param attr_name       The name of the attribute to create.
  !! \param buf             The data buffer.
  !! \param buf_type        Valid data types are CHARACTER, INTEGER or REAL.
  !!                        NOTE: only the first character matters and is case insensitive.
  !! \param SizeOf_buf_type Size of \p buf&apos;s data type, in bytes.
  !! \param errcode         \fortran_error
  !!
  !! See C API: @ref H5LTget_attribute()
  !!
  SUBROUTINE h5ltget_attribute_f(loc_id,&
       obj_name,&
       attr_name,&
       buf, buf_type, SizeOf_buf_type, &
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: obj_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    TYPE(C_PTR) :: buf
    CHARACTER(LEN=*), INTENT(in) :: buf_type
    INTEGER(size_t), INTENT(in) :: SizeOf_buf_type
    INTEGER, INTENT(out) :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: attrlen ! attr length
    CHARACTER(KIND=C_CHAR) :: buf_type_uppercase

    namelen = LEN(obj_name)
    attrlen = LEN(attr_name)

    buf_type_uppercase(1:1) = buf_type(1:1)
    IF(buf_type_uppercase(1:1).EQ.'i')THEN
       buf_type_uppercase(1:1) = 'I'
    ELSE IF(buf_type_uppercase(1:1).EQ.'r')THEN
       buf_type_uppercase(1:1) = 'R'
    ELSE IF(buf_type_uppercase(1:1).EQ.'c')THEN
       buf_type_uppercase(1:1) = 'C'
    ENDIF
    errcode = h5ltget_attribute_c(loc_id,namelen,obj_name,attrlen,attr_name, &
         buf, buf_type_uppercase//C_NULL_CHAR, SizeOf_buf_type)

  END SUBROUTINE h5ltget_attribute_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Reads an attribute from disk.
  !!
  !! \attention  \fortran_obsolete
  !!
  !! \param loc_id    Identifier of the object (dataset or group) to create the attribute within
  !! \param obj_name  The name of the object that the attribute is attached to.
  !! \param attr_name The attribute name.
  !! \param buf       Buffer with data to be written to the attribute.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTget_attribute_int()
  !!
  SUBROUTINE h5ltget_attribute_int_f(loc_id,&
       obj_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: obj_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    INTEGER :: errcode
    INTEGER, INTENT(inout), DIMENSION(*), TARGET :: buf
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: attrlen ! name length
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: SizeOf_buf

    f_ptr = C_LOC(buf(1))

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    SizeOf_buf = STORAGE_SIZE(buf(1), c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)
#else
    SizeOf_buf = SIZEOF(buf(1))
#endif
    namelen = LEN(obj_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_c(loc_id,namelen,obj_name,attrlen,attr_name,f_ptr,'I'//C_NULL_CHAR, SizeOf_buf)

  END SUBROUTINE h5ltget_attribute_int_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Reads an attribute from disk.
  !!
  !! \attention  \fortran_obsolete
  !!
  !! \param loc_id    Identifier of the object (dataset or group) to create the attribute within
  !! \param obj_name  The name of the object that the attribute is attached to.
  !! \param attr_name The attribute name.
  !! \param buf       Buffer with data to be written to the attribute.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTget_attribute_float()
  !!
  SUBROUTINE h5ltget_attribute_float_f(loc_id,&
       obj_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: obj_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    INTEGER :: errcode
    REAL(KIND=C_FLOAT), INTENT(inout), DIMENSION(*), TARGET :: buf
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: attrlen ! name length
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: SizeOf_buf

    f_ptr = C_LOC(buf(1))
#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    SizeOf_buf = STORAGE_SIZE(buf(1), c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)
#else
    SizeOf_buf = SIZEOF(buf(1))
#endif
    namelen = LEN(obj_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_c(loc_id,namelen,obj_name,attrlen,attr_name,f_ptr,'R'//C_NULL_CHAR, SizeOf_buf)

  END SUBROUTINE h5ltget_attribute_float_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Reads an attribute from disk.
  !!
  !! \attention  \fortran_obsolete
  !!
  !! \param loc_id    Identifier of the object (dataset or group) to create the attribute within
  !! \param obj_name  The name of the object that the attribute is attached to.
  !! \param attr_name The attribute name.
  !! \param buf       Buffer with data to be written to the attribute.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTget_attribute_double()
  !!
  SUBROUTINE h5ltget_attribute_double_f(loc_id,&
       obj_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: obj_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    INTEGER :: errcode
    REAL(KIND=C_DOUBLE),INTENT(inout),DIMENSION(*), TARGET :: buf
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: attrlen ! name length
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: SizeOf_buf

    f_ptr = C_LOC(buf(1))
#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    SizeOf_buf = STORAGE_SIZE(buf(1), c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)
#else
    SizeOf_buf = SIZEOF(buf(1))
#endif

    namelen = LEN(obj_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_c(loc_id,namelen,obj_name,attrlen,attr_name,f_ptr,'R'//C_NULL_CHAR, SizeOf_buf)

  END SUBROUTINE h5ltget_attribute_double_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Reads an attribute from disk.
  !!
  !! \attention  \fortran_obsolete
  !!
  !! \param loc_id    Identifier of the object (dataset or group) to create the attribute within
  !! \param obj_name  The name of the object that the attribute is attached to.
  !! \param attr_name The attribute name.
  !! \param buf       Buffer with data to be written to the attribute.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTget_attribute_string()
  !!
  SUBROUTINE h5ltget_attribute_string_f(loc_id,&
       obj_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: obj_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    INTEGER :: errcode
    CHARACTER(LEN=*), INTENT(inout) :: buf
    INTEGER(size_t) :: namelen  ! name length
    INTEGER(size_t) :: attrlen  ! name length
    INTEGER(size_t) :: buf_size ! buf size

   INTERFACE
       INTEGER FUNCTION h5ltget_attribute_string_c(loc_id,namelen,obj_name,attrlen,attr_name,buf,buf_size) &
            BIND(C,NAME='h5ltget_attribute_string_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         INTEGER(size_t) :: attrlen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: attr_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(inout) :: buf
         INTEGER(size_t) :: buf_size
       END FUNCTION h5ltget_attribute_string_c
    END INTERFACE

    namelen  = LEN(obj_name)
    attrlen  = LEN(attr_name)
    buf_size = LEN(buf)

    errcode = h5ltget_attribute_string_c(loc_id,namelen,obj_name,attrlen,attr_name,buf,buf_size)

  END SUBROUTINE h5ltget_attribute_string_f

  !-------------------------------------------------------------------------
  ! Query dataset functions
  !-------------------------------------------------------------------------

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Gets the dimensionality of a dataset.
  !!
  !! \param loc_id    Identifier of the object to locate the dataset within.
  !! \param dset_name The dataset name.
  !! \param rank      The dimensionality of the dataset.
  !! \param errcode   \fortran_error
  !!
  !! See C API: @ref H5LTget_dataset_ndims()
  !!
  SUBROUTINE h5ltget_dataset_ndims_f(loc_id,&
       dset_name,&
       rank,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER,          INTENT(inout) :: rank
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_dataset_ndims_c(loc_id,namelen,dset_name,rank) &
            BIND(C,NAME='h5ltget_dataset_ndims_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
         INTEGER,          INTENT(inout) :: rank
       END FUNCTION h5ltget_dataset_ndims_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltget_dataset_ndims_c(loc_id,namelen,dset_name,rank)

  END SUBROUTINE h5ltget_dataset_ndims_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Determines whether a dataset exists.
  !!
  !! \param loc_id    Identifier of the group containing the dataset.
  !! \param dset_name The dataset name.
  !!
  !! \result Returns zero (false), a positive (true) or a negative (failure) value.
  !!
  !! See C API: @ref H5LTfind_dataset()
  !!
  INTEGER FUNCTION h5ltfind_dataset_f(loc_id, dset_name)

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length

    INTERFACE
       INTEGER FUNCTION h5ltfind_dataset_c(loc_id,namelen,dset_name) &
            BIND(C,NAME='h5ltfind_dataset_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
       END FUNCTION h5ltfind_dataset_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltfind_dataset_c(loc_id,namelen,dset_name)
    h5ltfind_dataset_f = errcode

  END FUNCTION h5ltfind_dataset_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Retrieves information about a dataset.
  !!
  !! \param loc_id     Identifier of the object to locate the dataset within.
  !! \param dset_name  The dataset name.
  !! \param dims       The dimensions of the dataset.
  !! \param type_class The class identifier. See H5Tget_class_f() for a list of class types.
  !! \param type_size  The size of the datatype in bytes.
  !! \param errcode    \fortran_error
  !!
  !! See C API: @ref H5LTget_dataset_info()
  !!
  SUBROUTINE h5ltget_dataset_info_f(loc_id,&
       dset_name,&
       dims,&
       type_class,&
       type_size,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims
    INTEGER, INTENT(inout)         :: type_class
    INTEGER(size_t), INTENT(inout) :: type_size
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_dataset_info_c(loc_id,namelen,dset_name,dims,type_class,type_size) &
            BIND(C,NAME='h5ltget_dataset_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
         INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims
         INTEGER, INTENT(inout)         :: type_class
         INTEGER(size_t), INTENT(inout) :: type_size
       END FUNCTION h5ltget_dataset_info_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltget_dataset_info_c(loc_id,namelen,dset_name,dims,type_class,type_size)

  END SUBROUTINE h5ltget_dataset_info_f


  !-------------------------------------------------------------------------
  ! Query attribute functions
  !-------------------------------------------------------------------------

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Gets the dimensionality of an attribute.
  !!
  !! \param loc_id     Identifier of the object (dataset or group) to read the attribute from.
  !! \param obj_name   The name of the object that the attribute is attached to.
  !! \param attr_name  The attribute name.
  !! \param rank       The dimensionality of the attribute.
  !! \param errcode    \fortran_error
  !!
  !! See C API: @ref H5LTget_attribute_ndims()
  !!
  SUBROUTINE h5ltget_attribute_ndims_f(loc_id,&
       obj_name,&
       attr_name,&
       rank,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: obj_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    INTEGER,          INTENT(inout) :: rank
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: attrlen ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_attribute_ndims_c(loc_id,namelen,obj_name,attrlen,attr_name,rank) &
            BIND(C,NAME='h5ltget_attribute_ndims_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         INTEGER(size_t) :: attrlen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: attr_name
         INTEGER,          INTENT(inout) :: rank
       END FUNCTION h5ltget_attribute_ndims_c
    END INTERFACE

    namelen = LEN(obj_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_ndims_c(loc_id,namelen,obj_name,attrlen,attr_name,rank)

  END SUBROUTINE h5ltget_attribute_ndims_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Gets information about an attribute.
  !!
  !! \param loc_id     Identifier of the object (dataset or group) to read the attribute from.
  !! \param obj_name   The name of the object that the attribute is attached to.
  !! \param attr_name  The attribute name.
  !! \param dims       The dimensions of the attribute.
  !! \param type_class The class identifier. For a list of valid class types see: H5Tget_class_f().
  !! \param type_size  The size of the datatype in bytes.
  !! \param errcode    \fortran_error
  !!
  !! See C API: @ref H5LTget_attribute_info()
  !!
  SUBROUTINE h5ltget_attribute_info_f(loc_id,&
       obj_name,&
       attr_name,&
       dims,&
       type_class,&
       type_size,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: obj_name
    CHARACTER(LEN=*), INTENT(in) :: attr_name
    INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims
    INTEGER, INTENT(inout)         :: type_class
    INTEGER(size_t), INTENT(inout) :: type_size
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: attrlen ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_attribute_info_c(loc_id,namelen,obj_name,attrlen,attr_name,dims,type_class,type_size) &
            BIND(C,NAME='h5ltget_attribute_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         INTEGER(size_t) :: attrlen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: attr_name
         INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims
         INTEGER, INTENT(inout)         :: type_class
         INTEGER(size_t), INTENT(inout) :: type_size
       END FUNCTION h5ltget_attribute_info_c
    END INTERFACE

    namelen = LEN(obj_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_info_c(loc_id,namelen,obj_name,attrlen,attr_name,dims,type_class,type_size)

  END SUBROUTINE h5ltget_attribute_info_f

  !>
  !! \ingroup FH5LT
  !!
  !! \brief Determines whether an HDF5 path is valid and, optionally, whether the path resolves to an HDF5 object.
  !!
  !! \param loc_id              Identifier of an object in the file.
  !! \param path                The path to the object to check; links in path may be of any type.
  !! \param check_object_valid	Indicates whether to check if the final component of the path resolves to a valid object.
  !! \param path_valid          Object status.
  !! \param errcode             \fortran_error
  !!
  !! See C API: @ref H5LTpath_valid()
  !!
  SUBROUTINE h5ltpath_valid_f(loc_id, path, check_object_valid, path_valid, errcode)

    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN)  :: loc_id
    CHARACTER(LEN=*), INTENT(IN)  :: path
    LOGICAL         , INTENT(IN)  :: check_object_valid
    LOGICAL         , INTENT(OUT) :: path_valid
    INTEGER         , INTENT(OUT) :: errcode

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

#ifdef H5_DOXYGEN
END MODULE H5LT
#else
END MODULE H5LT_CONST
#endif






