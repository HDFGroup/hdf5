!> @defgroup FH5D Fortran Datasets (H5D) Interface
!!
!! @see H5D, C-API
!!
!! @see @ref H5D_UG, User Guide
!!

!> @ingroup FH5D
!!
!! @brief This module contains Fortran interfaces for H5D functions.
!
! COPYRIGHT
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
!
! NOTES
!  (1) The maximum rank of an array allowed in Fortran is 7, therefore
!  we only provide an interface for arrays up to and including rank 7.
!
!  (2) Unfortunately we are using a generic interface and one of the factors
!  used in determining the proper routine to select is that of the array
!  rank being passed. Therefore, we can not create just one subroutine for
!  each array type (integer, real, etc...) and use a
!  rank 1 array of assumed size to handle multiple ranks, i.e.
!  (i.e. integer, dimension(*) :: ... )
!  (i.e. real   , dimension(*) :: ... ) etc...
!
!  (3) Could not place the USE, INTRINSIC :: ISO_C_BINDING in the module header because it may
!  conflict with the USE, INTRINSIC :: ISO_C_BINDING included in the user&apos;s program. Moved
!  the statement instead to each subroutine.
!
!
!  (4) C_LOC and character strings according to the Fortran 2003 standard:
!
!  15.1.2.5 C_LOC(X)
!
!  Argument. X shall either
!
!  (A) have interoperable type and type parameters and be
!  (a) a variable that has the TARGET attribute and is interoperable,
!  (b) an allocated allocatable variable that has the TARGET attribute
!  and is not an array of zero size, or
!  (c) an associated scalar pointer, or
!  (B) be a nonpolymorphic scalar, have no length type parameters, and be
!  (a) a nonallocatable, nonpointer variable that has the TARGET attribute,
!  (b) an allocated allocatable variable that has the TARGET attribute, or
!  (c) an associated pointer.
!
!          - When X is a character, for interoperability the standard is:
!
!  15.2.1 Interoperability of intrinsic types
!
!  ...if the type is character, interoperability also requires that the length type parameter
!  be omitted or be specified by an initialization expression whose value is one.
!
!  THEREFORE compilers that have not extended the standard require the
!  argument in C_LOC to be of the variant:
!
!  CHARACTER(LEN=1), TARGET :: chr
!  or
!  CHARACTER, TARGET :: chr
!
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5D function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

#include <H5config_f.inc>

MODULE H5D

  USE H5GLOBAL
  USE H5LIB, ONLY : h5kind_to_type
  USE H5S, ONLY : H5Sget_simple_extent_ndims_f, H5Sclose_f
  IMPLICIT NONE

  PRIVATE h5dread_vl_integer, h5dread_vl_real, h5dread_vl_string
  PRIVATE h5dwrite_vl_integer, h5dwrite_vl_real, h5dwrite_vl_string
  PRIVATE h5dwrite_reference_obj, h5dwrite_reference_dsetreg, h5dwrite_char_scalar, h5dwrite_ptr
  PRIVATE h5dread_reference_obj, h5dread_reference_dsetreg, h5dread_char_scalar, h5dread_ptr
  PRIVATE h5dfill_integer, h5dfill_c_float, h5dfill_c_double, h5dfill_char, h5dfill_ptr
#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
  PRIVATE h5dfill_c_long_double
#endif

  INTERFACE h5dextend_f
     MODULE PROCEDURE h5dset_extent_f
  END INTERFACE

#ifndef H5_DOXYGEN

  INTERFACE h5dwrite_f
     MODULE PROCEDURE h5dwrite_reference_obj
     MODULE PROCEDURE h5dwrite_reference_dsetreg
     MODULE PROCEDURE h5dwrite_char_scalar
     ! This is the preferred way to call h5dwrite
     ! by passing an address
     MODULE PROCEDURE h5dwrite_ptr
  END INTERFACE

  INTERFACE h5dread_f
     MODULE PROCEDURE h5dread_reference_obj
     MODULE PROCEDURE h5dread_reference_dsetreg
     MODULE PROCEDURE h5dread_char_scalar
     ! This is the preferred way to call h5dread
     ! by passing an address
     MODULE PROCEDURE h5dread_ptr
  END INTERFACE

  INTERFACE h5dread_vl_f
     MODULE PROCEDURE h5dread_vl_integer
     MODULE PROCEDURE h5dread_vl_real
     MODULE PROCEDURE h5dread_vl_string
  END INTERFACE

  INTERFACE h5dwrite_vl_f
     MODULE PROCEDURE h5dwrite_vl_integer
     MODULE PROCEDURE h5dwrite_vl_real
     MODULE PROCEDURE h5dwrite_vl_string
  END INTERFACE


!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Dwrite routine

  INTERFACE
     INTEGER FUNCTION h5dwrite_f_c(dset_id, mem_type_id, &
          mem_space_id_default ,                         &
          file_space_id_default,                         &
          xfer_prp_default, buf ) BIND(C, NAME='h5dwrite_f_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: dset_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       INTEGER(HID_T) :: mem_space_id_default
       INTEGER(HID_T) :: file_space_id_default
       INTEGER(HID_T) :: xfer_prp_default
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5dwrite_f_c
  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Dread routine

  INTERFACE
     INTEGER FUNCTION h5dread_f_c(dset_id, mem_type_id, &
          mem_space_id_default,                         &
          file_space_id_default,                        &
          xfer_prp_default, buf) BIND(C, NAME='h5dread_f_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: dset_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       INTEGER(HID_T) :: mem_space_id_default
       INTEGER(HID_T) :: file_space_id_default
       INTEGER(HID_T) :: xfer_prp_default
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5dread_f_c
  END INTERFACE

  INTERFACE h5dfill_f
     MODULE PROCEDURE h5dfill_integer
     MODULE PROCEDURE h5dfill_c_float
     MODULE PROCEDURE h5dfill_c_double
#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
     MODULE PROCEDURE h5dfill_c_long_double
#endif
     MODULE PROCEDURE h5dfill_char
     MODULE PROCEDURE h5dfill_ptr
  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Dfill routine

  INTERFACE
     INTEGER FUNCTION h5dfill_c(f_ptr_fill_value, fill_type_id, space_id, &
          f_ptr_buf, mem_type_id) BIND(C, NAME='h5dfill_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       TYPE(C_PTR), VALUE :: f_ptr_fill_value
       INTEGER(HID_T) :: fill_type_id
       INTEGER(HID_T), INTENT(IN) :: space_id
       TYPE(C_PTR), VALUE :: f_ptr_buf
       INTEGER(HID_T) :: mem_type_id
     END FUNCTION h5dfill_c
  END INTERFACE
#endif

CONTAINS

!>
!! \ingroup FH5D
!!
!! \brief Creates a dataset at the specified location.
!!
!! \param loc_id   File or group identifier
!! \param name     Dataset name
!! \param type_id  Dataset datatype identifier
!! \param space_id Dataset dataspace identifier
!! \param dset_id  Dataset identifier
!! \param hdferr   \fortran_error
!! \param dcpl_id  Dataset creation property list
!! \param lcpl_id  Link creation property list
!! \param dapl_id  Dataset access property list
!!
!! See C API: @ref H5Dcreate2()
!!
  SUBROUTINE h5dcreate_f(loc_id, name, type_id, space_id, dset_id, &
       hdferr, dcpl_id, lcpl_id, dapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HID_T), INTENT(OUT) :: dset_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: dcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: dapl_id

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: dcpl_id_default
    INTEGER(HID_T) :: dapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Dcreate2(loc_id, name, type_id, &
            space_id, lcpl_id_default, dcpl_id_default, dapl_id_default) &
            BIND(C,NAME='H5Dcreate2')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(HID_T), VALUE :: type_id
         INTEGER(HID_T), VALUE :: space_id
         INTEGER(HID_T), VALUE :: lcpl_id_default
         INTEGER(HID_T), VALUE :: dcpl_id_default
         INTEGER(HID_T), VALUE :: dapl_id_default
       END FUNCTION H5Dcreate2
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    lcpl_id_default = H5P_DEFAULT_F
    dcpl_id_default = H5P_DEFAULT_F
    dapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    IF(PRESENT(dcpl_id)) dcpl_id_default = dcpl_id
    IF(PRESENT(dapl_id)) dapl_id_default = dapl_id

    dset_id = INT(h5dcreate2(loc_id, c_name, type_id, space_id, &
         lcpl_id_default, dcpl_id_default, dapl_id_default), HID_T)

    hdferr = 0
    IF(dset_id.LT.0) hdferr = -1

  END SUBROUTINE h5dcreate_f

!>
!! \ingroup FH5D
!!
!! \brief Asynchronously creates a dataset at the specified location.
!!
!! \param loc_id   File or group identifier
!! \param name     Dataset name
!! \param type_id  Dataset datatype identifier
!! \param space_id Dataset dataspace identifier
!! \param dset_id  Dataset identifier
!! \param es_id    \fortran_es_id
!! \param hdferr   \fortran_error
!! \param dcpl_id  Dataset creation property list
!! \param lcpl_id  Link creation property list
!! \param dapl_id  Dataset access property list
!! \param file     \fortran_file
!! \param func     \fortran_func
!! \param line     \fortran_line
!!
!! See C API: @ref H5Dcreate_async()
!!
  SUBROUTINE h5dcreate_async_f(loc_id, name, type_id, space_id, dset_id, es_id, &
       hdferr, dcpl_id, lcpl_id, dapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HID_T), INTENT(OUT) :: dset_id
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: dcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: dapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: dcpl_id_default
    INTEGER(HID_T) :: dapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Dcreate_async(file, func, line, loc_id, name, type_id, &
            space_id, lcpl_id_default, dcpl_id_default, dapl_id_default, es_id) &
            BIND(C,NAME='H5Dcreate_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(HID_T), VALUE :: type_id
         INTEGER(HID_T), VALUE :: space_id
         INTEGER(HID_T), VALUE :: lcpl_id_default
         INTEGER(HID_T), VALUE :: dcpl_id_default
         INTEGER(HID_T), VALUE :: dapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Dcreate_async
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    lcpl_id_default = H5P_DEFAULT_F
    dcpl_id_default = H5P_DEFAULT_F
    dapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    IF(PRESENT(dcpl_id)) dcpl_id_default = dcpl_id
    IF(PRESENT(dapl_id)) dapl_id_default = dapl_id
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    dset_id = h5dcreate_async(file_default, func_default, line_default, &
         loc_id, c_name, type_id, space_id, &
         lcpl_id_default, dcpl_id_default, dapl_id_default, es_id)

    hdferr = 0
    IF(dset_id.LT.0) hdferr = -1

  END SUBROUTINE h5dcreate_async_f

!>
!! \ingroup FH5D
!!
!! \brief Opens an existing dataset.
!!
!! \param loc_id  File or group identifier
!! \param name    Dataset name
!! \param dset_id Dataset identifier
!! \param hdferr  \fortran_error
!! \param dapl_id Dataset access property list
!!
!! See C API: @ref H5Dopen2()
!!
  SUBROUTINE h5dopen_f(loc_id, name, dset_id, hdferr, dapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(OUT) :: dset_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: dapl_id

    INTEGER(HID_T) :: dapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Dopen2(loc_id, name, dapl_id_default) &
            BIND(C,NAME='H5Dopen2')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(HID_T),VALUE :: dapl_id_default
       END FUNCTION H5Dopen2
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    dapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(dapl_id)) dapl_id_default = dapl_id

    dset_id = INT(H5Dopen2(loc_id, c_name, dapl_id_default), HID_T)

    hdferr = 0
    IF(dset_id.LT.0) hdferr = -1

  END SUBROUTINE h5dopen_f

!>
!! \ingroup FH5D
!!
!! \brief Asynchronously opens an existing dataset.
!!
!! \param loc_id  File or group identifier
!! \param name    Dataset name
!! \param dset_id Dataset identifier
!! \param es_id   \fortran_es_id
!! \param hdferr  \fortran_error
!! \param dapl_id Dataset access property list
!! \param file    \fortran_file
!! \param func    \fortran_func
!! \param line    \fortran_line
!!
!! See C API: @ref H5Dopen_async()
!!
  SUBROUTINE h5dopen_async_f(loc_id, name, dset_id, es_id, hdferr, dapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(OUT) :: dset_id
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: dapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T) :: dapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Dopen_async(file, func, line, loc_id, name, dapl_id_default, es_id) &
            BIND(C,NAME='H5Dopen_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(HID_T), VALUE :: dapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Dopen_async
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    dapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(dapl_id)) dapl_id_default = dapl_id
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    dset_id = H5Dopen_async(file_default, func_default, line_default, &
         loc_id, c_name, dapl_id_default, es_id)

    hdferr = 0
    IF(dset_id.LT.0) hdferr = -1

  END SUBROUTINE h5dopen_async_f

!>
!! \ingroup FH5D
!!
!! \brief Closes a dataset.
!!
!! \param dset_id Dataset identifier
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Dclose()
!!
  SUBROUTINE h5dclose_f(dset_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Dclose(dset_id) BIND(C,NAME='H5Dclose')
         IMPORT :: C_INT
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: dset_id
       END FUNCTION h5dclose
    END INTERFACE

    hdferr = INT(H5Dclose(dset_id))

  END SUBROUTINE h5dclose_f

!>
!! \ingroup FH5D
!!
!! \brief Asynchronously closes a dataset.
!!
!! \param dset_id Dataset identifier
!! \param es_id   \fortran_es_id
!! \param hdferr  \fortran_error
!! \param file    \fortran_file
!! \param func    \fortran_func
!! \param line    \fortran_line
!!
!! See C API: @ref H5Dclose_async()
!!
  SUBROUTINE h5dclose_async_f(dset_id, es_id, hdferr, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Dclose_async(file, func, line, dset_id, es_id) BIND(C,NAME='H5Dclose_async')
         IMPORT ::  C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: dset_id
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Dclose_async
    END INTERFACE

    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = INT(H5Dclose_async(file_default, func_default, line_default, dset_id, es_id))

  END SUBROUTINE h5dclose_async_f

!>
!! \ingroup FH5D
!!
!! \brief Returns an identifier for a copy of the datatype for a
!!       dataset.
!!
!! \param dataset_id  Dataset identifier
!! \param datatype_id Dataspace identifier
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Dget_type()
!!
  SUBROUTINE h5dget_type_f(dataset_id, datatype_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id
    INTEGER(HID_T), INTENT(OUT) :: datatype_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5dget_type_c(dataset_id, datatype_id) &
            BIND(C,NAME='h5dget_type_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dataset_id
         INTEGER(HID_T), INTENT(OUT) :: datatype_id
       END FUNCTION h5dget_type_c
    END INTERFACE

    hdferr = h5dget_type_c (dataset_id, datatype_id)
  END SUBROUTINE h5dget_type_f

!>
!! \ingroup FH5D
!!
!! \brief Extends a dataset with unlimited dimension.
!!
!! \param dataset_id Dataset identifier
!! \param fsize      Array containing the new magnitude of each dimension
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Dset_extent()
!!
  SUBROUTINE h5dset_extent_f(dataset_id, fsize, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: fsize
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: csize
    INTEGER(HID_T) :: space_id
    INTEGER :: rank
    INTEGER :: i

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Dset_extent(dataset_id, size) &
            BIND(C,NAME='H5Dset_extent')
         IMPORT :: C_INT
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: dataset_id
         INTEGER(HSIZE_T), DIMENSION(*) :: size
       END FUNCTION H5Dset_extent
    END INTERFACE

    CALL H5Dget_space_f(dataset_id, space_id, hdferr)
    IF(hdferr.LT.0) RETURN

    CALL H5Sget_simple_extent_ndims_f(space_id, rank, hdferr)
    IF( hdferr.LT.0 .OR. rank.LT.0 )THEN
       CALL H5Sclose_f(space_id, hdferr)
       hdferr = -1
       RETURN
    ENDIF
    CALL H5Sclose_f(space_id, hdferr)
    IF(hdferr.LT.0) RETURN

    ALLOCATE(csize(rank), STAT=hdferr)
    IF (hdferr .NE. 0 ) THEN
       hdferr = -1
       RETURN
    ENDIF

    !
    ! Reverse dimensions due to C-FORTRAN storage order.
    !
    DO i = 1, rank
       csize(i) = fsize(rank - i + 1)
    ENDDO

    hdferr = INT(H5Dset_extent(dataset_id, csize))

    DEALLOCATE(csize)

  END SUBROUTINE h5dset_extent_f


!>
!! \ingroup FH5D
!!
!! \brief Asynchronously  extends a dataset with unlimited dimension.
!!
!! \param dataset_id Dataset identifier
!! \param fsize      Array containing the new magnitude of each dimension
!! \param es_id      \fortran_es_id
!! \param hdferr     \fortran_error
!! \param file       \fortran_file
!! \param func       \fortran_func
!! \param line       \fortran_line
!!
!! See C API: @ref H5Dset_extent_async()
!!
  SUBROUTINE h5dset_extent_async_f(dataset_id, fsize, es_id, hdferr, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: fsize
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0
    INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: csize
    INTEGER(HID_T) :: space_id
    INTEGER :: rank
    INTEGER :: i

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Dset_extent_async(file, func, line, dataset_id, size, es_id) &
            BIND(C,NAME='H5Dset_extent_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: dataset_id
         INTEGER(HSIZE_T), DIMENSION(*) :: size
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Dset_extent_async
    END INTERFACE

    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    CALL H5Dget_space_f(dataset_id, space_id, hdferr)
    IF(hdferr.LT.0) RETURN

    CALL H5Sget_simple_extent_ndims_f(space_id, rank, hdferr)
    IF( hdferr.LT.0 .OR. rank.LT.0 )THEN
       CALL H5Sclose_f(space_id, hdferr)
       hdferr = -1
       RETURN
    ENDIF
    CALL H5Sclose_f(space_id, hdferr)
    IF(hdferr.LT.0) RETURN

    ALLOCATE(csize(rank), STAT=hdferr)
    IF (hdferr .NE. 0 ) THEN
       hdferr = -1
       RETURN
    ENDIF

    !
    ! Reverse dimensions due to C-FORTRAN storage order.
    !
    DO i = 1, rank
       csize(i) = fsize(rank - i + 1)
    ENDDO

    hdferr = INT(H5Dset_extent_async(file_default, func_default, line_default, &
         dataset_id, csize, es_id))

    DEALLOCATE(csize)

  END SUBROUTINE h5dset_extent_async_f

!>
!! \ingroup FH5D
!!
!! \brief Returns an identifier for a copy of the dataset creation property list for a dataset.
!!
!! \param dataset_id Dataset identifier
!! \param plist_id   Creation property list identifier
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Dget_create_plist()
!!
  SUBROUTINE h5dget_create_plist_f(dataset_id, plist_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id
    INTEGER(HID_T), INTENT(OUT) :: plist_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5dget_create_plist_c(dataset_id, plist_id) &
            BIND(C,NAME='h5dget_create_plist_c')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: dataset_id
         INTEGER(HID_T), INTENT(OUT) :: plist_id
       END FUNCTION h5dget_create_plist_c
    END INTERFACE

    hdferr = h5dget_create_plist_c(dataset_id, plist_id)
  END SUBROUTINE h5dget_create_plist_f

!>
!! \ingroup FH5D
!!
!! \brief Returns the amount of storage requires by a dataset
!!
!! \param dataset_id Dataset identifier
!! \param size       Datastorage size
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Dget_storage_size()
!!
  SUBROUTINE h5dget_storage_size_f(dataset_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id
    INTEGER(HSIZE_T),  INTENT(OUT)  :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5dget_storage_size_c(dataset_id, size) &
            BIND(C,NAME='h5dget_storage_size_c')
         IMPORT :: HID_T, HSIZE_T
         INTEGER(HID_T), INTENT(IN) :: dataset_id
         INTEGER(HSIZE_T), INTENT(OUT)  :: size
       END FUNCTION h5dget_storage_size_c
    END INTERFACE

    hdferr = h5dget_storage_size_c(dataset_id, size)
  END SUBROUTINE h5dget_storage_size_f

!>
!! \ingroup FH5D
!!
!! \brief Returns maximum length of the VL array elements
!!
!! \param dataset_id Dataset identifier
!! \param type_id    Datatype identifier
!! \param space_id   Dataspace identifier
!! \param len        Buffer size
!! \param hdferr     \fortran_error
!!
  SUBROUTINE h5dvlen_get_max_len_f(dataset_id, type_id, space_id, len,  hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(SIZE_T),  INTENT(OUT)  :: len
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5dvlen_get_max_len_c(dataset_id, type_id, space_id, len) &
            BIND(C,NAME='h5dvlen_get_max_len_c')
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), INTENT(IN) :: dataset_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(SIZE_T), INTENT(OUT)  :: len
       END FUNCTION h5dvlen_get_max_len_c
    END INTERFACE

    hdferr = h5dvlen_get_max_len_c(dataset_id, type_id,  space_id, len)
  END SUBROUTINE h5dvlen_get_max_len_f

!>
!! \ingroup FH5D
!!
!! \brief Returns the status of data space allocation.
!!
!! \param dset_id Dataset identifier
!! \param flag    Status; may have one of the following values:
!!                \li H5D_SPACE_STS_ERROR_F
!!                \li H5D_SPACE_STS_NOT_ALLOCATED_F
!!                \li H5D_SPACE_STS_PART_ALLOCATED_F
!!                \li H5D_SPACE_STS_ALLOCATED_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Dget_space_status()
!!
  SUBROUTINE h5dget_space_status_f(dset_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER, INTENT(OUT)       :: flag
    INTEGER, INTENT(OUT)       :: hdferr
    INTERFACE
       INTEGER FUNCTION h5dget_space_status_c(dset_id, flag) &
            BIND(C,NAME='h5dget_space_status_c')
         IMPORT :: HID_T
         INTEGER(HID_T) :: dset_id
         INTEGER        :: flag
       END FUNCTION h5dget_space_status_c
    END INTERFACE

    hdferr = h5dget_space_status_c(dset_id, flag)
  END SUBROUTINE h5dget_space_status_f

!>
!! \ingroup FH5D
!!
!! \brief Creates a dataset in a file without linking it into the file structure
!!
!! \param loc_id   Identifier of the file or group within which to create the dataset.
!! \param type_id  Identifier of the datatype to use when creating the dataset.
!! \param space_id Identifier of the dataspace to use when creating the dataset.
!! \param dset_id  Dataset identifier.
!! \param hdferr   \fortran_error
!! \param dcpl_id  Dataset creation property list identifier.
!! \param dapl_id  Dataset access property list identifier.
!!
!! See C API: @ref H5Dcreate_anon()
!!
  SUBROUTINE h5dcreate_anon_f(loc_id, type_id, space_id, dset_id, hdferr, dcpl_id, dapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HID_T), INTENT(OUT) :: dset_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: dcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: dapl_id
    INTEGER(HID_T) :: dcpl_id_default
    INTEGER(HID_T) :: dapl_id_default

    !
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dcreate_anon_c(loc_id, type_id, space_id, dcpl_id_default, dapl_id_default, dset_id) &
            BIND(C,NAME='h5dcreate_anon_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HID_T) :: dcpl_id_default
         INTEGER(HID_T) :: dapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: dset_id
       END FUNCTION h5dcreate_anon_c
    END INTERFACE

    dcpl_id_default = H5P_DEFAULT_F
    dapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(dcpl_id)) dcpl_id_default = dcpl_id
    IF(PRESENT(dapl_id)) dapl_id_default = dapl_id

    hdferr = h5dcreate_anon_c(loc_id, type_id, space_id, dcpl_id_default, dapl_id_default, dset_id)

  END SUBROUTINE h5dcreate_anon_f

#if H5_DOXYGEN
  !>
  !! \ingroup FH5D
  !!
  !! \brief Reads variable-length data. F2003 API h5dread_f should be used instead.
  !!
  !! \param dset_id       Dataset identifier.
  !! \param mem_type_id   Memory datatype identifier.
  !! \param buf           Data buffer; may be a scalar or an array, TYPE(TYPE) must be one of the following:
  !!                      \li INTEGER
  !!                      \li REAL
  !!                      \li CHARACTER
  !! \param dims          Array to hold corresponding dimension sizes of data buffer buf, dim(k) has value of the k-th
  !!                      dimension of buffer buf. Values are ignored if buf is a scalar.
  !! \param len           Array to store length of each element.
  !! \param hdferr        \fortran_error
  !! \param mem_space_id  Memory dataspace identifier, default value is H5S_ALL_F.
  !! \param file_space_id File dataspace identifier, default value is H5S_ALL_F.
  !! \param xfer_prp      Transfer property list identifier, default value is H5P_DEFAULT_F.
  !!
  SUBROUTINE h5dread_vl_f(dset_id, mem_type_id, buf, dims, len, hdferr, mem_space_id, file_space_id, xfer_prp)
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    TYPE(TYPE), INTENT(INOUT), DIMENSION(dims(1),dims(2)) :: buf

    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2)  :: dims
    INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*)  :: len
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
  END SUBROUTINE h5dread_vl_f
  !>
  !! \ingroup FH5D
  !!
  !! \brief Writes variable-length data. F2003 API h5dwrite_f should be used instead.
  !!
  !! \param dset_id       Dataset identifier.
  !! \param mem_type_id   Memory datatype identifier.
  !! \param buf           Data buffer; may be a scalar or an array, TYPE(TYPE) must be one of the following:
  !!                      \li INTEGER
  !!                      \li REAL
  !!                      \li CHARACTER
  !! \param dims          Array to hold corresponding dimension sizes of data buffer buf, dim(k) has value of the k-th
  !!                      dimension of buffer buf. Values are ignored if buf is a scalar.
  !! \param len           Array to store length of each element.
  !! \param hdferr        \fortran_error
  !! \param mem_space_id  Memory dataspace identifier, default value is H5S_ALL_F.
  !! \param file_space_id File dataspace identifier, default value is H5S_ALL_F.
  !! \param xfer_prp      Transfer property list identifier, default value is H5P_DEFAULT_F.
  !!
  SUBROUTINE h5dwrite_vl_f(dset_id, mem_type_id, buf, dims, len, hdferr, mem_space_id, file_space_id, xfer_prp)
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    TYPE(TYPE), INTENT(IN),DIMENSION(dims(1),dims(2)) :: buf
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2)  :: dims
    INTEGER(SIZE_T), INTENT(IN), DIMENSION(*)  :: len
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
  END SUBROUTINE h5dwrite_vl_f

#else
  SUBROUTINE h5dwrite_vl_integer(dset_id, mem_type_id, buf, dims, len, &
       hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
    INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len
    INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_vl_integer_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims, len) &
            BIND(C,NAME='h5dwrite_vl_integer_c')
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len
         INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dwrite_vl_integer_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_vl_integer_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, len)

  END SUBROUTINE h5dwrite_vl_integer

  SUBROUTINE h5dread_vl_integer(dset_id, mem_type_id, buf, dims, len, &
       hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
    INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len
    INTEGER, INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    INTEGER(HID_T) :: tmp
    INTEGER :: error

    INTERFACE
       INTEGER FUNCTION h5dread_vl_integer_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims, len) &
            BIND(C,NAME='h5dread_vl_integer_c')
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len
         INTEGER, INTENT(INOUT), DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dread_vl_integer_c
    END INTERFACE

    CALL h5dget_space_f(dset_id, tmp, error)
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = tmp
    file_space_id_default = tmp

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_vl_integer_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, len)

  END SUBROUTINE h5dread_vl_integer

  SUBROUTINE h5dwrite_vl_real(dset_id, mem_type_id, buf, dims, len, &
       hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
    INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2)) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_vl_real_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims, len) &
            BIND(C,NAME='h5dwrite_vl_real_c')
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dwrite_vl_real_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_vl_real_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, len)

  END SUBROUTINE h5dwrite_vl_real

  SUBROUTINE h5dread_vl_real(dset_id, mem_type_id, buf, dims, len, &
       hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
    INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    INTEGER(HID_T) :: tmp
    INTEGER :: error

    INTERFACE
       INTEGER FUNCTION h5dread_vl_real_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims, len) &
            BIND(C,NAME='h5dread_vl_real_c')
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dread_vl_real_c
    END INTERFACE

    CALL h5dget_space_f(dset_id, tmp, error)
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = tmp
    file_space_id_default = tmp

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_vl_real_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, len)

  END SUBROUTINE h5dread_vl_real

  SUBROUTINE h5dwrite_vl_string(dset_id, mem_type_id, buf, dims, str_len, &
       hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
    INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: str_len
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(2)) :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_vl_string_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            ! xfer_prp_default, tmp_buf, dims, str_len)
            xfer_prp_default, buf, dims, str_len) &
            BIND(C,NAME='h5dwrite_vl_string_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
         INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: str_len
         CHARACTER(KIND=C_CHAR), DIMENSION(dims(2)) :: buf
       END FUNCTION
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_vl_string_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, str_len)

  END SUBROUTINE h5dwrite_vl_string

  SUBROUTINE h5dread_vl_string(dset_id, mem_type_id, buf, dims, str_len, &
       hdferr, mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
    INTEGER(SIZE_T), INTENT(OUT), DIMENSION(*) :: str_len
    CHARACTER(LEN=*), INTENT(OUT), &
         DIMENSION(dims(2)) :: buf      ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_vl_string_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims, str_len) &
            BIND(C,NAME='h5dread_vl_string_c')
         IMPORT :: c_char
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
         INTEGER(SIZE_T), INTENT(OUT), DIMENSION(*) :: str_len
         CHARACTER(KIND=C_CHAR), DIMENSION(dims(2)) :: buf
       END FUNCTION h5dread_vl_string_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_vl_string_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, str_len)
    RETURN
  END SUBROUTINE h5dread_vl_string
#endif

!>
!! \ingroup FH5D
!!
!! \brief Returns dataset address in file.
!!
!! \param dset_id Dataset identifier.
!! \param offset  The offset in bytes.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Dget_offset()
!!
  SUBROUTINE h5dget_offset_f(dset_id, offset, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)    :: dset_id
    INTEGER(HADDR_T), INTENT(OUT) :: offset
    INTEGER, INTENT(OUT)          :: hdferr
    INTERFACE
       INTEGER(HADDR_T) FUNCTION h5dget_offset(dset_id) BIND(C,NAME='H5Dget_offset')
         IMPORT :: HID_T, HADDR_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: dset_id
       END FUNCTION h5dget_offset
    END INTERFACE

    offset = h5dget_offset(dset_id)

    hdferr = 0 ! never returns a function error because C API never returns a function error.

  END SUBROUTINE h5dget_offset_f

!>
!! \ingroup FH5D
!!
!! \brief Returns an identifier for a copy of the dataspace for a dataset.
!!
!! \param dataset_id   Dataset identifier.
!! \param dataspace_id Dataspace identifier.
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Dget_space()
!!
  SUBROUTINE h5dget_space_f(dataset_id, dataspace_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id
    INTEGER(HID_T), INTENT(OUT) :: dataspace_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Dget_space(dataset_id) BIND(C,NAME='H5Dget_space')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: dataset_id
       END FUNCTION H5Dget_space
    END INTERFACE

    dataspace_id = h5dget_space(dataset_id)

    hdferr = 0
    IF(dataspace_id.LT.0) hdferr = -1

  END SUBROUTINE h5dget_space_f

!>
!! \ingroup FH5D
!!
!! \brief Asynchronously returns an identifier for a copy of the dataspace for a dataset.
!!
!! \param dataset_id   Dataset identifier.
!! \param dataspace_id Dataspace identifier.
!! \param es_id        \fortran_es_id
!! \param hdferr       \fortran_error
!! \param file         \fortran_file
!! \param func         \fortran_func
!! \param line         \fortran_line
!!
!! See C API: @ref H5Dget_space_async()
!!
  SUBROUTINE h5dget_space_async_f(dataset_id, dataspace_id, es_id, hdferr, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id
    INTEGER(HID_T), INTENT(OUT) :: dataspace_id
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Dget_space_async(file, func, line, dataset_id, es_id) &
            BIND(C,NAME='H5Dget_space_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: dataset_id
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Dget_space_async
    END INTERFACE

    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    dataspace_id = H5Dget_space_async(file_default, func_default, line_default, &
         dataset_id, es_id)

    hdferr = 0
    IF(dataspace_id.LT.0) hdferr = -1

  END SUBROUTINE h5dget_space_async_f

!>
!! \ingroup FH5D
!!
!! \brief Returns a copy of the dataset creation property list.
!!
!! \param dset_id  Dataset identifier.
!! \param plist_id Dataset access property list identifier.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Dget_access_plist()
!!
  SUBROUTINE h5dget_access_plist_f(dset_id, plist_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: dset_id
    INTEGER(HID_T), INTENT(OUT) :: plist_id
    INTEGER       , INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5dget_access_plist_c(dset_id, plist_id) BIND(C,NAME='h5dget_access_plist_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(OUT) :: plist_id
       END FUNCTION h5dget_access_plist_c
    END INTERFACE

    hdferr = h5dget_access_plist_c(dset_id, plist_id)

  END SUBROUTINE h5dget_access_plist_f

!>
!! \ingroup FH5D
!!
!! \brief Reclaims VL datatype memory buffers.
!!
!! \param type_id  Identifier of the datatype.
!! \param space_id Identifier of the dataspace.
!! \param plist_id Identifier of the property list used to create the buffer.
!! \param buf      Pointer to the buffer to be reclaimed.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Dvlen_reclaim()
!!
  SUBROUTINE h5dvlen_reclaim_f(type_id, space_id, plist_id, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: type_id
    INTEGER(HID_T), INTENT(IN)  :: space_id
    INTEGER(HID_T), INTENT(IN)  :: plist_id
    TYPE(C_PTR)   , INTENT(IN)  :: buf
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5dvlen_reclaim_c(type_id, space_id, plist_id, buf) BIND(C, NAME='h5dvlen_reclaim_c')
         IMPORT :: C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T) :: type_id
         INTEGER(HID_T) :: space_id
         INTEGER(HID_T) :: plist_id
         TYPE(C_PTR), VALUE :: buf
       END FUNCTION h5dvlen_reclaim_c
    END INTERFACE

    hdferr = H5Dvlen_reclaim_c(type_id, space_id, plist_id, buf)

  END SUBROUTINE h5dvlen_reclaim_f

!>
!! \ingroup FH5D
!!
!! \brief Asynchronously reads raw data from a dataset into a buffer.
!!
!! \param dset_id       Identifier of the dataset read from.
!! \param mem_type_id   Identifier of the memory datatype.
!! \param buf           Buffer to receive data read from file.
!! \param es_id         \fortran_es_id
!! \param hdferr        \fortran_error
!! \param mem_space_id  Identifier of the memory dataspace.
!! \param file_space_id Identifier of dataset&apos;s dataspace in the file. (Default: H5S_ALL_F)
!! \param xfer_prp      Identifier of a transfer property list for this I/O operation.
!! \param file          \fortran_file
!! \param func          \fortran_func
!! \param line          \fortran_line
!!
!! See C API: @ref H5Dread_async()
!!
 SUBROUTINE h5dread_async_f(dset_id, mem_type_id, buf, es_id, hdferr, &
      mem_space_id, file_space_id, xfer_prp, file, func, line)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    TYPE(C_PTR), INTENT(IN)    :: buf
    INTEGER(HID_T), INTENT(IN) :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Dread_async(file, func, line, dset_id, mem_type_id, &
            mem_space_id, file_space_id, xfer_prp, buf, es_id) BIND(C,NAME='H5Dread_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: dset_id
         INTEGER(HID_T), VALUE :: mem_type_id
         INTEGER(HID_T), VALUE :: mem_space_id
         INTEGER(HID_T), VALUE :: file_space_id
         INTEGER(HID_T), VALUE :: xfer_prp
         TYPE(C_PTR)   , VALUE :: buf
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Dread_async
    END INTERFACE

    IF (PRESENT(file)) file_default = file
    IF (PRESENT(func)) func_default = func
    IF (PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = H5Dread_async(file_default, func_default, line_default, &
         dset_id, mem_type_id, mem_space_id, file_space_id, xfer_prp, buf, es_id )

  END SUBROUTINE h5dread_async_f

!>
!! \ingroup FH5D
!!
!! \brief Asynchronously writes raw data from a buffer to a dataset.
!!
!! \param dset_id       Identifier of the dataset to write to.
!! \param mem_type_id   Identifier of the memory datatype.
!! \param buf           Buffer with data to be written to the file.
!! \param es_id         \fortran_es_id
!! \param hdferr        \fortran_error
!! \param mem_space_id  Identifier of the memory dataspace.
!! \param file_space_id Identifier of the dataset&apos;s dataspace in the file.
!! \param xfer_prp      Identifier of a transfer property list for this I/O operation.
!! \param file          \fortran_file
!! \param func          \fortran_func
!! \param line          \fortran_line
!!
!! See C API: @ref H5Dwrite_async()
!!
 SUBROUTINE h5dwrite_async_f(dset_id, mem_type_id, buf, es_id, hdferr, &
      mem_space_id, file_space_id, xfer_prp, file, func, line)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    TYPE(C_PTR), INTENT(IN) :: buf
    INTEGER(HID_T), INTENT(IN) :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Dwrite_async(file, func, line, dset_id, mem_type_id, &
            mem_space_id, file_space_id, xfer_prp, buf, es_id) BIND(C,NAME='H5Dwrite_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: dset_id
         INTEGER(HID_T), VALUE :: mem_type_id
         INTEGER(HID_T), VALUE :: mem_space_id
         INTEGER(HID_T), VALUE :: file_space_id
         INTEGER(HID_T), VALUE :: xfer_prp
         TYPE(C_PTR)   , VALUE :: buf
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Dwrite_async
    END INTERFACE

    IF (PRESENT(file)) file_default = file
    IF (PRESENT(func)) func_default = func
    IF (PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = H5Dwrite_async(file_default, func_default, line_default, &
         dset_id, mem_type_id, mem_space_id, file_space_id, xfer_prp, buf, es_id)

  END SUBROUTINE h5dwrite_async_f

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5D
!!
!! \brief Writes raw data from a buffer to a dataset.
!!
!! \attention  \fortran_approved
!!
!! \param dset_id       Identifier of the dataset to write to.
!! \param mem_type_id   Identifier of the memory datatype.
!! \param buf           Buffer with data to be written to the file.
!! \param hdferr        \fortran_error
!! \param mem_space_id  Identifier of the memory dataspace.
!! \param file_space_id Identifier of the dataset&apos;s dataspace in the file.
!! \param xfer_prp      Identifier of a transfer property list for this I/O operation.
!!
!! See C API: @ref H5Dwrite()
!!
  SUBROUTINE h5dwrite_f(dset_id, mem_type_id, buf, hdferr, mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    TYPE(C_PTR), INTENT(IN) :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
  END SUBROUTINE h5dwrite
!>
!! \ingroup FH5D
!!
!! \brief Reads raw data from a dataset into a buffer (Passes Pointer).
!!
!! \attention  \fortran_approved
!!
!! \param dset_id       Identifier of the dataset read from.
!! \param mem_type_id   Identifier of the memory datatype.
!! \param buf           Buffer to receive data read from file.
!! \param hdferr        \fortran_error
!! \param mem_space_id  Identifier of the memory dataspace.
!! \param file_space_id Identifier of dataset&apos;s dataspace in the file. (Default: H5S_ALL_F)
!! \param xfer_prp      Identifier of a transfer property list for this I/O operation.
!!
!! See C API: @ref H5Dread()
!!
 SUBROUTINE h5dread_f(dset_id, mem_type_id, buf, hdferr, mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    TYPE(C_PTR), INTENT(IN) :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
  END SUBROUTINE h5dread_f

!>
!! \ingroup FH5D
!!
!! \brief There is no direct Fortran90 counterpart for the C function H5Dwrite. Instead, that
!!        functionality is provided by two Fortran90 subroutines:
!!        \li h5dwrite_f Purpose: Writes data other than variable-length data.
!!        \li h5dwrite_vl_f Purpose: Writes variable-length data.
!!
!! \attention  \fortran_obsolete
!!
!! \param dset_id       Identifier of the dataset to write to.
!! \param mem_type_id   Identifier of the memory datatype.
!! \param buf           Data buffer; may be a scalar or an array.
!! \param dims          Array to hold corresponding dimension sizes of data buffer buf; dim(k) has value.
!!                      of the k-th dimension of buffer buf; values are ignored if buf is a scalar.
!! \param hdferr        \fortran_error
!! \param mem_space_id  Identifier of the memory dataspace. Default value is H5S_ALL_F.
!! \param file_space_id Identifier of the dataset&apos;s dataspace in the file. Default value is H5S_ALL_F.
!! \param xfer_prp      Identifier of a transfer property list for this I/O operation. Default value is H5P_DEFAULT_F.
!!
  SUBROUTINE h5dwrite_f___F90_VERSION(dset_id, mem_type_id, buf, dims, hdferr, mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    TYPE(TYPE), INTENT(IN) :: buf
    DIMENSION(*), INTEGER(HSIZE_T), INTENT(IN)  :: dims
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
  END SUBROUTINE h5dwrite_f___F90_VERSION

!>
!! \ingroup FH5D
!!
!! \brief There is no direct Fortran90 counterpart for the C function H5Dread. Instead, that functionality
!!        is provided by two Fortran90 subroutines:
!!        \li h5dread_f    Purpose: Reads data other than variable-length data, uses DIMENSION argument and buf is not a pointer.
!!        \li h5dread_vl_f Purpose: Reads variable-length data.
!!
!! \attention  \fortran_obsolete
!!
!! \param dset_id       Identifier of the dataset read from.
!! \param mem_type_id   Identifier of the memory datatype.
!! \param buf           Buffer to receive data read from file, may be a scalar or an array.
!! \param dims          Array to hold corresponding dimension sizes of data buffer buf. dim(k) has value of the k-th.
!!                      dimension of buffer buf. Values are ignored if buf is a scalar.
!! \param hdferr        \fortran_error
!! \param mem_space_id  Identifier of the memory dataspace. (Default: H5S_ALL_F)
!! \param file_space_id Identifier of dataset&apos;s dataspace in the file. (Default: H5S_ALL_F)
!! \param xfer_prp      Identifier of a transfer property list for this I/O operation. (Default: H5P_DEFAULT_F)
!!
  SUBROUTINE h5dread_f___F90_VERSION(dset_id, mem_type_id, buf, dims, hdferr, mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    TYPE(TYPE), INTENT(INOUT) :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
  END SUBROUTINE h5dread_f___F90_VERSION

!>
!! \ingroup FH5D
!!
!! \brief Fills dataspace elements with a fill value in a memory buffer.
!!  Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes of the fillvalues and buffers are supported.
!!  Buffer and fillvalue are assumed to have the same datatype. Only one-dimesional buffers are supported.
!!
!! \attention  \fortran_obsolete
!!
!! \param fill_value Fill value.
!! \param space_id   Identifier of the memory datatype.
!! \param buf        Buffer to receive data read from file.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Dfill()
!!
  SUBROUTINE h5dfill_f(fill_value, space_id, buf,  hdferr)
    TYPE(TYPE), INTENT(IN) :: fill_value
    INTEGER(HID_T), INTENT(IN) :: space_id
    TYPE(TYPE), INTENT(OUT), DIMENSION(*) :: buf
    INTEGER, INTENT(OUT) :: hdferr
  END SUBROUTINE h5dfill_f
!>
!! \ingroup FH5D
!!
!! \brief Fills dataspace elements with a fill value in a memory buffer.
!!
!! \attention  \fortran_approved
!!
!! \param fill_value   Pointer to the fill value to be used.
!! \param fill_type_id Fill value datatype identifier,
!! \param buf          Pointer to the memory buffer containing the selection to be filled.
!! \param buf_type_id  Datatype of dataspace elements to be filled.
!! \param space_id     Dataspace identifier.
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Dfill()
!!
  SUBROUTINE h5dfill_f(fill_value, fill_type_id, buf, buf_type_id, space_id, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(IN)    :: fill_value
    INTEGER(HID_T), INTENT(IN) :: fill_type_id
    TYPE(C_PTR), INTENT(IN)    :: buf
    INTEGER(HID_T), INTENT(IN) :: buf_type_id
    INTEGER(HID_T), INTENT(IN) :: space_id
  END SUBROUTINE h5dfill_f

#else

  SUBROUTINE h5dwrite_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims
    TYPE(hobj_ref_t_f), DIMENSION(dims(1)), INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_reference_obj

  SUBROUTINE h5dwrite_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims
    TYPE(hdset_reg_ref_t_f), DIMENSION(dims(1)), INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
    INTEGER :: i
    INTEGER(HSIZE_T) :: j
    INTERFACE
       INTEGER FUNCTION h5dwrite_ref_reg_c(dset_id, mem_type_id,&
            mem_space_id_default, &
            file_space_id_default, xfer_prp_default, ref_buf, dims) &
            BIND(C,NAME='h5dwrite_ref_reg_c')
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER, DIMENSION(*) :: ref_buf
         INTEGER(HSIZE_T), DIMENSION(*) ::  dims
       END FUNCTION h5dwrite_ref_reg_c
    END INTERFACE
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    ALLOCATE(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
    IF (hdferr .NE. 0 ) THEN
       hdferr = -1
       RETURN
    ELSE
       DO j = 1, dims(1)
          DO i = 1, REF_REG_BUF_LEN
             ref_buf(REF_REG_BUF_LEN*(j-1) + i) = buf(j)%ref(i)
          ENDDO
       ENDDO
    ENDIF
    hdferr = h5dwrite_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, ref_buf, dims)
    DEALLOCATE(ref_buf)

  END SUBROUTINE h5dwrite_reference_dsetreg

  SUBROUTINE h5dwrite_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(*), INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    CALL h5dwrite_char_scalar_fix(dset_id, mem_type_id, buf, LEN(buf), dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)

  END SUBROUTINE h5dwrite_char_scalar

  SUBROUTINE h5dwrite_char_scalar_fix(dset_id, mem_type_id, buf, buf_len, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN) :: buf_len
    CHARACTER(LEN=buf_len), INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1:1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_scalar_fix

  SUBROUTINE h5dread_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    TYPE(hobj_ref_t_f), INTENT(INOUT) , &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_reference_obj

  SUBROUTINE h5dread_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    TYPE(hdset_reg_ref_t_f), INTENT(INOUT), &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
    INTEGER :: i
    INTEGER(HSIZE_T) :: j
    INTERFACE
       INTEGER FUNCTION h5dread_ref_reg_c(dset_id, mem_type_id,&
            mem_space_id_default, &
            file_space_id_default, xfer_prp_default, ref_buf, dims) &
            BIND(C,NAME='h5dread_ref_reg_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, DIMENSION(*) :: ref_buf
       END FUNCTION h5dread_ref_reg_c
    END INTERFACE

    ALLOCATE(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, ref_buf, dims)

    DO j = 1, dims(1)
       DO i = 1, REF_REG_BUF_LEN
          buf(j)%ref(i) = ref_buf(REF_REG_BUF_LEN*(j-1) + i)
       ENDDO
    ENDDO
    DEALLOCATE(ref_buf)

  END SUBROUTINE h5dread_reference_dsetreg

  SUBROUTINE h5dread_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims

    CHARACTER(LEN=*), INTENT(INOUT) :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    CALL h5dread_char_scalar_fix(dset_id, mem_type_id, buf, LEN(buf), hdferr, &
         mem_space_id_default, file_space_id_default, xfer_prp_default)

  END SUBROUTINE h5dread_char_scalar

  SUBROUTINE h5dread_char_scalar_fix(dset_id, mem_type_id, buf, buf_len, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER, INTENT(IN)  :: buf_len
    CHARACTER(LEN=buf_len), INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1:1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id, &
         file_space_id, xfer_prp, f_ptr)

  END SUBROUTINE h5dread_char_scalar_fix

  SUBROUTINE h5dwrite_ptr(dset_id, mem_type_id, buf, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    TYPE(C_PTR), INTENT(IN) :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf)

  END SUBROUTINE h5dwrite_ptr

  SUBROUTINE h5dread_ptr(dset_id, mem_type_id, buf, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    TYPE(C_PTR), INTENT(IN) :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf)

  END SUBROUTINE h5dread_ptr

 SUBROUTINE h5dfill_ptr(fill_value, fill_type_id, buf, buf_type_id, space_id, hdferr)
    IMPLICIT NONE
    TYPE(C_PTR)   , INTENT(IN) :: fill_value
    INTEGER(HID_T), INTENT(IN) :: fill_type_id
    TYPE(C_PTR)   , INTENT(IN) :: buf
    INTEGER(HID_T), INTENT(IN) :: buf_type_id
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5dfill(fill_value, fill_type_id, buf, buf_type_id, space_id) &
            BIND(C,NAME='H5Dfill')
         IMPORT :: HID_T, C_PTR
         IMPLICIT NONE
         TYPE(C_PTR)   , VALUE :: fill_value
         INTEGER(HID_T), VALUE :: fill_type_id
         TYPE(C_PTR)   , VALUE :: buf
         INTEGER(HID_T), VALUE :: buf_type_id
         INTEGER(HID_T), VALUE :: space_id
       END FUNCTION h5dfill
    END INTERFACE

    hdferr = INT(h5dfill(fill_value, fill_type_id, buf, buf_type_id, space_id))

  END SUBROUTINE h5dfill_ptr

  SUBROUTINE h5dfill_integer(fill_value, space_id, buf,  hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN), TARGET :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    INTEGER, INTENT(OUT), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id  ! Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf        ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf(1))

    fill_type_id = h5kind_to_type(KIND(fill_value), H5_INTEGER_KIND)
    mem_type_id  = fill_type_id

    CALL h5dfill_ptr(f_ptr_fill_value, fill_type_id, f_ptr_buf, mem_type_id, space_id, hdferr)

  END SUBROUTINE h5dfill_integer

  SUBROUTINE h5dfill_c_float(fill_value, space_id, buf,  hdferr)
    IMPLICIT NONE
    REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: fill_value
    INTEGER(HID_T), INTENT(IN) :: space_id
    REAL(KIND=C_FLOAT), INTENT(OUT), DIMENSION(*), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf(1))

    fill_type_id = h5kind_to_type(KIND(fill_value), H5_REAL_KIND)
    mem_type_id  = fill_type_id

    CALL h5dfill_ptr(f_ptr_fill_value, fill_type_id, f_ptr_buf, mem_type_id, space_id, hdferr)

  END SUBROUTINE h5dfill_c_float

  SUBROUTINE h5dfill_c_double(fill_value, space_id, buf,  hdferr)
    IMPLICIT NONE
    REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: fill_value
    INTEGER(HID_T), INTENT(IN) :: space_id
    REAL(KIND=C_DOUBLE), INTENT(OUT), DIMENSION(*), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf(1))

    fill_type_id = h5kind_to_type(KIND(fill_value), H5_REAL_KIND)
    mem_type_id  = fill_type_id

    CALL h5dfill_ptr(f_ptr_fill_value, fill_type_id, f_ptr_buf, mem_type_id, space_id, hdferr)

  END SUBROUTINE h5dfill_c_double

#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
  SUBROUTINE h5dfill_c_long_double(fill_value, space_id, buf,  hdferr)
    IMPLICIT NONE
    REAL(KIND=C_LONG_DOUBLE), INTENT(IN), TARGET :: fill_value
    INTEGER(HID_T), INTENT(IN) :: space_id
    REAL(KIND=C_LONG_DOUBLE), INTENT(OUT), DIMENSION(*), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf(1))

    fill_type_id = h5kind_to_type(KIND(fill_value), H5_REAL_KIND)
    mem_type_id  = fill_type_id

    CALL h5dfill_ptr(f_ptr_fill_value, fill_type_id, f_ptr_buf, mem_type_id, space_id, hdferr)

  END SUBROUTINE h5dfill_c_long_double
#endif

  SUBROUTINE h5dfill_char(fill_value, space_id, buf, hdferr)
    IMPLICIT NONE
    CHARACTER, INTENT(IN), TARGET :: fill_value
    INTEGER(HID_T), INTENT(IN) :: space_id
    CHARACTER, INTENT(OUT), DIMENSION(*), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf(1))

    fill_type_id = H5T_NATIVE_CHARACTER
    mem_type_id  = H5T_NATIVE_CHARACTER

    CALL h5dfill_ptr(f_ptr_fill_value, fill_type_id, f_ptr_buf, mem_type_id, space_id, hdferr)

  END SUBROUTINE h5dfill_char

#endif

!>
!! \ingroup FH5D
!!
!! \brief Reads data from a file to memory buffers for multiple datasets.
!!
!! \param count         Number of datasets to write to.
!! \param dset_id       Identifier of the dataset to write to.
!! \param mem_type_id   Identifier of the memory datatype.
!! \param mem_space_id  Identifier of the memory dataspace.
!! \param file_space_id Identifier of the dataset&apos;s dataspace in the file.
!! \param buf           Buffer with data to be written to the file.
!! \param hdferr        \fortran_error
!! \param xfer_prp      Identifier of a transfer property list for this I/O operation.
!!
!! See C API: @ref H5Dread_multi()
!!
  SUBROUTINE h5dread_multi_f(count, dset_id, mem_type_id, mem_space_id, file_space_id, buf, hdferr, xfer_prp)
    IMPLICIT NONE

    INTEGER(SIZE_T),      INTENT(IN)               :: count
    INTEGER(HID_T),       INTENT(IN), DIMENSION(*) :: dset_id
    INTEGER(HID_T),       INTENT(IN), DIMENSION(*) :: mem_type_id
    INTEGER(HID_T),       INTENT(IN), DIMENSION(*) :: mem_space_id
    INTEGER(HID_T),       INTENT(IN), DIMENSION(*) :: file_space_id
    TYPE(C_PTR),          INTENT(IN), DIMENSION(*) :: buf
    INTEGER,              INTENT(OUT)              :: hdferr
    INTEGER(HID_T),       INTENT(IN), OPTIONAL     :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default

    INTERFACE
       INTEGER FUNCTION H5Dread_multi(count, dset_id, mem_type_id, mem_space_id, file_space_id, xfer_prp, buf) &
            BIND(C, NAME='H5Dread_multi')
         IMPORT :: SIZE_T
         IMPORT :: HID_T
         IMPORT :: C_PTR
         IMPLICIT NONE
         INTEGER(SIZE_T), VALUE :: count
         INTEGER(HID_T), DIMENSION(*) :: dset_id
         INTEGER(HID_T), DIMENSION(*) :: mem_type_id
         INTEGER(HID_T), DIMENSION(*) :: mem_space_id
         INTEGER(HID_T), DIMENSION(*) :: file_space_id
         INTEGER(HID_T), VALUE :: xfer_prp
         TYPE(C_PTR), DIMENSION(*) :: buf
       END FUNCTION H5Dread_multi
    END INTERFACE 

    xfer_prp_default = H5P_DEFAULT_F
    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp

    hdferr = H5Dread_multi(count, dset_id, mem_type_id, mem_space_id, file_space_id, xfer_prp_default, buf)

  END SUBROUTINE h5dread_multi_f

!>
!! \ingroup FH5D
!!
!! \brief Writes data in memory to a file for multiple datasets.
!!
!! \param count         Number of datasets to write to.
!! \param dset_id       Identifier of the dataset to write to.
!! \param mem_type_id   Identifier of the memory datatype.
!! \param mem_space_id  Identifier of the memory dataspace.
!! \param file_space_id Identifier of the dataset&apos;s dataspace in the file.
!! \param buf           Buffer with data to be written to the file.
!! \param hdferr        \fortran_error
!! \param xfer_prp      Identifier of a transfer property list for this I/O operation.
!!
!! See C API: @ref H5Dwrite_multi()
!!
  SUBROUTINE h5dwrite_multi_f(count, dset_id, mem_type_id, mem_space_id, file_space_id, buf, hdferr, xfer_prp)
    IMPLICIT NONE

    INTEGER(SIZE_T),      INTENT(IN)               :: count
    INTEGER(HID_T),       INTENT(IN), DIMENSION(*) :: dset_id
    INTEGER(HID_T),       INTENT(IN), DIMENSION(*) :: mem_type_id
    INTEGER(HID_T),       INTENT(IN), DIMENSION(*) :: mem_space_id
    INTEGER(HID_T),       INTENT(IN), DIMENSION(*) :: file_space_id
    TYPE(C_PTR),          INTENT(IN), DIMENSION(*) :: buf
    INTEGER,              INTENT(OUT)              :: hdferr
    INTEGER(HID_T),       INTENT(IN), OPTIONAL     :: xfer_prp

    INTEGER(HID_T) :: xfer_prp_default

    INTERFACE
       INTEGER FUNCTION H5Dwrite_multi(count, dset_id, mem_type_id, mem_space_id, file_space_id, xfer_prp, buf) &
            BIND(C, NAME='H5Dwrite_multi')
         IMPORT :: SIZE_T
         IMPORT :: HID_T
         IMPORT :: C_PTR
         IMPLICIT NONE
         INTEGER(SIZE_T), VALUE :: count
         INTEGER(HID_T), DIMENSION(*) :: dset_id
         INTEGER(HID_T), DIMENSION(*) :: mem_type_id
         INTEGER(HID_T), DIMENSION(*) :: mem_space_id
         INTEGER(HID_T), DIMENSION(*) :: file_space_id
         INTEGER(HID_T), VALUE :: xfer_prp
         TYPE(C_PTR), DIMENSION(*) :: buf
       END FUNCTION H5Dwrite_multi
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp

    hdferr = H5Dwrite_multi(count, dset_id, mem_type_id, mem_space_id, file_space_id, xfer_prp_default, buf)

  END SUBROUTINE h5dwrite_multi_f

!>
!! \ingroup FH5D
!!
!! \brief Reads a raw data chunk directly from a dataset in a file into a buffer.
!!
!! \param dset_id   Identifier of the dataset to read from
!! \param offset    Logical position of the chunk&apos;s first element in the dataspace, \Bold{0-based indices}
!! \param filters   Mask for identifying the filters in use
!! \param buf       Buffer containing data to be read from the chunk
!! \param hdferr    \fortran_error
!! \param dxpl_id   Dataset transfer property list identifier
!!
!! See C API: @ref H5Dread_chunk()
!!
  SUBROUTINE h5dread_chunk_f(dset_id, offset, filters, buf, hdferr, dxpl_id)
    IMPLICIT NONE

    INTEGER(HID_T)    , INTENT(IN)  :: dset_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(:) :: offset
    INTEGER           , INTENT(INOUT) :: filters
    TYPE(C_PTR)                     :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    INTEGER(HID_T)    , INTENT(IN), OPTIONAL :: dxpl_id

    INTEGER(HID_T) :: dxpl_id_default
    INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: c_offset
    INTEGER(HSIZE_T) :: i, rank
    INTEGER(C_INT32_T) :: c_filters

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Dread_chunk(dset_id, dxpl_id, offset, filters, buf) &
            BIND(C, NAME='H5Dread_chunk')
         IMPORT :: SIZE_T, HSIZE_T, HID_T
         IMPORT :: C_PTR, C_INT32_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T)    , VALUE :: dset_id
         INTEGER(HID_T)    , VALUE :: dxpl_id
         INTEGER(HSIZE_T)  , DIMENSION(*) :: offset
         INTEGER(C_INT32_T) :: filters
         TYPE(C_PTR)       , VALUE :: buf
       END FUNCTION H5Dread_chunk
    END INTERFACE

    dxpl_id_default = H5P_DEFAULT_F
    IF (PRESENT(dxpl_id)) dxpl_id_default = dxpl_id

    c_filters = INT(filters, KIND=C_INT32_T)

    rank = SIZE(offset, KIND=HSIZE_T)

    ALLOCATE(c_offset(rank), STAT=hdferr)
    IF (hdferr .NE. 0 ) THEN
       hdferr = -1
       RETURN
    ENDIF

    !
    ! Reverse dimensions due to C-FORTRAN storage order
    !
    DO i = 1, rank
       c_offset(i) = offset(rank - i + 1)
    ENDDO

    hdferr = INT(H5Dread_chunk(dset_id, dxpl_id_default, c_offset, c_filters, buf))

    filters = INT(c_filters)

    DEALLOCATE(c_offset)

  END SUBROUTINE h5dread_chunk_f

!>
!! \ingroup FH5D
!!
!! \brief Writes a raw data chunk from a buffer directly to a dataset in a file.
!!
!! \param dset_id    Identifier of the dataset to write to
!! \param filters    Mask for identifying the filters in use
!! \param offset     Logical position of the chunk&apos;s first element in the dataspace, \Bold{0-based indices}
!! \param data_size  Size of the actual data to be written in bytes
!! \param buf        Buffer containing data to be written to the chunk
!! \param hdferr     \fortran_error
!! \param dxpl_id    Dataset transfer property list identifier
!!
!! See C API: @ref H5Dwrite_chunk()
!!
  SUBROUTINE h5dwrite_chunk_f(dset_id, filters, offset, data_size, buf, hdferr, dxpl_id)
    IMPLICIT NONE

    INTEGER(HID_T)    , INTENT(IN) :: dset_id
    INTEGER           , INTENT(IN) :: filters
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(:) :: offset
    INTEGER(SIZE_T)   , INTENT(IN)  :: data_size
    TYPE(C_PTR)                     :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    INTEGER(HID_T)    , INTENT(IN), OPTIONAL :: dxpl_id

    INTEGER(HID_T) :: dxpl_id_default
    INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: c_offset
    INTEGER(HSIZE_T) :: i, rank
    INTEGER(C_INT32_T) :: c_filters

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Dwrite_chunk(dset_id, dxpl_id, filters, offset, data_size, buf) &
            BIND(C, NAME='H5Dwrite_chunk')
         IMPORT :: SIZE_T, HSIZE_T, HID_T
         IMPORT :: C_PTR, C_INT32_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T)    , VALUE      :: dset_id
         INTEGER(HID_T)    , VALUE      :: dxpl_id
         INTEGER(C_INT32_T), VALUE      :: filters
         INTEGER(HSIZE_T), DIMENSION(*) :: offset
         INTEGER(SIZE_T)   , VALUE      :: data_size
         TYPE(C_PTR)       , VALUE      :: buf
       END FUNCTION H5Dwrite_chunk
    END INTERFACE

    dxpl_id_default = H5P_DEFAULT_F
    IF (PRESENT(dxpl_id)) dxpl_id_default = dxpl_id

    rank = SIZE(offset, KIND=HSIZE_T)

    ALLOCATE(c_offset(rank), STAT=hdferr)
    IF (hdferr .NE. 0 ) THEN
       hdferr = -1
       RETURN
    ENDIF

    !
    ! Reverse dimensions due to C-FORTRAN storage order
    !
    DO i = 1, rank
       c_offset(i) = offset(rank - i + 1)
    ENDDO

    c_filters = INT(filters, C_INT32_T)

    hdferr = INT(H5Dwrite_chunk(dset_id, dxpl_id_default, c_filters, c_offset, data_size, buf))

    DEALLOCATE(c_offset)

  END SUBROUTINE h5dwrite_chunk_f

END MODULE H5D


