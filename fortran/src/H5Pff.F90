!> @defgroup FH5P Fortran Property List (H5P) Interface
!!
!! @see H5P, C-API
!!
!! @see @ref H5P_UG, User Guide
!!

!> @ingroup FH5P
!!
!! @brief This module contains Fortran interfaces for H5P functions.
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
!
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5P function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.

#include <H5config_f.inc>

MODULE H5P

#ifdef H5_HAVE_PARALLEL
#ifdef H5_HAVE_MPI_F08
  USE MPI_F08, ONLY : MPI_INTEGER_KIND
#else
  USE MPI, ONLY : MPI_INTEGER_KIND
#endif
#endif
  USE H5GLOBAL
  USE H5fortkit

  PRIVATE h5pset_fapl_multi_l, h5pset_fapl_multi_s
  PRIVATE h5pset_fill_value_integer, h5pset_fill_value_char, h5pset_fill_value_ptr
  PRIVATE h5pget_fill_value_integer, h5pget_fill_value_char, h5pget_fill_value_ptr
  PRIVATE h5pset_integer, h5pset_char, h5pset_ptr
  PRIVATE h5pget_integer, h5pget_char, h5pget_ptr
  PRIVATE h5pregister_integer, h5pregister_ptr
  PRIVATE h5pinsert_integer, h5pinsert_char, h5pinsert_ptr
#ifdef H5_HAVE_PARALLEL
  PRIVATE MPI_INTEGER_KIND
  PRIVATE h5pset_fapl_mpio_f90, h5pget_fapl_mpio_f90
#ifdef H5_HAVE_MPI_F08
  PRIVATE h5pset_fapl_mpio_f08, h5pget_fapl_mpio_f08
#endif
#endif

#ifndef H5_DOXYGEN

  INTERFACE h5pset_fapl_multi_f
     MODULE PROCEDURE h5pset_fapl_multi_l
     MODULE PROCEDURE h5pset_fapl_multi_s
  END INTERFACE

  INTERFACE h5pset_fill_value_f
     MODULE PROCEDURE h5pset_fill_value_integer
     MODULE PROCEDURE h5pset_fill_value_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pset_fill_value_ptr

  END INTERFACE

  INTERFACE h5pget_fill_value_f
     MODULE PROCEDURE h5pget_fill_value_integer
     MODULE PROCEDURE h5pget_fill_value_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pget_fill_value_ptr

  END INTERFACE

  INTERFACE h5pset_f
     MODULE PROCEDURE h5pset_integer
     MODULE PROCEDURE h5pset_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pset_ptr

  END INTERFACE

  INTERFACE h5pget_f
     MODULE PROCEDURE h5pget_integer
     MODULE PROCEDURE h5pget_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pget_ptr
  END INTERFACE

  INTERFACE h5pregister_f
     MODULE PROCEDURE h5pregister_integer
     ! Recommended procedure:
     MODULE PROCEDURE h5pregister_ptr
  END INTERFACE

  INTERFACE h5pinsert_f
     MODULE PROCEDURE h5pinsert_integer
     MODULE PROCEDURE h5pinsert_char
     ! Recommended procedure:
     MODULE PROCEDURE h5pinsert_ptr
  END INTERFACE

  INTERFACE
     INTEGER(C_INT) FUNCTION H5Pset_fill_value(prp_id, type_id, fillvalue) &
          BIND(C, NAME='H5Pset_fill_value')
       IMPORT :: C_INT, C_PTR
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(hid_t), VALUE :: prp_id
       INTEGER(hid_t), VALUE :: type_id
       TYPE(C_PTR)   , VALUE :: fillvalue
     END FUNCTION H5Pset_fill_value
  END INTERFACE

  INTERFACE
     INTEGER(C_INT) FUNCTION H5Pget_fill_value(prp_id, type_id, fillvalue) &
          BIND(C, NAME='H5Pget_fill_value')
       IMPORT :: C_INT, C_PTR
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(hid_t), VALUE :: prp_id
       INTEGER(hid_t), VALUE :: type_id
       TYPE(C_PTR)   , VALUE :: fillvalue
     END FUNCTION H5Pget_fill_value
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pset_c(prp_id, name, name_len, value) &
          BIND(C, NAME='h5pset_c')
       IMPORT :: c_char, c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: prp_id
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
       INTEGER :: name_len
       TYPE(C_PTR), VALUE :: value
     END FUNCTION h5pset_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pget_c(prp_id, name, name_len, value) &
          BIND(C, NAME='h5pget_c')
       IMPORT :: c_char, c_ptr
       IMPORT :: HID_T
       INTEGER(HID_T), INTENT(IN) :: prp_id
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
       INTEGER :: name_len
       TYPE(C_PTR), VALUE :: value
     END FUNCTION h5pget_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pregister_c(class, name, name_len, size, value) &
          BIND(C, NAME='h5pregister_c')
       IMPORT :: c_char, c_ptr
       IMPORT :: HID_T, SIZE_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: class
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
       INTEGER, INTENT(IN)         :: name_len
       INTEGER(SIZE_T), INTENT(IN) :: size
       TYPE(C_PTR), INTENT(IN), VALUE :: value
     END FUNCTION h5pregister_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5pinsert_c(plist, name, name_len, size, value) &
          BIND(C, NAME='h5pinsert_c')
       IMPORT :: c_char, c_ptr
       IMPORT :: HID_T, SIZE_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: plist
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
       INTEGER, INTENT(IN)         :: name_len
       INTEGER(SIZE_T), INTENT(IN) :: size
       TYPE(C_PTR), INTENT(IN), VALUE :: value
     END FUNCTION h5pinsert_c
  END INTERFACE

#endif

#ifdef H5_HAVE_PARALLEL

  INTERFACE h5pset_fapl_mpio_f
      MODULE PROCEDURE h5pset_fapl_mpio_f90
#ifdef H5_HAVE_MPI_F08
      MODULE PROCEDURE h5pset_fapl_mpio_f08
#endif
    END INTERFACE

  INTERFACE h5pget_fapl_mpio_f
    MODULE PROCEDURE h5pget_fapl_mpio_f90      
#ifdef H5_HAVE_MPI_F08  
    MODULE PROCEDURE h5pget_fapl_mpio_f08
#endif
  END INTERFACE

  INTERFACE H5Pset_mpi_params_f
    MODULE PROCEDURE H5Pset_mpi_params_f90
#ifdef H5_HAVE_MPI_F08
    MODULE PROCEDURE H5Pset_mpi_params_f08
#endif
  END INTERFACE

  INTERFACE H5Pget_mpi_params_f
    MODULE PROCEDURE H5Pget_mpi_params_f90
#ifdef H5_HAVE_MPI_F08
    MODULE PROCEDURE H5Pget_mpi_params_f08
#endif
  END INTERFACE

#ifdef H5_HAVE_SUBFILING_VFD
!> \addtogroup FH5P
!> @{

  !> @brief H5FD_subfiling_params_t derived type used in the subfiling VFD.
  TYPE, BIND(C) :: H5FD_subfiling_params_t
    INTEGER(C_INT)    :: ioc_selection !< Method to select I/O concentrators
    INTEGER(C_INT64_T) :: stripe_size   !< Size (in bytes) of data stripes in subfiles
    INTEGER(C_INT32_T) :: stripe_count  !< Target number of subfiles to use
  END TYPE H5FD_subfiling_params_t

  !> @brief H5FD_subfiling_config_t derived type used in the subfiling VFD.
  TYPE, BIND(C) :: H5FD_subfiling_config_t
    INTEGER(C_INT32_T) :: magic                  !< Set to H5FD_SUBFILING_FAPL_MAGIC_F
    INTEGER(C_INT32_T) :: version                !< Set to H5FD_CURR_SUBFILING_FAPL_VERSION_F
    INTEGER(HID_T)     :: ioc_fapl_id            !< The FAPL setup with the stacked VFD to use for I/O concentrators
    LOGICAL(C_BOOL)    :: require_ioc            !< Whether to use the IOC VFD (currently must always be TRUE)
    TYPE(H5FD_subfiling_params_t) :: shared_cfg  !< Subfiling/IOC parameters (stripe size, stripe count, etc.)
  END TYPE H5FD_subfiling_config_t

  !> @brief H5FD_ioc_config_t derived type used in the IOC VFD (SUBFILING).
  TYPE, BIND(C) :: H5FD_ioc_config_t
    INTEGER(C_INT32_T) :: magic            !< Must be set to H5FD_IOC_FAPL_MAGIC_F
    INTEGER(C_INT32_T) :: version          !< Must be set to H5FD_IOC_CURR_FAPL_VERSION_F
    INTEGER(C_INT32_T) :: thread_pool_size !< Number of I/O concentrator worker threads to use
  END TYPE H5FD_ioc_config_t
!> @}
#endif
#endif

CONTAINS

!>
!! \ingroup FH5P
!!
!! \brief Creates a new property as an instance of a property list class.
!!
!! \param class  Type of the property class to be created. Possible values are:
!!               \li H5P_OBJECT_CREATE_F
!!               \li H5P_FILE_CREATE_F
!!               \li H5P_FILE_ACCESS_F
!!               \li H5P_DATASET_CREATE_F
!!               \li H5P_DATASET_ACCESS_F
!!               \li H5P_DATASET_XFER_F
!!               \li H5P_FILE_MOUNT_F
!!               \li H5P_GROUP_CREATE_F
!!               \li H5P_GROUP_ACCESS_F
!!               \li H5P_DATATYPE_CREATE_F
!!               \li H5P_DATATYPE_ACCESS_F
!!               \li H5P_STRING_CREATE_F
!!               \li H5P_ATTRIBUTE_CREATE _F
!!               \li H5P_OBJECT_COPY_F
!!               \li H5P_LINK_CREATE_F
!!               \li H5P_LINK_ACCESS_F
!! \param prp_id Property list identifier.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pcreate()
!!
  SUBROUTINE h5pcreate_f(class, prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: class
    INTEGER(HID_T), INTENT(OUT) :: prp_id
    INTEGER       , INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pcreate_c(class, prp_id) &
            BIND(C,NAME='h5pcreate_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: class
         INTEGER(HID_T), INTENT(OUT) :: prp_id
       END FUNCTION h5pcreate_c
    END INTERFACE

    hdferr = h5pcreate_c(class, prp_id)
  END SUBROUTINE h5pcreate_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the dataset transfer property list status to TRUE or FALSE for initializing
!!        compound datatype members during write/read operations.
!!
!! \param prp_id Property list identifier.
!! \param flag   Status flag.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_preserve()
!!
  SUBROUTINE h5pset_preserve_f(prp_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    LOGICAL, INTENT(IN) ::  flag
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: flag_c

    INTERFACE
       INTEGER FUNCTION h5pset_preserve_c(prp_id, flag_c) &
            BIND(C,NAME='h5pset_preserve_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER ::  flag_c
       END FUNCTION h5pset_preserve_c
    END INTERFACE
    flag_c = 0
    IF(flag) flag_c = 1
    hdferr = h5pset_preserve_c(prp_id, flag_c)
  END SUBROUTINE h5pset_preserve_f

!>
!! \ingroup FH5P
!!
!! \brief Checks status of the dataset transfer property list.
!!
!! \param prp_id Property list identifier.
!! \param flag   Status flag.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget_preserve()
!!
  SUBROUTINE h5pget_preserve_f(prp_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    LOGICAL, INTENT(OUT) ::  flag
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: flag_c

    INTERFACE
       INTEGER FUNCTION h5pget_preserve_c(prp_id, flag_c) &
            BIND(C,NAME='h5pget_preserve_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER ::  flag_c
       END FUNCTION h5pget_preserve_c
    END INTERFACE

    hdferr = h5pget_preserve_c(prp_id, flag_c)
    flag = .FALSE.
    IF(flag_c .EQ. 1) flag = .TRUE.
  END SUBROUTINE h5pget_preserve_f

!>
!! \ingroup FH5P
!!
!! \brief Returns the property list class for a property list.
!!
!! \param prp_id    Property list identifier.
!! \param classtype Property list class.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pget_class()
!!
  SUBROUTINE h5pget_class_f(prp_id, classtype, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(OUT) :: classtype
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pget_class_c(prp_id, classtype) &
            BIND(C,NAME='h5pget_class_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN)  :: prp_id
         INTEGER(HID_T), INTENT(OUT) :: classtype
       END FUNCTION h5pget_class_c
    END INTERFACE

    hdferr = h5pget_class_c(prp_id, classtype)
  END SUBROUTINE h5pget_class_f

!>
!! \ingroup FH5P
!!
!! \brief Copies an existing property list to create a new property list
!!
!! \param prp_id     Property list identifier.
!! \param new_prp_id New property list identifier.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Pcopy()
!!
  SUBROUTINE h5pcopy_f(prp_id, new_prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(OUT) :: new_prp_id
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pcopy_c(prp_id, new_prp_id) &
            BIND(C,NAME='h5pcopy_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(OUT) :: new_prp_id
       END FUNCTION h5pcopy_c
    END INTERFACE

    hdferr = h5pcopy_c(prp_id, new_prp_id)
  END SUBROUTINE h5pcopy_f

!>
!! \ingroup FH5P
!!
!! \brief Terminates access to a property list.
!!
!! \param prp_id Identifier of the property list to terminate access to.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pclose()
!!
  SUBROUTINE h5pclose_f(prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pclose(prp_id) &
            BIND(C,NAME='H5Pclose')
         IMPORT :: C_INT
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: prp_id
       END FUNCTION H5Pclose
    END INTERFACE

    hdferr = INT(H5Pclose(prp_id))
  END SUBROUTINE h5pclose_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the size of the chunks used to store
!!       a chunked layout dataset.
!!
!! \param prp_id Dataset creation property list identifier.
!! \param ndims  Number of dimensions for each chunk.
!! \param dims   Array with dimension sizes for each chunk.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_chunk()
!!
  SUBROUTINE h5pset_chunk_f(prp_id, ndims, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: ndims
    INTEGER(HSIZE_T), DIMENSION(ndims), INTENT(IN) :: dims
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_chunk_c(prp_id, ndims, dims) &
            BIND(C,NAME='h5pset_chunk_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: ndims
         INTEGER(HSIZE_T), DIMENSION(ndims), INTENT(IN) :: dims
       END FUNCTION h5pset_chunk_c
    END INTERFACE

    hdferr =  h5pset_chunk_c(prp_id, ndims, dims)
  END SUBROUTINE h5pset_chunk_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the size of chunks for the raw data of a chunked layout dataset
!!
!! \param prp_id Property list identifier.
!! \param ndims  Size of dims array.
!! \param dims   Array with dimension sizes for each chunk.
!! \param hdferr Returns number of chunk dimensions if successful and -1 if fails.
!!
!! See C API: @ref H5Pget_chunk()
!!
  SUBROUTINE h5pget_chunk_f(prp_id, ndims, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: ndims
    INTEGER(HSIZE_T), DIMENSION(ndims), INTENT(OUT) :: dims
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pget_chunk_c(prp_id, ndims, dims) &
            BIND(C,NAME='h5pget_chunk_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER :: ndims
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: dims
       END FUNCTION h5pget_chunk_c
    END INTERFACE

    hdferr =  h5pget_chunk_c(prp_id, ndims, dims)
  END SUBROUTINE h5pget_chunk_f

!>
!! \ingroup FH5P
!!
!! \brief Sets compression method and compression level.
!!
!! \param prp_id Property list identifier.
!! \param level  Compression level.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_deflate()
!!
  SUBROUTINE h5pset_deflate_f(prp_id, level, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: level
    INTEGER, INTENT(OUT) :: hdferr

!  INTEGER, EXTERNAL :: h5pset_deflate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_deflate_c(prp_id, level) &
            BIND(C,NAME='h5pset_deflate_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: level
       END FUNCTION h5pset_deflate_c
    END INTERFACE
    hdferr = h5pset_deflate_c(prp_id, level)

  END SUBROUTINE h5pset_deflate_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the version information of various objects for a file creation property list.
!!
!! \param prp_id   File creation property list identifier.
!! \param boot     Super block version number.
!! \param freelist Global freelist version number.
!! \param stab     Symbol table version number.
!! \param shhdr    Shared object header version number.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_version()
!!
  SUBROUTINE h5pget_version_f(prp_id, boot, freelist, stab, shhdr, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, DIMENSION(*), INTENT(OUT) :: boot
    INTEGER, DIMENSION(*), INTENT(OUT) :: freelist
    INTEGER, DIMENSION(*), INTENT(OUT) :: stab
    INTEGER, DIMENSION(*), INTENT(OUT) :: shhdr
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pget_version_c(prp_id, boot, freelist, stab, shhdr) &
            BIND(C,NAME='h5pget_version_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, DIMENSION(*), INTENT(OUT) :: boot
         INTEGER, DIMENSION(*), INTENT(OUT) :: freelist
         INTEGER, DIMENSION(*), INTENT(OUT) :: stab
         INTEGER, DIMENSION(*), INTENT(OUT) :: shhdr
       END FUNCTION h5pget_version_c
    END INTERFACE

    hdferr = h5pget_version_c(prp_id, boot, freelist, stab, shhdr)
  END SUBROUTINE h5pget_version_f

!>
!! \ingroup FH5P
!!
!! \brief Sets user block size.
!!
!! \param prp_id File creation property list to modify.
!! \param size   Size of the user-block in bytes.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_userblock()
!!
  SUBROUTINE h5pset_userblock_f(prp_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HSIZE_T), INTENT(IN) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION H5Pset_userblock(prp_id, size) &
            BIND(C,NAME='H5Pset_userblock')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , VALUE :: prp_id
         INTEGER(HSIZE_T), VALUE :: size
       END FUNCTION H5Pset_userblock
    END INTERFACE

    hdferr = H5Pset_userblock(prp_id, size)

  END SUBROUTINE h5pset_userblock_f

!>
!! \ingroup FH5P
!!
!! \brief Gets user block size.
!!
!! \param prp_id     File creation property list identifier.
!! \param block_size Size of the user block in bytes.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Pget_userblock()
!!
  SUBROUTINE h5pget_userblock_f(prp_id, block_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HSIZE_T), INTENT(OUT) ::  block_size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION H5Pget_userblock(prp_id, block_size) &
            BIND(C,NAME='H5Pget_userblock')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , VALUE :: prp_id
         INTEGER(HSIZE_T)        :: block_size
       END FUNCTION H5Pget_userblock
    END INTERFACE

    hdferr = H5Pget_userblock(prp_id,  block_size)

  END SUBROUTINE h5pget_userblock_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the selection I/O mode
!!
!! \param plist_id          \fortran_plist_id
!! \param selection_io_mode The selection I/O mode
!! \param hdferr            \fortran_error
!!
!! See C API: @ref H5Pset_selection_io()
!!
  SUBROUTINE h5pset_selection_io_f(plist_id, selection_io_mode, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER, INTENT(IN)        :: selection_io_mode
    INTEGER, INTENT(OUT)       :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pset_selection_io(plist_id, selection_io_mode) BIND(C, NAME='H5Pset_selection_io')
         IMPORT :: HID_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: plist_id
         INTEGER(C_INT), VALUE :: selection_io_mode
       END FUNCTION H5Pset_selection_io
    END INTERFACE

    hdferr = INT(H5Pset_selection_io(plist_id, INT(selection_io_mode, C_INT)))

  END SUBROUTINE h5pset_selection_io_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the selection I/O mode
!!
!! \param plist_id          \fortran_plist_id
!! \param selection_io_mode The selection I/O mode
!! \param hdferr            \fortran_error
!!
!! See C API: @ref H5Pget_selection_io()
!!
  SUBROUTINE h5pget_selection_io_f(plist_id, selection_io_mode, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER, INTENT(OUT)       :: selection_io_mode
    INTEGER, INTENT(OUT)       :: hdferr

    INTEGER(C_INT) :: c_selection_io_mode

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pget_selection_io(plist_id, selection_io_mode) BIND(C, NAME='H5Pget_selection_io')
         IMPORT :: HID_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: plist_id
         INTEGER(C_INT)        :: selection_io_mode
       END FUNCTION H5Pget_selection_io
    END INTERFACE

    hdferr = INT(H5Pget_selection_io(plist_id, c_selection_io_mode))
    selection_io_mode = INT(c_selection_io_mode)

  END SUBROUTINE h5pget_selection_io_f

!>
!! \ingroup FH5P
!!
!! \brief Allows the library to modify the contents of the write buffer
!!
!! \param plist_id         \fortran_plist_id
!! \param modify_write_buf Whether the library can modify the contents of the write buffer
!! \param hdferr           \fortran_error
!!
!! See C API: @ref  H5Pset_modify_write_buf()
!!
  SUBROUTINE h5pset_modify_write_buf_f(plist_id, modify_write_buf, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T), INTENT(IN) :: plist_id
    LOGICAL, INTENT(IN)        :: modify_write_buf
    INTEGER, INTENT(OUT)       :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pset_modify_write_buf(plist_id, modify_write_buf) BIND(C, NAME='H5Pset_modify_write_buf')
         IMPORT :: HID_T, C_INT, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: plist_id
         LOGICAL(C_BOOL), VALUE :: modify_write_buf
       END FUNCTION H5Pset_modify_write_buf
    END INTERFACE

    hdferr = INT(H5Pset_modify_write_buf(plist_id, LOGICAL(modify_write_buf, C_BOOL)))

  END SUBROUTINE h5pset_modify_write_buf_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the "modify write buffer" property
!!
!! \param plist_id         \fortran_plist_id
!! \param modify_write_buf Whether the library can modify the contents of the write buffer
!! \param hdferr           \fortran_error
!!
!! See C API: @ref  H5Pget_modify_write_buf()
!!
  SUBROUTINE h5pget_modify_write_buf_f(plist_id, modify_write_buf, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T), INTENT(IN) :: plist_id
    LOGICAL, INTENT(OUT)       :: modify_write_buf
    INTEGER, INTENT(OUT)       :: hdferr

    LOGICAL(C_BOOL) :: c_modify_write_buf

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pget_modify_write_buf(plist_id, modify_write_buf) BIND(C, NAME='H5Pget_modify_write_buf')
         IMPORT :: HID_T, C_INT, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: plist_id
         LOGICAL(C_BOOL) :: modify_write_buf
       END FUNCTION H5Pget_modify_write_buf
    END INTERFACE

    hdferr = INT(H5Pget_modify_write_buf(plist_id, c_modify_write_buf))
    modify_write_buf = LOGICAL(c_modify_write_buf)

  END SUBROUTINE h5pget_modify_write_buf_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the byte size of the offsets and lengths used to address objects in an HDF5 file.
!!
!! \param prp_id      File creation property list identifier.
!! \param sizeof_addr Size of an object offset in bytes.
!! \param sizeof_size Size of an object length in bytes.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Pset_sizes()
!!
  SUBROUTINE h5pset_sizes_f(prp_id, sizeof_addr, sizeof_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(SIZE_T), INTENT(IN) :: sizeof_addr
    INTEGER(SIZE_T), INTENT(IN) :: sizeof_size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_sizes_c(prp_id, sizeof_addr, sizeof_size) &
            BIND(C,NAME='h5pset_sizes_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(SIZE_T), INTENT(IN) :: sizeof_addr
         INTEGER(SIZE_T), INTENT(IN) :: sizeof_size
       END FUNCTION h5pset_sizes_c
    END INTERFACE

    hdferr = h5pset_sizes_c(prp_id, sizeof_addr, sizeof_size)
  END SUBROUTINE h5pset_sizes_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the size of the offsets and lengths used in an HDF5 file
!!
!! \param prp_id File Creation property list identifier.
!! \param sizeof_addr Size of an object offset in bytes.
!! \param sizeof_size Size of an object length in bytes.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Pget_sizes()
!!
  SUBROUTINE h5pget_sizes_f(prp_id, sizeof_addr, sizeof_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(SIZE_T), INTENT(OUT) :: sizeof_addr
    INTEGER(SIZE_T), INTENT(OUT) :: sizeof_size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_sizes_c(prp_id, sizeof_addr, sizeof_size) &
            BIND(C,NAME='h5pget_sizes_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(SIZE_T), INTENT(OUT) :: sizeof_addr
         INTEGER(SIZE_T), INTENT(OUT) :: sizeof_size
       END FUNCTION h5pget_sizes_c
    END INTERFACE

    hdferr = h5pget_sizes_c(prp_id, sizeof_addr, sizeof_size)
  END SUBROUTINE h5pget_sizes_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the size of parameters used to control the symbol table nodes.
!!
!! \param prp_id  File creation property list identifier.
!! \param ik      Symbol table tree rank.
!! \param lk      Symbol table node size.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pset_sym_k()
!!
  SUBROUTINE h5pset_sym_k_f(prp_id, ik, lk, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: ik
    INTEGER, INTENT(IN) :: lk
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_sym_k_c(prp_id, ik, lk) &
            BIND(C,NAME='h5pset_sym_k_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: ik
         INTEGER, INTENT(IN) :: lk
       END FUNCTION h5pset_sym_k_c
    END INTERFACE

    hdferr = h5pset_sym_k_c(prp_id, ik, lk)
  END SUBROUTINE h5pset_sym_k_f
!>
!! \ingroup FH5P
!!
!! \brief Retrieves the size of the symbol table B-tree 1/2 rank and the symbol table leaf node 1/2 size.
!!
!! \param prp_id File creation property list identifier.
!! \param ik     Symbol table tree 1/2 rank.
!! \param lk     Symbol table node 1/2 size.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget_sym_k()
!!
  SUBROUTINE h5pget_sym_k_f(prp_id, ik, lk, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: ik
    INTEGER, INTENT(OUT) :: lk
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_sym_k_c(prp_id, ik, lk) &
            BIND(C,NAME='h5pget_sym_k_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: ik
         INTEGER, INTENT(OUT) :: lk
       END FUNCTION h5pget_sym_k_c
    END INTERFACE

    hdferr = h5pget_sym_k_c(prp_id, ik, lk)
  END SUBROUTINE h5pget_sym_k_f
!>
!! \ingroup FH5P
!!
!! \brief Sets the size of the parameter used to control the B-trees for indexing chunked datasets
!!
!! \param  prp_id File creation property list identifier
!! \param ik      1/2 rank of chunked storage B-tree
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pset_istore_k()
!!
  SUBROUTINE h5pset_istore_k_f(prp_id, ik, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: ik
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_istore_k_c(prp_id, ik) &
            BIND(C,NAME='h5pset_istore_k_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: ik
       END FUNCTION h5pset_istore_k_c
    END INTERFACE

    hdferr = h5pset_istore_k_c(prp_id, ik)
  END SUBROUTINE h5pset_istore_k_f

!>
!! \ingroup FH5P
!!
!! \brief Queries the 1/2 rank of an indexed storage B-tree.
!!
!! \param prp_id File creation property list identifier.
!! \param ik     Rank of chunked storage B-tree.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget_istore_k()
!!
  SUBROUTINE h5pget_istore_k_f(prp_id, ik, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: ik
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_istore_k_c(prp_id, ik) &
            BIND(C,NAME='h5pget_istore_k_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: ik
       END FUNCTION h5pget_istore_k_c
    END INTERFACE

    hdferr = h5pget_istore_k_c(prp_id, ik)
  END SUBROUTINE h5pget_istore_k_f

!>
!! \ingroup FH5P
!!
!! \brief Returns low-lever driver identifier.
!!
!! \param prp_id File access or data transfer property list identifier.
!! \param driver Low-level driver identifier.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget_driver()
!!
  SUBROUTINE h5pget_driver_f(prp_id, driver, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(OUT) :: driver
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_driver_c(prp_id, driver) &
            BIND(C,NAME='h5pget_driver_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(OUT) :: driver
       END FUNCTION h5pget_driver_c
    END INTERFACE

    hdferr = h5pget_driver_c(prp_id, driver)
  END SUBROUTINE h5pget_driver_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the standard I/O driver.
!!
!! \param prp_id File access property list identifier.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_fapl_stdio()
!!
  SUBROUTINE h5pset_fapl_stdio_f(prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_stdio_c(prp_id) &
            BIND(C,NAME='h5pset_fapl_stdio_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
       END FUNCTION h5pset_fapl_stdio_c
    END INTERFACE

    hdferr = h5pset_fapl_stdio_c(prp_id)
  END SUBROUTINE h5pset_fapl_stdio_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the sec2 driver.
!!
!! \param prp_id File access property list identifier.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_fapl_sec2()
!!
  SUBROUTINE h5pset_fapl_sec2_f(prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_sec2_c(prp_id) &
            BIND(C,NAME='h5pset_fapl_sec2_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
       END FUNCTION h5pset_fapl_sec2_c
    END INTERFACE

    hdferr = h5pset_fapl_sec2_c(prp_id)
  END SUBROUTINE h5pset_fapl_sec2_f

!>
!! \ingroup FH5P
!!
!! \brief Sets alignment properties of a file access property list.
!!
!! \param prp_id    File access property list identifier.
!! \param threshold Threshold value.
!! \param alignment Alignment value.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pset_alignment()
!!
  SUBROUTINE h5pset_alignment_f(prp_id, threshold,  alignment, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HSIZE_T), INTENT(IN) :: threshold
    INTEGER(HSIZE_T), INTENT(IN) :: alignment
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_alignment_c(prp_id, threshold, alignment) &
            BIND(C,NAME='h5pset_alignment_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(IN) :: threshold
         INTEGER(HSIZE_T), INTENT(IN) :: alignment
       END FUNCTION h5pset_alignment_c
    END INTERFACE

    hdferr = h5pset_alignment_c(prp_id, threshold, alignment)
  END SUBROUTINE h5pset_alignment_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the current settings for alignment properties from a file access property list.
!!
!! \param prp_id    File access property list identifier.
!! \param threshold Threshold value.
!! \param alignment Alignment value.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pget_alignment()
!!
  SUBROUTINE h5pget_alignment_f(prp_id, threshold,  alignment, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HSIZE_T), INTENT(OUT) :: threshold
    INTEGER(HSIZE_T), INTENT(OUT) :: alignment
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_alignment_c(prp_id, threshold, alignment) &
            BIND(C,NAME='h5pget_alignment_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(OUT) :: threshold
         INTEGER(HSIZE_T), INTENT(OUT) :: alignment
       END FUNCTION h5pget_alignment_c
    END INTERFACE

    hdferr = h5pget_alignment_c(prp_id, threshold, alignment)
  END SUBROUTINE h5pget_alignment_f

!>
!! \ingroup FH5P
!!
!! \brief Modifies the file access property list to use the H5FD_CORE driver.
!!
!! \param prp_id        File access property list identifier.
!! \param increment     Size, in bytes, of memory increments.
!! \param backing_store Boolean flag indicating whether to write the file contents to disk when the file is closed.
!! \param hdferr        \fortran_error
!!
!! See C API: @ref H5Pset_fapl_core()
!!
  SUBROUTINE h5pset_fapl_core_f(prp_id, increment, backing_store, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(SIZE_T), INTENT(IN) :: increment
    LOGICAL, INTENT(IN) :: backing_store
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: backing_store_flag
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_core_c(prp_id, increment, backing_store_flag) &
            BIND(C,NAME='h5pset_fapl_core_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(SIZE_T), INTENT(IN) :: increment
         INTEGER :: backing_store_flag
       END FUNCTION h5pset_fapl_core_c
    END INTERFACE
    backing_store_flag = 0
    IF(backing_store) backing_store_flag = 1
    hdferr = h5pset_fapl_core_c(prp_id, increment, backing_store_flag)
  END SUBROUTINE h5pset_fapl_core_f

!>
!! \ingroup FH5P
!!
!! \brief Queries core file driver properties.
!!
!! \param prp_id        File access property list identifier.
!! \param increment     Size, in bytes, of memory increments.
!! \param backing_store Boolean flag indicating whether to write the file contents to disk when the file is closed.
!! \param hdferr        \fortran_error
!!
!! See C API: @ref H5Pget_fapl_core()
!!
  SUBROUTINE h5pget_fapl_core_f(prp_id, increment, backing_store, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(SIZE_T), INTENT(OUT) :: increment
    LOGICAL, INTENT(OUT) :: backing_store
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: backing_store_flag

    INTERFACE
       INTEGER FUNCTION h5pget_fapl_core_c(prp_id, increment, backing_store_flag) &
            BIND(C,NAME='h5pget_fapl_core_c')
         IMPORT :: HID_T,SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(SIZE_T), INTENT(OUT) :: increment
         INTEGER :: backing_store_flag
       END FUNCTION h5pget_fapl_core_c
    END INTERFACE

    hdferr = h5pget_fapl_core_c(prp_id, increment, backing_store_flag)
    backing_store =.FALSE.
    IF (backing_store_flag .EQ. 1) backing_store =.TRUE.
  END SUBROUTINE h5pget_fapl_core_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the file access property list to use the family driver.
!!
!! \param prp_id     File access property list identifier.
!! \param memb_size  Size in bytes of each file member.
!! \param memb_plist Identifier of the file access property list to be used for each family member
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Pset_fapl_family()
!!
  SUBROUTINE h5pset_fapl_family_f(prp_id, memb_size, memb_plist , hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HSIZE_T), INTENT(IN) :: memb_size
    INTEGER(HID_T), INTENT(IN) :: memb_plist
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_family_c(prp_id, memb_size, memb_plist) &
            BIND(C,NAME='h5pset_fapl_family_c')
         IMPORT :: HID_T,HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(IN) :: memb_size
         INTEGER(HID_T), INTENT(IN) :: memb_plist
       END FUNCTION h5pset_fapl_family_c
    END INTERFACE

    hdferr = h5pset_fapl_family_c(prp_id, memb_size, memb_plist)
  END SUBROUTINE h5pset_fapl_family_f

!>
!! \ingroup FH5P
!!
!! \brief Returns file access property list information.
!!
!! \param prp_id     File access property list identifier.
!! \param memb_size  Size in bytes of each file member.
!! \param memb_plist Identifier of the file access property list to be used for each family member
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Pget_fapl_family()
!!
  SUBROUTINE h5pget_fapl_family_f(prp_id, memb_size, memb_plist , hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HSIZE_T), INTENT(OUT) :: memb_size
    INTEGER(HID_T), INTENT(OUT) :: memb_plist
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_fapl_family_c(prp_id, memb_size, memb_plist) &
            BIND(C,NAME='h5pget_fapl_family_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(OUT) :: memb_size
         INTEGER(HID_T), INTENT(OUT) :: memb_plist
       END FUNCTION h5pget_fapl_family_c
    END INTERFACE

    hdferr = h5pget_fapl_family_c(prp_id, memb_size, memb_plist)
  END SUBROUTINE h5pget_fapl_family_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the meta data cache and raw data chunk cache parameters
!!
!! \param prp_id      File access property list identifier.
!! \param mdc_nelmts  Number of elements (objects) in the metadata cache.
!! \param rdcc_nelmts Number of elements (objects) in the raw data chunk cache.
!! \param rdcc_nbytes Total size of the raw data chunk cache, in bytes.
!! \param rdcc_w0     Preemption policy (0 or 1).
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Pset_cache()
!!
  SUBROUTINE h5pset_cache_f(prp_id, mdc_nelmts,rdcc_nelmts, rdcc_nbytes, rdcc_w0, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: mdc_nelmts
    INTEGER(SIZE_T), INTENT(IN) :: rdcc_nelmts
    INTEGER(SIZE_T), INTENT(IN) :: rdcc_nbytes
    REAL, INTENT(IN) :: rdcc_w0
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_cache_c(prp_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0) &
            BIND(C,NAME='h5pset_cache_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: mdc_nelmts
         INTEGER(SIZE_T), INTENT(IN) :: rdcc_nelmts
         INTEGER(SIZE_T), INTENT(IN) :: rdcc_nbytes
         REAL, INTENT(IN) :: rdcc_w0
       END FUNCTION h5pset_cache_c
    END INTERFACE

    hdferr = h5pset_cache_c(prp_id, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0 )
  END SUBROUTINE h5pset_cache_f

!>
!! \ingroup FH5P
!!
!! \brief Queries the meta data cache and raw data chunk cache parameters.
!!
!! \param prp_id      File access property list identifier.
!! \param mdc_nelmts  Number of elements (objects) in the metadata cache
!! \param rdcc_nelmts Number of elements (objects) in the raw data chunk cache
!! \param rdcc_nbytes Total size of the raw data chunk cache, in bytes.
!! \param rdcc_w0     Preemption policy (0 or 1).
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Pget_cache()
!!
  SUBROUTINE h5pget_cache_f(prp_id, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: mdc_nelmts
    INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nelmts
    INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nbytes
    REAL, INTENT(OUT) :: rdcc_w0
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_cache_c(prp_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0) &
            BIND(C,NAME='h5pget_cache_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: mdc_nelmts
         INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nelmts
         INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nbytes
         REAL, INTENT(OUT) :: rdcc_w0
       END FUNCTION h5pget_cache_c
    END INTERFACE

    hdferr = h5pget_cache_c(prp_id, mdc_nelmts,rdcc_nelmts, rdcc_nbytes, rdcc_w0 )
  END SUBROUTINE h5pget_cache_f

!>
!! \ingroup FH5P
!!
!! \brief Emulates the old split file driver.
!!
!! \param prp_id     File access property list identifier.
!! \param meta_ext   Name of the extension for the metafile filename.
!! \param meta_plist Identifier of the meta file access property list.
!! \param raw_ext    Name extension for the raw file filename.
!! \param raw_plist  Identifier of the raw file access property list.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Pset_fapl_split()
!!
  SUBROUTINE h5pset_fapl_split_f(prp_id, meta_ext, meta_plist, raw_ext, raw_plist, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: meta_ext
    INTEGER(HID_T), INTENT(IN) :: meta_plist
    CHARACTER(LEN=*), INTENT(IN) :: raw_ext
    INTEGER(HID_T), INTENT(IN) :: raw_plist
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: meta_len, raw_len
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_split_c(prp_id,meta_len,meta_ext,meta_plist,raw_len,raw_ext,raw_plist) &
            BIND(C,NAME='h5pset_fapl_split_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: meta_ext
         INTEGER(HID_T), INTENT(IN) :: meta_plist
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: raw_ext
         INTEGER(HID_T), INTENT(IN) :: raw_plist
         INTEGER :: meta_len, raw_len
       END FUNCTION h5pset_fapl_split_c
    END INTERFACE

    meta_len = LEN(meta_ext)
    raw_len = LEN(raw_ext)
    hdferr = h5pset_fapl_split_c(prp_id,meta_len,meta_ext,meta_plist,raw_len,raw_ext,raw_plist)
  END SUBROUTINE h5pset_fapl_split_f

!>
!! \ingroup FH5P
!!
!! \brief Sets garbage collecting references flag.
!!
!! \param prp_id       File access property list identifier.
!! \param gc_reference Flag for setting garbage collection on and off (1 or 0).
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Pset_gc_references()
!!
  SUBROUTINE h5pset_gc_references_f(prp_id, gc_reference, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: gc_reference
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_gc_references_c(prp_id, gc_reference) &
            BIND(C,NAME='h5pset_gc_references_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: gc_reference
       END FUNCTION h5pset_gc_references_c
    END INTERFACE

    hdferr = h5pset_gc_references_c(prp_id, gc_reference)
  END SUBROUTINE h5pset_gc_references_f

!>
!! \ingroup FH5P
!!
!! \brief Returns garbage collecting references setting.
!!
!! \param prp_id       File access property list identifier.
!! \param gc_reference Flag for setting garbage collection on and off (1 or 0)
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Pget_gc_references()
!!
  SUBROUTINE h5pget_gc_references_f(prp_id, gc_reference, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: gc_reference
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_gc_references_c(prp_id, gc_reference) &
            BIND(C,NAME='h5pget_gc_references_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: gc_reference
       END FUNCTION h5pget_gc_references_c
    END INTERFACE

    hdferr = h5pget_gc_references_c(prp_id, gc_reference)
  END SUBROUTINE h5pget_gc_references_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the type of storage used store the raw data
!!       for a dataset.
!!
!! \param prp_id Data creation property list identifier.
!! \param layout Type of storage layout for raw data. Possible values are:
!!               \li H5D_COMPACT_F
!!               \li H5D_CONTIGUOUS_F
!!               \li H5D_CHUNKED_F
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_layout()
!!
  SUBROUTINE h5pset_layout_f(prp_id, layout, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: layout
                                         !   H5D_CHUNKED_F
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_layout_c(prp_id, layout) &
            BIND(C,NAME='h5pset_layout_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: layout
       END FUNCTION h5pset_layout_c
    END INTERFACE

    hdferr = h5pset_layout_c(prp_id, layout)
  END SUBROUTINE h5pset_layout_f

!>
!! \ingroup FH5P
!!
!! \brief Returns the layout of the raw data for a dataset.
!!
!! \param prp_id Data creation property list identifier.
!! \param layout Type of storage layout for raw data. Possible values are:
!!               \li H5D_COMPACT_F
!!               \li H5D_CONTIGUOUS_F
!!               \li H5D_CHUNKED_F
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget_layout()
!!
  SUBROUTINE h5pget_layout_f(prp_id, layout, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: layout
                                         !  H5D_CHUNKED_F(2)
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_layout_c(prp_id, layout) &
            BIND(C,NAME='h5pget_layout_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: layout
       END FUNCTION h5pget_layout_c
    END INTERFACE

    hdferr = h5pget_layout_c(prp_id, layout)
  END SUBROUTINE h5pget_layout_f

!>
!! \ingroup FH5P
!!
!! \brief Adds a filter to the filter pipeline.
!!
!! \param prp_id    Data creation or transfer property list identifier.
!! \param filter    Filter to be added to the pipeline.
!! \param flags     Bit vector specifying certain general properties of the filter.
!! \param cd_nelmts Number of elements in cd_values.
!! \param cd_values Auxiliary data for the filter.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pset_filter()
!!
  SUBROUTINE h5pset_filter_f(prp_id, filter, flags, cd_nelmts, cd_values,  hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: filter
    INTEGER, INTENT(IN) :: flags
    INTEGER(SIZE_T), INTENT(IN) :: cd_nelmts
    INTEGER, DIMENSION(*), INTENT(IN) :: cd_values
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_filter_c(prp_id, filter, flags, cd_nelmts, cd_values) &
            BIND(C,NAME='h5pset_filter_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: filter
         INTEGER, INTENT(IN) :: flags
         INTEGER(SIZE_T), INTENT(IN) :: cd_nelmts
         INTEGER, DIMENSION(*), INTENT(IN) :: cd_values
       END FUNCTION h5pset_filter_c
    END INTERFACE

    hdferr = h5pset_filter_c(prp_id, filter, flags, cd_nelmts, cd_values )
  END SUBROUTINE h5pset_filter_f

!>
!! \ingroup FH5P
!!
!! \brief Returns the number of filters in the pipeline.
!!
!! \param prp_id   Data creation or transfer property list identifier.
!! \param nfilters Number of filters in the pipeline.
!! \param hdferr   \fortran_error
!!
  SUBROUTINE h5pget_nfilters_f(prp_id, nfilters, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: nfilters
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_nfilters_c(prp_id, nfilters) &
            BIND(C,NAME='h5pget_nfilters_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: nfilters
       END FUNCTION h5pget_nfilters_c
    END INTERFACE

    hdferr = h5pget_nfilters_c(prp_id, nfilters)
  END SUBROUTINE h5pget_nfilters_f

!>
!! \ingroup FH5P
!!
!! \brief Returns information about a filter in a pipeline
!!
!! \param prp_id        Data creation or transfer property list identifier
!! \param filter_number Sequence number within the filter pipeline of the filter for which information is sought
!! \param filter_id     Filter identification number.
!! \param flags         Bitbit vector specifying certain general properties of the filter.
!! \param cd_nelmts     Number of elements in cd_values.
!! \param cd_values     Auxiliary data for the filter.
!! \param namelen       Number of characters in the name buffer.
!! \param name          Buffer to retrieve filter name.
!! \param hdferr        \fortran_error
!!
!! See C API: @ref H5Pget_filter2()
!!
  SUBROUTINE h5pget_filter_f(prp_id, filter_number, flags, cd_nelmts, cd_values, namelen, name, filter_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: filter_number
    INTEGER, DIMENSION(*), INTENT(OUT) :: cd_values
    INTEGER, INTENT(OUT) :: flags
    INTEGER(SIZE_T), INTENT(INOUT) :: cd_nelmts
    INTEGER(SIZE_T), INTENT(IN) :: namelen
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER, INTENT(OUT) :: filter_id
    INTEGER, INTENT(OUT) :: hdferr

!            INTEGER, EXTERNAL :: h5pget_filter_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_filter_c(prp_id, filter_number, flags, cd_nelmts,  &
            cd_values, namelen, name, filter_id ) &
            BIND(C,NAME='h5pget_filter_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: filter_number
         INTEGER, DIMENSION(*), INTENT(OUT) :: cd_values
         INTEGER, INTENT(OUT) :: flags
         INTEGER(SIZE_T), INTENT(INOUT) :: cd_nelmts
         INTEGER(SIZE_T), INTENT(IN) :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: name
         INTEGER, INTENT(OUT) :: filter_id
       END FUNCTION h5pget_filter_c
    END INTERFACE

    hdferr = h5pget_filter_c(prp_id, filter_number, flags, cd_nelmts,  &
         cd_values, namelen, name, filter_id )
  END SUBROUTINE h5pget_filter_f

!>
!! \ingroup FH5P
!!
!! \brief Adds an external file to the list of external files.
!!
!! \param prp_id Dataset creation property list identifier.
!! \param name   Name of external file.
!! \param offset Offset in bytes from the beginning of the file to the location in the file where the data starts.
!! \param bytes  Size of the external file data.
!! \param hdferr \fortran_error
!!
!! \note On Windows, off_t is typically a 32-bit signed long value, which
!!       limits the valid offset that can be set to 2 GiB.
!!
!! See C API: @ref H5Pset_external()
!!
  SUBROUTINE h5pset_external_f(prp_id, name, offset, bytes, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(OFF_T), INTENT(IN) :: offset
    INTEGER(HSIZE_T), INTENT(IN) :: bytes
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen

    INTERFACE
       INTEGER FUNCTION h5pset_external_c(prp_id, name,namelen, offset, bytes) &
            BIND(C,NAME='h5pset_external_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, OFF_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(OFF_T), INTENT(IN) :: offset
         INTEGER(HSIZE_T), INTENT(IN) :: bytes
       END FUNCTION h5pset_external_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5pset_external_c(prp_id, name, namelen, offset, bytes)
  END SUBROUTINE h5pset_external_f

!>
!! \ingroup FH5P
!!
!! \brief Returns the number of external files for a dataset.
!!
!! \param prp_id Dataset creation property list identifier.
!! \param count  Number of external files for the specified dataset.
!! \param hdferr \fortran_error
!!
!! See C API: int H5Pget_external_count(hid_t plist_id);
!!
  SUBROUTINE h5pget_external_count_f(prp_id, count, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: count
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_external_count_c(prp_id, count) &
            BIND(C,NAME='h5pget_external_count_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: count
       END FUNCTION h5pget_external_count_c
    END INTERFACE

    hdferr = h5pget_external_count_c(prp_id, count)
  END SUBROUTINE h5pget_external_count_f

!>
!! \ingroup FH5P
!!
!! \brief Returns information about an external file.
!!
!! \param prp_id    Dataset creation property list identifier.
!! \param idx       External file index.
!! \param name_size Maximum size of name array.
!! \param name      Name of the external file.
!! \param offset    Offset in bytes from the beginning of the file to the location in the file where the data starts.
!! \param bytes     Size of the external file data.
!! \param hdferr    \fortran_error
!!
!! \note On Windows, off_t is typically a 32-bit signed long value, which
!!       limits the valid offset that can be returned to 2 GiB.
!!
!! See C API: @ref H5Pget_external()
!!
  SUBROUTINE h5pget_external_f(prp_id, idx, name_size, name, offset, bytes, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: idx
    INTEGER(SIZE_T), INTENT(IN) :: name_size
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER(OFF_T), INTENT(OUT) :: offset
    INTEGER(HSIZE_T), INTENT(OUT) :: bytes
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_external_c(prp_id, idx, name_size, name, offset, bytes) &
            BIND(C,NAME='h5pget_external_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T, OFF_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: idx
         INTEGER(SIZE_T), INTENT(IN) :: name_size
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: name
         INTEGER(OFF_T), INTENT(OUT) :: offset
         INTEGER(HSIZE_T), INTENT(OUT) :: bytes
       END FUNCTION h5pget_external_c
    END INTERFACE

    hdferr = h5pget_external_c(prp_id, idx, name_size, name, offset, bytes)
  END SUBROUTINE h5pget_external_f

!>
!! \ingroup FH5P
!!
!! \brief Sets B-tree split ratios for a dataset transfer property list.
!!
!! \param prp_id The dataset transfer property list identifier.
!! \param left   The B-tree split ratio for left-most nodes.
!! \param middle The B-tree split ratio for all other nodes.
!! \param right  The B-tree split ratio for right-most nodes.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_btree_ratios()
!!
  SUBROUTINE h5pset_btree_ratios_f(prp_id, left, middle, right, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    REAL, INTENT(IN) :: left
    REAL, INTENT(IN) :: middle
    REAL, INTENT(IN) :: right
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION  h5pset_btree_ratios_c(prp_id, left, middle, right) &
            BIND(C,NAME='h5pset_btree_ratios_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         REAL, INTENT(IN) :: left
         REAL, INTENT(IN) :: middle
         REAL, INTENT(IN) :: right
       END FUNCTION h5pset_btree_ratios_c
    END INTERFACE

    hdferr = h5pset_btree_ratios_c(prp_id, left, middle, right)
  END SUBROUTINE h5pset_btree_ratios_f

!>
!! \ingroup FH5P
!!
!! \brief Gets B-tree split ratios for a dataset transfer property list
!!
!! \param prp_id The dataset transfer property list identifier.
!! \param left   The B-tree split ratio for left-most nodes.
!! \param middle The B-tree split ratio for all other nodes.
!! \param right  The B-tree split ratio for right-most nodes.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget_btree_ratios()
!!
  SUBROUTINE h5pget_btree_ratios_f(prp_id, left, middle, right, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    REAL, INTENT(OUT) :: left
    REAL, INTENT(OUT) :: middle
    REAL, INTENT(OUT) :: right
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION  h5pget_btree_ratios_c(prp_id, left, middle, right) &
            BIND(C,NAME='h5pget_btree_ratios_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         REAL, INTENT(OUT) :: left
         REAL, INTENT(OUT) :: middle
         REAL, INTENT(OUT) :: right
       END FUNCTION h5pget_btree_ratios_c
    END INTERFACE

    hdferr = h5pget_btree_ratios_c(prp_id, left, middle, right)
  END SUBROUTINE h5pget_btree_ratios_f

!>
!! \ingroup FH5P
!!
!! \brief Returns the degree for the file close behavior.
!!
!! \param fapl_id File access property list identifier.
!! \param degree  Possible values are:
!!                \li H5F_CLOSE_DEFAULT_F
!!                \li H5F_CLOSE_WEAK_F
!!                \li H5F_CLOSE_SEMI_F
!!                \li H5F_CLOSE_STRONG_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pget_fclose_degree()
!!
  SUBROUTINE h5pget_fclose_degree_f(fapl_id, degree, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: fapl_id
    INTEGER, INTENT(OUT) :: degree
                                !  H5F_CLOSE_STRONG_F
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_fclose_degree_c(fapl_id, degree) &
            BIND(C,NAME='h5pget_fclose_degree_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: fapl_id
         INTEGER, INTENT(OUT) :: degree
       END FUNCTION h5pget_fclose_degree_c
    END INTERFACE

    hdferr = h5pget_fclose_degree_c(fapl_id, degree)
  END SUBROUTINE h5pget_fclose_degree_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the degree for the file close behavior.
!!
!! \param fapl_id File access property list identifier.
!! \param degree  Possible values are:
!!                \li H5F_CLOSE_DEFAULT_F
!!                \li H5F_CLOSE_WEAK_F
!!                \li H5F_CLOSE_SEMI_F
!!                \li H5F_CLOSE_STRONG_F
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_fclose_degree()
!!
  SUBROUTINE h5pset_fclose_degree_f(fapl_id, degree, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: fapl_id
    INTEGER, INTENT(IN) :: degree
                                !  H5F_CLOSE_STRONG_F
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_fclose_degree_c(fapl_id, degree) &
            BIND(C,NAME='h5pset_fclose_degree_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: fapl_id
         INTEGER, INTENT(IN) :: degree
       END FUNCTION h5pset_fclose_degree_c
    END INTERFACE

    hdferr = h5pset_fclose_degree_c(fapl_id, degree)
  END SUBROUTINE h5pset_fclose_degree_f

!>
!! \ingroup FH5P
!!
!! \brief Checks if two property lists are equal
!!
!! \param plist1_id Property list identifier.
!! \param plist2_id Property list identifier.
!! \param flag      Flag, Possible values: .TRUE. or .FALSE.
!! \param hdferr:   \fortran_error and flag is set to .FALSE.
!!
!! See C API: @ref H5Pequal()
!!
  SUBROUTINE h5pequal_f(plist1_id, plist2_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist1_id
    INTEGER(HID_T), INTENT(IN) :: plist2_id
    LOGICAL, INTENT(OUT)       :: flag
    INTEGER, INTENT(OUT)       :: hdferr
    INTEGER                    :: c_flag

    INTERFACE
       INTEGER FUNCTION h5pequal_c(plist1_id, plist2_id, c_flag) &
            BIND(C,NAME='h5pequal_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist1_id
         INTEGER(HID_T), INTENT(IN) :: plist2_id
         INTEGER, INTENT(OUT) :: c_flag
       END FUNCTION h5pequal_c
    END INTERFACE

    flag = .FALSE.
    hdferr = h5pequal_c(plist1_id, plist2_id, c_flag)
    IF (c_flag .GT. 0) flag = .TRUE.
  END SUBROUTINE h5pequal_f

!>
!! \ingroup FH5P
!!
!! \brief Sets sixe for conversion buffer
!!
!! \param plist_id Data transfer property list identifier.
!! \param size     Buffer size.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_buffer()
!!
  SUBROUTINE h5pset_buffer_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER(HSIZE_T), INTENT(IN) :: size
    INTEGER, INTENT(OUT)       :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pset_buffer_c(plist_id, size) &
            BIND(C,NAME='h5pset_buffer_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(IN) :: size
       END FUNCTION h5pset_buffer_c
    END INTERFACE

    hdferr = h5pset_buffer_c(plist_id, size)
  END SUBROUTINE h5pset_buffer_f

!>
!! \ingroup FH5P
!!
!! \brief Gets size for conversion buffer
!!
!! \param plist_id Data transfer property list identifier.
!! \param size     Buffer size.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_buffer()
!!
  SUBROUTINE h5pget_buffer_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER(HSIZE_T), INTENT(OUT) :: size
    INTEGER, INTENT(OUT)       :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pget_buffer_c(plist_id, size) &
            BIND(C,NAME='h5pget_buffer_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_buffer_c
    END INTERFACE

    hdferr = h5pget_buffer_c(plist_id, size)
  END SUBROUTINE h5pget_buffer_f

!>
!! \ingroup FH5P
!!
!! \brief Check if fill value is defined.
!!
!! \param plist_id Dataset creation property list identifier.
!! \param flag     Fill value status flag. Possible values are:
!!                 \li H5D_FILL_VALUE_ERROR_F
!!                 \li H5D_FILL_VALUE_UNDEFINED_F
!!                 \li H5D_FILL_VALUE_DEFAULT_F
!!                 \li H5D_FILL_VALUE_USER_DEFINED_F
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pfill_value_defined()
!!
  SUBROUTINE h5pfill_value_defined_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER, INTENT(OUT) :: flag
                                            !  H5D_FILL_VALUE_USER_DEFINED_F
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pfill_value_defined_c(plist_id, flag) &
            BIND(C,NAME='h5pfill_value_defined_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(OUT) :: flag
       END FUNCTION h5pfill_value_defined_c
    END INTERFACE

    hdferr = h5pfill_value_defined_c(plist_id, flag)
  END SUBROUTINE h5pfill_value_defined_f

!>
!! \ingroup FH5P
!!
!! \brief Set space allocation time for dataset during creation.
!!
!! \param plist_id Dataset creation property list identifier.
!! \param flag     Allocation time flag: Possible values are:
!!                 \li H5D_ALLOC_TIME_ERROR_F
!!                 \li H5D_ALLOC_TIME_DEFAULT_F
!!                 \li H5D_ALLOC_TIME_EARLY_F
!!                 \li H5D_ALLOC_TIME_LATE_F
!!                 \li H5D_ALLOC_TIME_INCR_F
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_alloc_time()
!!
  SUBROUTINE h5pset_alloc_time_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER, INTENT(IN) :: flag
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pset_alloc_time_c(plist_id, flag) &
            BIND(C,NAME='h5pset_alloc_time_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(IN) :: flag
       END FUNCTION h5pset_alloc_time_c
    END INTERFACE

    hdferr = h5pset_alloc_time_c(plist_id, flag)
  END SUBROUTINE h5pset_alloc_time_f

!>
!! \ingroup FH5P
!!
!! \brief Get space allocation time for dataset during creation.
!!
!! \param plist_id Dataset creation property list identifier.
!! \param flag     Allocation time flag. Possible values are:
!!                 \li H5D_ALLOC_TIME_ERROR_F
!!                 \li H5D_ALLOC_TIME_DEFAULT_F
!!                 \li H5D_ALLOC_TIME_EARLY_F
!!                 \li H5D_ALLOC_TIME_LATE_F
!!                 \li H5D_ALLOC_TIME_INCR_F
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_alloc_time()
!!
  SUBROUTINE h5pget_alloc_time_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER, INTENT(OUT) :: flag
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pget_alloc_time_c(plist_id, flag) &
            BIND(C,NAME='h5pget_alloc_time_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(OUT) :: flag
       END FUNCTION h5pget_alloc_time_c
    END INTERFACE

    hdferr = h5pget_alloc_time_c(plist_id, flag)
  END SUBROUTINE h5pget_alloc_time_f

!>
!! \ingroup FH5P
!!
!! \brief Set fill value writing time for dataset
!!
!! \param plist_id Dataset creation property list identifier.
!! \param flag     Fill time flag: Possible values are:
!!                 \li H5D_FILL_TIME_ERROR_F
!!                 \li H5D_FILL_TIME_ALLOC_F
!!                 \li H5D_FILL_TIME_NEVER_F
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_fill_time()
!!
  SUBROUTINE h5pset_fill_time_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER, INTENT(IN) :: flag
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pset_fill_time_c(plist_id, flag) &
            BIND(C,NAME='h5pset_fill_time_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(IN) :: flag
       END FUNCTION h5pset_fill_time_c
    END INTERFACE

    hdferr = h5pset_fill_time_c(plist_id, flag)
  END SUBROUTINE h5pset_fill_time_f

!>
!! \ingroup FH5P
!!
!! \brief Get fill value writing time for dataset
!!
!! \param plist_id Dataset creation property list identifier.
!!
!! \param flag     Fill time flag. Possible values are:
!!                 \li H5D_FILL_TIME_ERROR_F
!!                 \li H5D_FILL_TIME_ALLOC_F
!!                 \li H5D_FILL_TIME_NEVER_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pget_fill_time()
!!
  SUBROUTINE h5pget_fill_time_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER, INTENT(OUT) :: flag
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pget_fill_time_c(plist_id, flag) &
            BIND(C,NAME='h5pget_fill_time_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(OUT) :: flag
       END FUNCTION h5pget_fill_time_c
    END INTERFACE

    hdferr = h5pget_fill_time_c(plist_id, flag)
  END SUBROUTINE h5pget_fill_time_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the minimum size of metadata block allocations
!!
!! \param plist_id File access property list identifier.
!! \param size     Metadata block size.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_meta_block_size()
!!
  SUBROUTINE h5pset_meta_block_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER(HSIZE_T), INTENT(IN) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_meta_block_size_c(plist_id, size) &
            BIND(C,NAME='h5pset_meta_block_size_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(IN) :: size
       END FUNCTION h5pset_meta_block_size_c
    END INTERFACE

    hdferr = h5pset_meta_block_size_c(plist_id, size)
  END SUBROUTINE h5pset_meta_block_size_f

!>
!! \ingroup FH5P
!!
!! \brief Gets the minimum size of metadata block allocations
!!
!! \param plist_id File access property list identifier.
!! \param size     Metadata block size.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_meta_block_size()
!!
  SUBROUTINE h5pget_meta_block_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER(HSIZE_T), INTENT(OUT) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_meta_block_size_c(plist_id, size) &
            BIND(C,NAME='h5pget_meta_block_size_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_meta_block_size_c
    END INTERFACE

    hdferr = h5pget_meta_block_size_c(plist_id, size)
  END SUBROUTINE h5pget_meta_block_size_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the maximum size of the data sieve buffer
!!
!! \param plist_id File access property list identifier.
!! \param size     Sieve buffer size.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_sieve_buf_size()
!!
  SUBROUTINE h5pset_sieve_buf_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER(SIZE_T), INTENT(IN) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_sieve_buf_size_c(plist_id, size) &
            BIND(C,NAME='h5pset_sieve_buf_size_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(SIZE_T), INTENT(IN) :: size
       END FUNCTION h5pset_sieve_buf_size_c
    END INTERFACE

    hdferr = h5pset_sieve_buf_size_c(plist_id, size)
  END SUBROUTINE h5pset_sieve_buf_size_f

!>
!! \ingroup FH5P
!!
!! \brief Gets the maximum size of the data sieve buffer
!!
!! \param plist_id File access property list identifier.
!! \param size     Sieve buffer size.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_sieve_buf_size()
!!
  SUBROUTINE h5pget_sieve_buf_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER(SIZE_T), INTENT(OUT) :: size
    INTEGER, INTENT(OUT)       :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_sieve_buf_size_c(plist_id, size) &
            BIND(C,NAME='h5pget_sieve_buf_size_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(SIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_sieve_buf_size_c
    END INTERFACE

    hdferr = h5pget_sieve_buf_size_c(plist_id, size)
  END SUBROUTINE h5pget_sieve_buf_size_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the minimum size of "small" raw data block
!!
!! \param plist_id File access property list identifier.
!! \param size     Small raw data block size.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_small_data_block_size()
!!
  SUBROUTINE h5pset_small_data_block_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER(HSIZE_T), INTENT(IN) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_small_data_block_size_c(plist_id, size) &
            BIND(C,NAME='h5pset_small_data_block_size_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(IN) :: size
       END FUNCTION h5pset_small_data_block_size_c
    END INTERFACE

    hdferr = h5pset_small_data_block_size_c(plist_id, size)
  END SUBROUTINE h5pset_small_data_block_size_f

!>
!! \ingroup FH5P
!!
!! \brief Gets the minimum size of "small" raw data block
!!
!! \param plist_id File access property list identifier.
!! \param size     Small raw data block size.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_small_data_block_size()
!!
  SUBROUTINE h5pget_small_data_block_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER(HSIZE_T), INTENT(OUT) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_small_data_block_size_c(plist_id, size) &
            BIND(C,NAME='h5pget_small_data_block_size_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_small_data_block_size_c
    END INTERFACE

    hdferr = h5pget_small_data_block_size_c(plist_id, size)
  END SUBROUTINE h5pget_small_data_block_size_f

!>
!! \ingroup FH5P
!!
!! \brief Set the number of "I/O" vectors (vector size)
!!
!! \param plist_id Dataset transfer property list identifier.
!! \param size     Vector size.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_hyper_vector_size()
!!
  SUBROUTINE h5pset_hyper_vector_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER(SIZE_T), INTENT(IN) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_hyper_vector_size_c(plist_id, size) &
            BIND(C,NAME='h5pset_hyper_vector_size_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(SIZE_T), INTENT(IN) :: size
       END FUNCTION h5pset_hyper_vector_size_c
    END INTERFACE

    hdferr = h5pset_hyper_vector_size_c(plist_id, size)
  END SUBROUTINE h5pset_hyper_vector_size_f

!>
!! \ingroup FH5P
!!
!! \brief Get the number of "I/O" vectors (vector size)
!!
!! \param plist_id Dataset transfer property list identifier.
!! \param size     Vector size.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_hyper_vector_size()
!!
  SUBROUTINE h5pget_hyper_vector_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER(SIZE_T), INTENT(OUT) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_hyper_vector_size_c(plist_id, size) &
            BIND(C,NAME='h5pget_hyper_vector_size_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(SIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_hyper_vector_size_c
    END INTERFACE

    hdferr = h5pget_hyper_vector_size_c(plist_id, size)
  END SUBROUTINE h5pget_hyper_vector_size_f

!>
!! \ingroup FH5P
!!
!! \brief Queries whether a property name exists in a property list or class.
!!
!! \param prp_id Property list identifier to query.
!! \param name   Name of property to check for.
!! \param flag   Logical flag.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pexist()
!!
  SUBROUTINE h5pexist_f(prp_id, name, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    LOGICAL, INTENT(OUT) :: flag
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pexist_c(prp_id, name, name_len) &
            BIND(C,NAME='h5pexist_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
       END FUNCTION h5pexist_c
    END INTERFACE
    flag = .FALSE.
    name_len = LEN(name)
    hdferr = h5pexist_c(prp_id, name , name_len)
    IF (hdferr > 0) THEN
       flag = .TRUE.
       hdferr = 0
    ENDIF
  END SUBROUTINE h5pexist_f

!>
!! \ingroup FH5P
!!
!! \brief Queries the size of a property value in bytes.
!!
!! \param prp_id Property list identifier to query.
!! \param name   Name of property to query.
!! \param size   Size of property in bytes.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget_size()
!!
  SUBROUTINE h5pget_size_f(prp_id, name, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(OUT) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pget_size_c(prp_id, name, name_len, size) &
            BIND(C,NAME='h5pget_size_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(SIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_size_c
    END INTERFACE
    name_len = LEN(name)
    hdferr = h5pget_size_c(prp_id, name , name_len, size)
  END SUBROUTINE h5pget_size_f

!>
!! \ingroup FH5P
!!
!! \brief Queries number of properties in property list or class
!!
!! \param prp_id Iproperty list identifier to query.
!! \param nprops Number of properties in property object.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget_nprops()
!!
  SUBROUTINE h5pget_nprops_f(prp_id, nprops, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(SIZE_T), INTENT(OUT) :: nprops
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_nprops_c(prp_id, nprops) &
            BIND(C,NAME='h5pget_nprops_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(SIZE_T), INTENT(OUT) :: nprops
       END FUNCTION h5pget_nprops_c
    END INTERFACE
    hdferr = h5pget_nprops_c(prp_id, nprops)
  END SUBROUTINE h5pget_nprops_f

!>
!! \ingroup FH5P
!!
!! \brief Queries the name of a class.
!!
!! \param prp_id  Property list identifier to query.
!! \param name    Name of a class.
!! \param size    Actual length of the class name.
!!                NOTE: If provided buffer "name" is smaller, than name will be
!!                      truncated to fit into provided user buffer.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget_class_name()
!!
  SUBROUTINE h5pget_class_name_f(prp_id, name, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER, INTENT(OUT) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pget_class_name_c(prp_id, name, name_len) &
            BIND(C,NAME='h5pget_class_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: name
         INTEGER, INTENT(IN)         :: name_len
       END FUNCTION h5pget_class_name_c
    END INTERFACE

    name_len = LEN(name)
    size = h5pget_class_name_c(prp_id, name, name_len)

    hdferr = 0
    IF(size.LT.0) hdferr = -1

  END SUBROUTINE h5pget_class_name_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the parent class of a generic property class.
!!
!! \param prp_id    Property list identifier to query.
!! \param parent_id Identifier of the parent class.
!! \param hdferr:   \fortran_error
!!
!! See C API: @ref H5Pget_class_parent()
!!
  SUBROUTINE h5pget_class_parent_f(prp_id, parent_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(OUT) :: parent_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_class_parent_c(prp_id, parent_id) &
            BIND(C,NAME='h5pget_class_parent_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(OUT) :: parent_id
       END FUNCTION h5pget_class_parent_c
    END INTERFACE
    hdferr = h5pget_class_parent_c(prp_id, parent_id)
  END SUBROUTINE h5pget_class_parent_f

!>
!! \ingroup FH5P
!!
!! \brief Determines whether a property list is a member of a class.
!!
!! \param plist   Property list identifier.
!! \param pclass  Identifier of the property class.
!! \param flag    TRUE. if a member, .FALSE. otherwise.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pisa_class()
!!
  SUBROUTINE h5pisa_class_f(plist, pclass, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist
    INTEGER(HID_T), INTENT(IN) :: pclass
    LOGICAL, INTENT(OUT) :: flag
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pisa_class_c(plist, pclass) &
            BIND(C,NAME='h5pisa_class_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist
         INTEGER(HID_T), INTENT(IN) :: pclass
       END FUNCTION h5pisa_class_c
    END INTERFACE
    flag = .FALSE.
    hdferr = h5pisa_class_c(plist, pclass)
    IF (hdferr .GT. 0) THEN
       flag = .TRUE.
       hdferr = 0
    ENDIF
  END SUBROUTINE h5pisa_class_f

!>
!! \ingroup FH5P
!!
!! \brief Copies a property from one list or class to another.
!!
!! \param dst_id  Identifier of the destination property list.
!! \param src_id  Identifier of the source property list.
!! \param name    Name of the property to copy.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pcopy_prop()
!!
  SUBROUTINE h5pcopy_prop_f(dst_id, src_id, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dst_id
    INTEGER(HID_T), INTENT(IN) :: src_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pcopy_prop_c(dst_id, src_id, name, name_len) &
            BIND(C,NAME='h5pcopy_prop_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dst_id
         INTEGER(HID_T), INTENT(IN) :: src_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
       END FUNCTION h5pcopy_prop_c
    END INTERFACE
    name_len = LEN(name)
    hdferr = h5pcopy_prop_c(dst_id, src_id, name , name_len)
  END SUBROUTINE h5pcopy_prop_f

!>
!! \ingroup FH5P
!!
!! \brief Removes a property from a property list.
!!
!! \param plid    Property list identofoer.
!! \param name    Name of the property to remove.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Premove()
!!
  SUBROUTINE h5premove_f(plid, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plid
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5premove_c(plid, name, name_len) &
            BIND(C,NAME='h5premove_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plid
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
       END FUNCTION h5premove_c
    END INTERFACE
    name_len = LEN(name)
    hdferr = h5premove_c(plid, name , name_len)
  END SUBROUTINE h5premove_f

!>
!! \ingroup FH5P
!!
!! \brief Removes a property from a property list class.
!!
!! \param class   Property list class identifier.
!! \param name    Name of the property to remove.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Punregister()
!!
  SUBROUTINE h5punregister_f(class, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5punregister_c(class, name, name_len) &
            BIND(C,NAME='h5punregister_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: class
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
       END FUNCTION h5punregister_c
    END INTERFACE
    name_len = LEN(name)
    hdferr = h5punregister_c(class, name , name_len)
  END SUBROUTINE h5punregister_f

!>
!! \ingroup FH5P
!!
!! \brief Closes an existing property list class.
!!
!! \param class  Property list class identifier.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pclose_class()
!!
  SUBROUTINE h5pclose_class_f(class, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pclose_class_c(class) &
            BIND(C,NAME='h5pclose_class_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: class
       END FUNCTION h5pclose_class_c
    END INTERFACE
    hdferr = h5pclose_class_c(class)
  END SUBROUTINE h5pclose_class_f

!>
!! \ingroup FH5P
!!
!! \brief Sets shuffling filter
!!
!! \param prp_id Dataset creation property list identifier.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_shuffle()
!!
  SUBROUTINE h5pset_shuffle_f(prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_shuffle_c(prp_id) &
            BIND(C,NAME='h5pset_shuffle_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
       END FUNCTION h5pset_shuffle_c
    END INTERFACE
    hdferr = h5pset_shuffle_c(prp_id)

  END SUBROUTINE h5pset_shuffle_f

!>
!! \ingroup FH5P
!!
!! \brief Enables/disables error detecting
!!
!! \param prp_id Dataset creation property list identifier.
!! \param flag   EDC flag. Possible values:
!!               \li H5Z_DISABLE_EDC_F
!!               \li H5Z_ENABLE_EDC_F
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_edc_check()
!!
  SUBROUTINE h5pset_edc_check_f(prp_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: flag
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_edc_check_c(prp_id, flag) &
            BIND(C,NAME='h5pset_edc_check_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: flag
       END FUNCTION h5pset_edc_check_c
    END INTERFACE
    hdferr = h5pset_edc_check_c(prp_id, flag)

  END SUBROUTINE h5pset_edc_check_f

!>
!! \ingroup FH5P
!!
!! \brief Determines whether error-detection is enabled for dataset reads.
!!
!! \param prp_id Dataset creation property list identifier.
!! \param flag   EDC flag; possible values:
!!               \li H5Z_DISABLE_EDC_F
!!               \li H5Z_ENABLE_EDC_F
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget_edc_check()
!!
  SUBROUTINE h5pget_edc_check_f(prp_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: flag
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_edc_check_c(prp_id, flag) &
            BIND(C,NAME='h5pget_edc_check_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: flag
       END FUNCTION h5pget_edc_check_c
    END INTERFACE
    hdferr = h5pget_edc_check_c(prp_id, flag)

  END SUBROUTINE h5pget_edc_check_f

!>
!! \ingroup FH5P
!!
!! \brief Sets Fletcher32 checksum of EDC for a dataset creation property list.
!!
!! \param prp_id Dataset creation property list identifier.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_fletcher32()
!!
  SUBROUTINE h5pset_fletcher32_f(prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_fletcher32_c(prp_id) &
            BIND(C,NAME='h5pset_fletcher32_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
       END FUNCTION h5pset_fletcher32_c
    END INTERFACE
    hdferr = h5pset_fletcher32_c(prp_id)

  END SUBROUTINE h5pset_fletcher32_f

!>
!! \ingroup FH5P
!!
!! \brief Sets offset for family file driver.
!!
!! \param prp_id File creation property list identifier.
!! \param offset File offset.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_family_offset()
!!
  SUBROUTINE h5pset_family_offset_f(prp_id, offset, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HSIZE_T), INTENT(IN) :: offset
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_family_offset_c(prp_id, offset) &
            BIND(C,NAME='h5pset_family_offset_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(IN) :: offset
       END FUNCTION h5pset_family_offset_c
    END INTERFACE
    hdferr = h5pset_family_offset_c(prp_id, offset)

  END SUBROUTINE h5pset_family_offset_f

#ifdef H5_DOXYGEN

!>
!! \ingroup FH5P
!!
!! \brief Sets up use of the multi-file driver.
!!
!! \param prp_id    File creation property list identifier.
!! \param memb_map  Mapping array.
!! \param memb_fapl Property list for each memory usage type.
!! \param memb_name Names of member file.
!! \param memb_addr Offsets within the virtual address space, from 0 (zero) to HADDR_MAX_F, at which each type of data storage begins.
!! \param relax     Flag.
!! \param hdferr    \fortran_error
!!
  SUBROUTINE h5pset_fapl_multi_l(prp_id, memb_map, memb_fapl, memb_name, memb_addr, relax, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, DIMENSION(*), INTENT(IN) :: memb_map
    INTEGER(HID_T), DIMENSION(*), INTENT(IN) :: memb_fapl
    CHARACTER(LEN=*), DIMENSION(*), INTENT(IN) :: memb_name
    REAL, DIMENSION(*), INTENT(IN) :: memb_addr
    LOGICAL, INTENT(IN) :: relax
    INTEGER, INTENT(OUT) :: hdferr
  END SUBROUTINE h5pset_fapl_multi_l

#else

!>
!! \ingroup FH5P
!!
!! \brief Sets up use of the multi-file driver.
!!
!! \param prp_id    File creation property list identifier.
!! \param memb_map  Mapping array.
!! \param memb_fapl Property list for each memory usage type.
!! \param memb_name Names of member file.
!! \param memb_addr Offsets within the virtual address space, from 0 (zero) to HADDR_MAX_F,
!!                  at which each type of data storage begins.
!! \param relax     Flag.
!! \param hdferr    \fortran_error
!!
  SUBROUTINE h5pset_fapl_multi_l(prp_id, memb_map, memb_fapl, memb_name, memb_addr, relax, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, DIMENSION(*), INTENT(IN) :: memb_map
    INTEGER(HID_T), DIMENSION(*), INTENT(IN) :: memb_fapl
    CHARACTER(LEN=*), DIMENSION(*), INTENT(IN) :: memb_name
    REAL, DIMENSION(*), INTENT(IN) :: memb_addr
    LOGICAL, INTENT(IN) :: relax
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER, DIMENSION(1:H5FD_MEM_NTYPES_F) :: lenm
    INTEGER :: maxlen
    INTEGER :: flag = 0
    INTEGER :: i

    INTERFACE
       INTEGER FUNCTION h5pset_fapl_multi_c(prp_id, memb_map, memb_fapl, memb_name, lenm, &
            maxlen, memb_addr, flag) &
            BIND(C,NAME='h5pset_fapl_multi_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, DIMENSION(*), INTENT(IN) :: memb_map
         INTEGER(HID_T), DIMENSION(*), INTENT(IN) :: memb_fapl
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: memb_name
         REAL, DIMENSION(*), INTENT(IN) :: memb_addr
         INTEGER, DIMENSION(*) :: lenm
         INTEGER :: maxlen
         INTEGER, INTENT(IN) :: flag
       END FUNCTION h5pset_fapl_multi_c
    END INTERFACE

    maxlen = LEN(memb_name(1))
    DO i=1, H5FD_MEM_NTYPES_F
       lenm(i) = LEN_TRIM(memb_name(i))
    ENDDO
    IF(relax) flag = 1
    hdferr = h5pset_fapl_multi_c(prp_id, memb_map, memb_fapl, memb_name, lenm, maxlen, memb_addr, flag)

  END SUBROUTINE h5pset_fapl_multi_l
!>
!! \ingroup FH5P
!!
!! \brief Sets up use of the multi-file driver.
!!
!! \param prp_id File creation property list identifier.
!! \param relax  Flag.
!! \param hdferr \fortran_error
!!
  SUBROUTINE h5pset_fapl_multi_s(prp_id, relax, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    LOGICAL, INTENT(IN) :: relax
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: flag

    INTERFACE
       INTEGER FUNCTION h5pset_fapl_multi_sc(prp_id,flag) &
            BIND(C,NAME='h5pset_fapl_multi_sc')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: flag
       END FUNCTION h5pset_fapl_multi_sc
    END INTERFACE
    flag = 0
    IF (relax) flag = 1
    hdferr = h5pset_fapl_multi_sc(prp_id, flag)

  END SUBROUTINE h5pset_fapl_multi_s
!>
!! \ingroup FH5P
!!
!! \brief Sets up use of the multi-file driver.
!!
!! \param prp_id     File creation property list identifier.
!! \param memb_map   Mapping array.
!! \param memb_fapl  Property list for each memory usage type.
!! \param memb_name  Names of member file.
!! \param memb_addr  Offsets within the virtual address space, from 0 (zero) to HADDR_MAX_F, at which
!!                   each type of data storage begins.
!! \param relax      Flag.
!! \param hdferr     \fortran_error
!! \param maxlen_out Maximum length for memb_name array element.
!!
  SUBROUTINE h5pget_fapl_multi_f(prp_id, memb_map, memb_fapl, memb_name, memb_addr, relax, hdferr, maxlen_out)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, DIMENSION(*), INTENT(OUT) :: memb_map
    INTEGER(HID_T), DIMENSION(*), INTENT(OUT) :: memb_fapl
    CHARACTER(LEN=*), DIMENSION(*), INTENT(OUT) :: memb_name
    REAL, DIMENSION(*), INTENT(OUT) :: memb_addr
    INTEGER, INTENT(OUT), OPTIONAL :: maxlen_out
    LOGICAL, INTENT(OUT) :: relax
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER, DIMENSION(1:H5FD_MEM_NTYPES_F) :: lenm
    INTEGER :: maxlen
    INTEGER :: c_maxlen_out
    INTEGER :: flag
    INTEGER :: i

    INTERFACE
       INTEGER FUNCTION h5pget_fapl_multi_c(prp_id, memb_map, memb_fapl, memb_name, lenm, &
            maxlen, memb_addr, flag, c_maxlen_out) &
            BIND(C,NAME='h5pget_fapl_multi_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, DIMENSION(*), INTENT(OUT) :: memb_map
         INTEGER(HID_T), DIMENSION(*), INTENT(OUT) :: memb_fapl
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: memb_name
         REAL, DIMENSION(*), INTENT(OUT) :: memb_addr
         INTEGER, DIMENSION(*) :: lenm
         INTEGER :: maxlen
         INTEGER :: c_maxlen_out
         INTEGER, INTENT(OUT) :: flag
       END FUNCTION h5pget_fapl_multi_c
    END INTERFACE

    maxlen = LEN(memb_name(1))
    DO i=1, H5FD_MEM_NTYPES_F
       lenm(i) = LEN_TRIM(memb_name(i))
    ENDDO
    hdferr = h5pget_fapl_multi_c(prp_id, memb_map, memb_fapl, memb_name, lenm, maxlen, memb_addr, flag, c_maxlen_out)

    relax = .TRUE.
    IF(flag .EQ. 0) relax = .FALSE.
    IF(PRESENT(maxlen_out)) maxlen_out = c_maxlen_out
  END SUBROUTINE h5pget_fapl_multi_f
#endif
!>
!! \ingroup FH5P
!!
!! \brief Sets up use of szip compression
!!
!! \param prp_id           Dataset creation property list identifier.
!! \param options_mask     A bit-mask conveying the desired SZIP options. Current valid values in Fortran are:
!!                         \li H5_SZIP_EC_OM_F
!!                         \li H5_SZIP_NN_OM_F
!! \param pixels_per_block Szip parameters.
!! \param hdferr           \fortran_error
!!
!! See C API: @ref H5Pset_szip()
!!
  SUBROUTINE h5pset_szip_f(prp_id, options_mask, pixels_per_block, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: options_mask
                                         !    H5_SZIP_NN_OM_F
    INTEGER, INTENT(IN) :: pixels_per_block
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_szip_c(prp_id, options_mask, pixels_per_block) &
            BIND(C,NAME='h5pset_szip_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: options_mask
         INTEGER, INTENT(IN) :: pixels_per_block
       END FUNCTION h5pset_szip_c
    END INTERFACE
    hdferr = h5pset_szip_c(prp_id, options_mask, pixels_per_block)

  END SUBROUTINE h5pset_szip_f

!>
!! \ingroup FH5P
!!
!! \brief Checks if all filters set in the dataset creation property list are available.
!!
!! \param prp_id Data creation property list identifier.
!! \param flag   .TRUE. if all filters are available, .FALSE. otherwise.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pall_filters_avail()
!!
  SUBROUTINE h5pall_filters_avail_f(prp_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    LOGICAL, INTENT(OUT) :: flag
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: status

    INTERFACE
       INTEGER FUNCTION h5pall_filters_avail_c(prp_id, status) &
            BIND(C,NAME='h5pall_filters_avail_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: status
       END FUNCTION h5pall_filters_avail_c
    END INTERFACE
    flag = .TRUE.
    hdferr = h5pall_filters_avail_c(prp_id, status)
    IF (status .EQ. 0 ) flag = .FALSE.

  END SUBROUTINE h5pall_filters_avail_f

!>
!! \ingroup FH5P
!!
!! \brief Returns information about a filter in a pipeline
!!
!! \param prp_id    Data creation or transfer property list identifier
!! \param filter_id Filter identifier.
!! \param flags     Bit vector specifying certain general properties of the filter
!! \param cd_nelmts Number of elements in cd_values.
!! \param cd_values Auxiliary data for the filter.
!! \param namelen   Number of characters in the name buffer.
!! \param name      Buffer to retrieve filter name.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pget_filter_by_id2()
!!
  SUBROUTINE h5pget_filter_by_id_f(prp_id, filter_id, flags, cd_nelmts, cd_values, namelen, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: filter_id
    INTEGER(SIZE_T), INTENT(INOUT) :: cd_nelmts
    INTEGER, DIMENSION(*), INTENT(OUT) :: cd_values
    INTEGER, INTENT(OUT) :: flags
    INTEGER(SIZE_T), INTENT(IN) :: namelen
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_filter_by_id_c(prp_id, filter_id, flags, cd_nelmts,  &
            cd_values, namelen, name) &
            BIND(C,NAME='h5pget_filter_by_id_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: filter_id
         INTEGER, DIMENSION(*), INTENT(OUT) :: cd_values
         INTEGER, INTENT(OUT) :: flags
         INTEGER(SIZE_T), INTENT(INOUT) :: cd_nelmts
         INTEGER(SIZE_T), INTENT(IN) :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: name
       END FUNCTION h5pget_filter_by_id_c
    END INTERFACE

    hdferr = h5pget_filter_by_id_c(prp_id, filter_id, flags, cd_nelmts,  &
         cd_values, namelen, name)
  END SUBROUTINE h5pget_filter_by_id_f

!>
!! \ingroup FH5P
!!
!! \brief Adds a filter to the filter pipeline.
!!
!! \param prp_id    Data creation or transfer property list identifier
!! \param filter    Filter to be modified.
!! \param flags     Bit vector specifying certain general properties of the filter
!! \param cd_nelmts Number of elements in cd_values.
!! \param cd_values Auxiliary data for the filter.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pmodify_filter()
!!
  SUBROUTINE h5pmodify_filter_f(prp_id, filter, flags, cd_nelmts, cd_values,  hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: filter
    INTEGER, INTENT(IN) :: flags
    INTEGER(SIZE_T), INTENT(IN) :: cd_nelmts
    INTEGER, DIMENSION(*), INTENT(IN) :: cd_values
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pmodify_filter_c(prp_id, filter, flags, cd_nelmts, cd_values) &
            BIND(C,NAME='h5pmodify_filter_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: filter
         INTEGER, INTENT(IN) :: flags
         INTEGER(SIZE_T), INTENT(IN) :: cd_nelmts
         INTEGER, DIMENSION(*), INTENT(IN) :: cd_values
       END FUNCTION h5pmodify_filter_c
    END INTERFACE

    hdferr = h5pmodify_filter_c(prp_id, filter, flags, cd_nelmts, cd_values )
  END SUBROUTINE h5pmodify_filter_f

!>
!! \ingroup FH5P
!!
!! \brief Delete one or more filters from the filter pipeline.
!!
!! \param prp_id Data creation or transfer property list identifier
!! \param filter Filter to be removed.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Premove_filter()
!!
  SUBROUTINE h5premove_filter_f(prp_id, filter, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: filter
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5premove_filter_c(prp_id, filter) &
            BIND(C,NAME='h5premove_filter_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: filter
       END FUNCTION h5premove_filter_c
    END INTERFACE

    hdferr = h5premove_filter_c(prp_id, filter)
  END SUBROUTINE h5premove_filter_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves attribute storage phase change thresholds
!!
!! \param ocpl_id     Object (dataset or group) creation property list identifier.
!! \param max_compact Maximum number of attributes to be stored in compact storage (Default: 8).
!! \param min_dense   Minimum number of attributes to be stored in dense storage (Default: 6).
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Pget_attr_phase_change()
!!
  SUBROUTINE h5pget_attr_phase_change_f(ocpl_id, max_compact, min_dense, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocpl_id
    INTEGER, INTENT(OUT) :: max_compact
    INTEGER, INTENT(OUT) :: min_dense
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_attr_phase_change_c(ocpl_id, max_compact, min_dense) &
            BIND(C,NAME='h5pget_attr_phase_change_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: ocpl_id
         INTEGER, INTENT(OUT) :: max_compact
         INTEGER, INTENT(OUT) :: min_dense

       END FUNCTION h5pget_attr_phase_change_c
    END INTERFACE

    hdferr = h5pget_attr_phase_change_c(ocpl_id, max_compact, min_dense)
  END SUBROUTINE h5pget_attr_phase_change_f

!>
!! \ingroup FH5P
!!
!! \brief Sets tracking and indexing of attribute creation order
!!
!! \param ocpl_id         Object creation property list identifier.
!! \param crt_order_flags Flags specifying whether to track and index attribute creation order.
!! \param hdferr          \fortran_error
!!
!! See C API: @ref H5Pset_attr_creation_order()
!!
  SUBROUTINE h5pset_attr_creation_order_f(ocpl_id, crt_order_flags , hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocpl_id
    INTEGER, INTENT(IN) :: crt_order_flags
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION H5Pset_attr_creation_order_c(ocpl_id, crt_order_flags) &
            BIND(C,NAME='h5pset_attr_creation_order_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: ocpl_id
         INTEGER, INTENT(IN) :: crt_order_flags

       END FUNCTION H5Pset_attr_creation_order_c
    END INTERFACE

    hdferr = H5Pset_attr_creation_order_c(ocpl_id, crt_order_flags)
  END SUBROUTINE h5pset_attr_creation_order_f

!>
!! \ingroup FH5P
!!
!! \brief Sets number of shared object header message indexes
!!
!! \param plist_id File creation property list.
!! \param nindexes Number of shared object header message indexes to be available in files created with this property list.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_shared_mesg_nindexes()
!!
  SUBROUTINE h5pset_shared_mesg_nindexes_f( plist_id, nindexes, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER, INTENT(IN) :: nindexes
    INTEGER, INTENT(OUT) :: hdferr
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_shared_mesg_nindexes_c(plist_id, nindexes) &
            BIND(C,NAME='h5pset_shared_mesg_nindexes_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(IN) :: nindexes

       END FUNCTION H5pset_shared_mesg_nindexes_c
    END INTERFACE

    hdferr = h5pset_shared_mesg_nindexes_c(plist_id, nindexes)

  END SUBROUTINE h5pset_shared_mesg_nindexes_f

!>
!! \ingroup FH5P
!!
!! \brief Configures the specified shared object header message index
!!
!! \param fcpl_id         File creation property list identifier.
!! \param index_num       Index being configured.
!! \param mesg_type_flags Types of messages that should be stored in this index.
!! \param min_mesg_size   Minimum message size.
!! \param hdferr          \fortran_error
!!
!! See C API: @ref H5Pset_shared_mesg_index()
!!
  SUBROUTINE h5pset_shared_mesg_index_f(fcpl_id, index_num, mesg_type_flags, min_mesg_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: fcpl_id
    INTEGER, INTENT(IN) :: index_num
    INTEGER, INTENT(IN) :: mesg_type_flags
    INTEGER, INTENT(IN) :: min_mesg_size
    INTEGER, INTENT(OUT) :: hdferr
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_shared_mesg_index_c(fcpl_id, index_num, mesg_type_flags, min_mesg_size) &
            BIND(C,NAME='h5pset_shared_mesg_index_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: fcpl_id
         INTEGER, INTENT(IN) :: index_num
         INTEGER, INTENT(IN) :: mesg_type_flags
         INTEGER, INTENT(IN) :: min_mesg_size
       END FUNCTION H5pset_shared_mesg_index_c
    END INTERFACE

    hdferr = h5pset_shared_mesg_index_c(fcpl_id, index_num, mesg_type_flags, min_mesg_size)

  END SUBROUTINE h5pset_shared_mesg_index_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves tracking and indexing settings for attribute creation order
!!
!! \param ocpl_id         Object (group or dataset) creation property list identifier.
!! \param crt_order_flags Flags specifying whether to track and index attribute creation order.
!! \param hdferr          \fortran_error
!!
!! See C API: @ref H5Pget_attr_creation_order()
!!
  SUBROUTINE h5pget_attr_creation_order_f(ocpl_id, crt_order_flags, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocpl_id
    INTEGER, INTENT(OUT) :: crt_order_flags
    INTEGER, INTENT(OUT) :: hdferr
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_attr_creation_order_c(ocpl_id, crt_order_flags) &
            BIND(C,NAME='h5pget_attr_creation_order_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: ocpl_id
         INTEGER, INTENT(OUT) :: crt_order_flags
       END FUNCTION H5pget_attr_creation_order_c
    END INTERFACE

    hdferr = h5pget_attr_creation_order_c(ocpl_id, crt_order_flags)

  END SUBROUTINE h5pget_attr_creation_order_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the lower and upper bounds on the HDF5 library release versions that indirectly
!!        determine the object format versions used when creating objects in the file.
!!
!! \param fapl_id File access property list identifier.
!! \param low     The earliest version of the library that will be used for writing objects.
!! \param high    The latest version of the library that will be used for writing objects.
!! \param hdferr  \fortran_error
!!
!! Fortran Interface:
!! See C API: @ref H5Pget_libver_bounds()
!!
  SUBROUTINE h5pget_libver_bounds_f(fapl_id, low, high, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: fapl_id
    INTEGER, INTENT(OUT) :: low
    INTEGER, INTENT(OUT) :: high
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(C_INT) :: low_c, high_c
    INTEGER(C_INT) :: hdferr_c
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER(C_INT) FUNCTION h5pget_libver_bounds(fapl_id, low, high) &
            BIND(C,NAME='H5Pget_libver_bounds')
         IMPORT :: C_INT, HID_T
         IMPLICIT NONE
         INTEGER(HID_T) , INTENT(IN) , VALUE :: fapl_id
         INTEGER(C_INT), INTENT(OUT) :: low
         INTEGER(C_INT), INTENT(OUT) :: high
       END FUNCTION h5pget_libver_bounds
    END INTERFACE

    hdferr_c = H5Pget_libver_bounds(fapl_id, low_c, high_c)

    low  = INT(low_c)
    high = INT(high_c)

    hdferr = 0
    IF(hdferr_c.LT.0) hdferr = -1

  END SUBROUTINE h5pget_libver_bounds_f

!>
!! \ingroup FH5P
!!
!! \brief Sets bounds on library versions, and indirectly format versions, to be used when creating objects.
!!
!! \param fapl_id File access property list identifier.
!! \param low     The earliest version of the library that will be used for writing objects.
!! \param high    The latest version of the library that will be used for writing objects.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pset_libver_bounds()
!!
  SUBROUTINE h5pset_libver_bounds_f(fapl_id, low, high, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: fapl_id
    INTEGER, INTENT(IN)  :: low
    INTEGER, INTENT(IN)  :: high
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(C_INT) :: hdferr_c
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER(C_INT) FUNCTION h5pset_libver_bounds(fapl_id, low, high) &
            BIND(C,NAME='H5Pset_libver_bounds')
         IMPORT :: C_INT, HID_T
         IMPLICIT NONE
         INTEGER(HID_T),  INTENT(IN), VALUE :: fapl_id
         INTEGER(C_INT), INTENT(IN), VALUE :: low
         INTEGER(C_INT), INTENT(IN), VALUE :: high
       END FUNCTION h5pset_libver_bounds
    END INTERFACE

    hdferr_c = h5pset_libver_bounds(fapl_id, INT(low, C_INT), INT(high, C_INT))

    hdferr = 0
    IF(hdferr_c.LT.0) hdferr = -1

  END SUBROUTINE h5pset_libver_bounds_f

!>
!! \ingroup FH5P
!!
!! \brief Sets creation order tracking and indexing for links in a group.
!!
!! \param gcpl_id         Group creation property list identifier.
!! \param crt_order_flags Creation order flag(s).
!! \param hdferr          \fortran_error
!!
!! See C API: @ref H5Pset_link_creation_order()
!!
  SUBROUTINE h5pset_link_creation_order_f(gcpl_id, crt_order_flags, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id
    INTEGER, INTENT(IN) :: crt_order_flags
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_link_creation_order_c(gcpl_id, crt_order_flags) &
            BIND(C,NAME='h5pset_link_creation_order_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER, INTENT(IN) :: crt_order_flags

       END FUNCTION H5pset_link_creation_order_c
    END INTERFACE

    hdferr = h5pset_link_creation_order_c(gcpl_id, crt_order_flags)

  END SUBROUTINE h5pset_link_creation_order_f

!>
!! \ingroup FH5P
!!
!! \brief Queries the settings for conversion between compact and dense groups.
!!
!! \param gcpl_id     Group creation property list identifier.
!! \param max_compact Maximum number of attributes to be stored in compact storage.
!! \param min_dense   Minimum number of attributes to be stored in dense storage.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Pget_link_phase_change()
!!
  SUBROUTINE h5pget_link_phase_change_f(gcpl_id, max_compact, min_dense, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id
    INTEGER, INTENT(OUT) :: max_compact
    INTEGER, INTENT(OUT) :: min_dense
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_link_phase_change_c(gcpl_id, max_compact, min_dense) &
            BIND(C,NAME='h5pget_link_phase_change_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER, INTENT(OUT) :: max_compact
         INTEGER, INTENT(OUT) :: min_dense

       END FUNCTION h5pget_link_phase_change_c
    END INTERFACE

    hdferr = h5pget_link_phase_change_c(gcpl_id, max_compact, min_dense)
  END SUBROUTINE h5pget_link_phase_change_f

!>
!! \ingroup FH5P
!!
!! \brief Returns whether times are tracked for an object.
!!
!! \param plist_id Property list id.
!! \param flag     Object timestamp setting, .TRUE. or .FALSE.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_obj_track_times()
!!
  SUBROUTINE h5pget_obj_track_times_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    LOGICAL, INTENT(OUT) :: flag
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: status
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_obj_track_times_c(plist_id, status) &
            BIND(C,NAME='h5pget_obj_track_times_c')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(OUT) :: status
       END FUNCTION h5pget_obj_track_times_c
    END INTERFACE
    flag = .TRUE.
    hdferr = h5pget_obj_track_times_c(plist_id, status)
    IF(status.EQ.0) flag = .FALSE.

  END SUBROUTINE h5pget_obj_track_times_f

!>
!! \ingroup FH5P
!!
!! \brief Set whether the birth, access, modification & change times for an object are stored.
!!
!! \param plist_id Property list id.
!! \param flag     Object timestamp setting, .TRUE. or .FALSE.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_obj_track_times()
!!
  SUBROUTINE h5pset_obj_track_times_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    LOGICAL, INTENT(IN) :: flag
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: status
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_obj_track_times_c(plist_id, status) &
            BIND(C,NAME='h5pset_obj_track_times_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(IN) :: status
       END FUNCTION h5pset_obj_track_times_c
    END INTERFACE

    status = 0
    IF(flag) status = 1

    hdferr = h5pset_obj_track_times_c(plist_id, status)

  END SUBROUTINE h5pset_obj_track_times_f

!>
!! \ingroup FH5P
!!
!! \brief Specifies in property list whether to create missing intermediate groups.
!!
!! \param lcpl_id            Link creation property list identifier.
!! \param crt_intermed_group Specifies whether to create intermediate groups upon the creation of an object.
!! \param hdferr             \fortran_error
!!
!! See C API: @ref H5Pset_create_intermediate_group()
!!
  SUBROUTINE h5pset_create_inter_group_f(lcpl_id, crt_intermed_group, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: lcpl_id
    INTEGER, INTENT(IN) :: crt_intermed_group
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_create_inter_group_c(lcpl_id, crt_intermed_group) &
            BIND(C,NAME='h5pset_create_inter_group_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: lcpl_id
         INTEGER, INTENT(IN) :: crt_intermed_group
       END FUNCTION h5pset_create_inter_group_c
    END INTERFACE

    hdferr = h5pset_create_inter_group_c(lcpl_id, crt_intermed_group)

  END SUBROUTINE h5pset_create_inter_group_f

!>
!! \ingroup FH5P
!!
!! \brief Queries whether link creation order is tracked and/or indexed in a group.
!!
!! \param gcpl_id         Group creation property list identifier.
!! \param crt_order_flags Creation order flag(s).
!! \param hdferr          \fortran_error
!!
!! See C API: @ref H5Pget_link_creation_order()
!!
  SUBROUTINE h5pget_link_creation_order_f(gcpl_id, crt_order_flags, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id
    INTEGER, INTENT(OUT) :: crt_order_flags
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_link_creation_order_c(gcpl_id, crt_order_flags) &
            BIND(C,NAME='h5pget_link_creation_order_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER, INTENT(OUT) :: crt_order_flags

       END FUNCTION H5pget_link_creation_order_c
    END INTERFACE

    hdferr = h5pget_link_creation_order_c(gcpl_id, crt_order_flags)

  END SUBROUTINE h5pget_link_creation_order_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the character encoding used to encode a string.
!!
!! \param plist_id Property list identifier.
!! \param encoding Valid values for encoding are:
!!                 \li H5T_CSET_ASCII_F -> US ASCII
!!                 \li H5T_CSET_UTF8_F -> UTF-8 Unicode encoding
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_char_encoding()
!!
  SUBROUTINE h5pset_char_encoding_f(plist_id, encoding, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    INTEGER, INTENT(IN) :: encoding
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_char_encoding_c(plist_id, encoding) &
            BIND(C,NAME='h5pset_char_encoding_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(IN) :: encoding

       END FUNCTION H5pset_char_encoding_c
    END INTERFACE

    hdferr = h5pset_char_encoding_c(plist_id, encoding)

  END SUBROUTINE h5pset_char_encoding_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the character encoding used to create a string
!!
!! \param plist_id Property list identifier.
!! \param encoding Valid values for encoding are:
!!                 \li H5T_CSET_ASCII_F -> US ASCII
!!                 \li H5T_CSET_UTF8_F -> UTF-8 Unicode encoding
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_char_encoding()
!!
  SUBROUTINE  h5pget_char_encoding_f(plist_id, encoding, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id

    INTEGER, INTENT(OUT) :: encoding
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_char_encoding_c(plist_id, encoding) &
            BIND(C,NAME='h5pget_char_encoding_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(OUT) :: encoding

       END FUNCTION H5pget_char_encoding_c
    END INTERFACE

    hdferr = h5pget_char_encoding_c(plist_id, encoding)

  END SUBROUTINE h5pget_char_encoding_f

!>
!! \ingroup FH5P
!!
!! \brief Sets properties to be used when an object is copied.
!!
!! \param ocp_plist_id Object copy property list identifier.
!! \param copy_options Copy option(s) to be set.
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Pset_copy_object()
!!
  SUBROUTINE h5pset_copy_object_f(ocp_plist_id, copy_options, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocp_plist_id
    INTEGER, INTENT(IN) :: copy_options
                                        !   H5O_COPY_EXPAND_REFERENCE_F
                                        !   H5O_COPY_WITHOUT_ATTR_FLAG_F
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_copy_object_c(ocp_plist_id, copy_options) &
            BIND(C,NAME='h5pset_copy_object_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: ocp_plist_id
         INTEGER, INTENT(IN) :: copy_options
       END FUNCTION h5pset_copy_object_c
    END INTERFACE
    hdferr = h5pset_copy_object_c(ocp_plist_id, copy_options)
  END SUBROUTINE h5pset_copy_object_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the properties to be used when an object is copied.
!!
!! \param ocp_plist_id Object copy property list identifier.
!! \param copy_options Copy option(s) to be get.
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Pget_copy_object()
!!
  SUBROUTINE h5pget_copy_object_f(ocp_plist_id, copy_options, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocp_plist_id
    INTEGER, INTENT(OUT) :: copy_options
                                               !   H5O_COPY_EXPAND_REFERENCE_F
                                               !   H5O_COPY_WITHOUT_ATTR_FLAG_F
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_copy_object_c(ocp_plist_id, copy_options) &
            BIND(C,NAME='h5pget_copy_object_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: ocp_plist_id
         INTEGER, INTENT(OUT) :: copy_options
       END FUNCTION h5pget_copy_object_c
    END INTERFACE
    hdferr = h5pget_copy_object_c(ocp_plist_id, copy_options)
  END SUBROUTINE h5pget_copy_object_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves a data transform expression.
!!
!! \param plist_id   Identifier of the property list or class.
!! \param expression Buffer to hold transform expression.
!! \param hdferr     Error code:
!!                     Success:  Actual length of the expression. If provided buffer "expression" is
!!                               smaller, than expression will be truncated to fit into provided user buffer.
!!                     Failure: -1
!! \param size       Registered size of the transform expression
!!
!! See C API: @ref H5Pget_data_transform()
!!
SUBROUTINE h5pget_data_transform_f(plist_id, expression, hdferr, size)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    CHARACTER(LEN=*), INTENT(OUT) :: expression
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: expression_len
    INTEGER(SIZE_T) :: size_default

    INTERFACE
       INTEGER FUNCTION h5pget_data_transform_c(plist_id, expression, expression_len, size_default) &
            BIND(C,NAME='h5pget_data_transform_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: expression
         INTEGER(SIZE_T) :: size_default
         INTEGER :: expression_len
       END FUNCTION h5pget_data_transform_c
    END INTERFACE

    size_default = 0
    expression_len = LEN(expression)

    hdferr = h5pget_data_transform_c(plist_id, expression, expression_len, size_default)

    IF(present(size)) size = size_default

  END SUBROUTINE h5pget_data_transform_f

!>
!! \ingroup FH5P
!!
!! \brief Sets a data transform expression.
!!
!! \param plist_id   Identifier of the property list or class.
!! \param expression Buffer to hold transform expression.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Pset_data_transform()
!!
  SUBROUTINE h5pset_data_transform_f(plist_id, expression, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id
    CHARACTER(LEN=*), INTENT(IN) :: expression
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: expression_len

    INTERFACE
       INTEGER FUNCTION h5pset_data_transform_c(plist_id, expression, expression_len) &
            BIND(C,NAME='h5pset_data_transform_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: expression
         INTEGER :: expression_len
       END FUNCTION h5pset_data_transform_c
    END INTERFACE

    expression_len = LEN(expression)
    hdferr = h5pset_data_transform_c(plist_id, expression, expression_len)

  END SUBROUTINE h5pset_data_transform_f

!>
!! \ingroup FH5P
!!
!! \brief Queries the local heap size hint for original-style groups.
!!
!! \param gcpl_id   Group creation property list identifier.
!! \param size_hint Hint for size of local heap.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pget_local_heap_size_hint()
!!
  SUBROUTINE h5pget_local_heap_size_hint_f(gcpl_id, size_hint, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id
    INTEGER(SIZE_T), INTENT(OUT) :: size_hint
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_local_heap_size_hint_c(gcpl_id, size_hint) &
            BIND(C,NAME='h5pget_local_heap_size_hint_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER(SIZE_T), INTENT(OUT) :: size_hint
       END FUNCTION H5Pget_local_heap_size_hint_c
    END INTERFACE

    hdferr = H5Pget_local_heap_size_hint_c(gcpl_id, size_hint)

  END SUBROUTINE h5pget_local_heap_size_hint_f

!>
!! \ingroup FH5P
!!
!! \brief Queries data required to estimate required local heap or object header size.
!!
!! \param gcpl_id         Group creation property list identifier.
!! \param est_num_entries Estimated number of links to be inserted into group.
!! \param est_name_len    Estimated average length of link names.
!! \param hdferr          \fortran_error
!!
!! See C API: @ref H5Pget_est_link_info()
!!
  SUBROUTINE h5pget_est_link_info_f(gcpl_id, est_num_entries, est_name_len, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id
    INTEGER, INTENT(OUT) :: est_num_entries
    INTEGER, INTENT(OUT) :: est_name_len
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_est_link_info_c(gcpl_id, est_num_entries, est_name_len) &
            BIND(C,NAME='h5pget_est_link_info_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER, INTENT(OUT) :: est_num_entries
         INTEGER, INTENT(OUT) :: est_name_len
       END FUNCTION h5pget_est_link_info_c
    END INTERFACE

    hdferr = h5pget_est_link_info_c(gcpl_id, est_num_entries, est_name_len)

  END SUBROUTINE h5pget_est_link_info_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the local heap size hint for original-style groups.
!!
!! \param gcpl_id   Group creation property list identifier.
!! \param size_hint Hint for size of local heap.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pset_local_heap_size_hint()
!!
  SUBROUTINE h5pset_local_heap_size_hint_f(gcpl_id, size_hint, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id
    INTEGER(SIZE_T), INTENT(IN) :: size_hint
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_local_heap_size_hint_c(gcpl_id, size_hint) &
            BIND(C,NAME='h5pset_local_heap_size_hint_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER(SIZE_T), INTENT(IN) :: size_hint
       END FUNCTION h5pset_local_heap_size_hint_c
    END INTERFACE

    hdferr = H5Pset_local_heap_size_hint_c(gcpl_id, size_hint)

  END SUBROUTINE h5pset_local_heap_size_hint_f

!>
!! \ingroup FH5P
!!
!! \brief Sets estimated number of links and length of link names in a group.
!!
!! \param gcpl_id         Group creation property list identifier.
!! \param est_num_entries Estimated number of links to be inserted into group.
!! \param est_name_len    Estimated average length of link names.
!! \param hdferr          \fortran_error
!!
!! See C API: @ref H5Pset_est_link_info()
!!
  SUBROUTINE h5pset_est_link_info_f(gcpl_id, est_num_entries, est_name_len, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id
    INTEGER, INTENT(IN) :: est_num_entries
    INTEGER, INTENT(IN) :: est_name_len
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_est_link_info_c(gcpl_id, est_num_entries, est_name_len) &
            BIND(C,NAME='h5pset_est_link_info_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER, INTENT(IN) :: est_num_entries
         INTEGER, INTENT(IN) :: est_name_len
       END FUNCTION h5pset_est_link_info_c
    END INTERFACE

    hdferr = H5Pset_est_link_info_c(gcpl_id, est_num_entries, est_name_len)

  END SUBROUTINE h5pset_est_link_info_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the parameters for conversion between compact and dense groups.
!!
!! \param gcpl_id     Group creation property list identifier.
!! \param max_compact Maximum number of attributes to be stored in compact storage.
!! \param min_dense   Minimum number of attributes to be stored in dense storage.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Pset_link_phase_change()
!!
SUBROUTINE h5pset_link_phase_change_f(gcpl_id, max_compact, min_dense, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id
    INTEGER, INTENT(IN) :: max_compact
    INTEGER, INTENT(IN) :: min_dense
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_link_phase_change_c(gcpl_id, max_compact, min_dense) &
            BIND(C,NAME='h5pset_link_phase_change_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER, INTENT(IN) :: max_compact
         INTEGER, INTENT(IN) :: min_dense
       END FUNCTION h5pset_link_phase_change_c
    END INTERFACE

    hdferr = h5pset_link_phase_change_c(gcpl_id, max_compact, min_dense)
  END SUBROUTINE h5pset_link_phase_change_f

!>
!! \ingroup FH5P
!!
!! \brief Sets up use of the direct I/O driver.
!!
!! \param fapl_id    File access property list identifier.
!! \param alignment  Required memory alignment boundary.
!! \param block_size File system block size.
!! \param cbuf_size  Copy buffer size.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Pset_fapl_direct()
!!
SUBROUTINE h5pset_fapl_direct_f(fapl_id, alignment, block_size, cbuf_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: fapl_id
    INTEGER(SIZE_T), INTENT(IN) :: alignment
    INTEGER(SIZE_T), INTENT(IN) :: block_size
    INTEGER(SIZE_T), INTENT(IN) :: cbuf_size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_direct_c(fapl_id, alignment, block_size, cbuf_size) &
            BIND(C,NAME='h5pset_fapl_direct_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: fapl_id
         INTEGER(SIZE_T), INTENT(IN) :: alignment
         INTEGER(SIZE_T), INTENT(IN) :: block_size
         INTEGER(SIZE_T), INTENT(IN) :: cbuf_size
       END FUNCTION h5pset_fapl_direct_c
    END INTERFACE

    hdferr = H5Pset_fapl_direct_c(fapl_id, alignment, block_size, cbuf_size)
  END SUBROUTINE h5pset_fapl_direct_f

!>
!! \ingroup FH5P
!!
!! \brief Gets up use of the direct I/O driver.
!!
!! \param fapl_id    File access property list identifier.
!! \param alignment  Required memory alignment boundary.
!! \param block_size File system block size.
!! \param cbuf_size  Copy buffer size.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Pget_fapl_direct()
!!
  SUBROUTINE h5pget_fapl_direct_f(fapl_id, alignment, block_size, cbuf_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: fapl_id
    INTEGER(SIZE_T), INTENT(OUT) :: alignment
    INTEGER(SIZE_T), INTENT(OUT) :: block_size
    INTEGER(SIZE_T), INTENT(OUT) :: cbuf_size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_fapl_direct_c(fapl_id, alignment, block_size, cbuf_size) &
            BIND(C,NAME='h5pget_fapl_direct_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: fapl_id
         INTEGER(SIZE_T), INTENT(OUT) :: alignment
         INTEGER(SIZE_T), INTENT(OUT) :: block_size
         INTEGER(SIZE_T), INTENT(OUT) :: cbuf_size
       END FUNCTION h5pget_fapl_direct_c
    END INTERFACE

    hdferr = H5Pget_fapl_direct_c(fapl_id, alignment, block_size, cbuf_size)

  END SUBROUTINE h5pget_fapl_direct_f

!>
!! \ingroup FH5P
!!
!! \brief Sets attribute storage phase change thresholds.
!!
!! \param ocpl_id     Object (dataset or group) creation property list identifier.
!! \param max_compact Maximum number of attributes to be stored in compact storage, (Default: 8).
!! \param min_dense   Minimum number of attributes to be stored in dense storage, (Default: 6).
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Pset_attr_phase_change()
!!
SUBROUTINE h5pset_attr_phase_change_f(ocpl_id, max_compact, min_dense, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocpl_id
    INTEGER, INTENT(IN) :: max_compact
    INTEGER, INTENT(IN) :: min_dense
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_attr_phase_change_c(ocpl_id, max_compact, min_dense) &
            BIND(C,NAME='h5pset_attr_phase_change_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: ocpl_id
         INTEGER, INTENT(IN) :: max_compact
         INTEGER, INTENT(IN) :: min_dense

       END FUNCTION h5pset_attr_phase_change_c
    END INTERFACE

    hdferr = h5pset_attr_phase_change_c(ocpl_id, max_compact, min_dense)

  END SUBROUTINE h5pset_attr_phase_change_f

!>
!! \ingroup FH5P
!!
!! \brief Sets up the use of the N-Bit filter.
!!
!! \param plist_id Dataset creation property list identifier.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pset_nbit()
!!
  SUBROUTINE h5pset_nbit_f(plist_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: plist_id
    INTEGER       , INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION H5Pset_nbit_c(plist_id) &
            BIND(C,NAME='h5pset_nbit_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
       END FUNCTION H5Pset_nbit_c
    END INTERFACE

    hdferr = H5Pset_nbit_c(plist_id)

  END SUBROUTINE h5pset_nbit_f

!>
!! \ingroup FH5P
!!
!! \brief Sets up the use of the scale-offset filter.
!!
!! \param plist_id     Dataset creation property list identifier.
!! \param scale_type   Flag indicating compression method. Valid values:
!!                     \li H5Z_SO_FLOAT_DSCALE_F
!!                     \li H5Z_SO_FLOAT_ESCALE_F
!!                     \li H5Z_SO_INT_F
!! \param scale_factor Parameter related to scale.
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Pset_scaleoffset()
!!
  SUBROUTINE h5pset_scaleoffset_f(plist_id, scale_type, scale_factor, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: plist_id
    INTEGER       , INTENT(IN)  :: scale_type
    INTEGER       , INTENT(IN)  :: scale_factor
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pset_scaleoffset_c(plist_id, scale_type, scale_factor) &
            BIND(C,NAME='h5pset_scaleoffset_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(IN) :: scale_type
         INTEGER, INTENT(IN) :: scale_factor
       END FUNCTION h5pset_scaleoffset_c
    END INTERFACE

    hdferr = H5Pset_scaleoffset_c(plist_id, scale_type, scale_factor)

  END SUBROUTINE h5pset_scaleoffset_f

!>
!! \ingroup FH5P
!!
!! \brief Sets maximum number of soft or user-defined link traversals.
!!
!! \param lapl_id File access property list identifier.
!! \param nlinks  Maximum number of links to traverse.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pset_nlinks()
!!
  SUBROUTINE h5pset_nlinks_f(lapl_id, nlinks, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: lapl_id
    INTEGER(SIZE_T), INTENT(IN) :: nlinks
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_nlinks_c(lapl_id, nlinks) &
            BIND(C,NAME='h5pset_nlinks_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: lapl_id
         INTEGER(SIZE_T), INTENT(IN) :: nlinks
       END FUNCTION h5pset_nlinks_c
    END INTERFACE

    hdferr = h5pset_nlinks_c(lapl_id, nlinks)

  END SUBROUTINE h5pset_nlinks_f

!>
!! \ingroup FH5P
!!
!! \brief Gets maximum number of soft or user-defined link traversals.
!!
!! \param lapl_id File access property list identifier.
!! \param nlinks  Maximum number of links to traverse.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pget_nlinks()
!!
  SUBROUTINE h5pget_nlinks_f(lapl_id, nlinks, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: lapl_id
    INTEGER(SIZE_T), INTENT(OUT) :: nlinks
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_nlinks_c(lapl_id, nlinks) &
            BIND(C,NAME='h5pget_nlinks_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: lapl_id
         INTEGER(SIZE_T), INTENT(OUT) :: nlinks
       END FUNCTION h5pget_nlinks_c
    END INTERFACE

    hdferr = h5pget_nlinks_c(lapl_id, nlinks)

  END SUBROUTINE h5pget_nlinks_f

!>
!! \ingroup FH5P
!!
!! \brief Determines whether property is set to enable creating missing intermediate groups.
!!
!! \param lcpl_id            Link creation property list identifier.
!! \param crt_intermed_group Specifying whether to create intermediate groups upon the creation of an object.
!! \param hdferr             \fortran_error
!!
!! See C API: @ref H5Pget_create_intermediate_group()
!!
  SUBROUTINE h5pget_create_inter_group_f(lcpl_id, crt_intermed_group, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: lcpl_id
    INTEGER, INTENT(IN) :: crt_intermed_group
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_create_inter_group_c(lcpl_id, crt_intermed_group) &
            BIND(C,NAME='h5pget_create_inter_group_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: lcpl_id
         INTEGER, INTENT(IN) :: crt_intermed_group
       END FUNCTION h5pget_create_inter_group_c
    END INTERFACE

    hdferr = h5pget_create_inter_group_c(lcpl_id, crt_intermed_group)

  END SUBROUTINE h5pget_create_inter_group_f

!>
!! \ingroup FH5P
!!
!! \brief Set the number of objects in the meta data cache and the maximum number of chunks and bytes in the raw data chunk cache.
!!  Once set, these values will override the values in the file access
!!  property list.  Each of these values can be individually unset
!!  (or not set at all) by passing the macros:
!!    H5D_CHUNK_CACHE_NSLOTS_DFLT_F,
!!    H5D_CHUNK_CACHE_NBYTES_DFLT_F, and/or
!!    H5D_CHUNK_CACHE_W0_DFLT_F
!!    as appropriate.
!!
!!  The RDCC_W0 value should be between 0 and 1 inclusive and
!!  indicates how much chunks that have been fully read or fully
!!  written are favored for preemption.  A value of zero means
!!  fully read or written chunks are treated no differently than
!!  other chunks (the preemption is strictly LRU) while a value
!!  of one means fully read chunks are always preempted before
!!  other chunks.
!!
!! \param dapl_id     Dataset access property list identifier.
!! \param rdcc_nslots The number of chunk slots in the raw data chunk cache for this dataset.
!! \param rdcc_nbytes The total size of the raw data chunk cache for this dataset.
!! \param rdcc_w0     The chunk preemption policy for this dataset.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Pset_chunk_cache()
!!
  SUBROUTINE h5pset_chunk_cache_f(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dapl_id
    INTEGER(SIZE_T), INTENT(IN) :: rdcc_nslots
    INTEGER(SIZE_T), INTENT(IN) :: rdcc_nbytes
    REAL, INTENT(IN) :: rdcc_w0
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pset_chunk_cache_c(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0) &
            BIND(C,NAME='h5pset_chunk_cache_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dapl_id
         INTEGER(SIZE_T), INTENT(IN) :: rdcc_nslots
         INTEGER(SIZE_T), INTENT(IN) :: rdcc_nbytes
         REAL, INTENT(IN) :: rdcc_w0
       END FUNCTION h5pset_chunk_cache_c
    END INTERFACE

    hdferr = h5pset_chunk_cache_c(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0)

  END SUBROUTINE h5pset_chunk_cache_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the raw data chunk cache parameters.
!!
!! \param dapl_id     Dataset access property list identifier.
!! \param rdcc_nslots Number of chunk slots in the raw data chunk cache hash table.
!! \param rdcc_nbytes Total size of the raw data chunk cache, in bytes.
!! \param rdcc_w0     Preemption policy.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Pget_chunk_cache()
!!
  SUBROUTINE h5pget_chunk_cache_f(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dapl_id
    INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nslots
    INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nbytes
    REAL, INTENT(OUT) :: rdcc_w0
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_chunk_cache_c(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0) &
            BIND(C,NAME='h5pget_chunk_cache_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dapl_id
         INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nslots
         INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nbytes
         REAL, INTENT(OUT) :: rdcc_w0
       END FUNCTION h5pget_chunk_cache_c
    END INTERFACE

    hdferr = h5pget_chunk_cache_c(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0)

  END SUBROUTINE h5pget_chunk_cache_f

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5P
!!
!! \brief Sets fill value for a dataset creation property list
!!
!! \attention  \fortran_approved
!!
!! \param prp_id    Property list identifier.
!! \param type_id   Datatype identifier of fill value datatype (in memory).
!! \param fillvalue Fillvalue.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pset_fill_value()
!!
  SUBROUTINE h5pset_fill_value_f(prp_id, type_id, fillvalue, hdferr)
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    INTEGER(HID_T), INTENT(IN)  :: type_id
    TYPE(C_PTR)   , INTENT(IN)  :: fillvalue
    INTEGER       , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pset_fill_value_f

!>
!! \ingroup FH5P
!!
!! \brief Gets fill value for a dataset creation property list
!!
!! \attention  \fortran_approved
!!
!! \param prp_id    Property list identifier.
!! \param type_id   Datatype identifier of fill value datatype (in memory).
!! \param fillvalue Fillvalue.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pget_fill_value()
!!
  SUBROUTINE h5pget_fill_value_f(prp_id, type_id, fillvalue, hdferr)
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    INTEGER(HID_T), INTENT(IN)  :: type_id
    TYPE(C_PTR)   , INTENT(IN)  :: fillvalue
    INTEGER       , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pget_fill_value_f

!>
!! \ingroup FH5P
!!
!! \brief Sets fill value for a dataset creation property list
!!
!! \attention  \fortran_obsolete
!!
!! \param prp_id    Property list identifier.
!! \param type_id   Datatype identifier of fill value datatype (in memory).
!! \param fillvalue Fillvalue.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pset_fill_value()
!!
  SUBROUTINE h5pset_fill_value_f(prp_id, type_id, fillvalue, hdferr)
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    INTEGER(HID_T), INTENT(IN)  :: type_id
    TYPE(TYPE)    , INTENT(IN)  :: fillvalue
    INTEGER       , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pset_fill_value_f

!>
!! \ingroup FH5P
!!
!! \brief Gets fill value for a dataset creation property list.
!!
!! \attention  \fortran_obsolete
!!
!! \param prp_id    Property list identifier.
!! \param type_id   Datatype identifier of fill value datatype (in memory).
!! \param fillvalue Fillvalue.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pget_fill_value()
!!
  SUBROUTINE h5pget_fill_value_f(prp_id, type_id, fillvalue, hdferr)
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    INTEGER(HID_T), INTENT(IN)  :: type_id
    TYPE(TYPE)    , INTENT(OUT) :: fillvalue
    INTEGER       , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pget_fill_value_f

!>
!! \ingroup FH5P
!!
!! \brief Sets a property list value.
!!
!! \attention  \fortran_approved
!!
!! \param prp_id Property list identifier to modify.
!! \param name   Name of property to modify.
!! \param value  Pointer to value to set the property to.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset()
!!
  SUBROUTINE h5pset_f(prp_id, name, value, hdferr)
    INTEGER(HID_T)  , INTENT(IN)  :: prp_id
    CHARACTER(LEN=*), INTENT(IN)  :: name
    TYPE(C_PTR)     , INTENT(IN)  :: value
    INTEGER         , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pset_f

!>
!! \ingroup FH5P
!!
!! \brief Sets a property list value.
!!
!! \attention  \fortran_obsolete
!!
!! \param prp_id Property list identifier to modify.
!! \param name   Name of property to modify.
!! \param value  Property value, supported types are:
!!               \li INTEGER
!!               \li REAL
!!               \li DOUBLE PRECISION
!!               \li CHARACTER(LEN=*)
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset()
!!
  SUBROUTINE h5pset_f(prp_id, name, value, hdferr)
    INTEGER(HID_T)  , INTENT(IN)  :: prp_id
    CHARACTER(LEN=*), INTENT(IN)  :: name
    INTEGER         , INTENT(IN)  :: value
    INTEGER         , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pset_f
!>
!! \ingroup FH5P
!!
!! \brief Queries the value of a property.
!!
!! \attention  \fortran_approved
!!
!! \param prp_id Property list identifier to modify.
!! \param name   Name of property to get.
!! \param value  Pointer to a location to which to copy the value of of the property.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget()
!!
  SUBROUTINE h5pget_f(prp_id, name, value, hdferr)
    INTEGER(HID_T)  , INTENT(IN)  :: prp_id
    CHARACTER(LEN=*), INTENT(IN)  :: name
    TYPE(C_PTR)     , INTENT(IN)  :: value
    INTEGER         , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pget_f

!>
!! \ingroup FH5P
!!
!! \brief Queries the value of a property.
!!
!! \attention  \fortran_obsolete
!!
!! \param prp_id Property list identifier to modify.
!! \param name   Name of property to get.
!! \param value  Property value, supported types are:
!!               \li INTEGER
!!               \li REAL
!!               \li DOUBLE PRECISION
!!               \li CHARACTER(LEN=*)
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget()
!!
  SUBROUTINE h5pget_f(prp_id, name, value, hdferr)
    INTEGER(HID_T)  , INTENT(IN)  :: prp_id
    CHARACTER(LEN=*), INTENT(IN)  :: name
    INTEGER         , INTENT(OUT) :: value
    INTEGER         , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pget_f
!>
!! \ingroup FH5P
!!
!! \brief Registers a permanent property with a property list class.
!!
!! \attention  \fortran_approved
!!
!! \param class Property list class identifier.
!! \param name  Name of property to register.
!! \param size  Size of the property value.
!! \param value Pointer to value to set the property to.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pregister2()
!!
  SUBROUTINE h5pregister_f(class, name, size, value, hdferr)
    INTEGER(HID_T)  , INTENT(IN)  :: class
    CHARACTER(LEN=*), INTENT(IN)  :: name
    INTEGER(SIZE_T) , INTENT(IN)  :: size
    TYPE(C_PTR)     , INTENT(IN)  :: value
    INTEGER         , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pregister_f
!>
!! \ingroup FH5P
!!
!! \brief Registers a permanent property with a property list class.
!!
!! \attention  \fortran_obsolete
!!
!! \param class Property list class identifier.
!! \param name  Name of property to register.
!! \param size  Size of the property value.
!! \param value Property value, supported types are:
!!              \li INTEGER
!!              \li REAL
!!              \li DOUBLE PRECISION
!!              \li CHARACTER(LEN=*)
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pregister2()
!!
  SUBROUTINE h5pregister_f(class, name, size, value, hdferr)
    INTEGER(HID_T)  , INTENT(IN)  :: class
    CHARACTER(LEN=*), INTENT(IN)  :: name
    INTEGER(SIZE_T) , INTENT(IN)  :: size
    TYPE(TYPE)      , INTENT(IN)  :: value
    INTEGER         , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pregister_f

!>
!! \ingroup FH5P
!!
!! \brief Registers a temporary property with a property list class.
!!
!! \attention  \fortran_approved
!!
!! \param plist  Property list class identifier.
!! \param name   Name of property to insert.
!! \param size   Size of the property value.
!! \param value  Pointer to new value pointer for the property being modified.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pinsert2()
!!
  SUBROUTINE h5pinsert_f(plist, name, size, value, hdferr)
    INTEGER(HID_T)  , INTENT(IN)  :: plist
    CHARACTER(LEN=*), INTENT(IN)  :: name
    INTEGER(SIZE_T) , INTENT(IN)  :: size
    TYPE(C_PTR)     , INTENT(IN)  :: value
    INTEGER         , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pinsert_f

!>
!! \ingroup FH5P
!!
!! \brief Registers a temporary property with a property list class.
!!
!! \attention  \fortran_obsolete
!!
!! \param plist  Property list class identifier.
!! \param name   Name of property to insert.
!! \param size   Size of the property value.
!! \param value  Property value, supported types are:
!!               \li INTEGER
!!               \li REAL
!!               \li DOUBLE PRECISION
!!               \li CHARACTER(LEN=*)
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pinsert2()
!!
  SUBROUTINE h5pinsert_f(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: plist
    CHARACTER(LEN=*), INTENT(IN)  :: name
    INTEGER(SIZE_T) , INTENT(IN)  :: size
    TYPE(TYPE)      , INTENT(IN)  :: value
    INTEGER         , INTENT(OUT) :: hdferr
  END SUBROUTINE h5pinsert_f

#else

  SUBROUTINE h5pset_fill_value_integer(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN), TARGET :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr

    TYPE(C_PTR) :: f_ptr ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = INT(H5Pset_fill_value(prp_id, type_id, f_ptr))

  END SUBROUTINE h5pset_fill_value_integer

  SUBROUTINE h5pget_fill_value_integer(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT), TARGET :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr                      ! C address

    f_ptr = C_LOC(fillvalue)

    hdferr = INT(H5Pget_fill_value(prp_id, type_id, f_ptr))

  END SUBROUTINE h5pget_fill_value_integer

  SUBROUTINE h5pset_fill_value_char(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    CHARACTER(LEN=1), INTENT(IN), TARGET :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr                       ! C address

    f_ptr = C_LOC(fillvalue(1:1))
    hdferr = INT(H5Pset_fill_value(prp_id, type_id, f_ptr))

  END SUBROUTINE h5pset_fill_value_char

  SUBROUTINE h5pget_fill_value_char(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    CHARACTER(LEN=*), INTENT(OUT) :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER :: i
    CHARACTER(LEN=1), ALLOCATABLE, DIMENSION(:), TARGET :: chr
    INTEGER :: chr_len

    TYPE(C_PTR) :: f_ptr ! C address
    ! To resolve Issue #1 outlined in the preamble of this file we
    ! need to pack the character string into an array.

    chr_len = LEN(fillvalue(1:1))
    ALLOCATE(chr(1:chr_len), STAT=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF

    f_ptr = C_LOC(chr(1)(1:1))

    hdferr = INT(H5Pget_fill_value(prp_id, type_id, f_ptr))

    DO i = 1, chr_len
       fillvalue(i:i) = chr(i)
    ENDDO
    DEALLOCATE(chr)

  END SUBROUTINE h5pget_fill_value_char

  SUBROUTINE h5pset_fill_value_ptr(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    TYPE(C_PTR)                :: fillvalue
    INTEGER, INTENT(OUT)       :: hdferr

    hdferr = INT(H5Pset_fill_value(prp_id, type_id, fillvalue))

  END SUBROUTINE h5pset_fill_value_ptr

  SUBROUTINE h5pget_fill_value_ptr(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    INTEGER(HID_T), INTENT(IN)  :: type_id
    TYPE(C_PTR)                 :: fillvalue
    INTEGER       , INTENT(OUT) :: hdferr

    hdferr = INT(H5Pget_fill_value(prp_id, type_id, fillvalue))

  END SUBROUTINE h5pget_fill_value_ptr

  SUBROUTINE h5pset_integer(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER,   INTENT(IN), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pset_integer

  SUBROUTINE h5pset_char(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=*),   INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
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

! \brief Queries the value of a property.
!
! \param prp_id Property list identifier to modify.
! \param name   Name of property to get.
! \param value  Property value, supported types are:
!               \li INTEGER
!               \li REAL
!               \li DOUBLE PRECISION
!               \li CHARACTER(LEN=*)
! \param hdferr \fortran_error
!
  SUBROUTINE h5pget_integer(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER,   INTENT(OUT), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)

  END SUBROUTINE h5pget_integer

  SUBROUTINE h5pget_char(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=*), INTENT(OUT) :: value
    INTEGER, INTENT(OUT) :: hdferr
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

! \brief Sets a property list value
!
! \param prp_id Property list identifier to modify.
! \param name   Name of property to modify.
! \param value  Pointer to value to set the property to.
! \param hdferr \fortran_error
!
  SUBROUTINE h5pset_ptr(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(C_PTR), INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pset_c(prp_id, name, name_len, value)

  END SUBROUTINE h5pset_ptr

! \brief Queries the value of a property.
!
! \param prp_id Property list identifier to modify.
! \param name   Name of property to get.
! \param value  Pointer to a location to which to copy the value of of the property.
! \param hdferr \fortran_error
!
  SUBROUTINE h5pget_ptr(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(C_PTR), INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, value)

  END SUBROUTINE h5pget_ptr

  SUBROUTINE h5pregister_integer(class, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    INTEGER,   INTENT(IN), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(value)

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, f_ptr)

  END SUBROUTINE h5pregister_integer

  SUBROUTINE h5pregister_char(class, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    CHARACTER(LEN=*),   INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
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

  SUBROUTINE h5pregister_ptr(class, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    TYPE(C_PTR), INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pregister_c(class, name, name_len, size, value)
  END SUBROUTINE h5pregister_ptr

  SUBROUTINE h5pinsert_integer(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    INTEGER,   INTENT(IN), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr

    f_ptr = c_loc(value)

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)
  END SUBROUTINE h5pinsert_integer

  SUBROUTINE h5pinsert_char(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    CHARACTER(LEN=*),   INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
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

  SUBROUTINE h5pinsert_ptr(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    TYPE(c_ptr),   INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len

    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, value)
  END SUBROUTINE h5pinsert_ptr

#endif

!>
!! \ingroup FH5P
!!
!! \brief Create a new property list class
!!
!! \param parent     Parent property list class identifier. Possible values include:
!!                   \li H5P_ROOT_F
!!                   \li H5P_FILE_CREATE_F
!!                   \li H5P_FILE_ACCESS_F
!!                   \li H5P_DATASET_CREATE_F
!!                   \li H5P_DATASET_XFER_F
!!                   \li H5P_FILE_MOUNT_F
!! \param name        Name of property to create.
!! \param class       Property list class identifier.
!! \param hdferr      \fortran_error
!! \param create      (H5P_cls_create_func_t) - Callback routine called when a property list is created.
!! \param create_data User pointer to any class creation information needed.
!! \param copy        (H5P_cls_copy_func_t)   - Callback routine called when a property list is copied.
!! \param copy_data   User pointer to any class copy information needed.
!! \param close       (H5P_cls_close_func_t)  - Callback routine called when a property list is being closed.
!! \param close_data  User pointer to any class close information needed.
!!
!! See C API: @ref H5Pcreate_class()
!!
  SUBROUTINE h5pcreate_class_f(parent, name, class, hdferr, create, create_data, &
       copy, copy_data, close, close_data)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: parent
    CHARACTER(LEN=*), INTENT(IN)  :: name
    INTEGER(HID_T)  , INTENT(OUT) :: class
    INTEGER         , INTENT(OUT) :: hdferr
    TYPE(C_PTR)     , OPTIONAL, INTENT(IN) :: create_data, copy_data, close_data
    TYPE(C_FUNPTR)  , OPTIONAL, INTENT(IN) :: create, copy, close
    TYPE(C_PTR)    :: create_data_default, copy_data_default, close_data_default
    TYPE(C_FUNPTR) :: create_default, copy_default, close_default

    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Pcreate_class(parent, name, &
            create, create_data, copy, copy_data, close, close_data) &
            BIND(C, NAME='H5Pcreate_class')
         IMPORT :: C_CHAR, C_PTR, C_FUNPTR
         IMPORT :: HID_T
         INTEGER(HID_T), VALUE :: parent
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         TYPE(C_PTR), VALUE    :: create_data, copy_data, close_data
         TYPE(C_FUNPTR), VALUE  :: create, copy, close
       END FUNCTION H5Pcreate_class
    END INTERFACE

    c_name = TRIM(name)//C_NULL_CHAR

    create_default = C_NULL_FUNPTR
    create_data_default = C_NULL_PTR
    copy_default = C_NULL_FUNPTR
    copy_data_default = C_NULL_PTR
    close_default = C_NULL_FUNPTR
    close_data_default = C_NULL_PTR

    IF(PRESENT(create)) create_default = create
    IF(PRESENT(create_data)) create_data_default = create_data
    IF(PRESENT(copy)) copy_default = copy
    IF(PRESENT(copy_data)) copy_data_default = copy_data
    IF(PRESENT(close)) close_default = close
    IF(PRESENT(close_data)) close_data_default = close_data

    class = H5Pcreate_class(parent, c_name, &
         create_default, create_data_default, &
         copy_default, copy_data_default, &
         close_default, close_data_default)

    hdferr = 0
    IF(class.LT.0) hdferr = -1

  END SUBROUTINE h5pcreate_class_f

!>
!! \ingroup FH5P
!!
!! \brief Sets an initial file image in a memory buffer.
!!
!! \param fapl_id File access property list identifier.
!! \param buf_ptr Pointer to the initial file image, or C_NULL_PTR if no initial file image is desired.
!! \param buf_len Size of the supplied buffer, or 0 (zero) if no initial image is desired.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pset_file_image()
!!
  SUBROUTINE h5pset_file_image_f(fapl_id, buf_ptr, buf_len, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)  :: fapl_id
    TYPE(C_PTR)    , INTENT(IN)  :: buf_ptr
    INTEGER(SIZE_T), INTENT(IN)  :: buf_len
    INTEGER        , INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_file_image_c(fapl_id, buf_ptr, buf_len) &
            BIND(C, NAME='h5pset_file_image_c')
         IMPORT :: c_ptr
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), INTENT(IN) :: fapl_id
         TYPE(C_PTR), VALUE :: buf_ptr
         INTEGER(SIZE_T), INTENT(IN)  :: buf_len
       END FUNCTION h5pset_file_image_c
    END INTERFACE

    hdferr = h5pset_file_image_c(fapl_id, buf_ptr, buf_len)

  END SUBROUTINE h5pset_file_image_f
!>
!! \ingroup FH5P
!!
!! \brief Retrieves a copy of the file image designated as the initial content and structure of a file.
!!
!! \param fapl_id     File access property list identifier.
!! \param buf_ptr     Will hold either a C_NULL_PTR or a scalar of type c_loc. If buf_ptr is not C_NULL_PTR, on successful
!!                    return, buf_ptr shall contain a C pointer to a copy of the initial image provided in the last call to
!!                    H5Pset_file_image_f for the supplied fapl_id, or buf_ptr shall contain a C_NULL_PTR if there is no
!!                    initial image set.
!! \param buf_len_ptr Contains the value of the buffer parameter for the initial image in the supplied fapl_id. The value
!!                    will be 0 if no initial image is set.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Pget_file_image()
!!
  SUBROUTINE h5pget_file_image_f(fapl_id, buf_ptr, buf_len_ptr, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)               :: fapl_id
    TYPE(C_PTR)    , INTENT(IN), DIMENSION(*) :: buf_ptr
    INTEGER(SIZE_T), INTENT(OUT)              :: buf_len_ptr
    INTEGER        , INTENT(OUT)              :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pget_file_image_c(fapl_id, buf_ptr, buf_len_ptr) &
            BIND(C, NAME='h5pget_file_image_c')
         IMPORT :: c_ptr
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T) :: fapl_id
         TYPE(C_PTR), DIMENSION(*) :: buf_ptr
         INTEGER(SIZE_T) :: buf_len_ptr
       END FUNCTION h5pget_file_image_c
    END INTERFACE

    hdferr = h5pget_file_image_c(fapl_id, buf_ptr, buf_len_ptr)

  END SUBROUTINE h5pget_file_image_f

! *********************************************************************
! Fortran interfaces for H5P functions needed by parallel MPI programs.
! *********************************************************************

#ifdef H5_HAVE_PARALLEL

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5P
!!
!! \brief Stores MPI IO communicator information to the file access property list.
!!
!! \param prp_id File access property list identifier.
!! \param comm   MPI-2 communicator.
!! \param info   MPI-2 info object.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_fapl_mpio()
!!
  SUBROUTINE h5pset_fapl_mpio_f(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(IN) :: comm
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(IN) :: info
    INTEGER, INTENT(OUT) :: hdferr
  END SUBROUTINE h5pset_fapl_mpio_f
!>
!! \ingroup FH5P
!!
!! \brief Stores MPI IO communicator information to the file access property list.
!!
!! \note Supports MPI Fortran module mpi_f08
!!
!! \param prp_id File access property list identifier.
!! \param comm   MPI-3 communicator.
!! \param info   MPI-3 info object.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_fapl_mpio()
!!
  SUBROUTINE h5pset_fapl_mpio_f(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    TYPE(MPI_COMM), INTENT(IN) :: comm
    TYPE(MPI_INFO), INTENT(IN) :: info
    INTEGER, INTENT(OUT) :: hdferr
  END SUBROUTINE h5pset_fapl_mpio_f

#else

  SUBROUTINE h5pset_fapl_mpio_f90(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(IN) :: comm
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(IN) :: info
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_mpio_c(prp_id, comm, info) &
            BIND(C,NAME='h5pset_fapl_mpio_c')
         IMPORT :: HID_T, MPI_INTEGER_KIND
         IMPLICIT NONE
         INTEGER(HID_T) :: prp_id
         INTEGER(KIND=MPI_INTEGER_KIND) :: comm
         INTEGER(KIND=MPI_INTEGER_KIND) :: info
       END FUNCTION h5pset_fapl_mpio_c
    END INTERFACE

    hdferr = h5pset_fapl_mpio_c(prp_id, comm, info)

  END SUBROUTINE h5pset_fapl_mpio_f90

#ifdef H5_HAVE_MPI_F08
  SUBROUTINE h5pset_fapl_mpio_f08(prp_id, comm, info, hdferr)
    USE mpi_f08
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    TYPE(MPI_COMM), INTENT(IN) :: comm
    TYPE(MPI_INFO), INTENT(IN) :: info
    INTEGER, INTENT(OUT) :: hdferr

    CALL h5pset_fapl_mpio_f90(prp_id, INT(comm%mpi_val,MPI_INTEGER_KIND), INT(info%mpi_val,MPI_INTEGER_KIND), hdferr)

  END SUBROUTINE h5pset_fapl_mpio_f08
#endif

#endif

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5P
!!
!! \brief Returns MPI communicator information.
!!
!! \param prp_id File access property list identifier.
!! \param comm   MPI-2 communicator.
!! \param info   MPI-2 info object.
!! \param hdferr \fortran_error
!!
!! \attention It is the responsibility of the application to free the MPI objects.
!!
!! See C API: @ref H5Pget_fapl_mpio()
!!
SUBROUTINE h5pget_fapl_mpio_f(prp_id, comm, info, hdferr)
  IMPLICIT NONE
  INTEGER(HID_T), INTENT(IN) :: prp_id
  INTEGER, INTENT(OUT) :: comm
  INTEGER, INTENT(OUT) :: info
  INTEGER, INTENT(OUT) :: hdferr
END SUBROUTINE h5pget_fapl_mpio_f
!>
!! \ingroup FH5P
!!
!! \brief Returns MPI communicator information.
!!
!! \note Supports MPI Fortran module mpi_f08
!!
!! \param prp_id File access property list identifier.
!! \param comm   MPI-3 communicator.
!! \param info   MPI-3 info object.
!! \param hdferr \fortran_error
!!
!! \attention It is the responsibility of the application to free the MPI objects.
!!
!! See C API: @ref H5Pget_fapl_mpio()
!!
SUBROUTINE h5pget_fapl_mpio_f(prp_id, comm, info, hdferr)
  IMPLICIT NONE
  INTEGER(HID_T), INTENT(IN)  :: prp_id
  TYPE(MPI_COMM), INTENT(OUT) :: comm
  TYPE(MPI_INFO), INTENT(OUT) :: info
  INTEGER       , INTENT(OUT) :: hdferr
END SUBROUTINE h5pget_fapl_mpio_f

#else

  SUBROUTINE h5pget_fapl_mpio_f90(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(OUT) :: comm
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(OUT) :: info
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(KIND=C_INT) :: c_comm
    INTEGER(KIND=C_INT) :: c_info

    INTERFACE
       INTEGER FUNCTION h5pget_fapl_mpio_c(prp_id, comm, info) &
            BIND(C,NAME='h5pget_fapl_mpio_c')
         IMPORT :: HID_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T) :: prp_id
         INTEGER(KIND=C_INT)       :: comm
         INTEGER(KIND=C_INT)       :: info
       END FUNCTION h5pget_fapl_mpio_c
    END INTERFACE

    hdferr = h5pget_fapl_mpio_c(prp_id, c_comm, c_info)

    comm = INT(c_comm,KIND=MPI_INTEGER_KIND)
    info = INT(c_info,KIND=MPI_INTEGER_KIND)

  END SUBROUTINE h5pget_fapl_mpio_f90

#ifdef H5_HAVE_MPI_F08
  SUBROUTINE h5pget_fapl_mpio_f08(prp_id, comm, info, hdferr)
    USE mpi_f08
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    TYPE(MPI_COMM), INTENT(OUT) :: comm
    TYPE(MPI_INFO), INTENT(OUT) :: info
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(KIND=MPI_INTEGER_KIND) :: tmp_comm
    INTEGER(KIND=MPI_INTEGER_KIND) :: tmp_info

    CALL h5pget_fapl_mpio_f90(prp_id, tmp_comm, tmp_info, hdferr)

    comm%mpi_val = tmp_comm
    info%mpi_val = tmp_info

  END SUBROUTINE h5pget_fapl_mpio_f08
#endif
#endif

#ifdef H5_HAVE_SUBFILING_VFD
!>
!! \ingroup FH5P
!!
!! \brief Modifies the specified File Access Property List to use the #H5FD_SUBFILING driver.
!!
!! \param prp_id     File access property list identifier.
!! \param hdferr     \fortran_error
!! \param vfd_config #H5FD_SUBFILING driver configuration derived type.
!!
!! See C API: @ref H5Pset_fapl_subfiling()
!!
 SUBROUTINE h5pset_fapl_subfiling_f(prp_id, hdferr, vfd_config)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(H5FD_subfiling_config_t), OPTIONAL, TARGET :: vfd_config
    TYPE(C_PTR) :: f_ptr

    INTERFACE
       INTEGER FUNCTION H5Pset_fapl_subfiling(prp_id, vfd_config) &
            BIND(C,NAME='H5Pset_fapl_subfiling')
         IMPORT :: HID_T, C_PTR
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: prp_id
         TYPE(C_PTR)   , VALUE :: vfd_config
       END FUNCTION h5pset_fapl_subfiling
    END INTERFACE

    IF(PRESENT(vfd_config))THEN
       f_ptr = C_LOC(vfd_config)
    ELSE
       f_ptr = C_NULL_PTR
    ENDIF

    hdferr = h5pset_fapl_subfiling(prp_id, f_ptr)

  END SUBROUTINE h5pset_fapl_subfiling_f

!>
!! \ingroup FH5P
!!
!! \brief Queries a File Access Property List for #H5FD_SUBFILING file driver properties.
!!
!! \param prp_id     File access property list identifier.
!! \param vfd_config #H5FD_SUBFILING driver configuration derived type.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Pget_fapl_subfiling()
!!
  SUBROUTINE h5pget_fapl_subfiling_f(prp_id, vfd_config, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    TYPE(H5FD_subfiling_config_t), TARGET :: vfd_config
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr

    INTERFACE
       INTEGER FUNCTION H5Pget_fapl_subfiling(prp_id, vfd_config) &
            BIND(C,NAME='H5Pget_fapl_subfiling')
         IMPORT :: HID_T, C_PTR
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: prp_id
         TYPE(C_PTR)   , VALUE :: vfd_config
       END FUNCTION H5Pget_fapl_subfiling
    END INTERFACE

    f_ptr = C_LOC(vfd_config)
    hdferr = h5pget_fapl_subfiling(prp_id, f_ptr)

  END SUBROUTINE h5pget_fapl_subfiling_f

!>
!! \ingroup FH5P
!!
!! \brief Modifies the specified File Access Property List to use the #H5FD_IOC driver.
!!
!! \param prp_id     File access property list identifier.
!! \param hdferr     \fortran_error
!! \param vfd_config #H5FD_IOC driver configuration derived type.
!!
!! See C API: @ref H5Pset_fapl_ioc()
!!
 SUBROUTINE h5pset_fapl_ioc_f(prp_id, hdferr, vfd_config)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    TYPE(H5FD_ioc_config_t), OPTIONAL, TARGET :: vfd_config
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr

    INTERFACE
       INTEGER FUNCTION H5Pset_fapl_ioc(prp_id, vfd_config) &
            BIND(C,NAME='H5Pset_fapl_ioc')
         IMPORT :: HID_T, C_PTR
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: prp_id
         TYPE(C_PTR)   , VALUE :: vfd_config
       END FUNCTION h5pset_fapl_ioc
    END INTERFACE

    IF(PRESENT(vfd_config))THEN
       f_ptr = C_LOC(vfd_config)
    ELSE
       f_ptr = C_NULL_PTR
    ENDIF

    hdferr = h5pset_fapl_ioc(prp_id, f_ptr)

  END SUBROUTINE h5pset_fapl_ioc_f

!>
!! \ingroup FH5P
!!
!! \brief Queries a File Access Property List for #H5FD_IOC file driver properties.
!!
!! \param prp_id     File access property list identifier.
!! \param vfd_config #H5FD_IOC driver configuration derived type.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Pget_fapl_ioc()
!!
  SUBROUTINE h5pget_fapl_ioc_f(prp_id, vfd_config, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    TYPE(H5FD_ioc_config_t), TARGET :: vfd_config
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr

    INTERFACE
       INTEGER FUNCTION H5Pget_fapl_ioc(prp_id, vfd_config) &
            BIND(C,NAME='H5Pget_fapl_ioc')
         IMPORT :: HID_T, C_PTR
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: prp_id
         TYPE(C_PTR)   , VALUE :: vfd_config
       END FUNCTION H5Pget_fapl_ioc
    END INTERFACE

    f_ptr = C_LOC(vfd_config)
    hdferr = h5pget_fapl_ioc(prp_id, f_ptr)

  END SUBROUTINE h5pget_fapl_ioc_f

#endif

!>
!! \ingroup FH5P
!!
!! \brief Retrieves local and global causes that broke collective I/O on the last parallel I/O call.
!!
!! \param plist_id                   Dataset transfer property list identifier
!! \param local_no_collective_cause  An enumerated set value indicating the causes that prevented collective I/O in the local process
!! \param global_no_collective_cause An enumerated set value indicating the causes across all processes that prevented collective I/O
!! \param hdferr                     \fortran_error
!!
!! See C API: @ref H5Pget_mpio_no_collective_cause()
!!
   SUBROUTINE h5pget_mpio_no_collective_cause_f(plist_id, local_no_collective_cause, global_no_collective_cause, hdferr)
     IMPLICIT NONE
     INTEGER(HID_T)    , INTENT(IN)  :: plist_id
     INTEGER, INTENT(OUT) :: local_no_collective_cause
     INTEGER, INTENT(OUT) :: global_no_collective_cause
     INTEGER           , INTENT(OUT) :: hdferr

     INTEGER(C_INT32_T) :: c_local_no_collective_cause
     INTEGER(C_INT32_T) :: c_global_no_collective_cause

     INTERFACE
        INTEGER(C_INT) FUNCTION H5Pget_mpio_no_collective_cause(plist_id, local_no_collective_cause, global_no_collective_cause) &
             BIND(C, NAME='H5Pget_mpio_no_collective_cause')
          IMPORT :: HID_T, C_INT, C_INT32_T
          IMPLICIT NONE
          INTEGER(HID_T)    , VALUE :: plist_id
          INTEGER(C_INT32_T) :: local_no_collective_cause
          INTEGER(C_INT32_T) :: global_no_collective_cause
        END FUNCTION H5Pget_mpio_no_collective_cause
     END INTERFACE

     hdferr = INT(H5Pget_mpio_no_collective_cause(plist_id, c_local_no_collective_cause, c_global_no_collective_cause))

     local_no_collective_cause = INT(c_local_no_collective_cause)
     global_no_collective_cause = INT(c_global_no_collective_cause)

   END SUBROUTINE h5pget_mpio_no_collective_cause_f

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5P
!!
!! \brief Set the MPI communicator and information.
!!
!! \param prp_id File access property list identifier.
!! \param comm   MPI-2 communicator.
!! \param info   MPI-2 info object.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_mpi_params()
!!
  SUBROUTINE H5Pset_mpi_params_f(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(IN)  :: comm
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(IN)  :: info
    INTEGER       , INTENT(OUT) :: hdferr
  END SUBROUTINE H5Pset_mpi_params_f
!>
!! \ingroup FH5P
!!
!! \brief Set the MPI communicator and information.
!!
!! \note Supports MPI Fortran module mpi_f08
!!
!! \param prp_id File access property list identifier.
!! \param comm   MPI-3 communicator.
!! \param info   MPI-3 info object.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pset_mpi_params()
!!
  SUBROUTINE H5Pset_mpi_params_f(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    TYPE(MPI_COMM), INTENT(IN)  :: comm
    TYPE(MPI_INFO), INTENT(IN)  :: info
    INTEGER       , INTENT(OUT) :: hdferr
  END SUBROUTINE H5Pset_mpi_params_f

#else

  SUBROUTINE H5Pset_mpi_params_f90(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(IN)  :: comm
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(IN)  :: info
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pset_mpi_params_c(prp_id, comm, info) &
            BIND(C,NAME='h5pset_mpi_params_c')
         IMPORT :: HID_T, MPI_INTEGER_KIND
         IMPLICIT NONE
         INTEGER(HID_T) :: prp_id
         INTEGER(KIND=MPI_INTEGER_KIND) :: comm
         INTEGER(KIND=MPI_INTEGER_KIND) :: info
       END FUNCTION H5pset_mpi_params_c
    END INTERFACE

    hdferr = H5Pset_mpi_params_c(prp_id, comm, info)

  END SUBROUTINE H5Pset_mpi_params_f90

#ifdef H5_HAVE_MPI_F08
  SUBROUTINE H5Pset_mpi_params_f08(prp_id, comm, info, hdferr)
    USE mpi_f08
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    TYPE(MPI_COMM), INTENT(IN)  :: comm
    TYPE(MPI_INFO), INTENT(IN)  :: info
    INTEGER       , INTENT(OUT) :: hdferr

    CALL H5Pset_mpi_params_f90(prp_id, comm%mpi_val, info%mpi_val, hdferr)

  END SUBROUTINE H5Pset_mpi_params_f08
#endif

#endif

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5P
!!
!! \brief Get the MPI communicator and info.
!!
!! \param prp_id File access property list identifier.
!! \param comm   MPI-2 communicator.
!! \param info   MPI-2 info object.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Pget_mpi_params()
!!
  SUBROUTINE H5Pget_mpi_params_f(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    INTEGER, INTENT(OUT) :: comm
    INTEGER, INTENT(OUT) :: info
    INTEGER       , INTENT(OUT) :: hdferr
  END SUBROUTINE H5Pget_mpi_params_f
!>
!! \ingroup FH5P
!!
!! \brief Get the MPI communicator and information.
!!
!! \note Supports MPI Fortran module mpi_f08
!!
!! \param prp_id File access property list identifier.
!! \param comm   MPI-3 communicator.
!! \param info   MPI-3 info object.
!! \param hdferr \fortran_error
!!
!! \attention It is the responsibility of the application to free the MPI objects.
!!
!! See C API: @ref H5Pget_mpi_params()
!!
  SUBROUTINE H5Pget_mpi_params_f(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    TYPE(MPI_COMM), INTENT(OUT) :: comm
    TYPE(MPI_INFO), INTENT(OUT) :: info
    INTEGER       , INTENT(OUT) :: hdferr
  END SUBROUTINE H5Pget_mpi_params_f

#else

  SUBROUTINE H5Pget_mpi_params_f90(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(OUT) :: comm
    INTEGER(KIND=MPI_INTEGER_KIND), INTENT(OUT) :: info
    INTEGER       , INTENT(OUT) :: hdferr

    INTEGER(KIND=C_INT) :: c_comm
    INTEGER(KIND=C_INT) :: c_info

    INTERFACE
       INTEGER FUNCTION h5pget_mpi_params_c(prp_id, comm, info) &
            BIND(C,NAME='h5pget_mpi_params_c')
         IMPORT :: HID_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T) :: prp_id
         INTEGER(KIND=C_INT) :: comm
         INTEGER(KIND=C_INT) :: info
       END FUNCTION H5pget_mpi_params_c
    END INTERFACE

    hdferr = H5Pget_mpi_params_c(prp_id, c_comm, c_info)

    comm = INT(c_comm,KIND=MPI_INTEGER_KIND)
    info = INT(c_info,KIND=MPI_INTEGER_KIND)

  END SUBROUTINE H5Pget_mpi_params_f90

#ifdef H5_HAVE_MPI_F08
  SUBROUTINE H5Pget_mpi_params_f08(prp_id, comm, info, hdferr)
    USE mpi_f08
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: prp_id
    TYPE(MPI_COMM), INTENT(OUT) :: comm
    TYPE(MPI_INFO), INTENT(OUT) :: info
    INTEGER       , INTENT(OUT) :: hdferr

    INTEGER(KIND=MPI_INTEGER_KIND) :: tmp_comm
    INTEGER(KIND=MPI_INTEGER_KIND) :: tmp_info

    CALL H5Pget_mpi_params_f90(prp_id, tmp_comm, tmp_info, hdferr)

    comm%mpi_val = tmp_comm
    info%mpi_val = tmp_info

  END SUBROUTINE H5Pget_mpi_params_f08
#endif

#endif

!>
!! \ingroup FH5P
!!
!! \brief Sets data transfer mode.
!!
!! \param prp_id         Data transfer property list identifier.
!! \param data_xfer_mode Transfer mode; possible values are:
!!                       \li H5FD_MPIO_INDEPENDENT_F
!!                       \li H5FD_MPIO_COLLECTIVE_F
!! \param hdferr         \fortran_error
!!
!! See C API: @ref H5Pset_dxpl_mpio()
!!
  SUBROUTINE h5pset_dxpl_mpio_f(prp_id, data_xfer_mode, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(IN) :: data_xfer_mode
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_dxpl_mpio_c(prp_id, data_xfer_mode) &
            BIND(C,NAME='h5pset_dxpl_mpio_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN)  :: prp_id
         INTEGER       , INTENT(IN) :: data_xfer_mode
       END FUNCTION h5pset_dxpl_mpio_c
    END INTERFACE

    hdferr = h5pset_dxpl_mpio_c(prp_id, data_xfer_mode)
  END SUBROUTINE h5pset_dxpl_mpio_f

!>
!! \ingroup FH5P
!!
!! \brief Returns the data transfer mode.
!!
!! \param prp_id         Data transfer property list identifier.
!! \param data_xfer_mode Transfer mode; possible values are:
!!                       \li H5FD_MPIO_INDEPENDENT_F
!!                       \li H5FD_MPIO_COLLECTIVE_F
!! \param hdferr         \fortran_error
!!
!! See C API: @ref H5Pget_dxpl_mpio()
!!
  SUBROUTINE h5pget_dxpl_mpio_f(prp_id, data_xfer_mode, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER, INTENT(OUT) :: data_xfer_mode
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_dxpl_mpio_c(prp_id, data_xfer_mode) &
            BIND(C,NAME='h5pget_dxpl_mpio_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN)  :: prp_id
         INTEGER       , INTENT(OUT) :: data_xfer_mode
       END FUNCTION h5pget_dxpl_mpio_c
    END INTERFACE

    hdferr = h5pget_dxpl_mpio_c(prp_id, data_xfer_mode)
  END SUBROUTINE h5pget_dxpl_mpio_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the type of I/O that HDF5 actually performed on the last
!!       parallel I/O call. This is not necessarily the type of I/O requested.
!!
!! \param dxpl_id        Dataset transfer property list identifier.
!! \param actual_io_mode The type of I/O performed by this process.
!! \param hdferr         \fortran_error
!!
!! See C API: @ref H5Pget_mpio_actual_io_mode()
!!
  SUBROUTINE h5pget_mpio_actual_io_mode_f(dxpl_id, actual_io_mode, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: dxpl_id
    INTEGER       , INTENT(OUT) :: actual_io_mode
    INTEGER       , INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_mpio_actual_io_mode_c(dxpl_id, actual_io_mode) &
            BIND(C,NAME='h5pget_mpio_actual_io_mode_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN)  :: dxpl_id
         INTEGER       , INTENT(OUT) :: actual_io_mode
       END FUNCTION h5pget_mpio_actual_io_mode_c
    END INTERFACE

    actual_io_mode = -1

    hdferr = h5pget_mpio_actual_io_mode_c(dxpl_id, actual_io_mode)

  END SUBROUTINE h5pget_mpio_actual_io_mode_f

!>
!! \ingroup FH5P
!!
!! \brief Sets requirement whether HDF5 metadata read operations using the access property
!!        list are required to be collective or independent. If collective requirement is
!!        selected, the HDF5 library will optimize the metadata reads improving performance.
!!        The default setting is independent (false).
!!
!! \param plist_id      File access property list identifier.
!! \param is_collective Indicates if metadata writes are collective or not.
!! \param hdferr        \fortran_error
!!
!! See C API: @ref H5Pset_all_coll_metadata_ops()
!!
  SUBROUTINE h5pset_all_coll_metadata_ops_f(plist_id, is_collective, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: plist_id
    LOGICAL, INTENT(IN)           :: is_collective
    INTEGER, INTENT(OUT)          :: hdferr
    LOGICAL(C_BOOL) :: c_is_collective

    INTERFACE
       INTEGER FUNCTION h5pset_all_coll_metadata_ops(plist_id, is_collective) BIND(C, NAME='H5Pset_all_coll_metadata_ops')
         IMPORT :: HID_T, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T) , INTENT(IN), VALUE :: plist_id
         LOGICAL(C_BOOL), INTENT(IN), VALUE :: is_collective
       END FUNCTION h5pset_all_coll_metadata_ops
    END INTERFACE

    ! Transfer value of Fortran LOGICAL to C c_bool type
    c_is_collective = is_collective

    hdferr = INT(H5Pset_all_coll_metadata_ops(plist_id, c_is_collective))

  END SUBROUTINE h5pset_all_coll_metadata_ops_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves metadata read mode from the access property list.
!!
!! \param plist_id      File access property list identifier.
!! \param is_collective Collective access setting.
!! \param hdferr        \fortran_error
!!
!! See C API: @ref H5Pget_all_coll_metadata_ops()
!!
  SUBROUTINE h5pget_all_coll_metadata_ops_f(plist_id, is_collective, hdferr)

    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: plist_id
    LOGICAL, INTENT(OUT)          :: is_collective
    INTEGER, INTENT(OUT)          :: hdferr
    LOGICAL(C_BOOL) :: c_is_collective

    INTERFACE
       INTEGER FUNCTION h5pget_all_coll_metadata_ops(plist_id, is_collective) BIND(C, NAME='H5Pget_all_coll_metadata_ops')
         IMPORT :: HID_T, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T) , INTENT(IN), VALUE :: plist_id
         LOGICAL(C_BOOL), INTENT(OUT) :: is_collective
       END FUNCTION h5pget_all_coll_metadata_ops
    END INTERFACE

    hdferr = INT(H5Pget_all_coll_metadata_ops(plist_id, c_is_collective))

    ! Transfer value of C c_bool type to Fortran LOGICAL
    is_collective = c_is_collective

  END SUBROUTINE h5pget_all_coll_metadata_ops_f

!>
!! \ingroup FH5P
!!
!! \brief Sets metadata writes to collective or independent. Default setting is independent (false).
!!
!! \param plist_id      File access property list identifier.
!! \param is_collective Indicates if metadata writes are collective or not.
!! \param hdferr        \fortran_error
!!
!! See C API: @ref H5Pset_coll_metadata_write()
!!
  SUBROUTINE h5pset_coll_metadata_write_f(plist_id, is_collective, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: plist_id
    LOGICAL, INTENT(IN)           :: is_collective
    INTEGER, INTENT(OUT)          :: hdferr
    LOGICAL(C_BOOL) :: c_is_collective

    INTERFACE
       INTEGER FUNCTION h5pset_coll_metadata_write(plist_id, is_collective) BIND(C, NAME='H5Pset_coll_metadata_write')
         IMPORT :: HID_T, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T) , INTENT(IN), VALUE :: plist_id
         LOGICAL(C_BOOL), INTENT(IN), VALUE :: is_collective
       END FUNCTION h5pset_coll_metadata_write
    END INTERFACE

    ! Transfer value of Fortran LOGICAL to C c_bool type
    c_is_collective = is_collective

    hdferr = INT(H5Pset_coll_metadata_write(plist_id, c_is_collective))

  END SUBROUTINE h5pset_coll_metadata_write_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves metadata write mode from the file access property list.
!!
!! \param plist_id      File access property list identifier.
!! \param is_collective Collective access setting.
!! \param hdferr        \fortran_error
!!
!! See C API: @ref H5Pget_coll_metadata_write()
!!
  SUBROUTINE h5pget_coll_metadata_write_f(plist_id, is_collective, hdferr)

    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: plist_id
    LOGICAL, INTENT(OUT)          :: is_collective
    INTEGER, INTENT(OUT)          :: hdferr
    LOGICAL(C_BOOL) :: c_is_collective

    INTERFACE
       INTEGER FUNCTION h5pget_coll_metadata_write(plist_id, is_collective) BIND(C, NAME='H5Pget_coll_metadata_write')
         IMPORT :: HID_T, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T) , INTENT(IN), VALUE :: plist_id
         LOGICAL(C_BOOL), INTENT(OUT) :: is_collective
       END FUNCTION h5pget_coll_metadata_write
    END INTERFACE

    hdferr = INT(H5Pget_coll_metadata_write(plist_id, c_is_collective))

    ! Transfer value of C c_bool type to Fortran LOGICAL
    is_collective = c_is_collective

  END SUBROUTINE h5pget_coll_metadata_write_f

#endif

!
! V I R T U A L  D A T S E T S
!

!>
!! \ingroup FH5P
!!
!! \brief Sets the view of the virtual dataset (VDS) to include or exclude missing mapped elements.
!!
!! \param dapl_id Identifier Of the virtual dataset access property list.
!! \param view    Flag specifying the extent of the data to be included in the view. Valid values are:
!!                \li H5D_VDS_FIRST_MISSING_F
!!                \li H5D_VDS_LAST_AVAILABLE_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pset_virtual_view()
!!
  SUBROUTINE h5pset_virtual_view_f(dapl_id, view, hdferr)
    IMPLICIT NONE

    INTEGER(HID_T), INTENT(IN)  :: dapl_id
    INTEGER       , INTENT(IN)  :: view
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5pset_virtual_view(dapl_id, view) BIND(C,NAME='H5Pset_virtual_view')
         IMPORT :: C_INT, HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: dapl_id
         INTEGER(C_INT), INTENT(IN), VALUE :: view
       END FUNCTION h5pset_virtual_view
    END INTERFACE

    hdferr = INT( h5pset_virtual_view(dapl_id, INT(view,C_INT)) )

  END SUBROUTINE h5pset_virtual_view_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the view of a virtual dataset accessed with dapl_id.
!!
!! \param dapl_id Dataset access property list identifier for the virtual dataset.
!! \param view    The flag specifying the view of the virtual dataset. Valid values are:
!!                \li H5D_VDS_FIRST_MISSING_F
!!                \li H5D_VDS_LAST_AVAILABLE_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pget_virtual_view()
!!
  SUBROUTINE h5pget_virtual_view_f(dapl_id, view, hdferr)
    IMPLICIT NONE

    INTEGER(HID_T), INTENT(IN)  :: dapl_id
    INTEGER       , INTENT(INOUT) :: view
    INTEGER       , INTENT(OUT) :: hdferr
    INTEGER(C_INT) :: view_enum
    INTERFACE
       INTEGER FUNCTION h5pget_virtual_view(dapl_id, view) BIND(C,NAME='H5Pget_virtual_view')
         IMPORT :: C_INT, HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: dapl_id
         INTEGER(C_INT), INTENT(OUT) :: view
       END FUNCTION h5pget_virtual_view
    END INTERFACE

    hdferr = INT( h5pget_virtual_view(dapl_id, view_enum) )
    view = INT(view_enum)

  END SUBROUTINE h5pget_virtual_view_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the maximum number of missing source files and/or datasets with the printf-style names
!!        when getting the extent of an unlimited virtual dataset.
!!
!! \param dapl_id  Dataset access property list identifier for the virtual dataset.
!! \param gap_size Maximum number of files and/or datasets allowed to be missing for determining
!!                 the extent of an unlimited virtual dataset with printf-style mappings.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_virtual_printf_gap()
!!
  SUBROUTINE h5pset_virtual_printf_gap_f(dapl_id, gap_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: dapl_id
    INTEGER(HSIZE_T), INTENT(IN)  :: gap_size
    INTEGER         , INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pset_virtual_printf_gap(dapl_id, gap_size) BIND(C,NAME='H5Pset_virtual_printf_gap')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: dapl_id
         INTEGER(HSIZE_T), INTENT(IN), VALUE :: gap_size
       END FUNCTION h5pset_virtual_printf_gap
    END INTERFACE

    hdferr = INT( h5pset_virtual_printf_gap(dapl_id, gap_size) )

  END SUBROUTINE h5pset_virtual_printf_gap_f

!>
!! \ingroup FH5P
!!
!! \brief Returns the maximum number of missing source files and/or datasets with the
!!        printf-style names when getting the extent for an unlimited virtual dataset.
!!
!! \param dapl_id  Dataset access property list identifier for the virtual dataset.
!! \param gap_size Maximum Number of the files and/or datasets allowed to be missing for
!!                 determining the extent of an unlimited virtual dataset with printf-style mappings.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_virtual_printf_gap()
!!
  SUBROUTINE h5pget_virtual_printf_gap_f(dapl_id, gap_size, hdferr)
    IMPLICIT NONE

    INTEGER(HID_T)  , INTENT(IN)  :: dapl_id
    INTEGER(HSIZE_T), INTENT(OUT) :: gap_size
    INTEGER         , INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5pget_virtual_printf_gap(dapl_id, gap_size) BIND(C,NAME='H5Pget_virtual_printf_gap')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: dapl_id
         INTEGER(HSIZE_T), INTENT(OUT) :: gap_size
       END FUNCTION h5pget_virtual_printf_gap
    END INTERFACE

    hdferr = INT( h5pget_virtual_printf_gap(dapl_id, gap_size) )

  END SUBROUTINE h5pget_virtual_printf_gap_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the mapping between virtual and source datasets.
!!
!! \param dcpl_id       The identifier of the dataset creation property list that will be used when creating the
!!                      virtual dataset.
!! \param vspace_id     The dataspace identifier with the selection within the virtual dataset applied, possibly an
!!                      unlimited selection.
!! \param src_file_name The name of the HDF5 file where the source dataset is located.
!! \param src_dset_name The path to the HDF5 dataset in the file specified by src_file_name.
!! \param src_space_id  The source dataset&apos;s dataspace identifier with a selection applied, possibly an unlimited selection.
!! \param hdferr        \fortran_error
!!
!! See C API: @ref H5Pset_virtual()
!!
  SUBROUTINE h5pset_virtual_f(dcpl_id, vspace_id, src_file_name, src_dset_name, src_space_id, hdferr)
    IMPLICIT NONE

    INTEGER(HID_T), INTENT(IN)   :: dcpl_id
    INTEGER(HID_T), INTENT(IN)   :: vspace_id
    CHARACTER(LEN=*), INTENT(IN) :: src_file_name
    CHARACTER(LEN=*), INTENT(IN) :: src_dset_name
    INTEGER(HID_T), INTENT(IN)   :: src_space_id
    INTEGER, INTENT(OUT)         :: hdferr
    CHARACTER(LEN=LEN_TRIM(src_file_name)+1,KIND=C_CHAR) :: c_src_file_name
    CHARACTER(LEN=LEN_TRIM(src_dset_name)+1,KIND=C_CHAR) :: c_src_dset_name

    INTERFACE
       INTEGER FUNCTION h5pset_virtual(dcpl_id, vspace_id, c_src_file_name, c_src_dset_name, src_space_id) &
            BIND(C,NAME='H5Pset_virtual')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: dcpl_id
         INTEGER(HID_T), INTENT(IN), VALUE :: vspace_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: c_src_file_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: c_src_dset_name
         INTEGER(HID_T), INTENT(IN), VALUE :: src_space_id
       END FUNCTION h5pset_virtual
    END INTERFACE

    c_src_file_name = TRIM(src_file_name)//C_NULL_CHAR
    c_src_dset_name = TRIM(src_dset_name)//C_NULL_CHAR

    hdferr = h5pset_virtual(dcpl_id, vspace_id, c_src_file_name, c_src_dset_name, src_space_id)

  END SUBROUTINE h5pset_virtual_f

!>
!! \ingroup FH5P
!!
!! \brief Gets the number of mappings for the virtual dataset.
!!
!! \param dcpl_id The identifier of the virtual dataset creation property list.
!! \param count   The number of mappings.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pget_virtual_count()
!!
  SUBROUTINE h5pget_virtual_count_f(dcpl_id, count, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T), INTENT(IN)   :: dcpl_id
    INTEGER(SIZE_T), INTENT(OUT) :: count
    INTEGER, INTENT(OUT)         :: hdferr
    INTERFACE
       INTEGER(C_INT) FUNCTION h5pget_virtual_count(dcpl_id, count) BIND(C,NAME='H5Pget_virtual_count')
         IMPORT :: HID_T, SIZE_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T) , INTENT(IN), VALUE :: dcpl_id
         INTEGER(SIZE_T), INTENT(OUT) :: count
       END FUNCTION h5pget_virtual_count
    END INTERFACE

    hdferr = INT( h5pget_virtual_count(dcpl_id, count))

  END SUBROUTINE h5pget_virtual_count_f

!>
!! \ingroup FH5P
!!
!! \brief Gets a dataspace identifier for the selection within the virtual dataset used in the mapping.
!!
!! \param dcpl_id The identifier of the virtual dataset creation property list.
!! \param index   Mapping index. The value of index is 0 (zero) or greater and less than count (0 ≤ index < count),
!!                where count is the number of mappings returned by h5pget_virtual_count.
!! \param ds_id   Valid dataspace identifier identifier if successful; otherwise returns H5I_INVALID_HID_F.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pget_virtual_vspace()
!!
  SUBROUTINE h5pget_virtual_vspace_f(dcpl_id, index, ds_id, hdferr)
    IMPLICIT NONE

    INTEGER(HID_T) , INTENT(IN)  :: dcpl_id
    INTEGER(SIZE_T), INTENT(IN)  :: index
    INTEGER(HID_T) , INTENT(OUT) :: ds_id
    INTEGER, INTENT(OUT)         :: hdferr

    INTERFACE
       INTEGER(HID_T) FUNCTION h5pget_virtual_vspace(dcpl_id, index) BIND(C,NAME='H5Pget_virtual_vspace')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T) , INTENT(IN), VALUE :: dcpl_id
         INTEGER(SIZE_T), INTENT(IN), VALUE :: index
       END FUNCTION h5pget_virtual_vspace
    END INTERFACE

    ds_id = h5pget_virtual_vspace(dcpl_id, index)

    hdferr = 0
    IF(ds_id.LT.0) hdferr = -1

END SUBROUTINE h5pget_virtual_vspace_f

!>
!! \ingroup FH5P
!!
!! \brief Gets a dataspace identifier for the selection within the source dataset used in the mapping.
!!
!! \param dcpl_id The Identifier of the virtual dataset creation property list.
!! \param index   Mapping index.The value of index is 0 (zero) or greater and less than count (0 ≤ index < count),
!!                where count is the number of mappings returned by h5pget_virtual_count.
!! \param ds_id   Dataspace identifier.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Pget_virtual_srcspace()
!!
SUBROUTINE h5pget_virtual_srcspace_f(dcpl_id, index, ds_id, hdferr)
  IMPLICIT NONE

  INTEGER(HID_T) , INTENT(IN)  :: dcpl_id
  INTEGER(SIZE_T), INTENT(IN)  :: index
  INTEGER(HID_T) , INTENT(OUT) :: ds_id
  INTEGER, INTENT(OUT)         :: hdferr

  INTERFACE
     INTEGER(HID_T) FUNCTION h5pget_virtual_srcspace(dcpl_id, index) BIND(C,NAME='H5Pget_virtual_srcspace')
       IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
       INTEGER(HID_T) , INTENT(IN), VALUE :: dcpl_id
       INTEGER(SIZE_T), INTENT(IN), VALUE :: index
     END FUNCTION h5pget_virtual_srcspace
  END INTERFACE

  ds_id = h5pget_virtual_srcspace(dcpl_id, index)

  hdferr = 0
  IF(ds_id.LT.0) hdferr = -1

END SUBROUTINE h5pget_virtual_srcspace_f

!>
!! \ingroup FH5P
!!
!! \brief Gets the filename of a source dataset used in the mapping.
!!
!! \param dcpl_id  The identifier of the virtual dataset creation property list.
!! \param index    Mapping index. The value of index is 0 (zero) or greater and less than count (0 ≤ index < count),
!!                 where count is the number of mappings returned by h5pget_virtual_count.
!! \param name     A buffer containing the name of the file containing the source dataset.
!! \param hdferr   \fortran_error
!! \param name_len The size of name needed to hold the filename. (OUT)
!!
!!
!! See C API: @ref H5Pget_virtual_filename()
!!
SUBROUTINE h5pget_virtual_filename_f(dcpl_id, index, name, hdferr, name_len)

  IMPLICIT NONE
  INTEGER(HID_T)  , INTENT(IN)  :: dcpl_id
  INTEGER(SIZE_T) , INTENT(IN)  :: index
  CHARACTER(LEN=*), INTENT(OUT) :: name
  INTEGER, INTENT(OUT)          :: hdferr
  INTEGER(SIZE_T), OPTIONAL     :: name_len

  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(1:LEN(name)+1), TARGET :: c_name
  TYPE(C_PTR) :: f_ptr

  INTERFACE
     INTEGER(SIZE_T) FUNCTION h5pget_virtual_filename(dcpl_id, index, name, size) BIND(C, NAME='H5Pget_virtual_filename')
       IMPORT :: HID_T, SIZE_T, C_PTR
         IMPLICIT NONE
       INTEGER(HID_T) , INTENT(IN), VALUE :: dcpl_id
       INTEGER(SIZE_T), INTENT(IN), VALUE :: index
       TYPE(C_PTR), VALUE :: name
       INTEGER(SIZE_T), INTENT(IN), VALUE :: size
     END FUNCTION h5pget_virtual_filename
  END INTERFACE

  hdferr = 0
  IF(PRESENT(name_len))THEN
     name_len = INT(h5pget_virtual_filename(dcpl_id, index, C_NULL_PTR, 0_SIZE_T), SIZE_T)
     IF(name_len.LT.0) hdferr = -1
  ELSE
     f_ptr = C_LOC(c_name(1)(1:1))

     IF(INT(h5pget_virtual_filename(dcpl_id, index, f_ptr, INT(LEN(name)+1,SIZE_T)), SIZE_T).LT.0)THEN
        hdferr = -1
     ELSE
        CALL HD5c2fstring(name, c_name, LEN(name,KIND=SIZE_T), LEN(name,KIND=SIZE_T)+1_SIZE_T )
     ENDIF

  ENDIF

END SUBROUTINE h5pget_virtual_filename_f

!>
!! \ingroup FH5P
!!
!! \brief Gets the name of a source dataset used in the mapping.
!!
!! \param dcpl_id  The identifier of the virtual dataset creation property list.
!! \param index    Mapping index. The value of index is 0 (zero) or greater and less than count (0 ≤ index < count),
!!                 where count is the number of mappings returned by h5pget_virtual_count.
!! \param name     A buffer containing the name of the source dataset.
!! \param hdferr   \fortran_error
!! \param name_len The size of name needed to hold the source dataset name.
!!
!! See C API: @ref H5Pget_virtual_dsetname()
!!
SUBROUTINE h5pget_virtual_dsetname_f(dcpl_id, index, name, hdferr, name_len)

  IMPLICIT NONE
  INTEGER(HID_T)  , INTENT(IN)  :: dcpl_id
  INTEGER(SIZE_T) , INTENT(IN)  :: index
  CHARACTER(LEN=*), INTENT(OUT) :: name
  INTEGER, INTENT(OUT)          :: hdferr
  INTEGER(SIZE_T), OPTIONAL     :: name_len

  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(1:LEN(name)+1), TARGET :: c_name
  TYPE(C_PTR) :: f_ptr

  INTERFACE
     INTEGER(SIZE_T) FUNCTION h5pget_virtual_dsetname(dcpl_id, index, name, size) BIND(C, NAME='H5Pget_virtual_dsetname')
       IMPORT :: HID_T, SIZE_T, C_PTR
         IMPLICIT NONE
       INTEGER(HID_T) , INTENT(IN), VALUE :: dcpl_id
       INTEGER(SIZE_T), INTENT(IN), VALUE :: index
       TYPE(C_PTR), VALUE :: name
       INTEGER(SIZE_T), INTENT(IN), VALUE :: size
     END FUNCTION h5pget_virtual_dsetname
  END INTERFACE

  hdferr = 0
  IF(PRESENT(name_len))THEN
     name_len = INT(h5pget_virtual_dsetname(dcpl_id, index, C_NULL_PTR, 0_SIZE_T), SIZE_T)
     IF(name_len.LT.0) hdferr = -1
  ELSE
     f_ptr = C_LOC(c_name(1)(1:1))

     IF(INT(h5pget_virtual_dsetname(dcpl_id, index, f_ptr, INT(LEN(name)+1,SIZE_T)), SIZE_T).LT.0)THEN
        hdferr = -1
     ELSE
        CALL HD5c2fstring(name, c_name, LEN(name,KIND=SIZE_T), LEN(name,KIND=SIZE_T)+1_SIZE_T )
     ENDIF
  ENDIF

END SUBROUTINE h5pget_virtual_dsetname_f

!>
!! \ingroup FH5P
!!
!! \brief Gets the value of the "minimize dataset headers" value which creates
!!        smaller dataset object headers when its set and no attributes are present.
!!
!! \param dcpl_id  Target dataset creation property list identifier.
!! \param minimize Value of the setting.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_dset_no_attrs_hint()
!!
  SUBROUTINE h5pget_dset_no_attrs_hint_f(dcpl_id, minimize, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)              :: dcpl_id
    LOGICAL        , INTENT(OUT)             :: minimize
    INTEGER        , INTENT(OUT)             :: hdferr
    LOGICAL(C_BOOL) :: c_minimize

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pget_dset_no_attrs_hint_c(dcpl_id, minimize) BIND(C, NAME='H5Pget_dset_no_attrs_hint')
         IMPORT :: C_INT, HID_T, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: dcpl_id
         LOGICAL(C_BOOL), INTENT(OUT) :: minimize
       END FUNCTION H5Pget_dset_no_attrs_hint_c
    END INTERFACE

    hdferr = INT(H5Pget_dset_no_attrs_hint_c(dcpl_id, c_minimize))

    ! Transfer value of C C_BOOL type to Fortran LOGICAL
    minimize = c_minimize

   END SUBROUTINE h5pget_dset_no_attrs_hint_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the value of the "minimize dataset headers" value which creates
!!       smaller dataset object headers when its set and no attributes are present.
!!
!! \param dcpl_id  Target dataset creation property list identifier.
!! \param minimize Value of the setting.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_dset_no_attrs_hint()
!!
  SUBROUTINE h5pset_dset_no_attrs_hint_f(dcpl_id, minimize, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)              :: dcpl_id
    LOGICAL        , INTENT(IN)              :: minimize
    INTEGER        , INTENT(OUT)             :: hdferr
    LOGICAL(C_BOOL) :: c_minimize

    INTERFACE
       INTEGER FUNCTION h5pset_dset_no_attrs_hint_c(dcpl_id, minimize) BIND(C, NAME='H5Pset_dset_no_attrs_hint')
         IMPORT :: HID_T, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: dcpl_id
         LOGICAL(C_BOOL), INTENT(IN), VALUE :: minimize
       END FUNCTION h5pset_dset_no_attrs_hint_c
    END INTERFACE

    ! Transfer value of Fortran LOGICAL to C C_BOOL type
    c_minimize = minimize

    hdferr = INT(h5pset_dset_no_attrs_hint_c(dcpl_id, c_minimize))

  END SUBROUTINE h5pset_dset_no_attrs_hint_f

!>
!! \ingroup FH5P
!!
!! \brief Set the file VOL connector (VOL_ID) for a file access property list (PLIST_ID)
!!
!! \param plist_id     Access property list identifier.
!! \param new_vol_id   VOL connector id.
!! \param hdferr       \fortran_error
!! \param new_vol_info VOL connector info.
!!
!! See C API: @ref H5Pset_vol()
!!
  SUBROUTINE h5pset_vol_f(plist_id, new_vol_id, hdferr, new_vol_info)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)   :: plist_id
    INTEGER(HID_T) , INTENT(IN)   :: new_vol_id
    INTEGER        , INTENT(OUT)  :: hdferr
    TYPE(C_PTR)    , INTENT(IN), OPTIONAL :: new_vol_info

    TYPE(C_PTR) :: new_vol_info_default

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pset_vol(plist_id, new_vol_id, new_vol_info) BIND(C, NAME='H5Pset_vol')
         IMPORT :: C_INT, HID_T, C_PTR
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: plist_id
         INTEGER(HID_T), INTENT(IN), VALUE :: new_vol_id
         TYPE(C_PTR)   , INTENT(IN), VALUE :: new_vol_info
       END FUNCTION H5Pset_vol
    END INTERFACE

    new_vol_info_default = C_NULL_PTR
    IF(PRESENT(new_vol_info)) new_vol_info_default=new_vol_info

    hdferr = INT(H5Pset_vol(plist_id, new_vol_id, new_vol_info_default))

  END SUBROUTINE h5pset_vol_f

!>
!! \ingroup FH5P
!!
!! \brief Get the file VOL connector (VOL_ID) for a file access property list (PLIST_ID)
!
!! \param plist_id Access property list identifier.
!! \param vol_id   VOL connector id.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_vol_id()
!!
  SUBROUTINE h5pget_vol_id_f(plist_id, vol_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)   :: plist_id
    INTEGER(HID_T), INTENT(OUT)  :: vol_id
    INTEGER       , INTENT(OUT)  :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION h5pget_vol_id(plist_id, vol_id) BIND(C, NAME='H5Pget_vol_id')
         IMPORT :: C_INT, HID_T, C_PTR
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: plist_id
         INTEGER(HID_T), INTENT(OUT) :: vol_id
       END FUNCTION h5pget_vol_id
    END INTERFACE

    hdferr = INT(H5Pget_vol_id(plist_id, vol_id))

  END SUBROUTINE h5pget_vol_id_f

!>
!! \ingroup FH5P
!!
!! \brief Query the capability flags for the VOL connector that will be used with this file access property list (FAPL).
!
!! \param plist_id  File access property list identifier
!! \param cap_flags Flags that indicate the VOL connector capabilities
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pget_vol_cap_flags()
!!
  SUBROUTINE  h5pget_vol_cap_flags_f(plist_id, cap_flags, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN)   :: plist_id
    INTEGER(C_INT64_T), INTENT(OUT)  :: cap_flags
    INTEGER           , INTENT(OUT)  :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pget_vol_cap_flags(plist_id, cap_flags) BIND(C, NAME='H5Pget_vol_cap_flags')
         IMPORT :: C_INT, HID_T, C_PTR, C_INT64_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: plist_id
         INTEGER(C_INT64_T), INTENT(OUT)   :: cap_flags
       END FUNCTION H5Pget_vol_cap_flags
    END INTERFACE

    hdferr = INT(H5Pget_vol_cap_flags(plist_id, cap_flags))

  END SUBROUTINE h5pget_vol_cap_flags_f

!>
!! \ingroup FH5P
!!
!! \brief Gets the file locking properties. File locking is mainly used to help enforce SWMR semantics.
!!
!! \param fapl_id               Target fileTarget file access property list identifier.
!! \param use_file_locking      Whether or not to use file locks.
!! \param ignore_disabled_locks Whether or not to ignore file locks when locking is disabled on a file system.
!! \param hdferr                \fortran_error
!!
!! See C API: @ref H5Pget_file_locking()
!!
  SUBROUTINE h5pget_file_locking_f(fapl_id, use_file_locking, ignore_disabled_locks, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)              :: fapl_id
    LOGICAL        , INTENT(OUT)             :: use_file_locking
    LOGICAL        , INTENT(OUT)             :: ignore_disabled_locks
    INTEGER        , INTENT(OUT)             :: hdferr
    LOGICAL(C_BOOL) :: c_use_flag
    LOGICAL(C_BOOL) :: c_ignore_flag

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pget_file_locking(fapl_id, use_file_locking, ignore_disabled_locks) &
            BIND(C, NAME='H5Pget_file_locking')
         IMPORT :: C_INT, HID_T, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: fapl_id
         LOGICAL(C_BOOL), INTENT(OUT) :: use_file_locking
         LOGICAL(C_BOOL), INTENT(OUT) :: ignore_disabled_locks
       END FUNCTION H5Pget_file_locking
    END INTERFACE

    hdferr = INT(H5Pget_file_locking(fapl_id, c_use_flag, c_ignore_flag))

    ! Transfer value of C C_BOOL type to Fortran LOGICAL
    use_file_locking = c_use_flag
    ignore_disabled_locks = c_ignore_flag

   END SUBROUTINE h5pget_file_locking_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the file locking properties. File locking is mainly used to help enforce SWMR semantics.
!!
!! \param fapl_id               Target file access property list identifier.
!! \param use_file_locking      Whether or not to use file locks.
!! \param ignore_disabled_locks Whether or not to ignore file locks when locking is disabled on a file system.
!! \param hdferr                \fortran_error
!!
!! See C API: @ref H5Pset_file_locking()
!!
  SUBROUTINE h5pset_file_locking_f(fapl_id, use_file_locking, ignore_disabled_locks, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)              :: fapl_id
    LOGICAL        , INTENT(IN)              :: use_file_locking
    LOGICAL        , INTENT(IN)              :: ignore_disabled_locks
    INTEGER        , INTENT(OUT)             :: hdferr
    LOGICAL(C_BOOL) :: c_use_flag
    LOGICAL(C_BOOL) :: c_ignore_flag

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pset_file_locking(fapl_id, use_file_locking, ignore_disabled_locks) &
            BIND(C, NAME='H5Pset_file_locking')
         IMPORT :: C_INT, HID_T, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: fapl_id
         LOGICAL(C_BOOL), INTENT(IN), VALUE :: use_file_locking
         LOGICAL(C_BOOL), INTENT(IN), VALUE :: ignore_disabled_locks
       END FUNCTION H5Pset_file_locking
    END INTERFACE

    ! Transfer value of Fortran LOGICAL to C C_BOOL type
    c_use_flag = use_file_locking
    c_ignore_flag = ignore_disabled_locks

    hdferr = INT(H5Pset_file_locking(fapl_id, c_use_flag, c_ignore_flag))

  END SUBROUTINE h5pset_file_locking_f

!>
!! \ingroup FH5P
!!
!! \brief Retrieves the cause for not performing selection or vector I/O on the last parallel I/O call.
!!
!! \param plist_id              Dataset transfer property list identifier
!! \param no_selection_io_cause	A bitwise set value indicating the relevant causes that prevented selection I/O from being performed
!! \param hdferr                \fortran_error
!!
!! See C API: @ref H5Pget_no_selection_io_cause()
!!
   SUBROUTINE h5pget_no_selection_io_cause_f(plist_id, no_selection_io_cause, hdferr)
     IMPLICIT NONE
     INTEGER(HID_T), INTENT(IN)  :: plist_id
     INTEGER       , INTENT(OUT) :: no_selection_io_cause
     INTEGER       , INTENT(OUT) :: hdferr

     INTEGER(C_INT32_T) :: c_no_selection_io_cause

     INTERFACE
        INTEGER(C_INT) FUNCTION H5Pget_no_selection_io_cause(plist_id, no_selection_io_cause) &
             BIND(C, NAME='H5Pget_no_selection_io_cause')
          IMPORT :: HID_T, C_INT, C_INT32_T
          IMPLICIT NONE
          INTEGER(HID_T)    , VALUE :: plist_id
          INTEGER(C_INT32_T)        :: no_selection_io_cause
        END FUNCTION H5Pget_no_selection_io_cause
     END INTERFACE

     hdferr = INT( H5Pget_no_selection_io_cause(plist_id, c_no_selection_io_cause))

     no_selection_io_cause = INT(c_no_selection_io_cause)

   END SUBROUTINE h5pget_no_selection_io_cause_f


!>
!! \ingroup FH5P
!!
!! \brief Sets the file space handling strategy and persisting free-space values for a file creation property list.
!!
!! \param plist_id  File creation property list identifier
!! \param strategy  The file space handling strategy to be used. See: H5F_fspace_strategy_t
!! \param persist   Indicates whether free space should be persistent or not
!! \param threshold The smallest free-space section size that the free space manager will track
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pset_file_space_strategy()
!!
   SUBROUTINE H5Pset_file_space_strategy_f(plist_id, strategy, persist, threshold, hdferr)
     IMPLICIT NONE
     INTEGER(HID_T)  , INTENT(IN)  :: plist_id
     INTEGER         , INTENT(IN)  :: strategy
     LOGICAL         , INTENT(IN)  :: persist
     INTEGER(HSIZE_T), INTENT(IN)  :: threshold
     INTEGER         , INTENT(OUT) :: hdferr

     LOGICAL(C_BOOL) :: c_persist

     INTERFACE
        INTEGER(C_INT) FUNCTION H5Pset_file_space_strategy(plist_id, strategy, persist, threshold) &
             BIND(C, NAME='H5Pset_file_space_strategy')
          IMPORT :: HID_T, HSIZE_T, C_INT, C_BOOL
          IMPLICIT NONE
          INTEGER(HID_T)  , VALUE :: plist_id
          INTEGER(C_INT)  , VALUE :: strategy
          LOGICAL(C_BOOL) , VALUE :: persist
          INTEGER(HSIZE_T), VALUE :: threshold
        END FUNCTION H5Pset_file_space_strategy
     END INTERFACE

     ! Transfer value of Fortran LOGICAL to C C_BOOL type
     c_persist = persist

     hdferr = INT( H5Pset_file_space_strategy(plist_id, INT(strategy, C_INT), c_persist, threshold) )

   END SUBROUTINE H5Pset_file_space_strategy_f

!>
!! \ingroup FH5P
!!
!! \brief Gets the file space handling strategy and persisting free-space values for a file creation property list.
!!
!! \param plist_id  File creation property list identifier
!! \param strategy  The file space handling strategy to be used
!! \param persist   Indicate whether free space should be persistent or not
!! \param threshold The free-space section size threshold value
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Pget_file_space_strategy()
!!
   SUBROUTINE h5pget_file_space_strategy_f(plist_id, strategy, persist, threshold, hdferr)
     IMPLICIT NONE
     INTEGER(HID_T)  , INTENT(IN)  :: plist_id
     INTEGER         , INTENT(OUT) :: strategy
     LOGICAL         , INTENT(OUT) :: persist
     INTEGER(HSIZE_T), INTENT(OUT) :: threshold
     INTEGER         , INTENT(OUT) :: hdferr

     LOGICAL(C_BOOL) :: c_persist
     INTEGER(C_INT)  :: c_strategy

     INTERFACE
        INTEGER(C_INT) FUNCTION H5Pget_file_space_strategy(plist_id, strategy, persist, threshold) &
             BIND(C, NAME='H5Pget_file_space_strategy')
          IMPORT :: HID_T, HSIZE_T, C_INT, C_BOOL
          IMPLICIT NONE
          INTEGER(HID_T), VALUE :: plist_id
          INTEGER(C_INT)   :: strategy
          LOGICAL(C_BOOL)  :: persist
          INTEGER(HSIZE_T) :: threshold
        END FUNCTION H5Pget_file_space_strategy
     END INTERFACE


     hdferr = INT( H5Pget_file_space_strategy(plist_id, c_strategy, c_persist, threshold) )

     ! Transfer value of Fortran LOGICAL and C C_BOOL type
     persist = .FALSE.
     strategy = -1
     IF(hdferr .GE. 0)THEN
        persist = c_persist
        strategy = INT(c_strategy)
     ENDIF

   END SUBROUTINE h5pget_file_space_strategy_f

!>
!! \ingroup FH5P
!!
!! \brief Sets the file space page size for a file creation property list.
!!
!! \param prp_id   File creation property list identifier
!! \param fsp_size File space page size
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pset_file_space_page_size()
!!
  SUBROUTINE h5pset_file_space_page_size_f(prp_id, fsp_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HSIZE_T), INTENT(IN) :: fsp_size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pset_file_space_page_size(prp_id, fsp_size) &
            BIND(C,NAME='H5Pset_file_space_page_size')
         IMPORT :: C_INT, HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , VALUE :: prp_id
         INTEGER(HSIZE_T), VALUE :: fsp_size
       END FUNCTION H5Pset_file_space_page_size
    END INTERFACE

    hdferr = INT(h5pset_file_space_page_size(prp_id, fsp_size))

  END SUBROUTINE h5pset_file_space_page_size_f

!>
!! \ingroup FH5P
!!
!! \brief Gets the file space page size for a file creation property list.
!!
!! \param prp_id   File creation property list identifier
!! \param fsp_size File space page size
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Pget_file_space_page_size()
!!
  SUBROUTINE h5pget_file_space_page_size_f(prp_id, fsp_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HSIZE_T), INTENT(OUT) :: fsp_size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(C_INT) FUNCTION H5Pget_file_space_page_size(prp_id, fsp_size) &
            BIND(C,NAME='H5Pget_file_space_page_size')
         IMPORT :: C_INT, HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: prp_id
         INTEGER(HSIZE_T)      :: fsp_size
       END FUNCTION H5Pget_file_space_page_size
    END INTERFACE

    hdferr = INT(h5pget_file_space_page_size(prp_id, fsp_size))

  END SUBROUTINE h5pget_file_space_page_size_f
!>
!! \ingroup FH5P
!!
!! \brief Retrieves the type(s) of I/O that HDF5 actually performed on raw data
!!        during the last I/O call.
!!
!! \param plist_id                 File creation property list identifier
!! \param actual_selection_io_mode A bitwise set value indicating the type(s) of I/O performed
!! \param hdferr                   \fortran_error
!!
!! See C API: @ref H5Pget_actual_selection_io_mode()
!!
  SUBROUTINE h5pget_actual_selection_io_mode_f(plist_id, actual_selection_io_mode, hdferr)

    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: plist_id
    INTEGER       , INTENT(OUT) :: actual_selection_io_mode
    INTEGER       , INTENT(OUT) :: hdferr

    INTEGER(C_INT32_T) :: c_actual_selection_io_mode

    INTERFACE
        INTEGER(C_INT) FUNCTION H5Pget_actual_selection_io_mode(plist_id, actual_selection_io_mode) &
             BIND(C, NAME='H5Pget_actual_selection_io_mode')
          IMPORT :: HID_T, C_INT32_T, C_INT
          IMPLICIT NONE
          INTEGER(HID_T), VALUE :: plist_id
          INTEGER(C_INT32_T)    :: actual_selection_io_mode
        END FUNCTION H5Pget_actual_selection_io_mode
     END INTERFACE

     hdferr = INT(H5Pget_actual_selection_io_mode(plist_id, c_actual_selection_io_mode))

     actual_selection_io_mode = INT(c_actual_selection_io_mode)

   END SUBROUTINE h5pget_actual_selection_io_mode_f

END MODULE H5P

