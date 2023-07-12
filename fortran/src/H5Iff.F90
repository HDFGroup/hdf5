!> @defgroup FH5I Fortran Identifier (H5I) Interface
!!
!! @see H5I, C-API
!!
!! @see @ref H5I_UG, User Guide
!!

!> @ingroup FH5I
!!
!! @brief This module contains Fortran interfaces for H5I functions.
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5I function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5I

  USE H5GLOBAL
  IMPLICIT NONE

CONTAINS

!>
!! \ingroup FH5I
!!
!! \brief Retrieves the type of an object.
!!
!! \param obj_id Object identifier.
!! \param type   Type of the object, possible values:
!!               \li H5I_FILE_F
!!               \li H5I_GROUP_F
!!               \li H5I_DATATYPE_F
!!               \li H5I_DATASPACE_F
!!               \li H5I_DATASET_F
!!               \li H5I_ATTR_F
!!               \li H5I_BADID_F
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Iget_type()
!!
  SUBROUTINE h5iget_type_f(obj_id, TYPE, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    INTEGER, INTENT(OUT) :: TYPE
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5iget_type_c(obj_id, TYPE) BIND(C, NAME='h5iget_type_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER, INTENT(OUT) :: TYPE
       END FUNCTION h5iget_type_c
    END INTERFACE
    hdferr = h5iget_type_c(obj_id, TYPE)
  END SUBROUTINE h5iget_type_f

!>
!! \ingroup FH5I
!!
!! \brief Gets a name of an object specified by its identifier.
!!
!! \param obj_id    Attribute identifier.
!! \param buf_size  Size of a buffer to read name in.
!! \param buf       Buffer to read name in, name will be truncated if buffer is not big enough.
!! \param name_size Name size.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Iget_name()
!!
  SUBROUTINE h5iget_name_f(obj_id, buf, buf_size, name_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    INTEGER(SIZE_T), INTENT(IN) :: buf_size
    CHARACTER(LEN=*), INTENT(OUT) :: buf
    INTEGER(SIZE_T), INTENT(OUT) :: name_size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5iget_name_c(obj_id, buf, buf_size, name_size) BIND(C, NAME='h5iget_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: buf
         INTEGER(SIZE_T), INTENT(IN) :: buf_size
         INTEGER(SIZE_T), INTENT(OUT) :: name_size
       END FUNCTION h5iget_name_c
    END INTERFACE

    hdferr = h5iget_name_c(obj_id, buf, buf_size, name_size)
  END SUBROUTINE h5iget_name_f

!>
!! \ingroup FH5I
!!
!! \brief Increments the reference count of an ID.
!!
!! \param obj_id    Object identifier.
!! \param ref_count Current reference count of the ID.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Iinc_ref()
!!
  SUBROUTINE h5iinc_ref_f(obj_id, ref_count, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    INTEGER, INTENT(OUT) :: ref_count
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5iinc_ref_c(obj_id, ref_count) BIND(C, NAME='h5iinc_ref_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER, INTENT(OUT) :: ref_count
       END FUNCTION h5iinc_ref_c
    END INTERFACE
    hdferr = h5iinc_ref_c(obj_id, ref_count)
  END SUBROUTINE h5iinc_ref_f

!>
!! \ingroup FH5I
!!
!! \brief Decrements the reference count of an ID.
!!
!! \param obj_id    Object identifier.
!! \param ref_count Current reference count of the ID.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Idec_ref()
!!
  SUBROUTINE h5idec_ref_f(obj_id, ref_count, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    INTEGER, INTENT(OUT) :: ref_count
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5idec_ref_c(obj_id, ref_count) BIND(C, NAME='h5idec_ref_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER, INTENT(OUT) :: ref_count
       END FUNCTION h5idec_ref_c
    END INTERFACE
    hdferr = h5idec_ref_c(obj_id, ref_count)
  END SUBROUTINE h5idec_ref_f

!>
!! \ingroup FH5I
!!
!! \brief Retrieves the reference count of an ID.
!!
!! \param obj_id    Object identifier.
!! \param ref_count Current reference count of the ID.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Iget_ref()
!!
  SUBROUTINE h5iget_ref_f(obj_id, ref_count, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    INTEGER, INTENT(OUT) :: ref_count
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5iget_ref_c(obj_id, ref_count) BIND(C, NAME='h5iget_ref_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER, INTENT(OUT) :: ref_count
       END FUNCTION h5iget_ref_c
    END INTERFACE
    hdferr = h5iget_ref_c(obj_id, ref_count)
  END SUBROUTINE h5iget_ref_f
!>
!! \ingroup FH5I
!!
!! \brief Obtains file identifier from the object identifier.
!!
!! \param obj_id  Object identifier.
!! \param file_id File identifier.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Iget_file_id()
!!
  SUBROUTINE h5iget_file_id_f(obj_id, file_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: obj_id
    INTEGER(HID_T), INTENT(OUT) :: file_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5iget_file_id_c(obj_id, file_id) BIND(C, NAME='h5iget_file_id_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN)  :: obj_id
         INTEGER(HID_T), INTENT(OUT) :: file_id
       END FUNCTION h5iget_file_id_c
    END INTERFACE
    hdferr = h5iget_file_id_c(obj_id, file_id)
  END SUBROUTINE h5iget_file_id_f
!>
!! \ingroup FH5I
!!
!! \brief Check if an ID is valid without producing an error message.
!!
!! \param id      Identifier.
!! \param valid   Status of id as a valid identifier.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Iis_valid()
!!
  SUBROUTINE h5iis_valid_f(id, valid, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: id
    LOGICAL, INTENT(OUT) :: valid
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER  :: c_valid ! 0 = .false, 1 = .true.

    INTERFACE
       INTEGER FUNCTION h5iis_valid_c(id, c_valid) BIND(C, NAME='h5iis_valid_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN)  :: id
         INTEGER  :: c_valid
       END FUNCTION h5iis_valid_c
    END INTERFACE

    hdferr = h5iis_valid_c(id, c_valid)

    valid = .FALSE. ! Default
    IF(c_valid.EQ.1) valid = .TRUE.

  END SUBROUTINE h5iis_valid_f
END MODULE H5I

