!> @defgroup FH5DO Fortran High Level Optimized Interface
!!
!! @see H5DO, C-HL API
!!
!! @see @ref H5DO_UG, User Guide
!!

!> @ingroup FH5DO
!!
!! @brief This module contains Fortran interfaces for H5DO
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

MODULE H5DO

  USE h5fortran_types
  USE hdf5
  IMPLICIT NONE

CONTAINS

!>
!! \ingroup FH5DO
!!
!! \brief Appends data to a dataset along a specified dimension.
!!
!! \param dset_id   Dataset identifier
!! \param dxpl_id   Dataset transfer property list identifier
!! \param axis      Dataset Dimension (0-based) for the append
!! \param extension Number of elements to append for the axis-th dimension
!! \param memtype   The memory datatype identifier
!! \param buf       Buffer with data for the append
!! \param errcode   \fortran_error
!!
!! See C API: @ref H5DOappend()
!!
  SUBROUTINE H5DOappend_f (dset_id, dxpl_id, axis, extension, memtype, buf, errcode)

    IMPLICIT NONE

    INTEGER(hid_t) , INTENT(IN)  :: dset_id
    INTEGER(hid_t) , INTENT(IN)  :: dxpl_id
    INTEGER        , INTENT(IN)  :: axis
    INTEGER(SIZE_T), INTENT(IN)  :: extension
    INTEGER(hid_t) , INTENT(IN)  :: memtype
    TYPE(C_PTR)                  :: buf
    INTEGER        , INTENT(OUT) :: errcode

   INTERFACE
       INTEGER(C_INT) FUNCTION H5DOappend(dset_id, dxpl_id, axis, extension, memtype, buf) &
            BIND(C,NAME='H5DOappend')
         
         IMPORT :: C_INT, C_PTR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE

         INTEGER(hid_t) , VALUE :: dset_id
         INTEGER(hid_t) , VALUE :: dxpl_id
         INTEGER(C_INT) , VALUE :: axis
         INTEGER(SIZE_T), VALUE :: extension
         INTEGER(hid_t) , VALUE :: memtype
         TYPE(C_PTR)    , VALUE :: buf
       END FUNCTION H5DOappend
    END INTERFACE

    errcode = INT(H5DOappend(dset_id, dxpl_id, INT(axis,C_INT), extension, memtype, buf))

  END SUBROUTINE H5DOappend_f

END MODULE H5DO
