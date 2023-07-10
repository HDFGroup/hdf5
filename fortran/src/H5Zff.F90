!> @defgroup FH5Z Fortran Filter (H5Z) Interface
!!
!! @see H5Z, C-API
!!
!! @see @ref H5Z_UG, User Guide
!!

!> @ingroup FH5Z
!!
!! @brief This module contains Fortran interfaces for H5Z functions.
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
! NOTES!
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5Z function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5Z

  USE H5GLOBAL
  IMPLICIT NONE

CONTAINS

!>
!! \ingroup FH5Z
!!
!! \brief Unregisters specified filters.
!!
!! \param filter Filter; may have one of the following values:
!!               \li H5Z_FILTER_DEFLATE_F
!!               \li H5Z_FILTER_SZIP_F
!!               \li H5Z_FILTER_NBIT_F
!!               \li H5Z_FILTER_SCALEOFFSET_F
!!               \li H5Z_FILTER_SHUFFLE_F
!!               \li H5Z_FILTER_FLETCHER32_F
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Zunregister()
!!
  SUBROUTINE h5zunregister_f(filter, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5zunregister_c(filter) BIND(C,NAME='h5zunregister_c')
         INTEGER, INTENT(IN) :: filter
       END FUNCTION h5zunregister_c
    END INTERFACE
    hdferr = h5zunregister_c(filter)
  END SUBROUTINE h5zunregister_f

!>
!! \ingroup FH5Z
!!
!! \brief Queries if filter is available
!!
!! \param filter  Filter; may be one of the following:
!!                \li H5Z_FILTER_DEFLATE_F
!!                \li H5Z_FILTER_SZIP_F
!!                \li H5Z_FILTER_NBIT_F
!!                \li H5Z_FILTER_SCALEOFFSET_F
!!                \li H5Z_FILTER_SHUFFLE_F
!!                \li H5Z_FILTER_FLETCHER32_F
!! \param status  Flag; .TRUE. if filter is available, .FALSE. otherwise.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Zfilter_avail()
!!
  SUBROUTINE h5zfilter_avail_f(filter, status, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    LOGICAL, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: flag                     ! "TRUE/FALSE/ERROR from C"

    INTERFACE
       INTEGER FUNCTION h5zfilter_avail_c(filter, flag) BIND(C,NAME='h5zfilter_avail_c')
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: filter
         INTEGER :: flag
       END FUNCTION h5zfilter_avail_c
    END INTERFACE

    hdferr = h5zfilter_avail_c(filter, flag)
    status = .TRUE.
    IF (flag .EQ. 0) status = .FALSE.

  END SUBROUTINE h5zfilter_avail_f

!>
!! \ingroup FH5Z
!!
!! \brief Queries if filter has its encoder and/or decoder available.
!!
!! \param filter       Filter; may be one of the following:
!!                     \li H5Z_FILTER_DEFLATE_F
!!                     \li H5Z_FILTER_SZIP_F
!!                     \li H5Z_FILTER_NBIT_F
!!                     \li H5Z_FILTER_SCALEOFFSET_F
!!                     \li H5Z_FILTER_SHUFFLE_F
!!                     \li H5Z_FILTER_FLETCHER32_Ffilter
!! \param config_flags Flag, indicates if filter has its encoder and/or decoder available, possible values:
!!                     \li H5Z_FILTER_ENCODE_ENABLED_F
!!                     \li H5Z_FILTER_DECODE_ENABLED_F
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Zget_filter_info()
!!
  SUBROUTINE h5zget_filter_info_f(filter, config_flags, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    INTEGER, INTENT(OUT) :: config_flags
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5zget_filter_info_c(filter, config_flags) BIND(C,NAME='h5zget_filter_info_c')
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: filter
         INTEGER, INTENT(OUT) :: config_flags
       END FUNCTION h5zget_filter_info_c
    END INTERFACE

    hdferr = h5zget_filter_info_c(filter, config_flags)

  END SUBROUTINE h5zget_filter_info_f

END MODULE H5Z





