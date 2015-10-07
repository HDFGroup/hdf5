!****h* ROBODoc/H5Z
!
! NAME
!  MODULE H5Z
!
! PURPOSE
!  This file contains Fortran interfaces for H5Z functions.
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  Copyright by The HDF Group.                                                 *
!  Copyright by the Board of Trustees of the University of Illinois.           *
!  All rights reserved.                                                        *
!                                                                              *
!  This file is part of HDF5.  The full HDF5 copyright notice, including       *
!  terms governing use, modification, and redistribution, is contained in      *
!  the files COPYING and Copyright.html.  COPYING can be found at the root     *
!  of the source code distribution tree; Copyright.html can be found at the    *
!  root level of an installed copy of the electronic HDF5 document set and     *
!  is linked from the top-level documents page.  It can also be found at       *
!  http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have            *
!  access to either file, you may request a copy from help@hdfgroup.org.       *
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
!*****

MODULE H5Z

  USE H5GLOBAL

CONTAINS

!****s* H5Z/h5zunregister_f
!
! NAME
!  h5zunregister_f
!
! PURPOSE
!  Unregisters specified filetr
!
! INPUTS
!  filter - Filter; may have one of the following values:
!            H5Z_FILTER_DEFLATE_F
!            H5Z_FILTER_SZIP_F
!            H5Z_FILTER_NBIT_F
!            H5Z_FILTER_SCALEOFFSET_F
!            H5Z_FILTER_SHUFFLE_F
!            H5Z_FILTER_FLETCHER32_F
!            
! OUTPUTS
!  hdferr - Error code
!            Success:  0
!            Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
! SOURCE
  SUBROUTINE h5zunregister_f(filter, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    INTEGER, INTENT(OUT) :: hdferr
!*****
    INTERFACE
       INTEGER FUNCTION h5zunregister_c(filter) BIND(C,NAME='h5zunregister_c')
         INTEGER, INTENT(IN) :: filter
       END FUNCTION h5zunregister_c
    END INTERFACE
    hdferr = h5zunregister_c(filter)
  END SUBROUTINE h5zunregister_f

!****s* H5Z/h5zfilter_avail_f
! NAME
!  h5zfilter_avail_f
!
! PURPOSE
!  Queries if filter is available
!
! INPUTS
!  filter 	 - Filter; may be one of the following:
!                   H5Z_FILTER_DEFLATE_F
!                   H5Z_FILTER_SZIP_F
!                   H5Z_FILTER_NBIT_F
!                   H5Z_FILTER_SCALEOFFSET_F
!                   H5Z_FILTER_SHUFFLE_F
!                   H5Z_FILTER_FLETCHER32_F
! OUTPUTS
!  status 	 - Flag; .TRUE. if filter is available,
!                  .FALSE. otherwise
!  hdferr:	 - Error code
!                   Success:  0
!                   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
! SOURCE
  SUBROUTINE h5zfilter_avail_f(filter, status, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    LOGICAL, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!****s* H5Z/h5zget_filter_info_f
!
! NAME
!  h5zget_filter_info_f
!
! PURPOSE
!  Queries if filter has its encoder and/or decoder
!  available
!
! INPUTS
!  filter 	 - Filter; may be one of the following:
!                   H5Z_FILTER_DEFLATE_F
!                   H5Z_FILTER_SZIP_F
!                   H5Z_FILTER_NBIT_F
!                   H5Z_FILTER_SCALEOFFSET_F
!                   H5Z_FILTER_SHUFFLE_F
!                   H5Z_FILTER_FLETCHER32_Ffilter
! OUTPUTS
!  config_flags  - Flag, indicates if filter has its encoder 
!                  and/or decoder available, possibly containing the
!                  following values:
!                     H5Z_FILTER_ENCODE_ENABLED_F
!                     H5Z_FILTER_DECODE_ENABLED_F
!  hdferr:	 - Error code
!                   Success:  0
!                   Failure: -1
!
! AUTHOR
!  Nat Furrer and James Laird
!  June 16, 2004
! SOURCE
  SUBROUTINE h5zget_filter_info_f(filter, config_flags, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    INTEGER, INTENT(OUT) :: config_flags
    INTEGER, INTENT(OUT) :: hdferr
!*****

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





