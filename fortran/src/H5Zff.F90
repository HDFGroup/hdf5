!****h* ROBODoc/H5Z
!
! NAME
!  MODULE H5Z
!
! PURPOSE
!  This file contains Fortran interfaces for H5Z functions. It includes
!  all the functions that are independent on whether the Fortran 2003 functions
!  are enabled or disabled.
!
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  Copyright by The HDF Group.                                               *
!  Copyright by the Board of Trustees of the University of Illinois.         *
!  All rights reserved.                                                      *
!  *
!  This file is part of HDF5.  The full HDF5 copyright notice, including     *
!  terms governing use, modification, and redistribution, is contained in    *
!  the files COPYING and Copyright.html.  COPYING can be found at the root   *
!  of the source code distribution tree; Copyright.html can be found at the  *
!  root level of an installed copy of the electronic HDF5 document set and   *
!  is linked from the top-level documents page.  It can also be found at     *
!  http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!  access to either file, you may request a copy from help@hdfgroup.org.     *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES
!                         *** IMPORTANT ***
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
!  filter - filter; may have one of the following values:
!            H5Z_FILTER_DEFLATE_F
!            H5Z_FILTER_SZIP_F
!            H5Z_FILTER_NBIT_F
!            H5Z_FILTER_SCALEOFFSET_F
!            H5Z_FILTER_SHUFFLE_F
!            H5Z_FILTER_FLETCHER32_F
!            
! OUTPUTS
!  hdferr - error code
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
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5zunregister_c (filter)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ZUNREGISTER_C':: h5zunregister_c
         !DEC$ENDIF
         INTEGER, INTENT(IN) :: filter
       END FUNCTION h5zunregister_c
    END INTERFACE
    hdferr = h5zunregister_c (filter)
  END SUBROUTINE h5zunregister_f

!****s* H5Z/h5zfilter_avail_f
! NAME
!  h5zfilter_avail_f
!
! PURPOSE
!  Queries if filter is available
!
! INPUTS
!  filter 	 - filter
! OUTPUTS
!  status 	 - status; .TRUE. if filter is available,
!                  .FALSE. otherwise
!  hdferr:	 - error code
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
    INTEGER, INTENT(IN)  :: filter      ! Filter; may be one of the following:
                                        !   H5Z_FILTER_DEFLATE_F
                                        !   H5Z_FILTER_SZIP_F
                                        !   H5Z_FILTER_NBIT_F
                                        !   H5Z_FILTER_SCALEOFFSET_F
                                        !   H5Z_FILTER_SHUFFLE_F
                                        !   H5Z_FILTER_FLETCHER32_F
    LOGICAL, INTENT(OUT) :: status      ! Flag, idicates if filter
                                        ! is availble  not ( TRUE or
                                        ! FALSE)
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
!*****
    INTEGER :: flag                     ! "TRUE/FALSE/ERROR from C"

    INTERFACE
       INTEGER FUNCTION h5zfilter_avail_c(filter, flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ZFILTER_AVAIL_C'::h5zfilter_avail_c
         !DEC$ENDIF
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
!  filter 	 - filter
! OUTPUTS
!  config_flags  - Bit vector possibly containing the
!                  following values:
!                     H5Z_FILTER_ENCODE_ENABLED_F
!                     H5Z_FILTER_DECODE_ENABLED_F
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
!
! AUTHOR
!  Nat Furrer and James Laird
!  June 16, 2004
! SOURCE
  SUBROUTINE h5zget_filter_info_f(filter, config_flags, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter      ! Filter; may be one of the following:
                                        !   H5Z_FILTER_DEFLATE_F
                                        !   H5Z_FILTER_SZIP_F
                                        !   H5Z_FILTER_NBIT_F
                                        !   H5Z_FILTER_SCALEOFFSET_F
                                        !   H5Z_FILTER_SHUFFLE_F
                                        !   H5Z_FILTER_FLETCHER32_F
    INTEGER, INTENT(OUT) :: config_flags! Flag, indicates if filter
                                        ! has its encoder and/or decoder
                                        ! available
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
!*****

    INTERFACE
       INTEGER FUNCTION h5zget_filter_info_c(filter, config_flags)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ZGET_FILTER_INFO_C'::h5zget_filter_info_c
         !DEC$ENDIF
         INTEGER, INTENT(IN) :: filter
         INTEGER, INTENT(OUT) :: config_flags
       END FUNCTION h5zget_filter_info_c
    END INTERFACE

    hdferr = h5zget_filter_info_c(filter, config_flags)

  END SUBROUTINE h5zget_filter_info_f

END MODULE H5Z





