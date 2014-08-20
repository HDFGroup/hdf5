!****h* ROBODoc/H5FDMPIO
!
! NAME
!  MODULE H5FDMPIO
!
! PURPOSE
!  This file contains Fortran interfaces for H5P functions needed by
!  parallel MPI programs.
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
!  If you add a new H5P function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5FDMPIO
  USE H5GLOBAL
CONTAINS

!****s* H5FDMPIO/h5pset_fapl_mpio_f
!
! NAME
!  h5pset_fapl_mpio_f
!
! PURPOSE
!  Stores MPI IO communicator information to the file
!  access property list.
!
! INPUTS
!  prp_id 	 - file access property list identifier
!  comm 	 - MPI-2 communicator
!  info 	 - MPI-2 info object
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  November, 2000
!
! SOURCE
  SUBROUTINE h5pset_fapl_mpio_f(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    INTEGER, INTENT(IN) :: comm ! MPI communicator to be used for file open
                                ! as defined in MPI_FILE_OPEN of MPI-2
    INTEGER, INTENT(IN) :: info ! MPI info object to be used for file open
                                ! as defined in MPI_FILE_OPEN of MPI-2
    INTEGER, INTENT(OUT) :: hdferr ! Error code
!*****
    INTEGER, EXTERNAL :: h5pset_fapl_mpio_c
    hdferr = h5pset_fapl_mpio_c(prp_id, comm, info)
  END SUBROUTINE h5pset_fapl_mpio_f

!****s* H5FDMPIO/h5pget_fapl_mpio_f
!
! NAME
!  h5pget_fapl_mpio_f
!
! PURPOSE
!  Returns MPI communicator information.
!
! INPUTS
!  prp_id 	 - file access property list identifier
! OUTPUTS
!  comm 	 - MPI-2 communicator
!  info 	 - MPI-2 info object
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  November, 2000
!
! SOURCE
  SUBROUTINE h5pget_fapl_mpio_f(prp_id, comm, info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    INTEGER, INTENT(OUT) :: comm ! buffer to return communicator
    INTEGER, INTENT(OUT) :: info ! buffer to return info object
                                 ! as defined in MPI_FILE_OPEN of MPI-2
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
!*****
    INTEGER, EXTERNAL :: h5pget_fapl_mpio_c
    hdferr = h5pget_fapl_mpio_c(prp_id, comm, info)
  END SUBROUTINE h5pget_fapl_mpio_f

!****s* H5FDMPIO/h5pset_dxpl_mpio_f
!
! NAME
!  h5pset_dxpl_mpio_f
!
! PURPOSE
!  Sets data transfer mode.
!
! INPUTS
!  prp_id 	  - data transfer property list identifier
!  data_xfer_mode - transfer mode; possible values are:
!                     H5FD_MPIO_INDEPENDENT_F
!                     H5FD_MPIO_COLLECTIVE_F
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  November, 2000
!
! SOURCE
  SUBROUTINE h5pset_dxpl_mpio_f(prp_id, data_xfer_mode, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    INTEGER, INTENT(IN) :: data_xfer_mode ! Data transfer mode. Possible values are:
                                          ! H5FD_MPIO_INDEPENDENT_F
                                          ! H5FD_MPIO_COLLECTIVE_F
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER, EXTERNAL :: h5pset_dxpl_mpio_c
    hdferr = h5pset_dxpl_mpio_c(prp_id, data_xfer_mode)
  END SUBROUTINE h5pset_dxpl_mpio_f

!****s* H5FDMPIO/h5pget_dxpl_mpio_f
!
! NAME
!  h5pget_dxpl_mpio_f
!
! PURPOSE
!  Returns the data transfer mode.
!
! INPUTS
!  prp_id 	 - data transfer property list identifier
! OUTPUTS
!  data_xfer_mode- transfer mode; possible values are:
!                     H5FD_MPIO_INDEPENDENT_F
!                     H5FD_MPIO_COLLECTIVE_F
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  November, 2000
!
! SOURCE
  SUBROUTINE h5pget_dxpl_mpio_f(prp_id, data_xfer_mode, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id   ! Property list identifier
    INTEGER, INTENT(OUT) :: data_xfer_mode ! Data transfer mode. Possible values are:
                                           ! H5FD_MPIO_INDEPENDENT_F
                                           ! H5FD_MPIO_COLLECTIVE_F
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER, EXTERNAL :: h5pget_dxpl_mpio_c
    hdferr = h5pget_dxpl_mpio_c(prp_id, data_xfer_mode)
  END SUBROUTINE h5pget_dxpl_mpio_f

!****s* H5P/h5pget_mpio_actual_io_mode_f
! NAME
!  h5pget_mpio_actual_io_mode_f
!
! PURPOSE
!  Retrieves the type of I/O that HDF5 actually performed on the last 
!  parallel I/O call. This is not necessarily the type of I/O requested. 
!
! INPUTS
!  dxpl_id        - Dataset transfer property list identifier.
! OUTPUTS
!  actual_io_mode - The type of I/O performed by this process.
!  hdferr         - Returns 0 if successful and -1 if fails.
!
! AUTHOR
!  M. Scot Breitenfeld
!  July 27, 2012
!
! HISTORY
!
! Fortran90 Interface:
  SUBROUTINE h5pget_mpio_actual_io_mode_f(dxpl_id, actual_io_mode, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: dxpl_id
    INTEGER       , INTENT(OUT) :: actual_io_mode
    INTEGER       , INTENT(OUT) :: hdferr
!***** 
    INTERFACE
       INTEGER FUNCTION h5pget_mpio_actual_io_mode_c(dxpl_id, actual_io_mode)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_MPIO_ACTUAL_IO_MODE_C'::h5pget_mpio_actual_io_mode_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN)  :: dxpl_id
         INTEGER       , INTENT(OUT) :: actual_io_mode
       END FUNCTION h5pget_mpio_actual_io_mode_c
    END INTERFACE

    actual_io_mode = -1

    hdferr = h5pget_mpio_actual_io_mode_c(dxpl_id, actual_io_mode)

  END SUBROUTINE h5pget_mpio_actual_io_mode_f

END MODULE H5FDMPIO
