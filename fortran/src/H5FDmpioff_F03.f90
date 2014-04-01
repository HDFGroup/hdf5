!****h* ROBODoc/H5FDMPIO (F03)
!
! NAME
!  H5FDMPIO_PROVISIONAL
!
! FILE
!  src/fortran/src/H5FDmpioff_F03.f90 
!
! PURPOSE
!  This file contains Fortran 2003 interfaces for H5FDMPIO functions needed by
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
!  If you add a new function then you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5FDMPIO_PROVISIONAL

  USE ISO_C_BINDING
  USE H5GLOBAL

! Derived data type matching the C structure.

  TYPE, BIND(C) :: H5D_rw_multi_t
     INTEGER(HID_T) :: dset_id
     INTEGER(HID_T) :: dset_space_id
     INTEGER(HID_T) :: mem_type_id
     INTEGER(HID_T) :: mem_space_id
     TYPE(C_PTR)    :: buf
  END TYPE H5D_rw_multi_t

CONTAINS

!****s* H5FDMPIO/H5Dread_multi_f
!
! NAME
!  H5Dread_multi_f
!
! PURPOSE
!  Reads data from a file to memory buffers for multiple datasets
!
! INPUTS
!  file_id - file or group id for the location of datasets.
!  dxpl_id - dataset transfer property.
!  count   - the number of accessing datasets.
!
! OUTPUTS
!  info    - the array of dataset information and read buffer.
! AUTHOR
!  M. Scot Breitenfeld
!  March 25, 2014
!
! SOURCE
  SUBROUTINE H5Dread_multi_f(file_id, dxpl_id, count, info, hdferr)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE

    INTEGER(HID_T),       INTENT(IN)                      :: file_id
    INTEGER(SIZE_T),      INTENT(IN)                      :: count
    TYPE(H5D_rw_multi_t), INTENT(OUT), DIMENSION(1:count) :: info
    INTEGER(HID_T),       INTENT(IN)                      :: dxpl_id
    INTEGER,              INTENT(OUT)                     :: hdferr
!*****

    INTERFACE
       INTEGER FUNCTION H5Dread_multi_c(file_id, dxpl_id, count, info)
         USE H5GLOBAL
         USE, INTRINSIC :: ISO_C_BINDING
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_MULTI_C':: h5dread_multi_c
         !DEC$ENDIF
         IMPORT :: H5D_rw_multi_t
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HID_T),       INTENT(IN)                :: dxpl_id
         INTEGER(SIZE_T),      INTENT(IN)                :: count
         TYPE(H5D_rw_multi_t), INTENT(OUT), DIMENSION(1:count) :: info
       END FUNCTION H5Dread_multi_c
    END INTERFACE

    hdferr = H5Dread_multi_c(file_id, dxpl_id, count, info)

  END SUBROUTINE H5Dread_multi_f

!****s* H5FDMPIO/H5Dwrite_multi_f
!
! NAME
!  H5Dwrite_multi_f
!
! PURPOSE
!   Writes data in memory to a file for multiple datasets
!
! INPUTS
!  file_id - file or group id for the location of datasets,
!  count   - the number of accessing datasets.
!  dxpl_id - dataset transfer property.
!
! OUTPUTS
!  Info    - the array of dataset information and write buffer.
! AUTHOR
!  M. Scot Breitenfeld
!  March 25, 2014
!
! SOURCE
  SUBROUTINE H5Dwrite_multi_f(file_id, dxpl_id, count, info, hdferr)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE

    INTEGER(HID_T),       INTENT(IN)                     :: file_id
    INTEGER(HID_T),       INTENT(IN)                     :: dxpl_id
    INTEGER(SIZE_T),      INTENT(IN)                     :: count
    TYPE(H5D_rw_multi_t), INTENT(IN), DIMENSION(1:count) :: info
    INTEGER,              INTENT(OUT)                    :: hdferr
!*****

    INTERFACE
       INTEGER FUNCTION H5Dwrite_multi_c(file_id, dxpl_id, count, info)
         USE H5GLOBAL
         USE, INTRINSIC :: ISO_C_BINDING
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_MULTI_C':: h5dwrite_multi_c
         !DEC$ENDIF
         IMPORT :: H5D_rw_multi_t
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HID_T),       INTENT(IN)                :: dxpl_id
         INTEGER(SIZE_T),      INTENT(IN)                :: count
         TYPE(H5D_rw_multi_t), INTENT(OUT), DIMENSION(1:count) :: info
       END FUNCTION H5Dwrite_multi_c
    END INTERFACE

    hdferr = H5Dwrite_multi_c(file_id, dxpl_id, count, info)

  END SUBROUTINE H5Dwrite_multi_f
  
END MODULE H5FDMPIO_PROVISIONAL
