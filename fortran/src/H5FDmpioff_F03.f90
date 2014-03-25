!****h* ROBODoc/H5FDMPIO (F03)
!
! NAME
!  H5FDMPIO_PROVISIONAL
!
! FILE
!  src/fortran/src/H5FDmpioff_F03.f90 
!
! PURPOSE
!  This file contains Fortran interfaces for H5FDMPIO functions needed by
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

MODULE H5FDMPIO_PROVISIONAL

  USE ISO_C_BINDING

  USE H5GLOBAL

  TYPE, BIND(C) :: union_t
     TYPE(C_PTR) :: rbuf
     TYPE(C_PTR) :: wbuf
  END TYPE union_t

  TYPE, BIND(C) :: H5D_rw_multi_t
     INTEGER(HID_T) :: dset_id
     INTEGER(HID_T) :: dset_space_id
     INTEGER(HID_T) :: mem_type_id
     INTEGER(HID_T) :: mem_space_id
     TYPE(union_t)  :: u
  END TYPE H5D_rw_multi_t

CONTAINS


!****s* H5FDMPIO/H5Dread_multi_f
!
! NAME
!  H5Dread_multi_f
!
! PURPOSE
!
! INPUTS
! file_id - file or group id for the location of datasets
! count   - the number of accessing datasets.
! Info    - the array of dataset information and read buffer.
! dxpl_id - dataset transfer property
!
! OUTPUTS
! AUTHOR
!
! SOURCE
  SUBROUTINE H5Dread_multi_f(file_id, dxpl_id, count, info, hdferr)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE

    INTEGER(HID_T),       INTENT(IN)                :: file_id
    INTEGER(SIZE_T),      INTENT(IN)                :: count
    TYPE(H5D_rw_multi_t), INTENT(OUT), DIMENSION(*) :: info
    INTEGER(HID_T),       INTENT(IN)                :: dxpl_id
    INTEGER,              INTENT(OUT)               :: hdferr
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
         INTEGER(SIZE_T),      INTENT(IN)                :: count
         TYPE(H5D_rw_multi_t), INTENT(OUT), DIMENSION(*) :: info
         INTEGER(HID_T),       INTENT(IN)                :: dxpl_id
       END FUNCTION H5Dread_multi_c
    END INTERFACE

    hdferr = H5Dread_multi_c(file_id, dxpl_id, count, info)

  END SUBROUTINE H5Dread_multi_f
  
END MODULE H5FDMPIO_PROVISIONAL
