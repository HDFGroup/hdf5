!****h* ROBODoc/H5R
!
! NAME
!  MODULE H5R
!
! FILE
!  fortran/src/H5Rff.f90
!
! PURPOSE
!  This file contains Fortran interfaces for H5R functions. It includes
!  all the functions that are independent on whether the Fortran 2003 functions
!  are enabled or disabled.
!
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
!  If you add a new H5R function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5R
  USE H5GLOBAL

  ! If you change the value of these parameters, do not forget to change corresponding
  ! values in the H5f90.h file.
  !        INTEGER, PARAMETER :: REF_OBJ_BUF_LEN = 2
  !        INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3
  !
  !        TYPE hobj_ref_t_f
  !             INTEGER ref(REF_OBJ_BUF_LEN)
  !        END TYPE
  !
  !        TYPE hdset_reg_ref_t_f
  !             INTEGER ref(REF_REG_BUF_LEN)
  !        END TYPE
  !

  INTERFACE h5rget_object_type_f

     MODULE PROCEDURE h5rget_object_type_obj_f

  END INTERFACE

CONTAINS

!****s* H5R/h5rget_object_type_obj_f
!
! NAME
!  h5rget_object_type_obj_f
!
! PURPOSE
!  Retrieves the type of object that an object reference points to.
!
! INPUTS
!  dset_id 	 - identifier of the dataset containing
!                  reference to the objects
!  ref 	         - reference to open
! OUTPUTS
!  obj_type 	 - object_type, possible values:
!                    H5G_UNKNOWN_F
!                    H5G_GROUP_F 
!                    H5G_DATASET_F
!                    H5G_TYPE_F
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! NOTES
!  This is a module procedure for the h5rget_object_type_f
!  subroutine.
! SOURCE
  SUBROUTINE h5rget_object_type_obj_f(dset_id, ref, obj_type, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    TYPE(hobj_ref_t_f), INTENT(IN) :: ref   ! Object reference
    INTEGER, INTENT(OUT) :: obj_type     ! Object type
                                         !  H5G_UNKNOWN_F
                                         !  H5G_GROUP_F
                                         !  H5G_DATASET_F
                                         !  H5G_TYPE_F
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
!*****
    INTEGER(HADDR_T) :: ref_f          ! Local buffer to pass reference

    INTERFACE
       INTEGER FUNCTION h5rget_object_type_obj_c(dset_id, ref_f, obj_type)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RGET_OBJECT_TYPE_OBJ_C':: h5rget_object_type_obj_c
         !DEC$ENDIF
         !              INTEGER, PARAMETER :: REF_OBJ_BUF_LEN = 2
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HADDR_T) :: ref_f
         INTEGER, INTENT(OUT) :: obj_type
       END FUNCTION h5rget_object_type_obj_c
    END INTERFACE

    ref_f = ref%ref
    hdferr = h5rget_object_type_obj_c(dset_id, ref_f, obj_type )

  END SUBROUTINE h5rget_object_type_obj_f

END MODULE H5R
