!****h* root/fortran/test/tH5E.f90
!
! NAME
!  tH5E.f90
!
! FUNCTION
!  Basic testing of Fortran H5E APIs.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
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
!  Tests the H5D APIs functionalities of:
!   h5eprint_f
!
! CONTAINS SUBROUTINES
!  error_report_test
!
!*****
!
MODULE TH5E

CONTAINS

    SUBROUTINE error_report_test(cleanup, total_error)

!   This subroutine tests following functionalities: h5eprint_f

   USE HDF5 ! This module contains all necessary modules
   USE TH5_MISC

     IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(INOUT) :: total_error

     CHARACTER(LEN=6), PARAMETER :: filename = "etestf" ! File name
     CHARACTER(LEN=80) :: fix_filename
     CHARACTER(LEN=8), PARAMETER :: err_filename = "err_file"! Error output file
     CHARACTER(LEN=80)  :: fix_err_filename



     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: grp_id        ! Group identifier
     INTEGER :: error, tmp_error, err_flag

     err_flag = 0
     CALL h5eset_auto_f(err_flag, error)
     CALL check("h5eprint_f",error, total_error)
     !
     ! Create a new file using default properties.
     !
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
     CALL check("h5fcreate_f",error,total_error)

     !
     ! Try to open non-existing group in the file.
     ! Error message should go to the err_file_name file.
     !
     CALL h5gopen_f(file_id, "Doesnotexist1", grp_id, tmp_error)
          CALL h5_fixname_f(err_filename, fix_err_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
     CALL h5eprint_f(error, fix_err_filename)
     CALL h5gopen_f(file_id, "Doesnotexist2", grp_id, tmp_error)
     CALL h5eprint_f(error, fix_err_filename)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f",error,total_error)

          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          if(cleanup) CALL h5_cleanup_f(err_filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
     RETURN
     END SUBROUTINE error_report_test

END MODULE TH5E

