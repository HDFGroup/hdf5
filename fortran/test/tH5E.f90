! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
!   access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

    SUBROUTINE error_report_test(total_error)

!This subroutine tests following functionalities: h5eprint_f

   USE HDF5 ! This module contains all necessary modules 

     IMPLICIT NONE
     INTEGER, INTENT(OUT) :: total_error 

     CHARACTER(LEN=9), PARAMETER :: filename = "etestf.h5" ! File name
     CHARACTER(LEN=12), PARAMETER :: err_file_name = "err_file.tmp"! Error output file
          
          

     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: grp_id        ! Group identifier
     INTEGER :: error, tmp_error, err_flag
      
     err_flag = 0 
     CALL h5eset_auto_f(err_flag, error)
     CALL check("h5eprint_f",error, total_error)
     !
     ! Create a new file using default properties.
     ! 
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
     CALL check("h5fcreate_f",error,total_error)
     
     !
     ! Try to open non-existing group in the file.
     ! Error message should go to the err_file_name file.
     !
     CALL h5gopen_f(file_id, "Doesnotexist1", grp_id, tmp_error)
     CALL h5eprint_f(error, err_file_name)
     CALL h5gopen_f(file_id, "Doesnotexist2", grp_id, tmp_error)
     CALL h5eprint_f(error, err_file_name)
    
     ! 
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f",error,total_error)

     RETURN
     END SUBROUTINE error_report_test
