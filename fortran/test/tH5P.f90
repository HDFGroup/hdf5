     
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
!
    SUBROUTINE external_test(cleanup, total_error)

!   This subroutine tests following functionalities: 
!   h5pset_external_f,  h5pget_external_count_f,
!   h5pget_external_f

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(OUT) :: total_error 

     CHARACTER(LEN=8), PARAMETER :: filename = "external"
     CHARACTER(LEN=80) :: fix_filename
     INTEGER(HID_T) :: file_id     
     INTEGER(HID_T) :: plist_id     
     INTEGER(HID_T) :: space_id     
     INTEGER(HID_T) :: dataset_id
     INTEGER(HSIZE_T), DIMENSION(1) :: cur_size !data space current size
     INTEGER(HSIZE_T), DIMENSION(1) :: max_size !data space maximum size
     CHARACTER(LEN=256) :: name !external file name
     INTEGER :: file_offset !external file offset
     INTEGER(HSIZE_T) :: file_size   !sizeof external file segment
     INTEGER :: error !error code
     INTEGER(SIZE_T) :: int_size !size of integer
     INTEGER(HSIZE_T) :: file_bytes !Number of bytes reserved 
                                   !in the file for the data
     INTEGER :: RANK = 1 !dataset rank
     INTEGER :: count !number of external files for the
                      !specified dataset
     INTEGER(SIZE_T) :: namesize


     !
     !Create file "external.h5" using default properties.
     ! 
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
     CALL check("h5fcreate_f",error,total_error)

     CALL h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
     CALL check("h5pcreate_f",error,total_error)
     cur_size(1) =100
     max_size(1) = 100;
     call h5tget_size_f(H5T_NATIVE_INTEGER, int_size, error)
     CALL check("h5tget_size_f",error,total_error)
     file_size = int_size * max_size(1);     
     CALL h5pset_external_f(plist_id, "ext1.data", 0, file_size, error)
     CALL check("h5pset_external_f",error,total_error)
     CALL h5screate_simple_f(RANK, cur_size, space_id, error, max_size)
     CALL check("h5screate_simple_f", error, total_error)
     CALL h5dcreate_f(file_id, "dset1", H5T_NATIVE_INTEGER, space_id, &
                           dataset_id, error, plist_id)
     CALL check("h5dcreate_f", error, total_error)
     
     CALL h5dclose_f(dataset_id, error)
     CALL check("h5dclose_f", error, total_error)
     CALL h5pclose_f(plist_id, error)
     CALL check("h5pclose_f", error, total_error)
     CALL h5sclose_f(space_id, error)
     CALL check("h5sclose_f", error, total_error)
     ! Read dataset creation information 
     CALL h5dopen_f(file_id, "dset1", dataset_id, error)
     CALL check("h5dopen_f",error,total_error)
    
     CALL h5dget_create_plist_f(dataset_id, plist_id, error)
     CALL check("h5dget_create_plist_f",error,total_error)
     CALL h5pget_external_count_f(plist_id, count, error)
     CALL check("h5pget_external_count_f",error,total_error)
     if(count .ne. 1 ) then
         write (*,*) "got external_count is not correct"
         total_error = total_error + 1
     end if
     namesize = 10
     CALL h5pget_external_f(plist_id, 0, namesize, name, file_offset, &
                            file_bytes, error)
     CALL check("h5pget_external_f",error,total_error)
     if(file_offset .ne. 0 ) then
         write (*,*) "got external file offset is not correct"
         total_error = total_error + 1
     end if
     if(file_bytes .ne. file_size ) then
         write (*,*) "got external file size is not correct"
         total_error = total_error + 1
     end if
 
     CALL h5dclose_f(dataset_id, error)
     CALL check("h5dclose_f", error, total_error)
     CALL h5pclose_f(plist_id, error)
     CALL check("h5pclose_f", error, total_error)
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f", error, total_error)


          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
     RETURN
     END SUBROUTINE external_test
    
