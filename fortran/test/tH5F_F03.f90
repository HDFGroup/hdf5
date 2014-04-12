!****h* root/fortran/test/tH5F_F03.f90
!
! NAME
!  tH5F_F03.f90
!
! FUNCTION
!  Test FORTRAN HDF5 H5F APIs which are dependent on FORTRAN 2003
!  features. 
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
!  Tests the H5F APIs functionalities of:
!    h5fget_file_image_f
!
! CONTAINS SUBROUTINES
!  test_get_file_image
!
!*****

! *****************************************
! ***        H 5 F   T E S T S
! *****************************************

MODULE TH5F_F03

CONTAINS

SUBROUTINE test_get_file_image(total_error)
  !
  !  Tests the wrapper for h5fget_file_image
  !
  USE HDF5
  USE TH5_MISC 
  USE ISO_C_BINDING

  IMPLICIT NONE

  INTEGER, INTENT(INOUT) :: total_error ! returns error

  CHARACTER(kind=c_char), ALLOCATABLE, DIMENSION(:), TARGET :: file_image_ptr ! Image from file
  CHARACTER(kind=c_char), ALLOCATABLE, DIMENSION(:), TARGET :: image_ptr ! Image from h5fget_file_image_f

  INTEGER, DIMENSION(1:100), TARGET :: data ! Write data
  INTEGER :: i, file_sz
  INTEGER(hid_t) :: file_id = -1  ! File identifier
  INTEGER(hid_t) :: dset_id = -1  ! Dataset identifier
  INTEGER(hid_t) :: space_id = -1 ! Dataspace identifier
  INTEGER(hsize_t), DIMENSION(1:2) :: dims  ! Dataset dimensions
  INTEGER(size_t) :: itmp_a, itmp_b ! General purpose integers
  INTEGER(size_t) :: image_size     ! Size of image
  TYPE(C_PTR) :: f_ptr            ! Pointer
  INTEGER(hid_t) :: fapl          ! File access property
  INTEGER :: error                ! Error flag

  ! Create new properties for file access
  CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
  CALL check("h5pcreate_f", error, total_error)

  ! Set standard I/O driver
  CALL h5pset_fapl_stdio_f(fapl, error)
  CALL check("h5pset_fapl_stdio_f", error, total_error)

  ! Create the file
  CALL h5fcreate_f("tget_file_image.h5", H5F_ACC_TRUNC_F, file_id, error, H5P_DEFAULT_F, fapl)
  CALL check("h5fcreate_f", error, total_error)

  ! Set up data space for new data set 
  dims(1:2) = (/10,10/)
    
  CALL h5screate_simple_f(2, dims,  space_id, error)
  CALL check("h5screate_simple_f", error, total_error)

  ! Create a dataset 
  CALL h5dcreate_f(file_id, "dset 0", H5T_NATIVE_INTEGER, space_id, dset_id, error)
  CALL check("h5dcreate_f", error, total_error)

  ! Write some data to the data set 
  DO i = 1, 100
     data(i) = i
  ENDDO
  
  f_ptr = C_LOC(data(1))
  CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, error)
  CALL check("h5dwrite_f",error, total_error)

  ! Flush the file
  CALL h5fflush_f(file_id, H5F_SCOPE_GLOBAL_F, error)
  CALL check("h5fflush_f",error, total_error)

  ! Open the test file using standard I/O calls 
  OPEN(UNIT=10,FILE='tget_file_image.h5', ACCESS='STREAM')
  ! Get the size of the test file
  !
  ! Since we use the eoa to calculate the image size, the file size
  ! may be larger.  This is OK, as long as (in this specialized instance)
  ! the remainder of the file is all '\0's.
  !
  ! With latest mods to truncate call in core file drive, 
  ! file size should match image size; get the file size 
  INQUIRE(UNIT=10, SIZE=file_sz)
  CLOSE(UNIT=10)

  ! I. Get buffer size needed to hold the buffer

  !  A. Preferred way to get the size
  f_ptr = C_NULL_PTR
  CALL h5fget_file_image_f(file_id, f_ptr, INT(0, size_t), error, image_size)
  CALL check("h5fget_file_image_f",error, total_error)
  CALL verify("h5fget_file_image_f", file_sz, INT(image_size), total_error)

  !  B. f_ptr set to point to an incorrect buffer, should pass anyway
  f_ptr = C_LOC(data(1))
  itmp_a = 1
  CALL h5fget_file_image_f(file_id, f_ptr, itmp_a, error, image_size)
  CALL check("h5fget_file_image_f",error, total_error)
  CALL VERIFY("h5fget_file_image_f", INT(itmp_a), 1, total_error) ! Routine should not change the value
  CALL VERIFY("h5fget_file_image_f", file_sz, INT(image_size), total_error)

  ! Allocate a buffer of the appropriate size 
  ALLOCATE(image_ptr(1:image_size))

  ! Load the image of the file into the buffer
  f_ptr = C_LOC(image_ptr(1)(1:1))
  CALL h5fget_file_image_f(file_id, f_ptr, image_size, error)
  CALL check("h5fget_file_image_f",error, total_error)

  ! Close dset and space 
  CALL h5dclose_f(dset_id, error)
  CALL check("h5dclose_f", error, total_error)
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f", error, total_error)
  ! Close the test file
  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f",error, total_error)

  ! Allocate a buffer for the test file image
  ALLOCATE(file_image_ptr(1:image_size))

  ! Open the test file using standard I/O calls 
  OPEN(UNIT=10,FILE='tget_file_image.h5', FORM='UNFORMATTED', ACCESS='STREAM')

  ! Read the test file from disk into the buffer
  DO i = 1, image_size
     READ(10) file_image_ptr(i)
  ENDDO

  CLOSE(10)

  ! verify the file and the image contain the same data
  DO i = 1, image_size
     ! convert one byte to an unsigned integer
     IF( ICHAR(file_image_ptr(i)) .NE. ICHAR(image_ptr(i)))THEN
        total_error = total_error + 1
        EXIT
     ENDIF
  ENDDO

  ! release resources
  DEALLOCATE(file_image_ptr,image_ptr)

END SUBROUTINE test_get_file_image

END MODULE TH5F_F03
