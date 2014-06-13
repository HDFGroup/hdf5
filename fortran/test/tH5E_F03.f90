!****h* root/fortran/test/tH5E_F03.f90
!
! NAME
!  tH5E_F03.f90
!
! FUNCTION
!  Test FORTRAN HDF5 H5E APIs which are dependent on FORTRAN 2003
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
! USES
!  liter_cb_mod
!
! CONTAINS SUBROUTINES
!  test_error
!
!*****

! *****************************************
! ***        H 5 E   T E S T S
! *****************************************
MODULE test_my_hdf5_error_handler


CONTAINS

!/****************************************************************
!**
!**  my_hdf5_error_handler: Custom error callback routine.
!**
!****************************************************************/

    INTEGER FUNCTION my_hdf5_error_handler(estack_id, data_inout) bind(C)

    ! This error function handle works with only version 2 error stack

    USE HDF5
    USE ISO_C_BINDING
    IMPLICIT NONE

    ! estack_id is always passed from C as: H5E_DEFAULT
    INTEGER(HID_T) :: estack_id
    ! data that was registered with H5Eset_auto_f
    INTEGER :: data_inout

    PRINT*, " "
    PRINT*, " Subtest: H5Eset_auto_f custom error message with callback, WITH DATA"
    PRINT*, "         -This message should be written to standard out-  "
    PRINT*, "          Data Values Passed In =", data_inout
    PRINT*, " "

    data_inout = 10*data_inout

    my_hdf5_error_handler = 1 ! this is not used by the C routine

  END FUNCTION my_hdf5_error_handler

  INTEGER FUNCTION my_hdf5_error_handler_nodata(estack_id, data_inout) bind(C)

    ! This error function handle works with only version 2 error stack

    USE HDF5
    USE ISO_C_BINDING
    IMPLICIT NONE

    ! estack_id is always passed from C as: H5E_DEFAULT
    INTEGER(HID_T) :: estack_id
    ! data that was registered with H5Eset_auto_f
    TYPE(C_PTR) :: data_inout

    PRINT*, " "
    PRINT*, " Subtest: H5Eset_auto_f custom error message with callback, NO DATA"
    PRINT*, "         -This message should be written to standard out-  "
    PRINT*, " "

    my_hdf5_error_handler_nodata = 1 ! this is not used by the C routine

  END FUNCTION my_hdf5_error_handler_nodata

END MODULE test_my_hdf5_error_handler



MODULE TH5E_F03

CONTAINS

SUBROUTINE test_error(total_error)

  USE HDF5 
  USE TH5_MISC
  USE ISO_C_BINDING
  USE test_my_hdf5_error_handler

  IMPLICIT NONE

  INTEGER(hid_t), PARAMETER :: FAKE_ID = -1
  INTEGER :: total_error
  INTEGER(hid_t) :: file
  INTEGER(hid_t) :: dataset, space
  INTEGER(hsize_t), DIMENSION(1:2) :: dims
  INTEGER :: error
  INTEGER, DIMENSION(:), POINTER :: ptr_data
  INTEGER, TARGET :: my_hdf5_error_handler_data
  TYPE(C_PTR) :: f_ptr
  TYPE(C_FUNPTR) :: func

  TYPE(C_PTR), TARGET :: f_ptr1

  INTEGER, DIMENSION(1:1) :: array_shape

  my_hdf5_error_handler_data = 99
  CALL h5fcreate_f("terror.h5", H5F_ACC_TRUNC_F, file, error)
  CALL check("h5fcreate_f", error, total_error)

  ! Create the data space
  dims(1) = 10
  dims(2) = 20
  CALL H5Screate_simple_f(2, dims, space, error)
  CALL check("h5screate_simple_f", error, total_error)

  ! ** SET THE CUSTOMIZED PRINTING OF ERROR STACK **

  ! set the customized error handling routine
  func = c_funloc(my_hdf5_error_handler)

  ! set the data sent to the customized routine
  f_ptr = c_loc(my_hdf5_error_handler_data)

  ! turn on automatic printing, and use a custom error routine with input data
  CALL H5Eset_auto_f(1, error, H5E_DEFAULT_F, func, f_ptr)

  ! Create the erring dataset
  CALL h5dcreate_f(FAKE_ID,"a_dataset",H5T_NATIVE_INTEGER, space, dataset, error)
  CALL VERIFY("h5dcreate_f", error, -1, total_error)

!!$    CALL VERIFY("H5Eset_auto_f",my_hdf5_error_handler_data(1),10, total_error)
!!$    CALL VERIFY("H5Eset_auto_f",my_hdf5_error_handler_data(2),20, total_error)

!!$  ! Test enabling and disabling default printing
!!$
!!$  CALL H5Eget_auto_f(H5E_DEFAULT_F, func1, f_ptr1, error)
!!$  CALL VERIFY("H5Eget_auto_f", error, 0, total_error)

  !    PRINT*,c_associated(f_ptr1)

  ALLOCATE(ptr_data(1:2))
  ptr_data = 0
  array_shape(1) = 2
  CALL C_F_POINTER(f_ptr1, ptr_data, array_shape)

  !    ptr_data => f_ptr1(1)

  !    PRINT*,ptr_data(1)

!!$    if(old_data != NULL)
!!$	TEST_ERROR;
!!$#ifdef H5_USE_16_API
!!$    if (old_func != (H5E_auto_t)H5Eprint)
!!$	TEST_ERROR;
!!$#else /* H5_USE_16_API */
!!$    if (old_func != (H5E_auto2_t)H5Eprint2)
!!$	TEST_ERROR;
!!$#endif /* H5_USE_16_API */


  ! set the customized error handling routine
  func = c_funloc(my_hdf5_error_handler_nodata)
  ! set the data sent to the customized routine as null
  f_ptr = C_NULL_PTR
  ! turn on automatic printing, and use a custom error routine with no input data
  CALL H5Eset_auto_f(1, error, H5E_DEFAULT_F, func, f_ptr)

  CALL h5dcreate_f(FAKE_ID,"a_dataset",H5T_NATIVE_INTEGER, space, dataset, error)
  CALL VERIFY("h5dcreate_f", error, -1, total_error)


  ! turn on automatic printing with h5eprint_f which prints an error stack in the default manner.

  !    func = c_funloc(h5eprint_f)
  !    CALL H5Eset_auto_f(0, error, H5E_DEFAULT_F, func, C_NULL_PTR)

  CALL H5Eset_auto_f(0, error)
  CALL h5dcreate_f(FAKE_ID,"a_dataset",H5T_NATIVE_INTEGER, space, dataset, error)

  CALL H5Eset_auto_f(1, error)
  CALL h5dcreate_f(FAKE_ID,"a_dataset",H5T_NATIVE_INTEGER, space, dataset, error)

END SUBROUTINE test_error

END MODULE TH5E_F03
