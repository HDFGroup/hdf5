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
! USES
!  liter_cb_mod
!
! CONTAINS SUBROUTINES
!  test_error
!
!*****

#include <H5config_f.inc>

! *****************************************
! ***        H 5 E   T E S T S
! *****************************************
MODULE test_my_hdf5_error_handler

  USE HDF5
  USE TH5_MISC
  USE TH5_MISC_GEN

CONTAINS

!***************************************************************
!**
!**  my_hdf5_error_handler: Custom error callback routine.
!**
!***************************************************************

    INTEGER FUNCTION my_hdf5_error_handler(estack_id, data_inout) bind(C)

    ! This error function handle works with only version 2 error stack

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

  !-------------------------------------------------------------------------
  ! Function:    custom_print_cb
  !
  ! Purpose:     Callback function to print error stack in customized way.
  !
  !-------------------------------------------------------------------------
  !
  INTEGER(C_INT) FUNCTION custom_print_cb(n, err_desc, op_data) BIND(C)

    ! This error function handle works with only version 2 error stack

    IMPLICIT NONE

    INTEGER(SIZE_T), PARAMETER :: MSG_SIZE = 64

    INTEGER(C_INT)    :: n
    TYPE(h5e_error_t) :: err_desc
    TYPE(C_PTR)       :: op_data

    CHARACTER(LEN=MSG_SIZE) :: maj
    CHARACTER(LEN=MSG_SIZE) :: minn
    CHARACTER(LEN=MSG_SIZE) :: cls
    INTEGER :: indent = 4
    INTEGER(SIZE_T) :: size
    INTEGER :: msg_type

    INTEGER :: error

    TYPE(C_PTR) :: f_ptr

    CALL H5Eget_class_name_f(err_desc%cls_id, cls, error)
    IF(error .LT.0)THEN
       custom_print_cb = -1
       RETURN
    ENDIF

    IF(TRIM(cls).NE."Custom error class")THEN
       custom_print_cb = -1
       RETURN
    ENDIF

    size = 3
    CALL H5Eget_class_name_f(err_desc%cls_id, cls, error, size)
    IF(error .LT.0)THEN
       custom_print_cb = -1
       RETURN
    ENDIF
    IF(TRIM(cls).NE."Cus")THEN
       custom_print_cb = -1
       RETURN
    ENDIF

    size = 0
    CALL H5Eget_class_name_f(err_desc%cls_id, "", error, size)
    IF(error .LT.0)THEN
       custom_print_cb = -1
       RETURN
    ENDIF
    IF(size.NE.18)THEN
       custom_print_cb = -1
       RETURN
    ENDIF

    size = MSG_SIZE
    CALL H5Eget_msg_f(err_desc%maj_num, msg_type, maj, error, size)
    IF(error .LT.0)THEN
       custom_print_cb = -1
       RETURN
    ENDIF

    CALL h5eget_major_f(err_desc%maj_num, maj, size, error)
    IF("MAJOR MSG".NE.TRIM(maj))THEN
       custom_print_cb = -1
       RETURN
    ENDIF

    IF(error .LT. 0)THEN
       custom_print_cb = -1
       RETURN
    ENDIF

    CALL h5eget_minor_f(err_desc%min_num, minn, error)
    IF(error .LT. 0)THEN
       custom_print_cb = -1
       RETURN
    ENDIF
    IF("MIN MSG".NE.TRIM(minn))THEN
       custom_print_cb = -1
       RETURN
    ENDIF

    custom_print_cb = 0

  END FUNCTION custom_print_cb
#if 0
    FILE     *stream = (FILE *)client_data;

    fprintf(stream, "%*serror #%03d: %s in %s(): line %u\n", indent, "", n, err_desc->file_name,
            err_desc->func_name, err_desc->line);
    fprintf(stream, "%*sclass: %s\n", indent * 2, "", cls);
    fprintf(stream, "%*smajor: %s\n", indent * 2, "", maj);
    fprintf(stream, "%*sminor: %s\n", indent * 2, "", min);

#endif

END MODULE test_my_hdf5_error_handler

MODULE TH5E_F03

  USE ISO_C_BINDING
  USE test_my_hdf5_error_handler

CONTAINS

SUBROUTINE test_error(total_error)

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
  CALL verify("h5dcreate_f", error, -1, total_error)

!!$    CALL verify("H5Eset_auto_f",my_hdf5_error_handler_data(1),10, total_error)
!!$    CALL verify("H5Eset_auto_f",my_hdf5_error_handler_data(2),20, total_error)

!!$  ! Test enabling and disabling default printing
!!$
!!$  CALL H5Eget_auto_f(H5E_DEFAULT_F, func1, f_ptr1, error)
!!$  CALL verify("H5Eget_auto_f", error, 0, total_error)

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
!!$#else  H5_USE_16_API
!!$    if (old_func != (H5E_auto2_t)H5Eprint2)
!!$	TEST_ERROR;
!!$#endif  H5_USE_16_API


  ! set the customized error handling routine
  func = c_funloc(my_hdf5_error_handler_nodata)
  ! set the data sent to the customized routine as null
  f_ptr = C_NULL_PTR
  ! turn on automatic printing, and use a custom error routine with no input data
  CALL H5Eset_auto_f(1, error, H5E_DEFAULT_F, func, f_ptr)

  CALL h5dcreate_f(FAKE_ID,"a_dataset",H5T_NATIVE_INTEGER, space, dataset, error)
  CALL verify("h5dcreate_f", error, -1, total_error)


  ! turn on automatic printing with h5eprint_f which prints an error stack in the default manner.

  !    func = c_funloc(h5eprint_f)
  !    CALL H5Eset_auto_f(0, error, H5E_DEFAULT_F, func, C_NULL_PTR)

  CALL H5Eset_auto_f(0, error)
  CALL h5dcreate_f(FAKE_ID,"a_dataset",H5T_NATIVE_INTEGER, space, dataset, error)

  CALL H5Eset_auto_f(1, error)
  CALL h5dcreate_f(FAKE_ID,"a_dataset",H5T_NATIVE_INTEGER, space, dataset, error)

END SUBROUTINE test_error

SUBROUTINE test_error_stack(total_error)

  IMPLICIT NONE

  INTEGER :: total_error
  INTEGER :: error
  INTEGER(HID_T) :: cls_id, major, minor
  CHARACTER(LEN=18), TARGET :: file
  CHARACTER(LEN=18), TARGET :: func
  INTEGER          , TARGET :: line
  TYPE(C_PTR) :: ptr1, ptr2, ptr3, ptr4

  INTEGER :: msg_type
  CHARACTER(LEN=9) :: maj_mesg = "MAJOR MSG"
  CHARACTER(LEN=7) :: min_mesg = "MIN MSG"
  !file status
  LOGICAL :: status
  CHARACTER(LEN=180) :: chr180
  INTEGER :: idx
  INTEGER(SIZE_T) :: count
  CHARACTER(LEN=64), TARGET :: stderr
  TYPE(C_FUNPTR) :: func_ptr

#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
  CHARACTER(:), ALLOCATABLE :: msg_alloc
#endif

  CHARACTER(LEN=9) :: chr9
  INTEGER(SIZE_T) :: msg_size

  CALL h5eregister_class_f("Custom error class", "H5E_F03", "0.1", cls_id, error)
  CALL check("H5Eregister_class_f", error, total_error)

  CALL H5Ecreate_msg_f(cls_id, H5E_MAJOR_F, maj_mesg, major, error)
  CALL check("H5Ecreate_msg_f", error, total_error)
  CALL H5Ecreate_msg_f(cls_id, H5E_MINOR_F, min_mesg, minor, error)
  CALL check("H5Ecreate_msg_f", error, total_error)

  file = "FILE"//C_NULL_CHAR
  func = "FUNC"//C_NULL_CHAR
  line = 99

  ptr1 = C_LOC(file)
  ptr2 = C_LOC(func)
  ptr3 = C_LOC(line)

  ! push a custom error message onto the default stack
  CALL H5Epush_f(H5E_DEFAULT_F, cls_id, major, minor, "%s ERROR TEXT %s"//C_NEW_LINE, error, &
       ptr1, ptr2, ptr3, &
       arg1=ACHAR(27)//"[31m", arg2=ACHAR(27)//"[0m" )

  CALL check("H5Epush_f", error, total_error)

  CALL h5eget_num_f(H5E_DEFAULT_F, count, error)
  CALL check("h5eget_num_f", error, total_error)
  CALL VERIFY("h5eget_num_f", count, 1_SIZE_T, total_error)

  msg_size = 0
  CALL H5Eget_msg_f(major, msg_type, chr9, error, msg_size)
  CALL check("H5Eget_msg_f", error, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_type, H5E_MAJOR_F, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_size, 9_SIZE_T, total_error)

  ! Check when a shorter buffer length is passed as the msg_size
  msg_size = 3
  CALL H5Eget_msg_f(major, msg_type, chr9, error, msg_size)
  CALL check("H5Eget_msg_f", error, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_type, H5E_MAJOR_F, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_size, 9_SIZE_T, total_error)
  CALL VERIFY("H5Eget_msg_f", TRIM(chr9), maj_mesg(1:3), total_error)

  ! Check when a exact size buffer length is passed as the msg_size
  msg_size = 9
  CALL H5Eget_msg_f(major, msg_type, chr9, error, msg_size)
  CALL check("H5Eget_msg_f", error, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_type, H5E_MAJOR_F, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_size, 9_SIZE_T, total_error)
  CALL VERIFY("H5Eget_msg_f", TRIM(chr9), maj_mesg(1:9), total_error)

  msg_size = 0
  CALL H5Eget_msg_f(minor, msg_type, chr9, error, msg_size)
  CALL check("H5Eget_msg_f", error, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_type, H5E_MINOR_F, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_size, 7_SIZE_T, total_error)

  ! Check when a shorter buffer length is passed as the msg_size
  msg_size = 3
  CALL H5Eget_msg_f(minor, msg_type, chr9, error, msg_size)
  CALL check("H5Eget_msg_f", error, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_type, H5E_MINOR_F, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_size, 7_SIZE_T, total_error)
  CALL VERIFY("H5Eget_msg_f", TRIM(chr9), min_mesg(1:3), total_error)

  ! Check when a larger buffer length is passed as the msg_size
  msg_size = 9
  CALL H5Eget_msg_f(minor, msg_type, chr9, error, msg_size)
  CALL check("H5Eget_msg_f", error, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_type, H5E_MINOR_F, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_size, 7_SIZE_T, total_error)
  CALL VERIFY("H5Eget_msg_f", TRIM(chr9), min_mesg(1:7), total_error)

  ! Check with an allocatable character of the exact size
#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
  msg_size = 0
  CALL H5Eget_msg_f(minor, msg_type, "", error, msg_size)
  CALL check("H5Eget_msg_f", error, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_type, H5E_MINOR_F, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_size, 7_SIZE_T, total_error)

  ALLOCATE(CHARACTER(LEN=msg_size) :: msg_alloc)
  CALL H5Eget_msg_f(minor, msg_type, msg_alloc, error)
  CALL check("H5Eget_msg_f", error, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_type, H5E_MINOR_F, total_error)
  CALL VERIFY("H5Eget_msg_f", msg_alloc, min_mesg, total_error)
#endif

  INQUIRE(file="H5Etest.txt", EXIST=status)
  IF(status)THEN
     OPEN(UNIT=12, FILE="H5Etest.txt", status='old')
     CLOSE(12, STATUS='delete')
  ENDIF

  CALL h5eprint_f(error, "H5Etest.txt")
  CALL check("h5eprint_f", error, total_error)

  INQUIRE(file="H5Etest.txt", EXIST=status)
  IF(.NOT.status)THEN
     CALL check("h5eprint_f", -1, total_error)
  ELSE
     OPEN(UNIT=12, FILE="H5Etest.txt", status='old')

     READ(12,'(A)') chr180
     idx = INDEX(string=chr180,substring="Custom error class")
     IF(idx.EQ.0) CALL check("h5eprint_f1", -1, total_error)
     idx = INDEX(string=chr180,substring="H5E_F03")
     IF(idx.EQ.0) CALL check("h5eprint_f2", -1, total_error)
     idx = INDEX(string=chr180,substring="0.1")
     IF(idx.EQ.0) CALL check("h5eprint_f3", -1, total_error)

     READ(12,'(A)') chr180
     idx = INDEX(string=chr180,substring="FILE")
     IF(idx.EQ.0) CALL check("h5eprint_f4", -1, total_error)
     idx = INDEX(string=chr180,substring="99")
     IF(idx.EQ.0) CALL check("h5eprint_f5", -1, total_error)
     idx = INDEX(string=chr180,substring="FUNC")
     IF(idx.EQ.0) CALL check("h5eprint_f6", -1, total_error)
     idx = INDEX(string=chr180,substring="ERROR TEXT")
     IF(idx.EQ.0) CALL check("h5eprint_f7", -1, total_error)

     READ(12,'()')

     READ(12,"(A)") chr180
     idx = INDEX(string=chr180,substring=maj_mesg)
     IF(idx.EQ.0) CALL check("h5eprint_f", -1, total_error)

     READ(12,"(A)") chr180
     idx = INDEX(string=chr180,substring=min_mesg)
     IF(idx.EQ.0) CALL check("h5eprint_f", -1, total_error)

     CLOSE(12, STATUS='delete')
  ENDIF

  stderr = "** Print error stack in customized way **"//C_NULL_CHAR
  ptr4 = C_LOC(stderr(1:1))
  func_ptr = C_FUNLOC(custom_print_cb)
  CALL h5ewalk_f(H5P_DEFAULT_F, H5E_WALK_UPWARD_F, func_ptr, ptr4, error)
  CALL check("h5ewalk_f", error, total_error)

  CALL H5Eclose_msg_f(major, error)
  CALL check("H5Eclose_msg_f", error, total_error)
  CALL H5Eclose_msg_f(minor, error)
  CALL check("H5Eclose_msg_f", error, total_error)

  CALL h5eunregister_class_f(cls_id, error)
  CALL check("H5Eunregister_class_f", error, total_error)

END SUBROUTINE test_error_stack

END MODULE TH5E_F03
