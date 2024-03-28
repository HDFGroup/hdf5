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

    INTEGER(C_INT) FUNCTION my_hdf5_error_handler(estack_id, data_inout) bind(C)

    ! This error function handle works with only version 2 error stack

    IMPLICIT NONE

    ! estack_id is always passed from C as: H5E_DEFAULT
    INTEGER(HID_T), VALUE :: estack_id

    ! data that was registered with H5Eset_auto_f
    ! INTEGER :: data_inout ! another option
    ! or
    TYPE(C_PTR), VALUE :: data_inout

    INTEGER, POINTER :: iunit

    CALL C_F_POINTER(data_inout, iunit)

  ! iunit = data_inout

    WRITE(iunit,'(A)') "H5Eset_auto_f_msg"
    WRITE(iunit,'(I0)') iunit

    iunit = 10*iunit

    my_hdf5_error_handler = 1 ! this is not used by the C routine

  END FUNCTION my_hdf5_error_handler

  !-------------------------------------------------------------------------
  ! Function:    custom_print_cb
  !
  ! Purpose:     Callback function to print error stack in customized way.
  !
  !-------------------------------------------------------------------------
  !
  INTEGER(C_INT) FUNCTION custom_print_cb(n, err_desc, op_data) BIND(C)

    IMPLICIT NONE

    INTEGER(SIZE_T), PARAMETER :: MSG_SIZE = 64

    INTEGER(C_INT), VALUE :: n
    TYPE(h5e_error_t) :: err_desc
    TYPE(C_PTR)       :: op_data

    CHARACTER(LEN=MSG_SIZE) :: maj
    CHARACTER(LEN=MSG_SIZE) :: minn
    CHARACTER(LEN=MSG_SIZE) :: cls
    INTEGER(SIZE_T) :: size
    INTEGER :: msg_type

    INTEGER :: error

    IF(n.NE.0_C_INT)THEN
       custom_print_cb = -1
       RETURN
    ENDIF

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

END MODULE test_my_hdf5_error_handler

MODULE TH5E_F03

  USE ISO_C_BINDING
  USE test_my_hdf5_error_handler

CONTAINS

SUBROUTINE test_error(total_error)

  IMPLICIT NONE

  INTEGER :: total_error
  INTEGER(hid_t) :: file
  INTEGER :: error
  INTEGER, TARGET :: my_hdf5_error_handler_data
  INTEGER, TARGET :: iunit
  TYPE(C_PTR) :: f_ptr
  TYPE(C_FUNPTR) :: func
  CHARACTER(LEN=180) :: chr180
  INTEGER :: idx
  INTEGER(HID_T) :: fapl

  LOGICAL :: status

  ! set the error stack to the customized routine

  iunit = 12
  OPEN(iunit, FILE="stderr.txt")

  my_hdf5_error_handler_data = iunit

  ! ** SET THE CUSTOMIZED PRINTING OF ERROR STACK **

  ! set the customized error handling routine
  func = C_FUNLOC(my_hdf5_error_handler)

  ! set the data sent to the customized routine
  f_ptr = C_LOC(my_hdf5_error_handler_data)

  CALL H5Eset_auto_f(1, error, H5E_DEFAULT_F, func, f_ptr)
  CALL check("H5Eset_auto_f", error, total_error)

  ! If a fapl is not created, then the test will fail when using
  ! check-passthrough-vol because the callback function is called twice, gh #4137.
  CALL h5pcreate_f(H5P_DATASET_ACCESS_F, fapl, error)
  CALL check("h5pcreate_f", error, total_error)
  CALL h5fopen_f("DOESNOTEXIST", H5F_ACC_RDONLY_F, file, error, fapl)
  CALL VERIFY("h5fopen_f", error, -1, total_error)
  CALL h5pclose_f(fapl,error)
  CALL check("h5pclose_f", error, total_error)

  CLOSE(iunit)

  OPEN(iunit, FILE="stderr.txt")

  READ(iunit,'(A)') chr180
  idx = INDEX(string=chr180,substring="H5Eset_auto_f_msg")
  IF(idx.EQ.0) CALL check("H5Eset_auto_f", -1, total_error)
  READ(iunit, *) idx
  CALL VERIFY("H5Eset_auto_f", idx, iunit, total_error)
  CALL VERIFY("H5Eset_auto_f", my_hdf5_error_handler_data, 10*iunit, total_error)

  CLOSE(iunit, STATUS='delete')

  CALL H5Eset_auto_f(0, error)
  CALL check("H5Eset_auto_f", error, total_error)

  CALL h5fopen_f("DOESNOTEXIST", H5F_ACC_RDONLY_F, file, error)
  CALL VERIFY("h5fopen_f", error, -1, total_error)

  INQUIRE(file="H5Etest.txt", EXIST=status)
  IF(status)THEN
     CALL VERIFY("H5Eset_auto_f", error, -1, total_error)
  ENDIF

END SUBROUTINE test_error

SUBROUTINE test_error_stack(total_error)

  IMPLICIT NONE

  INTEGER :: total_error
  INTEGER :: error
  INTEGER(HID_T) :: cls_id, major, minor, estack_id, estack_id1, estack_id2
  CHARACTER(LEN=18) :: file
  CHARACTER(LEN=18) :: func
  INTEGER           :: line
  TYPE(C_PTR) :: ptr1

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

  file = "FILE"
  func = "FUNC"
  line = 99

  CALL h5ecreate_stack_f(estack_id, error)
  CALL check("h5ecreate_stack_f", error, total_error)

  ! push a custom error message onto the stack
  CALL H5Epush_f(estack_id, file, func, line, &
       cls_id, major, minor, "%s ERROR TEXT %s %s %s", error, &
       arg1=ACHAR(27)//"[31m"//C_NULL_CHAR, arg2=ACHAR(27)//"[0m"//C_NULL_CHAR, &
       arg3=ACHAR(0)//C_NULL_CHAR, arg4=ACHAR(10)//C_NULL_CHAR )
  CALL check("H5Epush_f", error, total_error)

  CALL h5eget_num_f(estack_id, count, error)
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

  CALL h5eprint_f(H5E_DEFAULT_F, error)
  CALL check("h5eprint_f", error, total_error)
  CALL h5eprint_f(error)
  CALL check("h5eprint_f", error, total_error)

  INQUIRE(file="H5Etest.txt", EXIST=status)
  IF(status)THEN
     OPEN(UNIT=12, FILE="H5Etest.txt", status='old')
     CLOSE(12, STATUS='delete')
  ENDIF

  CALL h5eprint_f(estack_id, error, "H5Etest.txt")
  CALL check("h5eprint_f", error, total_error)

  INQUIRE(file="H5Etest.txt", EXIST=status)
  IF(.NOT.status)THEN
     CALL check("h5eprint_f", -1, total_error)
  ELSE
!    The contents of the file should be:
!       Custom error class-DIAG: Error detected in H5E_F03 (0.1) thread 0:
!         #000: FILE line 99 in FUNC():  ERROR TEXT
!
!           major: MAJOR MSG
!           minor: MIN MSG
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
  ptr1 = C_LOC(stderr(1:1))
  func_ptr = C_FUNLOC(custom_print_cb)

  CALL h5ewalk_f(estack_id, H5E_WALK_UPWARD_F, func_ptr, ptr1, error)
  CALL check("h5ewalk_f", error, total_error)

  CALL h5eget_num_f(estack_id, count, error)
  CALL check("h5eget_num_f", error, total_error)
  CALL VERIFY("h5eget_num_f", count, 1_SIZE_T, total_error)

  CALL H5Ecreate_stack_f(estack_id2, error)
  CALL check("H5Ecreate_stack_f", error, total_error)

  CALL H5Eappend_stack_f(estack_id2, estack_id, .FALSE., error)
  CALL check("H5Eappend_stack_f", error, total_error)

  CALL h5eget_num_f(estack_id2, count, error)
  CALL check("h5eget_num_f", error, total_error)
  CALL VERIFY("h5eget_num_f", count, 1_SIZE_T, total_error)

  ! Copy error stack, which clears the original
  CALL H5Eget_current_stack_f(estack_id1, error)
  CALL check("H5Eget_current_stack_f", error, total_error)

  CALL h5eget_num_f(estack_id1, count, error)
  CALL check("h5eget_num_f", error, total_error)
  CALL VERIFY("h5eget_num_f", count, 0_SIZE_T, total_error)

  CALL H5Eclose_stack_f(estack_id2, error)
  CALL check(" H5Eclose_stack_f", error, total_error)

  CALL H5Eclose_stack_f(estack_id, error)
  CALL check("H5Eclose_stack_f", error, total_error)

  CALL H5Eclose_stack_f(estack_id1, error)
  CALL check("H5Eclose_stack_f", error, total_error)

  CALL h5ecreate_stack_f(estack_id1, error)
  CALL check("h5ecreate_stack_f", error, total_error)

  ! push a custom error message onto the stack
  CALL H5Epush_f(estack_id1, file, func, line, &
       cls_id, major, minor, "%s ERROR TEXT %s %s", error, &
       arg1=ACHAR(27)//"[31m", arg2=ACHAR(27)//"[0m", arg3=ACHAR(10) )
  CALL check("H5Epush_f", error, total_error)

  CALL H5Eset_current_stack_f(estack_id1, error) ! API will also close estack_id1
  CALL check("H5Eset_current_stack_f", error, total_error)

  CALL h5eget_num_f(H5E_DEFAULT_F, count, error)
  CALL check("h5eget_num_f", error, total_error)
  CALL VERIFY("h5eget_num_f", count, 1_SIZE_T, total_error)

  CALL h5epop_f(H5E_DEFAULT_F, 1_size_t, error)
  CALL check("h5epop_f", error, total_error)

  CALL h5eget_num_f(H5E_DEFAULT_F, count, error)
  CALL check("h5eget_num_f", error, total_error)
  CALL VERIFY("h5eget_num_f", count, 0_SIZE_T, total_error)

  CALL H5Eclose_msg_f(major, error)
  CALL check("H5Eclose_msg_f", error, total_error)

  CALL H5Eclose_msg_f(minor, error)
  CALL check("H5Eclose_msg_f", error, total_error)

  CALL h5eunregister_class_f(cls_id, error)
  CALL check("H5Eunregister_class_f", error, total_error)

END SUBROUTINE test_error_stack

END MODULE TH5E_F03
