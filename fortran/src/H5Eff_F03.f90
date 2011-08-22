!****h* ROBODoc/H5E (F03)
!
! NAME
!  H5L_PROVISIONAL
!
! FILE
!  src/fortran/src/H5Eff_F03.f90 
!
! PURPOSE
!
!  This file contains Fortran 90 and Fortran 2003 interfaces for H5E functions.
!  It contains the same functions as H5Eff_DEPRECIATE.f90 but includes the
!  Fortran 2003 functions and the interface listings. This file will be compiled
!  instead of H5Eff_DEPRECIATE.f90 if Fortran 2003 functions are enabled.
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
!  If you add a new H5E function to the module you must add the function name
!  to the Windows dll file 'hdf5_fortrandll.def' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5E_PROVISIONAL

  USE H5GLOBAL

CONTAINS

  INTEGER FUNCTION h5eprint_def() bind(C)

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER :: hdferr
    
    PRINT*,'Inside h5eprint_def'
!    STOP

!!$    CALL h5eprint_f(hdferr)
!!$    h5eprint_def = hdferr
    
  END FUNCTION h5eprint_def

!****s* H5E/h5eset_auto2_f
!
! NAME
!  h5eset_auto2_f
!
! PURPOSE
!  Returns settings for automatic error stack traversal function and its data.
!
! INPUTS
!  printflag   - Flag to turn automatic error printing on or off;
!                possible values are:
!                  printon (1)
!                  printoff(0)
!  estack_id   - Error stack identifier.
!  func        - Function to be called upon an error condition.
!  client_data - Data passed to the error function
!  
! OUTPUTS
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  July 10, 2009
!
! SOURCE
  SUBROUTINE h5eset_auto_f(printflag, hdferr, estack_id, func, client_data)
    USE ISO_C_BINDING
    INTEGER       , INTENT(IN)            :: printflag
    INTEGER       , INTENT(OUT)           :: hdferr
    INTEGER(HID_T), INTENT(IN) , OPTIONAL :: estack_id
    TYPE(C_FUNPTR), INTENT(IN) , OPTIONAL :: func
    TYPE(C_PTR)   , INTENT(IN) , OPTIONAL :: client_data
!*****
    INTEGER(HID_T) :: estack_id_default
    TYPE(C_FUNPTR) :: func_default
    TYPE(C_PTR)    :: client_data_default
    INTERFACE
       INTEGER FUNCTION h5eset_auto2_c(printflag, estack_id, func, client_data)
         USE ISO_C_BINDING
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ESET_AUTO2_C'::h5eset_auto2_c
         !DEC$ENDIF
         INTEGER :: printflag
         INTEGER(HID_T) :: estack_id
!!$         TYPE(C_FUNPTR) :: func
!!$         TYPE(C_PTR), VALUE :: client_data
         TYPE(C_FUNPTR), VALUE :: func
         TYPE(C_PTR), VALUE :: client_data
       END FUNCTION h5eset_auto2_c
    END INTERFACE

    estack_id_default = -1
    func_default = C_NULL_FUNPTR
    client_data_default = C_NULL_PTR

    IF(PRESENT(estack_id)) estack_id_default = estack_id
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(client_data)) client_data_default = client_data

    hdferr = h5eset_auto2_c(printflag, estack_id_default, func_default, client_data_default)
  END SUBROUTINE h5eset_auto_f

!****s* H5E/h5eget_auto_f
!
! NAME
!  h5eget_auto_f
!
! PURPOSE
!  Returns the settings for the automatic error stack traversal function and its data.
!
! INPUTS
!  estack_id    - Error stack identifier. H5E_DEFAULT_F indicates the current stack.
! OUTPUTS
!  func         - The function currently set to be called upon an error condition.
!  client_data  - Data currently set to be passed to the error function.
!  hdferr       - Returns 0 if successful and -1 if fails.
!
! AUTHOR
!  M. Scot Breitenfeld
!  July 10, 2009
!
! SOURCE
  SUBROUTINE h5eget_auto_f(estack_id, op, client_data, hdferr)
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: estack_id
!!$    TYPE(C_FUNPTR) :: op_f
!!$    TYPE(C_PTR) :: client_data_f
    TYPE(C_FUNPTR) :: op
    TYPE(C_PTR), VALUE :: client_data
    INTEGER, INTENT(OUT) :: hdferr
!*****
    INTEGER :: ret_func2
    !INTEGER(C_INT), DIMENSION(:), POINTER :: ptr_data
    INTEGER, DIMENSION(1:1) :: array_shape
    TYPE(C_PTR), TARGET :: f_ptr1
    INTEGER(C_INT) :: ptr_data
    INTEGER(C_INT) :: i
    TYPE(C_PTR) :: test
    INTEGER, POINTER :: a

    INTEGER, TARGET :: j
    TYPE(C_PTR) :: f_ptr2

    INTERFACE
       INTEGER FUNCTION h5eget_auto_c(estack_id, op, client_data, ret_func2)
         USE ISO_C_BINDING
         USE H5GLOBAL
         IMPLICIT NONE
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5EGET_AUTO_C'::h5eget_auto_c
         !DEC$ENDIF
         INTEGER(HID_T) :: estack_id
         TYPE(C_FUNPTR) :: op
         TYPE(C_PTR) :: client_data
         INTEGER :: ret_func2
       END FUNCTION h5eget_auto_c

!!$       TYPE(C_PTR) FUNCTION h5eget_auto_c2(estack_id, op, ret_func2)
!!$         USE ISO_C_BINDING
!!$         USE H5GLOBAL
!!$         IMPLICIT NONE
!!$         !DEC$IF DEFINED(HDF5F90_WINDOWS)
!!$         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5EGET_AUTO_C'::h5eget_auto_c
!!$         !DEC$ENDIF
!!$         INTEGER(HID_T) :: estack_id
!!$         TYPE(C_FUNPTR) :: op
!!$         INTEGER :: ret_func2
!!$       END FUNCTION h5eget_auto_c2

!!$    SUBROUTINE process_buffer(estack_id, buffer)
!!$       USE, INTRINSIC :: ISO_C_BINDING
!!$      USE H5GLOBAL
!!$       INTEGER(HID_T) :: estack_id
!!$       TYPE(C_PTR) :: buffer
!!$     END SUBROUTINE process_buffer

    END INTERFACE

!   j = -9999


    f_ptr2 = c_loc(j)
!    CALL process_buffer(estack_id,f_ptr2)

    hdferr = h5eget_auto_c(estack_id, op, f_ptr2, ret_func2)

!!!!!    PRINT*,c_associated(f_ptr2)
!!$    hdferr = h5eget_auto_c(estack_id, op, client_data, ret_func2)
    
    PRINT*,'fortran',j
    stop

!    client_data = h5eget_auto_c2(estack_id, op, ret_func2)

!    PRINT*,'Is client_data associated',C_associated(client_data)
!    PRINT*,'Is op_data associated',C_associated(op)

!    ALLOCATE(i(1:1))
!    CALL c_f_pointer(f_ptr2,a,[1])
!    CALL c_f_pointer(f_ptr2,i)
!    PRINT*,i
!    PRINT*,"Buffer in (F) = ", a(1)

!    stop

!    ALLOCATE(ptr_data(1:2))
!    ptr_data = 0
!    array_shape(1) = 1
!    CALL C_F_POINTER(client_data, ptr_data, array_shape)
!    CALL C_F_POINTER(f_ptr2, i,(/ 1 /))

!    ptr_data => f_ptr1(1)

!    PRINT*,'value in fortran',i
    

! Check to see if the user created their own function,
! otherwise we have to create a fortran version of the default

!!$    IF(ret_func2.EQ.0)THEN
!!$       op = c_funloc(h5eprint_def)
!!$    END IF

  END SUBROUTINE h5eget_auto_f

END MODULE H5E_PROVISIONAL
