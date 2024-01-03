!************************************************************
!
!  This example shows how to recursively traverse a file
!  using H5Ovisit.  The program prints all of
!  the objects in the file specified in FILE.  The default 
!  file used by this example implements the structure described 
!  in the User's Guide, chapter 4, figure 26.
!
!************************************************************

MODULE g_visit
  
  USE HDF5
  USE ISO_C_BINDING
  IMPLICIT NONE

CONTAINS

!************************************************************
!
!  Operator function for H5Ovisit.  This function prints the
!  name and type of the object passed to it.
!
!************************************************************

  INTEGER FUNCTION op_func(loc_id, name, info, cptr) bind(C)

    USE HDF5
    USE ISO_C_BINDING
    IMPLICIT NONE
    
    INTEGER(HID_T), VALUE :: loc_id
    CHARACTER(LEN=1), DIMENSION(1:50) :: name ! We must have LEN=1 for bind(C) strings
                                              ! in order to be standard compliant
    TYPE(H5O_info_t) :: info
    CHARACTER(LEN=50) :: name_string
    TYPE(C_PTR) :: cptr
    INTEGER   :: i

    name_string(:) = " "
    DO i = 1, 50
       IF(name(i)(1:1).EQ.C_NULL_CHAR) EXIT ! Read up to the C NULL termination
       name_string(i:i) = name(i)(1:1)
    ENDDO

    WRITE(*,"('/')",ADVANCE="NO")  !  Print root group in object path
    !
    ! Check if the current object is the root group, and if not print
    ! the full path name and type.
    !
    IF(name(1)(1:1) .EQ. '.')THEN         ! Root group, do not print '.'
        WRITE(*,"('  (Group)')")
    ELSE
       IF(info%type.EQ.H5O_TYPE_GROUP_F)THEN
          WRITE(*,'(A,"  (Group)")') TRIM(name_string)
       ELSE IF(info%type.EQ.H5O_TYPE_DATASET_F)THEN
          WRITE(*,'(A,"  (Dataset)")') TRIM(name_string)
       ELSE IF(info%type.EQ.H5O_TYPE_NAMED_DATATYPE_F)THEN
          WRITE(*,'(A,"  (Datatype)")') TRIM(name_string)
       ELSE
          WRITE(*,'(A,"  (Unknown)")') TRIM(name_string)
       ENDIF
    ENDIF

    op_func = 0 ! return successful

  END FUNCTION op_func


!************************************************************
!
!  Operator function for H5Lvisit_f.  This function simply
!  retrieves the info for the object the current link points
!  to, and calls the operator function for H5Ovisit_f.
!
! ************************************************************/
  INTEGER FUNCTION op_func_L(loc_id, name, info, cptr) bind(C)

    USE HDF5
    USE ISO_C_BINDING
    IMPLICIT NONE
    
    INTEGER(HID_T), VALUE :: loc_id
    CHARACTER(LEN=1), DIMENSION(1:50) :: name ! We must have LEN=1 for bind(C) strings
                                              ! in order to be standard compliant
    TYPE(H5L_info_t) :: info
    TYPE(C_PTR) :: cptr

    CHARACTER(LEN=50) :: name_string
    INTEGER   :: i
    INTEGER   ::  status;
    TYPE(H5O_info_t) :: infobuf

    TYPE(C_PTR) :: ptr

    name_string(:) = " "
    DO i = 1, 50
       IF(name(i)(1:1).EQ.C_NULL_CHAR) EXIT ! Read up to the C NULL termination
       name_string(i:i) = name(i)(1:1)
    ENDDO

    !
    !  Get type of the object and display its name and type.
    ! The name of the object is passed to this function by
    ! the Library.
    !
    CALL H5Oget_info_by_name_f(loc_id, name_string, infobuf, status);

    op_func_L = op_func(loc_id, name_string, infobuf, cptr)

  END FUNCTION op_func_L

END MODULE g_visit

PROGRAM main
  
  USE HDF5
  USE ISO_C_BINDING
  USE g_visit
  
  IMPLICIT NONE

  CHARACTER(LEN=15), PARAMETER :: filename  = "h5ex_g_visit.h5"
  INTEGER(HID_T) :: file ! Handle
  INTEGER :: status
  TYPE(C_FUNPTR) :: funptr
  TYPE(C_PTR) :: ptr
  INTEGER :: ret_value
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(status)

  CALL H5Fopen_f(filename, H5F_ACC_RDONLY_F, file, status)
  !
  ! Begin iteration using H5Ovisit
  !
  WRITE(*,'(A)') "Objects in the file:"
  
  funptr = C_FUNLOC(op_func)
  ptr = C_NULL_PTR
  CALL H5Ovisit_f(file, H5_INDEX_NAME_F, H5_ITER_NATIVE_F, funptr, ptr, ret_value, status)

  !
  ! Repeat the same process using H5Lvisit
  !
  WRITE(*,'(/,A)') "Links in the file:"

  funptr = C_FUNLOC(op_func_L)
  ptr = C_NULL_PTR
  CALL H5Lvisit_f(file, H5_INDEX_NAME_F, H5_ITER_NATIVE_F, funptr, ptr, ret_value, status)

  !
  ! Close and release resources.
  !
  CALL H5Fclose_f(file, status)
  
END PROGRAM main
