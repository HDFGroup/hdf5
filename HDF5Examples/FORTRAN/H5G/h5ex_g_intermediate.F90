!************************************************************
!
!  This example shows how to create intermediate groups with
!  a single call to H5Gcreate.
!
!************************************************************/

MODULE g_intermediate

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
    CHARACTER(LEN=50) :: name_string = ' '
    TYPE(C_PTR) :: cptr
    INTEGER   :: i

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

END MODULE g_intermediate

!************************************************************
!
!  Operator function to be called by H5Ovisit.
!
!************************************************************
PROGRAM main
  
  USE HDF5
  USE ISO_C_BINDING
  USE g_intermediate
  
  IMPLICIT NONE

  CHARACTER(LEN=22), PARAMETER :: filename  = "h5ex_g_intermediate.h5"
  INTEGER(HID_T)               :: file
  INTEGER(HID_T)               :: group
  INTEGER(HID_T)               :: lcpl
  INTEGER                      :: status
  TYPE(C_FUNPTR) :: funptr
  TYPE(C_PTR)    :: f_ptr
  INTEGER        :: ret_value

  !
  ! Initialize FORTRAN interface.
  !
  CALL H5open_f(status)

  file  = H5I_INVALID_HID_F
  group = H5I_INVALID_HID_F
  lcpl  = H5I_INVALID_HID_F

  !
  ! Create a new file using the default properties.
  !
  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file, status)
  !
  ! Create link creation property list and set it to allow creation
  ! of intermediate groups.
  !
  CALL H5Pcreate_f(H5P_LINK_CREATE_F, lcpl, status)
  CALL H5Pset_create_inter_group_f(lcpl, 1, status)
  !
  ! Create the group /G1/G2/G3.  Note that /G1 and /G1/G2 do not
  ! exist yet.  This call would cause an error if we did not use the
  ! previously created property list.
  !
  CALL H5Gcreate_f(file, "/G1/G2/G3", group, status, lcpl_id=lcpl)
  !
  ! Print all the objects in the files to show that intermediate
  ! groups have been created.  See h5ex_g_visit_f for more information
  ! on how to use H5Ovisit_f.
  !
  WRITE(*,'(A)') "Objects in the file:"
  funptr = C_FUNLOC(op_func)
  f_ptr = C_NULL_PTR
  CALL H5Ovisit_f(file, H5_INDEX_NAME_F, H5_ITER_NATIVE_F, funptr, f_ptr, ret_value, status)
  !
  ! Close and release resources.
  !
  CALL H5Pclose_f(lcpl, status)
  CALL H5Gclose_f(group, status)
  CALL H5Fclose_f(file, status)

END PROGRAM main
