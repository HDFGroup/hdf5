!
! The following example shows how to create and close a group. 
! It creates a file called 'group.h5', creates a group
! called MyGroup in the root group, and then closes the group and file.
! 


     PROGRAM GROUPEXAMPLE

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE

     CHARACTER(LEN=9), PARAMETER :: filename = "groupf.h5" ! File name
     CHARACTER(LEN=7), PARAMETER :: groupname = "MyGroup"  ! Group name

     INTEGER(HID_T) :: file_id       ! File identifier 
     INTEGER(HID_T) :: group_id      ! Group identifier 

     INTEGER     ::   error ! Error flag
!
!    Initialize FORTRAN interface.
!
     CALL h5open_f(error)     
     !
     ! Create a new file using default properties.
     ! 
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

     !
     ! Create a group named "/MyGroup" in the file.
     !
     CALL h5gcreate_f(file_id, groupname, group_id, error)

     !
     ! Close the group.
     !
     CALL h5gclose_f(group_id, error)

     !
     ! Terminate access to the file.
     !
     CALL h5fclose_f(file_id, error)
!
!    Close FORTRAN interface.
!
     CALL h5close_f(error)

     END PROGRAM GROUPEXAMPLE 
