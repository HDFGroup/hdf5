!
! The following example demonstrates how to create and close an HDF5 file.
! It creates a file called 'file.h5',  and then closes the file.
!

     PROGRAM FILEEXAMPLE

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE

     CHARACTER(LEN=8), PARAMETER :: filename = "filef.h5" ! File name
     INTEGER(HID_T) :: file_id                            ! File identifier
 
     INTEGER     ::   error  ! Error flag

     !
     ! Create a new file using default properties.
     ! 
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

     !
     ! Terminate access to the file.
     !
     CALL h5fclose_f(file_id, error)

     END PROGRAM FILEEXAMPLE 
