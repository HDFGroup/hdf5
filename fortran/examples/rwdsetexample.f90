!
! The following example shows how to write and read to/from an existing dataset. 
! It opens the file created in the previous example, obtains the dataset 
! identifier, writes the data to the dataset in the file, 
! then reads the dataset  to memory. 
!


     PROGRAM RWDSETEXAMPLE

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE

     CHARACTER(LEN=8), PARAMETER :: filename = "dsetf.h5" ! File name
     CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"     ! Dataset name

     INTEGER(HID_T) :: file_id       ! File identifier 
     INTEGER(HID_T) :: dset_id       ! Dataset identifier 

     INTEGER     ::   error ! Error flag
     INTEGER     ::  i, j

     INTEGER, DIMENSION(4,6) :: dset_data, data_out ! Data buffers
     
     !
     ! Initialize the dset_data array.
     !
     do i = 1, 4
          do j = 1, 6
               dset_data(i,j) = (i-1)*6 + j;
          end do
     end do

     !
     ! Initialize FORTRAN predefined datatypes
     !
     CALL h5init_types_f(error) 

     !
     ! Open an existing file.
     !
     CALL h5fopen_f (filename, H5F_ACC_RDWR_F, file_id, error)

     !
     ! Open an existing dataset. 
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)

     !
     ! Write the dataset.
     !
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, dset_data, error)

     !
     ! Read the dataset.
     !
     CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, error)

     !
     ! Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)
     
     !
     ! Close FORTRAN predefined datatypes.
     !
     CALL h5close_types_f(error)

     END PROGRAM RWDSETEXAMPLE 

               

