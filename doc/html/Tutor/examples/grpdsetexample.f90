!
! This example shows how to create a dataset in a particular group.
! It opens the file created in the previous example and creates two datasets.
! Absolute and relative dataset names are used.
!


     PROGRAM GRPDSETEXAMPLE

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE

     CHARACTER(LEN=10), PARAMETER :: filename = "groupsf.h5" ! File name
     CHARACTER(LEN=15), PARAMETER :: groupname = "MyGroup/Group_A" ! Group name
     CHARACTER(LEN=13), PARAMETER :: dsetname1 = "MyGroup/dset1"  ! Dataset name
     CHARACTER(LEN=5),  PARAMETER :: dsetname2 = "dset2" ! dataset name

     INTEGER(HID_T) :: file_id       ! File identifier 
     INTEGER(HID_T) :: group_id      ! Group identifier 
     INTEGER(HID_T) :: dataset_id    ! Dataset identifier 
     INTEGER(HID_T) :: dataspace_id  ! Data space identifier 

     INTEGER     ::  i, j 
     INTEGER     ::   error ! Error flag

     INTEGER, DIMENSION(3,3) :: dset1_data  ! Data arrays 
     INTEGER, DIMENSION(2,10) :: dset2_data !
     
     INTEGER(HSIZE_T), DIMENSION(2) :: dims1 = (/3,3/) ! Datasets dimensions
     INTEGER(HSIZE_T), DIMENSION(2) :: dims2 = (/2,10/)!

     INTEGER     ::   rank = 2 ! Datasets rank

     !
     !Initialize dset1_data array
     !
     do i = 1, 3
          do j = 1, 3
               dset1_data(i,j) = j;
          end do
     end do


     !
     !Initialize dset2_data array
     !
     do i = 1, 2
          do j = 1, 10
               dset2_data(i,j) = j;
          end do
     end do

     !
     ! Initialize FORTRAN predefined datatypes.
     !
     CALL h5init_types_f(error) 

     !
     ! Open an existing file.
     !
     CALL h5fopen_f (filename, H5F_ACC_RDWR_F, file_id, error)

     !
     ! Create the data space for the first dataset. 
     !
     CALL h5screate_simple_f(rank, dims1, dataspace_id, error)

     !
     ! Create a dataset in group "MyGroup" with default properties.
     !
     CALL h5dcreate_f(file_id, dsetname1, H5T_NATIVE_INTEGER, dataspace_id, &
                      dataset_id, error)

     !
     ! Write the first dataset.
     !
     CALL h5dwrite_f(dataset_id, H5T_NATIVE_INTEGER, dset1_data, error)

     !
     ! Close the dataspace for the first dataset.
     !
     CALL h5sclose_f(dataspace_id, error)

     !
     ! Close the first dataset.
     !
     CALL h5dclose_f(dataset_id, error)

     !
     ! Open an existing group in the specified file.
     !
     CALL h5gopen_f(file_id, groupname, group_id, error)

     !
     !Create the data space for the second dataset. 
     !
     CALL h5screate_simple_f(rank, dims2, dataspace_id, error)

     !
     ! Create the second dataset in group "Group_A" with default properties.
     !
     CALL h5dcreate_f(group_id, dsetname2, H5T_NATIVE_INTEGER, dataspace_id, &
                      dataset_id, error)

     !
     ! Write the second dataset.
     !
     CALL h5dwrite_f(dataset_id, H5T_NATIVE_INTEGER, dset2_data, error)

     !
     ! Close the dataspace for the second dataset.
     !
     CALL h5sclose_f(dataspace_id, error)

     !
     ! Close the second dataset.
     !
     CALL h5dclose_f(dataset_id, error)

     !
     ! Close the group.
     !
     CALL h5gclose_f(group_id, error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)

     !
     ! Close FORTRAN predefined datatypes.
     !
     CALL h5close_types_f(error)

     END PROGRAM GRPDSETEXAMPLE 
