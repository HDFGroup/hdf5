! This example shows how to create and write a dataset attribute. 
! It opens the existing file 'dset.h5', obtains the identifier of
! the dataset "/dset", defines attribute's dataspace, 
! creates dataset attribute, writes the attribute, and then closes
! the attribute's dataspace, attribute, dataset, and file. 

     PROGRAM ATTREXAMPLE

        
     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE

     CHARACTER(LEN=8), PARAMETER :: filename = "dsetf.h5" ! File name
     CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"     ! Dataset name
     CHARACTER(LEN=4), PARAMETER :: aname = "attr"        ! Attribute name

     INTEGER(HID_T) :: file_id       ! File identifier 
     INTEGER(HID_T) :: dset_id       ! Dataset identifier 
     INTEGER(HID_T) :: attr_id       ! Attribute identifier 
     INTEGER(HID_T) :: aspace_id     ! Attribute Dataspace identifier 

     INTEGER(HSIZE_T), DIMENSION(1) :: adims = (/2/) ! Attribute dimension
     INTEGER, DIMENSION(2) :: attr_data = (/100,200/)! Attribute data
     INTEGER     ::   arank = 1                      ! Attribure rank 

     INTEGER     ::   error ! Error flag
     

     !
     ! Initialize FORTRAN predefined datatypes.
     !
     CALL h5open_f(error) 
    
     !
     ! Open an existing file.
     !
     CALL h5fopen_f (filename, H5F_ACC_RDWR_F, file_id, error)

     !
     ! Open an existing dataset. 
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)

     !
     ! Create the data space for the attribute. 
     !
     CALL h5screate_simple_f(arank, adims, aspace_id, error)
     
     !
     ! Create dataset attribute.
     !
     CALL h5acreate_f(dset_id, aname, H5T_NATIVE_INTEGER,aspace_id, &
                      attr_id, error)
     
     !
     ! Write the attribute data.
     !
     CALL h5awrite_f(attr_id, H5T_NATIVE_INTEGER, attr_data, error)
     
     !
     ! Close the attribute. 
     !
     CALL h5aclose_f(attr_id, error)
     
     !
     ! Terminate access to the data space.
     !
     CALL h5sclose_f(aspace_id, error)

     !   
     ! End access to the dataset and release resources used by it.
     ! 
     CALL h5dclose_f(dset_id, error)

     ! 
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)

     !
     ! Close FORTRAN predefined datatypes.
     !
     CALL h5close_f(error)

     END PROGRAM ATTREXAMPLE 
 
