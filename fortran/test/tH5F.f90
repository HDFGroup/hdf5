!
! 
!    Testing File Interface functionality.
!
!      MODULE H5FTEST

!        USE HDF5  ! This module contains all necessary modules
        
!      CONTAINS

!In the mountingtest subroutine we create one file with a group in it, 
!and another file with a dataset. Mounting is used to
!access the dataset from the second file as a member of a group 
!in the first file. 
!
        SUBROUTINE mountingtest(total_error)
        USE HDF5  ! This module contains all necessary modules
          IMPLICIT NONE
          INTEGER, INTENT(OUT) :: total_error 

          !
          !the respective filename is "mount1.h5" and "mount2.h5"
          !
          CHARACTER(LEN=9), PARAMETER :: filename1 = "mount1.h5"
          CHARACTER(LEN=9), PARAMETER :: filename2 = "mount2.h5"

          !
          !data space rank and dimensions
          !
          INTEGER, PARAMETER :: RANK = 2
          INTEGER, PARAMETER :: NX = 4
          INTEGER, PARAMETER :: NY = 5

          !
          ! File identifiers
          !
          INTEGER(HID_T) :: file1_id, file2_id  

          !
          ! Group identifier
          !
          INTEGER(HID_T) :: gid 

          !
          ! dataset identifier
          !
          INTEGER(HID_T) :: dset_id

          !
          ! data space identifier
          !
          INTEGER(HID_T) :: dataspace

          !
          ! data type identifier
          !
          INTEGER(HID_T) :: dtype_id

          ! 
          !The dimensions for the dataset.
          !
          INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/NX,NY/)
   
          !
          !return value for testing whether a file is in hdf5 format
          !
          LOGICAL     ::  status

          !
          !flag to check operation success 
          !         
          INTEGER     ::   error

          !
          !general purpose integer 
          !         
          INTEGER     ::   i, j

          !
          !data buffers 
          !         
          INTEGER, DIMENSION(NX,NY) :: data_in, data_out

          !
          !Initialize FORTRAN predifined datatypes
          !
!          CALL h5init_types_f(error) 
!               CALL check("h5init_types_f",error,total_error)

          !
          !Initialize data_in buffer
          !
          do i = 1, NX
             do j = 1, NY
                data_in(i,j) =  (i-1) + (j-1)
             end do
          end do

          !
          !Create first file "mount1.h5" using default properties.
          ! 
          CALL h5fcreate_f(filename1, H5F_ACC_TRUNC_F, file1_id, error)
               CALL check("h5fcreate_f",error,total_error)
        

          !
          !Create group "/G" inside file "mount1.h5".
          ! 
          CALL h5gcreate_f(file1_id, "/G", gid, error)
               CALL check("h5gcreate_f",error,total_error)

          !
          !close file and group identifiers.
          ! 
          CALL h5gclose_f(gid, error)
               CALL check("h5gclose_f",error,total_error)
          CALL h5fclose_f(file1_id, error)
               CALL check("h5fclose_f",error,total_error)

          !
          !Create second file "mount2.h5" using default properties.
          ! 
          CALL h5fcreate_f(filename2, H5F_ACC_TRUNC_F, file2_id, error)
               CALL check("h5fcreate_f",error,total_error)

          !
          !Create data space for the dataset. 
          !
          CALL h5screate_simple_f(RANK, dims, dataspace, error)
               CALL check("h5screate_simple_f",error,total_error)

          !
          !Create dataset "/D" inside file "mount2.h5".
          ! 
          CALL h5dcreate_f(file2_id, "/D", H5T_NATIVE_INTEGER, dataspace, &
               dset_id, error)
              CALL check("h5dcreate_f",error,total_error)

          !
          ! Write data_in to the dataset
          !
          CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data_in, error)
               CALL check("h5dwrite_f",error,total_error)

          !
          !close file, dataset and dataspace identifiers.
          ! 
          CALL h5sclose_f(dataspace, error)
              CALL check("h5sclose_f",error,total_error)
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f",error,total_error)
          CALL h5fclose_f(file2_id, error)
              CALL check("h5fclose_f",error,total_error)

          !
          !test whether files are in hdf5 format
          !
          CALL h5fis_hdf5_f(filename1, status, error)
               CALL check("h5fis_hdf5_f",error,total_error)
          IF ( .NOT. status ) THEN
              write(*,*) "File ", filename1, " is not in hdf5 format"
              stop
          END IF

          CALL h5fis_hdf5_f(filename2, status, error)
               CALL check("h5fis_hdf5_f",error,total_error)
          IF ( .NOT. status ) THEN
              write(*,*) "File ", filename2, " is not in hdf5 format"
              stop
          END IF

          !
          !reopen both files.
          ! 
          CALL h5fopen_f (filename1, H5F_ACC_RDWR_F, file1_id, error)
              CALL check("hfopen_f",error,total_error)
          CALL h5fopen_f (filename2, H5F_ACC_RDWR_F, file2_id, error)
              CALL check("h5fopen_f",error,total_error)

          !
          !mount the second file under the first file's "/G" group.
          ! 
          CALL h5fmount_f (file1_id, "/G", file2_id, error)
              CALL check("h5fmount_f",error,total_error)


          !
          !Access dataset D in the first file under /G/D name.
          ! 
          CALL h5dopen_f(file1_id, "/G/D", dset_id, error)
              CALL check("h5dopen_f",error,total_error)

          !
          !Get dataset's data type.
          ! 
          CALL h5dget_type_f(dset_id, dtype_id, error)
              CALL check("h5dget_type_f",error,total_error)

          !
          !Read the dataset.
          ! 
          CALL h5dread_f(dset_id, dtype_id, data_out, error)
              CALL check("h5dread_f",error,total_error)

          !
          !Compare the data.
          ! 
          do i = 1, NX
              do j = 1, NY
                  IF (data_out(i,j) .NE. data_in(i, j)) THEN 
                      write(*, *) "mounting test error occured"
                  END IF
              end do    
          end do


          !
          !Close dset_id and dtype_id.
          ! 
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f",error,total_error)
          CALL h5tclose_f(dtype_id, error)
              CALL check("h5tclose_f",error,total_error)

          !
          !unmount the second file.
          ! 
          CALL h5funmount_f(file1_id, "/G", error);
              CALL check("h5funmount_f",error,total_error)

          !
          !Close both files.
          ! 
          CALL h5fclose_f(file1_id, error)
              CALL check("h5fclose_f",error,total_error)
          CALL h5fclose_f(file2_id, error)
              CALL check("h5fclose_f",error,total_error)

          !
          ! Close FORTRAN predefined datatypes.
          !
!          CALL h5close_types_f(error)
!              CALL check("h5close_types_f",error,total_error)
          RETURN
        END SUBROUTINE mountingtest

!
!The following subroutine tests h5freopen_f.
!It creates the file which has name "reopen.h5" and 
!the "/dset" dataset inside the file.
!writes the data to the file, close the dataset.
!Reopen the file based upon the file_id, open the 
!dataset use the reopen_id then reads the 
!dataset back to memory to test whether the data
!read is identical to the data written  
!

        SUBROUTINE reopentest(total_error)
        USE HDF5  ! This module contains all necessary modules
          IMPLICIT NONE
          INTEGER, INTENT(OUT) :: total_error 
     
          !
          !the dataset is stored in file "dsetf.h5"
          !
          CHARACTER(LEN=9), PARAMETER :: filename = "reopen.h5"

          INTEGER(HID_T) :: file_id, reopen_id  ! File identifiers 
          INTEGER(HID_T) :: dset_id             ! Dataset identifier 

          !
          !dataset name is "dset"
          !
          CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"

          !
          !data space rank and dimensions
          !
          INTEGER, PARAMETER :: RANK = 2
          INTEGER, PARAMETER :: NX = 4
          INTEGER, PARAMETER :: NY = 6

          !
          ! data space identifier
          !
          INTEGER(HID_T) :: dataspace

          ! 
          !The dimensions for the dataset.
          !
          INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/NX,NY/)

          !
          !flag to check operation success
          !          
          INTEGER     ::   error

          !
          !general purpose integer
          !          
          INTEGER     ::  i, j

          !
          !array to store data 
          !
          INTEGER, DIMENSION(4,6) :: dset_data, data_out
     
          !
          !initialize the dset_data array which will be written to the "/dset"
          !
          do i = 1, NX
               do j = 1, NY
                    dset_data(i,j) = (i-1)*6 + j;
               end do
          end do

          !
          !Initialize FORTRAN predifined datatypes
          !
!          CALL h5init_types_f(error) 
!               CALL check("h5init_types_f",error,total_error)


          !
          !Create file "reopen.h5" using default properties.
          ! 
          CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
               CALL check("h5fcreate_f",error,total_error)

          !
          !Create data space for the dataset. 
          !
          CALL h5screate_simple_f(RANK, dims, dataspace, error)
               CALL check("h5screate_simple_f",error,total_error)

          !
          !Create dataset "/dset" inside the file .
          ! 
          CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
               dset_id, error)
              CALL check("h5dcreate_f",error,total_error)

         !
         !Write the dataset.
         !
         CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, dset_data, error)
              CALL check("h5dwrite_f",error,total_error)

         !
         !close the dataset.
         !
         CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f",error,total_error)

         !
         !close the dataspace.
         !
         CALL h5sclose_f(dataspace, error)
              CALL check("h5sclose_f",error,total_error)

         !
         !Reopen file dsetf.h5. 
         !
         CALL h5freopen_f(file_id, reopen_id, error)
              CALL check("h5freopen_f",error,total_error)

         !
         !Open the dataset based on the reopen_id. 
         !
         CALL h5dopen_f(reopen_id, dsetname, dset_id, error)
              CALL check("h5dopen_f",error,total_error)

         !
         !Read the dataset.
         !
         CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, error)
              CALL check("h5dread_f",error,total_error)

          !
          !Compare the data.
          ! 
          do i = 1, NX
              do j = 1, NY
                  IF (data_out(i,j) .NE. dset_data(i, j)) THEN 
                      write(*, *) "reopen test error occured"
                  END IF
              end do    
          end do


         !
         !Close the dataset.
         !
         CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f",error,total_error)

         !
         !Close the file identifiers.
         !
         CALL h5fclose_f(file_id, error)
              CALL check("h5fclose_f",error,total_error)
         CALL h5fclose_f(reopen_id, error)
              CALL check("h5fclose_f",error,total_error)
     
         !
         !Close FORTRAN predifined datatypes
         !
!         CALL h5close_types_f(error)
!              CALL check("h5close_types_f",error,total_error)


          RETURN

        END SUBROUTINE reopentest

!
!The following example demonstrates how to get creation property list,
!and access property list.
!We first create a file using the default creation and access property
!list. Then, the file was closed and reopened. We then get the
!creation and access property lists of the first file. The second file is
!created using the got property lists 

        SUBROUTINE plisttest(total_error)
         USE HDF5  ! This module contains all necessary modules
          IMPLICIT NONE
          INTEGER, INTENT(OUT) :: total_error 

          !
          !file names are "plist1.h5" and "plist2.h5"
          !
          CHARACTER(LEN=9), PARAMETER :: filename1 = "plist1.h5"
          CHARACTER(LEN=9), PARAMETER :: filename2 = "plist2.h5"

          INTEGER(HID_T) :: file1_id, file2_id   ! File identifiers
          INTEGER(HID_T) :: prop_id    ! File creation property list identifier
          INTEGER(HID_T) :: access_id  ! File Access property list identifier

          !flag to check operation success          
          INTEGER     ::   error

          !
          !Initialize FORTRAN predifined datatypes
          !
!          CALL h5init_types_f(error) 
!               CALL check("h5init_types_f",error,total_error)

          !
          !Create a file1 using default properties.
          ! 
          CALL h5fcreate_f(filename1, H5F_ACC_TRUNC_F, file1_id, error)
              CALL check("h5fcreate_f",error,total_error)

          !
          !Terminate access to the file.
          !
          CALL h5fclose_f(file1_id, error)
              CALL check("h5fclose_f",error,total_error)

          !
          !Open an existing file.
          !
          CALL h5fopen_f (filename1, H5F_ACC_RDWR_F, file1_id, error)
              CALL check("h5fopen_f",error,total_error)

          !
          !get the creation property list.
          !
          CALL h5fget_create_plist_f(file1_id, prop_id, error)
              CALL check("h5fget_create_plist_f",error,total_error)

          !
          !get the access property list.
          !
          CALL h5fget_access_plist_f(file1_id, access_id, error)
              CALL check("h5fget_access_plist_f",error,total_error)

          !
          !based on the creation property list id and access property list id
          !create a new file
          !
          CALL h5fcreate_f(filename2, H5F_ACC_TRUNC_F, file2_id, error, &
               prop_id, access_id)
              CALL check("h5create_f",error,total_error)

          !
          !Close all the property lists.
          !
          CALL h5pclose_f(prop_id, error)
              CALL check("h5pclose_f",error,total_error)
          CALL h5pclose_f(access_id, error)
              CALL check("h5pclose_f",error,total_error)
          
          !
          !Terminate access to the files.
          !
          CALL h5fclose_f(file1_id, error)
              CALL check("h5fclose_f",error,total_error)

          CALL h5fclose_f(file2_id, error)
              CALL check("h5fclose_f",error,total_error)

          !
          !Close FORTRAN predifined datatypes
          !
!          CALL h5close_types_f(error)
!              CALL check("h5close_types_f",error,total_error)
          RETURN

        END SUBROUTINE plisttest
              
     

!      END MODULE H5FTEST


