!
!In the following example we create one file with a group in it, 
!and another file with a dataset. Mounting is used to
!access the dataset from the second file as a member of a group 
!in the first file. 
!

     PROGRAM MOUNTEXAMPLE

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE

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
     CALL h5init_types_f(error) 

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

     !
     !Create group "/G" inside file "mount1.h5".
     ! 
     CALL h5gcreate_f(file1_id, "/G", gid, error)

     !
     !close file and group identifiers.
     ! 
     CALL h5gclose_f(gid, error)
     CALL h5fclose_f(file1_id, error)

     !
     !Create second file "mount2.h5" using default properties.
     ! 
     CALL h5fcreate_f(filename2, H5F_ACC_TRUNC_F, file2_id, error)

     !
     !Create data space for the dataset. 
     !
     CALL h5screate_simple_f(RANK, dims, dataspace, error)

     !
     !Create dataset "/D" inside file "mount2.h5".
     ! 
     CALL h5dcreate_f(file2_id, "/D", H5T_NATIVE_INTEGER, dataspace, &
                      dset_id, error)
 
     !
     ! Write data_in to the dataset
     !
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data_in, error)

     !
     !close file, dataset and dataspace identifiers.
     ! 
     CALL h5sclose_f(dataspace, error)
     CALL h5dclose_f(dset_id, error)
     CALL h5fclose_f(file2_id, error)

     !
     !reopen both files.
     ! 
     CALL h5fopen_f (filename1, H5F_ACC_RDONLY_F, file1_id, error)
     CALL h5fopen_f (filename2, H5F_ACC_RDONLY_F, file2_id, error)

     !
     !mount the second file under the first file's "/G" group.
     ! 
     CALL h5fmount_f (file1_id, "/G", file2_id, error)


     !
     !Access dataset D in the first file under /G/D name.
     ! 
     CALL h5dopen_f(file1_id, "/G/D", dset_id, error)

     !
     !Get dataset's data type.
     ! 
     CALL h5dget_type_f(dset_id, dtype_id, error)

     !
     !Read the dataset.
     ! 
     CALL h5dread_f(dset_id, dtype_id, data_out, error)

     !
     !Print out the data.
     ! 
     do i = 1, NX
          print *, (data_out(i,j), j = 1, NY)
     end do


     !
     !Close dset_id and dtype_id.
     ! 
     CALL h5dclose_f(dset_id, error)
     CALL h5tclose_f(dtype_id, error)

     !
     !unmount the second file.
     ! 
     CALL h5funmount_f(file1_id, "/G", error);

     !
     !Close both files.
     ! 
     CALL h5fclose_f(file1_id, error)
     CALL h5fclose_f(file2_id, error)

     END PROGRAM MOUNTEXAMPLE

