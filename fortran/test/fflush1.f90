 !
 ! Purpose:	This is the first half of a two-part test that makes sure
 !		that a file can be read after an application crashes as long
 !		as the file was flushed first.  We simulate by exit the 
 !              the program using stop statement
 !

     PROGRAM FFLUSH1EXAMPLE

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE

     !
     !the respective filename is "fflush1.h5" 
     !
     CHARACTER(LEN=10), PARAMETER :: filename = "fflush1.h5"

     !
     !data space rank and dimensions
     !
     INTEGER, PARAMETER :: RANK = 2
     INTEGER, PARAMETER :: NX = 4
     INTEGER, PARAMETER :: NY = 5

     !
     ! File identifiers
     !
     INTEGER(HID_T) :: file_id 
     
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
     INTEGER     ::   i, j, total_error = 0

     !
     !data buffers 
     !         
     INTEGER, DIMENSION(NX,NY) :: data_in, data_out

     !
     !Initialize FORTRAN predifined datatypes
     !
     CALL h5open_f(error) 
          CALL check("h5init_types_f",error,total_error)

     !
     !Initialize data_in buffer
     !
     do i = 1, NX
          do j = 1, NY
               data_in(i,j) =  (i-1) + (j-1)
          end do
     end do

     !
     !Create file "fflush1.h5" using default properties.
     ! 
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
          CALL check("h5fcreate_f",error,total_error)

     !
     !Create group "/G" inside file "fflush1.h5".
     ! 
     CALL h5gcreate_f(file_id, "/G", gid, error)
          CALL check("h5gcreate_f",error,total_error)

     !
     !Create data space for the dataset. 
     !
     CALL h5screate_simple_f(RANK, dims, dataspace, error)
          CALL check("h5screate_simple_f",error,total_error)

     !
     !Create dataset "/D" inside file "fflush1.h5".
     ! 
     CALL h5dcreate_f(file_id, "/D", H5T_NATIVE_INTEGER, dataspace, &
                      dset_id, error)
          CALL check("h5dcreate_f",error,total_error)
 
     !
     ! Write data_in to the dataset
     !
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data_in, error)
          CALL check("h5dwrite_f",error,total_error)
  
     !
     !flush and exit without closing the library
     !
     CALL H5fflush_f(file_id, H5F_SCOPE_GLOBAL_F, error)
          CALL check("h5fflush_f",error,total_error)


     001 STOP


     END PROGRAM FFLUSH1EXAMPLE

