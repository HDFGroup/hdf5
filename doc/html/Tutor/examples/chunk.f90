!
!This example shows how to work with extendible datasets.
!It creates a 3 x 3 extendible dataset, write to that dataset, 
!extend the dataset to 10x3, and write to the dataset again
!




     PROGRAM CHUNKEXAMPLE

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE

     !
     !the dataset is stored in file "extf.h5"
     !
     CHARACTER(LEN=7), PARAMETER :: filename = "extf.h5"

     !
     !dataset name is "ExtendibleArray"
     !
     CHARACTER(LEN=15), PARAMETER :: dsetname = "ExtendibleArray"

     !
     !dataset rank is 2
     !
     INTEGER :: RANK = 2

     INTEGER(HID_T) :: file_id       ! File identifier 
     INTEGER(HID_T) :: dset_id       ! Dataset identifier 
     INTEGER(HID_T) :: dataspace     ! Dataspace identifier 
     INTEGER(HID_T) :: filespace     ! Dataspace identifier 
     INTEGER(HID_T) :: memspace      ! memspace identifier 
     INTEGER(HID_T) :: cparms        !dataset creatation property identifier 

     !
     !dataset dimensions at creation time
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/3,3/)

     !
     !data1 dimensions 
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: dims1 = (/3,3/)

     !
     !data2 dimensions
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: dims2 = (/7,1/)

     !
     !Maximum dimensions
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: maxdims 

     !
     !data1 dimensions
     !
     INTEGER, DIMENSION(3,3) :: data1

     !
     !data2 dimensions
     !
     INTEGER, DIMENSION(7,1) :: data2

     !
     !Size of the hyperslab in the file 
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: size

     !
     !hyperslab offset in the file 
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: offset

     !
     !general purpose integer 
     !
     INTEGER :: i, j, k 

     !
     !flag to check operation success 
     !
     INTEGER :: error, error_n 

     !
     !Variables used in reading data back
     !  
     INTEGER(HSIZE_T), DIMENSION(2) :: chunk_dims = (/5,2/)
     INTEGER(HSIZE_T), DIMENSION(2) :: chunk_dimsr
     INTEGER(HSIZE_T), DIMENSION(2) :: dimsr, maxdimsr
     INTEGER, DIMENSION(10,3) :: data_out
     INTEGER :: rankr, rank_chunk

     !
     !data initialization 
     !
     do i = 1, 3
          do j = 1, 3
               data1(i,j) = 1
          end do
     end do

     do j = 1, 7
         data2(j,1) = 2
     end do
   

     !
     !Initialize FORTRAN predifined datatypes
     !
     CALL h5init_types_f(error) 

     !
     !Create a new file using default properties.
     ! 
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)


     !
     !Create the data space with unlimited dimensions.
     !
     maxdims = (/H5S_UNLIMITED_f, H5S_UNLIMITED_f/)

     CALL h5screate_simple_f(RANK, dims, dataspace, error, maxdims)
   
     !
     !Modify dataset creation properties, i.e. enable chunking
     !
     CALL h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)

     CALL h5pset_chunk_f(cparms, RANK, chunk_dims, error)

     !
     !Create a new dataset within the file using cparms creation properties.
     !
     !CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INT_F, dataspace, &
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
                      dset_id, error, cparms)
   
     !
     !Extend the dataset. This call assures that dataset is 3 x 3.
     !
     size(1) = 3
     size(2) = 3
     CALL h5dextend_f(dset_id, size, error)

   
     !
     !Select a hyperslab.
     !
     CALL h5dget_space_f(dset_id, filespace, error)
     offset(1) = 0;
     offset(2) = 0;
     CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, &
                                offset, dims1, error) 

     !
     !Write the data to the hyperslab.
     !
     !CALL H5Dwrite_f(dset_id, H5T_NATIVE_INT_F, data1, error, &
     CALL H5Dwrite_f(dset_id, H5T_NATIVE_INTEGER, data1, error, &
                    filespace, dataspace)

     !
     !Extend the dataset. Dataset becomes 10 x 3.
     !
     dims(1)   = dims1(1) + dims2(1);
     size(1)   = dims(1);  
     size(2)   = dims(2); 
     CALL h5dextend_f(dset_id, size, error)

     !
     !Select a hyperslab.
     !
     CALL h5dget_space_f(dset_id, filespace, error)
     offset(1) = 3;
     offset(2) = 0;
     CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, &
                                offset, dims2, error) 

     !
     !create memory dataspace.
     !
     CALL h5screate_simple_f(RANK, dims2, memspace, error)

     !
     !Write the data to the hyperslab.
     !
     !CALL H5Dwrite_f(dset_id, H5T_NATIVE_INT_F, data2, error, &
     CALL H5Dwrite_f(dset_id, H5T_NATIVE_INTEGER, data2, error, &
                   mem_space_id=memspace, file_space_id=filespace)

     !
     !Close the dataspace for the dataset.
     !
     CALL h5sclose_f(dataspace, error)
     CALL h5sclose_f(filespace, error)

     !
     !Close the memoryspace.
     !
     CALL h5sclose_f(memspace, error)

     !
     !Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)

     !
     !Close the property list.
     !
     CALL h5pclose_f(cparms, error)

     !
     !Close the file.
     !
     CALL h5fclose_f(file_id, error)

     !
     !read the data back
     !
     !Open the file.
     !
     CALL h5fopen_f (filename, H5F_ACC_RDONLY_F, file_id, error)
       
     !
     !Open the  dataset.
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)

     !
     !Get dataset's dataspace handle.
     !
     CALL h5dget_space_f(dset_id, dataspace, error)

     !
     !Get dataspace's rank.
     !
     CALL h5sget_simple_extent_ndims_f(dataspace, rankr, error)


     !
     !Get dataspace's dimensinons.
     !
     CALL h5sget_simple_extent_dims_f(dataspace, dimsr, maxdimsr, error)


     !
     !Get creation property list.
     !
     CALL h5dget_create_plist_f(dset_id, cparms, error)

     !
     !Get chunk dimensions.
     !
     CALL h5pget_chunk_f(cparms, 2, chunk_dimsr, error)

     !
     !create memory dataspace.
     !
     CALL h5screate_simple_f(rankr, dimsr, memspace, error)
 
     !
     !Read data 
     !
     !CALL H5Dread_f(dset_id, H5T_NATIVE_INT_F, data_out, error, &
     CALL H5Dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, error, &
                    memspace, dataspace)
 
     !
     !Print data 
     !
     do i = 1, dimsr(1)
         print *, (data_out(i,j), j = 1,dimsr(2))
     end do
 
     !
     !Close the dataspace for the dataset.
     !
     CALL h5sclose_f(dataspace, error)

     !
     !Close the memoryspace.
     !
     CALL h5sclose_f(memspace, error)

     !
     !Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)

     !
     !Close the file.
     !
     CALL h5fclose_f(file_id, error)

     !
     !Close the property list.
     !
     CALL h5pclose_f(cparms, error)

     !
     ! Close FORTRAN predefined datatypes.
     !
     CALL h5close_types_f(error)

     END PROGRAM CHUNKEXAMPLE 
