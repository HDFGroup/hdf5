
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
!   access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!
! 
!    Testing Selection-related Dataspace Interface functionality.
!

!
!    The following subroutines tests the following functionalities:
!    h5sget_select_npoints_f, h5sselect_elements_f, h5sselect_all_f,
!    h5sselect_none_f, h5sselect_valid_f, h5sselect_hyperslab_f,
!    h5sget_select_bounds_f, h5sget_select_elem_pointlist_f,
!    h5sget_select_elem_npoints_f, h5sget_select_hyper_blocklist_f, 
!    h5sget_select_hyper_nblocks_f, h5sget_select_npoints_f
!

  SUBROUTINE test_select_hyperslab(cleanup, total_error)

    USE HDF5 ! This module contains all necessary modules 

    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(OUT) :: total_error 

    CHARACTER(LEN=7), PARAMETER :: filename = "tselect"
    CHARACTER(LEN=80) :: fix_filename

    !
    !dataset name is "IntArray"
    !
    CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray"

    INTEGER(HID_T) :: file_id       ! File identifier 
    INTEGER(HID_T) :: dset_id       ! Dataset identifier 
    INTEGER(HID_T) :: dataspace     ! Dataspace identifier 
    INTEGER(HID_T) :: memspace      ! memspace identifier 

    !
    !Memory space dimensions 
    !
    INTEGER(HSIZE_T), DIMENSION(3) :: dimsm = (/7,7,3/)

    !
    !to get Dataset dimensions 
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: dims_out

    !
    !Dataset dimensions 
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: dimsf = (/5,6/)

    !
    !Size of the hyperslab in the file 
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: count = (/3,4/)

    !
    !hyperslab offset in the file 
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: offset = (/1,2/)

    !
    !Size of the hyperslab in memory 
    !
    INTEGER(HSIZE_T), DIMENSION(3) :: count_out = (/3,4,1/)

    !
    !hyperslab offset in memory 
    !
    INTEGER(HSIZE_T), DIMENSION(3) :: offset_out = (/3,0,0/)

    !
    !data to write 
    !
    INTEGER, DIMENSION(5,6) :: data

    !
    !output buffer 
    !
    INTEGER, DIMENSION(7,7,3) :: data_out


    !
    !dataset space rank 
    !
    INTEGER :: dsetrank = 2 

    !
    !memspace rank 
    !
    INTEGER :: memrank = 3

    !
    !integer to get the dataspace rank from dataset
    !
    INTEGER :: rank 


    !
    !general purpose integer 
    !
    INTEGER :: i, j, k 

    !
    !flag to check operation success 
    !
    INTEGER :: error, error_n 
    INTEGER, DIMENSION(7) :: data_dims


    !
    !This writes data to the HDF5 file.  
    !

    !
    !data initialization 
    !
    do i = 1, 5
       do j = 1, 6
          data(i,j) = (i-1) + (j-1);
       end do
    end do
    !
    ! 0,  1,  2,  3,  4,  5
    ! 1,  2,  3,  4,  5,  6
    ! 2,  3,  4,  5,  6,  7
    ! 3,  4,  5,  6,  7,  8
    ! 4,  5,  6,  7,  8,  9
    !

    !
    !Initialize FORTRAN predifined datatypes
    !
!    CALL h5init_types_f(error) 
!    CALL check("h5init_types_f", error, total_error)

    !
    !Create a new file using default properties.
    ! 
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
    CALL check("h5fcreate_f", error, total_error)

    !
    !Create the data space for the  dataset. 
    !
    CALL h5screate_simple_f(dsetrank, dimsf, dataspace, error)
    CALL check("h5screate_simple_f", error, total_error)

    !
    ! Create the dataset with default properties
    !
    CALL h5dcreate_f(file_id, dsetname, H5T_STD_I32BE, dataspace, &
         dset_id, error)
    CALL check("h5dcreate_f", error, total_error)

    !
    ! Write the dataset
    !
    data_dims(1) = 5
    data_dims(2) = 6 
    CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, data_dims, error)
    CALL check("h5dwrite_f", error, total_error)

    !
    !Close the dataspace for the dataset.
    !
    CALL h5sclose_f(dataspace, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the dataset.
    !
    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f", error, total_error)

    !
    !Close the file.
    !
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f", error, total_error)

    !
    !This reads the hyperslab from the sds.h5 file just 
    !created, into a 2-dimensional plane of the 3-dimensional array.
    !

    !
    !initialize data_out array
    !
    !     do i = 1, 7
    !          do j = 1, 7
    !              do k = 1,3
    !                  data_out(i,j,k) = 0;
    !              end do
    !          end do
    !     end do

    !
    !Open the file.
    !
    CALL h5fopen_f (fix_filename, H5F_ACC_RDONLY_F, file_id, error)
    CALL check("h5fopen_f", error, total_error)

    !
    !Open the  dataset.
    !
    CALL h5dopen_f(file_id, dsetname, dset_id, error)
    CALL check("h5dopen_f", error, total_error)

    !
    !Get dataset's dataspace handle.
    !
    CALL h5dget_space_f(dset_id, dataspace, error)
    CALL check("h5dget_space_f", error, total_error)

    !
    !Select hyperslab in the dataset.
    !
    CALL h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, &
         offset, count, error) 
    CALL check("h5sselect_hyperslab_f", error, total_error)
    !
    !create memory dataspace.
    !
    CALL h5screate_simple_f(memrank, dimsm, memspace, error)
    CALL check("h5screate_simple_f", error, total_error)

    !
    !Select hyperslab in memory.
    !
    CALL h5sselect_hyperslab_f(memspace, H5S_SELECT_SET_F, &
         offset_out, count_out, error) 
    CALL check("h5sselect_hyperslab_f", error, total_error)

    !
    !Read data from hyperslab in the file into the hyperslab in 
    !memory and display.
    !
    data_dims(1) = 7
    data_dims(2) = 7 
    data_dims(3) = 3 
    CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error, &
         memspace, dataspace)
    CALL check("h5dread_f", error, total_error)

    !
    !Display data_out array
    !
    !do i = 1, 7
    !   print *, (data_out(i,j,1), j = 1,7)
    !end do

    ! 0 0 0 0 0 0 0
    ! 0 0 0 0 0 0 0
    ! 0 0 0 0 0 0 0
    ! 3 4 5 6 0 0 0  
    ! 4 5 6 7 0 0 0
    ! 5 6 7 8 0 0 0
    ! 0 0 0 0 0 0 0
    !

    !
    !Close the dataspace for the dataset.
    !
    CALL h5sclose_f(dataspace, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the memoryspace.
    !
    CALL h5sclose_f(memspace, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the dataset.
    !
    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f", error, total_error)

    !
    !Close the file.
    !
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f", error, total_error)


          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
    RETURN

  END SUBROUTINE test_select_hyperslab

  !
  !Subroutine to test element selection
  !

  SUBROUTINE test_select_element(cleanup, total_error)

    USE HDF5 ! This module contains all necessary modules 

    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(OUT) :: total_error 

    !
    !the dataset1 is stored in file "copy1.h5"
    !
    CHARACTER(LEN=13), PARAMETER :: filename1 = "tselect_copy1"
    CHARACTER(LEN=80) :: fix_filename1

    !
    !the dataset2 is stored in file "copy2.h5"
    !
    CHARACTER(LEN=13), PARAMETER :: filename2 = "tselect_copy2"
    CHARACTER(LEN=80) :: fix_filename2
    !
    !dataset1 name is "Copy1"
    !
    CHARACTER(LEN=8), PARAMETER :: dsetname1 = "Copy1"

    !
    !dataset2 name is "Copy2"
    !
    CHARACTER(LEN=8), PARAMETER :: dsetname2 = "Copy2"

    !
    !dataset rank 
    !
    INTEGER, PARAMETER :: RANK = 2

    !
    !number of points selected 
    !
    INTEGER(SIZE_T), PARAMETER :: NUMP = 2

    INTEGER(HID_T) :: file1_id       ! File1 identifier 
    INTEGER(HID_T) :: file2_id       ! File2 identifier 
    INTEGER(HID_T) :: dset1_id       ! Dataset1 identifier 
    INTEGER(HID_T) :: dset2_id       ! Dataset2 identifier 
    INTEGER(HID_T) :: dataspace1     ! Dataspace identifier 
    INTEGER(HID_T) :: dataspace2     ! Dataspace identifier 
    INTEGER(HID_T) :: memspace       ! memspace identifier 

    !
    !Memory space dimensions 
    !
    INTEGER(HSIZE_T), DIMENSION(1) :: dimsm = (/2/)

    !
    !Dataset dimensions 
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: dimsf = (/3,4/)

    !
    !Points positions in the file 
    !
    INTEGER(HSSIZE_T), DIMENSION(RANK,NUMP) :: coord

    !
    !data buffers 
    !
    INTEGER, DIMENSION(3,4) :: buf1, buf2, bufnew

    !
    !value to write 
    !
    INTEGER, DIMENSION(2) :: val = (/53, 59/)

    !
    !memory rank 
    !
    INTEGER :: memrank = 1 

    !
    !general purpose integer 
    !
    INTEGER :: i, j 

    !
    !flag to check operation success 
    !
    INTEGER :: error 
    LOGICAL :: status
    INTEGER, DIMENSION(7) :: data_dims


    !
    !Create two files containing identical datasets. Write 0's to one
    !and 1's to the other. 
    !

    !
    !data initialization 
    !
    do i = 1, 3
       do j = 1, 4
          buf1(i,j) = 0;
       end do
    end do

    do i = 1, 3
       do j = 1, 4
          buf2(i,j) = 1;
       end do
    end do

    !
    !Initialize FORTRAN predifined datatypes
    !
!    CALL h5init_types_f(error) 
!    CALL check("h5init_types_f", error, total_error)

    !
    !Create file1, file2  using default properties.
    ! 
          CALL h5_fixname_f(filename1, fix_filename1, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
    CALL h5fcreate_f(fix_filename1, H5F_ACC_TRUNC_F, file1_id, error)
    CALL check("h5fcreate_f", error, total_error)

          CALL h5_fixname_f(filename2, fix_filename2, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
    CALL h5fcreate_f(fix_filename2, H5F_ACC_TRUNC_F, file2_id, error)
    CALL check("h5fcreate_f", error, total_error)

    !
    !Create the data space for the  datasets. 
    !
    CALL h5screate_simple_f(RANK, dimsf, dataspace1, error)
    CALL check("h5screate_simple_f", error, total_error)

    CALL h5screate_simple_f(RANK, dimsf, dataspace2, error)
    CALL check("h5screate_simple_f", error, total_error)

    !
    ! Create the datasets with default properties
    !
    CALL h5dcreate_f(file1_id, dsetname1, H5T_NATIVE_INTEGER, dataspace1, &
         dset1_id, error)
    CALL check("h5dcreate_f", error, total_error)

    CALL h5dcreate_f(file2_id, dsetname2, H5T_NATIVE_INTEGER, dataspace2, &
         dset2_id, error)
    CALL check("h5dcreate_f", error, total_error)

    !
    ! Write the datasets
    !
    data_dims(1) = 3
    data_dims(2) = 4
    CALL h5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, buf1, data_dims, error)
    CALL check("h5dwrite_f", error, total_error)

    CALL h5dwrite_f(dset2_id, H5T_NATIVE_INTEGER, buf2, data_dims, error)
    CALL check("h5dwrite_f", error, total_error)

    !
    !Close the dataspace for the datasets.
    !
    CALL h5sclose_f(dataspace1, error)
    CALL check("h5sclose_f", error, total_error)

    CALL h5sclose_f(dataspace2, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the datasets.
    !
    CALL h5dclose_f(dset1_id, error)
    CALL check("h5dclose_f", error, total_error)

    CALL h5dclose_f(dset2_id, error)
    CALL check("h5dclose_f", error, total_error)

    !
    !Close the files.
    !
    CALL h5fclose_f(file1_id, error)
    CALL check("h5fclose_f", error, total_error)

    CALL h5fclose_f(file2_id, error)
    CALL check("h5fclose_f", error, total_error)

    !
    !Open the two files.  Select two points in one file, write values to 
    !those point locations, then do H5Scopy and write the values to the  
    !other file.  Close files.
    !

    !
    !Open the files.
    !
    CALL h5fopen_f (fix_filename1, H5F_ACC_RDWR_F, file1_id, error)
    CALL check("h5fopen_f", error, total_error)

    CALL h5fopen_f (fix_filename2, H5F_ACC_RDWR_F, file2_id, error)
    CALL check("h5fopen_f", error, total_error)

    !
    !Open the  datasets.
    !
    CALL h5dopen_f(file1_id, dsetname1, dset1_id, error)
    CALL check("h5dopen_f", error, total_error)

    CALL h5dopen_f(file2_id, dsetname2, dset2_id, error)
    CALL check("h5dopen_f", error, total_error)

    !
    !Get dataset1's dataspace handle.
    !
    CALL h5dget_space_f(dset1_id, dataspace1, error)
    CALL check("h5dget_space_f", error, total_error)

    !
    !create memory dataspace.
    !
    CALL h5screate_simple_f(memrank, dimsm, memspace, error)
    CALL check("h5screate_simple_f", error, total_error)

    !
    !Set the selected point positions.Because Fortran array index starts 
    ! from 1, so add one to the actual select points in C
    !
    coord(1,1) = 1     
    coord(2,1) = 2     
    coord(1,2) = 1     
    coord(2,2) = 4

    !
    !Select the elements in file space
    !
    CALL h5sselect_elements_f(dataspace1, H5S_SELECT_SET_F, RANK, NUMP,&
         coord, error)
    CALL check("h5sselect_elements_f", error, total_error)

    !
    !Write value into the selected points in dataset1
    !
    data_dims(1) = 2
    CALL H5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, val, data_dims, error, &
         mem_space_id=memspace, file_space_id=dataspace1)
    CALL check("h5dwrite_f", error, total_error)

    !
    !Copy the daspace1 into dataspace2
    !
    CALL h5scopy_f(dataspace1, dataspace2, error)  
    CALL check("h5scopy_f", error, total_error)

    !
    !Write value into the selected points in dataset2
    !
    CALL H5dwrite_f(dset2_id, H5T_NATIVE_INTEGER, val, data_dims, error, &
         mem_space_id=memspace, file_space_id=dataspace2)
    CALL check("h5dwrite_f", error, total_error)

    !
    !Close the dataspace for the datasets.
    !
    CALL h5sclose_f(dataspace1, error)
    CALL check("h5sclose_f", error, total_error)

    CALL h5sclose_f(dataspace2, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the memoryspace.
    !
    CALL h5sclose_f(memspace, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the datasets.
    !
    CALL h5dclose_f(dset1_id, error)
    CALL check("h5dclose_f", error, total_error)

    CALL h5dclose_f(dset2_id, error)
    CALL check("h5dclose_f", error, total_error)

    !
    !Close the files.
    !
    CALL h5fclose_f(file1_id, error)
    CALL check("h5fclose_f", error, total_error)

    CALL h5fclose_f(file2_id, error)
    CALL check("h5fclose_f", error, total_error)

    !
    !Open both files and print the contents of the datasets.
    !

    !
    !Open the files.
    !
    CALL h5fopen_f (fix_filename1, H5F_ACC_RDWR_F, file1_id, error)
    CALL check("h5fopen_f", error, total_error)

    CALL h5fopen_f (fix_filename2, H5F_ACC_RDWR_F, file2_id, error)
    CALL check("h5fopen_f", error, total_error)

    !
    !Open the  datasets.
    !
    CALL h5dopen_f(file1_id, dsetname1, dset1_id, error)
    CALL check("h5dopen_f", error, total_error)

    CALL h5dopen_f(file2_id, dsetname2, dset2_id, error)
    CALL check("h5dopen_f", error, total_error)

    !
    !Read dataset1.
    !
    data_dims(1) = 3
    data_dims(2) = 4
    CALL h5dread_f(dset1_id, H5T_NATIVE_INTEGER, bufnew, data_dims, error)
    CALL check("h5dread_f", error, total_error)

    !
    !Display the data read from dataset "Copy1"
    !
    !write(*,*) "The data in dataset Copy1 is: "
    !do i = 1, 3
    !   print *, (bufnew(i,j), j = 1,4)
    !end do

    !
    !Read dataset2.
    !
    CALL h5dread_f(dset2_id, H5T_NATIVE_INTEGER, bufnew, data_dims, error)
    CALL check("h5dread_f", error, total_error)

    !
    !Display the data read from dataset "Copy2"
    !
    !write(*,*) "The data in dataset Copy2 is: "
    !do i = 1, 3
    !   print *, (bufnew(i,j), j = 1,4)
    !end do

    !
    !Close the datasets.
    !
    CALL h5dclose_f(dset1_id, error)
    CALL check("h5dclose_f", error, total_error)

    CALL h5dclose_f(dset2_id, error)
    CALL check("h5dclose_f", error, total_error)

    !
    !Close the files.
    !
    CALL h5fclose_f(file1_id, error)
    CALL check("h5fclose_f", error, total_error)

    CALL h5fclose_f(file2_id, error)
    CALL check("h5fclose_f", error, total_error)


          if(cleanup) CALL h5_cleanup_f(filename1, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          if(cleanup) CALL h5_cleanup_f(filename2, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
     RETURN
  END SUBROUTINE  test_select_element


  SUBROUTINE test_basic_select(cleanup, total_error)
    USE HDF5 ! This module contains all necessary modules 

    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(OUT) :: total_error 

     !
     !the dataset is stored in file "testselect.h5"
     !
     CHARACTER(LEN=10), PARAMETER :: filename = "testselect"
     CHARACTER(LEN=80) :: fix_filename 

     !
     !dataspace rank 
     !
     INTEGER, PARAMETER :: RANK = 2

     !
     !select NUMP_POINTS points from the file 
     !
     INTEGER(SIZE_T), PARAMETER :: NUMPS = 10

     !
     !dataset name is "testselect"
     !
     CHARACTER(LEN=10), PARAMETER :: dsetname = "testselect"

     INTEGER(HID_T) :: file_id       ! File identifier 
     INTEGER(HID_T) :: dset_id       ! Dataset identifier 
     INTEGER(HID_T) :: dataspace     ! Dataspace identifier 
     INTEGER(HID_T) :: memspace      ! memspace identifier 

     !
     !Dataset dimensions 
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: dimsf = (/5,6/)

     !
     !Size of the hyperslab in the file 
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: count = (/2,2/)

     !
     !hyperslab offset in the file 
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: offset = (/0,0/)

     !
     !start block for getting the selected hyperslab 
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: startblock = (/0,0/)

     !
     !start point for getting the selected elements 
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: startpoint = (/0,0/)
!     INTEGER(HSIZE_T), DIMENSION(2) :: startpoint = (/1,1/)

     !
     !Stride of the hyperslab in the file 
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: stride = (/3,3/)

     !
     !BLock size of the hyperslab in the file 
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: block = (/2,2/)

     !
     !array to give selected points' coordinations 
     !
     INTEGER(HSSIZE_T), DIMENSION(RANK, NUMPS) :: coord

     !
     !Size of the hyperslab in memory 
     !
     INTEGER(HSIZE_T), DIMENSION(3) :: count_out = (/3,4,1/)

     !
     !Number of hyperslabs selected in the current dataspace 
     !
     INTEGER(HSSIZE_T) :: num_blocks

     !
     !allocatable array for putting a list of hyperslabs
     !selected in the current file dataspace 
     !
     INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: blocklist

     !
     !Number of points selected in the current dataspace 
     !
     INTEGER(HSSIZE_T) :: num_points
     INTEGER(HSIZE_T) :: num1_points

     !
     !allocatable array for putting a list of points
     !selected in the current file dataspace 
     !
     INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: pointlist

     !
     !start and end bounds in the current dataspac selection 
     !
     INTEGER(HSIZE_T), DIMENSION(RANK) :: startout, endout

     !
     !data to write 
     !
     INTEGER, DIMENSION(5,6) :: data

     !
     !output buffer 
     !
     INTEGER, DIMENSION(7,7,3) :: data_out

     !
     !general purpose integer 
     !
     INTEGER :: i, j, k 

     !
     !flag to check operation success 
     !
     INTEGER :: error, error_n 
     INTEGER, DIMENSION(7) :: data_dims

     !
     !initialize the coord array to give the selected points' position 
     !
     coord(1,1) = 1
     coord(2,1) = 1
     coord(1,2) = 1
     coord(2,2) = 3
     coord(1,3) = 1
     coord(2,3) = 5
     coord(1,4) = 3
     coord(2,4) = 1
     coord(1,5) = 3
     coord(2,5) = 3
     coord(1,6) = 3
     coord(2,6) = 5
     coord(1,7) = 4
     coord(2,7) = 3
     coord(1,8) = 4
     coord(2,8) = 1
     coord(1,9) = 5
     coord(2,9) = 3
     coord(1,10) = 5
     coord(2,10) = 5

     !
     !Create a new file using default properties.
     ! 
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
     CALL check("h5fcreate_f", error, total_error)

     !
     !Create the data space for the  dataset. 
     !
     CALL h5screate_simple_f(RANK, dimsf, dataspace, error)
     CALL check("h5screate_simple_f", error, total_error)

     !
     ! Create the dataset with default properties
     !
     CALL h5dcreate_f(file_id, dsetname, H5T_STD_I32BE, dataspace, &
                      dset_id, error)
     CALL check("h5dcreate_f", error, total_error)

     !
     ! Write the dataset
     !
     data_dims(1) = 5
     data_dims(2) = 6
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, data_dims, error)
     CALL check("h5dwrite_f", error, total_error)

     !
     !Close the dataspace for the dataset.
     !
     CALL h5sclose_f(dataspace, error)
     CALL check("h5sclose_f", error, total_error)

     !
     !Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)
     CALL check("h5dclose_f", error, total_error)

     !
     !Close the file.
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f", error, total_error)

     !
     !Open the file.
     !
     CALL h5fopen_f (fix_filename, H5F_ACC_RDONLY_F, file_id, error)
     CALL check("h5fopen_f", error, total_error)
       
     !
     !Open the  dataset.
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)
     CALL check("h5dopen_f", error, total_error)

     !
     !Get dataset's dataspace handle.
     !
     CALL h5dget_space_f(dset_id, dataspace, error)
     CALL check("h5dget_space_f", error, total_error)

     !
     !Select hyperslab in the dataset.
     !
     CALL h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, &
                                offset, count, error, stride, block) 
     CALL check("h5sselect_hyperslab_f", error, total_error)

     !
     !get the number of hyperslab blocks in the current dataspac selection 
     !
     CALL h5sget_select_hyper_nblocks_f(dataspace, num_blocks, error)
     CALL check("h5sget_select_hyper_nblocks_f", error, total_error)
     IF (num_blocks .NE. 4) write (*,*) "error occured with num_blocks"
     !write(*,*) num_blocks
     !result of num_blocks is 4

     !
     !allocate the blocklist array
     !
     ALLOCATE(blocklist(num_blocks*RANK*2), STAT= error)
     if(error .NE. 0) then
         STOP
     endif
     
     !
     !get the list of hyperslabs selected in the current dataspac selection 
     !
     CALL h5sget_select_hyper_blocklist_f(dataspace, startblock, &
                                          num_blocks, blocklist, error)
     CALL check("h5sget_select_hyper_blocklist_f", error, total_error)
     !write(*,*) (blocklist(i), i =1, num_blocks*RANK*2)
     !result of blocklist selected is:
     !1,  1,  2,  2,  4,  1,  5,  2,  1,  4,  2,  5,  4,  4,  5,  5     

     !
     !deallocate the blocklist array
     !
     DEALLOCATE(blocklist)

     !
     !get the selection bounds in the current dataspac selection 
     !
     CALL h5sget_select_bounds_f(dataspace, startout, endout, error)
     CALL check("h5sget_select_bounds_f", error, total_error)
     IF ( (startout(1) .ne. 1) .or. (startout(2) .ne. 1) ) THEN
        write(*,*) "error occured to select_bounds's start position"
     END IF

     IF ( (endout(1) .ne. 5) .or. (endout(2) .ne. 5) ) THEN
        write(*,*) "error occured to select_bounds's end position"
     END IF
     !write(*,*) (startout(i), i = 1, RANK)
     !result of startout is 0, 0

     !write(*,*) (endout(i), i = 1, RANK)
     !result of endout is 5, 5

     !
     !allocate the pointlist array
     !
!     ALLOCATE(pointlist(num_blocks*RANK), STAT= error)
     ALLOCATE(pointlist(20), STAT= error)
     if(error .NE. 0) then
         STOP
     endif

     !
     !Select the elements in file space
     !
     CALL h5sselect_elements_f(dataspace, H5S_SELECT_SET_F, RANK, NUMPS,&
                               coord, error)
     CALL check("h5sselect_elements_f", error, total_error)

     !
     !Get the number of selected elements
     !
     CALL h5sget_select_elem_npoints_f(dataspace, num_points, error)
     CALL check("h5sget_select_elem_npoints_f", error, total_error)
     IF (num_points .NE. 10) write(*,*) "error occured with num_points"
     !write(*,*) num_points 
     ! result of num_points is 10

     !
     !Get the list of selected elements
     !
     num1_points = num_points
     CALL h5sget_select_elem_pointlist_f(dataspace, startpoint, &
                                          num1_points, pointlist, error)
     CALL check("h5sget_select_elem_pointlist_f", error, total_error)
     !write(*,*) (pointlist(i), i =1, num1_points*RANK) 
     !result of pintlist is:
     !1,  1,  3,  1,  5,  1,  1,  3,  3,  3,  5,  3,  3, 
     !4,  1,  4,  3,  5,  5,  5
      
     !
     !deallocate the pointlist array
     !
     DEALLOCATE(pointlist)

     !
     !Close the dataspace for the dataset.
     !
     CALL h5sclose_f(dataspace, error)
     CALL check("h5sclose_f", error, total_error)

     !
     !Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)
     CALL check("h5dclose_f", error, total_error)

     !
     !Close the file.
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f", error, total_error)


          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)

     RETURN
  END SUBROUTINE  test_basic_select


