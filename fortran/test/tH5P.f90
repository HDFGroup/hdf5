! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!   access to either file, you may request a copy from help@hdfgroup.org.     *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!
    SUBROUTINE external_test(cleanup, total_error)

!   This subroutine tests following functionalities: 
!   h5pset_external_f,  h5pget_external_count_f,
!   h5pget_external_f

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(OUT) :: total_error 

     CHARACTER(LEN=8), PARAMETER :: filename = "external"
     CHARACTER(LEN=80) :: fix_filename
     INTEGER(HID_T) :: file_id     
     INTEGER(HID_T) :: plist_id     
     INTEGER(HID_T) :: space_id     
     INTEGER(HID_T) :: dataset_id
     INTEGER(HSIZE_T), DIMENSION(1) :: cur_size !data space current size
     INTEGER(HSIZE_T), DIMENSION(1) :: max_size !data space maximum size
     CHARACTER(LEN=256) :: name !external file name
     INTEGER :: file_offset !external file offset
     INTEGER(HSIZE_T) :: file_size   !sizeof external file segment
     INTEGER :: error !error code
     INTEGER(SIZE_T) :: int_size !size of integer
     INTEGER(HSIZE_T) :: file_bytes !Number of bytes reserved 
                                   !in the file for the data
     INTEGER :: RANK = 1 !dataset rank
     INTEGER :: count !number of external files for the
                      !specified dataset
     INTEGER(SIZE_T) :: namesize
     INTEGER(HSIZE_T) :: size, buf_size

     buf_size = 4*1024*1024



     !
     !Create file "external.h5" using default properties.
     ! 
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
     CALL check("h5fcreate_f",error,total_error)


     CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
         CALL check("h5pcreate_f", error, total_error)
     CALL h5pset_buffer_f(plist_id, buf_size, error)
         CALL check("h5pset_buffer_f", error, total_error)
     CALL h5pget_buffer_f(plist_id, size, error)
         CALL check("h5pget_buffer_f", error, total_error)
     if (size .ne.buf_size) then
         total_error = total_error + 1
         write(*,*) "h5pget_buffer_f returned wrong size, error"
      endif
     CALL h5pclose_f(plist_id, error)
         CALL check("h5pclose_f", error, total_error)

     CALL h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
     CALL check("h5pcreate_f",error,total_error)
     cur_size(1) =100
     max_size(1) = 100;
     call h5tget_size_f(H5T_NATIVE_INTEGER, int_size, error)
     CALL check("h5tget_size_f",error,total_error)
     file_size = int_size * max_size(1);     
     CALL h5pset_external_f(plist_id, "ext1.data", 0, file_size, error)
     CALL check("h5pset_external_f",error,total_error)
     CALL h5screate_simple_f(RANK, cur_size, space_id, error, max_size)
     CALL check("h5screate_simple_f", error, total_error)
     CALL h5dcreate_f(file_id, "dset1", H5T_NATIVE_INTEGER, space_id, &
                           dataset_id, error, plist_id)
     CALL check("h5dcreate_f", error, total_error)
     
     CALL h5dclose_f(dataset_id, error)
     CALL check("h5dclose_f", error, total_error)
     CALL h5pclose_f(plist_id, error)
     CALL check("h5pclose_f", error, total_error)
     CALL h5sclose_f(space_id, error)
     CALL check("h5sclose_f", error, total_error)
     ! Read dataset creation information 
     CALL h5dopen_f(file_id, "dset1", dataset_id, error)
     CALL check("h5dopen_f",error,total_error)
    
     CALL h5dget_create_plist_f(dataset_id, plist_id, error)
     CALL check("h5dget_create_plist_f",error,total_error)
     CALL h5pget_external_count_f(plist_id, count, error)
     CALL check("h5pget_external_count_f",error,total_error)
     if(count .ne. 1 ) then
         write (*,*) "got external_count is not correct"
         total_error = total_error + 1
     end if
     namesize = 10
     CALL h5pget_external_f(plist_id, 0, namesize, name, file_offset, &
                            file_bytes, error)
     CALL check("h5pget_external_f",error,total_error)
     if(file_offset .ne. 0 ) then
         write (*,*) "got external file offset is not correct"
         total_error = total_error + 1
     end if
     if(file_bytes .ne. file_size ) then
         write (*,*) "got external file size is not correct"
         total_error = total_error + 1
     end if
 
     CALL h5dclose_f(dataset_id, error)
     CALL check("h5dclose_f", error, total_error)
     CALL h5pclose_f(plist_id, error)
     CALL check("h5pclose_f", error, total_error)
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f", error, total_error)


          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
     RETURN
     END SUBROUTINE external_test
    
        SUBROUTINE multi_file_test(cleanup, total_error)
        USE HDF5 ! This module contains all necessary modules 

          IMPLICIT NONE
          LOGICAL, INTENT(IN) :: cleanup
          INTEGER, INTENT(OUT) :: total_error 

          CHARACTER(LEN=9), PARAMETER :: filename = "multidset" ! File name
          CHARACTER(LEN=80) :: fix_filename 
          CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"     ! Dataset name

          INTEGER(HID_T) :: file_id       ! File identifier 
          INTEGER(HID_T) :: dset_id       ! Dataset identifier 
          INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
          INTEGER(HID_T) :: dtype_id      ! Datatype identifier
          INTEGER(HID_T) :: fapl, fapl_1  ! File access property list identifier
          INTEGER(HID_T) :: driver
          INTEGER, DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: memb_map, memb_map_out
          INTEGER(HID_T), DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: memb_fapl, memb_fapl_out
          CHARACTER(LEN=20), DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: memb_name, memb_name_out
          REAL, DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: memb_addr, memb_addr_out 
          !INTEGER(HADDR_T), DIMENSION(0:H5FD_MEM_NTYPES_F) :: memb_addr
          LOGICAL :: relax  = .TRUE.
          LOGICAL :: relax_out = .TRUE.

          INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/4,6/) ! Dataset dimensions
          INTEGER     ::   rank = 2                        ! Dataset rank

          INTEGER, DIMENSION(4,6) :: dset_data, data_out ! Data buffers
          INTEGER     ::   error ! Error flag
 

          INTEGER     :: i, j    !general purpose integers
          INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
          INTEGER :: mdc_nelmts
          INTEGER(SIZE_T) :: rdcc_nelmts
          INTEGER(SIZE_T) :: rdcc_nbytes
          REAL :: rdcc_w0
          memb_fapl = H5P_DEFAULT_F
          memb_map = H5FD_MEM_SUPER_F
          memb_addr = 0.
          memb_map(H5FD_MEM_SUPER_F) = H5FD_MEM_SUPER_F
          memb_addr(H5FD_MEM_SUPER_F) = 0.
          memb_map(H5FD_MEM_BTREE_F) = H5FD_MEM_BTREE_F
          memb_addr(H5FD_MEM_BTREE_F) = 0.1
          memb_map(H5FD_MEM_DRAW_F)  = H5FD_MEM_DRAW_F
          memb_addr(H5FD_MEM_DRAW_F) = 0.5
          memb_map(H5FD_MEM_GHEAP_F) = H5FD_MEM_GHEAP_F
          memb_addr(H5FD_MEM_GHEAP_F) = 0.2
          memb_map(H5FD_MEM_LHEAP_F) = H5FD_MEM_LHEAP_F
          memb_addr(H5FD_MEM_LHEAP_F) = 0.3
          memb_map(H5FD_MEM_OHDR_F)  = H5FD_MEM_OHDR_F
          memb_addr(H5FD_MEM_OHDR_F) = 0.4

          memb_name = ''
          memb_name(H5FD_MEM_SUPER_F) = '%s-s.h5'
          memb_name(H5FD_MEM_BTREE_F) = '%s-b.h5'
          memb_name(H5FD_MEM_DRAW_F)  = '%s-r.h5'
          memb_name(H5FD_MEM_GHEAP_F) = '%s-g.h5'
          memb_name(H5FD_MEM_LHEAP_F) = '%s-l.h5'
          memb_name(H5FD_MEM_OHDR_F)  = '%s-o.h5'

          !
          ! Initialize the dset_data array.
          !
          do i = 1, 4
             do j = 1, 6
                dset_data(i,j) = (i-1)*6 + j;
             end do
          end do

          !
          ! Create a new file using default properties.
          ! 
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
          CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
              CALL check("h5pcreate_f", error, total_error)
          CALL h5pset_fapl_multi_f(fapl, memb_map, memb_fapl, memb_name, memb_addr, relax, error)
              CALL check("h5pset_fapl_multi_f", error, total_error)
          CALL h5pget_fapl_multi_f(fapl, memb_map_out, memb_fapl_out, memb_name_out, &
                                   memb_addr_out, relax_out, error)
              CALL check("h5pget_fapl_multi_f", error, total_error)
          CALL h5pget_driver_f(fapl, driver, error)
              CALL check("h5pget_driver_f",error, total_error)
          if(driver .ne. H5FD_MULTI_F) then
             write(*,*) "Wrong value for driver"
          endif
          !
          ! Let's check h5pget(set)cache_f APIs here for now 
          !
          CALL h5pget_cache_f(fapl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, &
                              rdcc_w0, error)
               CALL check("h5pget_cache_f", error, total_error)
 
          ! Set cache to some number
          !
          rdcc_nbytes = 1024*1024
          CALL h5pset_cache_f(fapl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, &
                              rdcc_w0, error)
               CALL check("h5pset_cache_f", error, total_error)
 
          CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error, access_prp = fapl)
              CALL check("h5fcreate_f", error, total_error)


          ! 
          ! Create the dataspace.
          !
          CALL h5screate_simple_f(rank, dims, dspace_id, error)
              CALL check("h5screate_simple_f", error, total_error)


          !
          ! Create the dataset with default properties.
          !
          CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dspace_id, &
                           dset_id, error)
              CALL check("h5dcreate_f", error, total_error)

          !
          ! Write the dataset.
          !
          data_dims(1) = 4
          data_dims(2) = 6 
          CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, dset_data, data_dims, error)
              CALL check("h5dwrite_f", error, total_error)


          !   
          ! End access to the dataset and release resources used by it.
          ! 
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f", error, total_error)

          !
          ! Terminate access to the data space.
          !
          CALL h5sclose_f(dspace_id, error)
              CALL check("h5sclose_f", error, total_error)

          ! 
          ! Close the file.
          !
          CALL h5fclose_f(file_id, error)
              CALL check("h5fclose_f", error, total_error)
          CALL h5pclose_f(fapl, error)
              CALL check("h5pclose_f", error, total_error)

         ! 
          ! Open the existing file.
          !
          CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
              CALL check("h5pcreate_f", error, total_error)
          CALL h5pset_fapl_multi_f(fapl, relax, error)
              CALL check("h5pset_fapl_multi_f", error, total_error)
          CALL h5fopen_f (fix_filename, H5F_ACC_RDWR_F, file_id, error, access_prp = fapl)
              CALL check("h5fopen_f", error, total_error)
          !
          CALL h5fget_access_plist_f(file_id, fapl_1, error)
              CALL check("h5fget_access_plist_f", error, total_error)
          !It doesn't work on Windows.
          !CALL h5pget_fapl_multi_f(fapl_1, memb_map_out, memb_fapl_out, memb_name_out, &
          !                         memb_addr_out, relax_out, error)
          ! write(*,*)  memb_map_out
          ! write(*,*)  memb_fapl_out
          ! write(*,*)  memb_name_out
          ! write(*,*)  memb_addr_out
          !    CALL check("h5pget_fapl_multi_f", error, total_error)

          !
          ! Open the existing dataset. 
          !
          CALL h5dopen_f(file_id, dsetname, dset_id, error)
              CALL check("h5dopen_f", error, total_error)

          !
          ! Get the dataset type. 
          !
          CALL h5dget_type_f(dset_id, dtype_id, error)
              CALL check("h5dget_type_f", error, total_error)

          !
          ! Get the data space. 
          !
          CALL h5dget_space_f(dset_id, dspace_id, error)
              CALL check("h5dget_space_f", error, total_error)

          !
          ! Read the dataset.
          !
          CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error)
              CALL check("h5dread_f", error, total_error)

          !
          !Compare the data.
          ! 
          do i = 1, 4
              do j = 1, 6
                  IF (data_out(i,j) .NE. dset_data(i, j)) THEN 
                      write(*, *) "dataset test error occured"
                      write(*,*) "data read is not the same as the data writen"
                  END IF
              end do    
          end do

          !   
          ! End access to the dataset and release resources used by it.
          ! 
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f", error, total_error)

          !
          ! Terminate access to the data space.
          !
          CALL h5sclose_f(dspace_id, error)
              CALL check("h5sclose_f", error, total_error)

          !
          ! Terminate access to the data type.
          !
          CALL h5tclose_f(dtype_id, error)
              CALL check("h5tclose_f", error, total_error)
          ! 
          ! Close the file.
          !
          CALL h5fclose_f(file_id, error)
          CALL check("h5fclose_f", error, total_error)
          CALL h5pclose_f(fapl, error)
          CALL check("h5pclose_f", error, total_error)
          CALL h5pclose_f(fapl_1, error)
          CALL check("h5pclose_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)
          
          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-b', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-g', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-l', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-o', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-r', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-s', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)          

          RETURN
        END SUBROUTINE multi_file_test
