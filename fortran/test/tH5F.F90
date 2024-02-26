!***rh* root/fortran/test/tH5F.f90
!
! NAME
!  tH5F.f90
!
! FUNCTION
!  Basic testing of Fortran H5F APIs.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! CONTAINS SUBROUTINES
!  mountingtest, reopentest, get_name_test, plisttest, 
!  file_close, file_space, h5openclose, test_get_file_image
!
!*****
!
!  In the mountingtest subroutine we create one file with a group in it,
!  and another file with a dataset. Mounting is used to
!  access the dataset from the second file as a member of a group
!  in the first file.

! *****************************************
! ***        H 5 F   T E S T S
! *****************************************

MODULE TH5F

  USE HDF5
  USE TH5_MISC
  USE TH5_MISC_GEN
  USE ISO_C_BINDING

CONTAINS

  SUBROUTINE h5openclose(total_error)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: total_error

    !
    ! flag to check operation success
    !
    INTEGER :: error
    INTEGER(SIZE_T) :: obj_count ! open object count
    INTEGER, DIMENSION(1:5) :: obj_type ! open object type to check
    INTEGER :: i, j

    DO j = 1, 2
       CALL h5open_f(error)
       CALL check("h5open_f",error,total_error)

       obj_type(1) = H5F_OBJ_ALL_F
       obj_type(2) = H5F_OBJ_FILE_F
       obj_type(3) = H5F_OBJ_GROUP_F
       obj_type(4) = H5F_OBJ_DATASET_F
       obj_type(5) = H5F_OBJ_DATATYPE_F

       CALL h5close_f(error)
       CALL check("h5close_f",error,total_error)
       ! Check all the datatypes created during h5open_f are closed in h5close_f
       DO i = 1, 5
          CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), obj_type(i), obj_count, error)
          CALL check("h5fget_obj_count_f",error,total_error)
          IF(obj_count.NE.0)THEN
             total_error = total_error + 1
          ENDIF
       ENDDO
    ENDDO

    ! Test calling h5open_f multiple times without calling h5close_f
    DO j = 1, 4
       CALL h5open_f(error)
       CALL check("h5open_f",error,total_error)
    ENDDO

    CALL h5close_f(error)
    CALL check("h5close_f",error,total_error)
    ! Check all the datatypes created during h5open_f are closed in h5close_f
    DO i = 1, 5
       CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), obj_type(i), obj_count, error)
       CALL check("h5fget_obj_count_f",error,total_error)
       IF(obj_count.NE.0)THEN
          total_error = total_error + 1
       ENDIF
    ENDDO

    ! Test calling h5open_f multiple times with a h5close_f in the series of h5open_f
    DO j = 1, 5
       CALL h5open_f(error)
       CALL check("h5open_f",error,total_error)
       IF(j.EQ.3)THEN
          CALL h5close_f(error)
          CALL check("h5close_f",error,total_error)
          ! Check all the datatypes created during h5open_f are closed in h5close_f
          DO i = 1, 5
             CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), obj_type(i), obj_count, error)
             CALL check("h5fget_obj_count_f",error,total_error)
             IF(obj_count.NE.0)THEN
                total_error = total_error + 1
             ENDIF
          ENDDO
       ENDIF
    ENDDO

    CALL h5close_f(error)
    CALL check("h5close_f",error,total_error)
    ! Check all the datatypes created during h5open_f are closed in h5close_f
    DO i = 1, 5
       CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), obj_type(i), obj_count, error)
       CALL check("h5fget_obj_count_f",error,total_error)
       IF(obj_count.NE.0)THEN
          total_error = total_error + 1
       ENDIF
    ENDDO

    ! Check calling h5close_f after already calling h5close_f
    CALL h5close_f(error)
    CALL check("h5close_f",error,total_error)
    ! Check all the datatypes created during h5open_f are closed in h5close_f
    DO i = 1, 5
       CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), obj_type(i), obj_count, error)
       CALL check("h5fget_obj_count_f",error,total_error)
       IF(obj_count.NE.0)THEN
          total_error = total_error + 1
       ENDIF
    ENDDO

    RETURN
  END SUBROUTINE h5openclose

  SUBROUTINE mountingtest(cleanup, total_error)
    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(INOUT) :: total_error

    !
    ! the respective filenames are "mount1.h5" and "mount2.h5"
    !
    CHARACTER(LEN=6)  :: filename1
    CHARACTER(LEN=6)  :: filename2
    CHARACTER(LEN=80) :: fix_filename1
    CHARACTER(LEN=80) :: fix_filename2

    !
    ! data space rank and dimensions
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
    INTEGER     ::   fintent

    !
    !general purpose integer
    !
    INTEGER     ::   i, j

    !number of objects
    INTEGER(SIZE_T) :: obj_count
    INTEGER(HID_T) :: t1, t2, t3, t4

    ! File numbers
    INTEGER  :: file_num1
    INTEGER  :: file_num2

    !
    !data buffers
    !
    INTEGER, DIMENSION(NX,NY) :: data_in, data_out
    INTEGER(HSIZE_T), DIMENSION(2) :: data_dims

    filename1 = "mount1"
    filename2 = "mount2"

    do i = 1,80
       fix_filename1(i:i) = " "
       fix_filename2(i:i) = " "
    enddo
    !
    !Initialize data_in buffer
    !
    do j = 1, NY
       do i = 1, NX
          data_in(i,j) =  (i-1) + (j-1)
       end do
    end do

    !
    ! Fix names of the files
    !
    CALL h5_fixname_f(filename1, fix_filename1, H5P_DEFAULT_F, error)
    if(error .ne. 0) stop
    CALL h5_fixname_f(filename2, fix_filename2, H5P_DEFAULT_F, error)
    if(error .ne. 0) stop

    ! Test object counts
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, t1, error)
    CALL check(" h5tcopy_f",error,total_error)
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, t2, error)
    CALL check(" h5tcopy_f",error,total_error)
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, t3, error)
    CALL check(" h5tcopy_f",error,total_error)
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, t4, error)
    CALL check(" h5tcopy_f",error,total_error)

    CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), H5F_OBJ_ALL_F, obj_count,  error)
    CALL check(" h5fget_obj_count_f",error,total_error)

    IF(obj_count.NE.4)THEN
       total_error = total_error + 1
    ENDIF

    !
    !Create first file "mount1.h5" using default properties.
    !
    CALL h5fcreate_f(fix_filename1, H5F_ACC_TRUNC_F, file1_id, error)
    CALL check("h5fcreate_f",error,total_error)

    CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), H5F_OBJ_ALL_F, obj_count,  error)
    CALL check(" h5fget_obj_count_f",error,total_error)

    IF(obj_count.NE.5)THEN
       total_error = total_error + 1
    ENDIF

    CALL h5tclose_f(t1, error)
    CALL check("h5tclose_f",error,total_error)
    CALL h5tclose_f(t2, error)
    CALL check("h5tclose_f",error,total_error)
    CALL h5tclose_f(t3, error)
    CALL check("h5tclose_f",error,total_error)
    CALL h5tclose_f(t4, error)
    CALL check("h5tclose_f",error,total_error)

    CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), H5F_OBJ_ALL_F, obj_count,  error)
    CALL check(" h5fget_obj_count_f",error,total_error)

    IF(obj_count.NE.1)THEN
       total_error = total_error + 1
    ENDIF

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
    CALL h5fcreate_f(fix_filename2, H5F_ACC_TRUNC_F, file2_id, error)
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
    data_dims(1) = NX
    data_dims(2) = NY
    CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data_in, data_dims, error)
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
    !test whether files are accessible as HDF5 (new, VOL-safe, way)
    !
    CALL h5fis_accessible_f(fix_filename1, status, error)
    CALL check("h5fis_accessible_f",error,total_error)
    IF ( .NOT. status ) THEN
       write(*,*) "File ", fix_filename1, " is not accessible as hdf5"
       stop
    END IF

    CALL h5fis_accessible_f(fix_filename2, status, error)
    CALL check("h5fis_accessible_f",error,total_error)
    IF ( .NOT. status ) THEN
       write(*,*) "File ", fix_filename2, " is not accessible as hdf5"
       stop
    END IF

    !
    !test whether files are in hdf5 format (old way)
    !
    CALL h5fis_hdf5_f(fix_filename1, status, error)
    CALL check("h5fis_hdf5_f",error,total_error)
    IF ( .NOT. status ) THEN
       write(*,*) "File ", fix_filename1, " is not in hdf5 format"
       stop
    END IF

    CALL h5fis_hdf5_f(fix_filename2, status, error)
    CALL check("h5fis_hdf5_f",error,total_error)
    IF ( .NOT. status ) THEN
       write(*,*) "File ", fix_filename2, " is not in hdf5 format"
       stop
    END IF

    !
    !reopen both files.
    !
    CALL h5fopen_f (fix_filename1, H5F_ACC_RDWR_F, file1_id, error)
    CALL check("hfopen_f",error,total_error)

    CALL h5fget_intent_f(file1_id, fintent, error)
    CALL check("h5fget_intent_f",error,total_error)

    IF(fintent.NE.H5F_ACC_RDWR_F)THEN
         total_error = total_error + 1
    ENDIF

    CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), H5F_OBJ_ALL_F, obj_count,  error)
    CALL check(" h5fget_obj_count_f",error,total_error)

    IF(obj_count.NE.1)THEN
       total_error = total_error + 1
    ENDIF

    CALL h5fopen_f (fix_filename2, H5F_ACC_RDWR_F, file2_id, error)
    CALL check("h5fopen_f",error,total_error)

    CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), H5F_OBJ_ALL_F, obj_count,  error)
    CALL check(" h5fget_obj_count_f",error,total_error)
    IF(obj_count.NE.2)THEN
       total_error = total_error + 1
    ENDIF

    !
    !Check file numbers
    !
    CALL h5fget_fileno_f(file1_id, file_num1, error)
    CALL check("h5fget_fileno_f",error,total_error)
    CALL h5fget_fileno_f(file2_id, file_num2, error)
    CALL check("h5fget_fileno_f",error,total_error)
    IF(file_num1 .EQ. file_num2) THEN
       write(*,*) "file numbers aren't supposed to match"
    END IF

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
    CALL h5dread_f(dset_id, dtype_id, data_out, data_dims, error)
    CALL check("h5dread_f",error,total_error)

    !
    !Compare the data.
    !
    do i = 1, NX
       do j = 1, NY
          IF (data_out(i,j) .NE. data_in(i, j)) THEN
             total_error = total_error + 1
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

    CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), H5F_OBJ_ALL_F, obj_count,  error)
    CALL check(" h5fget_obj_count_f",error,total_error)

    IF(obj_count.NE.2)THEN
       total_error = total_error + 1
    ENDIF

    CALL h5fclose_f(file1_id, error)
    CALL check("h5fclose_f",error,total_error)
    CALL h5fclose_f(file2_id, error)
    CALL check("h5fclose_f",error,total_error)

    CALL h5fget_obj_count_f(INT(H5F_OBJ_ALL_F,HID_T), H5F_OBJ_ALL_F, obj_count,  error)
    CALL check(" h5fget_obj_count_f",error,total_error)

    IF(obj_count.NE.0)THEN
       total_error = total_error + 1
    ENDIF

    IF(cleanup) CALL h5_cleanup_f(filename1, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)
    IF(cleanup) CALL h5_cleanup_f(filename2, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)

    RETURN
  END SUBROUTINE mountingtest

  !
  !    The following subroutine tests h5freopen_f.
  !    It creates the file which has name "reopen.h5" and
  !    the "/dset" dataset inside the file.
  !    writes the data to the file, close the dataset.
  !    Reopen the file based upon the file_id, open the
  !    dataset use the reopen_id then reads the
  !    dataset back to memory to test whether the data
  !    read is identical to the data written
  !

  SUBROUTINE reopentest(cleanup, total_error)
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(INOUT) :: total_error

    !
    CHARACTER(LEN=6), PARAMETER :: filename = "reopen"
    CHARACTER(LEN=80)  :: fix_filename

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
    INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
    INTEGER(HSIZE_T)  :: file_size
    INTEGER  :: file_num1
    INTEGER  :: file_num2
    CHARACTER(LEN=80) :: file_name
    INTEGER(SIZE_T) :: name_size

    !
    !initialize the dset_data array which will be written to the "/dset"
    !
    do j = 1, NY
       do i = 1, NX
          dset_data(i,j) = (i-1)*6 + j;
       end do
    end do

    !
    !Create file "reopen.h5" using default properties.
    !
    CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
    if (error .ne. 0) then
       write(*,*) "Cannot modify filename"
       stop
    endif
    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
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
    data_dims(1) = NX
    data_dims(2) = NY
    CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, dset_data, data_dims, error)
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
    !Check file size
    !
    CALL h5fget_filesize_f(file_id, file_size, error)
    CALL check("h5fget_filesize_f",error,total_error)

    !
    !Check file numbers
    !
    CALL h5fget_fileno_f(file_id, file_num1, error)
    CALL check("h5fget_fileno_f",error,total_error)
    CALL h5fget_fileno_f(reopen_id, file_num2, error)
    CALL check("h5fget_fileno_f",error,total_error)
    IF(file_num1 .NE. file_num2) THEN
       write(*,*) "file numbers don't match"
    END IF

    !
    !Open the dataset based on the reopen_id.
    !
    CALL h5dopen_f(reopen_id, dsetname, dset_id, error)
    CALL check("h5dopen_f",error,total_error)
    !
    !Get file name from the dataset identifier
    !
    CALL h5fget_name_f(dset_id, file_name, name_size, error)
    CALL check("h5fget_name_f",error,total_error)
    IF(file_name(1:name_size) .NE. fix_filename(1:name_size)) THEN
       write(*,*) "file name obtained from the dataset id is incorrect"
    END IF

    !
    !Read the dataset.
    !
    CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error)
    CALL check("h5dread_f",error,total_error)

    !
    !Compare the data.
    !
    do i = 1, NX
       do j = 1, NY
          IF (data_out(i,j) .NE. dset_data(i, j)) THEN
             write(*, *) "reopen test error occurred"
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


    if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)
    RETURN

  END SUBROUTINE reopentest

  !    The following subroutine checks that h5fget_name_f produces
  !    correct output for a given obj_id and filename.
  !
  SUBROUTINE check_get_name(obj_id, fix_filename, len_filename, total_error)
    IMPLICIT NONE
    INTEGER(HID_T) :: obj_id                          ! Object identifier
    CHARACTER(LEN=80), INTENT(IN) :: fix_filename     ! Expected filename
    INTEGER, INTENT(IN) :: len_filename               ! The length of the filename
    INTEGER, INTENT(INOUT) :: total_error             ! Error count

    CHARACTER(LEN=80):: file_name  ! Filename buffer
    INTEGER:: error                ! HDF5 error code
    INTEGER(SIZE_T):: name_size    ! Filename length

    INTEGER, PARAMETER :: sm_len = 2
    CHARACTER(LEN=len_filename) :: filename_exact
    CHARACTER(LEN=len_filename-sm_len) :: filename_sm

    !
    !Get file name from the dataset identifier
    !

    ! Use an uninitialized buffer
    CALL h5fget_name_f(obj_id, file_name, name_size, error)
    CALL check("h5fget_name_f",error,total_error)
    IF(name_size .NE. LEN_TRIM(fix_filename))THEN
       WRITE(*,*) "  file name size obtained from the object id is incorrect"
       total_error = total_error + 1
    ENDIF
    IF(file_name(1:name_size) .NE. TRIM(fix_filename)) THEN
       WRITE(*,*) "  file name obtained from the object id is incorrect"
       total_error = total_error + 1
    END IF

    ! Use a buffer initialized with spaces
    file_name(:) = " "
    CALL h5fget_name_f(obj_id, file_name, name_size, error)
    CALL check("h5fget_name_f",error,total_error)
    IF(name_size .NE. LEN_TRIM(fix_filename))THEN
       WRITE(*,*) "  file name size obtained from the object id is incorrect"
       total_error = total_error + 1
    ENDIF
    IF(file_name(1:name_size) .NE. TRIM(fix_filename)) THEN
       WRITE(*,*) "  file name obtained from the object id is incorrect"
       total_error = total_error + 1
    END IF

    ! Use a buffer initialized with non-whitespace characters
    file_name(:) = "a"
    CALL h5fget_name_f(obj_id, file_name, name_size, error)
    CALL check("h5fget_name_f",error,total_error)
    IF(name_size .NE. LEN_TRIM(fix_filename))THEN
       WRITE(*,*) "  file name size obtained from the object id is incorrect"
       total_error = total_error + 1
    ENDIF
    IF(file_name(1:name_size) .NE. TRIM(fix_filename)) THEN
       WRITE(*,*) "  file name obtained from the object id is incorrect"
       total_error = total_error + 1
    END IF

    ! Use a buffer which is the exact size needed to hold the filename
    CALL h5fget_name_f(obj_id, filename_exact, name_size, error)
    CALL check("h5fget_name_f",error,total_error)
    IF(name_size .NE. len_filename)THEN
       WRITE(*,*) "  file name size obtained from the object id is incorrect"
       total_error = total_error + 1
    ENDIF
    IF(filename_exact .NE. TRIM(fix_filename)) THEN
       WRITE(*,*) "  file name obtained from the object id is incorrect"
       total_error = total_error + 1
    END IF

    ! Use a buffer which is smaller than needed to hold the filename
    CALL h5fget_name_f(obj_id, filename_sm, name_size, error)
    CALL check("h5fget_name_f",error,total_error)
    IF(name_size .NE. len_filename)THEN
       WRITE(*,*) "  file name size obtained from the object id is incorrect"
       total_error = total_error + 1
    ENDIF
    IF(filename_sm(1:len_filename-sm_len) .NE. fix_filename(1:len_filename-sm_len)) THEN
       WRITE(*,*) "  file name obtained from the object id is incorrect"
       total_error = total_error + 1
    END IF

  END SUBROUTINE check_get_name

  !    The following subroutine tests h5fget_name_f.
  !    It creates the file which has name "filename.h5" and
  !    tests that h5fget_name_f also returns the name "filename.h5"
  !

  SUBROUTINE get_name_test(cleanup, total_error)
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(INOUT) :: total_error

    CHARACTER(LEN=*), PARAMETER :: filename = "filename"
    CHARACTER(LEN=80)  :: fix_filename
    INTEGER :: len_filename

    INTEGER(HID_T) :: file_id          ! File identifier
    INTEGER(HID_T) :: g_id             ! Group identifier

    !
    ! Flag to check operation success
    !
    INTEGER :: error

    !
    ! Create file "filename.h5" using default properties.
    !
    CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
    IF (error .NE. 0) THEN
       WRITE(*,*) "Cannot modify filename"
       STOP
    ENDIF
    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
    CALL check("h5fcreate_f",error,total_error)

    !
    ! Create group.
    !
    CALL h5gopen_f(file_id,"/",g_id, error)
    CALL check("h5gopen_f",error,total_error)

    len_filename = LEN_TRIM(fix_filename)
    CALL check_get_name(file_id, fix_filename, len_filename, total_error)
    CALL check_get_name(g_id, fix_filename, len_filename, total_error)

    ! Close the group.
    !
    CALL h5gclose_f(g_id, error)
    CALL check("h5gclose_f",error,total_error)

    !
    ! Close the file identifiers.
    !
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f",error,total_error)

    IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)
    RETURN

  END SUBROUTINE get_name_test


  !
  !    The following example demonstrates how to get creation property list,
  !    and access property list.
  !    We first create a file using the default creation and access property
  !    list. Then, the file was closed and reopened. We then get the
  !    creation and access property lists of the first file. The second file is
  !    created using the got property lists

  SUBROUTINE plisttest(cleanup, total_error)
    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(INOUT) :: total_error

    !
    !file names are "plist1.h5" and "plist2.h5"
    !
    CHARACTER(LEN=6), PARAMETER :: filename1 = "plist1"
    CHARACTER(LEN=80) :: fix_filename1
    CHARACTER(LEN=6), PARAMETER :: filename2 = "plist2"
    CHARACTER(LEN=80) :: fix_filename2

    INTEGER(HID_T) :: file1_id, file2_id   ! File identifiers
    INTEGER(HID_T) :: prop_id    ! File creation property list identifier
    INTEGER(HID_T) :: access_id  ! File Access property list identifier

    !flag to check operation success
    INTEGER     :: error
    !file status
    LOGICAL     :: status

    !
    !Create a file1 using default properties.
    !
    CALL h5_fixname_f(filename1, fix_filename1, H5P_DEFAULT_F, error)
    if (error .ne. 0) then
       write(*,*) "Cannot modify file name"
       stop
    endif
    CALL h5fcreate_f(fix_filename1, H5F_ACC_TRUNC_F, file1_id, error)
    CALL check("h5fcreate_f",error,total_error)

    !
    !Terminate access to the file.
    !
    CALL h5fclose_f(file1_id, error)
    CALL check("h5fclose_f",error,total_error)

    !
    !Open an existing file.
    !
    CALL h5fopen_f (fix_filename1, H5F_ACC_RDWR_F, file1_id, error)
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
    CALL h5_fixname_f(filename2, fix_filename2, H5P_DEFAULT_F, error)
    if (error .ne. 0) then
       write(*,*) "Cannot modify file name"
       stop
    endif
    CALL h5fcreate_f(fix_filename2, H5F_ACC_TRUNC_F, file2_id, error, &
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

    ! Test file deletion
    CALL h5fis_accessible_f(filename1, status, error)
    CALL check("h5fis_accessible_f",error,total_error)
    IF ( .NOT. status ) THEN
       WRITE(*,*) "ERROR: File ", filename1, " is not accessible as hdf5"
    END IF

    CALL h5fdelete_f(filename1, error, H5P_DEFAULT_F)
    CALL check("h5fdelete_f", error, total_error)

    INQUIRE(FILE=filename1, EXIST=status)
    IF ( status ) THEN
       WRITE(*,*) "ERROR: File ", filename1, " was not removed by H5Fdelete_f"
    END IF

    CALL h5fis_accessible_f(filename2, status, error)
    CALL check("h5fis_accessible_f",error,total_error)
    IF ( .NOT. status ) THEN
       WRITE(*,*) "ERROR: File ", filename2, " is not accessible as hdf5"
       total_error=total_error + 1
    END IF

    CALL h5fdelete_f(filename2, error)
    CALL check("h5fdelete_f", error, total_error)

    INQUIRE(FILE=filename2, EXIST=status)
    IF ( status ) THEN
       WRITE(*,*) "ERROR: File ", filename2, " was not removed by H5Fdelete_f"
       total_error=total_error + 1
    END IF

    RETURN

  END SUBROUTINE plisttest


  !
  !    The following subroutine tests h5pget(set)_fclose_degree_f
  !

  SUBROUTINE file_close(cleanup, total_error)
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(INOUT) :: total_error
    INTEGER              :: error

    !
    CHARACTER(LEN=10), PARAMETER :: filename = "file_close"
    CHARACTER(LEN=80)  :: fix_filename

    INTEGER(HID_T) :: fid, fid_d, fid1, fid2, fid3  ! File identifiers
    INTEGER(HID_T) :: fapl, fapl1, fapl2, fapl3 ! File access identifiers
    INTEGER(HID_T) :: fid_d_fapl, fid1_fapl     ! File access identifiers
    LOGICAL        :: flag
    INTEGER(SIZE_T) :: obj_count, obj_countf
    INTEGER(HID_T), ALLOCATABLE, DIMENSION(:) :: obj_ids
    INTEGER(SIZE_T) :: i

    CALL h5eset_auto_f(0, error)

    CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
    if (error .ne. 0) then
       write(*,*) "Cannot modify filename"
       stop
    endif
    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, fid, error)
    CALL check("h5fcreate_f",error,total_error)

    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
    CALL check("h5pcreate_f",error,total_error)
    CALL h5pset_fclose_degree_f(fapl, H5F_CLOSE_DEFAULT_F, error)
    CALL check("h5pset_fclose_degree_f",error,total_error)


    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl1, error)
    CALL check("h5pcreate_f",error,total_error)
    CALL h5pset_fclose_degree_f(fapl1, H5F_CLOSE_WEAK_F, error)
    CALL check("h5pset_fclose_degree_f",error,total_error)


    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl2, error)
    CALL check("h5pcreate_f",error,total_error)
    CALL h5pset_fclose_degree_f(fapl2, H5F_CLOSE_SEMI_F, error)
    CALL check("h5pset_fclose_degree_f",error,total_error)

    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl3, error)
    CALL check("h5pcreate_f",error,total_error)
    CALL h5pset_fclose_degree_f(fapl3, H5F_CLOSE_STRONG_F, error)
    CALL check("h5pset_fclose_degree_f",error,total_error)

    CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, fid1, error, access_prp=fapl1)
    CALL check("h5fopen_f",error,total_error)
    CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, fid_d, error, access_prp=fapl)
    CALL check("h5fopen_f",error,total_error)
    CALL h5fget_access_plist_f(fid1, fid1_fapl, error)
    CALL check("h5fget_access_plist_f",error,total_error)
    CALL h5fget_access_plist_f(fid_d, fid_d_fapl, error)
    CALL check("h5fget_access_plist_f",error,total_error)

    CALL h5pequal_f(fid_d_fapl, fid1_fapl, flag, error)
    CALL check("h5pequal_f",error,total_error)
    if (.NOT. flag) then
       write(*,*) " File access lists should be equal, error "
       total_error=total_error + 1
    endif
    CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, fid2, error, access_prp=fapl2)
    if( error .ne. -1) then
       total_error = total_error + 1
       write(*,*) " Open with H5F_CLOSE_SEMI should fail "
    endif
    CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, fid3, error, access_prp=fapl3)
    if( error .ne. -1) then
       total_error = total_error + 1
       write(*,*) " Open with H5F_CLOSE_STRONG should fail "
    endif
    
    CALL h5fget_obj_count_f(fid1, H5F_OBJ_ALL_F, obj_count, error)
    CALL check("h5fget_obj_count_f",error,total_error)
    if(error .eq.0 .and. obj_count .ne. 3) then
       total_error = total_error + 1
       write(*,*) "Wrong number of open objects reported, error"
    endif
    CALL h5fget_obj_count_f(fid1, H5F_OBJ_FILE_F, obj_countf, error)
    CALL check("h5fget_obj_count_f",error,total_error)
    if(error .eq.0 .and. obj_countf .ne. 3) then
       total_error = total_error + 1
       write(*,*) "Wrong number of open objects reported, error"
    endif
    allocate(obj_ids(obj_countf), stat = error)
    CALL h5fget_obj_ids_f(fid, H5F_OBJ_FILE_F, obj_countf, obj_ids, error)
    CALL check("h5fget_obj_ids_f",error,total_error)
    if(error .eq. 0) then
       do i = 1, obj_countf
          CALL h5fclose_f(obj_ids(i), error)
          CALL check("h5fclose_f",error,total_error)
       enddo
    endif

    CALL h5fclose_f(fid, error)
    if(error .eq. 0) then
       total_error = total_error + 1
       write(*,*) "File should be closed at this point, error"
    endif
    CALL h5fclose_f(fid1, error)
    if(error .eq. 0) then
       total_error = total_error + 1
       write(*,*) "File should be closed at this point, error"
    endif
    CALL h5fclose_f(fid_d, error)
    if(error .eq. 0) then
       total_error = total_error + 1
       write(*,*) "File should be closed at this point, error"
    endif

    if(cleanup) then
       CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
       CALL check("h5_cleanup_f", error, total_error)
    endif
    deallocate(obj_ids)
    RETURN

  END SUBROUTINE file_close

  !
  !    The following subroutine tests h5fget_freespace_f
  !

  SUBROUTINE file_space(filename, cleanup, total_error)
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN) :: filename
    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(INOUT) :: total_error
    INTEGER              :: error
    !
    CHARACTER(LEN=3), PARAMETER :: grpname = "grp"
    CHARACTER(LEN=80)  :: fix_filename

    INTEGER(HID_T) :: fid ! File identifiers
    INTEGER(HSSIZE_T) :: free_space
    INTEGER(HID_T) :: group_id      ! Group identifier

    INTEGER(HID_T) :: fcpl
    INTEGER(HSIZE_T), PARAMETER :: set_usrblck_size = 512
    INTEGER(HSIZE_T) :: usrblck_size

    CALL h5eset_auto_f(0, error)

    CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
    if (error .ne. 0) then
       write(*,*) "Cannot modify filename"
       stop
    endif

    CALL h5pcreate_f(H5P_FILE_CREATE_F, fcpl, error)
    CALL check("h5pcreate_f",error, total_error)

    CALL H5Pset_userblock_f(fcpl, set_usrblck_size, error )
    CALL check("h5pset_userblock_f", error, total_error)

    CALL H5Pget_userblock_f(fcpl, usrblck_size, error )
    CALL check("h5pget_userblock_f", error, total_error)

    IF(usrblck_size .NE. set_usrblck_size) THEN
       total_error = total_error + 1
       WRITE(*,*) "Wrong size of a user block, ", usrblck_size
    ENDIF

    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, fid, error, creation_prp=fcpl )
    CALL check("h5fcreate_f",error,total_error)

    CALL h5pclose_f(fcpl, error)
    CALL check("H5Pclose_f", error, total_error)

    CALL h5fget_freespace_f(fid, free_space, error)
    CALL check("h5fget_freespace_f",error,total_error)
    IF(error .EQ.0 .AND. free_space .NE. 1248) THEN
       total_error = total_error + 1
       WRITE(*,*) "1: Wrong amount of free space reported, ", free_space
    ENDIF

    ! Create group in the file.
    CALL h5gcreate_f(fid, grpname, group_id, error)
    CALL check("h5gcreate_f",error,total_error)

    ! Close group
    CALL h5gclose_f(group_id, error)
    CALL check("h5gclose_f", error, total_error)

    ! Check the free space now
    CALL h5fget_freespace_f(fid, free_space, error)
    CALL check("h5fget_freespace_f",error,total_error)
    IF(error .EQ.0 .AND. free_space .NE. 216) THEN
       total_error = total_error + 1
       WRITE(*,*) "2: Wrong amount of free space reported, ", free_space
    ENDIF

    !Unlink the group
    CALL h5gunlink_f(fid, grpname, error)
    CALL check("h5gunlink_f", error, total_error)

    ! Check the free space now
    CALL h5fget_freespace_f(fid, free_space, error)
    CALL check("h5fget_freespace_f",error,total_error)
    IF(error .EQ.0 .AND. free_space .NE. 1248) THEN
       total_error = total_error + 1
       WRITE(*,*) "3: Wrong amount of free space reported, ", free_space
    ENDIF

    IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)
    RETURN

  END SUBROUTINE file_space

  !
  !    The following subroutine tests h5fget_info_f
  !

  SUBROUTINE test_file_info(filename, cleanup, total_error)
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN) :: filename
    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(INOUT) :: total_error
    INTEGER              :: error
    !
    CHARACTER(LEN=3), PARAMETER :: grpname = "grp"
    CHARACTER(LEN=80)  :: fix_filename

    INTEGER(HID_T) :: fid ! File identifiers
    INTEGER(HID_T) :: group_id      ! Group identifier

    TYPE(H5F_INFO_T) :: file_info
    INTEGER(HID_T) :: fapl, fcpl
    INTEGER :: strategy
    LOGICAL :: persist
    INTEGER(HSIZE_T) :: threshold, fsp_size

    CALL h5eset_auto_f(0, error)

    CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
    if (error .ne. 0) then
       write(*,*) "Cannot modify filename"
       stop
    endif

    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
    CALL check("h5pcreate_f",error, total_error)

    CALL h5pcreate_f(H5P_FILE_CREATE_F, fcpl, error)
    CALL check("h5pcreate_f",error, total_error)

    CALL h5pset_libver_bounds_f(fapl, H5F_LIBVER_V114_F, H5F_LIBVER_V114_F, error)
    CALL check("h5pset_libver_bounds_f",error, total_error)
    CALL h5pset_file_space_strategy_f(fcpl, H5F_FSPACE_STRATEGY_PAGE_F, .TRUE., 4_HSIZE_T, error)
    CALL check("h5pset_file_space_strategy_f",error, total_error)

    CALL h5pget_file_space_strategy_f(fcpl, strategy, persist, threshold, error)
    CALL check("h5pget_file_space_strategy_f",error, total_error)

    IF(strategy .NE. H5F_FSPACE_STRATEGY_PAGE_F) THEN
       total_error = total_error + 1
       WRITE(*,*) "h5pget_file_space_strategy_f: wrong strategy, ",strategy
    ENDIF
    IF(persist .NEQV. .TRUE.) THEN
       total_error = total_error + 1
       WRITE(*,*) "h5pget_file_space_strategy_f: wrong persist, ",persist
    ENDIF
    IF(threshold .NE. 4_HSIZE_T) THEN
       total_error = total_error + 1
       WRITE(*,*) "h5pget_file_space_strategy_f: wrong threshold, ",threshold
    ENDIF

    CALL h5pset_file_space_page_size_f(fcpl, 512_HSIZE_T, error)
    CALL check("H5Pset_file_space_page_size_f",error, total_error)

    CALL h5pget_file_space_page_size_f(fcpl, fsp_size, error)
    CALL check("H5Pset_file_space_page_size_f",error, total_error)

    IF(fsp_size .NE. 512_HSIZE_T) THEN
       total_error = total_error + 1
       WRITE(*,*) "h5pget_file_space_page_size_f: wrong size, ",fsp_size
    ENDIF

    CALL h5pset_alignment_f(fapl, 1_HSIZE_T, 1024_HSIZE_T, error)
    CALL check("h5pset_alignment_f",error, total_error)

    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, fid, error, access_prp=fapl, creation_prp=fcpl)
    CALL check("h5fcreate_f",error,total_error)

    ! Create group in the file.
    CALL h5gcreate_f(fid, grpname, group_id, error)
    CALL check("h5gcreate_f",error,total_error)

    ! Close group
    CALL h5gclose_f(group_id, error)
    CALL check("h5gclose_f", error, total_error)

    !Unlink the group
    CALL h5gunlink_f(fid, grpname, error)
    CALL check("h5gunlink_f", error, total_error)

    ! Check H5Fget_info_f
    CALL h5fget_info_f(fid, file_info, error)
    CALL check("h5fget_info_f", error, total_error)

    IF(file_info%super%version .NE. 3) THEN
       total_error = total_error + 1
       WRITE(*,*) "Wrong super%version, ",file_info%free%tot_space
    ENDIF

    IF(file_info%super%super_size .NE. 48) THEN
       total_error = total_error + 1
       WRITE(*,*) "Wrong super%super_size, ",file_info%free%tot_space
    ENDIF

    IF(file_info%super%super_ext_size .NE. 156) THEN
       total_error = total_error + 1
       WRITE(*,*) "Wrong super%super_ext_size, ",file_info%super%super_ext_size
    ENDIF

    IF(file_info%free%version .NE. 0) THEN
       total_error = total_error + 1
       WRITE(*,*) "Wrong free%version, ",file_info%free%version
    ENDIF

    IF(file_info%free%tot_space .NE. 161) THEN
       total_error = total_error + 1
       WRITE(*,*) "Wrong free%tot_space, ",file_info%free%tot_space
    ENDIF

    IF(file_info%sohm%version.NE. 0) THEN
       total_error = total_error + 1
       WRITE(*,*) "Wrong sohm%version ",file_info%sohm%version
    ENDIF

    IF(file_info%sohm%hdr_size.NE. 0) THEN
       total_error = total_error + 1
       WRITE(*,*) "Wrong sohm%hdr_size ",file_info%sohm%hdr_size
    ENDIF

    IF(file_info%sohm%msgs_info%heap_size.NE. 0) THEN
       total_error = total_error + 1
       WRITE(*,*) "Wrong sohm%msgs_info%heap_size ",file_info%sohm%msgs_info%heap_size
    ENDIF

    IF(file_info%sohm%msgs_info%index_size.NE. 0) THEN
       total_error = total_error + 1
       WRITE(*,*) "Wrong sohm%msgs_info%heap_size ",file_info%sohm%msgs_info%index_size
    ENDIF

    CALL h5fclose_f(fid, error)
    CALL check("h5fclose_f",error,total_error)

    CALL h5pclose_f(fapl, error)
    CALL check("H5Pclose_f", error, total_error)
    CALL h5pclose_f(fcpl, error)
    CALL check("H5Pclose_f", error, total_error)

    if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)
    RETURN

  END SUBROUTINE test_file_info

  SUBROUTINE test_get_file_image(total_error)
    !
    !  Tests the wrapper for h5fget_file_image
    !
    IMPLICIT NONE

    INTEGER, INTENT(INOUT) :: total_error ! returns error

    CHARACTER(kind=c_char), ALLOCATABLE, DIMENSION(:), TARGET :: file_image_ptr ! Image from file
    CHARACTER(kind=c_char), ALLOCATABLE, DIMENSION(:), TARGET :: image_ptr ! Image from h5fget_file_image_f

    INTEGER, DIMENSION(1:100), TARGET :: data ! Write data
    INTEGER :: file_sz
    INTEGER(size_t) :: i
    INTEGER(hid_t) :: file_id = -1  ! File identifier
    INTEGER(hid_t) :: dset_id = -1  ! Dataset identifier
    INTEGER(hid_t) :: space_id = -1 ! Dataspace identifier
    INTEGER(hsize_t), DIMENSION(1:2) :: dims  ! Dataset dimensions
    INTEGER(size_t) :: itmp_a ! General purpose integer
    INTEGER(size_t) :: image_size     ! Size of image
    TYPE(C_PTR) :: f_ptr            ! Pointer
    INTEGER(hid_t) :: fapl          ! File access property
    INTEGER :: error                ! Error flag
    CHARACTER(LEN=18), PARAMETER :: filename="tget_file_image.h5"

    ! Create new properties for file access
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
    CALL check("h5pcreate_f", error, total_error)

    ! Set standard I/O driver
    CALL h5pset_fapl_stdio_f(fapl, error)
    CALL check("h5pset_fapl_stdio_f", error, total_error)

    ! Create the file
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, H5P_DEFAULT_F, fapl)
    CALL check("h5fcreate_f", error, total_error)

    ! Set up data space for new data set
    dims(1:2) = (/10,10/)

    CALL h5screate_simple_f(2, dims,  space_id, error)
    CALL check("h5screate_simple_f", error, total_error)

    ! Create a dataset
    CALL h5dcreate_f(file_id, "dset 0", H5T_NATIVE_INTEGER, space_id, dset_id, error)
    CALL check("h5dcreate_f", error, total_error)

    ! Write some data to the data set
    DO i = 1, 100
       data(i) = INT(i)
    ENDDO

    f_ptr = C_LOC(data(1))
    CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, error)
    CALL check("h5dwrite_f",error, total_error)

    ! Flush the file
    CALL h5fflush_f(file_id, H5F_SCOPE_GLOBAL_F, error)
    CALL check("h5fflush_f",error, total_error)

    ! Open the test file using standard I/O calls
    OPEN(UNIT=10,FILE=filename, ACCESS='STREAM')
    ! Get the size of the test file
    !
    ! Since we use the eoa to calculate the image size, the file size
    ! may be larger.  This is OK, as long as (in this specialized instance)
    ! the remainder of the file is all '\0's.
    !
    ! With latest mods to truncate call in core file drive,
    ! file size should match image size; get the file size
    INQUIRE(UNIT=10, SIZE=file_sz)
    CLOSE(UNIT=10)

    ! I. Get buffer size needed to hold the buffer

    !  A. Preferred way to get the size
    f_ptr = C_NULL_PTR
    CALL h5fget_file_image_f(file_id, f_ptr, INT(0, size_t), error, image_size)
    CALL check("h5fget_file_image_f",error, total_error)
    CALL verify("h5fget_file_image_f", file_sz, INT(image_size), total_error)

    !  B. f_ptr set to point to an incorrect buffer, should pass anyway
    f_ptr = C_LOC(data(1))
    itmp_a = 1
    CALL h5fget_file_image_f(file_id, f_ptr, itmp_a, error, image_size)
    CALL check("h5fget_file_image_f",error, total_error)
    CALL verify("h5fget_file_image_f", INT(itmp_a), 1, total_error) ! Routine should not change the value
    CALL verify("h5fget_file_image_f", file_sz, INT(image_size), total_error)

    ! Allocate a buffer of the appropriate size
    ALLOCATE(image_ptr(1:image_size))

    ! Load the image of the file into the buffer
    f_ptr = C_LOC(image_ptr(1)(1:1))
    CALL h5fget_file_image_f(file_id, f_ptr, image_size, error)
    CALL check("h5fget_file_image_f",error, total_error)

    ! Close dset and space
    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f", error, total_error)
    CALL h5sclose_f(space_id, error)
    CALL check("h5sclose_f", error, total_error)
    ! Close the test file
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f",error, total_error)

    ! Allocate a buffer for the test file image
    ALLOCATE(file_image_ptr(1:image_size))

    ! Open the test file using standard I/O calls
    OPEN(UNIT=10,FILE=filename, FORM='UNFORMATTED', ACCESS='STREAM')

    ! Read the test file from disk into the buffer
    DO i = 1, image_size
       READ(10) file_image_ptr(i)
    ENDDO

    CLOSE(10)

    ! verify the file and the image contain the same data
    DO i = 1, image_size
       ! convert one byte to an unsigned integer
       IF( ICHAR(file_image_ptr(i)) .NE. ICHAR(image_ptr(i)))THEN
          total_error = total_error + 1
          EXIT
       ENDIF
    ENDDO

    ! release resources
    DEALLOCATE(file_image_ptr,image_ptr)

  END SUBROUTINE test_get_file_image

END MODULE TH5F
