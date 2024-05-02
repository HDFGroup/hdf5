!****h* root/fortran/test/tH5R.f90
!
! NAME
!  tH5R.f90
!
! FUNCTION
!  Basic testing of Fortran H5R, Reference Interface, APIs.
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
! NOTES
!  Tests h5rcreate_f, h5rdereference_f, h5rget_name_f
!  and H5Rget_object_type functions
!
! CONTAINS SUBROUTINES
!  refobjtest, refregtest
!
!*****
!

#include <H5config_f.inc>

MODULE TH5R

  USE HDF5
  USE TH5_MISC
  USE TH5_MISC_GEN

CONTAINS

SUBROUTINE refobjtest(cleanup, total_error)
  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  CHARACTER(LEN=9), PARAMETER :: filename = "reference"
  CHARACTER(LEN=80) :: fix_filename
  CHARACTER(LEN=8), PARAMETER :: dsetnamei = "INTEGERS"
  CHARACTER(LEN=17), PARAMETER :: dsetnamer = "OBJECT_REFERENCES"
  CHARACTER(LEN=6), PARAMETER :: groupname1 = "GROUP1"
  CHARACTER(LEN=6), PARAMETER :: groupname2 = "GROUP2"

  INTEGER(HID_T) :: file_id       ! File identifier
  INTEGER(HID_T) :: grp1_id       ! Group identifier
  INTEGER(HID_T) :: grp2_id       ! Group identifier
  INTEGER(HID_T) :: dset1_id      ! Dataset identifier
  INTEGER(HID_T) :: dsetr_id      ! Dataset identifier
  INTEGER(HID_T) :: type_id       ! Type identifier
  INTEGER(HID_T) :: space_id      ! Dataspace identifier
  INTEGER(HID_T) :: spacer_id     ! Dataspace identifier
  INTEGER     ::   error, obj_type
  INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/5/)
  INTEGER(HSIZE_T), DIMENSION(1) :: dimsr= (/4/)
  INTEGER(HSIZE_T), DIMENSION(1) :: my_maxdims = (/5/)
  INTEGER :: rank = 1
  INTEGER :: rankr = 1
  TYPE(hobj_ref_t_f), DIMENSION(4) ::  ref
  TYPE(hobj_ref_t_f), DIMENSION(4) ::  ref_out
  INTEGER(HSIZE_T), DIMENSION(1) :: ref_dim
  INTEGER, DIMENSION(5) :: DATA = (/1, 2, 3, 4, 5/)
  INTEGER(HSIZE_T), DIMENSION(2) :: data_dims

  CHARACTER(LEN=7) :: buf        ! buffer to hold the region name
  CHARACTER(LEN=16) :: buf_big    ! buffer bigger than needed
  INTEGER(SIZE_T) :: buf_size     ! returned size of the region buffer name

  !
  !Create a new file with Default file access and
  !file creation properties .
  !
  CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
  IF (error .NE. 0) THEN
     WRITE(*,*) "Cannot modify filename"
     STOP
  ENDIF
  CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
  CALL check("h5fcreate_f",error,total_error)


  !
  ! Create a group inside the file
  !
  CALL h5gcreate_f(file_id, groupname1, grp1_id, error)
  CALL check("h5gcreate_f",error,total_error)

  !
  ! Create a group inside the group GROUP1
  !
  CALL h5gcreate_f(grp1_id, groupname2, grp2_id, error)
  CALL check("h5gcreate_f",error,total_error)

  !
  ! Create dataspaces for datasets
  !
  CALL h5screate_simple_f(rank, dims, space_id, error, maxdims=my_maxdims)
  CALL check("h5screate_simple_f",error,total_error)
  CALL h5screate_simple_f(rankr, dimsr, spacer_id, error)
  CALL check("h5screate_simple_f",error,total_error)

  !
  ! Create integer dataset
  !
  CALL h5dcreate_f(file_id, dsetnamei, H5T_NATIVE_INTEGER, space_id, &
       dset1_id, error)
  CALL check("h5dcreate_f",error,total_error)
  !
  ! Create dataset to store references to the objects
  !
  CALL h5dcreate_f(file_id, dsetnamer, H5T_STD_REF_OBJ, spacer_id, &
       dsetr_id, error)
  CALL check("h5dcreate_f",error,total_error)
  !
  ! Create a datatype and store in the file
  !
  CALL h5tcopy_f(H5T_NATIVE_REAL, type_id, error)
  CALL check("h5tcopy_f",error,total_error)
  CALL h5tcommit_f(file_id, "MyType", type_id, error)
  CALL check("h5tcommit_f",error,total_error)
  !
  !  Close dataspaces, groups and integer dataset
  !
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f",error,total_error)
  CALL h5sclose_f(spacer_id, error)
  CALL check("h5sclose_f",error,total_error)
  CALL h5dclose_f(dset1_id, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5tclose_f(type_id, error)
  CALL check("h5tclose_f",error,total_error)
  CALL h5gclose_f(grp1_id, error)
  CALL check("h5gclose_f",error,total_error)
  CALL h5gclose_f(grp2_id, error)
  CALL check("h5gclose_f",error,total_error)

  !
  ! Create references to two groups, integer dataset and shared datatype
  ! and write it to the dataset in the file
  !
  CALL h5rcreate_f(file_id, groupname1, ref(1), error)
  CALL check("h5rcreate_f",error,total_error)
  CALL h5rcreate_f(file_id, "/GROUP1/GROUP2", ref(2), error)
  CALL check("h5rcreate_f",error,total_error)
  CALL h5rcreate_f(file_id, dsetnamei, ref(3), error)
  CALL check("h5rcreate_f",error,total_error)
  CALL h5rcreate_f(file_id, "MyType", ref(4), error)
  CALL check("h5rcreate_f",error,total_error)
  ref_dim(1) = SIZE(ref)
  CALL h5dwrite_f(dsetr_id, H5T_STD_REF_OBJ, ref, ref_dim, error)
  CALL check("h5dwrite_f",error,total_error)

  ! getting path to normal dataset in root group

  CALL H5Rget_name_f(dsetr_id, ref(1), buf, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)


  CALL VERIFY("H5Rget_name_f", INT(buf_size),LEN("/"//groupname1), total_error)
  CALL verify("H5Rget_name_f", buf, "/GROUP1", total_error)

  ! with a buffer bigger than needed

  CALL H5Rget_name_f(dsetr_id, ref(1), buf_big, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)
  CALL verify("H5Rget_name_f", INT(buf_size),7,total_error)
  CALL verify("H5Rget_name_f", TRIM(buf_big), "/GROUP1", total_error)

  ! getting path to dataset in /Group1

  CALL H5Rget_name_f(dsetr_id, ref(2), buf_big, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)
  CALL verify("H5Rget_name_f", INT(buf_size),14,total_error)
  CALL verify("H5Rget_name_f", TRIM(buf_big), "/GROUP1/GROUP2", total_error)

  !
  !Close the dataset
  !
  CALL h5dclose_f(dsetr_id, error)
  CALL check("h5dclose_f",error,total_error)

  !
  ! Reopen the dataset with object references
  !
  CALL h5dopen_f(file_id, dsetnamer,dsetr_id,error)
  CALL check("h5dopen_f",error,total_error)
  ref_dim(1) = SIZE(ref_out)
  CALL h5dread_f(dsetr_id, H5T_STD_REF_OBJ, ref_out, ref_dim, error)
  CALL check("h5dread_f",error,total_error)

  !
  !get the third reference's type and Dereference it
  !
  CALL h5rget_object_type_f(dsetr_id, ref(3), obj_type, error)
  CALL check("h5rget_object_type_f",error,total_error)
  IF (obj_type == H5G_DATASET_F) THEN
     CALL h5rdereference_f(dsetr_id, ref(3), dset1_id, error)
     CALL check("h5rdereference_f",error,total_error)

     data_dims(1) = 5
     CALL h5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, DATA, data_dims, error)
     CALL check("h5dwrite_f",error,total_error)
  END IF

  !
  !get the fourth reference's type and Dereference it
  !
  CALL h5rget_object_type_f(dsetr_id, ref(4), obj_type, error)
  CALL check("h5rget_object_type_f",error,total_error)
  IF (obj_type == H5G_TYPE_F) THEN
     CALL h5rdereference_f(dsetr_id, ref(4), type_id, error)
     CALL check("h5rdereference_f",error,total_error)
  END IF

  !
  ! Close all objects.
  !
  CALL h5dclose_f(dset1_id, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5tclose_f(type_id, error)
  CALL check("h5tclose_f",error,total_error)

  CALL h5dclose_f(dsetr_id, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f",error,total_error)

  IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)
  RETURN

END SUBROUTINE refobjtest
!
!   The following subroutine tests h5rget_region_f, h5rcreate_f, h5rget_name_f,
!   and h5rdereference_f functionalities
!
SUBROUTINE refregtest(cleanup, total_error)

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  CHARACTER(LEN=6), PARAMETER :: filename = "Refreg"
  CHARACTER(LEN=80) :: fix_filename
  CHARACTER(LEN=6), PARAMETER :: dsetnamev = "MATRIX"
  CHARACTER(LEN=17), PARAMETER :: dsetnamer = "REGION_REFERENCES"

  CHARACTER(LEN=7) :: buf        ! buffer to hold the region name
  CHARACTER(LEN=11) :: buf_big    ! buffer bigger than needed
  CHARACTER(LEN=4) :: buf_small  ! buffer smaller than needed
  INTEGER(SIZE_T) :: buf_size     ! returned size of the region buffer name
  INTEGER(HID_T) :: file_id       ! File identifier
  INTEGER(HID_T) :: space_id      ! Dataspace identifier
  INTEGER(HID_T) :: spacer_id     ! Dataspace identifier
  INTEGER(HID_T) :: dsetv_id      ! Dataset identifier
  INTEGER(HID_T) :: dsetr_id      ! Dataset identifier
  INTEGER     ::   error
!  TYPE(hdset_reg_ref_t_f) , DIMENSION(1:2), TARGET :: ref
  TYPE(hdset_reg_ref_t_f) , DIMENSION(1:2) :: ref
  TYPE(hdset_reg_ref_t_f) , DIMENSION(1:2) :: ref_out
  INTEGER(HSIZE_T), DIMENSION(2) :: ref_dim   = (/0,0/)
  INTEGER(HSIZE_T), DIMENSION(2) :: data_dims ! = (/0,0/)
  INTEGER(HSIZE_T), DIMENSION(2) :: dims      = (/2,9/)  ! Datasets dimensions
  INTEGER(HSIZE_T), DIMENSION(1) :: dimsr     = (/2/)    !
  INTEGER(HSIZE_T), DIMENSION(2) :: start     ! = (/0,0/)
  INTEGER(HSIZE_T), DIMENSION(2) :: count     ! = (/0,0/)

  INTEGER :: rankr = 1
  INTEGER :: rank = 2
!  INTEGER , DIMENSION(2,9), TARGET ::  DATA
  INTEGER , DIMENSION(2,9) ::  DATA
  INTEGER , DIMENSION(2,9) ::  data_out = 0
  INTEGER(HSIZE_T) , DIMENSION(2,3) :: coord
  INTEGER(SIZE_T) ::num_points = 3  ! Number of selected points
!  type(c_ptr) :: f_ptr
  coord = RESHAPE((/1,1,2,7,1,9/), (/2,3/))   ! Coordinates of selected points
  DATA = RESHAPE ((/1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6/), (/2,9/))

  ref_out(1)%ref = 0
  ref_out(2)%ref = 0

  !
  !  Initialize FORTRAN predefined datatypes.
  !
  !          CALL h5init_types_f(error)
  !              CALL check("h5init_types_f", error, total_error)
  !
  !  Create a new file.
  !
  CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
  IF (error .NE. 0) THEN
     WRITE(*,*) "Cannot modify filename"
     STOP
  ENDIF
  CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
  ! Default file access and file creation
  ! properties are used.
  CALL check("h5fcreate_f", error, total_error)
  !
  ! Create  dataspaces:
  !
  ! for dataset with references to dataset regions
  !
  CALL h5screate_simple_f(rankr, dimsr, spacer_id, error)
  CALL check("h5screate_simple_f", error, total_error)
  !
  ! for integer dataset
  !
  CALL h5screate_simple_f(rank, dims, space_id, error)
  CALL check("h5screate_simple_f", error, total_error)
  !
  ! Create  and write datasets:
  !
  ! Integer dataset
  !
  CALL h5dcreate_f(file_id, dsetnamev, H5T_NATIVE_INTEGER, space_id, &
       dsetv_id, error)
  CALL check("h5dcreate_f", error, total_error)
  data_dims(1) = 2
  data_dims(2) = 9

!  f_ptr = c_loc(data)
!  CALL h5dwrite_f(dsetv_id, H5T_NATIVE_INTEGER, f_ptr, error)

  CALL h5dwrite_f(dsetv_id, H5T_NATIVE_INTEGER, DATA, data_dims, error)
  CALL check("h5dwrite_f", error, total_error)

  CALL h5dclose_f(dsetv_id, error)
  CALL check("h5dclose_f", error, total_error)

  !
  ! Dataset with references
  !
  CALL h5dcreate_f(file_id, dsetnamer, H5T_STD_REF_DSETREG, spacer_id, &
       dsetr_id, error)
  CALL check("h5dcreate_f", error, total_error)
  !
  ! Create a reference to the hyperslab selection.
  !
  start(1) = 0
  start(2) = 3
  COUNT(1) = 2
  COUNT(2) = 3
  CALL h5sselect_hyperslab_f(space_id, H5S_SELECT_SET_F, &
       start, count, error)
  CALL check("h5sselect_hyperslab_f", error, total_error)
  ref(1)%ref(:) = 0
!  f_ptr = C_LOC(ref(1))
!  CALL h5rcreate_f(file_id, dsetnamev, 1, space_id, f_ptr, error)
  CALL h5rcreate_f(file_id, dsetnamev, space_id, ref(1), error)
  CALL check("h5rcreate_f", error, total_error)

  !
  ! Create a reference to elements selection.
  !
  CALL h5sselect_none_f(space_id, error)
  CALL check("h5sselect_none_f", error, total_error)
  CALL h5sselect_elements_f(space_id, H5S_SELECT_SET_F, rank, num_points,&
       coord, error)
  CALL check("h5sselect_elements_f", error, total_error)
  ref(2)%ref(:) = 0
  CALL h5rcreate_f(file_id, dsetnamev, space_id, ref(2), error)
  CALL check("h5rcreate_f", error, total_error)
  !
  ! Write dataset with the references.
  !
  ref_dim(1) = SIZE(ref)
  CALL h5dwrite_f(dsetr_id, H5T_STD_REF_DSETREG, ref, ref_dim, error)
  CALL check("h5dwrite_f", error, total_error)
  !
  ! Close all objects.
  !
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5sclose_f(spacer_id, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5dclose_f(dsetr_id, error)
  CALL check("h5dclose_f", error, total_error)
  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f", error, total_error)

  !
  ! Reopen the file to test selections.
  !
  CALL h5fopen_f (fix_filename, H5F_ACC_RDWR_F, file_id, error)
  CALL check("h5fopen_f", error, total_error)
  CALL h5dopen_f(file_id, dsetnamer, dsetr_id, error)
  CALL check("h5dopen_f", error, total_error)
  !
  ! Read references to the dataset regions.
  !
  ref_dim(1) = SIZE(ref_out)
  CALL h5dread_f(dsetr_id, H5T_STD_REF_DSETREG, ref_out, ref_dim, error)
  CALL check("h5dread_f", error, total_error)

  ! Get name of the dataset the first region reference points to using H5Rget_name_f
  CALL H5Rget_name_f(dsetr_id, ref_out(1), buf, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)
  CALL verify("H5Rget_name_f", INT(buf_size),7,total_error)
  CALL verify("H5Rget_name_f", buf, "/MATRIX", total_error)

  ! Get name of the dataset the first region reference points to using H5Rget_name_f
  ! buffer bigger than needed
  CALL H5Rget_name_f(dsetr_id, ref_out(1), buf_big, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)
  CALL verify("H5Rget_name_f", INT(buf_size),7,total_error)
  CALL verify("H5Rget_name_f", TRIM(buf_big), "/MATRIX", total_error)


  ! Get name of the dataset the first region reference points to using H5Rget_name_f
  ! buffer smaller than needed
  CALL H5Rget_name_f(dsetr_id, ref_out(1), buf_small, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)
  CALL verify("H5Rget_name_f", INT(buf_size),7,total_error)
  CALL verify("H5Rget_name_f", TRIM(buf_small), "/MAT", total_error)
  !
  ! Dereference the first reference.
  !
  CALL H5rdereference_f(dsetr_id, ref_out(1), dsetv_id, error)
  CALL check("h5rdereference_f", error, total_error)
  CALL H5rget_region_f(dsetr_id, ref_out(1), space_id, error)
  CALL check("h5rget_region_f", error, total_error)

  ! Get name of the dataset the second region reference points to using H5Rget_name_f
  CALL H5Rget_name_f(dsetr_id, ref_out(2), buf, error) ! no optional size
  CALL check("H5Rget_name_f", error, total_error)
  CALL verify("H5Rget_name_f", TRIM(buf), "/MATRIX", total_error)
  !
  ! Read selected data from the dataset.
  !
  data_dims(1) = 2
  data_dims(2) = 9
  CALL h5dread_f(dsetv_id, H5T_NATIVE_INTEGER, data_out, data_dims, error, &
       mem_space_id = space_id, file_space_id = space_id)
  CALL check("h5dread_f", error, total_error)
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5dclose_f(dsetv_id, error)
  CALL check("h5dclose_f", error, total_error)
  data_out = 0
  !
  ! Dereference the second reference.
  !
  CALL H5rdereference_f(dsetr_id, ref_out(2), dsetv_id, error)
  CALL check("h5rdereference_f", error, total_error)

  CALL H5rget_region_f(dsetr_id, ref_out(2), space_id, error)
  CALL check("h5rget_region_f", error, total_error)
  !
  ! Read selected data from the dataset.
  !
  CALL h5dread_f(dsetv_id, H5T_NATIVE_INTEGER, data_out, data_dims, error, &
       mem_space_id = space_id, file_space_id = space_id)
  CALL check("h5dread_f", error, total_error)
  !
  ! Close all objects
  !
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5dclose_f(dsetv_id, error)
  CALL check("h5dclose_f", error, total_error)
  CALL h5dclose_f(dsetr_id, error)
  CALL check("h5dclose_f", error, total_error)
  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f", error, total_error)

  IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)
  RETURN

END SUBROUTINE refregtest

SUBROUTINE v3reftest(cleanup, total_error)
  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  CHARACTER(LEN=11), PARAMETER :: filename = "v3reference"
  CHARACTER(LEN=8) , PARAMETER :: dsetnamei  = "INTEGERS"
  CHARACTER(LEN=17), PARAMETER :: dsetnamer  = "OBJECT_REFERENCES"
  CHARACTER(LEN=6) , PARAMETER :: groupname1 = "GROUP1"
  CHARACTER(LEN=6) , PARAMETER :: groupname2 = "GROUP2"
  CHARACTER(LEN=5) , PARAMETER :: attrname = "ATTR1"
  INTEGER          , PARAMETER :: ref_size = 6
  INTEGER          , PARAMETER :: arr_size = 5
  INTEGER          , PARAMETER :: rank = 1
  INTEGER          , PARAMETER :: datawrite_size = 2

  CHARACTER(LEN=80) :: fix_filename
  INTEGER(HID_T) :: file_id       ! File identifier
  INTEGER(HID_T) :: grp1_id       ! Group identifier
  INTEGER(HID_T) :: grp2_id       ! Group identifier
  INTEGER(HID_T) :: dset1_id      ! Dataset identifier
  INTEGER(HID_T) :: dsetr_id      ! Dataset identifier
  INTEGER(HID_T) :: type_id       ! Type identifier
  INTEGER(HID_T) :: space_id      ! Dataspace identifier
  INTEGER(HID_T) :: spacer_id     ! Dataspace identifier
  INTEGER(HID_T) :: sid, sid2, aid, aid2, dspace_id, dspace_id1
  INTEGER        :: error, ref_type
  INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/arr_size/)
  INTEGER(HSIZE_T), DIMENSION(1) :: dimsr= (/ref_size/)
  INTEGER :: i
  TYPE(H5R_ref_t), DIMENSION(ref_size), TARGET :: ref_ptr
  TYPE(H5R_ref_t), DIMENSION(ref_size), TARGET :: ref_ptr_read
  TYPE(H5R_ref_t), TARGET :: ref_ptr_cp
  INTEGER, DIMENSION(arr_size), TARGET :: DATA = (/1, 2, 3, 4, 5/)
  INTEGER, DIMENSION(2), TARGET :: data_write = (/100, 500/)
  INTEGER(HSIZE_T), DIMENSION(rank) :: dimspt = (/datawrite_size/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: coord

#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
  CHARACTER(:), ALLOCATABLE :: buf_alloc ! buffer to hold the region name
#endif
  CHARACTER(LEN=16) :: buf_big    ! buffer bigger than needed
  INTEGER(SIZE_T) :: buf_size     ! returned size of the region buffer name
  INTEGER, TARGET :: a_data
  TYPE(C_PTR) :: f_ptr
  LOGICAL :: ref_eq
  INTEGER(hssize_t) :: num_points_ret

  INTEGER(HID_T) :: memspace

  !
  ! Create a new file with Default file access and
  ! file creation properties.
  !
  CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
  IF (error .NE. 0) THEN
     WRITE(*,*) "Cannot modify filename"
     STOP
  ENDIF
  CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
  CALL check("h5fcreate_f",error,total_error)

  !
  ! Create a group inside the file
  !
  CALL h5gcreate_f(file_id, groupname1, grp1_id, error)
  CALL check("h5gcreate_f",error,total_error)

  !
  ! Create a group inside the group GROUP1
  !
  CALL h5gcreate_f(grp1_id, groupname2, grp2_id, error)
  CALL check("h5gcreate_f",error,total_error)

  CALL H5Screate_f(H5S_SCALAR_F, sid, error)
  CALL check("H5Screate_f",error,total_error)
  CALL H5Acreate_f(grp2_id, attrname, H5T_NATIVE_INTEGER, sid, aid, error)
  CALL check("H5Acreate_f",error,total_error)

  !
  ! Create an attribute
  !
  a_data = 20
  f_ptr = C_LOC(a_data)
  CALL H5Awrite_f(aid, H5T_NATIVE_INTEGER, f_ptr, error)
  CALL check("H5Awrite_f",error,total_error)

  !
  ! Create dataspaces for datasets
  !
  CALL h5screate_simple_f(rank, dims, space_id, error)
  CALL check("h5screate_simple_f",error,total_error)
  CALL h5screate_simple_f(rank, dimsr, spacer_id, error)
  CALL check("h5screate_simple_f",error,total_error)

  !
  ! Create integer dataset
  !
  CALL h5dcreate_f(file_id, dsetnamei, H5T_NATIVE_INTEGER, space_id, dset1_id, error)
  CALL check("h5dcreate_f",error,total_error)
  !
  ! Create dataset to store references to the objects
  !
  CALL h5dcreate_f(file_id, dsetnamer, H5T_STD_REF, spacer_id, dsetr_id, error)
  CALL check("h5dcreate_f",error,total_error)
  !
  ! Create a datatype and store in the file
  !
  CALL h5tcopy_f(H5T_NATIVE_REAL, type_id, error)
  CALL check("h5tcopy_f",error,total_error)
  CALL h5tcommit_f(file_id, "MyType", type_id, error)
  CALL check("h5tcommit_f",error,total_error)
  !
  !  Close dataspaces, groups and integer dataset
  !
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f",error,total_error)
  CALL h5sclose_f(spacer_id, error)
  CALL check("h5sclose_f",error,total_error)
  CALL h5dclose_f(dset1_id, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5tclose_f(type_id, error)
  CALL check("h5tclose_f",error,total_error)
  CALL h5gclose_f(grp1_id, error)
  CALL check("h5gclose_f",error,total_error)
  CALL h5gclose_f(grp2_id, error)
  CALL check("h5gclose_f",error,total_error)
  CALL h5aclose_f(aid,error)
  CALL check("H5Aclose_f",error,total_error)
  CALL h5sclose_f(sid,error)
  CALL check("H5Sclose_f",error,total_error)
  !
  ! Create references to two groups, integer dataset, point selection and
  ! shared datatype and write it to the dataset in the file
  !
  f_ptr = C_LOC(ref_ptr(1))
  CALL h5rcreate_object_f(file_id, groupname1, f_ptr, error)
  CALL check("h5rcreate_f",error,total_error)
  f_ptr = C_LOC(ref_ptr(2))
  CALL h5rcreate_object_f(file_id, "/"//groupname1//"/"//groupname2, f_ptr, error)
  CALL check("h5rcreate_f",error,total_error)
  f_ptr = C_LOC(ref_ptr(3))
  CALL h5rcreate_object_f(file_id, dsetnamei, f_ptr, error)
  CALL check("h5rcreate_f",error,total_error)
  f_ptr = C_LOC(ref_ptr(4))
  CALL h5rcreate_object_f(file_id, "MyType", f_ptr, error)
  CALL check("h5rcreate_f",error,total_error)

  f_ptr = C_LOC(ref_ptr(5))
  CALL h5rcreate_attr_f(file_id, "/"//groupname1//"/"//groupname2, attrname, f_ptr, error, H5P_DEFAULT_F)
  CALL check("h5rcreate_attr_f",error,total_error)

  CALL h5screate_simple_f(1, dimspt, sid2, error)
  CALL check("h5screate_simple_f",error,total_error)

  coord(1) = 1
  coord(2) = dims(1)
  CALL h5sselect_elements_f(sid2, H5S_SELECT_SET_F, 1, SIZE(coord,KIND=SIZE_T), coord, error)
  CALL check("h5sselect_elements_f",error,total_error)

  f_ptr = C_LOC(ref_ptr(6))
  CALL h5rcreate_region_f(file_id, dsetnamei, sid2, f_ptr, error)
  CALL check("h5rcreate_region_f",error,total_error)
  CALL h5rget_obj_name_f(C_LOC(ref_ptr(6)), buf_big, error)

  f_ptr = C_LOC(ref_ptr(1))
  CALL h5dwrite_f(dsetr_id, H5T_STD_REF, f_ptr, error)
  CALL check("h5dwrite_f",error,total_error)

  CALL h5rget_obj_name_f(C_LOC(ref_ptr(3)), "", error, H5P_DEFAULT_F, buf_size)
  CALL check("h5rget_obj_name_f", error, total_error)
  CALL verify("h5rget_obj_name_f", buf_size, LEN(dsetnamei,KIND=SIZE_T)+1_SIZE_T, total_error)
  CALL h5rget_obj_name_f(C_LOC(ref_ptr(1)), "", error, H5P_DEFAULT_F, buf_size)
  CALL check("h5rget_obj_name_f", error, total_error)
  CALL verify("h5rget_obj_name_f", buf_size, 7_SIZE_T, total_error)

#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
  ALLOCATE(CHARACTER(LEN=buf_size) :: buf_alloc)
  CALL h5rget_obj_name_f(C_LOC(ref_ptr(1)), buf_alloc, error)
  CALL check("h5rget_obj_name_f", error, total_error)
  CALL VERIFY("h5rget_obj_name_f", buf_alloc, "/"//groupname1, total_error)
  DEALLOCATE(buf_alloc)
#endif
  ! with buffer bigger than needed
  CALL h5rget_obj_name_f(C_LOC(ref_ptr(1)), buf_big, error)
  CALL check("h5rget_obj_name_f", error, total_error)
  CALL verify("h5rget_obj_name_f", TRIM(buf_big), "/"//groupname1, total_error)

  ! getting path to dataset
  CALL h5rget_obj_name_f(C_LOC(ref_ptr(2)), "", error, name_len=buf_size )
  CALL check("H5Rget_name_f", error, total_error)
  CALL verify("H5Rget_name_f2", INT(buf_size),LEN("/"//groupname1//"/"//groupname2),total_error)

#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
  ALLOCATE(CHARACTER(LEN=buf_size) :: buf_alloc)
  CALL h5rget_obj_name_f(C_LOC(ref_ptr(2)), buf_alloc, error)
  CALL check("h5rget_obj_name_f", error, total_error)
  CALL VERIFY("h5rget_obj_name_f", buf_alloc, "/"//groupname1//"/"//groupname2, total_error)
  DEALLOCATE(buf_alloc)
#endif

  CALL h5rget_obj_name_f(C_LOC(ref_ptr(2)), buf_big, error)
  CALL check("H5Rget_name_f", error, total_error)
  CALL verify("H5Rget_name_f", TRIM(buf_big), "/"//groupname1//"/"//groupname2, total_error)

  ! CHECK COPYING REF

  f_ptr =  C_LOC(ref_ptr_cp)
  CALL h5rcopy_f(C_LOC(ref_ptr(3)), f_ptr, error)
  CALL check("h5rcopy_f", error, total_error)

  ! GET FILE NAME
  CALL h5rget_file_name_f(f_ptr, "", error, buf_size)
  CALL check("h5rget_file_name_f", error, total_error)
  CALL verify("h5rget_file_name_f", buf_size, LEN_TRIM(fix_filename,KIND=SIZE_T), total_error)

#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
  ALLOCATE(CHARACTER(LEN=buf_size) :: buf_alloc)
  CALL h5rget_file_name_f(f_ptr, buf_alloc, error)
  CALL check("h5rget_file_name_f", error, total_error)
  CALL VERIFY("h5rget_file_name_f", buf_alloc, TRIM(fix_filename), total_error)
  DEALLOCATE(buf_alloc)
#endif

  ! Check with buffer bigger than needed
  CALL h5rget_file_name_f(C_LOC(ref_ptr_cp), buf_big, error)
  CALL check("h5rget_file_name_f", error, total_error)
  CALL verify("h5rget_file_name_f", TRIM(buf_big), TRIM(fix_filename), total_error)

  ! CHECK EQUAL API
  CALL h5requal_f(C_LOC(ref_ptr(3)), f_ptr, ref_eq, error)
  CALL check("h5requal_f", error, total_error)
  CALL VERIFY("h5requal_f", ref_eq, .TRUE., total_error)

  CALL h5requal_f(C_LOC(ref_ptr(1)), C_LOC(ref_ptr(3)), ref_eq, error)
  CALL check("h5requal_f", error, total_error)
  CALL VERIFY("h5requal_f", ref_eq, .FALSE., total_error)

  CALL h5rdestroy_f(f_ptr, error)
  CALL check("h5rdestroy_f", error, total_error)

  !
  ! Close the dataset
  !
  CALL h5dclose_f(dsetr_id, error)
  CALL check("h5dclose_f",error,total_error)

  DO i = 1, ref_size
     CALL h5rdestroy_f(C_LOC(ref_ptr(i)), error)
     CALL check("h5rdestroy_f", error, total_error)
  END DO
  !
  ! Reopen the dataset with object references
  !
  CALL h5dopen_f(file_id, dsetnamer,dsetr_id,error)
  CALL check("h5dopen_f",error,total_error)

  ! Read the references dataset
  f_ptr = C_LOC(ref_ptr_read(1))
  CALL h5dread_f(dsetr_id, H5T_STD_REF, f_ptr, error)
  CALL check("h5dread_f",error,total_error)

  !
  ! Get information about the references read and check for correctness
  !
  CALL h5rget_obj_name_f(C_LOC(ref_ptr_read(1)), buf_big, error)
  CALL check("h5rget_obj_name_f", error, total_error)
  CALL verify("h5rget_obj_name_f", TRIM(buf_big), "/"//groupname1, total_error)
  CALL h5rget_obj_name_f(C_LOC(ref_ptr_read(2)), buf_big, error)
  CALL check("h5rget_obj_name_f", error, total_error)
  CALL verify("h5rget_obj_name_f", TRIM(buf_big), "/"//groupname1//"/"//groupname2, total_error)
  CALL h5rget_obj_name_f(C_LOC(ref_ptr_read(3)), buf_big, error)
  CALL check("h5rget_obj_name_f", error, total_error)
  CALL verify("h5rget_obj_name_f", TRIM(buf_big), "/"//dsetnamei, total_error)
  CALL h5rget_obj_name_f(C_LOC(ref_ptr_read(4)), buf_big, error)
  CALL check("h5rget_obj_name_f", error, total_error)
  CALL verify("h5rget_obj_name_f", TRIM(buf_big), "/"//"MyType", total_error)

  CALL h5rget_attr_name_f(C_LOC(ref_ptr_read(5)), buf_big, error, buf_size)
  CALL check("h5rget_attr_name_f", error, total_error)
  CALL VERIFY("h5rget_attr_name_f", buf_size, LEN(attrname, KIND=SIZE_T), total_error)

  CALL h5rget_attr_name_f(C_LOC(ref_ptr_read(5)), buf_big, error)
  CALL check("h5rget_attr_name_f", error, total_error)
  CALL verify("h5rget_attr_name_f", TRIM(buf_big), attrname, total_error)

  CALL h5ropen_attr_f( C_LOC(ref_ptr_read(5)), aid2, error, H5P_DEFAULT_F, H5P_DEFAULT_F )
  CALL check("h5ropen_attr_f",error,total_error)

  CALL h5aget_name_f(aid2, 16_size_t, buf_big, error)
  CALL check("h5aget_name_f",error,total_error)
  CALL verify("h5aget_name_f", TRIM(buf_big), attrname, total_error)

  CALL h5aclose_f(aid2, error)
  CALL check("h5aclose_f", error, total_error)

  CALL h5rget_type_f(C_LOC(ref_ptr_read(1)), ref_type, error)
  CALL check("h5rget_type_f", error, total_error)
  CALL verify("h5rget_type_f", ref_type, H5R_OBJECT2_F, total_error)

  IF (ref_type == H5R_OBJECT2_F) THEN
    CALL h5ropen_object_f(C_LOC(ref_ptr_read(3)), dset1_id, error, H5P_DEFAULT_F)
    CALL check("h5ropen_object_f", error, total_error)

    CALL h5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, C_LOC(data(1)), error)
    CALL check("h5dwrite_f",error,total_error)

    CALL h5rdestroy_f(C_LOC(ref_ptr_read(3)), error)
    CALL check("h5rdestroy_f", error, total_error)

    CALL h5oclose_f(dset1_id, error)
    CALL check("h5oclose_f1",error,total_error)

  END IF

  CALL h5rget_type_f(C_LOC(ref_ptr_read(6)), ref_type, error)
  CALL check("h5rget_type_f", error, total_error)

  IF(ref_type .EQ. H5R_DATASET_REGION2_F)THEN
     CALL h5ropen_object_f(C_LOC(ref_ptr_read(6)), dset1_id, error, H5P_DEFAULT_F)

     CALL H5Dget_space_f(dset1_id, dspace_id1, error)
     CALL check("H5Dget_space_f",error,total_error)
     CALL H5Sget_simple_extent_npoints_f(dspace_id1, num_points_ret, error)
     CALL VERIFY("H5Sget_simple_extent_npoints_f", num_points_ret, dims(1), total_error)

     CALL h5ropen_region_f(C_LOC(ref_ptr_read(6)), dspace_id, error, H5P_DEFAULT_F)
     CALL check("h5ropen_object_f", error, total_error)

     CALL h5screate_simple_f(1, dimspt, memspace, error)
     CALL check("h5screate_simple_f",error,total_error)

     CALL h5sget_select_elem_npoints_f(dspace_id, num_points_ret, error)
     CALL check("h5sget_select_elem_npoints_f",error,total_error)
     CALL VERIFY("h5sget_simple_extent_npoints_f", num_points_ret, 2_HSIZE_T, total_error)

     f_ptr =  C_LOC(data_write(1))
     CALL h5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, f_ptr, error, memspace, dspace_id)
     CALL check("h5dwrite_f",error,total_error)

     CALL h5rdestroy_f(C_LOC(ref_ptr_read(6)), error)
     CALL check("h5rdestroy_f", error, total_error)

     f_ptr = C_LOC(data(1))
     CALL h5dread_f(dset1_id, H5T_NATIVE_INTEGER, f_ptr, error)
     CALL check("h5dread_f", error, total_error)
     CALL VERIFY("h5dread_f", DATA(1), data_write(1), total_error)
     CALL VERIFY("h5dread_f", DATA(5), data_write(2), total_error)

     CALL h5oclose_f(dset1_id, error)
     CALL check("h5oclose_f",error,total_error)
  ELSE
     CALL check("h5rget_type_f", -1, total_error)
  END IF

  !
  ! Close all objects.
  !

  CALL h5dclose_f(dsetr_id, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f",error,total_error)

  DO i = 1, 4
     CALL h5rdestroy_f(C_LOC(ref_ptr_read(i)), error)
     CALL check("h5rdestroy_f", error, total_error)
  END DO

  IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)

  RETURN

END SUBROUTINE v3reftest

END MODULE TH5R
