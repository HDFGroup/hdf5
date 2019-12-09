!****h* root/fortran/test/tH5O_F03.f90
!
! NAME
!  tH5O_F03.f90
!
! FUNCTION
!  Test FORTRAN HDF5 H5O APIs which are dependent on FORTRAN 2003
!  features. 
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!*****

! *****************************************
! ***        H 5 O   T E S T S
! *****************************************
MODULE visit_cb

  USE HDF5 
  USE, INTRINSIC :: ISO_C_BINDING
  
  IMPLICIT NONE

  INTEGER, PARAMETER :: info_size = 9

  !-------------------------------------------------------------------------
  ! Function:    visit_obj_cb
  !
  ! Purpose:     Callback routine for visiting objects in a file
  !
  ! Return:      Success:        0
  !              Failure:        -1
  !
  ! Programmer:  M.S. Breitenfeld
  !              July 12, 2012
  !              Adopted from C test.
  !
  !-------------------------------------------------------------------------
  !
  ! Object visit structs
  TYPE, bind(c) :: obj_visit_t
     CHARACTER(KIND=C_CHAR), DIMENSION(1:180) :: path   ! Path to object
     INTEGER :: type_obj ! type of object
  END TYPE obj_visit_t

  TYPE, bind(c) :: ovisit_ud_t
     INTEGER :: idx              ! Index in object visit structure
     TYPE(obj_visit_t), DIMENSION(1:info_size) :: info   ! Pointer to the object visit structure to use
     INTEGER :: field
  END TYPE ovisit_ud_t

CONTAINS

! Compares the field values of a C h5O_info_t and a Fortran H5O_info_t. 

  INTEGER FUNCTION compare_h5o_info_t( oinfo_f, oinfo_c, field, full_f_field ) RESULT(status)
    
    IMPLICIT NONE
    TYPE(h5o_info_t)   :: oinfo_f
    TYPE(c_h5o_info_t) :: oinfo_c
    INTEGER :: field
    LOGICAL :: full_f_field ! All the fields of Fortran H5O_info_t where filled 
!   local
    INTEGER(C_INT), DIMENSION(1:8) :: atime, btime, ctime, mtime
    INTEGER :: i

    status = 0
    
    IF( (field .EQ. H5O_INFO_BASIC_F).OR.(field .EQ. H5O_INFO_ALL_F) )THEN
       IF( (oinfo_f%fileno.LE.0) .OR. (oinfo_c%fileno .NE. oinfo_f%fileno) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%addr.LE.0) .OR. (oinfo_c%addr .NE. oinfo_f%addr) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%type.LT.0) .OR. (oinfo_c%type .NE. oinfo_f%type) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%rc.LT.0) .OR. (oinfo_c%rc .NE. oinfo_f%rc) )THEN
          status = -1
          RETURN
       ENDIF

    ENDIF

    IF((field .EQ. H5O_INFO_TIME_F).OR.(field .EQ. H5O_INFO_ALL_F))THEN

       atime(1:8) = h5gmtime(oinfo_c%atime)
       btime(1:8) = h5gmtime(oinfo_c%btime)
       ctime(1:8) = h5gmtime(oinfo_c%ctime)
       mtime(1:8) = h5gmtime(oinfo_c%mtime)

       DO i = 1, 8
          IF( (atime(i) .NE. oinfo_f%atime(i)) )THEN
             status = -1
             RETURN
          ENDIF

          IF( (btime(i) .NE. oinfo_f%btime(i)) )THEN
             status = -1
             RETURN
          ENDIF

          IF( (ctime(i) .NE. oinfo_f%ctime(i)) )THEN
             status = -1
             RETURN
          ENDIF

          IF( (mtime(i) .NE. oinfo_f%mtime(i)) )THEN
             status = -1
             RETURN
          ENDIF
       ENDDO

    ELSE IF(field .EQ. H5O_INFO_TIME_F.AND. full_f_field)THEN
       ! check other field values are not filled (using only a small subset to check)
       status = 0
       IF( oinfo_c%fileno .NE. oinfo_f%fileno) status = status + 1
       IF( oinfo_c%addr .NE. oinfo_f%addr) status = status + 1
       IF( oinfo_c%type .NE. oinfo_f%type) status = status + 1
       IF( oinfo_c%rc .NE. oinfo_f%rc) status = status + 1
       IF(status.EQ.0) THEN ! There was no difference found, which is only possible if the field was filled.
          status = -1
          RETURN
       ENDIF
       status = 0 ! reset
    ENDIF

    IF((field .EQ. H5O_INFO_NUM_ATTRS_F).OR.(field .EQ. H5O_INFO_ALL_F))THEN
       IF( (oinfo_f%num_attrs.LT.0) .OR. (oinfo_c%num_attrs .NE. oinfo_f%num_attrs) )THEN
          status = -1
          RETURN
       ENDIF
    ELSE IF( field .EQ. H5O_INFO_ALL_F.AND.full_f_field)THEN
       ! check other field values are not filled (using only a small subset to check)
       status = 0
       IF( oinfo_c%fileno .NE. oinfo_f%fileno) status = status + 1
       IF( oinfo_c%addr .NE. oinfo_f%addr) status = status + 1
       IF( oinfo_c%type .NE. oinfo_f%type) status = status + 1
       IF( oinfo_c%rc .NE. oinfo_f%rc) status = status + 1
       IF(status.EQ.0) THEN ! There was no difference found, which is only possible if the field was filled.
          status = -1
          RETURN
       ENDIF
       status = 0 ! reset
       
    ENDIF

    IF((field).EQ.H5O_INFO_HDR_F.OR.(field .EQ. H5O_INFO_ALL_F))THEN
       IF( (oinfo_f%hdr%version.LT.0) .OR. (oinfo_c%hdr%version .NE. oinfo_f%hdr%version) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%hdr%nmesgs.LT.0) .OR. (oinfo_c%hdr%nmesgs .NE. oinfo_f%hdr%nmesgs) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%hdr%nchunks.LT.0) .OR. (oinfo_c%hdr%nchunks .NE. oinfo_f%hdr%nchunks) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%hdr%flags.LT.0) .OR. (oinfo_c%hdr%flags .NE. oinfo_f%hdr%flags) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%hdr%space%total.LT.0) .OR. (oinfo_c%hdr%space%total .NE. oinfo_f%hdr%space%total) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%hdr%space%meta.LT.0) .OR. (oinfo_c%hdr%space%meta .NE. oinfo_f%hdr%space%meta) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%hdr%space%mesg.LT.0) .OR. (oinfo_c%hdr%space%mesg .NE. oinfo_f%hdr%space%mesg) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%hdr%space%free.LT.0) .OR. (oinfo_c%hdr%space%free .NE. oinfo_f%hdr%space%free) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%hdr%mesg%present.LT.0) .OR. (oinfo_c%hdr%mesg%present .NE. oinfo_f%hdr%mesg%present) )THEN
          status = -1
          RETURN
       ENDIF
       IF( (oinfo_f%hdr%mesg%shared.LT.0) .OR. (oinfo_c%hdr%mesg%shared .NE. oinfo_f%hdr%mesg%shared) )THEN
          status = -1
          RETURN
       ENDIF
    ELSE IF( field .EQ. H5O_INFO_HDR_F.AND.full_f_field)THEN
       ! check other field values are not filled (using only a small subset to check)
       status = 0
       IF( oinfo_c%fileno .NE. oinfo_f%fileno) status = status + 1
       IF( oinfo_c%addr .NE. oinfo_f%addr) status = status + 1
       IF( oinfo_c%type .NE. oinfo_f%type) status = status + 1
       IF( oinfo_c%rc .NE. oinfo_f%rc) status = status + 1
       IF(status.EQ.0) THEN ! There was no difference found, which is only possible if the field was filled.
          status = -1
          RETURN
       ENDIF
       status = 0 ! reset
    ENDIF
    IF((field).EQ.H5O_INFO_META_SIZE_F.OR.(field .EQ. H5O_INFO_ALL_F))THEN
       IF((oinfo_f%meta_size%obj%index_size.LT.0).OR.(oinfo_c%meta_size%obj%index_size.NE.oinfo_f%meta_size%obj%index_size))THEN
          status = -1
          RETURN
       ENDIF
       IF((oinfo_f%meta_size%obj%heap_size.LT.0).OR.(oinfo_c%meta_size%obj%heap_size.NE.oinfo_f%meta_size%obj%heap_size))THEN
          status = -1
          RETURN
       ENDIF
       IF((oinfo_f%meta_size%attr%index_size.LT.0).OR.(oinfo_c%meta_size%attr%index_size.NE.oinfo_f%meta_size%attr%index_size))THEN
          status = -1
          RETURN
       ENDIF
       IF((oinfo_f%meta_size%attr%heap_size.LT.0).OR.(oinfo_c%meta_size%attr%heap_size.NE.oinfo_f%meta_size%attr%heap_size))THEN
          status = -1
          RETURN
       ENDIF
    ELSE IF( field .EQ. H5O_INFO_META_SIZE_F.AND.full_f_field)THEN
       ! check other field values are not filled (using only a small subset to check)
       status = 0
       IF( oinfo_c%fileno .NE. oinfo_f%fileno) status = status + 1
       IF( oinfo_c%addr .NE. oinfo_f%addr) status = status + 1
       IF( oinfo_c%type .NE. oinfo_f%type) status = status + 1
       IF( oinfo_c%rc .NE. oinfo_f%rc) status = status + 1
       IF(status.EQ.0) THEN ! There was no difference found, which is only possible if the field was filled.
          status = -1
          RETURN
       ENDIF
       status = 0 ! reset
    ENDIF

  END FUNCTION compare_h5o_info_t

  INTEGER FUNCTION visit_obj_cb( group_id, name, oinfo_c, op_data) bind(C)

    IMPLICIT NONE

    INTEGER(HID_T), VALUE :: group_id
    CHARACTER(LEN=1), DIMENSION(1:180) :: name
    CHARACTER(LEN=180) :: name2
    TYPE(c_h5o_info_t) :: oinfo_c
    TYPE(ovisit_ud_t) :: op_data
    TYPE(h5o_info_t) ::  oinfo_f
!
!   MEMBER      | TYPE | MEANING                 | RANGE
! A(1) = tm_sec	  int   seconds after the minute   0-61*
! A(2) = tm_min	  int   minutes after the hour     0-59
! A(3) = tm_hour  int   hours since midnight       0-23
! A(4) = tm_mday  int   day of the month           1-31
! A(5) = tm_mon	  int   months since January       0-11
! A(6) = tm_year  int   years since 1900
! A(7) = tm_wday  int   days since Sunday          0-6
! A(8) = tm_yday  int   days since January 1       0-365
! A(9) = tm_isdst int   Daylight Saving Time flag
!
    INTEGER :: len, i
    INTEGER :: idx
    INTEGER :: ierr

    visit_obj_cb = 0

    ! Since the name is generated in C and passed to a Fortran string, it
    ! will be NULL terminated, so we need to find the end of the string.

    len = 1
    DO len = 1, 180
       IF(name(len) .EQ. C_NULL_CHAR) EXIT
    ENDDO

    len = len - 1

    ! Check for correct object information
    name2(1:180) = ""
    DO i = 1, len
       name2(i:i) = name(i)(1:1)
    ENDDO

    IF(op_data%field .EQ. H5O_INFO_ALL_F)THEN

       idx = op_data%idx
     
       DO i = 1, len
          IF(op_data%info(idx)%path(i)(1:1) .NE. name(i)(1:1))THEN
             visit_obj_cb = -1
             RETURN
          ENDIF
          
          IF(op_data%info(idx)%type_obj .NE. oinfo_c%type)THEN
             visit_obj_cb = -1
             RETURN
          ENDIF
       ENDDO

    ENDIF

    ! Check H5Oget_info_by_name_f; if partial field values where filled correctly
    CALL H5Oget_info_by_name_f(group_id, name2, oinfo_f, ierr);
    visit_obj_cb =  compare_h5o_info_t( oinfo_f, oinfo_c, op_data%field, .TRUE. )
    IF(visit_obj_cb.EQ.-1) RETURN

    ! Check H5Oget_info_by_name_f, only check field values
    CALL H5Oget_info_by_name_f(group_id, name2, oinfo_f, ierr, fields = op_data%field);
    visit_obj_cb =  compare_h5o_info_t( oinfo_f, oinfo_c, op_data%field, .FALSE. )
    IF(visit_obj_cb.EQ.-1) RETURN


    IF(op_data%idx.EQ.1)THEN

       ! Check H5Oget_info_f, only check field values
       CALL H5Oget_info_f(group_id, oinfo_f, ierr, fields = op_data%field);
       visit_obj_cb =  compare_h5o_info_t( oinfo_f, oinfo_c, op_data%field, .FALSE.  )
       IF(visit_obj_cb.EQ.-1) RETURN

       ! Check H5Oget_info_f; if partial field values where filled correctly
       CALL H5Oget_info_f(group_id, oinfo_f, ierr);
       visit_obj_cb =  compare_h5o_info_t( oinfo_f, oinfo_c, op_data%field, .TRUE.  )
       IF(visit_obj_cb.EQ.-1) RETURN

    ENDIF

    ! Advance to next location in expected output
    op_data%idx = op_data%idx + 1

  END FUNCTION visit_obj_cb

END MODULE visit_cb

MODULE TH5O_F03

CONTAINS
!***************************************************************
!**
!**  test_h5o_refcount(): Test H5O refcounting functions.
!**
!***************************************************************

SUBROUTINE test_h5o_refcount(total_error)

  USE HDF5 
  USE TH5_MISC
  USE ISO_C_BINDING
  IMPLICIT NONE

  INTEGER, INTENT(INOUT) :: total_error

  CHARACTER(LEN=11), PARAMETER :: FILENAME = "th5o_ref.h5"
  INTEGER, PARAMETER :: DIM0 = 5
  INTEGER, PARAMETER :: DIM1 = 10
  INTEGER(hid_t) :: fid ! HDF5 File ID
  INTEGER(hid_t) :: grp, dset, dtype, dspace ! Object identifiers
  TYPE(h5o_info_t) :: oinfo                  ! Object info struct
  INTEGER(hsize_t), DIMENSION(1:2) :: dims
  INTEGER :: error                           ! Value returned from API calls

  ! Create a new HDF5 file
  CALL h5fcreate_f(FILENAME,H5F_ACC_TRUNC_F,fid,error)
  CALL check("h5fcreate_f", error, total_error) 

  ! Create a group, dataset, and committed datatype within the file
  ! Create the group
  CALL h5gcreate_f(fid, "group", grp, error)
  CALL check("h5gcreate_f",error, total_error)

  ! Commit the type inside the group
  CALL h5tcopy_f(H5T_NATIVE_INTEGER, dtype, error)
  CALL check("H5Tcopy_f",error, total_error)
  CALL h5tcommit_f(fid, "datatype", dtype, error)
  CALL check("h5tcommit_f", error, total_error)

  ! Create the data space for the dataset.
  dims(1) = DIM0
  dims(2) = DIM1

  CALL h5screate_simple_f(2, dims, dspace, error)
  CALL check("h5screate_simple_f", error, total_error)

  ! Create the dataset.
  CALL h5dcreate_f(fid, "dataset", H5T_NATIVE_INTEGER, dspace, dset, error)
  CALL check("h5dcreate_f", error, total_error)
  CALL h5sclose_f(dspace, error)
  CALL check("h5sclose_f", error, total_error)

  ! Get ref counts for each object.  They should all be 1, since each object has a hard link.
  CALL h5oget_info_by_name_f(fid, "group", oinfo, error)
  CALL check("h5oget_info_by_name_f", error, total_error)
  IF(oinfo%rc.NE.1)THEN
     CALL check("h5oget_info_by_name_f", -1, total_error)
  ENDIF
  CALL h5oget_info_by_name_f(fid, "datatype", oinfo, error)
  CALL check("h5oget_info_by_name_f", error, total_error)
  IF(oinfo%rc.NE.1)THEN
     CALL check("h5oget_info_by_name_f", -1, total_error)
  ENDIF
  CALL h5oget_info_by_name_f(fid, "dataset", oinfo, error)
  CALL check("h5oget_info_by_name_f", error, total_error)
  IF(oinfo%rc.NE.1)THEN
     CALL check("h5oget_info_by_name_f", -1, total_error)
  ENDIF

  ! Check h5oget_info
  CALL h5oget_info_f(grp, oinfo, error)
  CALL check("h5oget_info_f", error, total_error)
  IF(oinfo%rc.NE.1)THEN
     CALL check("h5oget_info_f", -1, total_error)
  ENDIF
  IF(oinfo%type.NE.H5O_TYPE_GROUP_F)THEN
     CALL check("h5oget_info_f", -1, total_error)
  ENDIF

  ! Increment each object's reference count.
  CALL h5oincr_refcount_f(grp, error)
  CALL check("h5oincr_refcount_f", error, total_error)
  CALL h5oincr_refcount_f(dtype, error)
  CALL check("h5oincr_refcount_f", error, total_error)
  CALL h5oincr_refcount_f(dset, error)
  CALL check("h5oincr_refcount_f", error, total_error)

  ! Get ref counts for each object.  They should all be 2 now.
  CALL h5oget_info_by_name_f(fid, "group", oinfo, error)
  CALL check("h5oget_info_by_name_f", error, total_error)
  IF(oinfo%rc.NE.2)THEN
     CALL check("h5oget_info_by_name_f", -1, total_error)
  ENDIF
  CALL h5oget_info_by_name_f(fid, "datatype", oinfo, error)
  CALL check("h5oget_info_by_name_f", error, total_error)
  IF(oinfo%rc.NE.2)THEN
     CALL check("h5oget_info_by_name_f", -1, total_error)
  ENDIF
  CALL h5oget_info_by_name_f(fid, "dataset", oinfo, error)
  CALL check("h5oget_info_by_name_f", error, total_error)
  IF(oinfo%rc.NE.2)THEN
     CALL check("h5oget_info_by_name_f", -1, total_error)
  ENDIF

  ! Decrement the reference counts and check that they decrease back to 1.
  CALL h5odecr_refcount_f(grp, error)
  CALL check("h5oincr_refcount_f", error, total_error)
  CALL h5odecr_refcount_f(dtype, error)
  CALL check("h5oincr_refcount_f", error, total_error)
  CALL h5odecr_refcount_f(dset, error)
  CALL check("h5oincr_refcount_f", error, total_error)

  CALL h5oget_info_by_name_f(fid, "group", oinfo, error)
  CALL check("h5oget_info_by_name_f", error, total_error)
  IF(oinfo%rc.NE.1)THEN
     CALL check("h5oget_info_by_name_f", -1, total_error)
  ENDIF
  CALL h5oget_info_by_name_f(fid, "datatype", oinfo, error)
  CALL check("h5oget_info_by_name_f", error, total_error)
  IF(oinfo%rc.NE.1)THEN
     CALL check("h5oget_info_by_name_f", -1, total_error)
  ENDIF
  CALL h5oget_info_by_name_f(fid, "dataset", oinfo, error)
  CALL check("h5oget_info_by_name_f", error, total_error)
  IF(oinfo%rc.NE.1)THEN
     CALL check("h5oget_info_by_name_f", -1, total_error)
  ENDIF

  CALL h5gclose_f(grp, error)
  CALL check("h5gclose_f",error, total_error)
  CALL h5tclose_f(dtype, error)
  CALL check("h5tclose_f",error, total_error)
  CALL h5dclose_f(dset, error)
  CALL check("h5dclose_f",error, total_error)
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error, total_error)

END SUBROUTINE test_h5o_refcount

!****************************************************************
!**
!**  test_h5o_refcount(): Test H5O visit functions.
!**
!****************************************************************

SUBROUTINE obj_visit(total_error)

  USE HDF5
  USE TH5_MISC

  USE visit_cb
  USE ISO_C_BINDING
  IMPLICIT NONE

  INTEGER, INTENT(INOUT) :: total_error

  TYPE(ovisit_ud_t), TARGET :: udata ! User-data for visiting
  INTEGER(hid_t) :: fid = -1
  TYPE(C_PTR) :: f_ptr
  TYPE(C_FUNPTR) :: fun_ptr
  CHARACTER(LEN=180) :: object_name
  INTEGER :: ret_val
  INTEGER :: error

  ! Construct "interesting" file to visit
  CALL build_visit_file(fid)

  ! Inialize udata for testing purposes
  udata%info(1)%path(1:1) ="."
  udata%info(1)%type_obj = H5O_TYPE_GROUP_F
  udata%info(2)%path(1:12) = &
       (/"D","a","t","a","s","e","t","_","z","e","r","o"/)
  udata%info(2)%type_obj =H5O_TYPE_DATASET_F
  udata%info(3)%path(1:6) = &
       (/"G","r","o","u","p","1"/)
  udata%info(3)%type_obj = H5O_TYPE_GROUP_F
  udata%info(4)%path(1:18) =&
       (/"G","r","o","u","p","1","/","D","a","t","a","s","e","t","_","o","n","e"/)
  udata%info(4)%type_obj = H5O_TYPE_DATASET_F
  udata%info(5)%path(1:13) =&
       (/"G","r","o","u","p","1","/","G","r","o","u","p","2"/)
  udata%info(5)%type_obj = H5O_TYPE_GROUP_F
  udata%info(6)%path(1:25) =&
       (/"G","r","o","u","p","1","/","G","r","o","u","p","2","/","D","a","t","a","s","e","t","_","t","w","o"/)
  udata%info(6)%type_obj = H5O_TYPE_DATASET_F
  udata%info(7)%path(1:22) =&
       (/"G","r","o","u","p","1","/","G","r","o","u","p","2","/","T","y","p","e","_","t","w","o"/)
  udata%info(7)%type_obj = H5O_TYPE_NAMED_DATATYPE_F
  udata%info(8)%path(1:15) =&
       (/"G","r","o","u","p","1","/","T","y","p","e","_","o","n","e"/)
  udata%info(8)%type_obj = H5O_TYPE_NAMED_DATATYPE_F
  udata%info(9)%path(1:9) =&
       (/"T","y","p","e","_","z","e","r","o"/)
  udata%info(9)%type_obj = H5O_TYPE_NAMED_DATATYPE_F

  ! Visit all the objects reachable from the root group (with file ID)

  fun_ptr = C_FUNLOC(visit_obj_cb)
  f_ptr = C_LOC(udata)

  ! Test h5ovisit_f
  udata%field = H5O_INFO_ALL_F
  udata%idx = 1
  CALL h5ovisit_f(fid, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error)
  CALL check("h5ovisit_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_f", -1, total_error)
  ENDIF

  ! Test fields option
  udata%field = H5O_INFO_ALL_F
  udata%idx = 1
  CALL h5ovisit_f(fid, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_f", -1, total_error)
  ENDIF
  udata%field = H5O_INFO_BASIC_F
  udata%idx = 1
  CALL h5ovisit_f(fid, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_f", -1, total_error)
  ENDIF
  udata%field = H5O_INFO_TIME_F
  udata%idx = 1
  CALL h5ovisit_f(fid, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_f", -1, total_error)
  ENDIF
  udata%field = H5O_INFO_NUM_ATTRS_F
  udata%idx = 1
  CALL h5ovisit_f(fid, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_f", -1, total_error)
  ENDIF
  udata%field = H5O_INFO_HDR_F
  udata%idx = 1
  CALL h5ovisit_f(fid, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_f", -1, total_error)
  ENDIF
  udata%field = H5O_INFO_META_SIZE_F
  udata%idx = 1
  CALL h5ovisit_f(fid, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_f", -1, total_error)
  ENDIF

  ! Test h5ovisit_by_name_f
  object_name = "/"
  udata%idx = 1
  udata%field = H5O_INFO_ALL_F
  CALL h5ovisit_by_name_f(fid, object_name, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_by_name_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_by_name_f", -1, total_error)
  ENDIF

  ! Test fields option
  udata%idx = 1
  udata%field = H5O_INFO_BASIC_F
  CALL h5ovisit_by_name_f(fid, object_name, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_by_name_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_by_name_f", -1, total_error)
  ENDIF
  udata%idx = 1
  udata%field = H5O_INFO_TIME_F
  CALL h5ovisit_by_name_f(fid, object_name, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_by_name_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_by_name_f", -1, total_error)
  ENDIF
  udata%idx = 1
  udata%field =  H5O_INFO_NUM_ATTRS_F
  CALL h5ovisit_by_name_f(fid, object_name, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_by_name_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_by_name_f", -1, total_error)
  ENDIF
  udata%idx = 1
  udata%field = H5O_INFO_HDR_F
  CALL h5ovisit_by_name_f(fid, object_name, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_by_name_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_by_name_f", -1, total_error)
  ENDIF
  udata%idx = 1
  udata%field = H5O_INFO_META_SIZE_F
  CALL h5ovisit_by_name_f(fid, object_name, H5_INDEX_NAME_F, H5_ITER_INC_F, fun_ptr, f_ptr, ret_val, error, fields=udata%field)
  CALL check("h5ovisit_by_name_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_by_name_f", -1, total_error)
  ENDIF
  
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error, total_error)

END SUBROUTINE obj_visit

!****************************************************************
!**
!**  test_h5o_refcount(): Test H5O info functions.
!**
!****************************************************************

SUBROUTINE obj_info(total_error)

  USE HDF5
  USE TH5_MISC
  USE ISO_C_BINDING
  IMPLICIT NONE

  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(hid_t) :: fid = -1             ! File ID 
  INTEGER(hid_t) :: gid = -1, gid2 = -1  ! Group IDs 
  INTEGER(hid_t) :: did                  ! Dataset ID 
  INTEGER(hid_t) :: sid                  ! Dataspace ID 
  TYPE(hobj_ref_t_f), TARGET :: wref     ! Reference to write 
  TYPE(hobj_ref_t_f), TARGET :: rref     ! Reference to read
  TYPE(H5O_info_t) :: oinfo              ! Object info struct 
  INTEGER :: error
  TYPE(C_PTR) :: f_ptr

  CHARACTER(LEN=6) :: GROUPNAME   =    "/group"
  CHARACTER(LEN=6) :: GROUPNAME2  =    "group2"
  CHARACTER(LEN=6) :: GROUPNAME3  =    "group3"
  CHARACTER(LEN=5) :: DSETNAME    =    "/dset"
  CHARACTER(LEN=5) :: DSETNAME2   =    "dset2"

  ! Create file with a group and a dataset containing an object reference to the group
  CALL h5fcreate_f("get_info.h5", H5F_ACC_TRUNC_F, fid, error)
  CALL check("h5fcreate_f",error, total_error)

  ! Create dataspace to use for dataset
  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f",error,total_error)

  ! Create group to refer to
  CALL h5gcreate_f(fid,  GROUPNAME, gid, error)
  CALL check("h5gcreate_f",error,total_error)

  ! Create nested groups 
  CALL h5gcreate_f(gid,  GROUPNAME2, gid2, error)
  CALL check("h5gcreate_f",error,total_error)
  CALL h5gclose_f(gid2, error)
  CALL check("h5gclose_f",error,total_error)

  CALL h5gcreate_f(gid,  GROUPNAME3, gid2, error)
  CALL check("h5gcreate_f",error,total_error)
  CALL h5gclose_f(gid2, error)
  CALL check("h5gclose_f",error,total_error)

  ! Create bottom dataset
  CALL h5dcreate_f(gid, DSETNAME2, H5T_NATIVE_INTEGER, sid, did, error)
  CALL check("h5dcreate_f",error, total_error)

  CALL h5dclose_f(did, error)
  CALL check("h5dclose_f", error, total_error)

  CALL h5gclose_f(gid, error)
  CALL check("h5gclose_f",error,total_error)

  ! Create dataset
  CALL h5dcreate_f(fid, DSETNAME, H5T_STD_REF_OBJ, sid, did, error)
  CALL check("h5dcreate_f",error, total_error)

  f_ptr = C_LOC(wref)

  ! Create reference to group
  CALL h5rcreate_f(fid, GROUPNAME, H5R_OBJECT_F, f_ptr, error)
  CALL check("h5rcreate_f",error, total_error)

  ! Write reference to disk
  CALL h5dwrite_f(did, H5T_STD_REF_OBJ, f_ptr, error)
  CALL check("h5dwrite_f",error, total_error)

  ! Close objects 
  CALL h5dclose_f(did, error)
  CALL check("h5dclose_f", error, total_error)
  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f", error, total_error)

  ! Re-open file
  CALL h5fopen_f("get_info.h5", H5F_ACC_RDWR_F, fid, error)
  CALL check("h5fopen_f", error, total_error)

  ! Re-open dataset
  CALL h5dopen_f(fid, DSETNAME, did, error)
  CALL check("h5dopen_f", error, total_error)

  ! Read in the reference

  f_ptr = C_LOC(rref)

  CALL h5dread_f(did, H5T_STD_REF_OBJ, f_ptr, error)
  CALL check("H5Dread_f",error, total_error)

  ! Dereference to get the group

  CALL h5rdereference_f(did, H5R_OBJECT_F, f_ptr, gid, error)
  CALL check("h5rdereference_f", error, total_error)

  CALL h5oget_info_by_idx_f(gid, ".", H5_INDEX_NAME_F, H5_ITER_INC_F, 0_hsize_t, oinfo, error)
  CALL check("h5oget_info_by_idx_f", error, total_error)

  IF(oinfo%rc.NE.1)THEN
     CALL check("h5oget_info_by_idx_f", -1, total_error)
  ENDIF
  IF(oinfo%type.NE.H5O_TYPE_DATASET_F)THEN
     CALL check("h5oget_info_by_idx_f", -1, total_error)
  ENDIF

  ! Check partial fields
  CALL h5oget_info_by_idx_f(gid, ".", H5_INDEX_NAME_F, H5_ITER_INC_F, 0_hsize_t, oinfo, error, fields=H5O_INFO_BASIC_F )
  CALL check("h5oget_info_by_idx_f", error, total_error)

  IF(oinfo%rc.NE.1)THEN
     CALL check("h5oget_info_by_idx_f", -1, total_error)
  ENDIF
  IF(oinfo%type.NE.H5O_TYPE_DATASET_F)THEN
     CALL check("h5oget_info_by_idx_f", -1, total_error)
  ENDIF

  CALL h5oget_info_by_idx_f(gid, ".", H5_INDEX_NAME_F, H5_ITER_INC_F, 0_hsize_t, oinfo, error, fields=H5O_INFO_TIME_F )
  CALL check("h5oget_info_by_idx_f", error, total_error)
  ! These field values should not be filled
  IF(oinfo%rc.EQ.1)THEN
     CALL check("h5oget_info_by_idx_f", -1, total_error)
  ENDIF
  IF(oinfo%type.EQ.H5O_TYPE_DATASET_F)THEN
     CALL check("h5oget_info_by_idx_f", -1, total_error)
  ENDIF


  ! Close objects
  CALL h5dclose_f(did, error)
  CALL check("h5dclose_f", error, total_error)
  CALL h5gclose_f(gid, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f", error, total_error)

END SUBROUTINE obj_info

!-------------------------------------------------------------------------
! Function:    build_visit_file
!
! Purpose:     Build an "interesting" file to use for visiting links & objects
!
! Programmer:  M. Scot Breitenfeld
!              July 12, 2012
!              NOTE: Adapted from C test.
!
!-------------------------------------------------------------------------
!

SUBROUTINE build_visit_file(fid)

  USE HDF5
  USE TH5_MISC
  IMPLICIT NONE

  INTEGER(hid_t) :: fid                   ! File ID 
  INTEGER(hid_t) :: gid = -1, gid2 = -1   ! Group IDs
  INTEGER(hid_t) :: sid = -1              ! Dataspace ID
  INTEGER(hid_t) :: did = -1              ! Dataset ID
  INTEGER(hid_t) :: tid = -1              ! Datatype ID
  INTEGER(hid_t) :: aid = -1, aid2 = -1, aid3 = -1 ! Attribute ID
  CHARACTER(LEN=20) :: filename = 'visit.h5'
  INTEGER :: error
  
  ! Create file for visiting
  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, fid, error)

  ! Create group
  CALL H5Gcreate_f(fid, "/Group1", gid, error)

  ! Create nested group
  CALL H5Gcreate_f(gid, "Group2", gid2, error)

  CALL H5Screate_f(H5S_SCALAR_F, sid, error)
  CALL H5Acreate_f(gid2, "Attr1", H5T_NATIVE_INTEGER, sid, aid, error)
  CALL H5Acreate_f(gid2, "Attr2", H5T_NATIVE_INTEGER, sid, aid2, error)
  CALL H5Acreate_f(gid2, "Attr3", H5T_NATIVE_INTEGER, sid, aid3, error)
  CALL H5Aclose_f(aid,error)
  CALL H5Aclose_f(aid2,error)
  CALL H5Aclose_f(aid3,error)
  CALL H5Sclose_f(sid,error)

  ! Close groups
  CALL h5gclose_f(gid2, error)
  CALL h5gclose_f(gid, error)

  ! Create soft links to groups created
  CALL H5Lcreate_soft_f("/Group1", fid, "/soft_one", error)
  CALL H5Lcreate_soft_f("/Group1/Group2", fid, "/soft_two", error)

  ! Create dangling soft link
  CALL H5Lcreate_soft_f("nowhere", fid, "/soft_dangle", error)

  ! Create hard links to all groups
  CALL H5Lcreate_hard_f(fid, "/", fid, "hard_zero", error)
  CALL H5Lcreate_hard_f(fid, "/Group1", fid, "hard_one", error)
  CALL H5Lcreate_hard_f(fid, "/Group1/Group2", fid, "hard_two", error)

  ! Create loops w/hard links
  CALL H5Lcreate_hard_f(fid, "/Group1", fid, "/Group1/hard_one", error)
  CALL H5Lcreate_hard_f(fid, "/", fid, "/Group1/Group2/hard_zero", error)

  ! Create dataset in each group
  CALL H5Screate_f(H5S_SCALAR_F, sid, error)

  CALL H5Dcreate_f(fid, "/Dataset_zero", H5T_NATIVE_INTEGER, sid, did, error)
  CALL H5Dclose_f(did, error)

  CALL H5Dcreate_f(fid, "/Group1/Dataset_one", H5T_NATIVE_INTEGER, sid, did, error)
  CALL H5Dclose_f(did, error)

  CALL H5Dcreate_f(fid, "/Group1/Group2/Dataset_two", H5T_NATIVE_INTEGER, sid, did, error)
  CALL H5Dclose_f(did, error)

  CALL H5Sclose_f(sid, error)

  ! Create named datatype in each group
  CALL H5Tcopy_f(H5T_NATIVE_INTEGER, tid, error)

  CALL H5Tcommit_f(fid, "/Type_zero", tid, error)
  CALL H5Tclose_f(tid, error)

  CALL H5Tcopy_f(H5T_NATIVE_INTEGER, tid, error)
  CALL H5Tcommit_f(fid, "/Group1/Type_one", tid, error)
  CALL H5Tclose_f(tid, error)

  CALL H5Tcopy_f(H5T_NATIVE_INTEGER, tid, error)
  CALL H5Tcommit_f(fid, "/Group1/Group2/Type_two", tid, error)
  CALL H5Tclose_f(tid, error)

END SUBROUTINE build_visit_file

END MODULE TH5O_F03
