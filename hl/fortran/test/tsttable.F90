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
!
! This file contains the FORTRAN90 tests for H5LT
!
#include <H5config_f.inc>

MODULE TSTTABLE

CONTAINS

!-------------------------------------------------------------------------
! test_begin
!-------------------------------------------------------------------------

SUBROUTINE test_begin(string)
  CHARACTER(LEN=*), INTENT(IN) :: string
  WRITE(*, fmt = '(A)', ADVANCE = 'no') string
END SUBROUTINE test_begin

!-------------------------------------------------------------------------
! passed
!-------------------------------------------------------------------------

SUBROUTINE passed()
  WRITE(*, fmt = '(T12,A6)')  'PASSED'
END SUBROUTINE passed

END MODULE TSTTABLE


MODULE TSTTABLE_TESTS

  USE TH5_MISC_GEN
  IMPLICIT NONE

CONTAINS

!-------------------------------------------------------------------------
! test_table1
!-------------------------------------------------------------------------

SUBROUTINE test_table1()

  USE H5TB ! module of H5TB
  USE HDF5 ! module of HDF5 library
  USE TSTTABLE ! module for testing table support routines

  IMPLICIT NONE
  
  CHARACTER(len=8), PARAMETER :: filename = "f1tab.h5"   ! File name
  CHARACTER(LEN=5), PARAMETER :: dsetname1 = "dset1"     ! Dataset name
  INTEGER(HID_T) :: file_id                              ! File identifier
  INTEGER(HSIZE_T), PARAMETER :: nfields  = 4            ! nfields
  INTEGER(HSIZE_T), PARAMETER :: nrecords = 5            ! nrecords
  CHARACTER(LEN=9),DIMENSION(1:nfields) :: field_names  ! field names
  INTEGER(SIZE_T),  DIMENSION(1:nfields) :: field_offset ! field offset
  INTEGER(HID_T),   DIMENSION(1:nfields) :: field_types  ! field types
  INTEGER(HSIZE_T), PARAMETER  :: chunk_size = 5         ! chunk size
  INTEGER, PARAMETER :: compress = 0                     ! compress
  INTEGER            :: errcode = 0                      ! Error flag
  INTEGER            :: i                                ! general purpose integer
  INTEGER(SIZE_T)    :: type_size                        ! Size of the datatype
  INTEGER(SIZE_T)    :: type_sizec                       ! Size of the character datatype
  INTEGER(SIZE_T)    :: type_sizei                       ! Size of the integer datatype
  INTEGER(SIZE_T)    :: type_sized                       ! Size of the double precision datatype
  INTEGER(SIZE_T)    :: type_sizer                       ! Size of the real datatype
  INTEGER(HID_T)     :: type_id_c                        ! Memory datatype identifier (for character field)
  INTEGER(SIZE_T)    :: offset                           ! Member's offset
  INTEGER(HSIZE_T)   :: start = 0                        ! start record
  INTEGER, DIMENSION(nrecords) :: bufi                   ! Data buffer
  INTEGER, DIMENSION(nrecords) :: bufir                  ! Data buffer
  REAL, DIMENSION(nrecords) :: bufr                      ! Data buffer
  REAL, DIMENSION(nrecords) :: bufrr                     ! Data buffer
  DOUBLE PRECISION, DIMENSION(nrecords) :: bufd          ! Data buffer
  DOUBLE PRECISION, DIMENSION(nrecords) :: bufdr         ! Data buffer
  CHARACTER(LEN=2), DIMENSION(nrecords), PARAMETER :: bufs = (/"AB","CD","EF","GH","IJ"/) ! Data buffer
  CHARACTER(LEN=2), DIMENSION(nrecords) :: bufsr         ! Data buffer
  INTEGER(HSIZE_T) :: nfieldsr                           ! nfields
  INTEGER(HSIZE_T) :: nrecordsr                          ! nrecords
  CHARACTER(LEN=9), DIMENSION(1:nfields) :: field_namesr  ! field names
  INTEGER(SIZE_T),  DIMENSION(1:nfields) :: field_offsetr ! field offset
  INTEGER(SIZE_T),  DIMENSION(1:nfields) :: field_sizesr  ! field sizes
  INTEGER(SIZE_T)  :: type_sizeout = 0                    ! size of the datatype
  INTEGER :: maxlen = 0                                   ! max chararter length of a field name
  INTEGER :: Cs_sizeof_double = H5_SIZEOF_DOUBLE          ! C's sizeof double
  INTEGER :: SIZEOF_X
  LOGICAL :: Exclude_double
  CHARACTER(LEN=62) :: test_txt

  ! Find size of DOUBLE PRECISION
#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
  SIZEOF_X = storage_size(bufd(1))/storage_size(c_char_'a')
#else
  SIZEOF_X = SIZEOF(bufd(1))
#endif

  ! If Fortran DOUBLE PRECISION and C DOUBLE sizeofs don't match then disable 
  ! creating a DOUBLE RECISION field, and instead create a REAL field. This
  ! is needed to handle when DOUBLE PRECISION is promoted via a compiler flag.
  Exclude_double = .FALSE.
  IF(Cs_sizeof_double.NE.SIZEOF_X)THEN
     Exclude_double = .TRUE.
  ENDIF

  !
  ! Initialize the data arrays.
  !
  DO i = 1, nrecords
     bufi(i) = i
     bufr(i) = i
     bufd(i) = i
  END DO

  !
  ! Create a new file using default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)


  !-------------------------------------------------------------------------
  ! make table
  ! initialize the table parameters
  !-------------------------------------------------------------------------

  field_names(1) = "field1"
  field_names(2) = "field2a"
  field_names(3) = "field3ab"
  field_names(4) = "field4abc"

  !
  ! calculate total size by calculating sizes of each member
  !
  CALL h5tcopy_f(H5T_NATIVE_CHARACTER, type_id_c, errcode)
  type_size = 2
  CALL h5tset_size_f(type_id_c, type_size, errcode)
  CALL h5tget_size_f(type_id_c, type_sizec, errcode)
  CALL h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, errcode)
  IF(exclude_double)THEN
     CALL h5tget_size_f(H5T_NATIVE_REAL, type_sized, errcode)
  ELSE
     CALL h5tget_size_f(H5T_NATIVE_DOUBLE, type_sized, errcode)
  ENDIF
  CALL h5tget_size_f(H5T_NATIVE_REAL, type_sizer, errcode)
  type_size = type_sizec + type_sizei + type_sized + type_sizer

  !
  ! type ID's
  !
  field_types(1) = type_id_c
  field_types(2) = H5T_NATIVE_INTEGER
  IF(exclude_double)THEN
     field_types(3) = H5T_NATIVE_REAL
  ELSE
     field_types(3) = H5T_NATIVE_DOUBLE
  ENDIF
  field_types(4) = H5T_NATIVE_REAL

  !
  ! offsets
  !
  offset = 0
  field_offset(1) = offset
  offset = offset + type_sizec ! Offset of the second memeber is 2
  field_offset(2) = offset
  offset = offset + type_sizei ! Offset of the second memeber is 6
  field_offset(3) = offset
  offset = offset + type_sized ! Offset of the second memeber is 14
  field_offset(4) = offset

  !-------------------------------------------------------------------------
  ! make table
  !-------------------------------------------------------------------------

  test_txt = "Make table"
  CALL test_begin(test_txt)

  CALL h5tbmake_table_f(dsetname1,&
       file_id,&
       dsetname1,&
       nfields,&
       nrecords,&
       type_size,&
       field_names,&
       field_offset,&
       field_types,&
       chunk_size,&
       compress,&
       errcode )

  CALL passed()


  !-------------------------------------------------------------------------
  ! write field
  !-------------------------------------------------------------------------

  test_txt = "Read/Write field by name"
  CALL test_begin(test_txt)

  CALL h5tbwrite_field_name_f(file_id,dsetname1,field_names(1),start,nrecords,type_sizec,&
       bufs,errcode)

  CALL h5tbwrite_field_name_f(file_id,dsetname1,field_names(2),start,nrecords,type_sizei,&
       bufi,errcode)
  IF(exclude_double)THEN
     CALL h5tbwrite_field_name_f(file_id,dsetname1,field_names(3),start,nrecords,type_sized,&
          bufr,errcode)
  ELSE
     CALL h5tbwrite_field_name_f(file_id,dsetname1,field_names(3),start,nrecords,type_sized,&
          bufd,errcode)
  ENDIF

  CALL h5tbwrite_field_name_f(file_id,dsetname1,field_names(4),start,nrecords,type_sizer,&
       bufr,errcode)

  !-------------------------------------------------------------------------
  ! read field
  !-------------------------------------------------------------------------

  ! Read an invalid field, should fail
  CALL h5tbread_field_name_f(file_id,dsetname1,'DoesNotExist',start,nrecords,type_sizec,&
       bufsr,errcode)

  IF(errcode.GE.0)THEN
     PRINT *, 'error in h5tbread_field_name_f'
     CALL h5fclose_f(file_id, errcode)
     CALL h5close_f(errcode)
     STOP
  ENDIF

  ! Read a valid field, should pass
  CALL h5tbread_field_name_f(file_id,dsetname1,field_names(1),start,nrecords,type_sizec,&
       bufsr,errcode)
  IF(errcode.LT.0)THEN
     PRINT *, 'error in h5tbread_field_name_f'
     CALL h5fclose_f(file_id, errcode)
     CALL h5close_f(errcode)
     STOP
  ENDIF

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     IF ( bufsr(i) .NE. bufs(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufsr(i), ' and ',   bufs(i)
        STOP
     ENDIF
  END DO

  CALL h5tbread_field_name_f(file_id,dsetname1,field_names(2),start,nrecords,type_sizei,&
       bufir,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     IF ( bufir(i) .NE. bufi(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufir(i), ' and ',   bufi(i)
        STOP
     ENDIF
  END DO

  IF(exclude_double)THEN

     CALL h5tbread_field_name_f(file_id,dsetname1,field_names(3),start,nrecords,type_sized,&
          bufrr,errcode)

  !
  ! compare read and write buffers.
  !
     DO i = 1, nrecords
       CALL VERIFY("h5tbread_field_name_f", bufrr(i), bufr(i), errcode)
       IF (errcode .NE.0 ) THEN
           PRINT *, 'read buffer differs from write buffer'
           PRINT *,  bufrr(i), ' and ',   bufr(i)
           STOP
        ENDIF
     END DO

  ELSE
     CALL h5tbread_field_name_f(file_id,dsetname1,field_names(3),start,nrecords,type_sized,&
          bufdr,errcode)

  !
  ! compare read and write buffers.
  !
     DO i = 1, nrecords
        CALL VERIFY("h5tbread_field_name_f", bufdr(i), bufd(i), errcode)
        IF (errcode .NE.0 ) THEN
           PRINT *, 'read buffer differs from write buffer'
           PRINT *,  bufdr(i), ' and ',   bufd(i)
           STOP
        ENDIF
     END DO
  ENDIF



  CALL h5tbread_field_name_f(file_id,dsetname1,field_names(4),start,nrecords,type_sizer,&
       bufrr,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     CALL VERIFY("h5tbread_field_name_f", bufrr(i), bufr(i), errcode)
     IF (errcode .NE.0 ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufrr(i), ' and ',   bufr(i)
        STOP
     ENDIF
  END DO

  CALL passed()


  !-------------------------------------------------------------------------
  ! write field
  !-------------------------------------------------------------------------

  test_txt = "Read/Write field by index"
  CALL test_begin(test_txt)

  CALL h5tbwrite_field_index_f(file_id,dsetname1,1,start,nrecords,type_sizec,&
       bufs,errcode)

  CALL h5tbwrite_field_index_f(file_id,dsetname1,2,start,nrecords,type_sizei,&
       bufi,errcode)

  IF(exclude_double)THEN
     CALL h5tbwrite_field_index_f(file_id,dsetname1,3,start,nrecords,type_sized,&
          bufr,errcode)
  ELSE
     CALL h5tbwrite_field_index_f(file_id,dsetname1,3,start,nrecords,type_sized,&
          bufd,errcode)
  ENDIF

  CALL h5tbwrite_field_index_f(file_id,dsetname1,4,start,nrecords,type_sizer,&
       bufr,errcode)



  !-------------------------------------------------------------------------
  ! read field
  !-------------------------------------------------------------------------

  CALL h5tbread_field_index_f(file_id,dsetname1,1,start,nrecords,type_sizec,&
       bufsr,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     CALL VERIFY("h5tbread_field_index_f", bufsr(i), bufs(i), errcode)
     IF (errcode .NE.0 ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufsr(i), ' and ',   bufs(i)
        STOP
     ENDIF
  END DO

  CALL h5tbread_field_index_f(file_id,dsetname1,2,start,nrecords,type_sizei,&
       bufir,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     CALL VERIFY("h5tbread_field_index_f", bufir(i), bufi(i), errcode)
     IF (errcode .NE.0 ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufir(i), ' and ',   bufi(i)
        STOP
     ENDIF
  END DO
  IF(exclude_double)THEN
     CALL h5tbread_field_index_f(file_id,dsetname1,3,start,nrecords,type_sized,&
          bufrr,errcode)

     !
     ! compare read and write buffers.
     !
     DO i = 1, nrecords
        CALL VERIFY("h5tbread_field_index_f", bufrr(i), bufr(i), errcode)
        IF (errcode .NE.0 ) THEN
           PRINT *, 'read buffer differs from write buffer'
           PRINT *,  bufrr(i), ' and ',   bufr(i)
           STOP
        ENDIF
     END DO
  ELSE
     CALL h5tbread_field_index_f(file_id,dsetname1,3,start,nrecords,type_sized,&
          bufdr,errcode)

     !
     ! compare read and write buffers.
     !
     DO i = 1, nrecords
        CALL VERIFY("h5tbread_field_index_f", bufdr(i), bufd(i), errcode)
        IF (errcode .NE.0 ) THEN
           PRINT *, 'read buffer differs from write buffer'
           PRINT *,  bufdr(i), ' and ',   bufd(i)
           STOP
        ENDIF
     END DO
  ENDIF

  CALL h5tbread_field_index_f(file_id,dsetname1,4,start,nrecords,type_sizer,&
       bufrr,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     CALL VERIFY("h5tbread_field_index_f", bufrr(i), bufr(i), errcode)
     IF (errcode .NE.0 ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufrr(i), ' and ',   bufr(i)
        STOP
     ENDIF
  END DO


  CALL passed()


  !-------------------------------------------------------------------------
  ! Insert field
  ! we insert a field callsed "field5" with the same type and buffer as field 4 (Real)
  !-------------------------------------------------------------------------
  test_txt = "Insert field"
  CALL test_begin(test_txt)

  CALL h5tbinsert_field_f(file_id,dsetname1,"field5",field_types(4),4,bufr,errcode)
  CALL h5tbread_field_index_f(file_id,dsetname1,5,start,nrecords,type_sizer,&
       bufrr,errcode)
  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     CALL VERIFY("h5tbread_field_index_f", bufrr(i), bufr(i), errcode)
     IF (errcode .NE.0 ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufrr(i), ' and ',   bufr(i)
        STOP
     ENDIF
  END DO


  CALL passed()

  !-------------------------------------------------------------------------
  ! Delete field
  !-------------------------------------------------------------------------

  test_txt = "Delete field"
  CALL test_begin(test_txt)

  CALL h5tbdelete_field_f(file_id,dsetname1,"field4abc",errcode)

  CALL passed()


  !-------------------------------------------------------------------------
  ! Gets the number of records and fields
  !-------------------------------------------------------------------------

  test_txt = "Get table info"
  CALL test_begin(test_txt)

  CALL h5tbget_table_info_f(file_id,dsetname1,nfieldsr,nrecordsr,errcode )

  IF ( nfieldsr .NE. nfields .AND. nrecordsr .NE. nrecords ) THEN
     PRINT *, 'h5tbget_table_info_f return error'
     STOP
  ENDIF

  CALL passed()

  !-------------------------------------------------------------------------
  ! Get information about fields
  !-------------------------------------------------------------------------

  test_txt = "Get fields info"
  CALL test_begin(test_txt)

  CALL h5tbget_field_info_f(file_id, dsetname1, nfields, field_namesr, field_sizesr,&
       field_offsetr, type_sizeout, errcode, maxlen )

  IF ( errcode.NE.0 ) THEN
     WRITE(*,'(/,5X,"H5TBGET_FIELD_INFO_F: RETURN ERROR")')
     STOP
  ENDIF
  ! "field4abc" was deleted and "field5" was added.
  field_names(4) = "field5"

  IF ( maxlen .NE. 8 ) THEN
     WRITE(*,'(/,5X,"H5TBGET_FIELD_INFO_F: INCORRECT MAXIMUM CHARACTER LENGTH OF THE FIELD NAMES")')
     WRITE(*,'(5X,"RETURNED VALUE = ", I0, ", CORRECT VALUE = ", I0)') maxlen, 8 
     STOP
  ENDIF

  DO i = 1,  nfields
     IF ( field_namesr(i) .NE. field_names(i)) THEN
        WRITE(*,'(/,5X,"H5TBGET_FIELD_INFO_F: READ/WRITE FIELD NAMES DIFFER")')
        WRITE(*,'(27X,A," AND ",A)') TRIM(field_namesr(i)), TRIM(field_names(i))
        STOP
     ENDIF
  END DO

  CALL passed()


  !-------------------------------------------------------------------------
  ! end
  !-------------------------------------------------------------------------

  !
  ! Close the file.
  !
  CALL h5fclose_f(file_id, errcode)

  !
  ! end function.
  !
END SUBROUTINE test_table1

!-------------------------------------------------------------------------
! test_table2
! Tests F2003 versions of H5TBread_table_f and H5TBmake_table_f
!-------------------------------------------------------------------------

SUBROUTINE test_table2()

  USE H5TB ! module of H5TB
  USE HDF5 ! module of HDF5 library
  USE TSTTABLE ! module for testing table support routines

  IMPLICIT NONE
  
  INTEGER, PARAMETER :: i8 = SELECTED_INT_KIND(9)   !should map to INTEGER*4 on most modern processors
  INTEGER, PARAMETER :: i16 = SELECTED_INT_KIND(9) ! (18) !should map to INTEGER*8 on most modern processors
  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(5)  ! This should map to REAL*4 on most modern processors
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(10) ! This should map to REAL*8 on most modern processors

  TYPE particle_t
     SEQUENCE
     CHARACTER(LEN=11) :: name
     INTEGER(KIND=i8) :: lati
     INTEGER(KIND=i16) :: long
     REAL(KIND=sp) :: pressure
     REAL(KIND=dp) :: temperature
  END TYPE particle_t

  INTEGER(HSIZE_T), PARAMETER :: nfields  = 5            ! nfields
  INTEGER(HSIZE_T), PARAMETER :: nrecords = 8            ! nrecords

  CHARACTER(len=8), PARAMETER :: filename = "f2tab.h5"   ! File name
  CHARACTER(LEN=5), PARAMETER :: table_name = "tabel"    ! table name
  CHARACTER(LEN=10), PARAMETER :: table_name_fill = "tabel_fill"    ! table name

  ! Define field information
  CHARACTER(LEN=11), DIMENSION(1:NFIELDS), PARAMETER :: field_names = (/&
       "Name       ", &
       "Latitude   ", &
       "Longitude  ", &
       "Pressure   ", &
       "Temperature"  &
       /)

  INTEGER(hid_t), DIMENSION(1:nfields) :: field_type
  INTEGER(hid_t) :: string_type
  INTEGER(hid_t) :: file_id
  INTEGER(hsize_t), PARAMETER :: chunk_size = 10
  TYPE(particle_t), DIMENSION(1:nrecords), TARGET :: fill_data
  INTEGER :: compress
  INTEGER :: i
  INTEGER(SIZE_T) :: dst_size
  TYPE(particle_t), DIMENSION(1:nrecords), TARGET :: dst_buf
  INTEGER(SIZE_T), DIMENSION(1:nfields) :: dst_offset
  INTEGER(SIZE_T), DIMENSION(1:nfields) :: dst_sizes
  TYPE(particle_t), DIMENSION(1:nrecords), TARGET :: p_data
  TYPE(particle_t), DIMENSION(1:nrecords), TARGET :: r_data

  TYPE(C_PTR) :: f_ptr1, f_ptr2, f_ptr3

  INTEGER :: errcode
  CHARACTER(LEN=62) :: test_txt

  test_txt = "Testing H5TBread_table_f and H5TBmake_table_f (F2003)"
  CALL test_begin(test_txt)

  
  ! Define an array of Particles
  p_data(1:nrecords) = (/ &
       particle_t("zero       ",0_i8,0_i16,0.0_sp,0.0_dp),     &
       particle_t("one        ",10_i8,10_i16,10.0_sp,10.0_dp),  &
       particle_t("two        ",20_i8,20_i16,20.0_sp,20.0_dp),  &
       particle_t("three      ",30_i8,30_i16,30.0_sp,30.0_dp),&
       particle_t("four       ",40_i8,40_i16,40.0_sp,40.0_dp), &
       particle_t("five       ",50_i8,50_i16,50.0_sp,50.0_dp), &
       particle_t("six        ",60_i8,60_i16,60.0_sp,60.0_dp),  &
       particle_t("seven      ",70_i8,70_i16,70.0_sp,70.0_dp) &
       /)

  fill_data(1:nrecords) = particle_t("no data",-1_i8, -2_i16, -99.0_sp, -100.0_dp)

  compress = 0
  dst_size = H5OFFSETOF(C_LOC(dst_buf(1)), C_LOC(dst_buf(2)))

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
  dst_sizes(1:nfields) = (/ &
       storage_size(dst_buf(1)%name)/storage_size(c_char_'a'), &
       storage_size(dst_buf(1)%lati)/storage_size(c_char_'a'), &
       storage_size(dst_buf(1)%long)/storage_size(c_char_'a'), &
       storage_size(dst_buf(1)%pressure)/storage_size(c_char_'a'), &
       storage_size(dst_buf(1)%temperature)/storage_size(c_char_'a') &
       /)
#else
  dst_sizes(1:nfields) = (/ &
       sizeof(dst_buf(1)%name), &
       sizeof(dst_buf(1)%lati), &
       sizeof(dst_buf(1)%long), &
       sizeof(dst_buf(1)%pressure), &
       sizeof(dst_buf(1)%temperature) &
       /)
#endif

  dst_offset(1:nfields) = (/ & 
       H5OFFSETOF(C_LOC(dst_buf(1)), C_LOC(dst_buf(1)%name(1:1))), &
       H5OFFSETOF(C_LOC(dst_buf(1)), C_LOC(dst_buf(1)%lati)), &
       H5OFFSETOF(C_LOC(dst_buf(1)), C_LOC(dst_buf(1)%long)), &
       H5OFFSETOF(C_LOC(dst_buf(1)), C_LOC(dst_buf(1)%pressure)), &
       H5OFFSETOF(C_LOC(dst_buf(1)), C_LOC(dst_buf(1)%temperature)) &
       /)

  ! Initialize field_type
  CALL H5Tcopy_f(H5T_FORTRAN_S1, string_type, errcode)
  CALL H5Tset_size_f(string_type, INT(11,size_t), errcode)

  field_type(1:5) = (/ &
       string_type,&
       h5kind_to_type(KIND(dst_buf(1)%lati), H5_INTEGER_KIND),&
       h5kind_to_type(KIND(dst_buf(1)%long), H5_INTEGER_KIND),&
       h5kind_to_type(KIND(dst_buf(1)%pressure), H5_REAL_KIND),&
       h5kind_to_type(KIND(dst_buf(1)%temperature), H5_REAL_KIND) &
       /)

  !
  ! Create a new file using default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

  ! Check setting the fill values

  f_ptr1 = C_NULL_PTR
  f_ptr2 = C_LOC(fill_data(1)%name(1:1))
  CALL h5tbmake_table_f("Table Title Fill", file_id, table_name_fill, nfields, nrecords, &
       dst_size, field_names, dst_offset, field_type, &
       chunk_size, f_ptr2, compress, f_ptr1, errcode )

  f_ptr3 = C_LOC(r_data(1)%name(1:1))
  CALL h5tbread_table_f(file_id, table_name_fill, nfields, dst_size, dst_offset, dst_sizes, f_ptr3, errcode)

  DO i = 1, nfields
     CALL VERIFY("h5tbread_table_f", r_data(i)%name, fill_data(i)%name, errcode)
     CALL VERIFY("h5tbread_table_f", r_data(i)%lati, fill_data(i)%lati, errcode)
     CALL VERIFY("h5tbread_table_f", r_data(i)%long, fill_data(i)%long, errcode)
     CALL VERIFY("h5tbread_table_f", r_data(i)%pressure, fill_data(i)%pressure, errcode)
     CALL VERIFY("h5tbread_table_f", r_data(i)%temperature, fill_data(i)%temperature, errcode)
     IF (errcode .NE.0 ) THEN
        PRINT*,'H5TBmake/read_table_f --filled-- FAILED'
        STOP
     ENDIF
  ENDDO

  ! Check setting the table values

  f_ptr1 = C_LOC(p_data(1)%name(1:1))
  f_ptr2 = C_NULL_PTR
  
  CALL h5tbmake_table_f("Table Title",file_id, table_name, nfields, nrecords, &
       dst_size, field_names, dst_offset, field_type, &
       chunk_size, f_ptr2, compress, f_ptr1, errcode )

  f_ptr3 = C_LOC(r_data(1)%name(1:1))
  CALL h5tbread_table_f(file_id, table_name, nfields, dst_size, dst_offset, dst_sizes, f_ptr3, errcode)

  DO i = 1, nfields
     CALL VERIFY("h5tbread_table_f", r_data(i)%name, p_data(i)%name, errcode)
     CALL VERIFY("h5tbread_table_f", r_data(i)%lati, p_data(i)%lati, errcode)
     CALL VERIFY("h5tbread_table_f", r_data(i)%long, p_data(i)%long, errcode)
     CALL VERIFY("h5tbread_table_f", r_data(i)%pressure, p_data(i)%pressure, errcode)
     CALL VERIFY("h5tbread_table_f", r_data(i)%temperature, p_data(i)%temperature, errcode)
     IF (errcode .NE.0 ) THEN
        PRINT*,'H5TBmake/read_table_f FAILED'
        STOP
     ENDIF
  ENDDO

  CALL passed()

  !-------------------------------------------------------------------------
  ! end
  !-------------------------------------------------------------------------

  !
  ! Close the file.
  !
  CALL h5fclose_f(file_id, errcode)

END SUBROUTINE test_table2

END MODULE TSTTABLE_TESTS


PROGRAM table_test

  USE H5TB ! module of H5TB
  USE HDF5 ! module of HDF5 library
  USE TSTTABLE_TESTS ! module for testing table routines

  IMPLICIT NONE
  INTEGER :: errcode = 0

  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(errcode)

  CALL test_table1()
  CALL test_table2()

  !
  ! Close FORTRAN predefined datatypes.
  !
  CALL h5close_f(errcode)

END PROGRAM table_test


