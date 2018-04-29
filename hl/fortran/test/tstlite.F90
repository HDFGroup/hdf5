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
!
! This file contains the FORTRAN90 tests for H5LT
!
#include <H5config_f.inc>

MODULE TSTLITE

  USE TH5_MISC_GEN
  IMPLICIT NONE

CONTAINS

  !-------------------------------------------------------------------------
  ! test_begin
  !-------------------------------------------------------------------------

  SUBROUTINE test_begin(string)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: string
    WRITE(*, fmt = '(14a)', advance = 'no') string
    WRITE(*, fmt = '(40x,a)', advance = 'no') ' '
  END SUBROUTINE test_begin

  !-------------------------------------------------------------------------
  ! passed
  !-------------------------------------------------------------------------

  SUBROUTINE passed()
    IMPLICIT NONE
    WRITE(*, fmt = '(6a)')  'PASSED'
  END SUBROUTINE passed

END MODULE TSTLITE

MODULE TSTLITE_TESTS

  USE, INTRINSIC :: ISO_C_BINDING
  USE H5LT    ! module of H5LT
  USE HDF5    ! module of HDF5 library
  USE TSTLITE ! module for testing lite support routines
  IMPLICIT NONE

CONTAINS


  !-------------------------------------------------------------------------
  ! test_dataset1D
  !-------------------------------------------------------------------------

  SUBROUTINE test_dataset1D()

    IMPLICIT NONE

    INTEGER, PARAMETER :: DIM1 = 4                       ! Dimension of array
    CHARACTER(len=9), PARAMETER :: filename = "dsetf1.h5"! File name
    CHARACTER(LEN=5), PARAMETER :: dsetname1 = "dset1"   ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2"   ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname3 = "dset3"   ! Dataset name
    INTEGER(HID_T) :: file_id                            ! File identifier
    INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/DIM1/)    ! Dataset dimensions
    INTEGER        :: rank = 1                           ! Dataset rank
    INTEGER, DIMENSION(DIM1) :: buf1                     ! Data buffer
    INTEGER, DIMENSION(DIM1) :: bufr1                    ! Data buffer
    REAL, DIMENSION(DIM1)    :: buf2                     ! Data buffer
    REAL, DIMENSION(DIM1)    :: bufr2                    ! Data buffer
    DOUBLE PRECISION, DIMENSION(DIM1), TARGET :: buf3    ! Data buffer
    DOUBLE PRECISION, DIMENSION(DIM1), TARGET :: bufr3   ! Data buffer
    INTEGER        :: errcode                            ! Error flag
    INTEGER        :: i                                  ! general purpose integer
    TYPE(C_PTR) :: f_ptr
    integer(HID_T) :: mytype

    CALL test_begin(' Make/Read datasets (1D)        ')

    !
    ! Initialize the data array.
    !
    DO i = 1, DIM1
       buf1(i) = i
       buf2(i) = i
       buf3(i) = i
    END DO

    !
    ! Initialize FORTRAN predefined datatypes.
    !
    CALL h5open_f(errcode)

    !
    ! Create a new file using default properties.
    !
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_INTEGER
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    CALL h5ltmake_dataset_f(file_id, dsetname1, rank, dims, H5T_NATIVE_INTEGER, buf1, errcode)
    !
    ! read dataset.
    !
    CALL h5ltread_dataset_f(file_id, dsetname1, H5T_NATIVE_INTEGER, bufr1, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, DIM1
       CALL VERIFY("h5ltread_dataset_f",buf1(i), bufr1(i), errcode)
       IF (errcode .NE.0 ) THEN
          PRINT *, 'read buffer differs from write buffer (I)'
          PRINT *,  bufr1(i), ' and ',   buf1(i)
          STOP
       ENDIF
    END DO

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_REAL
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims, H5T_NATIVE_REAL, buf2, errcode)

    !
    ! read dataset.
    !
    CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_REAL, bufr2, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, DIM1
       CALL VERIFY("h5ltread_dataset_f",buf2(i), bufr2(i), errcode)
       IF (errcode .NE.0 ) THEN
          PRINT *, 'read buffer differs from write buffer (R)'
          PRINT *,  bufr2(i), ' and ',   buf2(i)
          STOP
       ENDIF
    END DO

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_DOUBLE
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    f_ptr = C_LOC(buf3(1))
    mytype = h5kind_to_type(KIND(buf3(1)), H5_REAL_KIND)
    CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims, &
         mytype, f_ptr, errcode)
    !CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_DOUBLE, buf3, errcode)
    ! h5kind_to_type(KIND(buf3(1)), H5_REAL_KIND)
    !
    ! read dataset.
    !
    f_ptr = C_LOC(bufr3(1))
    CALL h5ltread_dataset_f(file_id, dsetname3, &
         h5kind_to_type(KIND(bufr3(1)), H5_REAL_KIND), f_ptr, errcode)
    !CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_DOUBLE, bufr3, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, DIM1
       CALL VERIFY("h5ltread_dataset_f",buf3(i), bufr3(i), errcode)
       IF (errcode .NE.0 ) THEN
          PRINT *, 'read buffer differs from write buffer (D)'
          PRINT *,  bufr3(i), ' and ',   buf3(i)
          STOP
       ENDIF
    END DO

    !
    ! Close the file.
    !
    CALL h5fclose_f(file_id, errcode)

    !
    ! Close FORTRAN predefined datatypes.
    !
    CALL h5close_f(errcode)

    CALL passed()
    !
    ! end function.
    !
  END SUBROUTINE test_dataset1D

  !-------------------------------------------------------------------------
  ! test_dataset2D
  !-------------------------------------------------------------------------

  SUBROUTINE test_dataset2D()

    IMPLICIT NONE

    INTEGER(HSIZE_T), PARAMETER :: DIM1 = 4              ! columns
    INTEGER(HSIZE_T), PARAMETER :: DIM2 = 6              ! rows
    CHARACTER(len=9), PARAMETER :: filename = "dsetf2.h5"! File name
    CHARACTER(LEN=5), PARAMETER :: dsetname1 = "dset1"   ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2"   ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname3 = "dset3"   ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname4 = "dset4"   ! Dataset name
    INTEGER(HID_T) :: file_id                            ! File identifier
    INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/4,6/)     ! Dataset dimensions
    INTEGER        :: rank = 2                           ! Dataset rank
    INTEGER, DIMENSION(DIM1*DIM2) :: buf                 ! Data buffer
    INTEGER, DIMENSION(DIM1*DIM2) :: bufr                ! Data buffer
    INTEGER, DIMENSION(DIM1,DIM2) :: buf2                ! Data buffer
    INTEGER, DIMENSION(DIM1,DIM2) :: buf2r               ! Data buffer
    REAL, DIMENSION(DIM1,DIM2), TARGET    :: buf3                ! Data buffer
    REAL, DIMENSION(DIM1,DIM2), TARGET    :: buf3r               ! Data buffer
    DOUBLE PRECISION, DIMENSION(DIM1,DIM2), TARGET :: buf4       ! Data buffer
    DOUBLE PRECISION, DIMENSION(DIM1,DIM2), TARGET :: buf4r      ! Data buffer
    INTEGER        :: errcode                            ! Error flag
    INTEGER(HSIZE_T) :: i, j, n                            ! general purpose integers
    TYPE(C_PTR) :: f_ptr

    CALL test_begin(' Make/Read datasets (2D)        ')


    !
    ! Initialize the data arrays.
    !
    n=1
    DO i = 1, DIM1*DIM2
       buf(i) = INT(n)
       n = n + 1
    END DO

    DO i = 1, dims(1)
       DO j = 1, dims(2)
          buf2(i,j) = INT((i-1)*dims(2) + j)
          buf3(i,j) = INT((i-1)*dims(2) + j)
          buf4(i,j) = INT((i-1)*dims(2) + j)
       END DO
    END DO

    !
    ! Initialize FORTRAN predefined datatypes.
    !
    CALL h5open_f(errcode)

    !
    ! Create a new file using default properties.
    !
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_INT 1D buffer
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    CALL h5ltmake_dataset_f(file_id, dsetname1, rank, dims, H5T_NATIVE_INTEGER, buf, errcode)

    !
    ! read dataset.
    !
    CALL h5ltread_dataset_f(file_id, dsetname1, H5T_NATIVE_INTEGER, bufr, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, DIM1*DIM2
       IF ( buf(i) .NE. bufr(i) ) THEN
          PRINT *, 'read buffer differs from write buffer'
          PRINT *,  bufr(i), ' and ',   buf(i)
          STOP
       ENDIF
    END DO

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_INT 2D buffer
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims, H5T_NATIVE_INTEGER, buf2, errcode)

    !
    ! read dataset.
    !
    CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, buf2r, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          IF ( buf2(i,j) .NE. buf2r(i,j) ) THEN
             PRINT *, 'read buffer differs from write buffer'
             PRINT *,  buf2r(i,j), ' and ',   buf2(i,j)
             STOP
          ENDIF
       END DO
    END DO

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_REAL
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    f_ptr = C_LOC(buf3(1,1))
    CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_REAL, f_ptr, errcode)
    !CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_REAL, buf3, errcode)

    !
    ! read dataset.
    !
    f_ptr = C_LOC(buf3r(1,1))
    CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, f_ptr, errcode)
    !CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, buf3r, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          CALL VERIFY("h5ltread_dataset_f",buf3(i,j), buf3r(i,j), errcode)
          IF (errcode .NE.0 ) THEN
             PRINT *, 'read buffer differs from write buffer'
             PRINT *,  buf3r(i,j), ' and ',   buf3(i,j)
             STOP
          ENDIF
       END DO
    END DO

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_DOUBLE
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    f_ptr = C_LOC(buf4(1,1))
    CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims, H5T_NATIVE_DOUBLE, f_ptr, errcode)
    !CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims, H5T_NATIVE_DOUBLE, buf4, errcode)

    !
    ! read dataset.
    f_ptr = C_LOC(buf4r(1,1))
    CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, f_ptr, errcode)

    !CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, buf4r, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          CALL VERIFY("h5ltread_dataset_f", buf4(i,j), buf4r(i,j), errcode)
          IF (errcode .NE.0 ) THEN
             PRINT *, 'read buffer differs from write buffer'
             PRINT *,  buf4r(i,j), ' and ',   buf4(i,j)
             STOP
          ENDIF
       END DO
    END DO

    !
    ! Close the file.
    !
    CALL h5fclose_f(file_id, errcode)

    !
    ! Close FORTRAN predefined datatypes.
    !
    CALL h5close_f(errcode)

    CALL passed()
    !
    ! end function.
    !
  END SUBROUTINE test_dataset2D


  !-------------------------------------------------------------------------
  ! test_dataset3D
  !-------------------------------------------------------------------------


  SUBROUTINE test_dataset3D()

    IMPLICIT NONE

    INTEGER, PARAMETER :: DIM1 = 6                             ! columns
    INTEGER, PARAMETER :: DIM2 = 4                             ! rows
    INTEGER, PARAMETER :: DIM3 = 2                             ! layers
    CHARACTER(len=9), PARAMETER :: filename = "dsetf3.h5"       ! File name
    CHARACTER(LEN=5), PARAMETER :: dsetname1 = "dset1"          ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2"          ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname3 = "dset3"          ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname4 = "dset4"          ! Dataset name
    INTEGER(HID_T) :: file_id                                   ! File identifier
    INTEGER(HSIZE_T), DIMENSION(3) :: dims = (/DIM1,DIM2,DIM3/) ! Dataset dimensions
    INTEGER(HSIZE_T), DIMENSION(3) :: dimsr                     ! Dataset dimensions
    INTEGER, DIMENSION(DIM1*DIM2*DIM3) :: buf                   ! Data buffer
    INTEGER, DIMENSION(DIM1*DIM2*DIM3) :: bufr                  ! Data buffer
    INTEGER, DIMENSION(DIM1,DIM2,DIM3) :: buf2                  ! Data buffer
    INTEGER, DIMENSION(DIM1,DIM2,DIM3) :: buf2r                 ! Data buffer
    REAL, DIMENSION(DIM1,DIM2,DIM3), TARGET    :: buf3                  ! Data buffer
    REAL, DIMENSION(DIM1,DIM2,DIM3), TARGET    :: buf3r                 ! Data buffer
    DOUBLE PRECISION, DIMENSION(DIM1,DIM2,DIM3), TARGET :: buf4         ! Data buffer
    DOUBLE PRECISION, DIMENSION(DIM1,DIM2,DIM3), TARGET :: buf4r        ! Data buffer
    INTEGER        :: rank = 3                                  ! Dataset rank
    INTEGER        :: errcode                                   ! Error flag
    INTEGER(HSIZE_T) :: i, j, k, n                                ! general purpose integers
    INTEGER          :: type_class
    INTEGER(SIZE_T)  :: type_size
    TYPE(C_PTR) :: f_ptr
#if H5_HAVE_Fortran_INTEGER_SIZEOF_16!=0
    INTEGER, PARAMETER :: int_kind_32 = SELECTED_INT_KIND(36) !should map to INTEGER*16 on most modern processors
    INTEGER(int_kind_32), DIMENSION(DIM1,DIM2,DIM3), TARGET :: dset_data_i32, data_out_i32
    CHARACTER(LEN=7), PARAMETER :: dsetname16a = "dset16a"     ! Dataset name
    CHARACTER(LEN=7), PARAMETER :: dsetname16b = "dset16b"     ! Dataset name
    CHARACTER(LEN=7), PARAMETER :: dsetname16c = "dset16c"     ! Dataset name
    INTEGER(HID_T) :: type_id
#endif

    CALL test_begin(' Make/Read datasets (3D)        ')


    !
    ! Initialize the data array.
    !
    n=1
    DO i = 1, DIM1*DIM2*DIM3
       buf(i) = INT(n)
       n = n + 1
    END DO

    n = 1
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          DO k = 1, dims(3)
             buf2(i,j,k) = INT(n)
             buf3(i,j,k) = INT(n)
             buf4(i,j,k) = INT(n)
#if H5_HAVE_Fortran_INTEGER_SIZEOF_16!=0
             dset_data_i32(i,j,k) = HUGE(1_int_kind_32)-INT(n,int_kind_32)
#endif
             n = n + 1
          END DO
       END DO
    END DO

    !
    ! Initialize FORTRAN predefined datatypes.
    !
    CALL h5open_f(errcode)

    !
    ! Create a new file using default properties.
    !
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_INT 1D buffer
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    CALL h5ltmake_dataset_f(file_id, dsetname1, rank, dims, H5T_NATIVE_INTEGER, buf, errcode)

    !
    ! read dataset.
    !
    CALL h5ltread_dataset_f(file_id, dsetname1, H5T_NATIVE_INTEGER, bufr, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, DIM1*DIM2*DIM3
       IF ( buf(i) .NE. bufr(i) ) THEN
          PRINT *, 'read buffer differs from write buffer'
          PRINT *,  bufr(i), ' and ',   buf(i)
          STOP
       ENDIF
    END DO

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_INT 3D buffer
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims, H5T_NATIVE_INTEGER, buf2, errcode)

    !
    ! read dataset.
    !
    CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, buf2r, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          DO k = 1, dims(3)
             IF ( buf2(i,j,k) .NE. buf2r(i,j,k) ) THEN
                PRINT *, 'read buffer differs from write buffer'
                PRINT *,  buf2r(i,j,k), ' and ',   buf2(i,j,k)
                STOP
             ENDIF
          END DO
       END DO
    END DO

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_REAL
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    f_ptr = C_LOC(buf3(1,1,1))
    CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_REAL, f_ptr, errcode)
    !CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_REAL, buf3, errcode)

    !
    ! read dataset.
    !
    f_ptr = C_LOC(buf3r(1,1,1))
    CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, f_ptr, errcode)
    !CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, buf3r, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          DO k = 1, dims(3)
             CALL VERIFY("h5ltread_dataset_f",buf3(i,j,k), buf3r(i,j,k), errcode)
             IF (errcode .NE.0 ) THEN
                PRINT *, 'read buffer differs from write buffer'
                PRINT *,  buf3r(i,j,k), ' and ',   buf3(i,j,k)
                STOP
             ENDIF
          END DO
       END DO
    END DO

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_DOUBLE
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    f_ptr = C_LOC(buf4(1,1,1))
    CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims, H5T_NATIVE_DOUBLE, f_ptr, errcode)

    !
    ! read dataset.
    !
    f_ptr = C_LOC(buf4r(1,1,1))
    CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, f_ptr, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          DO k = 1, dims(3)
             CALL VERIFY("h5ltread_dataset_f", buf4(i,j,k), buf4r(i,j,k), errcode)
             IF (errcode .NE.0 ) THEN
                PRINT *, 'read buffer differs from write buffer'
                PRINT *,  buf4r(i,j,k), ' and ',   buf4(i,j,k)
                STOP
             ENDIF
          END DO
       END DO
    END DO

    CALL h5ltget_dataset_info_f(file_id,dsetname4,dimsr,type_class,type_size,errcode )

    !
    ! compare dimensions
    !
    DO i = 1, rank
       IF ( dimsr(i) .NE. dims(i) ) THEN
          PRINT *, 'dimensions differ '
          STOP
       ENDIF
    END DO

    !-------------------------------------------------------------------------
    ! CHECKING NON-NATIVE INTEGER TYPES
    !-------------------------------------------------------------------------

#if H5_HAVE_Fortran_INTEGER_SIZEOF_16!=0
    ! (A) CHECKING INTEGER*16
    !
    !    (i.a) write dataset using F2003 interface
    !
    type_id = H5kind_to_type(KIND(dset_data_i32(1,1,1)), H5_INTEGER_KIND)
    f_ptr = C_LOC(dset_data_i32(1,1,1))
    CALL h5ltmake_dataset_f(file_id, dsetname16a, rank, dims, type_id, f_ptr, errcode)
    !
    !    (i.b) read dataset using F2003 interface
    !
    f_ptr = C_LOC(data_out_i32(1,1,1))
    CALL h5ltread_dataset_f(file_id, dsetname16a, type_id, f_ptr, errcode)

    !
    !        compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          DO k = 1, dims(3)
             IF ( dset_data_i32(i,j,k) .NE. data_out_i32(i,j,k) ) THEN
                PRINT *, 'read buffer differs from write buffer'
                PRINT *,  dset_data_i32(i,j,k), ' and ', data_out_i32(i,j,k) 
                STOP
             ENDIF
          END DO
       END DO
    ENDDO

    !
    !    (ii.a) write dataset using F90 interface
    !
    type_id = H5kind_to_type(KIND(dset_data_i32(1,1,1)), H5_INTEGER_KIND)
    CALL h5ltmake_dataset_f(file_id, dsetname16b, rank, dims, type_id, dset_data_i32, errcode)
    !
    !    (ii.b) read dataset using F90 interface
    !
    CALL h5ltread_dataset_f(file_id, dsetname16b, type_id, data_out_i32, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          DO k = 1, dims(3)
             IF ( dset_data_i32(i,j,k) .NE. data_out_i32(i,j,k) ) THEN
                PRINT *, 'read buffer differs from write buffer'
                PRINT *,  dset_data_i32(i,j,k), ' and ', data_out_i32(i,j,k) 
                STOP
             ENDIF
          END DO
       END DO
    ENDDO

    !
    !     (iii.a) write dataset using F90 H5LTmake_dataset_int_f interface
    !
    CALL h5ltmake_dataset_int_f(file_id, dsetname16c, rank, dims, dset_data_i32, errcode)

    !
    !     (iii.b) read dataset using F90 H5LTmake_dataset_int_f interface
    !
    CALL h5ltread_dataset_int_f(file_id, dsetname16c, data_out_i32, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          DO k = 1, dims(3)
             IF ( dset_data_i32(i,j,k) .NE. data_out_i32(i,j,k) ) THEN
                PRINT *, 'read buffer differs from write buffer'
                PRINT *,  dset_data_i32(i,j,k), ' and ', data_out_i32(i,j,k) 
                STOP
             ENDIF
          END DO
       END DO
    ENDDO

#endif

    !
    ! Close the file.
    !
    CALL h5fclose_f(file_id, errcode)

    !
    ! Close FORTRAN predefined datatypes.
    !
    CALL h5close_f(errcode)

    CALL passed()
    !
    ! end function.
    !
  END SUBROUTINE test_dataset3D

  !-------------------------------------------------------------------------
  ! test_datasetND
  !-------------------------------------------------------------------------


  SUBROUTINE test_datasetND(rank)

    IMPLICIT NONE

    INTEGER            :: rank                                  ! Dataset rank

    INTEGER, PARAMETER :: DIM1 = 2                              ! columns
    INTEGER, PARAMETER :: DIM2 = 4                              ! rows
    INTEGER, PARAMETER :: DIM3 = 2                              ! layers
    INTEGER, PARAMETER :: DIM4 = 5                              ! columns
    INTEGER, PARAMETER :: DIM5 = 4                              ! rows
    INTEGER, PARAMETER :: DIM6 = 3                              ! layers
    INTEGER, PARAMETER :: DIM7 = 2                              ! layers
    CHARACTER(len=9), PARAMETER :: filename = "dsetf3.h5"       ! File name
    CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2"          ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname3 = "dset3"          ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname4 = "dset4"          ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname5 = "dset5"          ! Dataset name
    INTEGER(HID_T) :: file_id                                   ! File identifier
    INTEGER(HSIZE_T), DIMENSION(7) :: dims
    INTEGER(HSIZE_T), DIMENSION(7) :: dimsr                       ! Dataset dimensions
    INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:) :: ibuf_4            ! Data buffer
    INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:) :: ibufr_4           ! Data buffer
    INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:), TARGET :: ibuf_5  ! Data buffer
    INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:), TARGET :: ibufr_5 ! Data buffer
    INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: ibuf_6        ! Data buffer
    INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: ibufr_6       ! Data buffer
    INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:) :: ibuf_7      ! Data buffer
    INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:) :: ibufr_7     ! Data buffer
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:), TARGET :: rbuf_4                ! Data buffer
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:), TARGET :: rbufr_4               ! Data buffer
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:), TARGET :: rbuf_5      ! Data buffer
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:), TARGET :: rbufr_5     ! Data buffer
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:), TARGET :: rbuf_6            ! Data buffer
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:), TARGET :: rbufr_6           ! Data buffer
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:), TARGET :: rbuf_7          ! Data buffer
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:), TARGET :: rbufr_7         ! Data buffer
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:), TARGET :: dbuf_4    ! Data buffer
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:), TARGET :: dbufr_4            ! Data buffer
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:), TARGET :: dbuf_5           ! Data buffer
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:), TARGET :: dbufr_5          ! Data buffer
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:,:), TARGET :: dbuf_6         ! Data buffer
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:,:), TARGET :: dbufr_6        ! Data buffer
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:), TARGET :: dbuf_7       ! Data buffer
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:), TARGET :: dbufr_7      ! Data buffer
    CHARACTER(LEN=5), ALLOCATABLE, DIMENSION(:,:,:,:), TARGET :: cbuf_4            ! Data buffer
    CHARACTER(LEN=5), ALLOCATABLE, DIMENSION(:,:,:,:), TARGET :: cbufr_4           ! Data buffer
    CHARACTER(LEN=5), ALLOCATABLE, DIMENSION(:,:,:,:,:), TARGET :: cbuf_5          ! Data buffer
    CHARACTER(LEN=5), ALLOCATABLE, DIMENSION(:,:,:,:,:), TARGET :: cbufr_5         ! Data buffer
    CHARACTER(LEN=5), ALLOCATABLE, DIMENSION(:,:,:,:,:,:), TARGET :: cbuf_6        ! Data buffer
    CHARACTER(LEN=5), ALLOCATABLE, DIMENSION(:,:,:,:,:,:), TARGET :: cbufr_6       ! Data buffer
    CHARACTER(LEN=5), ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:), TARGET :: cbuf_7      ! Data buffer
    CHARACTER(LEN=5), ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:), TARGET :: cbufr_7     ! Data buffer
    INTEGER        :: errcode                            ! Error flag
    INTEGER(HSIZE_T) :: i, j, k, l, m, n, o, nn   ! general purpose integers
    INTEGER          :: type_class
    INTEGER(SIZE_T)  :: type_size
    CHARACTER(LEN=1) :: ichr1
    TYPE(C_PTR) :: f_ptr
    INTEGER(HID_T) :: type_id

    WRITE(ichr1,'(I1.1)') rank
    CALL test_begin(' Make/Read datasets ('//ichr1//'D)        ')
    !
    ! Initialize the data array.
    !
    IF(rank.EQ.4)THEN

       ALLOCATE(ibuf_4 (1:DIM1,1:DIM2,1:DIM3,1:DIM4))
       ALLOCATE(ibufr_4(1:DIM1,1:DIM2,1:DIM3,1:DIM4))
       ALLOCATE(rbuf_4 (1:DIM1,1:DIM2,1:DIM3,1:DIM4))
       ALLOCATE(rbufr_4(1:DIM1,1:DIM2,1:DIM3,1:DIM4))
       ALLOCATE(dbuf_4 (1:DIM1,1:DIM2,1:DIM3,1:DIM4))
       ALLOCATE(dbufr_4(1:DIM1,1:DIM2,1:DIM3,1:DIM4))
       ALLOCATE(cbuf_4 (1:DIM1,1:DIM2,1:DIM3,1:DIM4))
       ALLOCATE(cbufr_4(1:DIM1,1:DIM2,1:DIM3,1:DIM4))

       dims(1:7) = (/DIM1,DIM2,DIM3,DIM4,0,0,0/)

       nn = 1
       DO i = 1, DIM1
          DO j = 1, DIM2
             DO k = 1, DIM3
                DO l = 1, DIM4
                   ibuf_4(i,j,k,l) = INT(nn)
                   rbuf_4(i,j,k,l) = INT(nn)
                   dbuf_4(i,j,k,l) = INT(nn)
                   WRITE(cbuf_4(i,j,k,l),'(I5.5)') nn
                   nn = nn + 1
                END DO
             END DO
          END DO

       ENDDO

    ELSE IF(rank.EQ.5)THEN

       ALLOCATE(ibuf_5 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
       ALLOCATE(ibufr_5(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
       ALLOCATE(rbuf_5 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
       ALLOCATE(rbufr_5(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
       ALLOCATE(dbuf_5 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
       ALLOCATE(dbufr_5(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
       ALLOCATE(cbuf_5 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
       ALLOCATE(cbufr_5(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))

       dims(1:7) = (/DIM1,DIM2,DIM3,DIM4,DIM5,0,0/)

       nn = 1
       DO i = 1, DIM1
          DO j = 1, DIM2
             DO k = 1, DIM3
                DO l = 1, DIM4
                   DO m = 1, DIM5
                      ibuf_5(i,j,k,l,m) = INT(nn)
                      rbuf_5(i,j,k,l,m) = INT(nn)
                      dbuf_5(i,j,k,l,m) = INT(nn)
                      WRITE(cbuf_5(i,j,k,l,m),'(I5.5)') nn
                      nn = nn + 1
                   END DO
                END DO
             END DO
          ENDDO
       ENDDO

    ELSE IF(rank.EQ.6)THEN

       ALLOCATE(ibuf_6 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
       ALLOCATE(ibufr_6(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
       ALLOCATE(rbuf_6 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
       ALLOCATE(rbufr_6(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
       ALLOCATE(dbuf_6 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
       ALLOCATE(dbufr_6(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
       ALLOCATE(cbuf_6 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
       ALLOCATE(cbufr_6(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))

       dims(1:7) = (/DIM1,DIM2,DIM3,DIM4,DIM5,DIM6,0/)

       nn = 1
       DO i = 1, DIM1
          DO j = 1, DIM2
             DO k = 1, DIM3
                DO l = 1, DIM4
                   DO m = 1, DIM5
                      DO n = 1, DIM6
                         ibuf_6(i,j,k,l,m,n) = INT(nn)
                         rbuf_6(i,j,k,l,m,n) = INT(nn)
                         dbuf_6(i,j,k,l,m,n) = INT(nn)
                         WRITE(cbuf_6(i,j,k,l,m,n),'(I5.5)') nn
                         nn = nn + 1
                      END DO
                   END DO
                END DO
             ENDDO
          ENDDO
       ENDDO

    ELSE IF(rank.EQ.7)THEN

       ALLOCATE(ibuf_7 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
       ALLOCATE(ibufr_7(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
       ALLOCATE(rbuf_7 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
       ALLOCATE(rbufr_7(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
       ALLOCATE(dbuf_7 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
       ALLOCATE(dbufr_7(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
       ALLOCATE(cbuf_7 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
       ALLOCATE(cbufr_7(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))

       dims(1:7) = (/DIM1,DIM2,DIM3,DIM4,DIM5,DIM6,DIM7/)

       nn = 1
       DO i = 1, DIM1
          DO j = 1, DIM2
             DO k = 1, DIM3
                DO l = 1, DIM4
                   DO m = 1, DIM5
                      DO n = 1, DIM6
                         DO o = 1, DIM7
                            ibuf_7(i,j,k,l,m,n,o) = INT(nn)
                            rbuf_7(i,j,k,l,m,n,o) = INT(nn)
                            dbuf_7(i,j,k,l,m,n,o) = INT(nn)
                            WRITE(cbuf_7(i,j,k,l,m,n,o),'(I5.5)') nn
                            nn = nn + 1
                         END DO
                      END DO
                   END DO
                ENDDO
             ENDDO
          ENDDO
       ENDDO

    ENDIF

    !
    ! Initialize FORTRAN predefined datatypes.
    !
    CALL h5open_f(errcode)

    !
    ! Create a new file using default properties.
    !
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_INT ND buffer
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    IF(rank.EQ.4)THEN
       CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims(1:rank), H5T_NATIVE_INTEGER, ibuf_4, errcode)
    ELSE IF(rank.EQ.5)THEN
       f_ptr = C_LOC(ibuf_5(1,1,1,1,1))
       CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims(1:rank), H5T_NATIVE_INTEGER, f_ptr, errcode)
    ELSE IF(rank.EQ.6)THEN
       CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims(1:rank), H5T_NATIVE_INTEGER, ibuf_6, errcode)
    ELSE IF(rank.EQ.7)THEN
       CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims(1:rank), H5T_NATIVE_INTEGER, ibuf_7, errcode)
    ENDIF


    !
    ! read dataset.
    !
    IF(rank.EQ.4)THEN
       CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, ibufr_4, dims(1:rank), errcode)
    ELSE IF(rank.EQ.5)THEN
       f_ptr = C_LOC(ibufr_5(1,1,1,1,1))
       CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, f_ptr, errcode)
    ELSE IF(rank.EQ.6)THEN
       CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, ibufr_6, dims(1:rank), errcode)
    ELSE IF(rank.EQ.7)THEN
       CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, ibufr_7, dims(1:rank), errcode)
    ENDIF

    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          DO k = 1, dims(3)
             DO l = 1, dims(4)
                IF(rank.EQ.4)THEN
                   IF ( ibuf_4(i,j,k,l) .NE. ibufr_4(i,j,k,l) ) THEN
                      PRINT *, 'read buffer differs from write buffer'
                      PRINT *,  ibuf_4(i,j,k,l), ' and ', ibufr_4(i,j,k,l)
                      STOP
                   ENDIF
                ENDIF
                DO m = 1, dims(5)
                   IF(rank.EQ.5)THEN
                      IF ( ibuf_5(i,j,k,l,m) .NE. ibufr_5(i,j,k,l,m) ) THEN
                         PRINT *, 'read buffer differs from write buffer'
                         PRINT *,  ibuf_5(i,j,k,l,m), ' and ', ibufr_5(i,j,k,l,m)
                         STOP
                      ENDIF
                   ENDIF
                   DO n = 1, dims(6)
                      IF(rank.EQ.6)THEN
                         IF ( ibuf_6(i,j,k,l,m,n) .NE. ibufr_6(i,j,k,l,m,n) ) THEN
                            PRINT *, 'read buffer differs from write buffer'
                            PRINT *,  ibuf_6(i,j,k,l,m,n), ' and ', ibufr_6(i,j,k,l,m,n)
                            STOP
                         ENDIF
                      ENDIF
                      DO o = 1, dims(7)
                         IF(rank.EQ.7)THEN
                            IF ( ibuf_7(i,j,k,l,m,n,o) .NE. ibufr_7(i,j,k,l,m,n,o) ) THEN
                               PRINT *, 'read buffer differs from write buffer'
                               PRINT *,  ibuf_7(i,j,k,l,m,n,o), ' and ', ibufr_7(i,j,k,l,m,n,o)
                               STOP
                            ENDIF
                         ENDIF
                      ENDDO
                   ENDDO
                ENDDO
             ENDDO
          ENDDO
       ENDDO
    ENDDO
    !-------------------------------------------------------------------------
    ! H5T_NATIVE_REAL
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    IF(rank.EQ.4)THEN
       f_ptr = C_LOC(rbuf_4(1,1,1,1))
       CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims(1:rank), H5T_NATIVE_REAL, f_ptr, errcode)
       ! CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims(1:rank), H5T_NATIVE_REAL, rbuf_4, errcode)
    ELSE IF(rank.EQ.5)THEN
       f_ptr = C_LOC(rbuf_5(1,1,1,1,1))
       CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims(1:rank), H5T_NATIVE_REAL, f_ptr, errcode)
    ELSE IF(rank.EQ.6)THEN
       f_ptr = C_LOC(rbuf_6(1,1,1,1,1,1))
       CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims(1:rank), H5T_NATIVE_REAL, f_ptr, errcode)
       !CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims(1:rank), H5T_NATIVE_REAL, rbuf_6, errcode)
    ELSE IF(rank.EQ.7)THEN
       f_ptr = C_LOC(rbuf_7(1,1,1,1,1,1,1))
       CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims(1:rank), H5T_NATIVE_REAL, f_ptr, errcode)
       !CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims(1:rank), H5T_NATIVE_REAL, rbuf_7, errcode)
    ENDIF


    !
    ! read dataset.
    !
    IF(rank.EQ.4)THEN
       f_ptr = C_LOC(rbufr_4(1,1,1,1))
       CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, f_ptr, errcode)
    ELSE IF(rank.EQ.5)THEN
       f_ptr = C_LOC(rbufr_5(1,1,1,1,1))
       CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, f_ptr, errcode)
    ELSE IF(rank.EQ.6)THEN
       f_ptr = C_LOC(rbufr_6(1,1,1,1,1,1))
       CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, f_ptr, errcode)
    ELSE IF(rank.EQ.7)THEN
       f_ptr = C_LOC(rbufr_7(1,1,1,1,1,1,1))
       CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, f_ptr, errcode)
    ENDIF

    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          DO k = 1, dims(3)
             DO l = 1, dims(4)
                IF(rank.EQ.4)THEN
                   CALL VERIFY("h5ltread_dataset_f",rbuf_4(i,j,k,l), rbufr_4(i,j,k,l), errcode)
                   IF (errcode .NE.0 ) THEN
                      PRINT *, 'read buffer differs from write buffer'
                      PRINT *,  rbuf_4(i,j,k,l), ' and ', rbufr_4(i,j,k,l)
                      STOP
                   ENDIF
                ENDIF
                DO m = 1, dims(5)
                   IF(rank.EQ.5)THEN
                      CALL VERIFY("h5ltread_dataset_f",rbuf_5(i,j,k,l,m), rbufr_5(i,j,k,l,m), errcode)
                      IF (errcode .NE.0 ) THEN
                         PRINT *, 'read buffer differs from write buffer'
                         PRINT *,  rbuf_5(i,j,k,l,m), ' and ', rbufr_5(i,j,k,l,m)
                         STOP
                      ENDIF
                   ENDIF
                   DO n = 1, dims(6)
                      IF(rank.EQ.6)THEN
                         CALL VERIFY("h5ltread_dataset_f",rbuf_6(i,j,k,l,m,n), rbufr_6(i,j,k,l,m,n), errcode)
                         IF (errcode .NE.0 ) THEN
                            PRINT *, 'read buffer differs from write buffer'
                            PRINT *,  rbuf_6(i,j,k,l,m,n), ' and ', rbufr_6(i,j,k,l,m,n)
                            STOP
                         ENDIF
                      ENDIF
                      DO o = 1, dims(7)
                         IF(rank.EQ.7)THEN
                            CALL VERIFY("h5ltread_dataset_f",rbuf_7(i,j,k,l,m,n,o), rbufr_7(i,j,k,l,m,n,o), errcode)
                            IF (errcode .NE.0 ) THEN
                               PRINT *, 'read buffer differs from write buffer'
                               PRINT *,  rbuf_7(i,j,k,l,m,n,o), ' and ', rbufr_7(i,j,k,l,m,n,o)
                               STOP
                            ENDIF
                         ENDIF
                      ENDDO
                   ENDDO
                ENDDO
             ENDDO
          ENDDO
       ENDDO
    ENDDO

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_DOUBLE
    !-------------------------------------------------------------------------

    !
    ! write dataset.
    !
    IF(rank.EQ.4)THEN
       f_ptr = C_LOC(dbuf_4(1,1,1,1))
       CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims(1:rank), H5T_NATIVE_DOUBLE, f_ptr, errcode)
    ELSE IF(rank.EQ.5)THEN
       f_ptr = C_LOC(dbuf_5(1,1,1,1,1))
       CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims(1:rank), H5T_NATIVE_DOUBLE, f_ptr, errcode)
    ELSE IF(rank.EQ.6)THEN
       f_ptr = C_LOC(dbuf_6(1,1,1,1,1,1))
       CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims(1:rank), H5T_NATIVE_DOUBLE, f_ptr, errcode)
    ELSE IF(rank.EQ.7)THEN
       f_ptr = C_LOC(dbuf_7(1,1,1,1,1,1,1))
       CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims(1:rank), H5T_NATIVE_DOUBLE, f_ptr, errcode)
    ENDIF

    !
    ! read dataset.
    !
    IF(rank.EQ.4)THEN
       f_ptr = C_LOC(dbufr_4(1,1,1,1))
       CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, f_ptr, errcode)
    ELSE IF(rank.EQ.5)THEN
       f_ptr = C_LOC(dbufr_5(1,1,1,1,1))
       CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, f_ptr, errcode)
    ELSE IF(rank.EQ.6)THEN
       f_ptr = C_LOC(dbufr_6(1,1,1,1,1,1))
       CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, f_ptr, errcode)
    ELSE IF(rank.EQ.7)THEN
       f_ptr = C_LOC(dbufr_7(1,1,1,1,1,1,1))
       CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, f_ptr, errcode)
    ENDIF

    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          DO k = 1, dims(3)
             DO l = 1, dims(4)
                IF(rank.EQ.4)THEN
                   CALL VERIFY("h5ltread_dataset_f",dbuf_4(i,j,k,l), dbufr_4(i,j,k,l), errcode)
                   IF (errcode .NE.0 ) THEN
                      PRINT *, 'read buffer differs from write buffer'
                      PRINT *,  dbuf_4(i,j,k,l), ' and ', dbufr_4(i,j,k,l)
                      STOP
                   ENDIF
                ENDIF
                DO m = 1, dims(5)
                   IF(rank.EQ.5)THEN
                      CALL VERIFY("h5ltread_dataset_f",dbuf_5(i,j,k,l,m), dbufr_5(i,j,k,l,m), errcode)
                      IF (errcode .NE.0 ) THEN
                         PRINT *, 'read buffer differs from write buffer'
                         PRINT *,  dbuf_5(i,j,k,l,m), ' and ', dbufr_5(i,j,k,l,m)
                         STOP
                      ENDIF
                   ENDIF
                   DO n = 1, dims(6)
                      IF(rank.EQ.6)THEN
                         CALL VERIFY("h5ltread_dataset_f",dbuf_6(i,j,k,l,m,n), dbufr_6(i,j,k,l,m,n), errcode)
                         IF (errcode .NE.0 ) THEN
                            PRINT *, 'read buffer differs from write buffer'
                            PRINT *,  dbuf_6(i,j,k,l,m,n), ' and ', dbufr_6(i,j,k,l,m,n)
                            STOP
                         ENDIF
                      ENDIF
                      DO o = 1, dims(7)
                         IF(rank.EQ.7)THEN
                            CALL VERIFY("h5ltread_dataset_f",dbuf_7(i,j,k,l,m,n,o), dbufr_7(i,j,k,l,m,n,o), errcode)
                            IF (errcode .NE.0 ) THEN
                               PRINT *, 'read buffer differs from write buffer'
                               PRINT *,  dbuf_7(i,j,k,l,m,n,o), ' and ', dbufr_7(i,j,k,l,m,n,o)
                               STOP
                            ENDIF
                         ENDIF
                      ENDDO
                   ENDDO
                ENDDO
             ENDDO
          ENDDO
       ENDDO
    ENDDO

    !-------------------------------------------------------------------------
    ! H5T_NATIVE_CHARACTER ND buffer
    !-------------------------------------------------------------------------

    CALL H5Tcopy_f(H5T_FORTRAN_S1, type_id, errcode)
    CALL H5Tset_size_f(type_id, 5_SIZE_T, errcode)
    !
    ! write dataset.
    !
    IF(rank.EQ.4)THEN
       f_ptr = C_LOC(cbuf_4(1,1,1,1)(1:1))
       CALL h5ltmake_dataset_f(file_id, dsetname5, rank, dims(1:rank), type_id, f_ptr, errcode)
    ELSE IF(rank.EQ.5)THEN
       f_ptr = C_LOC(cbuf_5(1,1,1,1,1)(1:1))
       CALL h5ltmake_dataset_f(file_id, dsetname5, rank, dims(1:rank), type_id, f_ptr, errcode)
    ELSE IF(rank.EQ.6)THEN
       f_ptr = C_LOC(cbuf_6(1,1,1,1,1,1)(1:1))
       CALL h5ltmake_dataset_f(file_id, dsetname5, rank, dims(1:rank), type_id, f_ptr, errcode)
    ELSE IF(rank.EQ.7)THEN
       f_ptr = C_LOC(cbuf_7(1,1,1,1,1,1,1)(1:1))
       CALL h5ltmake_dataset_f(file_id, dsetname5, rank, dims(1:rank), type_id, f_ptr, errcode)
    ENDIF

    !
    ! read dataset.
    !
    IF(rank.EQ.4)THEN
       f_ptr = C_LOC(cbufr_4(1,1,1,1)(1:1))
       CALL h5ltread_dataset_f(file_id, dsetname5, type_id, f_ptr, errcode)
    ELSE IF(rank.EQ.5)THEN
       f_ptr = C_LOC(cbufr_5(1,1,1,1,1)(1:1))
       CALL h5ltread_dataset_f(file_id, dsetname5, type_id, f_ptr, errcode)
    ELSE IF(rank.EQ.6)THEN
       f_ptr = C_LOC(cbufr_6(1,1,1,1,1,1)(1:1))
       CALL h5ltread_dataset_f(file_id, dsetname5, type_id, f_ptr, errcode)
    ELSE IF(rank.EQ.7)THEN
       f_ptr = C_LOC(cbufr_7(1,1,1,1,1,1,1)(1:1))
       CALL h5ltread_dataset_f(file_id, dsetname5, type_id, f_ptr, errcode)
    ENDIF


    !
    ! compare read and write buffers.
    !
    DO i = 1, dims(1)
       DO j = 1, dims(2)
          DO k = 1, dims(3)
             DO l = 1, dims(4)
                IF(rank.EQ.4)THEN
                   IF ( cbuf_4(i,j,k,l) .NE. cbufr_4(i,j,k,l) ) THEN
                      PRINT *, 'read buffer differs from write buffer (character)'
                      PRINT *,  cbuf_4(i,j,k,l), ' and ', cbufr_4(i,j,k,l)
                      STOP
                   ENDIF
                ENDIF
                DO m = 1, dims(5)
                   IF(rank.EQ.5)THEN
                      IF ( cbuf_5(i,j,k,l,m) .NE. cbufr_5(i,j,k,l,m) ) THEN
                         PRINT *, 'read buffer differs from write buffer (character)'
                         PRINT *,  cbuf_5(i,j,k,l,m), ' and ', cbufr_5(i,j,k,l,m)
                         STOP
                      ENDIF
                   ENDIF
                   DO n = 1, dims(6)
                      IF(rank.EQ.6)THEN
                         IF ( cbuf_6(i,j,k,l,m,n) .NE. cbufr_6(i,j,k,l,m,n) ) THEN
                            PRINT *, 'read buffer differs from write buffer (character)'
                            PRINT *,  cbuf_6(i,j,k,l,m,n), ' and ', cbufr_6(i,j,k,l,m,n)
                            STOP
                         ENDIF
                      ENDIF
                      DO o = 1, dims(7)
                         IF(rank.EQ.7)THEN
                            IF ( cbuf_7(i,j,k,l,m,n,o) .NE. cbufr_7(i,j,k,l,m,n,o) ) THEN
                               PRINT *, 'read buffer differs from write buffer (character)'
                               PRINT *,  cbuf_7(i,j,k,l,m,n,o), ' and ', cbufr_7(i,j,k,l,m,n,o)
                               STOP
                            ENDIF
                         ENDIF
                      ENDDO
                   ENDDO
                ENDDO
             ENDDO
          ENDDO
       ENDDO
    ENDDO

    CALL h5ltget_dataset_info_f(file_id,dsetname4,dimsr,type_class,type_size,errcode )

    CALL h5tclose_f(type_id,errcode)

    !
    ! compare dimensions
    !
    DO i = 1, rank
       IF ( dimsr(i) .NE. dims(i) ) THEN
          PRINT *, 'dimensions differ '
          STOP
       ENDIF
    END DO

    !
    ! Close the file.
    !
    CALL h5fclose_f(file_id, errcode)

    !
    ! Close FORTRAN predefined datatypes.
    !
    CALL h5close_f(errcode)

    ! DEALLOCATE RESOURCES

    IF(rank.EQ.4)THEN
       DEALLOCATE(ibuf_4, ibufr_4, rbuf_4, rbufr_4, dbuf_4, dbufr_4, cbuf_4, cbufr_4)
    ELSE IF(rank.EQ.5)THEN
       DEALLOCATE(ibuf_5, ibufr_5, rbuf_5, rbufr_5, dbuf_5, dbufr_5, cbuf_5, cbufr_5)
    ELSE IF(rank.EQ.6)THEN
       DEALLOCATE(ibuf_6, ibufr_6, rbuf_6, rbufr_6, dbuf_6, dbufr_6, cbuf_6, cbufr_6)
    ELSE IF(rank.EQ.7)THEN
       DEALLOCATE(ibuf_7, ibufr_7, rbuf_7, rbufr_7, dbuf_7, dbufr_7, cbuf_7, cbufr_7)
    ENDIF

    CALL passed()
    !
    ! end function.
    !
  END SUBROUTINE test_datasetND


  !-------------------------------------------------------------------------
  ! test_datasets
  !-------------------------------------------------------------------------

  SUBROUTINE test_datasets()

    IMPLICIT NONE

    CHARACTER(len=9), PARAMETER :: filename = "dsetf4.h5"! File name
    INTEGER(HID_T) :: file_id                            ! File identifier
    INTEGER        :: errcode                            ! Error flag
    INTEGER, PARAMETER :: DIM1 = 10                      ! Dimension of array
    INTEGER, PARAMETER :: LEN0 = 3
    INTEGER, PARAMETER :: LEN1 = 12
    CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2"   ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname3 = "dset3"   ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname4 = "dset4"   ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname5 = "dset5"   ! Dataset name
    CHARACTER(LEN=5), PARAMETER :: dsetname6 = "dset6"   ! Dataset name
    INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/DIM1/)    ! Dataset dimensions
    INTEGER(HSIZE_T), DIMENSION(1) :: dimsr              ! Dataset dimensions
    INTEGER        :: rank = 1                           ! Dataset rank
    INTEGER        :: rankr                              ! Dataset rank
    CHARACTER(LEN=8), PARAMETER :: buf1 = "mystring"     ! Data buffer
    CHARACTER(LEN=8)            :: buf1r                 ! Data buffer
    INTEGER, DIMENSION(DIM1)          :: buf2            ! Data buffer
    INTEGER, DIMENSION(DIM1)          :: bufr2           ! Data buffer
    REAL, DIMENSION(DIM1), TARGET             :: buf3    ! Data buffer
    REAL, DIMENSION(DIM1) , TARGET            :: bufr3   ! Data buffer
    DOUBLE PRECISION, DIMENSION(DIM1), TARGET :: buf4    ! Data buffer
    DOUBLE PRECISION, DIMENSION(DIM1), TARGET :: bufr4   ! Data buffer
    INTEGER          :: i, n                             ! general purpose integer
    INTEGER(SIZE_T)  :: i_sz, j_sz                       ! general purpose integer
    INTEGER          :: has                              ! general purpose integer
    INTEGER          :: type_class
    INTEGER(SIZE_T)  :: type_size
    LOGICAL :: path_valid  ! status of the path
    CHARACTER(LEN=6) :: chr_exact
    CHARACTER(LEN=8) :: chr_lg
    TYPE(C_PTR) :: f_ptr

    ! vl data
    TYPE vl
       INTEGER, DIMENSION(:), POINTER :: DATA
    END TYPE vl
    TYPE(vl), DIMENSION(:), ALLOCATABLE, TARGET :: ptr
    TYPE(hvl_t), DIMENSION(1:2), TARGET :: wdata ! Array of vlen structures
    TYPE(hvl_t), DIMENSION(1:2), TARGET :: rdata ! Pointer to vlen structures
    INTEGER(hsize_t), DIMENSION(1:1) :: dims_vl = (/2/)
    INTEGER, DIMENSION(:), POINTER :: ptr_r
    INTEGER(HID_T) :: type_id 

    !
    ! Initialize FORTRAN predefined datatypes.
    !
    CALL h5open_f(errcode)

    !
    ! Create a new file using default properties.
    !
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

    !
    ! Initialize the data array.
    !
    n = 1
    DO i = 1, DIM1
       buf2(i) = n
       buf3(i) = n
       buf4(i) = n
       n = n + 1
    END DO

    !
    ! Initialize variable-length data.  wdata(1) is a countdown of
    ! length LEN0, wdata(2) is a Fibonacci sequence of length LEN1.
    !
    wdata(1)%len = LEN0
    wdata(2)%len = LEN1

    ALLOCATE( ptr(1:2) )
    ALLOCATE( ptr(1)%data(1:wdata(1)%len) )
    ALLOCATE( ptr(2)%data(1:wdata(2)%len) )

    DO i_sz=1, wdata(1)%len
       ptr(1)%data(i_sz) = INT(wdata(1)%len) - INT(i_sz) + 1 ! 3 2 1
    ENDDO
    wdata(1)%p = C_LOC(ptr(1)%data(1))

    ptr(2)%data(1:2) = 1
    DO i_sz = 3, wdata(2)%len
       ptr(2)%data(i_sz) = ptr(2)%data(i_sz-1_size_t) + ptr(2)%data(i_sz-2_size_t) ! (1 1 2 3 5 8 etc.)
    ENDDO
    wdata(2)%p = C_LOC(ptr(2)%data(1))

    !-------------------------------------------------------------------------
    ! int
    !-------------------------------------------------------------------------

    CALL test_begin(' Make/Read datasets (integer)   ')

    !
    ! write dataset.
    !
    CALL h5ltmake_dataset_int_f(file_id, dsetname2, rank, dims, buf2, errcode)

    !
    ! read dataset.
    !
    CALL h5ltread_dataset_int_f(file_id, dsetname2, bufr2, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, DIM1
       IF ( buf2(i) .NE. bufr2(i) ) THEN
          PRINT *, 'read buffer differs from write buffer'
          PRINT *,  bufr2(i), ' and ',   buf2(i)
          STOP
       ENDIF
    END DO

    CALL passed()


    !-------------------------------------------------------------------------
    ! real
    !-------------------------------------------------------------------------

    CALL test_begin(' Make/Read datasets (float)     ')


    !
    ! write dataset.
    !
    f_ptr = C_LOC(buf3(1))
    CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_REAL, f_ptr, errcode)

    !
    ! read dataset.
    !
    f_ptr = C_LOC(bufr3(1))
    CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, f_ptr, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, DIM1
       CALL VERIFY("h5ltread_dataset_f", buf3(i), bufr3(i), errcode)
       IF (errcode .NE.0 ) THEN
          PRINT *, 'read buffer differs from write buffer'
          PRINT *,  bufr3(i), ' and ',   buf3(i)
          STOP
       ENDIF
    END DO

    CALL passed()

    !-------------------------------------------------------------------------
    ! double
    !-------------------------------------------------------------------------

    CALL test_begin(' Make/Read datasets (double)    ')


    !
    ! write dataset.
    !
    !f_ptr = C_LOC(buf4(1))
    !CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims, H5T_NATIVE_DOUBLE, f_ptr, errcode)
    CALL h5ltmake_dataset_double_f(file_id, dsetname4, rank, dims, buf4, errcode)

    !
    ! read dataset.
    !
    !f_ptr = C_LOC(buf4(1))
    !CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, f_ptr, errcode)
    CALL h5ltread_dataset_double_f(file_id, dsetname4, bufr4, dims, errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, DIM1
       CALL VERIFY("h5ltread_dataset_double_f", buf4(i), bufr4(i), errcode)
       IF (errcode .NE.0 ) THEN
          PRINT *, 'read buffer differs from write buffer'
          PRINT *,  bufr4(i), ' and ',   buf4(i)
          STOP
       ENDIF
    END DO

    CALL passed()


    !-------------------------------------------------------------------------
    ! string
    !-------------------------------------------------------------------------

    CALL test_begin(' Make/Read datasets (string)    ')


    !
    ! write dataset.
    !
    CALL h5ltmake_dataset_string_f(file_id, dsetname5, buf1, errcode)

    !
    ! read dataset.
    !
    CALL h5ltread_dataset_string_f(file_id, dsetname5, buf1r, errcode)

    !
    ! compare read and write buffers.
    !
    IF ( buf1 .NE. buf1r ) THEN
       PRINT *, 'read buffer differs from write buffer'
       PRINT *,  buf1, ' and ',   buf1r
       STOP
    ENDIF

    CALL passed()


    !-------------------------------------------------------------------------
    ! variable-length dataset
    !-------------------------------------------------------------------------
    CALL test_begin(' Make/Read datasets (vl)        ')
    !
    ! Create variable-length datatype.
    !
    CALL H5Tvlen_create_f(H5T_NATIVE_INTEGER, type_id, errcode)

    f_ptr = C_LOC(wdata(1))
    CALL h5ltmake_dataset_f(file_id, dsetname6, 1, dims_vl, type_id, f_ptr, errcode)

    ! Read the variable-length datatype
    f_ptr = C_LOC(rdata(1))
    CALL h5ltread_dataset_f(file_id, dsetname6, type_id, f_ptr, errcode)

    DO i = 1, INT(dims_vl(1))
       CALL c_f_pointer(rdata(i)%p, ptr_r, [rdata(i)%len] )
       DO j_sz = 1, rdata(i)%len
          CALL VERIFY("h5ltread_dataset_f", ptr_r(j_sz), ptr(i)%data(j_sz), errcode)
          IF (errcode .NE.0 ) THEN
             PRINT *, 'Writing/Reading variable-length dataset failed'
             STOP
          ENDIF
       ENDDO
    ENDDO

    CALL H5Tclose_f(type_id, errcode)
    DEALLOCATE(ptr)

    CALL passed()

    CALL test_begin(' Test h5ltpath_valid_f          ')
    !
    ! test function h5ltpath_valid_f
    !
    chr_exact = "/"//dsetname2 ! test character buffer the exact size needed
    CALL h5ltpath_valid_f(file_id, chr_exact, .TRUE., path_valid, errcode)
    IF(errcode.LT.0.OR..NOT.path_valid)THEN
       PRINT *, 'error in h5ltpath_valid_f'
       STOP
    ENDIF
    chr_lg = "/"//dsetname2 ! test character buffer larger then needed
    CALL h5ltpath_valid_f(file_id, chr_lg, .TRUE., path_valid, errcode)
    IF(errcode.LT.0.OR..NOT.path_valid)THEN
       PRINT *, 'error in h5ltpath_valid_f'
       STOP
    ENDIF

    CALL h5ltpath_valid_f(file_id, chr_lg, .FALSE., path_valid, errcode)
    IF(errcode.LT.0.OR..NOT.path_valid)THEN
       PRINT *, 'error in h5ltpath_valid_f'
       STOP
    ENDIF

    ! Should fail, dataset does not exist
    CALL h5ltpath_valid_f(file_id, "/"//dsetname2//"junk", .TRUE., path_valid, errcode)
    IF(errcode.LT.0.OR.path_valid)THEN
       PRINT *, 'error in h5ltpath_valid_f'
       STOP
    ENDIF

    CALL h5ltpath_valid_f(file_id, "/"//dsetname2//"junk", .FALSE., path_valid, errcode)
    IF(errcode.LT.0.OR.path_valid)THEN
       PRINT *, 'error in h5ltpath_valid_f'
       STOP
    ENDIF

    ! Create a dangling soft link
    CALL h5lcreate_soft_f("/G2", file_id, "/G3", errcode)

    ! Should pass, does not check for dangled link
    CALL h5ltpath_valid_f(file_id, "/G3", .FALSE., path_valid, errcode)
    IF(.NOT.path_valid)THEN
       PRINT *, 'error in h5ltpath_valid_f'
       STOP
    ENDIF

    ! Should fail, dangled link
    CALL h5ltpath_valid_f(file_id, "/G2", .TRUE., path_valid, errcode)
    IF(path_valid)THEN
       PRINT *, 'error in h5ltpath_valid_f'
       STOP
    ENDIF

    CALL passed()

    CALL test_begin(' Get dataset dimensions/info    ')

    !-------------------------------------------------------------------------
    ! h5ltget_dataset_ndims_f
    !-------------------------------------------------------------------------

    CALL h5ltget_dataset_ndims_f(file_id, dsetname4, rankr, errcode)
    IF ( rankr .NE. rank ) THEN
       PRINT *, 'h5ltget_dataset_ndims_f return error'
       STOP
    ENDIF

    !-------------------------------------------------------------------------
    ! test h5ltfind_dataset_f function
    !-------------------------------------------------------------------------


    has = h5ltfind_dataset_f(file_id,dsetname4)
    IF ( has .NE. 1 ) THEN
       PRINT *, 'h5ltfind_dataset_f return error'
       STOP
    ENDIF

    !-------------------------------------------------------------------------
    ! test h5ltget_dataset_info_f function
    !-------------------------------------------------------------------------

    CALL h5ltget_dataset_info_f(file_id,dsetname4,dimsr,type_class,type_size,errcode )

    !
    ! compare dimensions
    !
    DO i = 1, rank
       IF ( dimsr(i) .NE. dims(i) ) THEN
          PRINT *, 'dimensions differ '
          STOP
       ENDIF
    END DO

    IF ( type_class .NE. 1 ) THEN ! H5T_FLOAT
       PRINT *, 'wrong type class '
       STOP
    ENDIF

    CALL passed()

    !
    ! Close the file.
    !
    CALL h5fclose_f(file_id, errcode)
    !
    ! Close FORTRAN predefined datatypes.
    !
    CALL h5close_f(errcode)

    !
    ! end function.
    !
  END SUBROUTINE test_datasets


  !-------------------------------------------------------------------------
  ! test_attributes
  !-------------------------------------------------------------------------

  SUBROUTINE test_attributes()

    IMPLICIT NONE

    CHARACTER(len=9), PARAMETER :: filename = "dsetf5.h5"! File name
!!$  CHARACTER(len=9), PARAMETER :: filename1 ="tattr.h5" ! C written attribute file
    INTEGER(HID_T) :: file_id                            ! File identifier
    !  INTEGER(HID_T) :: file_id1
    INTEGER, PARAMETER :: DIM1 = 10                     ! Dimension of array
    CHARACTER(LEN=5), PARAMETER :: attrname2 = "attr2"   ! Attribute name
    CHARACTER(LEN=5), PARAMETER :: attrname3 = "attr3"   ! Attribute name
    CHARACTER(LEN=5), PARAMETER :: attrname4 = "attr4"   ! Attribute name
    CHARACTER(LEN=5), PARAMETER :: attrname5 = "attr5"   ! Attribute name
    CHARACTER(LEN=8), PARAMETER :: buf1 = "mystring"     ! Data buffer
!!$  CHARACTER(LEN=16), PARAMETER :: buf_c = "string attribute"
    CHARACTER(LEN=8)                  :: bufr1           ! Data buffer
    CHARACTER(LEN=10)                 :: bufr1_lg        ! Data buffer
    !  CHARACTER(LEN=16)                 :: bufr_c          ! Data buffer
    !  CHARACTER(LEN=18)                 :: bufr_c_lg       ! Data buffer
    INTEGER, DIMENSION(DIM1)          :: buf2            ! Data buffer
    INTEGER, DIMENSION(DIM1)          :: bufr2           ! Data buffer
    REAL, DIMENSION(DIM1), target     :: buf3            ! Data buffer
    REAL, DIMENSION(DIM1), target     :: bufr3           ! Data buffer
    DOUBLE PRECISION, DIMENSION(DIM1), TARGET :: buf4            ! Data buffer
    DOUBLE PRECISION, DIMENSION(DIM1), TARGET :: bufr4           ! Data buffer
    INTEGER        :: errcode                            ! Error flag
    INTEGER        :: i, n                               ! general purpose integer
    INTEGER(SIZE_T) size                                 ! size of attribute array
    INTEGER        :: rankr                              ! rank
    INTEGER(HSIZE_T), DIMENSION(1) :: dimsr              ! attribute dimensions
    INTEGER          :: type_class
    INTEGER(SIZE_T)  :: type_size
    INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/DIM1/)    ! Dataset dimensions
    INTEGER        :: rank = 1                           ! Dataset rank
    CHARACTER(LEN=5), PARAMETER :: dsetname1 = "dset1"   ! Dataset name
    INTEGER, DIMENSION(DIM1)    :: buf                   ! Data buffer
    INTEGER(SIZE_T)  :: SizeOf_buf_type
    TYPE(C_PTR) :: f_ptr

    !
    ! Initialize FORTRAN predefined datatypes.
    !
    CALL h5open_f(errcode)
    !
    ! Create a new file using default properties.
    !
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)
    !
    ! make a dataset.
    !
    CALL h5ltmake_dataset_int_f(file_id, dsetname1, rank, dims, buf, errcode)

    !
    ! Initialize the data array.
    !
    size = DIM1
    n = 1
    DO i = 1, DIM1
       buf2(i) = n
       buf3(i) = n
       buf4(i) = n
       n = n + 1
    END DO


    !-------------------------------------------------------------------------
    ! int
    !-------------------------------------------------------------------------

    CALL test_begin(' Set/Get attributes int         ')


    !
    ! write attribute.
    !
    CALL h5ltset_attribute_int_f(file_id,dsetname1,attrname2,buf2,size,errcode)

    !
    ! read attribute.
    !
    CALL h5ltget_attribute_int_f(file_id,dsetname1,attrname2,bufr2,errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, DIM1
       IF ( buf2(i) .NE. bufr2(i) ) THEN
          PRINT *, 'read buffer differs from write buffer'
          PRINT *,  bufr2(i), ' and ',   buf2(i)
          STOP
       ENDIF
    END DO

    CALL passed()

    !-------------------------------------------------------------------------
    ! float
    !-------------------------------------------------------------------------

    CALL test_begin(' Set/Get attributes float       ')


    !
    ! write attribute.
    !
#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    SizeOf_buf_type = STORAGE_SIZE(buf3(1), c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)
#else
    SizeOf_buf_type = SIZEOF(buf3(1))
#endif
    f_ptr = C_LOC(buf3(1))
    CALL h5ltset_attribute_f(file_id,dsetname1,attrname3,f_ptr,"REAL", SizeOf_buf_type, size,errcode)
    !CALL h5ltset_attribute_float_f(file_id,dsetname1,attrname3,buf3,size,errcode)
    !
    ! read attribute.
    !
#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    SizeOf_buf_type = STORAGE_SIZE(bufr3(1), c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)
#else
    SizeOf_buf_type = SIZEOF(bufr3(1))
#endif

    f_ptr = C_LOC(bufr3(1))
    CALL h5ltget_attribute_f(file_id,dsetname1,attrname3,f_ptr,"REAL",SizeOf_buf_type,errcode)
    !CALL h5ltget_attribute_float_f(file_id,dsetname1,attrname3,bufr3,errcode)

    !
    ! compare read and write buffers.
    !
    DO i = 1, DIM1
       CALL VERIFY("h5ltget_attribute_f",buf3(i), bufr3(i), errcode)
       IF (errcode .NE.0 ) THEN
          PRINT *, 'read buffer differs from write buffer'
          PRINT *,  bufr3(i), ' and ',   buf3(i)
          STOP
       ENDIF
    END DO

    CALL passed()

    !-------------------------------------------------------------------------
    ! double
    !-------------------------------------------------------------------------

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    SizeOf_buf_type = STORAGE_SIZE(buf4(1), c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)
#else
    SizeOf_buf_type = SIZEOF(buf4(1))
#endif

    IF(SizeOf_buf_type.LT.16)THEN ! MSB can't handle 16 byte reals

       CALL test_begin(' Set/Get attributes double      ')

       !
       ! write attribute.
       !
       f_ptr = C_LOC(buf4(1))
       CALL h5ltset_attribute_f(file_id,dsetname1,attrname4,f_ptr,"real", SizeOf_buf_type, size, errcode)

       !  CALL h5ltset_attribute_double_f(file_id,dsetname1,attrname4,buf4, size, errcode)

       !
       ! read attribute.
       !

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
       SizeOf_buf_type = STORAGE_SIZE(bufr4(1), c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)
#else
       SizeOf_buf_type = SIZEOF(bufr4(1))
#endif

       f_ptr = C_LOC(bufr4(1))
       CALL h5ltget_attribute_f(file_id,dsetname1,attrname4,f_ptr,"REAL",SizeOf_buf_type,errcode)

       !
       ! compare read and write buffers.
       !
       DO i = 1, DIM1
          CALL VERIFY("h5ltget_attribute_f",buf4(i), bufr4(i), errcode)
          IF (errcode .NE.0 ) THEN
             PRINT *, 'read buffer differs from write buffer'
             PRINT *,  bufr4(i), ' and ',   buf4(i)
             STOP
          ENDIF
       END DO

       CALL passed()

    ENDIF

    !-------------------------------------------------------------------------
    ! string
    !-------------------------------------------------------------------------

    CALL test_begin(' Set/Get attributes string      ')

    !
    ! write attribute.
    !
    CALL h5ltset_attribute_string_f(file_id,dsetname1,attrname5,buf1,errcode)

    !
    ! read attribute into a fortran character buf that is the same size as buf1.
    !
    CALL h5ltget_attribute_string_f(file_id,dsetname1,attrname5,bufr1,errcode)

    !
    ! compare read and write buffers.
    !
    IF ( buf1 .NE. bufr1 ) THEN
       PRINT *, 'read buffer differs from write buffer'
       PRINT *,  buf1, ' and ',   bufr1
       STOP
    ENDIF

    !
    ! read attribute into a fortran character buf that is larger then buf1.
    !
    CALL h5ltget_attribute_string_f(file_id,dsetname1,attrname5,bufr1_lg,errcode)

    !
    ! compare read and write buffers, make sure C NULL character was removed.
    !
    IF ( buf1(1:8) .NE. bufr1_lg(1:8) .AND. bufr1_lg(9:10) .NE. '  ' ) THEN
       PRINT *, 'larger read buffer differs from write buffer'
       PRINT *,  buf1, ' and ',   bufr1_lg
       STOP
    ENDIF

    ! 
    ! ** Test reading a string that was created with a C program **
    !

!!$  CALL h5fopen_f(filename1, H5F_ACC_RDONLY_F, file_id1, errcode)
!!$
!!$  CALL h5ltget_attribute_string_f(file_id1, "/", "attr5", bufr_c, errcode)
!!$  !
!!$  ! compare read and write buffers.
!!$  !
!!$  IF ( bufr_c .NE. buf_c ) THEN
!!$     PRINT *, 'read buffer differs from write buffer'
!!$     PRINT *,  bufr1, ' and ',  buf_c 
!!$     STOP
!!$  ENDIF
!!$  !
!!$  ! read attribute into a fortran character buf that is larger then buf_c.
!!$  !
!!$  CALL h5ltget_attribute_string_f(file_id1, "/", "attr5", bufr_c_lg, errcode)
!!$
!!$  !
!!$  ! compare read and write buffers, make sure C NULL character was removed.
!!$  !
!!$  IF ( buf_c(1:16) .NE. bufr_c_lg(1:16) .AND. bufr_c_lg(17:18) .NE. '  ' ) THEN
!!$     PRINT *, 'larger read buffer differs from write buffer'
!!$     PRINT *,  buf_c, ' and ',  bufr_c_lg 
!!$     STOP
!!$  ENDIF  

!!$  CALL h5fclose_f(file_id1,  errcode)

    CALL passed()

    !-------------------------------------------------------------------------
    ! get attribute rank
    !-------------------------------------------------------------------------

    CALL test_begin(' Get attribute rank/info        ')


    CALL h5ltget_attribute_ndims_f(file_id,dsetname1,attrname2,rankr,errcode)

    IF ( rankr .NE. 1 ) THEN
       PRINT *, 'h5ltget_attribute_ndims_f return error'
       STOP
    ENDIF


    CALL h5ltget_attribute_info_f(file_id,dsetname1,attrname2,dimsr,type_class,type_size,errcode)

    !
    ! compare dimensions
    !
    DO i = 1, rank
       IF ( dimsr(i) .NE. dims(i) ) THEN
          PRINT *, 'dimensions differ '
          STOP
       ENDIF
    END DO


    !
    ! Close the file.
    !
    CALL h5fclose_f(file_id, errcode)
    !
    ! Close FORTRAN predefined datatypes.
    !
    CALL h5close_f(errcode)

    CALL passed()
    !
    ! end function.
    !
  END SUBROUTINE test_attributes

END MODULE TSTLITE_TESTS

PROGRAM lite_test

  USE TSTLITE_TESTS ! module for testing lite routines
  IMPLICIT NONE

  CALL test_dataset1D()
  CALL test_dataset2D()
  CALL test_dataset3D()
  CALL test_datasetND(4)
  CALL test_datasetND(5)
  CALL test_datasetND(6)
  CALL test_datasetND(7)
  CALL test_datasets()
  CALL test_attributes()

END PROGRAM lite_test


