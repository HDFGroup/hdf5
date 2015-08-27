! 
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! Copyright by the Board of Trustees of the University of Illinois.         *
! All rights reserved.                                                      *
!                                                                           *
! This file is part of HDF5.  The full HDF5 copyright notice, including     *
! terms governing use, modification, and redistribution, is contained in    *
! the files COPYING and Copyright.html.  COPYING can be found at the root   *
! of the source code distribution tree; Copyright.html can be found at the  *
! root level of an installed copy of the electronic HDF5 document set and   *
! is linked from the top-level documents page.  It can also be found at     *
! http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
! access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! This example shows how to create a nested compound data type,
! write an array which has the compound data type to the file,
! and read back fields' subsets.

PROGRAM main
  USE HDF5
  USE ISO_C_BINDING
  IMPLICIT NONE

! KIND parameters
  INTEGER, PARAMETER :: int_k1 = SELECTED_INT_KIND(2)  ! This should map to INTEGER*1 on most modern processors
  INTEGER, PARAMETER :: int_k4 = SELECTED_INT_KIND(4)  ! This should map to INTEGER*2 on most modern processors
  INTEGER, PARAMETER :: int_k8 = SELECTED_INT_KIND(9)  ! This should map to INTEGER*4 on most modern processors
  INTEGER, PARAMETER :: int_k16 = SELECTED_INT_KIND(18) ! This should map to INTEGER*8 on most modern processors

  INTEGER, PARAMETER :: r_k4 = SELECTED_REAL_KIND(6,37) ! This should map to REAL*4 on most modern processors
  INTEGER, PARAMETER :: r_k8 = SELECTED_REAL_KIND(15,307) ! This should map to REAL*8 on most modern processors

! FILES

  CHARACTER(LEN=*), PARAMETER :: H5FILE_NAME = "SDScompound.h5"
  CHARACTER(LEN=*), PARAMETER :: DATASETNAME = "ArrayOfStructures"

  INTEGER, PARAMETER :: LENGTH = 10
  INTEGER, PARAMETER :: RANK = 1

!----------------------------------------------------------------
! Nested derived-type and dataset

  TYPE s4_t
     INTEGER(int_k8), DIMENSION(1:4) :: x
  ENDTYPE s4_t

  TYPE s1_t
     CHARACTER(LEN=1), DIMENSION(1:7) :: chr
     INTEGER(KIND=int_k1) :: a
     REAL(KIND=r_k4) :: b
     TYPE(s4_t) :: d
     REAL(KIND=r_k8) :: c
  END TYPE s1_t

  TYPE(s1_t), TARGET, DIMENSION(1:length) :: s1
  INTEGER(hid_t) :: s1_tid     ! File datatype identifier

!----------------------------------------------------------------
! Second derived-type (subset of s1_t) and dataset
  TYPE s2_t
     CHARACTER(LEN=1), DIMENSION(1:7) :: chr
     REAL(KIND=r_k8) :: c
     INTEGER(KIND=int_k1) :: a
     TYPE(s4_t) :: d
  END TYPE s2_t

  TYPE(s2_t), TARGET :: s2(LENGTH)
  integer(hid_t) :: s2_tid    ! Memory datatype handle
  INTEGER(hid_t) :: tid3a
!----------------------------------------------------------------
! Third "derived-type" (will be used to read float field of s1)
  INTEGER(hid_t) :: s3_tid   ! Memory datatype handle
  REAL(KIND=r_k4), TARGET :: s3(LENGTH)

  INTEGER :: i
  INTEGER(hid_t) :: file, dataset, space
  !type(H5F_fileid_type) :: file
  !type(H5D_dsetid_type) :: dataset
  !type(H5S_spaceid_type) :: space
  INTEGER(hsize_t) :: DIM(1) = (/LENGTH/)   ! Dataspace dimensions
  INTEGER(SIZE_T) :: type_size  ! Size of the datatype
  INTEGER(SIZE_T) :: offset, sizeof_compound
  INTEGER :: hdferr
  TYPE(C_PTR) :: f_ptr
  
  INTEGER(SIZE_T) :: type_sizei  ! Size of the integer datatype 
  INTEGER(SIZE_T) :: type_sizer  ! Size of the real datatype 
  INTEGER(SIZE_T) :: type_sized  ! Size of the double datatype 
  INTEGER(hid_t) :: tid3      ! /* Nested Array Datatype ID	*/
  INTEGER(HSIZE_T), DIMENSION(1) :: tdims1=(/7/)
  INTEGER(HSIZE_T), DIMENSION(1) :: tdims1a=(/4/)
  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Initialize the data
  !
  DO i = 0, LENGTH-1
     s1(i+1)%chr(1:7)(1:1) = (/'a','b','c',' ',' ',' ','d'/)
     s1(i+1)%a = i
     s1(i+1)%b = i*i
     s1(i+1)%c = 1./REAL(i+1)
     s1(i+1)%d%x(1:4) = (/i,i*10,i*100,i*1000/)
  END DO
  !
  ! Create the data space.
  !
  !
  CALL H5Screate_simple_f(RANK, dim, space, hdferr)
  !
  ! Create the file.
  !
  CALL H5Fcreate_f(H5FILE_NAME, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create the memory data type.
  !
  CALL H5Tcreate_f(H5T_COMPOUND_F, H5OFFSETOF(C_LOC(s1(1)), C_LOC(s1(2))), s1_tid, hdferr)

  CALL h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, tdims1, tid3, hdferr)

  CALL H5Tinsert_f(s1_tid, "chr_name", H5OFFSETOF(C_LOC(s1(1)),C_LOC(s1(1)%chr)),tid3, hdferr)
  CALL H5Tinsert_f(s1_tid, "a_name", H5OFFSETOF(C_LOC(s1(1)),C_LOC(s1(1)%a)), h5kind_to_type(int_k1,H5_INTEGER_KIND), hdferr)
  CALL H5Tinsert_f(s1_tid, "c_name", H5OFFSETOF(C_LOC(s1(1)),C_LOC(s1(1)%c)), h5kind_to_type(r_k8,H5_REAL_KIND), hdferr)
  CALL H5Tinsert_f(s1_tid, "b_name", H5OFFSETOF(C_LOC(s1(1)),C_LOC(s1(1)%b)), h5kind_to_type(r_k4,H5_REAL_KIND), hdferr)

  ! Create an array of integer datatype
  CALL h5tarray_create_f(h5kind_to_type(int_k8,H5_INTEGER_KIND), 1, tdims1a, tid3a, hdferr)
  CALL H5Tinsert_f(s1_tid, "d_name", H5OFFSETOF(C_LOC(s1(1)),C_LOC(s1(1)%d%x)), tid3a, hdferr)

  !
  ! Create the dataset.
  !
  CALL H5Dcreate_f(file, DATASETNAME, s1_tid, space, dataset, hdferr)

  !
  ! Write data to the dataset
  !
  
  f_ptr = C_LOC(s1(1))
  CALL H5Dwrite_f(dataset, s1_tid, f_ptr, hdferr)

  !
  ! Release resources
  !
  CALL H5Tclose_f(s1_tid, hdferr)
  CALL H5Sclose_f(space, hdferr)
  CALL H5Dclose_f(dataset, hdferr)
  CALL H5Fclose_f(file, hdferr)

  !
  ! Open the file and the dataset.
  !

  CALL H5Fopen_f(H5FILE_NAME, H5F_ACC_RDONLY_F, file, hdferr)
  
  CALL H5Dopen_f(file, DATASETNAME, dataset,hdferr)
  !
  ! Create a data type for s2
  !
  CALL H5Tcreate_f(H5T_COMPOUND_F,  H5OFFSETOF(C_LOC(s2(1)), C_LOC(s2(2))), s2_tid, hdferr)

  CALL H5Tinsert_f(s2_tid, "chr_name", H5OFFSETOF(C_LOC(s2(1)),C_LOC(s2(1)%chr)), tid3, hdferr)
  CALL H5Tinsert_f(s2_tid, "c_name", H5OFFSETOF(C_LOC(s2(1)),C_LOC(s2(1)%c)), h5kind_to_type(r_k8,H5_REAL_KIND), hdferr)
  CALL H5Tinsert_f(s2_tid, "a_name", H5OFFSETOF(C_LOC(s2(1)),C_LOC(s2(1)%a)), h5kind_to_type(int_k1,H5_INTEGER_KIND), hdferr)
  CALL H5Tinsert_f(s2_tid, "d_name", H5OFFSETOF(C_LOC(s2(1)),C_LOC(s2(1)%d%x)), tid3a, hdferr)

  !
  ! Read two fields c and a from s1 dataset. Fields in the file
  ! are found by their names "c_name" and "a_name".

  f_ptr = C_LOC(s2(1))
  CALL H5Dread_f(dataset, s2_tid, f_ptr, hdferr)

  !
  ! Display the fields
  !
  DO i = 1, length
     WRITE(*,'(/,A,/,999(A,1X))') "Field chr :", s2(i)%chr(1:7)(1:1)
  ENDDO
  WRITE(*,'(/,A,/,999(F8.4,1X))') "Field c :", s2(:)%c
  WRITE(*,'(/,A,/,999(I0,1X))') "Field a :", s2(:)%a
  DO i = 1, length
     WRITE(*,'(/,A,/,999(I0,1X))') "Field d%x :", s2(i)%d%x(:)
  ENDDO
  !
  ! Create a data type for s3.
  !
  CALL H5Tcreate_f(H5T_COMPOUND_F, H5OFFSETOF(C_LOC(s3(1)),C_LOC(s3(2))),s3_tid, hdferr)

  CALL H5Tinsert_f(s3_tid, "b_name", 0_size_t, h5kind_to_type(r_k4,H5_REAL_KIND), hdferr)
  !
  ! Read field b from s1 dataset. Field in the file is found by its name.
  !
  s3(:)=-1
  f_ptr = C_LOC(s3(1))
  CALL H5Dread_f(dataset, s3_tid, f_ptr, hdferr)
  !
  ! Display the field
  !
  WRITE(*,'(/,A,/,999(F8.4,1X))') "Field b :",s3(:)
  !
  ! Release resources
  !
  CALL H5Tclose_f(s2_tid, hdferr)
  CALL H5Tclose_f(s3_tid, hdferr)
  CALL H5Dclose_f(dataset, hdferr)
  CALL H5Fclose_f(file, hdferr)

END PROGRAM main
