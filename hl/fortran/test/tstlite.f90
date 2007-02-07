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

program lite_test

call test_dataset1D()
call test_dataset2D()
call test_dataset3D()
call test_datasets()
call test_attributes()

end program lite_test 


!-------------------------------------------------------------------------
! test_dataset1D 
!-------------------------------------------------------------------------

subroutine test_dataset1D()

use H5LT ! module of H5LT 
use HDF5 ! module of HDF5 library

implicit none

integer, parameter :: DIM1 = 4;                      ! Dimension of array
character(len=9), parameter :: filename = "dsetf1.h5"! File name
character(LEN=5), parameter :: dsetname1 = "dset1"   ! Dataset name
character(LEN=5), parameter :: dsetname2 = "dset2"   ! Dataset name
character(LEN=5), parameter :: dsetname3 = "dset3"   ! Dataset name
integer(HID_T) :: file_id                            ! File identifier 
integer(HSIZE_T), dimension(1) :: dims = (/DIM1/)    ! Dataset dimensions
integer        :: rank = 1                           ! Dataset rank
integer, dimension(DIM1) :: buf1                     ! Data buffer
integer, dimension(DIM1) :: bufr1                    ! Data buffer
real, dimension(DIM1)    :: buf2                     ! Data buffer
real, dimension(DIM1)    :: bufr2                    ! Data buffer
double precision, dimension(DIM1) :: buf3            ! Data buffer
double precision, dimension(DIM1) :: bufr3           ! Data buffer
integer        :: errcode                            ! Error flag
integer        :: i                                  ! general purpose integer


call test_begin(' Make/Read datasets (1D)        ')


!
! Initialize the data array.
!
do i = 1, DIM1
  buf1(i) = i;
  buf2(i) = i;
  buf3(i) = i;
end do

!
! Initialize FORTRAN predefined datatypes.
!
call h5open_f(errcode) 

!
! Create a new file using default properties.
!
call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

!-------------------------------------------------------------------------
! H5T_NATIVE_INTEGER 
!-------------------------------------------------------------------------

!
! write dataset. 
!
call h5ltmake_dataset_f(file_id, dsetname1, rank, dims, H5T_NATIVE_INTEGER, buf1, errcode)

!
! read dataset. 
!
call h5ltread_dataset_f(file_id, dsetname1, H5T_NATIVE_INTEGER, bufr1, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, DIM1
 if ( buf1(i) .ne. bufr1(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr1(i), ' and ',   buf1(i)
   stop
  endif
end do

!-------------------------------------------------------------------------
! H5T_NATIVE_REAL 
!-------------------------------------------------------------------------

!
! write dataset. 
!
call h5ltmake_dataset_f(file_id, dsetname2, rank, dims, H5T_NATIVE_REAL, buf2, errcode)

!
! read dataset. 
!
call h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_REAL, bufr2, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, DIM1
 if ( buf2(i) .ne. bufr2(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr2(i), ' and ',   buf2(i)
   stop
  endif
end do

!-------------------------------------------------------------------------
! H5T_NATIVE_DOUBLE 
!-------------------------------------------------------------------------

!
! write dataset. 
!
call h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_DOUBLE, buf3, errcode)

!
! read dataset. 
!
call h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_DOUBLE, bufr3, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, DIM1
 if ( buf3(i) .ne. bufr3(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr3(i), ' and ',   buf3(i)
   stop
  endif
end do

!
! Close the file.
!
call h5fclose_f(file_id, errcode)

!
! Close FORTRAN predefined datatypes.
!
call h5close_f(errcode)

call passed()
!
! end function.
!
end subroutine test_dataset1D
 
!-------------------------------------------------------------------------
! test_dataset2D 
!-------------------------------------------------------------------------

subroutine test_dataset2D()

use H5LT ! module of H5LT 
use HDF5 ! module of HDF5 library

implicit none


integer, parameter :: DIM1 = 4;                             ! columns
integer, parameter :: DIM2 = 6;                             ! rows
character(len=9), parameter :: filename = "dsetf2.h5"! File name
character(LEN=5), parameter :: dsetname1 = "dset1"   ! Dataset name
character(LEN=5), parameter :: dsetname2 = "dset2"   ! Dataset name
character(LEN=5), parameter :: dsetname3 = "dset3"   ! Dataset name
character(LEN=5), parameter :: dsetname4 = "dset4"   ! Dataset name
integer(HID_T) :: file_id                            ! File identifier 
integer(HSIZE_T), dimension(2) :: dims = (/4,6/)     ! Dataset dimensions
integer        :: rank = 2                           ! Dataset rank
integer, dimension(DIM1*DIM2) :: buf                 ! Data buffer
integer, dimension(DIM1*DIM2) :: bufr                ! Data buffer
integer, dimension(DIM1,DIM2) :: buf2                ! Data buffer
integer, dimension(DIM1,DIM2) :: buf2r               ! Data buffer
real, dimension(DIM1,DIM2)    :: buf3                ! Data buffer
real, dimension(DIM1,DIM2)    :: buf3r               ! Data buffer
double precision, dimension(DIM1,DIM2) :: buf4       ! Data buffer
double precision, dimension(DIM1,DIM2) :: buf4r      ! Data buffer
integer        :: errcode                            ! Error flag
integer        :: i, j, n                            ! general purpose integers

call test_begin(' Make/Read datasets (2D)        ')


!
! Initialize the data arrays.
!
n=1
do i = 1, DIM1*DIM2
   buf(i) = n;
   n = n + 1
end do

do i = 1, dims(1)
 do j = 1, dims(2)
  buf2(i,j) = (i-1)*dims(2) + j;
  buf3(i,j) = (i-1)*dims(2) + j;
  buf4(i,j) = (i-1)*dims(2) + j;
 end do
end do
     

!
! Initialize FORTRAN predefined datatypes.
!
call h5open_f(errcode) 

!
! Create a new file using default properties.
!
call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

!-------------------------------------------------------------------------
! H5T_NATIVE_INT 1D buffer
!-------------------------------------------------------------------------

!
! write dataset. 
!
call h5ltmake_dataset_f(file_id, dsetname1, rank, dims, H5T_NATIVE_INTEGER, buf, errcode)

!
! read dataset. 
!
call h5ltread_dataset_f(file_id, dsetname1, H5T_NATIVE_INTEGER, bufr, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, DIM1*DIM2
  if ( buf(i) .ne. bufr(i) ) then
    print *, 'read buffer differs from write buffer'
    print *,  bufr(i), ' and ',   buf(i)
    stop
  endif
end do

!-------------------------------------------------------------------------
! H5T_NATIVE_INT 2D buffer
!-------------------------------------------------------------------------

!
! write dataset. 
!
call h5ltmake_dataset_f(file_id, dsetname2, rank, dims, H5T_NATIVE_INTEGER, buf2, errcode)

!
! read dataset. 
!
call h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, buf2r, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, dims(1)
 do j = 1, dims(2)
  if ( buf2(i,j) .ne. buf2r(i,j) ) then
    print *, 'read buffer differs from write buffer'
    print *,  buf2r(i,j), ' and ',   buf2(i,j)
    stop
  endif
 end do
end do

!-------------------------------------------------------------------------
! H5T_NATIVE_REAL
!-------------------------------------------------------------------------

!
! write dataset. 
!
call h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_REAL, buf3, errcode)

!
! read dataset. 
!
call h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, buf3r, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, dims(1)
 do j = 1, dims(2)
  if ( buf3(i,j) .ne. buf3r(i,j) ) then
    print *, 'read buffer differs from write buffer'
    print *,  buf3r(i,j), ' and ',   buf3(i,j)
    stop
  endif
 end do
end do

!-------------------------------------------------------------------------
! H5T_NATIVE_DOUBLE
!-------------------------------------------------------------------------

!
! write dataset. 
!
call h5ltmake_dataset_f(file_id, dsetname4, rank, dims, H5T_NATIVE_DOUBLE, buf4, errcode)

!
! read dataset. 
!
call h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, buf4r, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, dims(1)
 do j = 1, dims(2)
  if ( buf4(i,j) .ne. buf4r(i,j) ) then
    print *, 'read buffer differs from write buffer'
    print *,  buf4r(i,j), ' and ',   buf4(i,j)
    stop
  endif
 end do
end do

!
! Close the file.
!
call h5fclose_f(file_id, errcode)

!
! Close FORTRAN predefined datatypes.
!
call h5close_f(errcode)

call passed()
!
! end function.
!
end subroutine test_dataset2D 


!-------------------------------------------------------------------------
! test_dataset3D
!-------------------------------------------------------------------------


subroutine test_dataset3D()

use H5LT ! module of H5LT 
use HDF5 ! module of HDF5 library

implicit none

integer, parameter :: DIM1 = 6;                             ! columns
integer, parameter :: DIM2 = 4;                             ! rows
integer, parameter :: DIM3 = 2;                             ! layers
character(len=9), parameter :: filename = "dsetf3.h5"       ! File name
character(LEN=5), parameter :: dsetname1 = "dset1"          ! Dataset name
character(LEN=5), parameter :: dsetname2 = "dset2"          ! Dataset name
character(LEN=5), parameter :: dsetname3 = "dset3"          ! Dataset name
character(LEN=5), parameter :: dsetname4 = "dset4"          ! Dataset name
integer(HID_T) :: file_id                                   ! File identifier 
integer(HSIZE_T), dimension(3) :: dims = (/DIM1,DIM2,DIM3/) ! Dataset dimensions
integer, dimension(DIM1*DIM2*DIM3) :: buf                   ! Data buffer
integer, dimension(DIM1*DIM2*DIM3) :: bufr                  ! Data buffer
integer, dimension(DIM1,DIM2,DIM3) :: buf2                  ! Data buffer
integer, dimension(DIM1,DIM2,DIM3) :: buf2r                 ! Data buffer
real, dimension(DIM1,DIM2,DIM3)    :: buf3                  ! Data buffer
real, dimension(DIM1,DIM2,DIM3)    :: buf3r                 ! Data buffer
double precision, dimension(DIM1,DIM2,DIM3) :: buf4         ! Data buffer
double precision, dimension(DIM1,DIM2,DIM3) :: buf4r        ! Data buffer
integer        :: rank = 3                                  ! Dataset rank
integer        :: errcode                                   ! Error flag
integer        :: i, j, k, n                                ! general purpose integers

call test_begin(' Make/Read datasets (3D)        ')


!
! Initialize the data array.
!
n=1
do i = 1, DIM1*DIM2*DIM3
   buf(i) = n;
   n = n + 1
end do

n = 1
do i = 1, dims(1)
 do j = 1, dims(2)
 do k = 1, dims(3)
  buf2(i,j,k) = n;
  buf3(i,j,k) = n;
  buf4(i,j,k) = n;
  n = n + 1
 end do
 end do
end do

!
! Initialize FORTRAN predefined datatypes.
!
call h5open_f(errcode) 

!
! Create a new file using default properties.
!
call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

!-------------------------------------------------------------------------
! H5T_NATIVE_INT 1D buffer
!-------------------------------------------------------------------------

!
! write dataset. 
!
call h5ltmake_dataset_f(file_id, dsetname1, rank, dims, H5T_NATIVE_INTEGER, buf, errcode)

!
! read dataset. 
!
call h5ltread_dataset_f(file_id, dsetname1, H5T_NATIVE_INTEGER, bufr, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, DIM1*DIM2*DIM3
  if ( buf(i) .ne. bufr(i) ) then
    print *, 'read buffer differs from write buffer'
    print *,  bufr(i), ' and ',   buf(i)
    stop
  endif
end do

!-------------------------------------------------------------------------
! H5T_NATIVE_INT 3D buffer
!-------------------------------------------------------------------------

!
! write dataset. 
!
call h5ltmake_dataset_f(file_id, dsetname2, rank, dims, H5T_NATIVE_INTEGER, buf2, errcode)

!
! read dataset. 
!
call h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, buf2r, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, dims(1)
 do j = 1, dims(2)
 do k = 1, dims(3)
  if ( buf2(i,j,k) .ne. buf2r(i,j,k) ) then
    print *, 'read buffer differs from write buffer'
    print *,  buf2r(i,j,k), ' and ',   buf2(i,j,k)
    stop
  endif
 end do
 end do
end do

!-------------------------------------------------------------------------
! H5T_NATIVE_REAL
!-------------------------------------------------------------------------

!
! write dataset. 
!
call h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_REAL, buf3, errcode)

!
! read dataset. 
!
call h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, buf3r, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, dims(1)
 do j = 1, dims(2)
 do k = 1, dims(3)
  if ( buf3(i,j,k) .ne. buf3r(i,j,k) ) then
    print *, 'read buffer differs from write buffer'
    print *,  buf3r(i,j,k), ' and ',   buf3(i,j,k)
    stop
  endif
 end do
 end do
end do

!-------------------------------------------------------------------------
! H5T_NATIVE_DOUBLE
!-------------------------------------------------------------------------

!
! write dataset. 
!
call h5ltmake_dataset_f(file_id, dsetname4, rank, dims, H5T_NATIVE_DOUBLE, buf4, errcode)

!
! read dataset. 
!
call h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, buf4r, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, dims(1)
 do j = 1, dims(2)
 do k = 1, dims(3)
  if ( buf4(i,j,k) .ne. buf4r(i,j,k) ) then
    print *, 'read buffer differs from write buffer'
    print *,  buf4r(i,j,k), ' and ',   buf4(i,j,k)
    stop
  endif
 end do
 end do
end do

!
! Close the file.
!
call h5fclose_f(file_id, errcode)

!
! Close FORTRAN predefined datatypes.
!
call h5close_f(errcode)

call passed()
!
! end function.
!
end subroutine test_dataset3D



!-------------------------------------------------------------------------
! test_datasets
!-------------------------------------------------------------------------

subroutine test_datasets()

use H5LT ! module of H5LT 
use HDF5 ! module of HDF5 library

implicit none

character(len=9), parameter :: filename = "dsetf4.h5"! File name
integer(HID_T) :: file_id                            ! File identifier 
integer        :: errcode                            ! Error flag
integer, parameter :: DIM1 = 10;                     ! Dimension of array
character(LEN=5), parameter :: dsetname1 = "dset1"   ! Dataset name
character(LEN=5), parameter :: dsetname2 = "dset2"   ! Dataset name
character(LEN=5), parameter :: dsetname3 = "dset3"   ! Dataset name
character(LEN=5), parameter :: dsetname4 = "dset4"   ! Dataset name
integer(HSIZE_T), dimension(1) :: dims = (/DIM1/)    ! Dataset dimensions
integer(HSIZE_T), dimension(1) :: dimsr              ! Dataset dimensions
integer        :: rank = 1                           ! Dataset rank
integer        :: rankr                              ! Dataset rank
character(LEN=8), parameter :: buf1 = "mystring"     ! Data buffer
character(LEN=8)            :: buf1r                 ! Data buffer
integer, dimension(DIM1)          :: buf2            ! Data buffer
integer, dimension(DIM1)          :: bufr2           ! Data buffer
real, dimension(DIM1)             :: buf3            ! Data buffer
real, dimension(DIM1)             :: bufr3           ! Data buffer
double precision, dimension(DIM1) :: buf4            ! Data buffer
double precision, dimension(DIM1) :: bufr4           ! Data buffer
integer          :: i, n                             ! general purpose integer
integer          :: has                              ! general purpose integer
integer          :: type_class
integer(SIZE_T)  :: type_size

!
! Initialize FORTRAN predefined datatypes.
!
call h5open_f(errcode) 

!
! Create a new file using default properties.
!
call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

!
! Initialize the data array.
!
n = 1
do i = 1, DIM1
  buf2(i) = n;
  buf3(i) = n;
  buf4(i) = n;
  n = n + 1;
end do

!-------------------------------------------------------------------------
! int 
!-------------------------------------------------------------------------

call test_begin(' Make/Read datasets (integer)   ')

!
! write dataset. 
!
call h5ltmake_dataset_int_f(file_id, dsetname2, rank, dims, buf2, errcode)

!
! read dataset. 
!
call h5ltread_dataset_int_f(file_id, dsetname2, bufr2, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, DIM1
 if ( buf2(i) .ne. bufr2(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr2(i), ' and ',   buf2(i)
   stop
  endif
end do

call passed()

!-------------------------------------------------------------------------
! real 
!-------------------------------------------------------------------------

call test_begin(' Make/Read datasets (float)     ')


!
! write dataset. 
!
call h5ltmake_dataset_float_f(file_id, dsetname3, rank, dims, buf3, errcode)

!
! read dataset. 
!
call h5ltread_dataset_float_f(file_id, dsetname3, bufr3, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, DIM1
 if ( buf3(i) .ne. bufr3(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr3(i), ' and ',   buf3(i)
   stop
  endif
end do

call passed()

!-------------------------------------------------------------------------
! double 
!-------------------------------------------------------------------------

call test_begin(' Make/Read datasets (double)    ')


!
! write dataset. 
!
call h5ltmake_dataset_double_f(file_id, dsetname4, rank, dims, buf4, errcode)

!
! read dataset. 
!
call h5ltread_dataset_double_f(file_id, dsetname4, bufr4, dims, errcode)

!
! compare read and write buffers.
!
do i = 1, DIM1
 if ( buf4(i) .ne. bufr4(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr4(i), ' and ',   buf4(i)
   stop
  endif
end do

call passed()

call test_begin(' Get dataset dimensions         ')

!-------------------------------------------------------------------------
! h5ltget_dataset_ndims_f 
!-------------------------------------------------------------------------

call h5ltget_dataset_ndims_f(file_id, dsetname4, rankr, errcode)
if ( rankr .ne. rank ) then
 print *, 'h5ltget_dataset_ndims_f return error'
 stop
endif

call passed()

!-------------------------------------------------------------------------
! test find dataset function
!-------------------------------------------------------------------------

call test_begin(' Find dataset                   ')


!has = h5ltfind_dataset_f(file_id,dsetname4)
!if ( has .ne. 1 ) then
! print *, 'h5ltfind_dataset_f return error'
! stop
!endif

!
! Close the file.
!
call h5fclose_f(file_id, errcode)
!
! Close FORTRAN predefined datatypes.
!
call h5close_f(errcode)

call passed()
!
! end function.
!
end subroutine test_datasets
 
 

!-------------------------------------------------------------------------
! test_attributes 
!-------------------------------------------------------------------------

subroutine test_attributes()

use H5LT ! module of H5LT 
use HDF5 ! module of HDF5 library

implicit none

character(len=9), parameter :: filename = "dsetf4.h5"! File name
integer(HID_T) :: file_id                            ! File identifier 
integer, parameter :: DIM1 = 10;                     ! Dimension of array
character(LEN=5), parameter :: attrname1 = "attr1"   ! Attribute name
character(LEN=5), parameter :: attrname2 = "attr2"   ! Attribute name
character(LEN=5), parameter :: attrname3 = "attr3"   ! Attribute name
character(LEN=5), parameter :: attrname4 = "attr4"   ! Attribute name
character(LEN=8), parameter :: buf1 = "mystring"     ! Data buffer
character(LEN=8)                  :: bufr1           ! Data buffer
integer, dimension(DIM1)          :: buf2            ! Data buffer
integer, dimension(DIM1)          :: bufr2           ! Data buffer
real, dimension(DIM1)             :: buf3            ! Data buffer
real, dimension(DIM1)             :: bufr3           ! Data buffer
double precision, dimension(DIM1) :: buf4            ! Data buffer
double precision, dimension(DIM1) :: bufr4           ! Data buffer
integer        :: errcode                            ! Error flag
integer        :: i, n                               ! general purpose integer
integer(SIZE_T) size                                 ! size of attribute array
integer        :: rankr                              ! rank
integer(HSIZE_T), dimension(1) :: dimsr              ! attribute dimensions
integer          :: type_class
integer(SIZE_T)  :: type_size
integer(HSIZE_T), dimension(1) :: dims = (/DIM1/)    ! Dataset dimensions
integer        :: rank = 1                           ! Dataset rank
character(LEN=5), parameter :: dsetname1 = "dset1"   ! Dataset name
integer, dimension(DIM1)    :: buf                   ! Data buffer

!
! Initialize FORTRAN predefined datatypes.
!
call h5open_f(errcode) 
!
! Create a new file using default properties.
!
call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)
!
! make a dataset. 
!
call h5ltmake_dataset_int_f(file_id, dsetname1, rank, dims, buf, errcode)

!
! Initialize the data array.
!
size = DIM1
n = 1
do i = 1, DIM1
  buf2(i) = n;
  buf3(i) = n;
  buf4(i) = n;
  n = n + 1;
end do


!-------------------------------------------------------------------------
! int
!-------------------------------------------------------------------------

call test_begin(' Set/Get attributes int         ')


!
! write attribute. 
!
call h5ltset_attribute_int_f(file_id,dsetname1,attrname2,buf2,size,errcode)

!
! read attribute. 
!
call h5ltget_attribute_int_f(file_id,dsetname1,attrname2,bufr2,errcode)

!
! compare read and write buffers.
!
do i = 1, DIM1
 if ( buf2(i) .ne. bufr2(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr2(i), ' and ',   buf2(i)
   stop
  endif
end do

call passed()

!-------------------------------------------------------------------------
! float
!-------------------------------------------------------------------------

call test_begin(' Set/Get attributes float       ')


!
! write attribute. 
!
call h5ltset_attribute_float_f(file_id,dsetname1,attrname3,buf3,size,errcode)

!
! read attribute. 
!
call h5ltget_attribute_float_f(file_id,dsetname1,attrname3,bufr3,errcode)

!
! compare read and write buffers.
!
do i = 1, DIM1
 if ( buf3(i) .ne. bufr3(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr3(i), ' and ',   buf3(i)
   stop
  endif
end do


call passed()

!-------------------------------------------------------------------------
! double
!-------------------------------------------------------------------------

call test_begin(' Set/Get attributes double      ')


!
! write attribute. 
!
call h5ltset_attribute_double_f(file_id,dsetname1,attrname4,buf4,size,errcode)

!
! read attribute. 
!
call h5ltget_attribute_double_f(file_id,dsetname1,attrname4,bufr4,errcode)

!
! compare read and write buffers.
!
do i = 1, DIM1
 if ( buf4(i) .ne. bufr4(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr4(i), ' and ',   buf4(i)
   stop
  endif
end do

call passed()


!-------------------------------------------------------------------------
! get attribute rank
!-------------------------------------------------------------------------

call test_begin(' Get attribute rank             ')


call h5ltget_attribute_ndims_f(file_id,dsetname1,attrname2,rankr,errcode) 

if ( rankr .ne. 1 ) then
 print *, 'h5ltget_attribute_ndims_f return error'
 stop
endif


!
! Close the file.
!
call h5fclose_f(file_id, errcode)
!
! Close FORTRAN predefined datatypes.
!
call h5close_f(errcode)

call passed()
!
! end function.
!
end subroutine test_attributes






!-------------------------------------------------------------------------
! test_begin 
!-------------------------------------------------------------------------

subroutine test_begin(string)
character(LEN=*), intent(IN) :: string
write(*, fmt = '(14a)', advance = 'no') string
write(*, fmt = '(40x,a)', advance = 'no') ' ' 
end subroutine test_begin

!-------------------------------------------------------------------------
! passed 
!-------------------------------------------------------------------------

subroutine passed()
write(*, fmt = '(6a)')  'PASSED'
end subroutine passed
