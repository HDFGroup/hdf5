! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source errcode distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
!   access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!
!
! This file contains the FORTRAN90 tests for H5LT 
!

program image_test

call make_image1()

end program image_test 


!-------------------------------------------------------------------------
! make_image1 
!-------------------------------------------------------------------------

subroutine make_image1()

use H5IM ! module of H5IM
use HDF5 ! module of HDF5 library

implicit none

character(len=8), parameter :: filename = "f1img.h5" ! File name
character(LEN=4), parameter :: dsetname1 = "img1"    ! Dataset name
character(LEN=4), parameter :: dsetname2 = "img2"    ! Dataset name
character(LEN=15), parameter :: il ="INTERLACE_PIXEL"! Dataset name
integer(HID_T) :: file_id                            ! File identifier 
integer(HSIZE_T), parameter :: width  = 30           ! width
integer(HSIZE_T), parameter :: height = 10           ! width
integer*1, dimension(width*height) :: buf1           ! Data buffer
integer*1, dimension(width*height) :: bufr1          ! Data buffer
integer*1, dimension(width*height*3) :: buf2         ! Data buffer
integer*1, dimension(width*height*3) :: bufr2        ! Data buffer
integer(HSIZE_T) :: widthr                           ! width of image  
integer(HSIZE_T) :: heightr                          ! height of image
integer(HSIZE_T) :: planesr                          ! color planes
integer(HSIZE_T) :: npalsr                           ! palettes
character(LEN=15) :: interlacer                      ! interlace 
integer :: errcode                                   ! Error flag
integer :: is_image                                  ! Error flag
integer :: i, n                                      ! general purpose integer
!
! palette
! create a 9 entry grey palette 
!
character(LEN=4), parameter :: pal_name = "pal1"     ! Dataset name
integer(HSIZE_T), dimension(2) :: pal_dims = (/9,3/) ! Dataset dimensions
integer(HSIZE_T), dimension(2) :: pal_dims_out       ! Dataset dimensions
integer*1, dimension(9*3) :: pal_data_in = (/0,0,0,25,25,25,50,50,50,75,75,75,100,100,100,&
 125,125,125,125,125,125,125,125,125,125,125,125/)
integer*1, dimension(9*3) :: pal_data_out            ! Data buffer
integer(HSIZE_T) :: npals                            ! number of palettes
integer          :: pal_number                       ! palette number 
integer          :: is_palette                       ! is palette 

!
! Initialize the data array.
!
n = 0
do i = 1, width*height
  buf1(i) = n;
  n = n + 1;
end do

n = 0
do i = 1, width*height*3
  buf2(i) = n;
  n = n + 1;
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
! indexed image 
!-------------------------------------------------------------------------

call test_begin(' Make/Read image 8bit           ')

!
! write image. 
!
call h5immake_image_8bit_f(file_id,dsetname1,width,height,buf1,errcode)

!
! read image. 
!
call h5imread_image_f(file_id,dsetname1,bufr1,errcode)
                            
!
! compare read and write buffers.
!
do i = 1, width*height
 if ( buf1(i) .ne. bufr1(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr1(i), ' and ',   buf1(i)
   stop
  endif
end do

!
! get image info. 
!
call h5imget_image_info_f(file_id,dsetname1,widthr,heightr,planesr,interlacer,npalsr,errcode)

if ( (widthr .ne. widthr) .or. (heightr .ne. height) .or. (planesr .ne. 1)) then
 print *, 'h5imget_image_info_f bad value'
 stop
endif

is_image = h5imis_image_f(file_id,dsetname1)
if ( is_image .ne. 1) then
 print *, 'h5imis_image_f bad value'
 stop
endif


call passed()

!-------------------------------------------------------------------------
! true color image 
!-------------------------------------------------------------------------

call test_begin(' Make/Read image 24bit          ')

!
! write image. 
!
call h5immake_image_24bit_f(file_id,dsetname2,width,height,il,buf2,errcode)

!
! read image. 
!
call h5imread_image_f(file_id,dsetname2,bufr2,errcode)

!
! compare read and write buffers.
!
do i = 1, width*height*3
 if ( buf2(i) .ne. bufr2(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr2(i), ' and ',   buf2(i)
   stop
  endif
end do

!
! get image info. 
!
call h5imget_image_info_f(file_id,dsetname2,widthr,heightr,planesr,interlacer,npalsr,errcode)

if ( (widthr .ne. widthr) .or. (heightr .ne. height) .or. (planesr .ne. 3)) then
 print *, 'h5imget_image_info_f bad value'
 stop
endif

is_image = h5imis_image_f(file_id,dsetname2)
if ( is_image .ne. 1) then
 print *, 'h5imis_image_f bad value'
 stop
endif



call passed()

!-------------------------------------------------------------------------
! palette 
!-------------------------------------------------------------------------

call test_begin(' Make palette                   ')

!
! make palette. 
!
call h5immake_palette_f(file_id,pal_name,pal_dims,pal_data_in,errcode)           

call passed()


call test_begin(' Link/Unlink palette            ')

!
! link palette. 
!
call h5imlink_palette_f(file_id,dsetname1,pal_name,errcode)


!
! read palette. 
!
pal_number = 0
call h5imget_palette_f(file_id,dsetname1,pal_number,pal_data_out,errcode)

!
! compare read and write buffers.
!
do i = 1, 9*3
 if ( pal_data_in(i) .ne. pal_data_out(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  pal_data_in(i), ' and ',   pal_data_out(i)
   stop
  endif
end do

!
! get number of palettes
!
call h5imget_npalettes_f(file_id,dsetname1,npals,errcode) 

if ( npals .ne. 1) then
 print *, 'h5imget_npalettes_f bad value'
 stop
endif

!
! get palette info
!
pal_number = 0
call h5imget_palette_info_f(file_id,dsetname1,pal_number,pal_dims_out,errcode) 

if ( (pal_dims_out(1) .ne. pal_dims(1)) .or. (pal_dims_out(2) .ne. pal_dims(2))) then
 print *, 'h5imget_palette_info_f bad value'
 stop
endif

!
! is palette 
!
is_palette = h5imis_palette_f(file_id,pal_name) 

if ( is_palette .ne. 1 ) then
 print *, 'h5imis_palette_f bad value'
 stop
endif

!
! unlink palette. 
!
call h5imunlink_palette_f(file_id,dsetname1,pal_name,errcode)

!
! get number of palettes
!
call h5imget_npalettes_f(file_id,dsetname1,npals,errcode ) 

if ( npals .ne. 0) then
 print *, 'h5imget_npalettes_f bad value'
 stop
endif

call passed()


!-------------------------------------------------------------------------
! end 
!-------------------------------------------------------------------------

!
! Close the file.
!
call h5fclose_f(file_id, errcode)

!
! Close FORTRAN predefined datatypes.
!
call h5close_f(errcode)


!
! end function.
!
end subroutine make_image1




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
