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
! This file contains the FORTRAN90 tests for H5LT 
!

program table_test

call test_table1()


end program table_test 


!-------------------------------------------------------------------------
! test_table1 
!-------------------------------------------------------------------------

subroutine test_table1()

use H5TB ! module of H5TB 
use HDF5 ! module of HDF5 library

implicit none

character(len=8), parameter :: filename = "f1tab.h5"   ! File name
character(LEN=5), parameter :: dsetname1 = "dset1"     ! Dataset name
integer(HID_T) :: file_id                              ! File identifier 
integer(HSIZE_T), parameter :: nfields  = 4;           ! nfields
integer(HSIZE_T), parameter :: nrecords = 5;           ! nrecords
character(LEN=6), dimension(nfields) :: field_names    ! field names
integer(SIZE_T),  dimension(nfields) :: field_offset   ! field offset
integer(HID_T),   dimension(nfields) :: field_types    ! field types
integer(HSIZE_T), parameter  :: chunk_size = 5         ! chunk size
integer, parameter :: compress = 0                     ! compress
integer            :: errcode                          ! Error flag
integer            :: i                                ! general purpose integer
integer(SIZE_T)    :: type_size                        ! Size of the datatype
integer(SIZE_T)    :: type_sizec                       ! Size of the character datatype 
integer(SIZE_T)    :: type_sizei                       ! Size of the integer datatype
integer(SIZE_T)    :: type_sized                       ! Size of the double precision datatype
integer(SIZE_T)    :: type_sizer                       ! Size of the real datatype
integer(HID_T)     :: type_id_c                        ! Memory datatype identifier (for character field)
integer(SIZE_T)    :: offset                           ! Member's offset
integer(HSIZE_T)   :: start = 0                        ! start record
integer, dimension(nrecords) :: bufi                   ! Data buffer
integer, dimension(nrecords) :: bufir                  ! Data buffer
real, dimension(nrecords) :: bufr                      ! Data buffer
real, dimension(nrecords) :: bufrr                     ! Data buffer
double precision, dimension(nrecords) :: bufd          ! Data buffer
double precision, dimension(nrecords) :: bufdr         ! Data buffer
character(LEN=2), dimension(nrecords), parameter :: bufs = (/"AB","CD","EF","GH","IJ"/) ! Data buffer
character(LEN=2), dimension(nrecords) :: bufsr         ! Data buffer
integer(HSIZE_T) :: nfieldsr                           ! nfields
integer(HSIZE_T) :: nrecordsr                          ! nrecords
character(LEN=6), dimension(nfields) :: field_namesr   ! field names
integer(SIZE_T),  dimension(nfields) :: field_offsetr  ! field offset
integer(SIZE_T),  dimension(nfields) :: field_sizesr   ! field sizes
integer(SIZE_T)   :: type_sizeout                      ! size of the datatype


!
! Initialize the data arrays.
!
do i = 1, nrecords
  bufi(i) = i;
  bufr(i) = i;
  bufd(i) = i;
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
! make table
! initialize the table parameters
!-------------------------------------------------------------------------

field_names(1) = "field1"
field_names(2) = "field2"
field_names(3) = "field3"
field_names(4) = "field4"

!
! calculate total size by calculating sizes of each member
!
call h5tcopy_f(H5T_NATIVE_CHARACTER, type_id_c, errcode)
type_size = 2
call h5tset_size_f(type_id_c, type_size, errcode)
call h5tget_size_f(type_id_c, type_sizec, errcode)
call h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, errcode)
call h5tget_size_f(H5T_NATIVE_DOUBLE, type_sized, errcode)
call h5tget_size_f(H5T_NATIVE_REAL, type_sizer, errcode)
type_size = type_sizec + type_sizei + type_sized + type_sizer

!
! type ID's
!
field_types(1) = type_id_c
field_types(2) = H5T_NATIVE_INTEGER
field_types(3) = H5T_NATIVE_DOUBLE
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

call test_begin(' Make table                     ')


call h5tbmake_table_f(dsetname1,&
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

call passed()

!-------------------------------------------------------------------------
! write field
!-------------------------------------------------------------------------

call test_begin(' Read/Write field by name       ')

call h5tbwrite_field_name_f(file_id,dsetname1,field_names(1),start,nrecords,type_sizec,&
                            bufs,errcode) 

call h5tbwrite_field_name_f(file_id,dsetname1,field_names(2),start,nrecords,type_sizei,&
                            bufi,errcode)

call h5tbwrite_field_name_f(file_id,dsetname1,field_names(3),start,nrecords,type_sized,&
                            bufd,errcode)

call h5tbwrite_field_name_f(file_id,dsetname1,field_names(4),start,nrecords,type_sizer,&
                            bufr,errcode)



!-------------------------------------------------------------------------
! read field
!-------------------------------------------------------------------------

call h5tbread_field_name_f(file_id,dsetname1,field_names(1),start,nrecords,type_sizec,&
                            bufsr,errcode)

!
! compare read and write buffers.
!
do i = 1, nrecords
 if ( bufsr(i) .ne. bufs(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufsr(i), ' and ',   bufs(i)
   stop
  endif
end do

call h5tbread_field_name_f(file_id,dsetname1,field_names(2),start,nrecords,type_sizei,&
                            bufir,errcode)

!
! compare read and write buffers.
!
do i = 1, nrecords
 if ( bufir(i) .ne. bufi(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufir(i), ' and ',   bufi(i)
   stop
  endif
end do

call h5tbread_field_name_f(file_id,dsetname1,field_names(3),start,nrecords,type_sized,&
                            bufdr,errcode)

!
! compare read and write buffers.
!
do i = 1, nrecords
 if ( bufdr(i) .ne. bufd(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufdr(i), ' and ',   bufd(i)
   stop
  endif
end do

call h5tbread_field_name_f(file_id,dsetname1,field_names(4),start,nrecords,type_sizer,&
                            bufrr,errcode)

!
! compare read and write buffers.
!
do i = 1, nrecords
 if ( bufrr(i) .ne. bufr(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufrr(i), ' and ',   bufr(i)
   stop
  endif
end do

                     
call passed()


!-------------------------------------------------------------------------
! write field
!-------------------------------------------------------------------------

call test_begin(' Read/Write field by index      ')

call h5tbwrite_field_index_f(file_id,dsetname1,1,start,nrecords,type_sizec,&
                            bufs,errcode) 

call h5tbwrite_field_index_f(file_id,dsetname1,2,start,nrecords,type_sizei,&
                            bufi,errcode)

call h5tbwrite_field_index_f(file_id,dsetname1,3,start,nrecords,type_sized,&
                            bufd,errcode)

call h5tbwrite_field_index_f(file_id,dsetname1,4,start,nrecords,type_sizer,&
                            bufr,errcode)



!-------------------------------------------------------------------------
! read field
!-------------------------------------------------------------------------

call h5tbread_field_index_f(file_id,dsetname1,1,start,nrecords,type_sizec,&
                            bufsr,errcode)

!
! compare read and write buffers.
!
do i = 1, nrecords
 if ( bufsr(i) .ne. bufs(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufsr(i), ' and ',   bufs(i)
   stop
  endif
end do

call h5tbread_field_index_f(file_id,dsetname1,2,start,nrecords,type_sizei,&
                            bufir,errcode)

!
! compare read and write buffers.
!
do i = 1, nrecords
 if ( bufir(i) .ne. bufi(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufir(i), ' and ',   bufi(i)
   stop
  endif
end do

call h5tbread_field_index_f(file_id,dsetname1,3,start,nrecords,type_sized,&
                            bufdr,errcode)

!
! compare read and write buffers.
!
do i = 1, nrecords
 if ( bufdr(i) .ne. bufd(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufdr(i), ' and ',   bufd(i)
   stop
  endif
end do

call h5tbread_field_index_f(file_id,dsetname1,4,start,nrecords,type_sizer,&
                            bufrr,errcode)

!
! compare read and write buffers.
!
do i = 1, nrecords
 if ( bufrr(i) .ne. bufr(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufrr(i), ' and ',   bufr(i)
   stop
  endif
end do

                     
call passed()


!-------------------------------------------------------------------------
! Insert field 
! we insert a field callsed "field5" with the same type and buffer as field 4 (Real)
!-------------------------------------------------------------------------

call test_begin(' Insert field                   ')

call h5tbinsert_field_f(file_id,dsetname1,"field5",field_types(4),4,bufr,errcode)

call h5tbread_field_index_f(file_id,dsetname1,5,start,nrecords,type_sizer,&
                            bufrr,errcode)

!
! compare read and write buffers.
!
do i = 1, nrecords
 if ( bufrr(i) .ne. bufr(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufrr(i), ' and ',   bufr(i)
   stop
  endif
end do

call passed()


!-------------------------------------------------------------------------
! Delete field
!-------------------------------------------------------------------------

call test_begin(' Delete field                   ')

call h5tbdelete_field_f(file_id,dsetname1,"field4",errcode)

call passed()


!-------------------------------------------------------------------------
! Gets the number of records and fields 
!-------------------------------------------------------------------------

call test_begin(' Get table info                 ')

call h5tbget_table_info_f(file_id,dsetname1,nfieldsr,nrecordsr,errcode ) 

if ( nfieldsr .ne. nfields .and. nrecordsr .ne. nrecords ) then
 print *, 'h5tbget_table_info_f return error'
 stop
endif

call passed()


!-------------------------------------------------------------------------
! Get information about fields
!-------------------------------------------------------------------------

!call test_begin(' Get fields info                ')

!call h5tbget_field_info_f(file_id,dsetname1,nfields,field_namesr,field_sizesr,&
!						  field_offsetr,type_sizeout,errcode ) 


!call passed()



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
end subroutine test_table1





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


