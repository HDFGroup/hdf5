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
! This file contains FORTRAN90 interfaces for H5TB functions
!
      
module H5TB
use H5FORTRAN_TYPES
use HDF5 


interface h5tbwrite_field_name_f
 module procedure h5tbwrite_field_name_f_int
 module procedure h5tbwrite_field_name_f_float
 module procedure h5tbwrite_field_name_f_double
 module procedure h5tbwrite_field_name_f_string
end interface

interface h5tbread_field_name_f
 module procedure h5tbread_field_name_f_int
 module procedure h5tbread_field_name_f_float
 module procedure h5tbread_field_name_f_double
 module procedure h5tbread_field_name_f_string
end interface

interface h5tbwrite_field_index_f
 module procedure h5tbwrite_field_index_f_int
 module procedure h5tbwrite_field_index_f_float
 module procedure h5tbwrite_field_index_f_double
 module procedure h5tbwrite_field_index_f_string
end interface

interface h5tbread_field_index_f
 module procedure h5tbread_field_index_f_int
 module procedure h5tbread_field_index_f_float
 module procedure h5tbread_field_index_f_double
 module procedure h5tbread_field_index_f_string
end interface


interface h5tbinsert_field_f
 module procedure h5tbinsert_field_f_int
 module procedure h5tbinsert_field_f_float
 module procedure h5tbinsert_field_f_double
 module procedure h5tbinsert_field_f_string
end interface





contains


!-------------------------------------------------------------------------
! Function: h5tbmake_table_f
!
! Purpose: Make a table
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbmake_table_f(table_title,&
                            loc_id,& 
                            dset_name,&
                            nfields,& 
                            nrecords,&
                            type_size,&
                            field_names,&
                            field_offset,&
                            field_types,&
                            chunk_size,&
                            compress,&
                            errcode ) 

 implicit none
 character(LEN=*), intent(IN) :: table_title                      ! name of the dataset 
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 integer(HSIZE_T), intent(IN) :: nfields                          ! fields 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 character(LEN=*), dimension(nfields), intent(IN) :: field_names  ! field names
 integer(SIZE_T),  dimension(nfields), intent(IN) :: field_offset ! field offset
 integer(HID_T),   dimension(nfields), intent(IN) :: field_types  ! field types
 integer(HSIZE_T), intent(IN) :: chunk_size                       ! chunk size
 integer,          intent(IN) :: compress                         ! compress
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length
 integer :: errcode                                               ! error code
 integer, dimension(nfields) :: namelen2                          ! name lengths
 integer :: i                                                     ! general purpose integer

 
 interface
  integer function h5tbmake_table_c(namelen1,&
  table_title,&
  loc_id,&
  namelen,&
  dset_name,&
  nfields,&
  nrecords,&
  type_size,&
  field_offset,&
  field_types,&
  chunk_size,&
  compress,&
  namelen2,&
  field_names)

  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBMAKE_TABLE_C'::h5tbmake_table_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: table_title
  character(LEN=*), intent(IN) :: table_title                      ! name of the dataset 
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  integer(HSIZE_T), intent(IN) :: nfields                          ! fields 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  character(LEN=*), dimension(nfields), intent(IN) :: field_names  ! field names
  integer(SIZE_T),  dimension(nfields), intent(IN) :: field_offset ! field offset
  integer(HID_T),   dimension(nfields), intent(IN) :: field_types  ! field types
  integer(HSIZE_T), intent(IN) :: chunk_size                       ! chunk size
  integer,          intent(IN) :: compress                         ! compress
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length
  integer, dimension(nfields) :: namelen2                          ! name lengths
  end function h5tbmake_table_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(table_title)
 do i = 1, nfields
  namelen2(i) = len(field_names(i));
 end do

 errcode = h5tbmake_table_c(namelen1,&
  table_title,&
  loc_id,&
  namelen,&
  dset_name,&
  nfields,&
  nrecords,&
  type_size,&
  field_offset,&
  field_types,&
  chunk_size,&
  compress,&
  namelen2,&
  field_names)

end subroutine h5tbmake_table_f


!-------------------------------------------------------------------------
! Function: h5tbwrite_field_name_f_int
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbwrite_field_name_f_int(loc_id,& 
                                      dset_name,&
                                      field_name,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 integer, intent(IN), dimension(*) :: buf                         ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length

 interface
  integer function h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBWRITE_FIELD_NAME_C'::h5tbwrite_field_name_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  integer, intent(IN), dimension(*) :: buf                         ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length
  end function h5tbwrite_field_name_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

end subroutine h5tbwrite_field_name_f_int

!-------------------------------------------------------------------------
! Function: h5tbwrite_field_name_f_float
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbwrite_field_name_f_float(loc_id,& 
                                      dset_name,&
                                      field_name,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 real, intent(IN), dimension(*) :: buf                            ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length

 interface
  integer function h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBWRITE_FIELD_NAME_C'::h5tbwrite_field_name_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  real, intent(IN), dimension(*) :: buf                            ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length
  end function h5tbwrite_field_name_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

end subroutine h5tbwrite_field_name_f_float



!-------------------------------------------------------------------------
! Function: h5tbwrite_field_name_f_double
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbwrite_field_name_f_double(loc_id,& 
                                      dset_name,&
                                      field_name,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 double precision, intent(IN), dimension(*) :: buf                ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length

 interface
  integer function h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBWRITE_FIELD_NAME_C'::h5tbwrite_field_name_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  double precision, intent(IN), dimension(*) :: buf                ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length
  end function h5tbwrite_field_name_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

end subroutine h5tbwrite_field_name_f_double

!-------------------------------------------------------------------------
! Function: h5tbwrite_field_name_f_string
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbwrite_field_name_f_string(loc_id,& 
                                      dset_name,&
                                      field_name,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 character(LEN=*), intent(IN), dimension(*) :: buf                ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length

 interface
  integer function h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBWRITE_FIELD_NAME_C'::h5tbwrite_field_name_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  character(LEN=*), intent(IN), dimension(*) :: buf                ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length
  end function h5tbwrite_field_name_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

end subroutine h5tbwrite_field_name_f_string


!-------------------------------------------------------------------------
! Function: h5tbread_field_name_f_int
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbread_field_name_f_int(loc_id,& 
                                      dset_name,&
                                      field_name,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 integer, intent(IN), dimension(*) :: buf                         ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length

 interface
  integer function h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBREAD_FIELD_NAME_C'::h5tbread_field_name_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  integer, intent(IN), dimension(*) :: buf                         ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length
  end function h5tbread_field_name_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

end subroutine h5tbread_field_name_f_int

!-------------------------------------------------------------------------
! Function: h5tbread_field_name_f_float
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbread_field_name_f_float(loc_id,& 
                                      dset_name,&
                                      field_name,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 real, intent(IN), dimension(*) :: buf                            ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length

 interface
  integer function h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBREAD_FIELD_NAME_C'::h5tbread_field_name_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  real, intent(IN), dimension(*) :: buf                            ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length
  end function h5tbread_field_name_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

end subroutine h5tbread_field_name_f_float

!-------------------------------------------------------------------------
! Function: h5tbread_field_name_f_double
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbread_field_name_f_double(loc_id,& 
                                      dset_name,&
                                      field_name,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 double precision, intent(IN), dimension(*) :: buf                ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length

 interface
  integer function h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBREAD_FIELD_NAME_C'::h5tbread_field_name_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  double precision, intent(IN), dimension(*) :: buf                ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length
  end function h5tbread_field_name_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

end subroutine h5tbread_field_name_f_double

!-------------------------------------------------------------------------
! Function: h5tbread_field_name_f_string
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbread_field_name_f_string(loc_id,& 
                                      dset_name,&
                                      field_name,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 character(LEN=*), intent(IN), dimension(*) :: buf                ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length

 interface
  integer function h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBREAD_FIELD_NAME_C'::h5tbread_field_name_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  character(LEN=*), intent(IN), dimension(*) :: buf                ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length
  end function h5tbread_field_name_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

end subroutine h5tbread_field_name_f_string





!-------------------------------------------------------------------------
! Function: h5tbwrite_field_index_f_int
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbwrite_field_index_f_int(loc_id,& 
                                      dset_name,&
                                      field_index,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 integer, intent(IN) :: field_index                               ! index
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 integer, intent(IN), dimension(*) :: buf                         ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length

 interface
  integer function h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBWRITE_FIELD_INDEX_C'::h5tbwrite_field_index_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  integer, intent(IN) :: field_index                               ! index
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  integer, intent(IN), dimension(*) :: buf                         ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  end function h5tbwrite_field_index_c
 end interface

 namelen  = len(dset_name)
 
 errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

end subroutine h5tbwrite_field_index_f_int

!-------------------------------------------------------------------------
! Function: h5tbwrite_field_index_f_float
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbwrite_field_index_f_float(loc_id,& 
                                      dset_name,&
                                      field_index,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 integer, intent(IN) :: field_index                               ! index
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 real, intent(IN), dimension(*) :: buf                            ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length

 interface
  integer function h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBWRITE_FIELD_INDEX_C'::h5tbwrite_field_index_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  integer, intent(IN) :: field_index                               ! index
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  real, intent(IN), dimension(*) :: buf                            ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  end function h5tbwrite_field_index_c
 end interface

 namelen  = len(dset_name)
 
 errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

end subroutine h5tbwrite_field_index_f_float



!-------------------------------------------------------------------------
! Function: h5tbwrite_field_index_f_double
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbwrite_field_index_f_double(loc_id,& 
                                      dset_name,&
                                      field_index,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 integer, intent(IN) :: field_index                               ! index
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 double precision, intent(IN), dimension(*) :: buf                ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length

 interface
  integer function h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBWRITE_FIELD_INDEX_C'::h5tbwrite_field_index_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  integer, intent(IN) :: field_index                               ! index
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  double precision, intent(IN), dimension(*) :: buf                ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  end function h5tbwrite_field_index_c
 end interface

 namelen  = len(dset_name)
 
 errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

end subroutine h5tbwrite_field_index_f_double

!-------------------------------------------------------------------------
! Function: h5tbwrite_field_index_f_string
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbwrite_field_index_f_string(loc_id,& 
                                      dset_name,&
                                      field_index,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 integer, intent(IN) :: field_index                               ! index
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 character(LEN=*), intent(IN), dimension(*) :: buf                ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length

 interface
  integer function h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBWRITE_FIELD_INDEX_C'::h5tbwrite_field_index_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  integer, intent(IN) :: field_index                               ! index
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  character(LEN=*), intent(IN), dimension(*) :: buf                ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  end function h5tbwrite_field_index_c
 end interface

 namelen  = len(dset_name)
 
 errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

end subroutine h5tbwrite_field_index_f_string


!-------------------------------------------------------------------------
! Function: h5tbread_field_index_f_int
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbread_field_index_f_int(loc_id,& 
                                      dset_name,&
                                      field_index,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 integer, intent(IN) :: field_index                               ! index
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 integer, intent(IN), dimension(*) :: buf                         ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length

 interface
  integer function h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBREAD_FIELD_INDEX_C'::h5tbread_field_index_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  integer, intent(IN) :: field_index                               ! index
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  integer, intent(IN), dimension(*) :: buf                         ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  end function h5tbread_field_index_c
 end interface

 namelen  = len(dset_name)
 
 errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

end subroutine h5tbread_field_index_f_int

!-------------------------------------------------------------------------
! Function: h5tbread_field_index_f_float
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbread_field_index_f_float(loc_id,& 
                                      dset_name,&
                                      field_index,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 integer, intent(IN) :: field_index                               ! index
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 real, intent(IN), dimension(*) :: buf                            ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length

 interface
  integer function h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
   start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBREAD_FIELD_INDEX_C'::h5tbread_field_index_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  integer, intent(IN) :: field_index                               ! index
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  real, intent(IN), dimension(*) :: buf                            ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  end function h5tbread_field_index_c
 end interface

 namelen  = len(dset_name)
 
 errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

end subroutine h5tbread_field_index_f_float

!-------------------------------------------------------------------------
! Function: h5tbread_field_index_f_double
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbread_field_index_f_double(loc_id,& 
                                      dset_name,&
                                      field_index,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 integer, intent(IN) :: field_index                               ! index
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 double precision, intent(IN), dimension(*) :: buf                ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length

 interface
  integer function h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
   start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBREAD_FIELD_INDEX_C'::h5tbread_field_index_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  integer, intent(IN) :: field_index                               ! index
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  double precision, intent(IN), dimension(*) :: buf                ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  end function h5tbread_field_index_c
 end interface

 namelen  = len(dset_name)
 
 errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

end subroutine h5tbread_field_index_f_double

!-------------------------------------------------------------------------
! Function: h5tbread_field_index_f_string
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbread_field_index_f_string(loc_id,& 
                                      dset_name,&
                                      field_index,& 
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 integer, intent(IN) :: field_index                               ! index
 integer(HSIZE_T), intent(IN) :: start                            ! start record 
 integer(HSIZE_T), intent(IN) :: nrecords                         ! records
 integer(SIZE_T),  intent(IN) :: type_size                        ! type size
 character(LEN=*), intent(IN), dimension(*) :: buf                ! data buffer 
 integer :: errcode                                               ! error code
 integer :: namelen                                               ! name length

 interface
  integer function h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
   start,nrecords,type_size,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBREAD_FIELD_INDEX_C'::h5tbread_field_index_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  integer, intent(IN) :: field_index                               ! index
  integer(HSIZE_T), intent(IN) :: start                            ! start record 
  integer(HSIZE_T), intent(IN) :: nrecords                         ! records
  integer(SIZE_T),  intent(IN) :: type_size                        ! type size
  character(LEN=*), intent(IN), dimension(*) :: buf                ! data buffer 
  integer :: errcode                                               ! error code
  integer :: namelen                                               ! name length
  end function h5tbread_field_index_c
 end interface

 namelen  = len(dset_name)
 
 errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

end subroutine h5tbread_field_index_f_string





!-------------------------------------------------------------------------
! Function: h5tbinsert_field_f_int
!
! Purpose: Inserts one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbinsert_field_f_int(loc_id,& 
                                  dset_name,&
                                  field_name,& 
                                  field_type,&
                                  field_index,&
                                  buf,&
                                  errcode ) 
 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HID_T), intent(IN)   :: field_type                       ! field type
 integer, intent(IN) :: field_index                               ! field_index
 integer, intent(IN), dimension(*) :: buf                         ! data buffer 
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length
 integer :: errcode                                               ! error code


 interface
  integer function h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBINSERT_FIELD_C'::h5tbinsert_field_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HID_T), intent(IN)   :: field_type                       ! field type
  integer, intent(IN) :: field_index                               ! field_index
  integer, intent(IN), dimension(*) :: buf                         ! data buffer 
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length length
  end function h5tbinsert_field_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

end subroutine h5tbinsert_field_f_int



!-------------------------------------------------------------------------
! Function: h5tbinsert_field_f_float
!
! Purpose: Inserts one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbinsert_field_f_float(loc_id,& 
                                  dset_name,&
                                  field_name,& 
                                  field_type,&
                                  field_index,&
                                  buf,&
                                  errcode ) 
 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HID_T), intent(IN)   :: field_type                       ! field type
 integer, intent(IN) :: field_index                               ! field_index
 real, intent(IN), dimension(*) :: buf                            ! data buffer 
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length
 integer :: errcode                                               ! error code


 interface
  integer function h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBINSERT_FIELD_C'::h5tbinsert_field_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HID_T), intent(IN)   :: field_type                       ! field type
  integer, intent(IN) :: field_index                               ! field_index
  real, intent(IN), dimension(*) :: buf                            ! data buffer 
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length length
  end function h5tbinsert_field_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

end subroutine h5tbinsert_field_f_float



!-------------------------------------------------------------------------
! Function: h5tbinsert_field_f_double
!
! Purpose: Inserts one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbinsert_field_f_double(loc_id,& 
                                  dset_name,&
                                  field_name,& 
                                  field_type,&
                                  field_index,&
                                  buf,&
                                  errcode ) 
 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HID_T), intent(IN)   :: field_type                       ! field type
 integer, intent(IN) :: field_index                               ! field_index
 double precision, intent(IN), dimension(*) :: buf                ! data buffer 
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length
 integer :: errcode                                               ! error code


 interface
  integer function h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBINSERT_FIELD_C'::h5tbinsert_field_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HID_T), intent(IN)   :: field_type                       ! field type
  integer, intent(IN) :: field_index                               ! field_index
  double precision, intent(IN), dimension(*) :: buf                ! data buffer 
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length length
  end function h5tbinsert_field_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

end subroutine h5tbinsert_field_f_double




!-------------------------------------------------------------------------
! Function: h5tbinsert_field_f_string
!
! Purpose: Inserts one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbinsert_field_f_string(loc_id,& 
                                  dset_name,&
                                  field_name,& 
                                  field_type,&
                                  field_index,&
                                  buf,&
                                  errcode ) 
 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer(HID_T), intent(IN)   :: field_type                       ! field type
 integer, intent(IN) :: field_index                               ! field_index
 character(LEN=*), intent(IN), dimension(*) :: buf                ! data buffer 
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length
 integer :: errcode                                               ! error code


 interface
  integer function h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBINSERT_FIELD_C'::h5tbinsert_field_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer(HID_T), intent(IN)   :: field_type                       ! field type
  integer, intent(IN) :: field_index                               ! field_index
  character(LEN=*), intent(IN), dimension(*) :: buf                ! data buffer 
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length length
  end function h5tbinsert_field_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

end subroutine h5tbinsert_field_f_string




!-------------------------------------------------------------------------
! Function: h5tbdelete_field_f_int
!
! Purpose: Inserts one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbdelete_field_f(loc_id,& 
                              dset_name,&
                              field_name,&
                              errcode ) 
 implicit none
 integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
 character(LEN=*), intent(IN) :: field_name                       ! name of the field
 integer :: namelen                                               ! name length
 integer :: namelen1                                              ! name length
 integer :: errcode                                               ! error code


 interface
  integer function h5tbdelete_field_c(loc_id,namelen,dset_name,namelen1,field_name)
 
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBDELETE_FIELD_C'::h5tbdelete_field_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  integer(HID_T),   intent(IN) :: loc_id                           ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                        ! name of the dataset 
  character(LEN=*), intent(IN) :: field_name                       ! name of the field
  integer :: namelen                                               ! name length
  integer :: namelen1                                              ! name length length
  end function h5tbdelete_field_c
 end interface

 namelen  = len(dset_name)
 namelen1 = len(field_name)
 
 errcode = h5tbdelete_field_c(loc_id,namelen,dset_name,namelen1,field_name)

end subroutine h5tbdelete_field_f



!-------------------------------------------------------------------------
! Function: h5tbget_table_info_f
!
! Purpose: Gets the number of records and fields of a table
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbget_table_info_f(loc_id,& 
                                dset_name,&
                                nfields,&
                                nrecords,&
                                errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id             ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name          ! name of the dataset 
 integer(HSIZE_T), intent(INOUT):: nfields          ! nfields 
 integer(HSIZE_T), intent(INOUT):: nrecords         ! nrecords 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5tbget_table_info_c(loc_id,namelen,dset_name,nfields,nrecords)
  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBGET_TABLE_INFO_C'::h5tbget_table_info_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(HID_T),   intent(IN) :: loc_id             ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name          ! name of the dataset 
  integer(HSIZE_T), intent(INOUT):: nfields          ! nfields 
  integer(HSIZE_T), intent(INOUT):: nrecords         ! nrecords 
  integer :: namelen                                 ! name length
  end function h5tbget_table_info_c
 end interface

 namelen = len(dset_name)
 errcode = h5tbget_table_info_c(loc_id,namelen,dset_name,nfields,nrecords)

end subroutine h5tbget_table_info_f






!-------------------------------------------------------------------------
! Function: h5tbget_field_info_f
!
! Purpose: Get information about fields
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5tbget_field_info_f(loc_id,& 
                                dset_name,&
                                nfields,&
                                field_names,&
								field_sizes,&
								field_offsets,&
								type_size,&
                                errcode ) 

 implicit none
 integer(HID_T),   intent(IN) :: loc_id                                ! file or group identifier 
 character(LEN=*), intent(IN) :: dset_name                             ! name of the dataset 
 integer(HSIZE_T), intent(IN):: nfields                                ! nfields 
 character(LEN=*), dimension(nfields), intent(INOUT) :: field_names    ! field names
 integer(SIZE_T),  dimension(nfields), intent(INOUT) :: field_sizes    ! field sizes
 integer(SIZE_T),  dimension(nfields), intent(INOUT) :: field_offsets  ! field offsets
 integer(SIZE_T),  intent(INOUT):: type_size                           ! type size 
 integer :: errcode                                                    ! error code
 integer :: namelen                                                    ! name length
 integer, dimension(nfields) :: namelen2                               ! name lengths
 integer :: i                                                          ! general purpose integer

 interface
  integer function h5tbget_field_info_c(loc_id,namelen,dset_name,nfields,&
   field_sizes,field_offsets,type_size,namelen2,field_names)

  use H5GLOBAL
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5TBGET_FIELD_INFO_C'::h5tbget_field_info_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(HID_T),   intent(IN) :: loc_id                                ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name                             ! name of the dataset 
  integer(HSIZE_T), intent(IN):: nfields                                ! nfields 
  character(LEN=*), dimension(nfields), intent(INOUT) :: field_names    ! field names
  integer(SIZE_T),  dimension(nfields), intent(INOUT) :: field_sizes    ! field sizes
  integer(SIZE_T),  dimension(nfields), intent(INOUT) :: field_offsets  ! field offsets
  integer(SIZE_T),  intent(INOUT):: type_size                           ! type size 
  integer :: namelen                                                    ! name length
  integer, dimension(nfields) :: namelen2                               ! name lengths
  end function h5tbget_field_info_c
 end interface

 namelen = len(dset_name)
 do i = 1, nfields
  namelen2(i) = len(field_names(i));
 end do
 errcode = h5tbget_field_info_c(loc_id,namelen,dset_name,nfields,&
   field_sizes,field_offsets,type_size,namelen2,field_names)

end subroutine h5tbget_field_info_f



!  end
!
end module H5TB
            

                                     



