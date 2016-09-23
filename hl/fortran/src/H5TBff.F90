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
! This file contains FORTRAN interfaces for H5TB functions
!
!
! NOTES
!
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!                             
!  If you add a new function here then you MUST add the function name to the
!  Windows dll file 'hdf5_hl_fortrandll.def.in' in the hl/fortran/src directory.
!  This is needed for Windows based operating systems.
!
#include "H5config_f.inc"

MODULE h5tb_CONST
  
  USE, INTRINSIC :: ISO_C_BINDING
  USE h5fortran_types
  USE hdf5

  INTERFACE h5tbwrite_field_name_f
     MODULE PROCEDURE h5tbwrite_field_name_f_int
     MODULE PROCEDURE h5tbwrite_field_name_f_string
  END INTERFACE
  
  INTERFACE h5tbread_field_name_f
     MODULE PROCEDURE h5tbread_field_name_f_int
     MODULE PROCEDURE h5tbread_field_name_f_string
  END INTERFACE
  
  INTERFACE h5tbwrite_field_index_f
     MODULE PROCEDURE h5tbwrite_field_index_f_int
     MODULE PROCEDURE h5tbwrite_field_index_f_string
  END INTERFACE
  
  INTERFACE h5tbread_field_index_f
     MODULE PROCEDURE h5tbread_field_index_f_int
     MODULE PROCEDURE h5tbread_field_index_f_string
  END INTERFACE
  
  INTERFACE h5tbinsert_field_f
     MODULE PROCEDURE h5tbinsert_field_f_int
     MODULE PROCEDURE h5tbinsert_field_f_string
  END INTERFACE

  INTERFACE h5tbmake_table_f
     MODULE PROCEDURE h5tbmake_table_f90
     MODULE PROCEDURE h5tbmake_table_ptr_f
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
          start,nrecords,type_size,buf) &
          BIND(C,NAME='h5tbwrite_field_name_c')
       IMPORT :: C_CHAR, C_PTR
       IMPORT :: HID_T, SIZE_T, HSIZE_T
       IMPLICIT NONE
       INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name    ! name of the dataset
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: field_name   ! name of the field
       INTEGER(hsize_t), INTENT(in) :: start                            ! start record
       INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
       INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
       TYPE(C_PTR), VALUE :: buf                                        ! data buffer
       INTEGER :: errcode                                               ! error code
       INTEGER(size_t) :: namelen                                       ! name length
       INTEGER(size_t) :: namelen1                                      ! name length
     END FUNCTION h5tbwrite_field_name_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name, &
          start,nrecords,type_size,buf) &
          BIND(C,NAME='h5tbread_field_name_c')
       IMPORT :: C_CHAR, C_PTR
       IMPORT :: HID_T, SIZE_T, HSIZE_T
       IMPLICIT NONE
       INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name    ! name of the dataset
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: field_name   ! name of the field
       INTEGER(hsize_t), INTENT(in) :: start                            ! start record
       INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
       INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
       TYPE(C_PTR), VALUE :: buf                                        ! data buffer
       INTEGER :: errcode                                               ! error code
       INTEGER(size_t) :: namelen                                       ! name length
       INTEGER(size_t) :: namelen1                                      ! name length
     END FUNCTION h5tbread_field_name_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
          start,nrecords,type_size,buf) &
          BIND(C,NAME='h5tbwrite_field_index_c')
       IMPORT :: C_CHAR, C_PTR
       IMPORT :: HID_T, SIZE_T, HSIZE_T
       IMPLICIT NONE
       INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name    ! name of the dataset
       INTEGER, INTENT(in) :: field_index                               ! index
       INTEGER(hsize_t), INTENT(in) :: start                            ! start record
       INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
       INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
       TYPE(C_PTR), VALUE :: buf                                        ! data buffer
       INTEGER :: errcode                                               ! error code
       INTEGER(size_t) :: namelen                                       ! name length
     END FUNCTION h5tbwrite_field_index_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
          start,nrecords,type_size,buf) &
          BIND(C,NAME='h5tbread_field_index_c')
       IMPORT :: C_CHAR, C_PTR
       IMPORT :: HID_T, SIZE_T, HSIZE_T
       IMPLICIT NONE
       INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name    ! name of the dataset
       INTEGER, INTENT(in) :: field_index                               ! index
       INTEGER(hsize_t), INTENT(in) :: start                            ! start record
       INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
       INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
       TYPE(C_PTR), VALUE :: buf                                        ! data buffer
       INTEGER :: errcode                                               ! error code
       INTEGER(size_t) :: namelen                                       ! name length
     END FUNCTION h5tbread_field_index_c
  END INTERFACE


  INTERFACE
     INTEGER FUNCTION h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
          field_type,field_index,buf) &
          BIND(C,NAME='h5tbinsert_field_c')
       IMPORT :: C_CHAR, C_PTR
       IMPORT :: HID_T, SIZE_T, HSIZE_T
       IMPLICIT NONE
       INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name                        ! name of the dataset
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: field_name                       ! name of the field
       INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
       INTEGER, INTENT(in) :: field_index                               ! field_index
       TYPE(C_PTR), VALUE :: buf                                        ! data buffer
       INTEGER(size_t) :: namelen                                       ! name length
       INTEGER(size_t) :: namelen1                                      ! name length length
     END FUNCTION h5tbinsert_field_c
  END INTERFACE
  
CONTAINS

!-------------------------------------------------------------------------
! Function: h5tbmake_table_f90
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

  SUBROUTINE h5tbmake_table_f90(table_title,&
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

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(in) :: table_title                      ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    INTEGER(hsize_t), INTENT(in) :: nfields                          ! fields
    INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
    INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
    CHARACTER(LEN=*), DIMENSION(1:nfields), INTENT(in) :: field_names  ! field names
    INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: field_offset ! field offset
    INTEGER(hid_t),   DIMENSION(1:nfields), INTENT(in) :: field_types  ! field types
    INTEGER(hsize_t), INTENT(in) :: chunk_size                       ! chunk size
    INTEGER,          INTENT(in) :: compress                         ! compress
    INTEGER(size_t) :: namelen                                       ! name length
    INTEGER(size_t) :: namelen1                                      ! name length
    INTEGER :: errcode                                               ! error code
    INTEGER(size_t), DIMENSION(1:nfields) :: char_len_field_names    ! field name lengths
    INTEGER(size_t) :: max_char_size_field_names                     ! character len of field names
    INTEGER(hsize_t) :: i                                            ! general purpose integer

    INTERFACE
       INTEGER FUNCTION h5tbmake_table_c(namelen1,&
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
            char_len_field_names,&
            max_char_size_field_names,&
            field_names) &
            BIND(C,NAME='h5tbmake_table_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: table_title  ! name of the dataset
         INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name    ! name of the dataset
         INTEGER(hsize_t), INTENT(in) :: nfields                          ! fields
         INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
         INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
         CHARACTER(KIND=C_CHAR), DIMENSION(nfields), INTENT(in) :: field_names  ! field names
         INTEGER(size_t),  DIMENSION(nfields), INTENT(in) :: field_offset ! field offset
         INTEGER(hid_t),   DIMENSION(nfields), INTENT(in) :: field_types  ! field types
         INTEGER(hsize_t), INTENT(in) :: chunk_size                       ! chunk size
         INTEGER,          INTENT(in) :: compress                         ! compress
         INTEGER(size_t) :: namelen                                       ! name length
         INTEGER(size_t) :: namelen1                                      ! name length
         INTEGER(size_t), DIMENSION(nfields) :: char_len_field_names      ! field name's lengths
         INTEGER(size_t) :: max_char_size_field_names                     ! character len of field names
       END FUNCTION h5tbmake_table_c
    END INTERFACE
 
    namelen  = LEN(dset_name)
    namelen1 = LEN(table_title)
 
    ! Find the size of each character string in the array
    DO i = 1, nfields
       char_len_field_names(i) = LEN_TRIM(field_names(i))
    END DO
    
    max_char_size_field_names = LEN(field_names(1)) 

    errcode = h5tbmake_table_c(namelen1, table_title, loc_id, namelen, dset_name, nfields, nrecords,&
         type_size, field_offset, field_types, chunk_size, compress, char_len_field_names, &
         max_char_size_field_names, field_names)

  END SUBROUTINE h5tbmake_table_f90

  SUBROUTINE h5tbmake_table_ptr_f(table_title,&
       loc_id,&
       dset_name,&
       nfields,&
       nrecords,&
       type_size,&
       field_names,&
       field_offset,&
       field_types,&
       chunk_size,&
       fill_data,&
       compress,&
       data,&
       errcode )

    USE ISO_C_BINDING
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(in) :: table_title                      ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    INTEGER(hsize_t), INTENT(in) :: nfields                          ! fields
    INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
    INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
    CHARACTER(LEN=*), DIMENSION(1:nfields), INTENT(in) :: field_names  ! field names
    INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: field_offset ! field offset
    INTEGER(hid_t),   DIMENSION(1:nfields), INTENT(in) :: field_types  ! field types
    INTEGER(hsize_t), INTENT(in) :: chunk_size                       ! chunk size
    TYPE(C_PTR), INTENT(in) :: fill_data                             ! Fill values data
    INTEGER,          INTENT(in) :: compress                         ! compress
    TYPE(C_PTR), INTENT(in) :: data                                  ! Buffer with data to be written to the table
    INTEGER(size_t) :: namelen                                       ! name length
    INTEGER(size_t) :: namelen1                                      ! name length
    INTEGER :: errcode                                               ! error code
    INTEGER(size_t), DIMENSION(1:nfields) :: char_len_field_names    ! field name lengths
    INTEGER(size_t) :: max_char_size_field_names                     ! character len of field names
    INTEGER(hsize_t) :: i                                            ! general purpose integer

    INTERFACE
       INTEGER FUNCTION h5tbmake_table_ptr_c(namelen1,&
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
            fill_data,&
            compress,&
            char_len_field_names,&
            max_char_size_field_names,&
            field_names,&
            data) &
            BIND(C,NAME='h5tbmake_table_ptr_c')
         IMPORT :: C_CHAR, C_PTR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: table_title  ! name of the dataset
         INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name    ! name of the dataset
         INTEGER(hsize_t), INTENT(in) :: nfields                          ! fields
         INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
         INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
         CHARACTER(KIND=C_CHAR), DIMENSION(nfields), INTENT(in) :: field_names  ! field names
         INTEGER(size_t),  DIMENSION(nfields), INTENT(in) :: field_offset ! field offset
         INTEGER(hid_t),   DIMENSION(nfields), INTENT(in) :: field_types  ! field types
         INTEGER(hsize_t), INTENT(in) :: chunk_size                       ! chunk size
         TYPE(C_PTR), INTENT(in), VALUE :: fill_data                      ! Fill values data
         INTEGER,          INTENT(in) :: compress                         ! compress
         INTEGER(size_t) :: namelen                                       ! name length
         INTEGER(size_t) :: namelen1                                      ! name length
         INTEGER(size_t), DIMENSION(nfields) :: char_len_field_names      ! field name's lengths
         INTEGER(size_t) :: max_char_size_field_names                     ! character len of field names
         TYPE(C_PTR), INTENT(in), VALUE :: data
       END FUNCTION h5tbmake_table_ptr_c
    END INTERFACE
 
    namelen  = LEN(dset_name)
    namelen1 = LEN(table_title)
 
    ! Find the size of each character string in the array
    DO i = 1, nfields
       char_len_field_names(i) = LEN_TRIM(field_names(i))
    END DO
    
    max_char_size_field_names = LEN(field_names(1)) 

    errcode = h5tbmake_table_ptr_c(namelen1, table_title, loc_id, namelen, dset_name, nfields, nrecords,&
         type_size, field_offset, field_types, chunk_size, fill_data, compress, char_len_field_names, &
         max_char_size_field_names, field_names, data)

  END SUBROUTINE h5tbmake_table_ptr_f

  SUBROUTINE h5tbread_table_f(loc_id, table_name, nfields, dst_size, dst_offset, &
       dst_sizes, dst_buf, errcode)

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id                            ! An array containing the sizes of the fields
    CHARACTER(LEN=*), INTENT(in) :: table_name                        ! The name of the dataset to read
    INTEGER(hsize_t), INTENT(in) :: nfields                           ! number of fields
    INTEGER(size_t),  INTENT(in) :: dst_size                          ! The size of the structure type
    INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: dst_offset  ! An array containing the offsets of the fields
    INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: dst_sizes   ! An array containing the sizes of the fields
    TYPE(C_PTR)                                        :: dst_buf     ! Buffer with data !! do not use INTENT, causes NAG
                                                                      ! to segfault in C APIs
    INTEGER :: errcode                                                ! error code

    INTEGER(size_t) :: namelen                                        ! name length

    INTERFACE
       INTEGER FUNCTION h5tbread_table_c(loc_id,&
            table_name,&
            namelen,&
            nfields,&
            dst_size,&
            dst_offset, &
            dst_sizes, &
            dst_buf) &
            BIND(C,NAME='h5tbread_table_c')
         IMPORT :: C_PTR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                    ! file or group identifier
         CHARACTER(LEN=1), INTENT(in) :: table_name                ! name of the dataset
         INTEGER(hsize_t), INTENT(in) :: nfields 
         INTEGER(size_t),  INTENT(in) :: dst_size                  ! type size
         INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: dst_offset  ! An array containing the sizes of the fields
         INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: dst_sizes   ! An array containing the sizes of the fields
         INTEGER(size_t) :: namelen                                ! name length
         TYPE(C_PTR), VALUE :: dst_buf

       END FUNCTION h5tbread_table_c
    END INTERFACE
 
    namelen = LEN(table_name)

    errcode = h5tbread_table_c(loc_id,&
         table_name,&
         namelen, &
         nfields, &
         dst_size,&
         dst_offset, &
         dst_sizes, &
         dst_buf)


  END SUBROUTINE h5tbread_table_f

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

  SUBROUTINE h5tbwrite_field_name_f_int(loc_id,&
       dset_name,&
       field_name,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )
    
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
    INTEGER(hsize_t), INTENT(in) :: start                            ! start record
    INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
    INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf                         ! data buffer
    INTEGER :: errcode                                               ! error code
    INTEGER(size_t) :: namelen                                       ! name length
    INTEGER(size_t) :: namelen1                                      ! name length
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1))
    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)
    
    errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
         start,nrecords,type_size,f_ptr)
    
  END SUBROUTINE h5tbwrite_field_name_f_int


  SUBROUTINE h5tbwrite_field_name_f_string(loc_id,&
       dset_name,&
       field_name,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
    INTEGER(hsize_t), INTENT(in) :: start                            ! start record
    INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
    INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
    CHARACTER(LEN=*), INTENT(in), DIMENSION(*), TARGET :: buf        ! data buffer
    INTEGER :: errcode                                               ! error code
    INTEGER(size_t) :: namelen                                       ! name length
    INTEGER(size_t) :: namelen1
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1)(1:1))

    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)
    
    errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
         start,nrecords,type_size,f_ptr)
    
  END SUBROUTINE h5tbwrite_field_name_f_string


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

  SUBROUTINE h5tbread_field_name_f_int(loc_id,&
       dset_name,&
       field_name,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
    INTEGER(hsize_t), INTENT(in) :: start                            ! start record
    INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
    INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf                 ! data buffer
    INTEGER :: errcode                                               ! error code
    INTEGER(size_t) :: namelen                                       ! name length
    INTEGER(size_t) :: namelen1
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1))                                    ! name length

    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)
    
    errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
         start,nrecords,type_size,f_ptr)
    
  END SUBROUTINE h5tbread_field_name_f_int

  SUBROUTINE h5tbread_field_name_f_string(loc_id,&
       dset_name,&
       field_name,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
    INTEGER(hsize_t), INTENT(in) :: start                            ! start record
    INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
    INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
    CHARACTER(LEN=*), INTENT(in), DIMENSION(*), TARGET :: buf        ! data buffer
    INTEGER :: errcode                                               ! error code
    INTEGER(size_t) :: namelen                                       ! name length
    INTEGER(size_t) :: namelen1                                      ! name length
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1)(1:1)) 

    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)
    
    errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
         start,nrecords,type_size,f_ptr)
    
  END SUBROUTINE h5tbread_field_name_f_string


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
  
  SUBROUTINE h5tbwrite_field_index_f_int(loc_id,&
       dset_name,&
       field_index,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )
    
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    INTEGER, INTENT(in) :: field_index                               ! index
    INTEGER(hsize_t), INTENT(in) :: start                            ! start record
    INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
    INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf                 ! data buffer
    INTEGER :: errcode                                               ! error code
    INTEGER(size_t) :: namelen                                       ! name length
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1))

    namelen  = LEN(dset_name)
    
    errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
         start,nrecords,type_size,f_ptr)
    
  END SUBROUTINE h5tbwrite_field_index_f_int

  SUBROUTINE h5tbwrite_field_index_f_string(loc_id,&
       dset_name,&
       field_index,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )
    
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    INTEGER, INTENT(in) :: field_index                               ! index
    INTEGER(hsize_t), INTENT(in) :: start                            ! start record
    INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
    INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
    CHARACTER(LEN=*), INTENT(in), DIMENSION(*), TARGET :: buf                ! data buffer
    INTEGER :: errcode                                               ! error code
    INTEGER(size_t) :: namelen                                       ! name length
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1)(1:1))
    namelen  = LEN(dset_name)
    
    errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
         start,nrecords,type_size,f_ptr)
    
  END SUBROUTINE h5tbwrite_field_index_f_string


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

  SUBROUTINE h5tbread_field_index_f_int(loc_id,&
       dset_name,&
       field_index,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )
    
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    INTEGER, INTENT(in) :: field_index                               ! index
    INTEGER(hsize_t), INTENT(in) :: start                            ! start record
    INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
    INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf                 ! data buffer
    INTEGER :: errcode                                               ! error code
    INTEGER(size_t) :: namelen                                       ! name length
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1))
    namelen  = LEN(dset_name)
    
    errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
         start,nrecords,type_size,f_ptr)
    
  END SUBROUTINE h5tbread_field_index_f_int

  SUBROUTINE h5tbread_field_index_f_string(loc_id,&
       dset_name,&
       field_index,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    INTEGER, INTENT(in) :: field_index                               ! index
    INTEGER(hsize_t), INTENT(in) :: start                            ! start record
    INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
    INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
    CHARACTER(LEN=*), INTENT(in), DIMENSION(*), TARGET :: buf        ! data buffer
    INTEGER :: errcode                                               ! error code
    INTEGER(size_t) :: namelen                                       ! name length
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1)(1:1))
    namelen  = LEN(dset_name)
    
    errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
         start,nrecords,type_size,f_ptr)
    
  END SUBROUTINE h5tbread_field_index_f_string

!-------------------------------------------------------------------------
! Function: h5tbinsert_field_f
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

  SUBROUTINE h5tbinsert_field_f_int(loc_id,&
       dset_name,&
       field_name,&
       field_type,&
       field_index,&
       buf,&
       errcode )
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
    INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
    INTEGER, INTENT(in) :: field_index                               ! field_index
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf                 ! data buffer
    INTEGER(size_t) :: namelen                                       ! name length
    INTEGER(size_t) :: namelen1                                      ! name length
    INTEGER :: errcode                                               ! error code
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1))

    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)
    
    errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
         field_type,field_index,f_ptr)

  END SUBROUTINE h5tbinsert_field_f_int

  SUBROUTINE h5tbinsert_field_f_string(loc_id,&
       dset_name,&
       field_name,&
       field_type,&
       field_index,&
       buf,&
       errcode )
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
    INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
    INTEGER, INTENT(in) :: field_index                               ! field_index
    CHARACTER(LEN=*), INTENT(in), DIMENSION(*), TARGET :: buf        ! data buffer
    INTEGER(size_t) :: namelen                                       ! name length
    INTEGER(size_t) :: namelen1                                      ! name length
    INTEGER :: errcode                                               ! error code
    TYPE(C_PTR) :: f_ptr
    
    f_ptr = C_LOC(buf(1)(1:1))
    
    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)
    
    errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
         field_type,field_index,f_ptr)
    
  END SUBROUTINE h5tbinsert_field_f_string

!-------------------------------------------------------------------------
! Function: h5tbdelete_field_f
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

  SUBROUTINE h5tbdelete_field_f(loc_id,&
       dset_name,&
       field_name,&
       errcode )
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
    CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
    INTEGER(size_t) :: namelen                                       ! name length
    INTEGER(size_t) :: namelen1                                      ! name length
    INTEGER :: errcode                                               ! error code


    INTERFACE
       INTEGER FUNCTION h5tbdelete_field_c(loc_id,namelen,dset_name,namelen1,field_name) &
            BIND(C,NAME='h5tbdelete_field_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T),   INTENT(IN) :: loc_id                           ! file or group identifier
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: dset_name    ! name of the dataset
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: field_name   ! name of the field
         INTEGER(size_t) :: namelen                                       ! name length
         INTEGER(size_t) :: namelen1                                      ! name length length
       END FUNCTION h5tbdelete_field_c
    END INTERFACE

    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)

    errcode = h5tbdelete_field_c(loc_id,namelen,dset_name,namelen1,field_name)

  END SUBROUTINE h5tbdelete_field_f

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

  SUBROUTINE h5tbget_table_info_f(loc_id,&
       dset_name,&
       nfields,&
       nrecords,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), INTENT(inout):: nfields          ! nfields
    INTEGER(hsize_t), INTENT(inout):: nrecords         ! nrecords
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length

    INTERFACE
       INTEGER FUNCTION h5tbget_table_info_c(loc_id,namelen,dset_name,nfields,nrecords) &
            BIND(C,NAME='h5tbget_table_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name ! name of the dataset
         INTEGER(hsize_t), INTENT(inout):: nfields          ! nfields
         INTEGER(hsize_t), INTENT(inout):: nrecords         ! nrecords
         INTEGER(size_t) :: namelen                         ! name length
       END FUNCTION h5tbget_table_info_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5tbget_table_info_c(loc_id,namelen,dset_name,nfields,nrecords)
    
  END SUBROUTINE h5tbget_table_info_f
  

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
!  Added optional parameter for returning the maximum character length
!  in the field name array. March 3, 2011 
!
!-------------------------------------------------------------------------

  SUBROUTINE h5tbget_field_info_f(loc_id,&
       dset_name,&
       nfields,&
       field_names,&
       field_sizes,&
       field_offsets,&
       type_size,&
       errcode, maxlen_out )
    
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id                                ! file or group identifier
    CHARACTER(LEN=*), INTENT(in) :: dset_name                             ! name of the dataset
    INTEGER(hsize_t), INTENT(in) :: nfields                               ! nfields
    CHARACTER(LEN=*), DIMENSION(nfields), INTENT(inout) :: field_names    ! field names
    INTEGER(size_t),  DIMENSION(nfields), INTENT(inout) :: field_sizes    ! field sizes
    INTEGER(size_t),  DIMENSION(nfields), INTENT(inout) :: field_offsets  ! field offsets
    INTEGER(size_t),  INTENT(inout):: type_size                           ! type size
    INTEGER :: errcode                                                    ! error code
    INTEGER, OPTIONAL :: maxlen_out                                       ! maximum character len of the field names
    INTEGER(size_t) :: namelen                                            ! name length
    INTEGER(size_t), DIMENSION(nfields) :: namelen2                       ! name lengths
    INTEGER(hsize_t) :: i                                                          ! general purpose integer
    INTEGER(size_t) :: maxlen
    INTEGER(size_t) :: c_maxlen_out
    
    INTERFACE
       INTEGER FUNCTION h5tbget_field_info_c(loc_id,namelen,dset_name,nfields,&
            field_sizes,field_offsets,type_size,namelen2, maxlen, field_names, c_maxlen_out) &
            BIND(C,NAME='h5tbget_field_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                                 ! file or group identifier
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name          ! name of the dataset
         INTEGER(hsize_t), INTENT(in):: nfields                                 ! nfields
         CHARACTER(KIND=C_CHAR), DIMENSION(1:nfields), INTENT(inout) :: field_names   ! field names
         INTEGER(size_t),  DIMENSION(1:nfields), INTENT(inout) :: field_sizes   ! field sizes
         INTEGER(size_t),  DIMENSION(1:nfields), INTENT(inout) :: field_offsets ! field offsets
         INTEGER(size_t),  INTENT(inout):: type_size                            ! type size
         INTEGER(size_t) :: namelen                                             ! name length
         INTEGER(size_t) :: maxlen                                              ! maxiumum length of input field names
         INTEGER(size_t), DIMENSION(1:nfields) :: namelen2                      ! name lengths
         INTEGER(size_t) :: c_maxlen_out                  ! maximum character length of a field array element 
       END FUNCTION h5tbget_field_info_c
    END INTERFACE

    namelen = LEN(dset_name)
    DO i = 1, nfields
       namelen2(i) = LEN_TRIM(field_names(i))
    END DO
    maxlen = LEN(field_names(1))
    c_maxlen_out = 0
    
    errcode = h5tbget_field_info_c(loc_id, namelen,dset_name, nfields, &
         field_sizes, field_offsets, type_size, namelen2, maxlen, field_names, c_maxlen_out)
    
    IF(PRESENT(maxlen_out)) maxlen_out = c_maxlen_out
    
  END SUBROUTINE h5tbget_field_info_f
  
END MODULE H5TB_CONST






