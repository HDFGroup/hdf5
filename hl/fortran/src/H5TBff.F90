!> @defgroup FH5TB Fortran High Level Table (H5TB) Interface
!!
!! @see H5TB, C-HL API
!!
!! @see @ref H5TB_UG, User Guide
!!

!> @ingroup FH5TB
!!
!! @brief This module contains Fortran interfaces for H5TB.
!
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
#ifdef H5_DOXYGEN
   MODULE H5TB
#else
   MODULE H5TB_CONST
#endif

  USE, INTRINSIC :: ISO_C_BINDING
  USE h5fortran_types
  USE hdf5

  INTERFACE h5tbwrite_field_name_f
#ifdef H5_DOXYGEN
     MODULE PROCEDURE h5tbwrite_field_name_f
#else
     MODULE PROCEDURE h5tbwrite_field_name_f_int
     MODULE PROCEDURE h5tbwrite_field_name_f_string
#endif
  END INTERFACE

  INTERFACE h5tbread_field_name_f
#ifdef H5_DOXYGEN
     MODULE PROCEDURE h5tbread_field_name_f
#else
     MODULE PROCEDURE h5tbread_field_name_f_int
     MODULE PROCEDURE h5tbread_field_name_f_string
#endif
  END INTERFACE

  INTERFACE h5tbwrite_field_index_f
#ifdef H5_DOXYGEN
     MODULE PROCEDURE h5tbwrite_field_index_f
#else
     MODULE PROCEDURE h5tbwrite_field_index_f_int
     MODULE PROCEDURE h5tbwrite_field_index_f_string
#endif
  END INTERFACE

  INTERFACE h5tbread_field_index_f
#ifdef H5_DOXYGEN
     MODULE PROCEDURE h5tbread_field_index_f
#else
     MODULE PROCEDURE h5tbread_field_index_f_int
     MODULE PROCEDURE h5tbread_field_index_f_string
#endif
  END INTERFACE

  INTERFACE h5tbinsert_field_f
#ifdef H5_DOXYGEN
     MODULE PROCEDURE h5tbinsert_field_f
#else
     MODULE PROCEDURE h5tbinsert_field_f_int
     MODULE PROCEDURE h5tbinsert_field_f_string
#endif
  END INTERFACE


#ifndef H5_DOXYGEN

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
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name    ! name of the dataset
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: field_name   ! name of the field
       INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
       INTEGER, INTENT(in) :: field_index                               ! field_index
       TYPE(C_PTR), VALUE :: buf                                        ! data buffer
       INTEGER(size_t) :: namelen                                       ! name length
       INTEGER(size_t) :: namelen1                                      ! name length length
     END FUNCTION h5tbinsert_field_c
  END INTERFACE

#endif

CONTAINS

!>
!! \ingroup FH5TB
!!
!! \brief Creates (DOES NOT WRITE) a dataset named \p dset_name attached to the object specified by the identifier \p loc_id.
!!
!! \attention  \fortran_obsolete
!!
!! \param table_title   The title of the table.
!! \param loc_id        Location identifier. The identifier may be that of a file or group.
!! \param dset_name     The name of the dataset to create.
!! \param nfields       The number of fields.
!! \param nrecords      The number of records.
!! \param type_size     The size in bytes of the structure associated with the table. Obtained with sizeof or storage_size.
!! \param field_names   An array containing the names of the fields.
!! \param field_offset  An array containing the offsets of the fields.
!! \param field_types   An array containing the type of the fields.
!! \param chunk_size    The chunk size.
!! \param compress      Flag that turns compression on or off.
!! \param errcode       \fortran_error
!!
!! See C API: @ref H5TBmake_table()
!!
#ifdef H5_DOXYGEN
  SUBROUTINE h5tbmake_table_f(&
#else
  SUBROUTINE h5tbmake_table_f90(&
#endif
       table_title,&
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
    CHARACTER(LEN=*), INTENT(in) :: table_title
    INTEGER(hid_t)  , INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), INTENT(in) :: nfields
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t) , INTENT(in) :: type_size
    CHARACTER(LEN=*), DIMENSION(1:nfields), INTENT(in) :: field_names
    INTEGER(size_t) , DIMENSION(1:nfields), INTENT(in) :: field_offset
    INTEGER(hid_t),   DIMENSION(1:nfields), INTENT(in) :: field_types
    INTEGER(hsize_t), INTENT(in) :: chunk_size
    INTEGER,          INTENT(in) :: compress
    INTEGER :: errcode
    INTEGER(size_t) :: namelen                                       ! name length
    INTEGER(size_t) :: namelen1                                      ! name length
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
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: table_title
         INTEGER(hid_t),   INTENT(in) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
         INTEGER(hsize_t), INTENT(in) :: nfields
         INTEGER(hsize_t), INTENT(in) :: nrecords
         INTEGER(size_t),  INTENT(in) :: type_size
         CHARACTER(KIND=C_CHAR), DIMENSION(nfields), INTENT(in) :: field_names
         INTEGER(size_t),  DIMENSION(nfields), INTENT(in) :: field_offset
         INTEGER(hid_t),   DIMENSION(nfields), INTENT(in) :: field_types
         INTEGER(hsize_t), INTENT(in) :: chunk_size
         INTEGER,          INTENT(in) :: compress
         INTEGER(size_t) :: namelen
         INTEGER(size_t) :: namelen1
         INTEGER(size_t), DIMENSION(nfields) :: char_len_field_names
         INTEGER(size_t) :: max_char_size_field_names
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

#ifdef H5_DOXYGEN
  END SUBROUTINE h5tbmake_table_f
#else
  END SUBROUTINE h5tbmake_table_f90
#endif

!>
!! \ingroup FH5TB
!!
!! \brief Creates and writes a dataset named \p dset_name attached to the object specified by the identifier \p loc_id.
!!
!! \attention  \fortran_approved
!!
!! \param table_title   The title of the table
!! \param loc_id        Location identifier. The identifier may be that of a file or group.
!! \param dset_name     The name of the dataset to create
!! \param nfields       The number of fields
!! \param nrecords      The number of records
!! \param type_size     The size in bytes of the structure associated with the table; This value is obtained with sizeof().
!! \param field_names   An array containing the names of the fields
!! \param field_offset  An array containing the offsets of the fields
!! \param field_types   An array containing the type of the fields
!! \param chunk_size    The chunk size
!! \param fill_data     Fill values data
!! \param compress      Flag that turns compression on or off
!! \param data	        Buffer with data to be written to the table
!! \param errcode       \fortran_error
!!
!! See C API: @ref H5TBmake_table()
!!
#ifdef H5_DOXYGEN
  SUBROUTINE h5tbmake_table_f(&
#else
  SUBROUTINE h5tbmake_table_ptr_f(&
#endif
       table_title,&
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
    CHARACTER(LEN=*), INTENT(in) :: table_title
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), INTENT(in) :: nfields
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    CHARACTER(LEN=*), DIMENSION(1:nfields), INTENT(in) :: field_names
    INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: field_offset
    INTEGER(hid_t),   DIMENSION(1:nfields), INTENT(in) :: field_types
    INTEGER :: errcode
    INTEGER(hsize_t), INTENT(in) :: chunk_size
    TYPE(C_PTR), INTENT(in) :: fill_data
    INTEGER,          INTENT(in) :: compress
    TYPE(C_PTR), INTENT(in) :: data
    INTEGER(size_t) :: namelen                                       ! name length
    INTEGER(size_t) :: namelen1                                      ! name length
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
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: table_title
         INTEGER(hid_t),   INTENT(in) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
         INTEGER(hsize_t), INTENT(in) :: nfields
         INTEGER(hsize_t), INTENT(in) :: nrecords
         INTEGER(size_t),  INTENT(in) :: type_size
         CHARACTER(KIND=C_CHAR), DIMENSION(nfields), INTENT(in) :: field_names
         INTEGER(size_t),  DIMENSION(nfields), INTENT(in) :: field_offset
         INTEGER(hid_t),   DIMENSION(nfields), INTENT(in) :: field_types
         INTEGER(hsize_t), INTENT(in) :: chunk_size
         TYPE(C_PTR), INTENT(in), VALUE :: fill_data
         INTEGER,          INTENT(in) :: compress
         INTEGER(size_t) :: namelen
         INTEGER(size_t) :: namelen1
         INTEGER(size_t), DIMENSION(nfields) :: char_len_field_names
         INTEGER(size_t) :: max_char_size_field_names
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

#ifdef H5_DOXYGEN
   END SUBROUTINE h5tbmake_table_f
#else
   END SUBROUTINE h5tbmake_table_ptr_f
#endif
!>
!! \ingroup FH5TB
!!
!! \brief Reads a table.
!!
!! \param loc_id     Location identifier. The identifier may be that of a file or group.
!! \param dset_name  The name of the dataset to read
!! \param nfields    Number of fields, i.e., size of dst_offset and dst_sizes arrays.
!! \param dst_size   The size of the structure type, as calculated by sizeof or storage_size
!! \param dst_offset An array containing the offsets of the fields. These offsets can be calculated with H5OFFSETOF.
!! \param dst_sizes  An array containing the sizes of the fields. These sizes can be calculated with sizeof or storage_size.
!! \param dst_buf    Pointer to buffer with data.
!! \param errcode    \fortran_error
!!
!! See C API: @ref H5TBread_table()
!!
  SUBROUTINE h5tbread_table_f(loc_id, dset_name, nfields, dst_size, dst_offset, &
       dst_sizes, dst_buf, errcode)

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), INTENT(in) :: nfields
    INTEGER(size_t),  INTENT(in) :: dst_size
    INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: dst_offset
    INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: dst_sizes
    TYPE(C_PTR)                                        :: dst_buf !!! do not use INTENT, causes NAG to segfault in C APIs
    INTEGER :: errcode

    INTEGER(size_t) :: namelen  ! name length

    INTERFACE
       INTEGER FUNCTION h5tbread_table_c(loc_id,&
            dset_name,&
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
         INTEGER(hid_t),   INTENT(in) :: loc_id
         CHARACTER(LEN=1), INTENT(in) :: dset_name
         INTEGER(hsize_t), INTENT(in) :: nfields
         INTEGER(size_t),  INTENT(in) :: dst_size
         INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: dst_offset
         INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: dst_sizes
         INTEGER(size_t) :: namelen
         TYPE(C_PTR), VALUE :: dst_buf

       END FUNCTION h5tbread_table_c
    END INTERFACE

    namelen = LEN(dset_name)

    errcode = h5tbread_table_c(loc_id,&
         dset_name,&
         namelen, &
         nfields, &
         dst_size,&
         dst_offset, &
         dst_sizes, &
         dst_buf)

  END SUBROUTINE h5tbread_table_f

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5TB
!!
!! \brief Overwrites field.
!!
!! \param loc_id      Location identifier. The identifier may be that of a file or group.
!! \param dset_name   The name of the dataset to overwrite
!! \param field_name  The names of the fields to write
!! \param start       The zero index record to start writing
!! \param nrecords    The number of records to write
!! \param type_size   The size of the structure type, as calculated by sizeof or storage_size.
!! \param buf         Buffer with data.
!! \param errcode     \fortran_error
!!
!! See similar C API: @ref H5TBwrite_fields_name()
!!
  SUBROUTINE h5tbwrite_field_name_f(&
#else
  SUBROUTINE h5tbwrite_field_name_f_int(&
#endif
       loc_id,&
       dset_name,&
       field_name,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
#ifdef H5_DOXYGEN
    TYPE(TYPE), INTENT(in), DIMENSION(*) :: buf
#else
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf
#endif
    INTEGER :: errcode
    INTEGER(size_t) :: namelen  ! name length
    INTEGER(size_t) :: namelen1 ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))
    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)

    errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
         start,nrecords,type_size,f_ptr)
#ifdef H5_DOXYGEN
  END SUBROUTINE h5tbwrite_field_name_f
#else
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
#endif

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5TB
!!
!! \brief Reads one or several fields. The fields are identified by name.
!!
!! \param loc_id      Location identifier. The identifier may be that of a file or group.
!! \param dset_name   The name of the dataset to read.
!! \param field_name  An array containing the names of the fields to read.
!! \param start       The start record to read from.
!! \param nrecords    The number of records to read.
!! \param type_size   The size in bytes of the structure associated with the table.  Obtained with sizeof or storage_size.
!! \param buf         Buffer with data
!! \param errcode     \fortran_error
!!
!! See similar C API: @ref H5TBread_fields_name()
!!
  SUBROUTINE h5tbread_field_name_f(&
#else
  SUBROUTINE h5tbread_field_name_f_int(&
#endif
       loc_id,&
       dset_name,&
       field_name,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
#ifdef H5_DOXYGEN
    TYPE(TYPE), INTENT(INOUT), DIMENSION(*):: buf
#else
    INTEGER, INTENT(INOUT), DIMENSION(*), TARGET :: buf
#endif
    INTEGER :: errcode
    INTEGER(size_t) :: namelen   ! name length
    INTEGER(size_t) :: namelen1  ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)

    errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
         start,nrecords,type_size,f_ptr)

#ifdef H5_DOXYGEN
  END SUBROUTINE h5tbread_field_name_f
#else
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
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode
    INTEGER(size_t) :: namelen  ! name length
    INTEGER(size_t) :: namelen1 ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1)(1:1))

    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)

    errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,&
         start,nrecords,type_size,f_ptr)

  END SUBROUTINE h5tbread_field_name_f_string
#endif

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5TB
!!
!! \brief Overwrites a field.
!!
!! \param loc_id       Location identifier. The identifier may be that of a file or group.
!! \param dset_name    The name of the dataset to overwrite.
!! \param field_index  The indexe of the fields to write.
!! \param start        The zero based index record to start writing.
!! \param nrecords     The number of records to write.
!! \param type_size    The size of the structure type, as calculated by sizeof or storage_size.
!! \param buf	       Buffer with data.
!! \param errcode      \fortran_error
!!
!! See similar C API: @ref H5TBwrite_fields_index()
!!
  SUBROUTINE h5tbwrite_field_index_f(&
#else
  SUBROUTINE h5tbwrite_field_index_f_int(&
#endif
       loc_id,&
       dset_name,&
       field_index,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
#ifdef H5_DOXYGEN
    INTEGER, INTENT(in), DIMENSION(*) :: buf
#else
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf
#endif
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen  = LEN(dset_name)

    errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
         start,nrecords,type_size,f_ptr)

#ifdef H5_DOXYGEN
  END SUBROUTINE h5tbwrite_field_index_f
#else
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
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    CHARACTER(LEN=*), INTENT(in), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1)(1:1))
    namelen  = LEN(dset_name)

    errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,&
         start,nrecords,type_size,f_ptr)

  END SUBROUTINE h5tbwrite_field_index_f_string
#endif

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5TB
!!
!! \brief Reads field. The fields are identified by index.
!!
!! \param loc_id       Location identifier. The identifier may be that of a file or group.
!! \param dset_name    The name of the dataset to read.
!! \param field_index  The indexes of the fields to read.
!! \param start        The start record to read from.
!! \param nrecords     The number of records to read.
!! \param type_size    The size in bytes of the structure associated with the table. Obtained with sizeof or storage_size.
!! \param buf          Buffer with data.
!! \param errcode      \fortran_error
!!
!! See similar C API: @ref H5TBread_fields_index()
!!
  SUBROUTINE h5tbread_field_index_f(&
#else
  SUBROUTINE h5tbread_field_index_f_int(&
#endif
       loc_id,&
       dset_name,&
       field_index,&
       start,&
       nrecords,&
       type_size,&
       buf,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
#ifdef H5_DOXYGEN
    TYPE(TYPE), INTENT(INOUT), DIMENSION(*) :: buf
#else
    INTEGER, INTENT(INOUT), DIMENSION(*), TARGET :: buf
#endif
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))
    namelen  = LEN(dset_name)

    errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
         start,nrecords,type_size,f_ptr)
#ifdef H5_DOXYGEN
  END SUBROUTINE h5tbread_field_index_f
#else
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
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode
    INTEGER(size_t) :: namelen
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1)(1:1))
    namelen  = LEN(dset_name)

    errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,&
         start,nrecords,type_size,f_ptr)

  END SUBROUTINE h5tbread_field_index_f_string
#endif

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5TB
!!
!! \brief Insert a new field into a table.
!!
!! \param loc_id     Location identifier. The identifier may be that of a file or group.
!! \param dset_name  The name of the table.
!! \param field_name The name of the field to insert.
!! \param field_type The data type of the field.
!! \param position   The zero based index position where to insert the field.
!! \param buf	     Buffer with data.
!! \param errcode    \fortran_error
!!
!! See C API: @ref H5TBinsert_field()
!!
  SUBROUTINE h5tbinsert_field_f(&
#else
  SUBROUTINE h5tbinsert_field_f_int(&
#endif
       loc_id,&
       dset_name,&
       field_name,&
       field_type,&
       position,&
       buf,&
       errcode )
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hid_t), INTENT(in)   :: field_type
    INTEGER, INTENT(in) :: position
#ifdef H5_DOXYGEN
    TYPE(TYPE), INTENT(in), DIMENSION(*) :: buf
#else
    INTEGER, INTENT(in), DIMENSION(*), TARGET :: buf
#endif
    INTEGER :: errcode

    INTEGER(size_t) :: namelen  ! name length
    INTEGER(size_t) :: namelen1 ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1))

    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)

    errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
         field_type,position,f_ptr)

#ifdef H5_DOXYGEN
  END SUBROUTINE h5tbinsert_field_f
#else
  END SUBROUTINE h5tbinsert_field_f_int

  SUBROUTINE h5tbinsert_field_f_string(loc_id,&
       dset_name,&
       field_name,&
       field_type,&
       position,&
       buf,&
       errcode )
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hid_t), INTENT(in)   :: field_type
    INTEGER, INTENT(in) :: position
    CHARACTER(LEN=*), INTENT(in), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode

    INTEGER(size_t) :: namelen   ! name length
    INTEGER(size_t) :: namelen1  ! name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1)(1:1))

    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)

    errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,&
         field_type,position,f_ptr)

  END SUBROUTINE h5tbinsert_field_f_string
#endif

!>
!! \ingroup FH5TB
!!
!! \brief Deletes a field from a table.
!!
!! \param loc_id	Location identifier. The identifier may be that of a file or group.
!! \param dset_name	The name of the table.
!! \param field_name	The name of the field to delete.
!! \param errcode       \fortran_error
!!
!! See C API: @ref H5TBdelete_field()
!!
  SUBROUTINE h5tbdelete_field_f(loc_id,&
       dset_name,&
       field_name,&
       errcode )
    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER :: errcode

    INTEGER(size_t) :: namelen  ! name length
    INTEGER(size_t) :: namelen1 ! name length


    INTERFACE
       INTEGER FUNCTION h5tbdelete_field_c(loc_id,namelen,dset_name,namelen1,field_name) &
            BIND(C,NAME='h5tbdelete_field_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T),   INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: dset_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: field_name
         INTEGER(size_t) :: namelen
         INTEGER(size_t) :: namelen1
       END FUNCTION h5tbdelete_field_c
    END INTERFACE

    namelen  = LEN(dset_name)
    namelen1 = LEN(field_name)

    errcode = h5tbdelete_field_c(loc_id,namelen,dset_name,namelen1,field_name)

  END SUBROUTINE h5tbdelete_field_f
!>
!! \ingroup FH5TB
!!
!! \brief Gets the table dimensions.
!!
!! \param loc_id    Location identifier. The identifier may be that of a file or group.
!! \param dset_name The name of the dataset to read.
!! \param nfields   The number of fields.
!! \param nrecords  The number of records.
!! \param errcode   \fortran_error
!!
!! See C API: @ref H5TBget_table_info()
!!
  SUBROUTINE h5tbget_table_info_f(loc_id,&
       dset_name,&
       nfields,&
       nrecords,&
       errcode )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), INTENT(inout):: nfields
    INTEGER(hsize_t), INTENT(inout):: nrecords
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length

    INTERFACE
       INTEGER FUNCTION h5tbget_table_info_c(loc_id,namelen,dset_name,nfields,nrecords) &
            BIND(C,NAME='h5tbget_table_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
         INTEGER(hsize_t), INTENT(inout):: nfields
         INTEGER(hsize_t), INTENT(inout):: nrecords
         INTEGER(size_t) :: namelen
       END FUNCTION h5tbget_table_info_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5tbget_table_info_c(loc_id,namelen,dset_name,nfields,nrecords)

  END SUBROUTINE h5tbget_table_info_f

!>
!! \ingroup FH5TB
!!
!! \brief Gets information about a table.
!!
!! \param loc_id        Location identifier. The identifier may be that of a file or group.
!! \param dset_name     The name of the dataset to read.
!! \param nfields       The number of fields.
!! \param field_names   An array containing the names of the fields.
!! \param field_sizes   An array containing the size of the fields.
!! \param field_offsets	An array containing the offsets of the fields.
!! \param type_size	The size of the HDF5 datatype associated with the table
!!                      (i.e., the size in bytes of the HDF5 compound datatype used to define a row, or record, in the table).
!! \param errcode       \fortran_error
!! \param maxlen_out    Maximum character length of the field names.
!!
!! See C API: @ref H5TBget_field_info()
!!
  SUBROUTINE h5tbget_field_info_f(loc_id,&
       dset_name,&
       nfields,&
       field_names,&
       field_sizes,&
       field_offsets,&
       type_size,&
       errcode, maxlen_out )

    IMPLICIT NONE
    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(LEN=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), INTENT(in) :: nfields
    CHARACTER(LEN=*), DIMENSION(nfields), INTENT(inout) :: field_names
    INTEGER(size_t),  DIMENSION(nfields), INTENT(inout) :: field_sizes
    INTEGER(size_t),  DIMENSION(nfields), INTENT(inout) :: field_offsets
    INTEGER(size_t),  INTENT(inout):: type_size
    INTEGER :: errcode
    INTEGER(size_t), OPTIONAL :: maxlen_out
    INTEGER(size_t) :: namelen                       ! name length
    INTEGER(size_t), DIMENSION(nfields) :: namelen2  ! name lengths
    INTEGER(hsize_t) :: i
    INTEGER(size_t) :: maxlen
    INTEGER(size_t) :: c_maxlen_out

    INTERFACE
       INTEGER FUNCTION h5tbget_field_info_c(loc_id,namelen,dset_name,nfields,&
            field_sizes,field_offsets,type_size,namelen2, maxlen, field_names, c_maxlen_out) &
            BIND(C,NAME='h5tbget_field_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
         INTEGER(hsize_t), INTENT(in):: nfields
         CHARACTER(KIND=C_CHAR), DIMENSION(1:nfields), INTENT(inout) :: field_names
         INTEGER(size_t),  DIMENSION(1:nfields), INTENT(inout) :: field_sizes
         INTEGER(size_t),  DIMENSION(1:nfields), INTENT(inout) :: field_offsets
         INTEGER(size_t),  INTENT(inout):: type_size
         INTEGER(size_t) :: namelen
         INTEGER(size_t) :: maxlen
         INTEGER(size_t), DIMENSION(1:nfields) :: namelen2
         INTEGER(size_t) :: c_maxlen_out
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

#ifdef H5_DOXYGEN
END MODULE H5TB
#else
END MODULE H5TB_CONST
#endif






