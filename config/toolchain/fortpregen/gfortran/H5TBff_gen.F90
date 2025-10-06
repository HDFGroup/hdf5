!****h* ROBODoc/H5TBff_gen.F90
!
! NAME
!  H5TBff_gen
! 
! PURPOSE
!  This module is generated at build by H5HL_buildiface.F90 to handle all the
!  detected REAL/INTEGER KINDs for APIs being passed those KINDs. Currently these 
!  are H5LT and H5TB APIs
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the LICENSE file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! AUTHOR
!   H5HL_buildiface.F90
!
!*****

MODULE H5TB
  USE, INTRINSIC :: ISO_C_BINDING
  USE h5fortran_types
  USE H5TB_CONST
  IMPLICIT NONE
  INTERFACE h5tbwrite_field_name_f
     MODULE PROCEDURE h5tbwrite_field_name_kind_4_rank_1
     MODULE PROCEDURE h5tbwrite_field_name_kind_8_rank_1
     MODULE PROCEDURE h5tbwrite_field_name_kind_10_rank_1
     MODULE PROCEDURE h5tbwrite_field_name_kind_16_rank_1
  END INTERFACE
  INTERFACE h5tbread_field_name_f
     MODULE PROCEDURE h5tbread_field_name_kind_4_rank_1
     MODULE PROCEDURE h5tbread_field_name_kind_8_rank_1
     MODULE PROCEDURE h5tbread_field_name_kind_10_rank_1
     MODULE PROCEDURE h5tbread_field_name_kind_16_rank_1
  END INTERFACE
  INTERFACE h5tbwrite_field_index_f
     MODULE PROCEDURE h5tbwrite_field_index_kind_4_rank_1
     MODULE PROCEDURE h5tbwrite_field_index_kind_8_rank_1
     MODULE PROCEDURE h5tbwrite_field_index_kind_10_rank_1
     MODULE PROCEDURE h5tbwrite_field_index_kind_16_rank_1
  END INTERFACE
  INTERFACE h5tbread_field_index_f
     MODULE PROCEDURE h5tbread_field_index_kind_4_rank_1
     MODULE PROCEDURE h5tbread_field_index_kind_8_rank_1
     MODULE PROCEDURE h5tbread_field_index_kind_10_rank_1
     MODULE PROCEDURE h5tbread_field_index_kind_16_rank_1
  END INTERFACE
  INTERFACE h5tbinsert_field_f
     MODULE PROCEDURE h5tbinsert_field_kind_4_rank_1
     MODULE PROCEDURE h5tbinsert_field_kind_8_rank_1
     MODULE PROCEDURE h5tbinsert_field_kind_10_rank_1
     MODULE PROCEDURE h5tbinsert_field_kind_16_rank_1
  END INTERFACE
CONTAINS
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_name_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5tbwrite_field_name_kind_4_rank_1(loc_id,dset_name,field_name,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=4),INTENT(IN), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    INTEGER(size_t) :: namelen1
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbwrite_field_name_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_name_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5tbwrite_field_name_kind_8_rank_1(loc_id,dset_name,field_name,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=8),INTENT(IN), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    INTEGER(size_t) :: namelen1
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbwrite_field_name_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_name_kind_10_rank_1
!DEC$endif
  SUBROUTINE h5tbwrite_field_name_kind_10_rank_1(loc_id,dset_name,field_name,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=10),INTENT(IN), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    INTEGER(size_t) :: namelen1
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbwrite_field_name_kind_10_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_name_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5tbwrite_field_name_kind_16_rank_1(loc_id,dset_name,field_name,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=16),INTENT(IN), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    INTEGER(size_t) :: namelen1
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbwrite_field_name_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbread_field_name_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5tbread_field_name_kind_4_rank_1(loc_id,dset_name,field_name,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=4),INTENT(INOUT), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    INTEGER(size_t) :: namelen1
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbread_field_name_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbread_field_name_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5tbread_field_name_kind_8_rank_1(loc_id,dset_name,field_name,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=8),INTENT(INOUT), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    INTEGER(size_t) :: namelen1
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbread_field_name_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbread_field_name_kind_10_rank_1
!DEC$endif
  SUBROUTINE h5tbread_field_name_kind_10_rank_1(loc_id,dset_name,field_name,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=10),INTENT(INOUT), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    INTEGER(size_t) :: namelen1
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbread_field_name_kind_10_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbread_field_name_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5tbread_field_name_kind_16_rank_1(loc_id,dset_name,field_name,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    CHARACTER(LEN=*), INTENT(in) :: field_name
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=16),INTENT(INOUT), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    INTEGER(size_t) :: namelen1
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbread_field_name_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_index_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5tbwrite_field_index_kind_4_rank_1(loc_id,dset_name,field_index,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=4),INTENT(IN), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbwrite_field_index_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_index_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5tbwrite_field_index_kind_8_rank_1(loc_id,dset_name,field_index,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=8),INTENT(IN), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbwrite_field_index_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_index_kind_10_rank_1
!DEC$endif
  SUBROUTINE h5tbwrite_field_index_kind_10_rank_1(loc_id,dset_name,field_index,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=10),INTENT(IN), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbwrite_field_index_kind_10_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_index_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5tbwrite_field_index_kind_16_rank_1(loc_id,dset_name,field_index,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=16),INTENT(IN), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbwrite_field_index_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbread_field_index_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5tbread_field_index_kind_4_rank_1(loc_id,dset_name,field_index,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=4),INTENT(INOUT), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbread_field_index_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbread_field_index_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5tbread_field_index_kind_8_rank_1(loc_id,dset_name,field_index,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=8),INTENT(INOUT), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbread_field_index_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbread_field_index_kind_10_rank_1
!DEC$endif
  SUBROUTINE h5tbread_field_index_kind_10_rank_1(loc_id,dset_name,field_index,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=10),INTENT(INOUT), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbread_field_index_kind_10_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbread_field_index_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5tbread_field_index_kind_16_rank_1(loc_id,dset_name,field_index,start, nrecords,type_size,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER, INTENT(in) :: field_index
    INTEGER(hsize_t), INTENT(in) :: start
    INTEGER(hsize_t), INTENT(in) :: nrecords
    INTEGER(size_t),  INTENT(in) :: type_size
    REAL(KIND=16),INTENT(INOUT), DIMENSION(*), TARGET :: buf
    INTEGER :: errcode 
    INTEGER(size_t) :: namelen
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,start,nrecords,type_size,f_ptr)
  END SUBROUTINE h5tbread_field_index_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbinsert_field_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5tbinsert_field_kind_4_rank_1(loc_id,dset_name,field_name,field_type,field_index,buf,errcode)
    IMPLICIT NONE
        INTEGER(hid_t),   INTENT(in) :: loc_id
        CHARACTER(LEN=*), INTENT(in) :: dset_name
        CHARACTER(LEN=*), INTENT(in) :: field_name
        INTEGER(hid_t), INTENT(in)   :: field_type
        INTEGER, INTENT(in) :: field_index
        REAL(KIND=4), INTENT(IN), DIMENSION(*), TARGET :: buf
        INTEGER(size_t) :: namelen
        INTEGER(size_t) :: namelen1
        INTEGER :: errcode
        TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,field_type,field_index,f_ptr)
  END SUBROUTINE h5tbinsert_field_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbinsert_field_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5tbinsert_field_kind_8_rank_1(loc_id,dset_name,field_name,field_type,field_index,buf,errcode)
    IMPLICIT NONE
        INTEGER(hid_t),   INTENT(in) :: loc_id
        CHARACTER(LEN=*), INTENT(in) :: dset_name
        CHARACTER(LEN=*), INTENT(in) :: field_name
        INTEGER(hid_t), INTENT(in)   :: field_type
        INTEGER, INTENT(in) :: field_index
        REAL(KIND=8), INTENT(IN), DIMENSION(*), TARGET :: buf
        INTEGER(size_t) :: namelen
        INTEGER(size_t) :: namelen1
        INTEGER :: errcode
        TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,field_type,field_index,f_ptr)
  END SUBROUTINE h5tbinsert_field_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbinsert_field_kind_10_rank_1
!DEC$endif
  SUBROUTINE h5tbinsert_field_kind_10_rank_1(loc_id,dset_name,field_name,field_type,field_index,buf,errcode)
    IMPLICIT NONE
        INTEGER(hid_t),   INTENT(in) :: loc_id
        CHARACTER(LEN=*), INTENT(in) :: dset_name
        CHARACTER(LEN=*), INTENT(in) :: field_name
        INTEGER(hid_t), INTENT(in)   :: field_type
        INTEGER, INTENT(in) :: field_index
        REAL(KIND=10), INTENT(IN), DIMENSION(*), TARGET :: buf
        INTEGER(size_t) :: namelen
        INTEGER(size_t) :: namelen1
        INTEGER :: errcode
        TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,field_type,field_index,f_ptr)
  END SUBROUTINE h5tbinsert_field_kind_10_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5tbinsert_field_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5tbinsert_field_kind_16_rank_1(loc_id,dset_name,field_name,field_type,field_index,buf,errcode)
    IMPLICIT NONE
        INTEGER(hid_t),   INTENT(in) :: loc_id
        CHARACTER(LEN=*), INTENT(in) :: dset_name
        CHARACTER(LEN=*), INTENT(in) :: field_name
        INTEGER(hid_t), INTENT(in)   :: field_type
        INTEGER, INTENT(in) :: field_index
        REAL(KIND=16), INTENT(IN), DIMENSION(*), TARGET :: buf
        INTEGER(size_t) :: namelen
        INTEGER(size_t) :: namelen1
        INTEGER :: errcode
        TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    namelen1 = LEN(field_name)
    errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,field_type,field_index,f_ptr)
  END SUBROUTINE h5tbinsert_field_kind_16_rank_1
END MODULE H5TB
