!****p* Program/H5HL_buildiface
!
! NAME
!  Executable: H5HL_buildiface
!
! FILE
!  fortran/src/H5HL_buildiface.f90
!
! PURPOSE
!  This stand alone program is used at build time to generate the program
!  H5HL_gen.f90. It cycles through all the available KIND parameters for
!  integers and reals. The appropriate program and subroutines are then generated
!  depending on which of the KIND values are found.
!
! NOTES
!  This program uses the Fortran 2008 intrinsic function STORAGE_SIZE or SIZEOF 
!  depending on availablity.It generates code that makes use of 
!  STORAGE_SIZE/SIZEOF in H5fortran_detect.f90. STORAGE_SIZE is standard
!  compliant and should always be chosen over SIZEOF.
!
!  The availability of STORAGE_SIZE/SIZEOF is checked at configure time and the TRUE/FALSE
!  condition is set in the configure variable "FORTRAN_HAVE_STORAGE_SIZE" or
!  "FORTRAN_HAVE_SIZEOF".
!
!  The use of C_SIZOF(X) is not used since the argument X must be an interoperable
!  data entity.
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  Copyright by The HDF Group.                                                 *
!  Copyright by the Board of Trustees of the University of Illinois.           *
!  All rights reserved.                                                        *
!                                                                              *
!  This file is part of HDF5.  The full HDF5 copyright notice, including       *
!  terms governing use, modification, and redistribution, is contained in      *
!  the files COPYING and Copyright.html.  COPYING can be found at the root     *
!  of the source code distribution tree; Copyright.html can be found at the    *
!  root level of an installed copy of the electronic HDF5 document set and     *
!  is linked from the top-level documents page.  It can also be found at       *
!  http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have            *
!  access to either file, you may request a copy from help@hdfgroup.org.       *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! AUTHOR
!  M. Scot Breitenfeld
!
!*****

#include <H5config_f.inc>

PROGRAM H5HL_buildiface
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

! These values are valid REAL KINDs (with corresponding C float) found during configure
  H5_H5CONFIG_F_NUM_RKIND
  H5_H5CONFIG_F_RKIND
! These values are valid INTEGER KINDs (with corresponding C float) found during configure
  H5_H5CONFIG_F_NUM_IKIND
  H5_H5CONFIG_F_IKIND

  INTEGER :: i, j, k
  CHARACTER(LEN=2) :: chr2
! subroutine rank of array being passed in
  CHARACTER(LEN=2), DIMENSION(1:8), PARAMETER :: chr_rank=(/"_0","_1","_2","_3","_4","_5","_6","_7"/)
! rank definitions
  CHARACTER(LEN=70), DIMENSION(1:8), PARAMETER :: rank_dim_line=(/ &
       '                                                                    ', &
       ', DIMENSION(dims(1))                                                ', &
       ', DIMENSION(dims(1),dims(2))                                        ', &
       ', DIMENSION(dims(1),dims(2),dims(3))                                ', &
       ', DIMENSION(dims(1),dims(2),dims(3),dims(4))                        ', &
       ', DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5))                ', &
       ', DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6))        ', &
       ', DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7))' &
            /)
! pointer to the buffer
  CHARACTER(LEN=37), DIMENSION(1:8), PARAMETER :: f_ptr_line=(/ &
       '    f_ptr = C_LOC(buf               )', &
       '    f_ptr = C_LOC(buf(1)            )', &
       '    f_ptr = C_LOC(buf(1,1)          )', &
       '    f_ptr = C_LOC(buf(1,1,1)        )', &
       '    f_ptr = C_LOC(buf(1,1,1,1)      )', &
       '    f_ptr = C_LOC(buf(1,1,1,1,1)    )', &
       '    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )', &
       '    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))' &
            /)

! Generate Fortran H5LT* interfaces having multiple KIND interfaces.
!
! Developer's notes:
!
! Only interfaces with arrays of rank 7 and less are provided. Even-though the F2008 
! standard extended the maximum rank to 15, it was decided that they should use the
! new APIs to handle those use cases. Handling rank 7 and less is for backward compatibility
! with the Fortran 90/95 APIs codes which could never handle rank 8-15 array sizes.

  OPEN(11,FILE='H5LTff_gen.F90')
  WRITE(11,'(40(A,/))') &
'!****h* ROBODoc/H5LTff_gen.F90',&
'!',&
'! NAME',&
'!  H5LTff_gen',&
'! ',&
'! PURPOSE',&
'!  This module is generated at build by H5HL_buildiface.F90 to handle all the',&
'!  detected REAL/INTEGER KINDs for APIs being passed those KINDs. Currently these ',&
'!  are H5LT and H5TB APIs',&
'!',&
'! COPYRIGHT',&
'! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *',&
'!   Copyright by The HDF Group.                                               *',&
'!   All rights reserved.                                                      *',&
'!                                                                             *',&
'!   This file is part of HDF5.  The full HDF5 copyright notice, including     *',&
'!   terms governing use, modification, and redistribution, is contained in    *',&
'!   the files COPYING and Copyright.html.  COPYING can be found at the root   *',&
'!   of the source code distribution tree; Copyright.html can be found at the  *',&
'!   root level of an installed copy of the electronic HDF5 document set and   *',&
'!   is linked from the top-level documents page.  It can also be found at     *',&
'!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *',&
'!   access to either file, you may request a copy from help@hdfgroup.org.     *',&
'! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *',&
'!',&
'! AUTHOR',&
'!   H5HL_buildiface.F90',&
'!',&
'!*****'

  WRITE(11,'(a)') "MODULE H5LT"

  WRITE(11,'(A)') '  USE, INTRINSIC :: ISO_C_BINDING'
  WRITE(11,'(A)') '  USE h5fortran_types'
  WRITE(11,'(A)') '  USE H5LT_CONST'
  WRITE(11,'(A)') '  IMPLICIT NONE'
!***************
! H5LT INTERFACES
!***************
!
! H5LTmake_dataset_f
!
  WRITE(11,'(A)') "  INTERFACE h5ltmake_dataset_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(A)') "     MODULE PROCEDURE h5ltmake_dataset_real_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(A)') "     MODULE PROCEDURE h5ltmake_dataset_integer_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

!  h5ltread_dataset_f
  WRITE(11,'(A)') "  INTERFACE h5ltread_dataset_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(A)') "     MODULE PROCEDURE h5ltread_dataset_real_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(A)') "     MODULE PROCEDURE h5ltread_dataset_integer_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

! h5ltread_dataset_int_f
  WRITE(11,'(A)') "  INTERFACE h5ltread_dataset_int_f"
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(A)') "     MODULE PROCEDURE h5ltread_dataset_int_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

!  h5ltmake_dataset_int_f
  WRITE(11,'(A)') "  INTERFACE h5ltmake_dataset_int_f"
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(A)') "     MODULE PROCEDURE h5ltmake_dataset_int_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

!  h5ltmake_dataset_float_f
  WRITE(11,'(A)') "  INTERFACE h5ltmake_dataset_float_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(A)') "     MODULE PROCEDURE h5ltmake_dataset_float_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

!  h5ltmake_dataset_double_f
  WRITE(11,'(A)') "  INTERFACE h5ltmake_dataset_double_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(A)') "     MODULE PROCEDURE h5ltmake_dataset_double_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

!  h5ltread_dataset_float_f
  WRITE(11,'(A)') "  INTERFACE h5ltread_dataset_float_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(A)') "     MODULE PROCEDURE h5ltread_dataset_float_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

!  h5ltread_dataset_double_f
  WRITE(11,'(A)') "  INTERFACE h5ltread_dataset_double_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(A)') "     MODULE PROCEDURE h5ltread_dataset_double_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

  WRITE(11,'(A)') 'CONTAINS'

!**********************
! H5LT APIs
!**********************
!
! h5ltmake_dataset_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8

! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5ltmake_dataset_real_kind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(loc_id,dset_name,rank,dims,type_id,buf,errcode)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
        WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
        WRITE(11,'(A)') '    INTEGER,          INTENT(IN) :: rank'
        WRITE(11,'(A)') '    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims'
        WRITE(11,'(A)') '    INTEGER(hid_t),   INTENT(in) :: type_id'
        WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER :: errcode '
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    INTEGER(size_t) :: namelen'
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    namelen = LEN(dset_name)'
        WRITE(11,'(A)') '    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5ltmake_dataset_real_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO

! h5ltread_dataset_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8

! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5ltread_dataset_real_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5ltread_dataset_real_kind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(loc_id,dset_name,type_id,buf,dims,errcode)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
        WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
        WRITE(11,'(A)') '    INTEGER(hid_t),   INTENT(in) :: type_id'
        WRITE(11,'(A)') '    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims'
        WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER :: errcode '
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    INTEGER(size_t) :: namelen'
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    namelen = LEN(dset_name)'
        WRITE(11,'(A)') '    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5ltread_dataset_real_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO

! h5ltmake_dataset_float_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8

! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5ltmake_dataset_float_kind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(loc_id,dset_name,rank,dims,buf,errcode)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
        WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
        WRITE(11,'(A)') '    INTEGER,          INTENT(IN) :: rank'
        WRITE(11,'(A)') '    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims'
        WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER :: errcode '
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    INTEGER(size_t) :: namelen' 
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    namelen = LEN(dset_name)'
        WRITE(11,'(A)') '    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5ltmake_dataset_float_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO

! h5ltread_dataset_float_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8

! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5ltread_dataset_float_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5ltread_dataset_float_kind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(loc_id,dset_name,buf,dims,errcode)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
        WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
        WRITE(11,'(A)') '    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims'
        WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER :: errcode '
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    INTEGER(size_t) :: namelen' 
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    namelen = LEN(dset_name)'
        WRITE(11,'(A)') '    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5ltread_dataset_float_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO

! h5ltmake_dataset_double_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8
! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5ltmake_dataset_double_kind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(loc_id,dset_name,rank,dims,buf,errcode)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
        WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
        WRITE(11,'(A)') '    INTEGER,          INTENT(IN) :: rank'
        WRITE(11,'(A)') '    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims'
        WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER :: errcode '
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    INTEGER(size_t) :: namelen' 
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    namelen = LEN(dset_name)'
        WRITE(11,'(A)') '    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5ltmake_dataset_double_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO

! h5ltread_dataset_double_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8

! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5ltread_dataset_double_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5ltread_dataset_double_kind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(loc_id,dset_name,buf,dims,errcode)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
        WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
        WRITE(11,'(A)') '    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims'
        WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER :: errcode '
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    INTEGER(size_t) :: namelen' 
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    namelen = LEN(dset_name)'
        WRITE(11,'(A)') '    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5ltread_dataset_double_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO

! h5ltmake_dataset_f
  DO i = 1, num_ikinds
     k = ikind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8

! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5ltmake_dataset_integer_kind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(loc_id,dset_name,rank,dims,type_id,buf,errcode)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
        WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
        WRITE(11,'(A)') '    INTEGER,          INTENT(IN) :: rank'
        WRITE(11,'(A)') '    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims'
        WRITE(11,'(A)') '    INTEGER(hid_t),   INTENT(in) :: type_id'
        WRITE(11,'(A)') '    INTEGER(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER :: errcode '
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    INTEGER(size_t) :: namelen'
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    namelen = LEN(dset_name)'
        WRITE(11,'(A)') '    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5ltmake_dataset_integer_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO

! h5ltmake_dataset_int_f
  DO i = 1, num_ikinds
     k = ikind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8

! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5ltmake_dataset_int_kind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(loc_id,dset_name,rank,dims,buf,errcode)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
        WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
        WRITE(11,'(A)') '    INTEGER,          INTENT(IN) :: rank'
        WRITE(11,'(A)') '    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims'
        WRITE(11,'(A)') '    INTEGER(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER :: errcode '
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    INTEGER(size_t) :: namelen'
        WRITE(11,'(A)') '    INTEGER(hid_t) :: type_id'
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    namelen = LEN(dset_name)'
        WRITE(11,'(A)') '    type_id = h5kind_to_type(KIND('//f_ptr_line(j)(19:36)//'), H5_INTEGER_KIND)'
        WRITE(11,'(A)') '    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5ltmake_dataset_int_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO

! h5ltread_dataset_f
  DO i = 1, num_ikinds
     k = ikind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8

! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5ltread_dataset_integer_kind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(loc_id,dset_name, type_id, buf,dims,errcode)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
        WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
        WRITE(11,'(A)') '    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims'
        WRITE(11,'(A)') '    INTEGER(hid_t),   INTENT(in) :: type_id'
        WRITE(11,'(A)') '    INTEGER(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER :: errcode '
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    INTEGER(size_t) :: namelen'

        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    namelen = LEN(dset_name)'
        WRITE(11,'(A)') '    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5ltread_dataset_integer_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO

! h5ltread_dataset_int_f
  DO i = 1, num_ikinds
     k = ikind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8

! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5ltread_dataset_int_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5ltread_dataset_int_kind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(loc_id,dset_name, buf,dims,errcode)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
        WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
        WRITE(11,'(A)') '    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims'
        WRITE(11,'(A)') '    INTEGER(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER :: errcode '
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    INTEGER(size_t) :: namelen'
        WRITE(11,'(A)') '    INTEGER(hid_t) :: type_id'

        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    namelen = LEN(dset_name)'
        WRITE(11,'(A)') '    type_id = h5kind_to_type(KIND('//f_ptr_line(j)(19:36)//'), H5_INTEGER_KIND)'
        WRITE(11,'(A)') '    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5ltread_dataset_int_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO

  WRITE(11,'(A)') 'END MODULE H5LT' ! change this to be generic MSB

  CLOSE(11)

! Generate Fortran H5TB* interfaces having multiple KIND interfaces.


  OPEN(11,FILE='H5TBff_gen.F90')
  WRITE(11,'(40(A,/))') &
'!****h* ROBODoc/H5TBff_gen.F90',&
'!',&
'! NAME',&
'!  H5TBff_gen',&
'! ',&
'! PURPOSE',&
'!  This module is generated at build by H5HL_buildiface.F90 to handle all the',&
'!  detected REAL/INTEGER KINDs for APIs being passed those KINDs. Currently these ',&
'!  are H5LT and H5TB APIs',&
'!',&
'! COPYRIGHT',&
'! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *',&
'!   Copyright by The HDF Group.                                               *',&
'!   All rights reserved.                                                      *',&
'!                                                                             *',&
'!   This file is part of HDF5.  The full HDF5 copyright notice, including     *',&
'!   terms governing use, modification, and redistribution, is contained in    *',&
'!   the files COPYING and Copyright.html.  COPYING can be found at the root   *',&
'!   of the source code distribution tree; Copyright.html can be found at the  *',&
'!   root level of an installed copy of the electronic HDF5 document set and   *',&
'!   is linked from the top-level documents page.  It can also be found at     *',&
'!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *',&
'!   access to either file, you may request a copy from help@hdfgroup.org.     *',&
'! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *',&
'!',&
'! AUTHOR',&
'!   H5HL_buildiface.F90',&
'!',&
'!*****'

  WRITE(11,'(a)') "MODULE H5TB"

  WRITE(11,'(A)') '  USE, INTRINSIC :: ISO_C_BINDING'
  WRITE(11,'(A)') '  USE h5fortran_types'
  WRITE(11,'(A)') '  USE H5TB_CONST'
  WRITE(11,'(A)') '  IMPLICIT NONE'

!***************
! H5TB INTERFACES
!***************

!  h5tbwrite_field_name_f
  WRITE(11,'(A)') "  INTERFACE h5tbwrite_field_name_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     k =2
     WRITE(11,'(A)') "     MODULE PROCEDURE h5tbwrite_field_name_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

!  h5tbread_field_name_f
  WRITE(11,'(A)') "  INTERFACE h5tbread_field_name_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     k = 2
     WRITE(11,'(A)') "     MODULE PROCEDURE h5tbread_field_name_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

!  h5tbwrite_field_index_f
  WRITE(11,'(A)') "  INTERFACE h5tbwrite_field_index_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     k = 2
     WRITE(11,'(A)') "     MODULE PROCEDURE h5tbwrite_field_index_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

!  h5tbread_field_index_f
  WRITE(11,'(A)') "  INTERFACE h5tbread_field_index_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     k = 2
     WRITE(11,'(A)') "     MODULE PROCEDURE h5tbread_field_index_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

!  h5tbinsert_field_f
  WRITE(11,'(A)') "  INTERFACE h5tbinsert_field_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     k = 2
     WRITE(11,'(A)') "     MODULE PROCEDURE h5tbinsert_field_kind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
  END DO
  WRITE(11,'(A)') "  END INTERFACE"

  WRITE(11,'(A)') 'CONTAINS'

  !**********************
  ! H5TB APIs
  !**********************

  ! h5tbwrite_field_name_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     j = 2
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5tbwrite_field_name_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5tbwrite_field_name_kind_'//TRIM(ADJUSTL(chr2))&
          &//'_rank'//chr_rank(j)//'(loc_id,dset_name,field_name,start, nrecords,type_size,buf,errcode)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
     WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
     WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(in) :: field_name'
     WRITE(11,'(A)') '    INTEGER(hsize_t), INTENT(in) :: start'
     WRITE(11,'(A)') '    INTEGER(hsize_t), INTENT(in) :: nrecords'
     WRITE(11,'(A)') '    INTEGER(size_t),  INTENT(in) :: type_size'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN), DIMENSION(*), TARGET :: buf'
     WRITE(11,'(A)') '    INTEGER :: errcode '
     WRITE(11,'(A)') '    INTEGER(size_t) :: namelen' 
     WRITE(11,'(A)') '    INTEGER(size_t) :: namelen1' 
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        
     WRITE(11,'(A)') f_ptr_line(j)
     WRITE(11,'(A)') '    namelen = LEN(dset_name)'
     WRITE(11,'(A)') '    namelen1 = LEN(field_name)'
     WRITE(11,'(A)') &
          '    errcode = h5tbwrite_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,start,nrecords,type_size,f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5tbwrite_field_name_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
  ENDDO

  ! h5tbread_field_name_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     j = 2
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5tbread_field_name_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5tbread_field_name_kind_'//TRIM(ADJUSTL(chr2))&
          &//'_rank'//chr_rank(j)//'(loc_id,dset_name,field_name,start, nrecords,type_size,buf,errcode)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
     WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
     WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(in) :: field_name'
     WRITE(11,'(A)') '    INTEGER(hsize_t), INTENT(in) :: start'
     WRITE(11,'(A)') '    INTEGER(hsize_t), INTENT(in) :: nrecords'
     WRITE(11,'(A)') '    INTEGER(size_t),  INTENT(in) :: type_size'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN), DIMENSION(*), TARGET :: buf'
     WRITE(11,'(A)') '    INTEGER :: errcode '
     WRITE(11,'(A)') '    INTEGER(size_t) :: namelen' 
     WRITE(11,'(A)') '    INTEGER(size_t) :: namelen1' 
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        
     WRITE(11,'(A)') f_ptr_line(j)
     WRITE(11,'(A)') '    namelen = LEN(dset_name)'
     WRITE(11,'(A)') '    namelen1 = LEN(field_name)'
     WRITE(11,'(A)') &
          '    errcode = h5tbread_field_name_c(loc_id,namelen,dset_name,namelen1,field_name,start,nrecords,type_size,f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5tbread_field_name_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
  ENDDO

  ! h5tbwrite_field_index_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     j = 2
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5tbwrite_field_index_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5tbwrite_field_index_kind_'//TRIM(ADJUSTL(chr2))&
          &//'_rank'//chr_rank(j)//'(loc_id,dset_name,field_index,start, nrecords,type_size,buf,errcode)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
     WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
     WRITE(11,'(A)') '    INTEGER, INTENT(in) :: field_index'
     WRITE(11,'(A)') '    INTEGER(hsize_t), INTENT(in) :: start'
     WRITE(11,'(A)') '    INTEGER(hsize_t), INTENT(in) :: nrecords'
     WRITE(11,'(A)') '    INTEGER(size_t),  INTENT(in) :: type_size'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN), DIMENSION(*), TARGET :: buf'
     WRITE(11,'(A)') '    INTEGER :: errcode '
     WRITE(11,'(A)') '    INTEGER(size_t) :: namelen' 
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        
     WRITE(11,'(A)') f_ptr_line(j)
     WRITE(11,'(A)') '    namelen = LEN(dset_name)'
     WRITE(11,'(A)') &
          '    errcode = h5tbwrite_field_index_c(loc_id,namelen,dset_name,field_index,start,nrecords,type_size,f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5tbwrite_field_index_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
  ENDDO

  ! h5tbread_field_index_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     j = 2
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5tbread_field_index_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5tbread_field_index_kind_'//TRIM(ADJUSTL(chr2))&
          &//'_rank'//chr_rank(j)//'(loc_id,dset_name,field_index,start, nrecords,type_size,buf,errcode)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(hid_t)  , INTENT(IN) :: loc_id'
     WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: dset_name'
     WRITE(11,'(A)') '    INTEGER, INTENT(in) :: field_index'
     WRITE(11,'(A)') '    INTEGER(hsize_t), INTENT(in) :: start'
     WRITE(11,'(A)') '    INTEGER(hsize_t), INTENT(in) :: nrecords'
     WRITE(11,'(A)') '    INTEGER(size_t),  INTENT(in) :: type_size'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN), DIMENSION(*), TARGET :: buf'
     WRITE(11,'(A)') '    INTEGER :: errcode '
     WRITE(11,'(A)') '    INTEGER(size_t) :: namelen'
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        
     WRITE(11,'(A)') f_ptr_line(j)
     WRITE(11,'(A)') '    namelen = LEN(dset_name)'
     WRITE(11,'(A)') &
          '    errcode = h5tbread_field_index_c(loc_id,namelen,dset_name,field_index,start,nrecords,type_size,f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5tbread_field_index_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
  ENDDO

  ! h5tbinsert_field_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     j = 2
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_HL_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5tbinsert_field_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5tbinsert_field_kind_'//TRIM(ADJUSTL(chr2))&
          &//'_rank'//chr_rank(j)//'(loc_id,dset_name,field_name,field_type,field_index,buf,errcode)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '        INTEGER(hid_t),   INTENT(in) :: loc_id'
     WRITE(11,'(A)') '        CHARACTER(LEN=*), INTENT(in) :: dset_name'
     WRITE(11,'(A)') '        CHARACTER(LEN=*), INTENT(in) :: field_name'
     WRITE(11,'(A)') '        INTEGER(hid_t), INTENT(in)   :: field_type'
     WRITE(11,'(A)') '        INTEGER, INTENT(in) :: field_index'
     WRITE(11,'(A)') '        REAL(KIND='//TRIM(ADJUSTL(chr2))//'), INTENT(in), DIMENSION(*), TARGET :: buf'
     WRITE(11,'(A)') '        INTEGER(size_t) :: namelen'
     WRITE(11,'(A)') '        INTEGER(size_t) :: namelen1'
     WRITE(11,'(A)') '        INTEGER :: errcode'
     WRITE(11,'(A)') '        TYPE(C_PTR) :: f_ptr'
        
     WRITE(11,'(A)') f_ptr_line(j)
     WRITE(11,'(A)') '    namelen = LEN(dset_name)'
     WRITE(11,'(A)') '    namelen1 = LEN(field_name)'
     WRITE(11,'(A)') &
          '    errcode = h5tbinsert_field_c(loc_id,namelen,dset_name,namelen1,field_name,field_type,field_index,f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5tbinsert_field_kind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
  ENDDO

  WRITE(11,'(A)') 'END MODULE H5TB'

  CLOSE(11)

END PROGRAM H5HL_buildiface



