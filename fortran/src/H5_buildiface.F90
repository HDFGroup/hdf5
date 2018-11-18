!****p* Program/H5_buildiface
!
! NAME
!  Executable: H5_buildiface
!
! FILE
!  fortran/src/H5_buildiface.F90
!
! PURPOSE
!  This stand alone program is used at build time to generate the module
!  H5_gen (H5_gen.F90). It cycles through all the available KIND parameters for
!  integers and reals. The appropriate program and subroutines are then generated
!  depending on which of the KIND values are found.
!
! NOTES
!  This program uses the Fortran 2008 intrinsic function STORAGE_SIZE or SIZEOF 
!  depending on availablity.It generates code that makes use of 
!  STORAGE_SIZE/SIZEOF in H5_gen.F90. STORAGE_SIZE is standard
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
!  the COPYING file, which can be found at the root of the source code         *
!  distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.    *
!  If you do not have access to either file, you may request a copy from       *
!  help@hdfgroup.org.                                                          *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! AUTHOR
!  M. Scot Breitenfeld
!
!*****

#include <H5config_f.inc>

PROGRAM H5_buildiface
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

! These values are valid REAL KINDs (with corresponding C float) found during configure
  H5_H5CONFIG_F_NUM_RKIND
  H5_H5CONFIG_F_RKIND
! These values are valid INTEGER KINDs (with corresponding C integer) found during configure
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
       '    f_ptr = C_LOC(buf)               ', &
       '    f_ptr = C_LOC(buf(1))            ', &
       '    f_ptr = C_LOC(buf(1,1))          ', &
       '    f_ptr = C_LOC(buf(1,1,1))        ', &
       '    f_ptr = C_LOC(buf(1,1,1,1))      ', &
       '    f_ptr = C_LOC(buf(1,1,1,1,1))    ', &
       '    f_ptr = C_LOC(buf(1,1,1,1,1,1))  ', &
       '    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))'  &
            /)
  CHARACTER(LEN=42), DIMENSION(1:8), PARAMETER :: fchr_ptr_line=(/ &
       '    f_ptr = C_LOC(buf(1:1))               ', &
       '    f_ptr = C_LOC(buf(1)(1:1))            ', &
       '    f_ptr = C_LOC(buf(1,1)(1:1))          ', &
       '    f_ptr = C_LOC(buf(1,1,1)(1:1))        ', &
       '    f_ptr = C_LOC(buf(1,1,1,1)(1:1))      ', &
       '    f_ptr = C_LOC(buf(1,1,1,1,1)(1:1))    ', &
       '    f_ptr = C_LOC(buf(1,1,1,1,1,1)(1:1))  ', &
       '    f_ptr = C_LOC(buf(1,1,1,1,1,1,1)(1:1))'  &
            /)

! (a) Generate Fortran H5* interfaces having multiple KIND interfaces.
!
! DEVELOPER'S NOTES:
!
! Only interfaces with arrays of rank 7 and less are provided. Even-though, the F2008 
! standard extended the maximum rank to 15, it was decided that user's should use the
! new APIs to handle those use cases. Handling rank 7 and less is for backward compatibility
! with the Fortran 90/95 APIs codes which could never handle ranks greater than 7.

  OPEN(11,FILE='H5_gen.F90')
  WRITE(11,'(40(A,/))') &
'!****h* ROBODoc/H5_gen.F90',&
'!',&
'! NAME',&
'!  H5_gen',&
'! ',&
'! PURPOSE',&
'!  This module is generated at build by H5_buildiface.F90 to handle all the',&
'!  detected KINDs for APIs being passed INTEGERs, REALs and CHARACTERs. Currently ',&
'!  these are H5A, H5D and H5P APIs',&
'!',&
'! COPYRIGHT',&
'! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *',&
'!   Copyright by The HDF Group.                                               *',&
'!   All rights reserved.                                                      *',&
'!                                                                             *',&
'!   This file is part of HDF5.  The full HDF5 copyright notice, including     *',&
'!   terms governing use, modification, and redistribution, is contained in    *',&
'!   the COPYING file, which can be found at the root of the source code       *',&
'!   distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *',&
'!   If you do not have access to either file, you may request a copy from     *',&
'!   help@hdfgroup.org.                                                        *',&
'! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *',&
'!',&
'! AUTHOR',&
'!  H5_buildiface.F90',&
'!',&
'!*****'

  WRITE(11,'(A)') "MODULE H5_GEN"

  WRITE(11,'(2X,A)') 'USE, INTRINSIC :: ISO_C_BINDING'
  WRITE(11,'(2X,A)') 'USE H5GLOBAL'

  WRITE(11,'(2X,A)') 'USE H5A'
  WRITE(11,'(2X,A)') 'USE H5D'
  WRITE(11,'(2X,A)') 'USE H5P'
  WRITE(11,'(2X,A)') 'IMPLICIT NONE'

!******************************
! DECLARE PRIVATE INTERFACES
!******************************

  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(2X,A)') "PRIVATE h5awrite_rkind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(2X,A)') "PRIVATE h5awrite_ikind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO k = 2, 8
     WRITE(11,'(2X,A)') "PRIVATE h5awrite_ckind_rank"//chr_rank(k)
  ENDDO
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(2X,A)') "PRIVATE h5aread_rkind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(2X,A)') "PRIVATE h5aread_ikind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO k = 2, 8
     WRITE(11,'(2X,A)') "PRIVATE h5aread_ckind_rank"//chr_rank(k)
  ENDDO

  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(2X,A)') "PRIVATE h5dwrite_rkind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(2X,A)') "PRIVATE h5dwrite_ikind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO 
  DO k = 2, 8
     WRITE(11,'(2X,A)') "PRIVATE h5dwrite_ckind_rank"//chr_rank(k)
  END DO
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(2X,A)') "PRIVATE h5dread_rkind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(2X,A)') "PRIVATE h5dread_ikind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO 
  END DO
  DO k = 2, 8
     WRITE(11,'(2X,A)') "PRIVATE h5dread_ckind_rank"//chr_rank(k)
  ENDDO

  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(2X,A)') "PRIVATE h5pset_fill_value_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(2X,A)') "PRIVATE h5pget_fill_value_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(2X,A)') "PRIVATE h5pset_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(2X,A)') "PRIVATE h5pget_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(2X,A)') "PRIVATE h5pregister_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(2X,A)') "PRIVATE h5pinsert_kind_"//TRIM(ADJUSTL(chr2))
  END DO

!***************
! H5A INTERFACES
!***************
!
! H5Awrite_f
!
  WRITE(11,'(2X,A)') "INTERFACE h5awrite_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(5X,A)') "MODULE PROCEDURE h5awrite_rkind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(5X,A)') "MODULE PROCEDURE h5awrite_ikind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO k = 2, 8
     WRITE(11,'(5X,A)') "MODULE PROCEDURE h5awrite_ckind_rank"//chr_rank(k)
  ENDDO
  WRITE(11,'(2X,A)') "END INTERFACE"

! H5Aread_f
  WRITE(11,'(2X,A)') "INTERFACE h5aread_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(5X,A)') "MODULE PROCEDURE h5aread_rkind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(5X,A)') "MODULE PROCEDURE h5aread_ikind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO k = 2, 8
     WRITE(11,'(5X,A)') "MODULE PROCEDURE h5aread_ckind_rank"//chr_rank(k)
  ENDDO
  WRITE(11,'(2X,A)') "END INTERFACE"
!***************
! H5D INTERFACES
!***************
!
! H5Dwrite_f
  WRITE(11,'(2X,A)') "INTERFACE h5dwrite_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(5X,A)') "MODULE PROCEDURE h5dwrite_rkind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(5X,A)') "MODULE PROCEDURE h5dwrite_ikind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO 
  DO k = 2, 8
     WRITE(11,'(5X,A)') "MODULE PROCEDURE h5dwrite_ckind_rank"//chr_rank(k)
  END DO
  WRITE(11,'(2X,A)') "END INTERFACE"

! H5Dread_f
  WRITE(11,'(2X,A)') "INTERFACE h5dread_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(5X,A)') "MODULE PROCEDURE h5dread_rkind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO
  END DO
  DO i = 1, num_ikinds
     j = ikind(i)
     WRITE(chr2,'(I2)') j
     DO k = 1, 8
        WRITE(11,'(5X,A)') "MODULE PROCEDURE h5dread_ikind_"//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(k)
     ENDDO 
  END DO
  DO k = 2, 8
     WRITE(11,'(5X,A)') "MODULE PROCEDURE h5dread_ckind_rank"//chr_rank(k)
  ENDDO
  WRITE(11,'(2X,A)') "END INTERFACE"

!***************
! H5P INTERFACES
!***************
!
! H5Pset_fill_value_f
  WRITE(11,'(2X,A)') "INTERFACE h5pset_fill_value_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(5X,A)') "MODULE PROCEDURE h5pset_fill_value_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  WRITE(11,'(2X,A)') "END INTERFACE"

! H5Pget_fill_value_f
  WRITE(11,'(2X,A)') "INTERFACE h5pget_fill_value_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(5X,A)') "MODULE PROCEDURE h5pget_fill_value_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  WRITE(11,'(2X,A)') "END INTERFACE"

! H5Pset_f
  WRITE(11,'(2X,A)') "INTERFACE h5pset_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(5X,A)') "MODULE PROCEDURE h5pset_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  WRITE(11,'(2X,A)') "END INTERFACE"

! H5Pget_f
  WRITE(11,'(2X,A)') "INTERFACE h5pget_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(5X,A)') "MODULE PROCEDURE h5pget_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  WRITE(11,'(2X,A)') "END INTERFACE"

! H5Pregister_f
  WRITE(11,'(2X,A)') "INTERFACE h5pregister_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(5X,A)') "MODULE PROCEDURE h5pregister_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  WRITE(11,'(2X,A)') "END INTERFACE"

! H5Pinsert_f
  WRITE(11,'(2X,A)') "INTERFACE h5pinsert_f"
  DO i = 1, num_rkinds
     j = rkind(i)
     WRITE(chr2,'(I2)') j
     WRITE(11,'(5X,A)') "MODULE PROCEDURE h5pinsert_kind_"//TRIM(ADJUSTL(chr2))
  END DO
  WRITE(11,'(2X,A)') "END INTERFACE"

  WRITE(11,'(A)') 'CONTAINS'

!**********************
! H5A APIs
!**********************
!
! H5Awrite_f

!****s* H5A (F03)/H5Awrite_f_F90
!
! NAME
!  H5Awrite_f_F90
!
! PURPOSE
!  Writes an attribute.
!
! Inputs:
!  attr_id     - Attribute identifier
!  memtype_id  - Attribute datatype identifier  (in memory)
!  dims        - Array to hold corresponding dimension sizes of data buffer buf;
!                dim(k) has value of the k-th dimension of buffer buf;
!                values are ignored if buf is a scalar
!  buf 	       - Data buffer; may be a scalar or an array
!
! Outputs:
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
!  dims parameter was added to make code portable;
!  Aprile 4, 2001
!
!  Changed buf intent to INOUT to be consistant
!  with how the C functions handles it. The pg
!  compiler will return 0 if a buf value is not set.
!  February, 2008
!
! NOTES
!  This function is overloaded to write INTEGER,
!  REAL, REAL(KIND=C_DOUBLE) and CHARACTER buffers
!  up to 7 dimensions.
!
! Fortran90 Interface:
!!  SUBROUTINE H5Awrite_f(attr_id, memtype_id, buf, dims, hdferr) 
!!    INTEGER(HID_T)  , INTENT(IN)               :: attr_id
!!    INTEGER(HID_T)  , INTENT(IN)               :: memtype_id
!!    TYPE            , INTENT(IN)               :: buf
!!    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
!!    INTEGER         , INTENT(OUT)              :: hdferr
!*****

  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8

! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5awrite_rkind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5awrite_rkind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(attr_id, memtype_id, buf, dims, hdferr)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: attr_id'
        WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: memtype_id'
        WRITE(11,'(A)') '    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims'
        WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER           , INTENT(OUT) :: hdferr'
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5awrite_rkind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO
  DO i = 1, num_ikinds
     k = ikind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8

! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5awrite_ikind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5awrite_ikind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(attr_id, memtype_id, buf, dims, hdferr)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: attr_id'
        WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: memtype_id'
        WRITE(11,'(A)') '    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims'
        WRITE(11,'(A)') '    INTEGER(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER           , INTENT(OUT) :: hdferr'
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5awrite_ikind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO
  DO j = 2, 8

! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5awrite_ckind_rank'//chr_rank(j)
     WRITE(11,'(A)') '!DEC$endif'

! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5awrite_ckind_rank'//chr_rank(j)//'(attr_id, memtype_id, buf, dims, hdferr)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: attr_id'
     WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: memtype_id'
     WRITE(11,'(A)') '    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims'
     WRITE(11,'(A)') '    CHARACTER(LEN=*)  , INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
     WRITE(11,'(A)') '    INTEGER           , INTENT(OUT) :: hdferr'
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
     
     WRITE(11,'(A)') fchr_ptr_line(j)
     WRITE(11,'(A)') '    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5awrite_ckind_rank'//chr_rank(j)
  ENDDO

!
! H5Aread_f

!****s* H5A (F03)/H5Aread_f_F90
!
! NAME
!  H5Aread_f_F90
!
! PURPOSE
!  Reads an attribute.
!
! Inputs:
!  attr_id     - Attribute identifier
!  memtype_id  - Attribute datatype identifier  (in memory)
!  dims        - Array to hold corresponding dimension sizes of data buffer buf;
!                dim(k) has value of the k-th dimension of buffer buf;
!                values are ignored if buf is a scalar
!
! Outputs:
!  buf 	       - Data buffer; may be a scalar or an array
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
!  dims parameter was added to make code portable;
!  Aprile 4, 2001
!
!  Changed buf intent to INOUT to be consistant
!  with how the C functions handles it. The pg
!  compiler will return 0 if a buf value is not set.
!  February, 2008
!
! NOTES
!  This function is overloaded to write INTEGER,
!  REAL, REAL(KIND=C_DOUBLE) and CHARACTER buffers
!  up to 7 dimensions.
! Fortran90 Interface:
!!  SUBROUTINE H5Aread_f(attr_id, memtype_id, buf, dims, hdferr) 
!!    INTEGER(HID_T)  , INTENT(IN)               :: attr_id
!!    INTEGER(HID_T)  , INTENT(IN)               :: memtype_id
!!    TYPE            , INTENT(INOUT)            :: buf
!!    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
!!    INTEGER         , INTENT(OUT)              :: hdferr
!*****
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8
! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5aread_rkind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5aread_rkind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(attr_id, memtype_id, buf, dims, hdferr)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: attr_id'
        WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: memtype_id'
        WRITE(11,'(A)') '    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims'
        WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(INOUT)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER           , INTENT(OUT) :: hdferr'
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5aread_rkind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO
  DO i = 1, num_ikinds
     k = ikind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8
! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5aread_ikind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5aread_ikind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(attr_id, memtype_id, buf, dims, hdferr)'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: attr_id'
        WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: memtype_id'
        WRITE(11,'(A)') '    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims'
        WRITE(11,'(A)') '    INTEGER(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(INOUT)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER           , INTENT(OUT) :: hdferr'
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5aread_ikind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO
  DO j = 2, 8
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5aread_ckind_rank'//chr_rank(j)
     WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5aread_ckind_rank'//chr_rank(j)//'(attr_id, memtype_id, buf, dims, hdferr)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: attr_id'
     WRITE(11,'(A)') '    INTEGER(HID_T)    , INTENT(IN) :: memtype_id'
     WRITE(11,'(A)') '    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims'
     WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
     WRITE(11,'(A)') '    INTEGER           , INTENT(OUT) :: hdferr'
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
     
     WRITE(11,'(A)') fchr_ptr_line(j)
     WRITE(11,'(A)') '    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5aread_ckind_rank'//chr_rank(j)
  ENDDO

!**********************
! H5D APIs
!**********************
!
! h5dread_f

!
! NAME		
!  h5dread_f
!
! PURPOSE
!  Reads raw data from the specified dataset into buf,
!  converting from file datatype and dataspace to memory
!  datatype and dataspace.
!
! Inputs:
!		dset_id		- dataset identifier
!		mem_type_id	- memory type identifier
!		dims		- 1-dim array of size 7; dims(k) has the size
!				- of k-th dimension of the buf array
! Outputs:
!		buf		- buffer to read data in
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!		mem_space_id	- memory dataspace identifier
!		file_space_id 	- file dataspace identifier
!		xfer_prp	- trasfer property list identifier
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY 	
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
!  dims parameter was added to make code portable;
!  n parameter was replaced with dims parameter in
!  the h5dwrite_reference_obj and h5dwrite_reference_dsetreg
!  functions.  April 2, 2001
!
! NOTES	
!  This function is overloaded to read INTEGER,
!  REAL, DOUBLE PRECISION and CHARACTER buffers
!  up to 7 dimensions, and one dimensional buffers
!  of the TYPE(hobj_ref_t_f) and TYPE(hdset_reg_ref_t_f)
!  types.
!
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8
! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5dread_rkind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5dread_rkind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(dset_id, mem_type_id, buf, dims, hdferr, &'
        WRITE(11,'(A)') '       mem_space_id, file_space_id, xfer_prp)'
        WRITE(11,'(A)') '    USE, INTRINSIC :: ISO_C_BINDING'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: dset_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: mem_type_id'
        WRITE(11,'(A)') '    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims'
        WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(INOUT)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER, INTENT(OUT) :: hdferr'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: xfer_prp_default'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: mem_space_id_default'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: file_space_id_default'
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    xfer_prp_default = H5P_DEFAULT_F'
        WRITE(11,'(A)') '    mem_space_id_default = H5S_ALL_F'
        WRITE(11,'(A)') '    file_space_id_default = H5S_ALL_F'
        WRITE(11,'(A)') '    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp'
        WRITE(11,'(A)') '    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id'
        WRITE(11,'(A)') '    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id'
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &'
        WRITE(11,'(A)') '         file_space_id_default, xfer_prp_default, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5dread_rkind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)

     ENDDO
  ENDDO

  DO i = 1, num_ikinds
     k = ikind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8
! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5dread_ikind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5dread_ikind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(dset_id, mem_type_id, buf, dims, hdferr, &'
        WRITE(11,'(A)') '       mem_space_id, file_space_id, xfer_prp)'
        WRITE(11,'(A)') '    USE, INTRINSIC :: ISO_C_BINDING'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: dset_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: mem_type_id'
        WRITE(11,'(A)') '    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims'
        WRITE(11,'(A)') '    INTEGER(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(INOUT)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER, INTENT(OUT) :: hdferr'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: xfer_prp_default'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: mem_space_id_default'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: file_space_id_default'
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    xfer_prp_default = H5P_DEFAULT_F'
        WRITE(11,'(A)') '    mem_space_id_default = H5S_ALL_F'
        WRITE(11,'(A)') '    file_space_id_default = H5S_ALL_F'
        WRITE(11,'(A)') '    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp'
        WRITE(11,'(A)') '    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id'
        WRITE(11,'(A)') '    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id'
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &'
        WRITE(11,'(A)') '         file_space_id_default, xfer_prp_default, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5dread_ikind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)

     ENDDO
  ENDDO
  DO j = 2, 8
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5dread_ckind_rank'//chr_rank(j)
     WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5dread_ckind_rank'//chr_rank(j)//'(dset_id, mem_type_id, buf, dims, hdferr, &'
     WRITE(11,'(A)') '       mem_space_id, file_space_id, xfer_prp)'
     WRITE(11,'(A)') '    USE, INTRINSIC :: ISO_C_BINDING'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: dset_id'
     WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: mem_type_id'
     WRITE(11,'(A)') '    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims'
     WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(INOUT)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
     WRITE(11,'(A)') '    INTEGER, INTENT(OUT) :: hdferr'
     WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id'
     WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id'
     WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp'
     WRITE(11,'(A)') '    INTEGER(HID_T) :: xfer_prp_default'
     WRITE(11,'(A)') '    INTEGER(HID_T) :: mem_space_id_default'
     WRITE(11,'(A)') '    INTEGER(HID_T) :: file_space_id_default'
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
     WRITE(11,'(A)') '    xfer_prp_default = H5P_DEFAULT_F'
     WRITE(11,'(A)') '    mem_space_id_default = H5S_ALL_F'
     WRITE(11,'(A)') '    file_space_id_default = H5S_ALL_F'
     WRITE(11,'(A)') '    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp'
     WRITE(11,'(A)') '    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id'
     WRITE(11,'(A)') '    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id'
     WRITE(11,'(A)') fchr_ptr_line(j)
     WRITE(11,'(A)') '    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &'
     WRITE(11,'(A)') '         file_space_id_default, xfer_prp_default, f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5dread_ckind_rank'//chr_rank(j)
  ENDDO
!**********************
! h5dwrite_f
!**********************
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8
! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5dwrite_rkind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5dwrite_rkind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(dset_id, mem_type_id, buf, dims, hdferr, &'
        WRITE(11,'(A)') '       mem_space_id, file_space_id, xfer_prp)'
        WRITE(11,'(A)') '    USE, INTRINSIC :: ISO_C_BINDING'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: dset_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: mem_type_id'
        WRITE(11,'(A)') '    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims'
        WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER, INTENT(OUT) :: hdferr'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: xfer_prp_default'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: mem_space_id_default'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: file_space_id_default'
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    xfer_prp_default  = H5P_DEFAULT_F'
        WRITE(11,'(A)') '    mem_space_id_default = H5S_ALL_F'
        WRITE(11,'(A)') '    file_space_id_default = H5S_ALL_F'
        WRITE(11,'(A)') '    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp'
        WRITE(11,'(A)') '    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id'
        WRITE(11,'(A)') '    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id'
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &'
        WRITE(11,'(A)') '         file_space_id_default, xfer_prp_default, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5dwrite_rkind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO

  DO i = 1, num_ikinds
     k = ikind(i)
     WRITE(chr2,'(I2)') k
     DO j = 1, 8
! DLL definitions for windows
        WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
        WRITE(11,'(A)') '!DEC$attributes dllexport :: h5dwrite_ikind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
        WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
        WRITE(11,'(A)') '  SUBROUTINE h5dwrite_ikind_'//TRIM(ADJUSTL(chr2))&
             &//'_rank'//chr_rank(j)//'(dset_id, mem_type_id, buf, dims, hdferr, &'
        WRITE(11,'(A)') '       mem_space_id, file_space_id, xfer_prp)'
        WRITE(11,'(A)') '    USE, INTRINSIC :: ISO_C_BINDING'
        WRITE(11,'(A)') '    IMPLICIT NONE'
        WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: dset_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: mem_type_id'
        WRITE(11,'(A)') '    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims'
        WRITE(11,'(A)') '    INTEGER(KIND='//TRIM(ADJUSTL(chr2))//'),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
        WRITE(11,'(A)') '    INTEGER, INTENT(OUT) :: hdferr'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id'
        WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: xfer_prp_default'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: mem_space_id_default'
        WRITE(11,'(A)') '    INTEGER(HID_T) :: file_space_id_default'
        WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
        WRITE(11,'(A)') '    xfer_prp_default  = H5P_DEFAULT_F'
        WRITE(11,'(A)') '    mem_space_id_default = H5S_ALL_F'
        WRITE(11,'(A)') '    file_space_id_default = H5S_ALL_F'
        WRITE(11,'(A)') '    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp'
        WRITE(11,'(A)') '    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id'
        WRITE(11,'(A)') '    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id'
        WRITE(11,'(A)') f_ptr_line(j)
        WRITE(11,'(A)') '    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &'
        WRITE(11,'(A)') '         file_space_id_default, xfer_prp_default, f_ptr)'
        WRITE(11,'(A)') '  END SUBROUTINE h5dwrite_ikind_'//TRIM(ADJUSTL(chr2))//'_rank'//chr_rank(j)
     ENDDO
  ENDDO  
  DO j = 2, 8
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5dwrite_ckind_rank'//chr_rank(j)
     WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5dwrite_ckind_rank'//chr_rank(j)//'(dset_id, mem_type_id, buf, dims, hdferr, &'
     WRITE(11,'(A)') '       mem_space_id, file_space_id, xfer_prp)'
     WRITE(11,'(A)') '    USE, INTRINSIC :: ISO_C_BINDING'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: dset_id'
     WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: mem_type_id'
     WRITE(11,'(A)') '    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims'
     WRITE(11,'(A)') '    CHARACTER(LEN=*),INTENT(IN)'//TRIM(rank_dim_line(j))//', TARGET :: buf'
     WRITE(11,'(A)') '    INTEGER, INTENT(OUT) :: hdferr'
     WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id'
     WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id'
     WRITE(11,'(A)') '    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp'
     WRITE(11,'(A)') '    INTEGER(HID_T) :: xfer_prp_default'
     WRITE(11,'(A)') '    INTEGER(HID_T) :: mem_space_id_default'
     WRITE(11,'(A)') '    INTEGER(HID_T) :: file_space_id_default'
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
     WRITE(11,'(A)') '    xfer_prp_default  = H5P_DEFAULT_F'
     WRITE(11,'(A)') '    mem_space_id_default = H5S_ALL_F'
     WRITE(11,'(A)') '    file_space_id_default = H5S_ALL_F'
     WRITE(11,'(A)') '    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp'
     WRITE(11,'(A)') '    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id'
     WRITE(11,'(A)') '    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id'
     WRITE(11,'(A)') fchr_ptr_line(j)
     WRITE(11,'(A)') '    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &'
     WRITE(11,'(A)') '         file_space_id_default, xfer_prp_default, f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5dwrite_ckind_rank'//chr_rank(j)
  ENDDO

!**********************
! H5P APIs
!**********************
!
! H5Pset_fill_value_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5pset_fill_value_kind_'//TRIM(ADJUSTL(chr2))
     WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5pset_fill_value_kind_'//TRIM(ADJUSTL(chr2))&
          &//'(prp_id, type_id, fillvalue, hdferr)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: prp_id'
     WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: type_id'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'), INTENT(IN), TARGET :: fillvalue' 
     WRITE(11,'(A)') '    INTEGER, INTENT(OUT) :: hdferr '
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr '
     WRITE(11,'(A)') '    f_ptr = C_LOC(fillvalue)'
     WRITE(11,'(A)') '    hdferr = h5pset_fill_value_c(prp_id, type_id, f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5pset_fill_value_kind_'//TRIM(ADJUSTL(chr2))
  ENDDO

! H5Pget_fill_value_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5pget_fill_value_kind_'//TRIM(ADJUSTL(chr2))
     WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5pget_fill_value_kind_'//TRIM(ADJUSTL(chr2))&
          &//'(prp_id, type_id, fillvalue, hdferr)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: prp_id'
     WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: type_id'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'), INTENT(OUT), TARGET :: fillvalue'
     WRITE(11,'(A)') '    INTEGER, INTENT(OUT) :: hdferr'
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
     WRITE(11,'(A)') '    f_ptr = C_LOC(fillvalue)'
     WRITE(11,'(A)') '    hdferr = h5pget_fill_value_c(prp_id, type_id, f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5pget_fill_value_kind_'//TRIM(ADJUSTL(chr2))
  ENDDO

! H5Pset_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5pset_kind_'//TRIM(ADJUSTL(chr2))
     WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5pset_kind_'//TRIM(ADJUSTL(chr2))&
          &//'(prp_id, name, value, hdferr)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: prp_id'
     WRITE(11,'(A)') '    CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'), INTENT(IN), TARGET :: value'
     WRITE(11,'(A)') '    INTEGER, INTENT(OUT) :: hdferr'
     WRITE(11,'(A)') '    INTEGER :: name_len'
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
     WRITE(11,'(A)') '    f_ptr = C_LOC(value)'
     WRITE(11,'(A)') '    name_len = LEN(name)'
     WRITE(11,'(A)') '    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)'
     
     WRITE(11,'(A)') '  END SUBROUTINE h5pset_kind_'//TRIM(ADJUSTL(chr2))
  ENDDO

! H5Pget_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5pget_kind_'//TRIM(ADJUSTL(chr2))
     WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5pget_kind_'//TRIM(ADJUSTL(chr2))&
          &//'(prp_id, name, value, hdferr)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: prp_id'
     WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: name'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),   INTENT(OUT), TARGET :: value'
     WRITE(11,'(A)') '    INTEGER, INTENT(OUT) :: hdferr'
     WRITE(11,'(A)') '    INTEGER :: name_len'
     WRITE(11,'(A)') '    TYPE(C_PTR) :: f_ptr'
     WRITE(11,'(A)') '    f_ptr = C_LOC(value)'
     WRITE(11,'(A)') '    name_len = LEN(name)'
     WRITE(11,'(A)') '    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5pget_kind_'//TRIM(ADJUSTL(chr2))
  ENDDO

! H5Pregister_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5pregister_kind_'//TRIM(ADJUSTL(chr2))
     WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
     WRITE(11,'(A)') 'SUBROUTINE h5pregister_kind_'//TRIM(ADJUSTL(chr2))&
          &//'(class, name, size, value, hdferr)'
     WRITE(11,'(A)') '  IMPLICIT NONE'
     WRITE(11,'(A)') '  INTEGER(HID_T), INTENT(IN) :: class'
     WRITE(11,'(A)') '  CHARACTER(LEN=*), INTENT(IN) :: name'
     WRITE(11,'(A)') '  INTEGER(SIZE_T), INTENT(IN) :: size'
     WRITE(11,'(A)') '  REAL(KIND='//TRIM(ADJUSTL(chr2))//'), INTENT(IN), TARGET :: value'
     WRITE(11,'(A)') '  INTEGER, INTENT(OUT) :: hdferr'
     WRITE(11,'(A)') '  INTEGER :: name_len'
     WRITE(11,'(A)') '  TYPE(C_PTR) :: f_ptr'
     WRITE(11,'(A)') '  f_ptr = C_LOC(value)'
     WRITE(11,'(A)') '  name_len = LEN(name)'
     WRITE(11,'(A)') '  hdferr = h5pregister_c(class, name, name_len, size, f_ptr)'
     WRITE(11,'(A)') 'END SUBROUTINE h5pregister_kind_'//TRIM(ADJUSTL(chr2))
  ENDDO

! H5Pinsert_f
  DO i = 1, num_rkinds
     k = rkind(i)
     WRITE(chr2,'(I2)') k
! DLL definitions for windows
     WRITE(11,'(A)') '!DEC$if defined(BUILD_HDF5_DLL)'
     WRITE(11,'(A)') '!DEC$attributes dllexport :: h5pinsert_kind_'//TRIM(ADJUSTL(chr2))
     WRITE(11,'(A)') '!DEC$endif'
! Subroutine API
     WRITE(11,'(A)') '  SUBROUTINE h5pinsert_kind_'//TRIM(ADJUSTL(chr2))&
          &//'(plist, name, size, value, hdferr)'
     WRITE(11,'(A)') '    IMPLICIT NONE'
     WRITE(11,'(A)') '    INTEGER(HID_T), INTENT(IN) :: plist'
     WRITE(11,'(A)') '    CHARACTER(LEN=*), INTENT(IN) :: name'
     WRITE(11,'(A)') '    INTEGER(SIZE_T), INTENT(IN) :: size'
     WRITE(11,'(A)') '    REAL(KIND='//TRIM(ADJUSTL(chr2))//'),   INTENT(IN), TARGET :: value'
     WRITE(11,'(A)') '    INTEGER, INTENT(OUT) :: hdferr'
     WRITE(11,'(A)') '    INTEGER :: name_len'
     WRITE(11,'(A)') '    TYPE(c_ptr) :: f_ptr'
     WRITE(11,'(A)') '    f_ptr = c_loc(value)'
     WRITE(11,'(A)') '    name_len = LEN(name)'
     WRITE(11,'(A)') '    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)'
     WRITE(11,'(A)') '  END SUBROUTINE h5pinsert_kind_'//TRIM(ADJUSTL(chr2))
  ENDDO

  WRITE(11,'(A)') 'END MODULE H5_gen'

  CLOSE(11)

END PROGRAM H5_buildiface



