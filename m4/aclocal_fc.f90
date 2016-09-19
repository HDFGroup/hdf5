! This file contains all the configure test programs 
! used by autotools and cmake. This avoids having to
! duplicate code for both cmake and autotool tests.
! For autotools, a program below is chosen via a
! sed command in aclocal_fc.m4. For cmake, a program
! below is chosen via the macro READ_SOURCE in 
! HDF5UseFortran.cmake
!

PROGRAM PROG_FC_ISO_FORTRAN_ENV
  USE, INTRINSIC :: ISO_FORTRAN_ENV
END PROGRAM PROG_FC_ISO_FORTRAN_ENV

PROGRAM PROG_FC_SIZEOF
  i = sizeof(x)
END PROGRAM PROG_FC_SIZEOF

PROGRAM PROG_FC_C_SIZEOF
  USE ISO_C_BINDING
  INTEGER(C_INT) :: a
  INTEGER(C_SIZE_T) :: RESULT
  RESULT = C_SIZEOF(a)
END PROGRAM PROG_FC_C_SIZEOF

PROGRAM PROG_FC_STORAGE_SIZE
  INTEGER :: a
  INTEGER :: RESULT
  RESULT = STORAGE_SIZE(a)
END PROGRAM PROG_FC_STORAGE_SIZE

PROGRAM PROG_FC_HAVE_C_LONG_DOUBLE
  USE ISO_C_BINDING
  REAL(KIND=C_LONG_DOUBLE) :: d
END PROGRAM PROG_FC_HAVE_C_LONG_DOUBLE

PROGRAM PROG_FC_HAVE_F2003_REQUIREMENTS
  USE iso_c_binding
  IMPLICIT NONE
  TYPE(C_PTR) :: ptr
  TYPE(C_FUNPTR) :: funptr
  CHARACTER(LEN=80, KIND=c_char), TARGET :: ichr
  ptr = C_LOC(ichr(1:1))
END PROGRAM PROG_FC_HAVE_F2003_REQUIREMENTS

!---- START ----- Check to see C_LONG_DOUBLE is different from C_DOUBLE
MODULE type_mod
  USE ISO_C_BINDING
  INTERFACE h5t	
     MODULE PROCEDURE h5t_c_double
     MODULE PROCEDURE h5t_c_long_double
  END INTERFACE
CONTAINS
  SUBROUTINE h5t_c_double(r)
    REAL(KIND=C_DOUBLE) :: r
  END SUBROUTINE h5t_c_double
  SUBROUTINE h5t_c_long_double(d)
    REAL(KIND=C_LONG_DOUBLE) :: d
  END SUBROUTINE h5t_c_long_double
END MODULE type_mod
PROGRAM PROG_FC_C_LONG_DOUBLE_EQ_C_DOUBLE
  USE ISO_C_BINDING
  USE type_mod
  REAL(KIND=C_DOUBLE)      :: r
  REAL(KIND=C_LONG_DOUBLE) :: d
  CALL h5t(r)
  CALL h5t(d)
END PROGRAM PROG_FC_C_LONG_DOUBLE_EQ_C_DOUBLE
!---- END ------- Check to see C_LONG_DOUBLE is different from C_DOUBLE

!---- START ----- Determine the available KINDs for REALs and INTEGERs
PROGRAM FC_AVAIL_KINDS
      IMPLICIT NONE
      INTEGER :: ik, jk, k, max_decimal_prec
      INTEGER :: num_rkinds = 1, num_ikinds = 1
      INTEGER, DIMENSION(1:10) :: list_ikinds = -1
      INTEGER, DIMENSION(1:10) :: list_rkinds = -1
  
      OPEN(8, FILE='pac_fconftest.out', FORM='formatted')

      ! Find integer KINDs
      list_ikinds(num_ikinds)=SELECTED_INT_KIND(1)
      DO ik = 2, 36
         k = SELECTED_INT_KIND(ik)
         IF(k.LT.0) EXIT
         IF(k.GT.list_ikinds(num_ikinds))THEN
            num_ikinds = num_ikinds + 1
            list_ikinds(num_ikinds) = k
         ENDIF
      ENDDO

      DO k = 1, num_ikinds
         WRITE(8,'(I0)', ADVANCE='NO') list_ikinds(k)
         IF(k.NE.num_ikinds)THEN
            WRITE(8,'(A)',ADVANCE='NO') ','
         ELSE
            WRITE(8,'()')
         ENDIF
      ENDDO

      ! Find real KINDs
      list_rkinds(num_rkinds)=SELECTED_REAL_KIND(1)
      max_decimal_prec = 1

      prec: DO ik = 2, 36
         exp: DO jk = 1, 17000
            k = SELECTED_REAL_KIND(ik,jk)
            IF(k.LT.0) EXIT exp
            IF(k.GT.list_rkinds(num_rkinds))THEN
               num_rkinds = num_rkinds + 1
               list_rkinds(num_rkinds) = k
            ENDIF
            max_decimal_prec = ik
         ENDDO exp
      ENDDO prec

      DO k = 1, num_rkinds
         WRITE(8,'(I0)', ADVANCE='NO') list_rkinds(k)
         IF(k.NE.num_rkinds)THEN
            WRITE(8,'(A)',ADVANCE='NO') ','
         ELSE
            WRITE(8,'()')
         ENDIF
      ENDDO

     WRITE(8,'(I0)') max_decimal_prec
     WRITE(8,'(I0)') num_ikinds
     WRITE(8,'(I0)') num_rkinds
END PROGRAM FC_AVAIL_KINDS
!---- END ----- Determine the available KINDs for REALs and INTEGERs

PROGRAM FC_MPI_CHECK
  INCLUDE 'mpif.h'
  INTEGER :: comm, amode, info, fh, ierror
  CHARACTER(LEN=1) :: filename 
  CALL MPI_File_open( comm, filename, amode, info, fh, ierror)
END PROGRAM FC_MPI_CHECK
