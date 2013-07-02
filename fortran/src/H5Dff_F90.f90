!****h* ROBODoc/H5D (F90)
!
! NAME
!
!  H5D_PROVISIONAL
!
! PURPOSE
!
!  This file contains Fortran 90 interfaces for H5D functions. It contains
!  the same functions as H5Dff_F03.f90 but excludes the Fortran 2003 functions
!  and the interface listings. This file will be compiled instead of H5Dff_F03.f90
!  if Fortran 2003 functions are not enabled.
!
! COPYRIGHT
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
! NOTES
!  (1) The maximum rank of an array allowed in Fortran is 7, therefore
!  we only provide an interface for arrays up to and including rank 7.
!
!  (2) Unfortunately we are using a generic interface and one of the factors
!  used in determining the proper routine to select is that of the array
!  rank being passed, therefore we can not create just one subroutine for
!  each array type (integer, real, etc...) of various ranks and then use a
!  rank 1 array of assumed size in the just one subroutine,
!  (i.e. integer, dimension(*) :: ... )
!  (i.e. real   , dimension(*) :: ... ) etc...
!
!  (3)
!                         *** IMPORTANT ***
!  If you add a new H5D function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!*****

MODULE H5D_PROVISIONAL
  USE H5GLOBAL

  INTERFACE h5dwrite_f

     MODULE PROCEDURE h5dwrite_reference_obj
     MODULE PROCEDURE h5dwrite_reference_dsetreg
     MODULE PROCEDURE h5dwrite_integer_scalar
     MODULE PROCEDURE h5dwrite_integer_1
     MODULE PROCEDURE h5dwrite_integer_2
     MODULE PROCEDURE h5dwrite_integer_3
     MODULE PROCEDURE h5dwrite_integer_4
     MODULE PROCEDURE h5dwrite_integer_5
     MODULE PROCEDURE h5dwrite_integer_6
     MODULE PROCEDURE h5dwrite_integer_7
     MODULE PROCEDURE h5dwrite_char_scalar
     MODULE PROCEDURE h5dwrite_char_1
     MODULE PROCEDURE h5dwrite_char_2
     MODULE PROCEDURE h5dwrite_char_3
     MODULE PROCEDURE h5dwrite_char_4
     MODULE PROCEDURE h5dwrite_char_5
     MODULE PROCEDURE h5dwrite_char_6
     MODULE PROCEDURE h5dwrite_char_7
     MODULE PROCEDURE h5dwrite_real_scalar
     MODULE PROCEDURE h5dwrite_real_1
     MODULE PROCEDURE h5dwrite_real_2
     MODULE PROCEDURE h5dwrite_real_3
     MODULE PROCEDURE h5dwrite_real_4
     MODULE PROCEDURE h5dwrite_real_5
     MODULE PROCEDURE h5dwrite_real_6
     MODULE PROCEDURE h5dwrite_real_7

  END INTERFACE

  INTERFACE h5dread_f

     MODULE PROCEDURE h5dread_reference_obj
     MODULE PROCEDURE h5dread_reference_dsetreg
     MODULE PROCEDURE h5dread_integer_scalar
     MODULE PROCEDURE h5dread_integer_1
     MODULE PROCEDURE h5dread_integer_2
     MODULE PROCEDURE h5dread_integer_3
     MODULE PROCEDURE h5dread_integer_4
     MODULE PROCEDURE h5dread_integer_5
     MODULE PROCEDURE h5dread_integer_6
     MODULE PROCEDURE h5dread_integer_7
     MODULE PROCEDURE h5dread_char_scalar
     MODULE PROCEDURE h5dread_char_1
     MODULE PROCEDURE h5dread_char_2
     MODULE PROCEDURE h5dread_char_3
     MODULE PROCEDURE h5dread_char_4
     MODULE PROCEDURE h5dread_char_5
     MODULE PROCEDURE h5dread_char_6
     MODULE PROCEDURE h5dread_char_7
     MODULE PROCEDURE h5dread_real_scalar
     MODULE PROCEDURE h5dread_real_1
     MODULE PROCEDURE h5dread_real_2
     MODULE PROCEDURE h5dread_real_3
     MODULE PROCEDURE h5dread_real_4
     MODULE PROCEDURE h5dread_real_5
     MODULE PROCEDURE h5dread_real_6
     MODULE PROCEDURE h5dread_real_7
  END INTERFACE

  INTERFACE h5dfill_f
     MODULE PROCEDURE h5dfill_integer
     MODULE PROCEDURE h5dfill_real
     MODULE PROCEDURE h5dfill_char
  END INTERFACE

CONTAINS

!****s* H5D/h5dread_f
!
! NAME
!  h5dread_f
!
! PURPOSE
!  Reads raw data from the specified dataset into buf,
!  converting from file datatype and dataspace to memory
!  datatype and dataspace.
!
! INPUTS
!  dset_id 	 - dataset identifier
!  mem_type_id 	 - memory type identifier
!  dims 	 - 1-dim array of size 7; dims(k) has the size
!   	           of k-th dimension of the buf array
! OUTPUTS
!  buf 	   - buffer to read data in
!  hdferr: - error code
!             Success:  0
!             Failure: -1
!
! OPTIONAL PARAMETERS
!  mem_space_id  - memory dataspace identifier
!  file_space_id - file dataspace identifier
!  xfer_prp 	 - trasfer property list identifier
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
!  of the TYPE(hobj_ref_t_f) and TYPE(hdset_reg_ref_t_f) types.
!*****
  SUBROUTINE h5dread_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    TYPE(hobj_ref_t_f), INTENT(INOUT) , &
         DIMENSION(dims(1)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    INTEGER(HADDR_T), ALLOCATABLE, DIMENSION(:) :: ref_buf
    INTEGER :: j

    INTERFACE
       INTEGER FUNCTION h5dread_ref_obj_c(dset_id, mem_type_id,&
            mem_space_id_default, &
            file_space_id_default, xfer_prp_default, ref_buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REF_OBJ_C'::h5dread_ref_obj_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER(HADDR_T), DIMENSION(*) :: ref_buf
       END FUNCTION h5dread_ref_obj_c
    END INTERFACE

    ALLOCATE(ref_buf(dims(1)), stat=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_ref_obj_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, ref_buf, dims)
    DO j = 1, dims(1)
       buf(j)%ref = ref_buf(j)
    ENDDO
    DEALLOCATE(ref_buf)
  END SUBROUTINE h5dread_reference_obj

  SUBROUTINE h5dread_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    TYPE(hdset_reg_ref_t_f), INTENT(INOUT), &
         DIMENSION(dims(1)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
    INTEGER :: i,j

    INTERFACE
       INTEGER FUNCTION h5dread_ref_reg_c(dset_id, mem_type_id,&
            mem_space_id_default, &
            file_space_id_default, xfer_prp_default, ref_buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REF_REG_C'::h5dread_ref_reg_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, DIMENSION(*) :: ref_buf
       END FUNCTION h5dread_ref_reg_c
    END INTERFACE

    ALLOCATE(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, ref_buf, dims)

    DO j = 1, dims(1)
       DO i = 1, REF_REG_BUF_LEN
          buf(j)%ref(i) = ref_buf(REF_REG_BUF_LEN*(j-1) + i)
       ENDDO
    ENDDO
    DEALLOCATE(ref_buf)
  END SUBROUTINE h5dread_reference_dsetreg

  SUBROUTINE h5dread_integer_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_integer_s_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_S_C'::h5dread_integer_s_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(OUT) :: buf
       END FUNCTION h5dread_integer_s_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_integer_s_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_integer_scalar

  SUBROUTINE h5dread_integer_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), DIMENSION(dims(1)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_integer_1_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_1_C'::h5dread_integer_1_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(OUT), DIMENSION(dims(1)) :: buf
       END FUNCTION h5dread_integer_1_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_integer_1_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_integer_1

  SUBROUTINE h5dread_integer_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), DIMENSION(dims(1),dims(2)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_integer_2_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_2_C'::h5dread_integer_2_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(OUT), DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dread_integer_2_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_integer_2_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dread_integer_2

  SUBROUTINE h5dread_integer_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_integer_3_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_3_C'::h5dread_integer_3_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(OUT), DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5dread_integer_3_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_integer_3_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dread_integer_3

  SUBROUTINE h5dread_integer_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_integer_4_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_4_C'::h5dread_integer_4_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(OUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5dread_integer_4_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_integer_4_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dread_integer_4

  SUBROUTINE h5dread_integer_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_integer_5_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_5_C'::h5dread_integer_5_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5dread_integer_5_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_integer_5_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dread_integer_5

  SUBROUTINE h5dread_integer_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default


    INTERFACE
       INTEGER FUNCTION h5dread_integer_6_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_6_C'::h5dread_integer_6_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5dread_integer_6_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_integer_6_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dread_integer_6

  SUBROUTINE h5dread_integer_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_integer_7_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_7_C'::h5dread_integer_7_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5dread_integer_7_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_integer_7_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dread_integer_7

  SUBROUTINE h5dread_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dreadc_s_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_S_C'::h5dreadc_s_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(OUT) :: buf
       END FUNCTION h5dreadc_s_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dreadc_s_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_char_scalar

  SUBROUTINE h5dread_char_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dreadc_1_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_1_C'::h5dreadc_1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5dreadc_1_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dreadc_1_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_char_1

  SUBROUTINE h5dread_char_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default


    INTERFACE
       INTEGER FUNCTION h5dreadc_2_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_2_C'::h5dreadc_2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dreadc_2_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dreadc_2_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_char_2

  SUBROUTINE h5dread_char_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dreadc_3_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_3_C'::h5dreadc_3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5dreadc_3_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dreadc_3_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_char_3

  SUBROUTINE h5dread_char_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dreadc_4_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_4_C'::h5dreadc_4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5dreadc_4_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dreadc_4_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_char_4

  SUBROUTINE h5dread_char_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dreadc_5_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_5_C'::h5dreadc_5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5dreadc_5_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dreadc_5_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_char_5

  SUBROUTINE h5dread_char_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dreadc_6_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_6_C'::h5dreadc_6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5dreadc_6_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dreadc_6_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_char_6

  SUBROUTINE h5dread_char_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dreadc_7_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_7_C'::h5dreadc_7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5dreadc_7_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dreadc_7_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_char_7

  SUBROUTINE h5dread_real_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_real_s_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_S_C'::h5dread_real_s_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(OUT) :: buf
       END FUNCTION h5dread_real_s_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_real_s_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_real_scalar

  SUBROUTINE h5dread_real_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dread_real_1_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_real_1_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_1_C'::h5dread_real_1_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5dread_real_1_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_real_1_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_real_1

  SUBROUTINE h5dread_real_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_real_2_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_2_C'::h5dread_real_2_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dread_real_2_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_real_2_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_real_2

  SUBROUTINE h5dread_real_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_real_3_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_3_C'::h5dread_real_3_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5dread_real_3_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_real_3_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_real_3

  SUBROUTINE h5dread_real_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3), dims(4)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_real_4_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_4_C'::h5dread_real_4_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3), dims(4)) :: buf
       END FUNCTION h5dread_real_4_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_real_4_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_real_4

  SUBROUTINE h5dread_real_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_real_5_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_5_C'::h5dread_real_5_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5dread_real_5_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_real_5_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_real_5

  SUBROUTINE h5dread_real_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_real_6_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_6_C'::h5dread_real_6_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5dread_real_6_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_real_6_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_real_6

  SUBROUTINE h5dread_real_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_real_7_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_7_C'::h5dread_real_7_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5dread_real_7_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_real_7_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_real_7

  SUBROUTINE h5dwrite_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf
    TYPE(hobj_ref_t_f), DIMENSION(dims(1)), INTENT(IN) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    INTEGER(HADDR_T), ALLOCATABLE, DIMENSION(:) :: ref_buf
    INTEGER :: j

    INTERFACE
       INTEGER FUNCTION h5dwrite_ref_obj_c(dset_id, mem_type_id,&
            mem_space_id_default, &
            file_space_id_default, xfer_prp_default, ref_buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REF_OBJ_C'::h5dwrite_ref_obj_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HADDR_T), DIMENSION(*) :: ref_buf
         INTEGER(HSIZE_T), DIMENSION(*) :: dims
       END FUNCTION h5dwrite_ref_obj_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    ALLOCATE(ref_buf(dims(1)), stat=hdferr)
    IF (hdferr .NE. 0 ) THEN
       hdferr = -1
       RETURN
    ELSE
       DO j = 1, dims(1)
          ref_buf(j) = buf(j)%ref
       ENDDO
    ENDIF
    hdferr = h5dwrite_ref_obj_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, ref_buf, dims(1))
    DEALLOCATE(ref_buf)

  END SUBROUTINE h5dwrite_reference_obj

  SUBROUTINE h5dwrite_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf
    TYPE(hdset_reg_ref_t_f), DIMENSION(dims(1)), INTENT(IN) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
    INTEGER :: i,j

    INTERFACE
       INTEGER FUNCTION h5dwrite_ref_reg_c(dset_id, mem_type_id,&
            mem_space_id_default, &
            file_space_id_default, xfer_prp_default, ref_buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REF_REG_C'::h5dwrite_ref_reg_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER, DIMENSION(*) :: ref_buf
         INTEGER(HSIZE_T), DIMENSION(*) ::  dims
       END FUNCTION h5dwrite_ref_reg_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    ALLOCATE(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
    IF (hdferr .NE. 0 ) THEN
       hdferr = -1
       RETURN
    ELSE
       DO j = 1, dims(1)
          DO i = 1, REF_REG_BUF_LEN
             ref_buf(REF_REG_BUF_LEN*(j-1) + i) = buf(j)%ref(i)
          ENDDO
       ENDDO
    ENDIF
    hdferr = h5dwrite_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, ref_buf, dims)
    DEALLOCATE(ref_buf)

  END SUBROUTINE h5dwrite_reference_dsetreg

  SUBROUTINE h5dwrite_integer_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER, INTENT(IN) :: buf ! Data buffer
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_integer_s_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_S_C'::h5dwrite_integer_s_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(IN) :: buf
       END FUNCTION h5dwrite_integer_s_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_integer_s_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_integer_scalar

  SUBROUTINE h5dwrite_integer_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_integer_1_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_1_C'::h5dwrite_integer_1_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5dwrite_integer_1_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_integer_1_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_integer_1

  SUBROUTINE h5dwrite_integer_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2)) :: buf   ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_integer_2_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_2_C'::h5dwrite_integer_2_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dwrite_integer_2_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id
    hdferr = h5dwrite_integer_2_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dwrite_integer_2

  SUBROUTINE h5dwrite_integer_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_integer_3_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_3_C'::h5dwrite_integer_3_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5dwrite_integer_3_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_integer_3_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dwrite_integer_3

  SUBROUTINE h5dwrite_integer_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_integer_4_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_4_C'::h5dwrite_integer_4_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5dwrite_integer_4_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_integer_4_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dwrite_integer_4

  SUBROUTINE h5dwrite_integer_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_integer_5_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_5_C'::h5dwrite_integer_5_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5dwrite_integer_5_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F


    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_integer_5_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dwrite_integer_5

  SUBROUTINE h5dwrite_integer_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_integer_6_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_6_C'::h5dwrite_integer_6_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5dwrite_integer_6_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_integer_6_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dwrite_integer_6

  SUBROUTINE h5dwrite_integer_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_integer_7_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_7_C'::h5dwrite_integer_7_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5dwrite_integer_7_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_integer_7_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims)

  END SUBROUTINE h5dwrite_integer_7


  SUBROUTINE h5dwrite_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwritec_s_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_S_C'::h5dwritec_s_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(IN) :: buf
       END FUNCTION h5dwritec_s_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwritec_s_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_char_scalar

  SUBROUTINE h5dwrite_char_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwritec_1_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_1_C'::h5dwritec_1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5dwritec_1_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwritec_1_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_char_1

  SUBROUTINE h5dwrite_char_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwritec_2_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_2_C'::h5dwritec_2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dwritec_2_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwritec_2_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_char_2

  SUBROUTINE h5dwrite_char_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwritec_3_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_3_C'::h5dwritec_3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5dwritec_3_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwritec_3_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_char_3

  SUBROUTINE h5dwrite_char_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwritec_4_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_4_C'::h5dwritec_4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5dwritec_4_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwritec_4_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_char_4

  SUBROUTINE h5dwrite_char_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwritec_5_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_5_C'::h5dwritec_5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5dwritec_5_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwritec_5_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_char_5

  SUBROUTINE h5dwrite_char_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwritec_6_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_6_C'::h5dwritec_6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5dwritec_6_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwritec_6_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_char_6

  SUBROUTINE h5dwrite_char_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwritec_7_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_7_C'::h5dwritec_7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5dwritec_7_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwritec_7_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_char_7

  SUBROUTINE h5dwrite_real_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_real_s_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_S_C'::h5dwrite_real_s_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(IN) :: buf
       END FUNCTION h5dwrite_real_s_c
    END INTERFACE

    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_real_s_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_real_scalar

  SUBROUTINE h5dwrite_real_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_real_1_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_1_C'::h5dwrite_real_1_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5dwrite_real_1_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_real_1_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_real_1

  SUBROUTINE h5dwrite_real_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_real_2_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_2_C'::h5dwrite_real_2_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dwrite_real_2_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_real_2_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_real_2

  SUBROUTINE h5dwrite_real_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_real_3_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_3_C'::h5dwrite_real_3_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5dwrite_real_3_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_real_3_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_real_3

  SUBROUTINE h5dwrite_real_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_real_4_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_4_C'::h5dwrite_real_4_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5dwrite_real_4_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_real_4_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_real_4

  SUBROUTINE h5dwrite_real_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_real_5_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_5_C'::h5dwrite_real_5_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5dwrite_real_5_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_real_5_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_real_5

  SUBROUTINE h5dwrite_real_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_real_6_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_6_C'::h5dwrite_real_6_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5dwrite_real_6_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_real_6_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_real_6

  SUBROUTINE h5dwrite_real_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_real_7_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_7_C'::h5dwrite_real_7_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5dwrite_real_7_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_real_7_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_real_7

!
! NAME
!  h5dfill_integer
!
! PURPOSE
!  Fills dataspace elements with a fill value in a memory buffer.
!  Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes
!  of the fillvalues and buffers are supported. Buffer and fillvalue
!  are assumed to have the same datatype.
!  Only one-dimesional buffers are supported.
!
! INPUTS
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! OUTPUTS
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
!

  SUBROUTINE h5dfill_integer(fill_value, space_id, buf, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    INTEGER, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    !            INTEGER, EXTERNAL :: h5dfill_integer_c
    ! MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dfill_integer_c(fill_value, fill_type_id, space_id, &
            buf, mem_type_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DFILL_INTEGER_C'::h5dfill_integer_c
         !DEC$ENDIF
         INTEGER, INTENT(IN) :: fill_value  ! Fill value
         INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
         INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
         INTEGER, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
         INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier
       END FUNCTION h5dfill_integer_c
    END INTERFACE
    fill_type_id = H5T_NATIVE_INTEGER
    mem_type_id  = H5T_NATIVE_INTEGER

    hdferr = h5dfill_integer_c(fill_value, fill_type_id, space_id, &
         buf, mem_type_id)

  END SUBROUTINE h5dfill_integer

!
! NAME
!  h5dfill_real
!
! PURPOSE
!  Fills dataspace elements with a fill value in a memory buffer.
!  Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes
!  of the fillvalues and buffers are supported. Buffer and fillvalue
!  are assumed to have the same datatype.
!  Only one-dimesional buffers are supported.
!
! INPUTS
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! OUTPUTS
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
!

  SUBROUTINE h5dfill_real(fill_valuer, space_id, buf, hdferr)
    IMPLICIT NONE
    REAL, INTENT(IN) :: fill_valuer  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    REAL, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    !            INTEGER, EXTERNAL :: h5dfill_real_c
    ! MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dfill_real_c(fill_valuer, fill_type_id, space_id, &
            buf, mem_type_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DFILL_REAL_C'::h5dfill_real_c
         !DEC$ENDIF
         REAL, INTENT(IN) :: fill_valuer  ! Fill value
         INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
         INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
         REAL, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
         INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier
       END FUNCTION h5dfill_real_c
    END INTERFACE
    fill_type_id = H5T_NATIVE_REAL
    mem_type_id  = H5T_NATIVE_REAL

    hdferr = h5dfill_real_c(fill_valuer, fill_type_id, space_id, &
         buf, mem_type_id)
  END SUBROUTINE h5dfill_real

!
! NAME
!  h5dfill_char
!
! PURPOSE
!  Fills dataspace elements with a fill value in a memory buffer.
!  Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes
!  of the fillvalues and buffers are supported. Buffer and fillvalue
!  are assumed to have the same datatype.
!  Only one-dimesional buffers are supported.
!
! INPUTS
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! OUTPUTS
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
!

  SUBROUTINE h5dfill_char(fill_value, space_id, buf, hdferr)
    IMPLICIT NONE
    CHARACTER, INTENT(IN) :: fill_value    ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    CHARACTER, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id  ! Buffer dadtype identifier

    !            INTEGER, EXTERNAL :: h5dfillc_c
    ! MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dfillc_c(fill_value, fill_type_id, space_id, &
            buf, mem_type_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DFILLC_C'::h5dfillc_c
         !DEC$ENDIF
         CHARACTER, INTENT(IN) :: fill_value  ! Fill value
         INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
         INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
         CHARACTER, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
         INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier
       END FUNCTION h5dfillc_c
    END INTERFACE
    fill_type_id = H5T_NATIVE_CHARACTER
    mem_type_id  = H5T_NATIVE_CHARACTER

    hdferr = h5dfillc_c(fill_value, fill_type_id, space_id, &
         buf, mem_type_id)

  END SUBROUTINE h5dfill_char


END MODULE H5D_PROVISIONAL
