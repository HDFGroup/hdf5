!****h* ROBODoc/H5D (F03)
!
! NAME
!  H5D_PROVISIONAL
!
! PURPOSE
!  This file contains Fortran 90 and Fortran 2003 interfaces for H5D functions.
!  It contains the same functions as H5Dff_F90.f90 but includes the
!  Fortran 2003 functions and the interface listings. This file will be compiled
!  instead of H5Dff_F90.f90 if Fortran 2003 functions are enabled.
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
! NOTES
!  (1) The maximum rank of an array allowed in Fortran is 7, therefore
!  we only provide an interface for arrays up to and including rank 7.
!
!  (2) Unfortunately we are using a generic interface and one of the factors
!  used in determining the proper routine to select is that of the array
!  rank being passed. Therefore, we can not create just one subroutine for
!  each array type (integer, real, etc...) and use a
!  rank 1 array of assumed size to handle multiple ranks, i.e.
!  (i.e. integer, dimension(*) :: ... )
!  (i.e. real   , dimension(*) :: ... ) etc...
!
!  (3) Could not place the USE, INTRINSIC :: ISO_C_BINDING in the module header because it may
!  conflict with the USE, INTRINSIC :: ISO_C_BINDING included in the user's program. Moved
!  the statement instead to each subroutine.
!
!
!  (4) C_LOC and character strings according to the Fortran 2003 standard:
!
!  15.1.2.5 C_LOC(X)
!
!  Argument. X shall either
!
!  (A) have interoperable type and type parameters and be
!  (a) a variable that has the TARGET attribute and is interoperable,
!  (b) an allocated allocatable variable that has the TARGET attribute
!  and is not an array of zero size, or
!  (c) an associated scalar pointer, or
!  (B) be a nonpolymorphic scalar, have no length type parameters, and be
!  (a) a nonallocatable, nonpointer variable that has the TARGET attribute,
!  (b) an allocated allocatable variable that has the TARGET attribute, or
!  (c) an associated pointer.
!
!   	 - When X is a character, for interoperability the standard is:
!
!  15.2.1 Interoperability of intrinsic types
!
!  ...if the type is character, interoperability also requires that the length type parameter
!  be omitted or be specified by an initialization expression whose value is one.
!
!  THEREFORE compilers that have not extended the standard require the
!  argument in C_LOC to be of the variant:
!
!  CHARACTER(LEN=1), TARGET :: chr
!  or
!  CHARACTER, TARGET :: chr
!  
!                         *** IMPORTANT ***
!  If you add a new H5D function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
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

     ! This is the preferred way to call h5dwrite
     ! by passing an address
     MODULE PROCEDURE h5dwrite_ptr

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

     ! This is the preferred way to call h5dread
     ! by passing an address
     MODULE PROCEDURE h5dread_ptr

  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Dwrite routine

  INTERFACE
     INTEGER FUNCTION h5dwrite_f_c(dset_id, mem_type_id, &
          mem_space_id_default ,                         &
          file_space_id_default,                         &
          xfer_prp_default, buf )
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_F_C'::h5dwrite_f_c
       !DEC$ENDIF
       INTEGER(HID_T), INTENT(IN) :: dset_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       INTEGER(HID_T) :: mem_space_id_default
       INTEGER(HID_T) :: file_space_id_default
       INTEGER(HID_T) :: xfer_prp_default
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5dwrite_f_c
  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Dread routine

  INTERFACE
     INTEGER FUNCTION h5dread_f_c(dset_id, mem_type_id, &
          mem_space_id_default,                         &
          file_space_id_default,                        &
          xfer_prp_default, buf)
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_F_C'::h5dread_f_c
       !DEC$ENDIF
       INTEGER(HID_T), INTENT(IN) :: dset_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       INTEGER(HID_T) :: mem_space_id_default
       INTEGER(HID_T) :: file_space_id_default
       INTEGER(HID_T) :: xfer_prp_default
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5dread_f_c
  END INTERFACE

  INTERFACE h5dfill_f
     MODULE PROCEDURE h5dfill_integer
     MODULE PROCEDURE h5dfill_real
     MODULE PROCEDURE h5dfill_char
  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Dfill routine

  INTERFACE
     INTEGER FUNCTION h5dfill_c(f_ptr_fill_value, fill_type_id, space_id, &
          f_ptr_buf, mem_type_id)
       USE H5GLOBAL
       USE, INTRINSIC :: ISO_C_BINDING
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DFILL_C'::h5dfill_c
       !DEC$ENDIF
       TYPE(C_PTR), VALUE :: f_ptr_fill_value
       INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
       INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
       TYPE(C_PTR), VALUE :: f_ptr_buf
       INTEGER(HID_T) :: mem_type_id
     END FUNCTION h5dfill_c
  END INTERFACE

CONTAINS

  SUBROUTINE h5dwrite_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf
    TYPE(hobj_ref_t_f), DIMENSION(dims(1)), INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)


  END SUBROUTINE h5dwrite_reference_obj

  SUBROUTINE h5dwrite_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf
    TYPE(hdset_reg_ref_t_f), DIMENSION(dims(1)), INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
    INTEGER :: i
    INTEGER(HSIZE_T) :: j
    TYPE(C_PTR) :: f_ptr
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

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

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
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER, INTENT(IN), TARGET :: buf  ! Data buffer
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_scalar

  SUBROUTINE h5dwrite_integer_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_1

  SUBROUTINE h5dwrite_integer_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2)),TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_2

  SUBROUTINE h5dwrite_integer_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_3

  SUBROUTINE h5dwrite_integer_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr            ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_4

  SUBROUTINE h5dwrite_integer_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_5

  SUBROUTINE h5dwrite_integer_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_6

  SUBROUTINE h5dwrite_integer_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_integer_7

  SUBROUTINE h5dwrite_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(*), INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
                                 
    CALL h5dwrite_char_scalar_fix(dset_id, mem_type_id, buf, LEN(buf), dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)

  END SUBROUTINE h5dwrite_char_scalar

  SUBROUTINE h5dwrite_char_scalar_fix(dset_id, mem_type_id, buf, buf_len, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN) :: buf_len
    CHARACTER(LEN=buf_len), INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1:1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_scalar_fix

  SUBROUTINE h5dwrite_char_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1)(1:1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_1

  SUBROUTINE h5dwrite_char_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1)(1:1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_2

  SUBROUTINE h5dwrite_char_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1,1)(1:1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_3

  SUBROUTINE h5dwrite_char_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1,1,1)(1:1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_4

  SUBROUTINE h5dwrite_char_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1,1,1,1)(1:1))
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_5

  SUBROUTINE h5dwrite_char_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1,1,1,1,1)(1:1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_6

  SUBROUTINE h5dwrite_char_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1)(1:1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_7


  SUBROUTINE h5dwrite_real_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_scalar


  SUBROUTINE h5dwrite_real_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_1

  SUBROUTINE h5dwrite_real_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_2

  SUBROUTINE h5dwrite_real_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_3

  SUBROUTINE h5dwrite_real_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_4

  SUBROUTINE h5dwrite_real_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_5

  SUBROUTINE h5dwrite_real_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_6

  SUBROUTINE h5dwrite_real_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_real_7

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
  SUBROUTINE h5dread_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    TYPE(hobj_ref_t_f), INTENT(INOUT) , &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_reference_obj

  SUBROUTINE h5dread_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    TYPE(hdset_reg_ref_t_f), INTENT(INOUT), &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
    INTEGER :: i
    INTEGER(HSIZE_T) :: j
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

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

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
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT) , TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)


  END SUBROUTINE h5dread_integer_scalar

  SUBROUTINE h5dread_integer_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_1

  SUBROUTINE h5dread_integer_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_2

  SUBROUTINE h5dread_integer_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_3

  SUBROUTINE h5dread_integer_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_4

  SUBROUTINE h5dread_integer_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_5

  SUBROUTINE h5dread_integer_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_6

  SUBROUTINE h5dread_integer_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_integer_7

  SUBROUTINE h5dread_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id         ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id   ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT) :: buf     ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    CALL h5dread_char_scalar_fix(dset_id, mem_type_id, buf, LEN(buf), hdferr, &
         mem_space_id_default, file_space_id_default, xfer_prp_default)

  END SUBROUTINE h5dread_char_scalar

  SUBROUTINE h5dread_char_scalar_fix(dset_id, mem_type_id, buf, buf_len, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER, INTENT(IN)  :: buf_len
    CHARACTER(LEN=buf_len), INTENT(INOUT), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1:1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id, &
         file_space_id, xfer_prp, f_ptr)

  END SUBROUTINE h5dread_char_scalar_fix

  SUBROUTINE h5dread_char_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1)(1:1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_1

  SUBROUTINE h5dread_char_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1)(1:1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_2

  SUBROUTINE h5dread_char_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1,1)(1:1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_3

  SUBROUTINE h5dread_char_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1,1,1)(1:1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_4

  SUBROUTINE h5dread_char_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1,1,1,1)(1:1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_5

  SUBROUTINE h5dread_char_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1,1,1,1,1)(1:1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_6

  SUBROUTINE h5dread_char_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1,1,1,1,1,1,1)(1:1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_char_7

  SUBROUTINE h5dread_real_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT) , TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_scalar

  SUBROUTINE h5dread_real_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_1

  SUBROUTINE h5dread_real_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_2

  SUBROUTINE h5dread_real_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_3

  SUBROUTINE h5dread_real_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3), dims(4)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_4

  SUBROUTINE h5dread_real_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_5

  SUBROUTINE h5dread_real_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_6

  SUBROUTINE h5dread_real_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) , TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_real_7

!****s* H5D (F03)/h5dwrite_f_F03
!
! NAME		
!  h5dwrite_f_F03
!
! PURPOSE
!  Writes raw data from a dataset into a buffer. 
!
! Inputs:
!  dset_id	 - Identifier of the dataset to write to.
!  mem_type_id	 - Identifier of the memory datatype.
!  buf		 - Buffer with data to be written to the file.
!  
! Outputs:
!  hdferr        - Returns 0 if successful and -1 if fails
!
! Optional parameters:
!  mem_space_id	 - Identifier of the memory dataspace.
!  file_space_id - Identifier of the dataset's dataspace in the file.
!  xfer_prp	 - Identifier of a transfer property list for this I/O operation.
!
! AUTHOR
!  M. Scot Breitenfeld
!  September 17, 2011
!
! Fortran2003 Interface:
!!  SUBROUTINE h5dwrite_f(dset_id, mem_type_id, buf, hdferr, &
!!                        mem_space_id, file_space_id, xfer_prp)
!!    INTEGER(HID_T), INTENT(IN)              :: dset_id
!!    INTEGER(HID_T), INTENT(IN)              :: mem_type_id
!!    TYPE(C_PTR)   , INTENT(IN)              :: buf
!!    INTEGER       , INTENT(OUT)             :: hdferr
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: mem_space_id
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: file_space_id
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: xfer_prp
!*****
  SUBROUTINE h5dwrite_ptr(dset_id, mem_type_id, buf, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    TYPE(C_PTR), INTENT(IN) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf)

  END SUBROUTINE h5dwrite_ptr
!****s* H5D (F03)/h5dread_f_F03
!
! NAME		
!  h5dread_f_F03
!
! PURPOSE
!  Reads raw data from a dataset into a buffer. 
!
! Inputs:
!  dset_id	 - Identifier of the dataset read from.
!  mem_type_id	 - Identifier of the memory datatype.
!  
! Outputs:
!  buf		 - Buffer to receive data read from file.
!  hdferr        - Returns 0 if successful and -1 if fails
!
! Optional parameters:
!  mem_space_id	 - Identifier of the memory dataspace.
!  file_space_id - Identifier of the dataset's dataspace in the file.
!  xfer_prp	 - Identifier of a transfer property list for this I/O operation.
!
! AUTHOR
!  M. Scot Breitenfeld
!  September 17, 2011
!
! Fortran2003 Interface:
!!  SUBROUTINE h5dread_f(dset_id, mem_type_id, buf, hdferr, &
!!                       mem_space_id, file_space_id, xfer_prp)
!!    INTEGER(HID_T), INTENT(IN)              :: dset_id
!!    INTEGER(HID_T), INTENT(IN)              :: mem_type_id
!!    TYPE(C_PTR)   , INTENT(INOUT)           :: buf
!!    INTEGER       , INTENT(OUT)             :: hdferr
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: mem_space_id
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: file_space_id
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: xfer_prp
!*****
  SUBROUTINE h5dread_ptr(dset_id, mem_type_id, buf, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    TYPE(C_PTR), INTENT(INOUT) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf)

  END SUBROUTINE h5dread_ptr

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
! Inputs:
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
!

  SUBROUTINE h5dfill_integer(fill_value, space_id, buf,  hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER, INTENT(IN), TARGET :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    INTEGER, INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id  ! Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf        ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf(1))

    fill_type_id = H5T_NATIVE_INTEGER
    mem_type_id  = H5T_NATIVE_INTEGER

    hdferr = h5dfill_c(f_ptr_fill_value, fill_type_id, space_id, &
         f_ptr_buf, mem_type_id)

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
! Inputs:
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
  SUBROUTINE h5dfill_real(fill_valuer, space_id, buf,  hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    REAL, INTENT(IN), TARGET :: fill_valuer  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    REAL, INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_valuer ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_valuer = C_LOC(fill_valuer)
    f_ptr_buf = C_LOC(buf(1))

    fill_type_id = H5T_NATIVE_REAL
    mem_type_id  = H5T_NATIVE_REAL

    hdferr = h5dfill_c(f_ptr_fill_valuer, fill_type_id, space_id, &
         f_ptr_buf, mem_type_id)

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
! Inputs:
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
  SUBROUTINE h5dfill_char(fill_value, space_id, buf,  hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    CHARACTER, INTENT(IN), TARGET :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    CHARACTER, INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf(1))

    hdferr = h5dfill_c(f_ptr_fill_value, fill_type_id, space_id, &
         f_ptr_buf, mem_type_id)

  END SUBROUTINE h5dfill_char
!
!****s* H5D (F03)/h5dvlen_reclaim_f
! NAME
!  h5dvlen_reclaim_f
!
! PURPOSE 
!  Reclaims VL datatype memory buffers. 
!
! Inputs:
!
!  type_id  - Identifier of the datatype. 
!  space_id - Identifier of the dataspace. 
!  plist_id - Identifier of the property list used to create the buffer. 
!  buf      - Pointer to the buffer to be reclaimed. 
!
! Outputs:
!  hdferr   - Returns 0 if successful and -1 if fails
!
! AUTHOR
! M. Scot Breitenfeld
! January 11, 2011
!
! Fortran2003 Interface:
  SUBROUTINE h5dvlen_reclaim_f(type_id, space_id, plist_id, buf, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)    :: type_id
    INTEGER(HID_T), INTENT(IN)    :: space_id
    INTEGER(HID_T), INTENT(IN)    :: plist_id
    TYPE(C_PTR)   , INTENT(INOUT) :: buf
    INTEGER       , INTENT(OUT)   :: hdferr
!*****

    INTERFACE
       INTEGER FUNCTION h5dvlen_reclaim_c(type_id, space_id, plist_id, buf)
         USE H5GLOBAL
         USE, INTRINSIC :: ISO_C_BINDING
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DVLEN_RECLAIM_C'::h5dvlen_reclaim_c
         !DEC$ENDIF
         INTEGER(HID_T) :: type_id
         INTEGER(HID_T) :: space_id
         INTEGER(HID_T) :: plist_id
         TYPE(C_PTR), VALUE :: buf
       END FUNCTION h5dvlen_reclaim_c
    END INTERFACE

    hdferr = H5Dvlen_reclaim_c(type_id, space_id, plist_id, buf)

  END SUBROUTINE H5Dvlen_reclaim_f

END MODULE H5D_PROVISIONAL


