!!
!! @see H5R, C-API
!!
!! @see @ref H5R_UG, User Guide
!!

!> @ingroup FH5R
!!
!! @brief This module contains Fortran interfaces for H5R functions.
!
! NAME
!  MODULE H5R
!
! FILE
!  fortran/src/H5Rff.F90
!
! PURPOSE
!  This file contains Fortran interfaces for H5R functions.
!
! COPYRIGHT
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
!  If you add a new H5R function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5R

  USE H5GLOBAL
  USE H5fortkit

  IMPLICIT NONE

  ! If you change the value of these parameters, do not forget to change corresponding
  ! values in the H5f90.h file.
  !        INTEGER, PARAMETER :: REF_OBJ_BUF_LEN = 2
  !        INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3
  !
  !        TYPE hobj_ref_t_f
  !             INTEGER ref(REF_OBJ_BUF_LEN)
  !        END TYPE
  !
  !        TYPE hdset_reg_ref_t_f
  !             INTEGER ref(REF_REG_BUF_LEN)
  !        END TYPE
  !

  PRIVATE h5rget_object_type_obj_f
  PRIVATE h5rget_region_region_f, h5rget_region_ptr_f
  PRIVATE h5rcreate_object_deprec_f, h5rcreate_region_deprec_f, h5rcreate_ptr_f
  PRIVATE h5rdereference_object_f, h5rdereference_region_f, h5rdereference_ptr_f
  PRIVATE h5rget_name_object_f, h5rget_name_region_f, h5rget_name_ptr_f

!> @brief hdset_reg_ref_t_f03 C compatible reference
  TYPE :: hdset_reg_ref_t_f03
     INTEGER(C_SIGNED_CHAR), DIMENSION(1:H5R_DSET_REG_REF_BUF_SIZE_F) :: ref
  END TYPE hdset_reg_ref_t_f03

  TYPE, BIND(C) :: H5R_ref_t
      INTEGER(C_INT8_T), DIMENSION(1:H5R_REF_BUF_SIZE_F) :: data
  END TYPE

  INTERFACE h5rget_object_type_f
     MODULE PROCEDURE h5rget_object_type_obj_f
  END INTERFACE

#ifndef H5_DOXYGEN

  INTERFACE h5rget_region_f
     MODULE PROCEDURE h5rget_region_ptr_f    ! F2003
     MODULE PROCEDURE h5rget_region_region_f ! obsolete
  END INTERFACE

  INTERFACE h5rcreate_f
     MODULE PROCEDURE h5rcreate_ptr_f           ! F2003
     MODULE PROCEDURE h5rcreate_object_deprec_f ! obsolete
     MODULE PROCEDURE h5rcreate_region_deprec_f ! obsolete
  END INTERFACE

  INTERFACE h5rdereference_f
     MODULE PROCEDURE h5rdereference_ptr_f    ! F2003
     MODULE PROCEDURE h5rdereference_object_f ! obsolete
     MODULE PROCEDURE h5rdereference_region_f ! obsolete
  END INTERFACE

  INTERFACE h5rget_name_f
     MODULE PROCEDURE h5rget_name_ptr_f    ! F2003
     MODULE PROCEDURE h5rget_name_object_f ! obsolete
     MODULE PROCEDURE h5rget_name_region_f ! obsolete
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5rget_name_ptr_c(loc_id, ref_type, ref, name, name_len, size_default) &
          BIND(C, NAME='h5rget_name_ptr_c')
       IMPORT :: c_char, c_ptr
       IMPORT :: HID_T, SIZE_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: loc_id
       INTEGER, INTENT(IN) :: ref_type
       TYPE(C_PTR), INTENT(IN), VALUE :: ref
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
       INTEGER(SIZE_T) :: name_len
       INTEGER(SIZE_T) :: size_default
     END FUNCTION h5rget_name_ptr_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5rdereference_ptr_c(obj_id, ref_type, ref, ref_obj_id) &
          BIND(C, NAME='h5rdereference_ptr_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: obj_id
       INTEGER, INTENT(IN) :: ref_type
       TYPE(C_PTR), INTENT(IN), VALUE :: ref
       INTEGER(HID_T), INTENT(OUT) :: ref_obj_id
     END FUNCTION h5rdereference_ptr_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5rcreate_ptr_c(ref, loc_id, name, namelen, ref_type, space_id) &
          BIND(C, NAME='h5rcreate_ptr_c')
       IMPORT :: c_ptr, c_char
       IMPORT :: HID_T
       IMPLICIT NONE
       TYPE(C_PTR), VALUE :: ref
       INTEGER(HID_T), INTENT(IN) :: loc_id
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
       INTEGER :: namelen
       INTEGER, INTENT(IN) :: ref_type
       INTEGER(HID_T), INTENT(IN) :: space_id
     END FUNCTION h5rcreate_ptr_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5rget_region_ptr_c(dset_id, ref, space_id) &
          BIND(C, NAME='h5rget_region_ptr_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: dset_id
       TYPE(C_PTR), VALUE :: ref
       INTEGER(HID_T), INTENT(OUT) :: space_id
     END FUNCTION h5rget_region_ptr_c
  END INTERFACE
#endif

CONTAINS

!>
!! \ingroup FH5R
!!
!! \brief Retrieves the type of object that an object reference points to.
!!
!! \attention  \fortran_obsolete
!!
!! \param dset_id  Identifier of the dataset containing reference to the objects.
!! \param ref      Reference to open.
!! \param obj_type Object_type, possible values:
!!                 \li H5G_UNKNOWN_F
!!                 \li H5G_GROUP_F
!!                 \li H5G_DATASET_F
!!                 \li H5G_TYPE_F
!! \param hdferr   \fortran_error
!!
#ifdef H5_DOXYGEN
  SUBROUTINE h5rget_object_type_f(&
#else
  SUBROUTINE h5rget_object_type_obj_f(&
#endif
    dset_id, ref, obj_type, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    TYPE(hobj_ref_t_f), INTENT(IN) :: ref
    INTEGER, INTENT(OUT) :: obj_type
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HADDR_T) :: ref_f          ! Local buffer to pass reference

    INTERFACE
       INTEGER FUNCTION h5rget_object_type_obj_c(dset_id, ref_f, obj_type) BIND(C, NAME='h5rget_object_type_obj_c')
         IMPORT :: HID_T, HADDR_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HADDR_T) :: ref_f
         INTEGER, INTENT(OUT) :: obj_type
       END FUNCTION h5rget_object_type_obj_c
    END INTERFACE

    ref_f = ref%ref
    hdferr = h5rget_object_type_obj_c(dset_id, ref_f, obj_type )

#ifdef H5_DOXYGEN
  END SUBROUTINE h5rget_object_type_f
#else
  END SUBROUTINE h5rget_object_type_obj_f
#endif

!>
!! \ingroup FH5R
!!
!! \brief Retrieves a dataspace with the specified region selected.
!!
!! \attention  \fortran_obsolete
!!
!! \param dset_id  Identifier of the dataset containing reference to the regions.
!! \param ref      Reference to open.
!! \param space_id Dataspace identifier.
!! \param hdferr   \fortran_error
!!
  SUBROUTINE h5rget_region_region_f(dset_id, ref, space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    TYPE(hdset_reg_ref_t_f), INTENT(IN) :: ref
    INTEGER(HID_T), INTENT(OUT) :: space_id
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER :: ref_f(REF_REG_BUF_LEN)          ! Local buffer to pass reference

    INTERFACE
       INTEGER FUNCTION h5rget_region_region_c(dset_id, ref_f, space_id) BIND(C, NAME='h5rget_region_region_c')
         IMPORT :: HID_T, REF_REG_BUF_LEN
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER :: ref_f(REF_REG_BUF_LEN)
         INTEGER(HID_T), INTENT(OUT) :: space_id
       END FUNCTION h5rget_region_region_c
    END INTERFACE

    ref_f = ref%ref
    hdferr = h5rget_region_region_c(dset_id, ref_f, space_id )

  END SUBROUTINE h5rget_region_region_f

!>
!! \ingroup FH5R
!!
!! \brief Retrieves a dataspace with the specified region selected using pointer.
!!
!! \attention  \fortran_approved
!!
!! \param dset_id  Identifier of the dataset containing reference to the regions.
!! \param ref      Reference to open.
!! \param space_id Dataspace identifier.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Rget_region()
!!
#ifdef H5_DOXYGEN
  SUBROUTINE h5rget_region_f(&
#else
  SUBROUTINE h5rget_region_ptr_f(&
#endif
       dset_id, ref, space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    TYPE(C_PTR), INTENT(IN) :: ref
    INTEGER(HID_T), INTENT(OUT) :: space_id
    INTEGER, INTENT(OUT) :: hdferr

    hdferr = h5rget_region_ptr_c(dset_id, ref, space_id)

#ifdef H5_DOXYGEN
  END SUBROUTINE h5rget_region_f
#else
  END SUBROUTINE h5rget_region_ptr_f
#endif

!>
!! \ingroup FH5R
!!
!! \brief Creates reference to the object.
!!
!! \attention  \fortran_obsolete
!!
!! \param loc_id Location identifier.
!! \param name   Name of the object at the specified location.
!! \param ref    Reference to the specified object.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Rcreate_object()
!!
  SUBROUTINE h5rcreate_object_deprec_f(loc_id, name, ref, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(hobj_ref_t_f), INTENT(INOUT), TARGET :: ref
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen                     ! Name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(ref)

    namelen = LEN(name)

    hdferr = h5rcreate_ptr_c(f_ptr, loc_id, name, namelen, INT(0), INT(-1,HID_T))

  END SUBROUTINE h5rcreate_object_deprec_f

!>
!! \ingroup FH5R
!!
!! \brief Creates reference to the dataset region
!!
!! \attention  \fortran_obsolete
!!
!! \param loc_id   Location identifier.
!! \param name     Name of the dataset at the specified location.
!! \param space_id Dataspace identifier that describes selected region.
!! \param ref      Reference to the dataset region.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Rcreate_region()
!!
  SUBROUTINE h5rcreate_region_deprec_f(loc_id, name, space_id, ref, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(IN) :: space_id
    TYPE(hdset_reg_ref_t_f), INTENT(OUT) :: ref
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen                     ! Name length
    INTEGER :: ref_f(REF_REG_BUF_LEN)      ! Local buffer to pass reference

    INTERFACE
       INTEGER FUNCTION h5rcreate_region_c(ref_f, loc_id, name, namelen, space_id) BIND(C,NAME='h5rcreate_region_c')
         IMPORT :: HID_T, REF_REG_BUF_LEN
         IMPORT :: C_CHAR
         IMPLICIT NONE
         INTEGER :: ref_f(REF_REG_BUF_LEN)
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(HID_T), INTENT(IN) :: space_id
       END FUNCTION h5rcreate_region_c
    END INTERFACE

    namelen = LEN(name)
    ref_f = 0
    hdferr = h5rcreate_region_c(ref_f, loc_id, name, namelen, space_id )
    ref%ref = ref_f

  END SUBROUTINE h5rcreate_region_deprec_f

!>
!! \ingroup FH5R
!!
!! \brief Creates a reference.
!!
!! \attention  \fortran_approved
!!
!! \param loc_id   Location identifier.
!! \param name     Name of the dataset at the specified location.
!! \param ref_type Type of reference:
!!                 \li H5R_OBJECT_F
!!                 \li H5T_STD_REF_DSETREG_F
!! \param ref      Reference created by the function call.
!! \param hdferr   \fortran_error
!! \param space_id Dataspace identifier that describes selected region.
!!
#ifdef H5_DOXYGEN
!! See C API: @ref H5Rcreate_object()
!!
  SUBROUTINE h5rcreate_f(&
#else
  SUBROUTINE h5rcreate_ptr_f(&
#endif
       loc_id, name, ref_type, ref, hdferr, space_id)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: ref_type
    TYPE(C_PTR), INTENT(INOUT) :: ref
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: space_id
    INTEGER :: namelen                       ! Name length
    INTEGER(HID_T) :: space_id_c

    namelen = LEN(name)
    space_id_c = -1
    IF(PRESENT(space_id)) space_id_c =  space_id
    hdferr = h5rcreate_ptr_c(ref, loc_id, name, namelen, ref_type, space_id_c)

#ifdef H5_DOXYGEN
  END SUBROUTINE h5rcreate_f
#else
  END SUBROUTINE h5rcreate_ptr_f
#endif
!>
!! \ingroup FH5R
!!
!! \brief Opens the HDF5 object referenced
!!
!! \attention  \fortran_obsolete
!!
!! \param obj_id     Identifier of the dataset containing reference.
!! \param ref        Reference to open.
!! \param ref_obj_id Object_identifier.
!! \param hdferr     \fortran_error
!!
  SUBROUTINE h5rdereference_object_f(obj_id, ref, ref_obj_id, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    TYPE(hobj_ref_t_f), INTENT(IN), TARGET :: ref
    INTEGER(HID_T), INTENT(OUT) :: ref_obj_id
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(ref)
    hdferr = h5rdereference_ptr_c(obj_id, 0, f_ptr, ref_obj_id)

  END SUBROUTINE h5rdereference_object_f

!>
!! \ingroup FH5R
!!
!! \brief Opens the dataset region
!!
!! \attention  \fortran_obsolete
!!
!! \param obj_id     Object identifier.
!! \param ref        Reference to open.
!! \param ref_obj_id Identifier of the object containing reference to the regions.
!! \param hdferr     \fortran_error
!!
  SUBROUTINE h5rdereference_region_f(obj_id, ref, ref_obj_id, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    TYPE(hdset_reg_ref_t_f), INTENT(IN), TARGET :: ref
    INTEGER(HID_T), INTENT(OUT) :: ref_obj_id
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(ref)
    hdferr = h5rdereference_ptr_c(obj_id, 1, f_ptr, ref_obj_id)

  END SUBROUTINE h5rdereference_region_f

!>
!! \ingroup FH5R
!!
!! \brief Opens the HDF5 object referenced.
!!
!! \attention  \fortran_approved
!!
!! \param obj_id     Valid identifier for the file containing the referenced object or any object in that file.
!! \param ref_type   The reference type of ref.
!! \param ref        Reference to open.
!! \param ref_obj_id Identifier of referenced object.
!! \param hdferr     \fortran_error
!!
  SUBROUTINE h5rdereference_ptr_f(obj_id, ref_type, ref, ref_obj_id, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    INTEGER, INTENT(IN) :: ref_type
    TYPE(C_PTR), INTENT(IN) :: ref
    INTEGER(HID_T), INTENT(OUT) :: ref_obj_id
    INTEGER, INTENT(OUT) :: hdferr

    hdferr = h5rdereference_ptr_c(obj_id, ref_type, ref, ref_obj_id)

  END SUBROUTINE h5rdereference_ptr_f

!>
!! \ingroup FH5R
!!
!! \brief Retrieves a name of a referenced object.
!!
!! \attention  \fortran_obsolete
!!
!! \param loc_id Identifier for the file containing the reference or for any object in that file.
!! \param ref    An object or dataset region reference.
!! \param name   A name associated with the referenced object or dataset region.
!! \param hdferr \fortran_error
!! \param size   The size of the name buffer, returning 0 (zero) if no name is associated with the identifier.
!!
  SUBROUTINE h5rget_name_object_f(loc_id,  ref, name, hdferr, size)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    TYPE(hobj_ref_t_f), INTENT(IN), TARGET :: ref
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: size
    CHARACTER(LEN=*), INTENT(INOUT) :: name
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(SIZE_T) :: size_default
    INTEGER(SIZE_T) :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(ref)

    name_len=LEN(name)

    hdferr = h5rget_name_ptr_c(loc_id, 0, f_ptr, name, name_len, size_default)

    IF(PRESENT(size)) size = size_default

  END SUBROUTINE h5rget_name_object_f

!>
!! \ingroup FH5R
!!
!! \brief Retrieves a name of a dataset region.
!!
!! \attention  \fortran_obsolete
!!
!! \param loc_id Identifier for the file containing the reference or for any object in that file.
!! \param ref    An object or dataset region reference.
!! \param name   A name associated with the referenced object or dataset region.
!! \param hdferr \fortran_error
!! \param size   The size of the name buffer, returning 0 (zero) if no name is associated with the identifier.
!!
  SUBROUTINE h5rget_name_region_f(loc_id, ref, name, hdferr, size)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    TYPE(hdset_reg_ref_t_f), INTENT(IN), TARGET :: ref
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: size
    CHARACTER(LEN=*), INTENT(INOUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(SIZE_T) :: size_default
    INTEGER(SIZE_T) :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(ref)

    name_len=LEN(name)

    hdferr = h5rget_name_ptr_c(loc_id, 1, f_ptr, name, name_len, size_default)

    IF(PRESENT(size)) size = size_default

  END SUBROUTINE h5rget_name_region_f

!>
!! \ingroup FH5R
!!
!! \brief Retrieves a name of a referenced object.
!!
!! \attention  \fortran_approved
!!
!! \param loc_id   Identifier for the file containing the reference or for any object in that file.
!! \param ref_type Type of reference.
!! \param ref      An object or dataset region reference.
!! \param name     A name associated with the referenced object or dataset ptr.
!! \param hdferr   \fortran_error
!!\param  size     The size of the name buffer, returning 0 (zero) if no name is associated with the identifier.
!!

#ifdef H5_DOXYGEN
!! See C API: @ref H5Rget_name()
!!
  SUBROUTINE h5rget_name_f(&
#else
  SUBROUTINE h5rget_name_ptr_f(&
#endif
       loc_id, ref_type, ref, name, hdferr, size)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    INTEGER, INTENT(IN) :: ref_type
    TYPE(C_PTR), INTENT(IN) :: ref
    CHARACTER(LEN=*), INTENT(INOUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: size
    INTEGER(SIZE_T) :: size_default
    INTEGER(SIZE_T) :: name_len

    name_len=LEN(name)

    hdferr = h5rget_name_ptr_c(loc_id, ref_type, ref, name, name_len, size_default)

    IF(PRESENT(size)) size = size_default

#ifdef H5_DOXYGEN
  END SUBROUTINE h5rget_name_f
#else
  END SUBROUTINE h5rget_name_ptr_f
#endif

!>
!! \ingroup FH5R
!!
!! \brief Retrieves the type of object that an object reference points to.
!!
!! \param loc_id   Identifier for the dataset containing the reference or for the group that dataset is in.
!! \param ref_type Type of reference to query.
!! \param ref      Reference to query.
!! \param obj_type Type of referenced object:
!!                   \li H5G_UNKNOWN_F
!!                   \li H5G_GROUP_F
!!                   \li H5G_DATASET_F
!!                   \li H5G_TYPE_F
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Rget_obj_type3()
!!
  SUBROUTINE h5rget_obj_type_f(loc_id, ref_type, ref, obj_type, hdferr)

    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    INTEGER, INTENT(IN) :: ref_type
    TYPE(C_PTR), INTENT(IN) :: ref
    INTEGER, INTENT(OUT) :: obj_type
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5rget_obj_type_c(loc_id, ref_type, ref, obj_type) &
            BIND(C, NAME='h5rget_obj_type_c')
         IMPORT :: C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER, INTENT(IN) :: ref_type
         TYPE(C_PTR), VALUE :: ref
         INTEGER :: obj_type
       END FUNCTION h5rget_obj_type_c
    END INTERFACE

    hdferr = h5rget_obj_type_c(loc_id, ref_type, ref, obj_type)

  END SUBROUTINE h5rget_obj_type_f
!>
!! \ingroup FH5R
!!
!! \brief Opens the HDF5 object referenced.
!!
!! \param ref_ptr  Pointer to reference to open, points object of TYPE(H5R_ref_t)
!! \param obj_id   Object identifier
!! \param hdferr   \fortran_error
!! \param rapl_id  Reference access property list identifier
!! \param oapl_id  Object access property list identifier
!!
!! See C API: @ref H5Ropen_object()
!!
  SUBROUTINE h5ropen_object_f(ref_ptr, obj_id, hdferr, rapl_id, oapl_id)

    IMPLICIT NONE

    TYPE(C_PTR)                           :: ref_ptr
    INTEGER(HID_T)          , INTENT(OUT) :: obj_id
    INTEGER                 , INTENT(OUT) :: hdferr
    INTEGER(HID_T), OPTIONAL, INTENT(IN)  :: rapl_id
    INTEGER(HID_T), OPTIONAL, INTENT(IN)  :: oapl_id

    INTEGER(HID_T)  :: rapl_id_default
    INTEGER(HID_T)  :: oapl_id_default

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Ropen_object(ref_ptr, rapl_id, oapl_id) &
            BIND(C, NAME='H5Ropen_object')
         IMPORT :: C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR)   , VALUE :: ref_ptr
         INTEGER(HID_T), VALUE :: rapl_id
         INTEGER(HID_T), VALUE :: oapl_id
       END FUNCTION H5Ropen_object
    END INTERFACE

    rapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(rapl_id)) rapl_id_default = rapl_id
    oapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(oapl_id)) oapl_id_default = oapl_id

    obj_id = H5Ropen_object(ref_ptr, rapl_id_default, oapl_id_default)

    hdferr = 0
    IF(obj_id.LT.0) hdferr = -1

  END SUBROUTINE h5ropen_object_f
!>
!! \ingroup FH5R
!!
!! \brief Opens the HDF5 attribute referenced.
!!
!! \param ref_ptr  Pointer to reference to open, points object of TYPE(H5R_ref_t)
!! \param obj_id   Object identifier
!! \param hdferr   \fortran_error
!! \param rapl_id  Reference access property list identifier
!! \param aapl_id  Attribute access property list identifier
!!
!! See C API: @ref H5Ropen_attr()
!!
  SUBROUTINE h5ropen_attr_f(ref_ptr, obj_id, hdferr, rapl_id, aapl_id)

    IMPLICIT NONE

    TYPE(C_PTR)                           :: ref_ptr
    INTEGER(HID_T)          , INTENT(OUT) :: obj_id
    INTEGER                 , INTENT(OUT) :: hdferr
    INTEGER(HID_T), OPTIONAL, INTENT(IN)  :: rapl_id
    INTEGER(HID_T), OPTIONAL, INTENT(IN)  :: aapl_id

    INTEGER(HID_T)  :: rapl_id_default
    INTEGER(HID_T)  :: aapl_id_default

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Ropen_attr(ref_ptr, rapl_id, aapl_id) &
            BIND(C, NAME='H5Ropen_attr')
         IMPORT :: C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR)   , VALUE :: ref_ptr
         INTEGER(HID_T), VALUE :: rapl_id
         INTEGER(HID_T), VALUE :: aapl_id
       END FUNCTION H5Ropen_attr
    END INTERFACE

    rapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(rapl_id)) rapl_id_default = rapl_id
    aapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id

    obj_id = H5Ropen_attr(ref_ptr, rapl_id_default, aapl_id_default)

    hdferr = 0
    IF(obj_id.LT.0) hdferr = -1

  END SUBROUTINE h5ropen_attr_f
!>
!! \ingroup FH5R
!!
!! \brief Sets up a dataspace and selection as specified by a region reference.
!!
!! \param ref_ptr  Pointer to reference to open, points object of TYPE(H5R_ref_t)
!! \param space_id Dataspace identifier
!! \param hdferr   \fortran_error
!! \param rapl_id  Reference access property list identifier
!! \param oapl_id  Object access property list identifier
!!
!! See C API: @ref H5Ropen_region()
!!
  SUBROUTINE h5ropen_region_f(ref_ptr, space_id, hdferr, rapl_id, oapl_id)

    IMPLICIT NONE

    TYPE(C_PTR)                           :: ref_ptr
    INTEGER(HID_T)          , INTENT(OUT) :: space_id
    INTEGER                 , INTENT(OUT) :: hdferr
    INTEGER(HID_T), OPTIONAL, INTENT(IN)  :: rapl_id
    INTEGER(HID_T), OPTIONAL, INTENT(IN)  :: oapl_id

    INTEGER(HID_T)  :: rapl_id_default
    INTEGER(HID_T)  :: oapl_id_default

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Ropen_region(ref_ptr, rapl_id, oapl_id) &
            BIND(C, NAME='H5Ropen_region')
         IMPORT :: C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR)   , VALUE :: ref_ptr
         INTEGER(HID_T), VALUE :: rapl_id
         INTEGER(HID_T), VALUE :: oapl_id
       END FUNCTION H5Ropen_region
    END INTERFACE

    rapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(rapl_id)) rapl_id_default = rapl_id
    oapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(oapl_id)) oapl_id_default = oapl_id

    space_id = H5Ropen_region(ref_ptr, rapl_id_default, oapl_id_default)

    hdferr = 0
    IF(space_id.LT.0) hdferr = -1

  END SUBROUTINE h5ropen_region_f
!>
!! \ingroup FH5R
!!
!! \brief Copies an existing reference.
!!
!! \param src_ref_ptr Pointer to reference to copy, of TYPE(H5R_ref_t)
!! \param dst_ref_ptr Pointer to output reference, of TYPE(H5R_ref_t)
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Rcopy()
!!
  SUBROUTINE h5rcopy_f(src_ref_ptr, dst_ref_ptr, hdferr)

    IMPLICIT NONE

    TYPE(C_PTR) :: src_ref_ptr
    TYPE(C_PTR) :: dst_ref_ptr
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Rcopy(src_ref_ptr, dst_ref_ptr) &
            BIND(C, NAME='H5Rcopy')
         IMPORT :: C_PTR, C_INT
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: src_ref_ptr
         TYPE(C_PTR), VALUE :: dst_ref_ptr
       END FUNCTION H5Rcopy
    END INTERFACE

    hdferr = INT(H5Rcopy(src_ref_ptr, dst_ref_ptr))

  END SUBROUTINE h5rcopy_f
!>
!! \ingroup FH5R
!!
!! \brief Determines whether two references are equal
!!
!! \param ref1_ptr Pointer to reference to compare, of TYPE(H5R_ref_t)
!! \param ref2_ptr Pointer to reference to compare, of TYPE(H5R_ref_t)
!! \param equal    If reference are equal
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Requal()
!!
  SUBROUTINE h5requal_f(ref1_ptr, ref2_ptr, equal, hdferr)

    IMPLICIT NONE

    TYPE(C_PTR) :: ref1_ptr
    TYPE(C_PTR) :: ref2_ptr
    LOGICAL, INTENT(OUT) :: equal
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(C_INT) :: c_equal

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Requal(ref1_ptr, ref2_ptr) &
            BIND(C, NAME='H5Requal')
         IMPORT :: C_PTR, C_INT
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: ref1_ptr
         TYPE(C_PTR), VALUE :: ref2_ptr
       END FUNCTION H5Requal
    END INTERFACE

    c_equal = INT(H5Requal(ref1_ptr, ref2_ptr))

    hdferr = 0
    equal = .FALSE.
    IF(c_equal .EQ. 1)THEN
       equal = .TRUE.
    ELSE IF(c_equal .LT. 0)THEN
       hdferr = -1
    ENDIF

  END SUBROUTINE h5requal_f

!>
!! \ingroup FH5R
!!
!! \brief Retrieves the type of a reference.
!!
!! \param ref_ptr  Pointer to reference to copy, of TYPE(H5R_ref_t)
!! \param ref_type A reference type
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Rget_type()
!!
  SUBROUTINE h5rget_type_f(ref_ptr, ref_type, hdferr)

    IMPLICIT NONE

    TYPE(C_PTR) :: ref_ptr
    INTEGER, INTENT(OUT) :: ref_type
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Rget_type(ref_ptr) &
            BIND(C, NAME='H5Rget_type')
         IMPORT :: C_PTR, C_INT
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: ref_ptr
       END FUNCTION H5Rget_type
    END INTERFACE

    ref_type = INT(H5Rget_type(ref_ptr))

    hdferr = 0
    IF(ref_type .EQ. H5R_BADTYPE_F) hdferr = -1

  END SUBROUTINE h5rget_type_f

!>
!! \ingroup FH5R
!!
!! \brief Closes a reference.
!!
!! \param ref_ptr Pointer to reference, of TYPE(H5R_ref_t)
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Rdestroy()
!!
  SUBROUTINE h5rdestroy_f(ref_ptr, hdferr)

    IMPLICIT NONE

    TYPE(C_PTR) :: ref_ptr
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Rdestroy(ref_ptr) &
            BIND(C, NAME='H5Rdestroy')
         IMPORT :: C_PTR, C_INT
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: ref_ptr
       END FUNCTION H5Rdestroy
    END INTERFACE

    hdferr = INT(H5Rdestroy(ref_ptr))

  END SUBROUTINE h5rdestroy_f

!>
!! \ingroup FH5R
!!
!! \brief Creates a reference.
!!
!! \attention  \fortran_approved
!!
!! \param loc_id   Location identifier.
!! \param name     Name of the dataset at the specified location
!! \param ref      Reference created by the function call
!! \param hdferr   \fortran_error
!! \param oapl_id  Object access property list identifier
!!
!! See C API: @ref H5Rcreate_object()
!!
  SUBROUTINE h5rcreate_object_f(loc_id, name, ref, hdferr, oapl_id)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(C_PTR)                  :: ref
    INTEGER       , INTENT(OUT)  :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: oapl_id

    INTEGER(HID_T) :: oapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Rcreate_object(loc_id, c_name, oapl_id_default, ref) &
            BIND(C, NAME='H5Rcreate_object')
         IMPORT :: C_PTR, C_INT, C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: c_name
         INTEGER(HID_T), VALUE :: oapl_id_default
         TYPE(C_PTR), VALUE :: ref
       END FUNCTION H5Rcreate_object
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    oapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(oapl_id)) oapl_id_default = oapl_id

    hdferr = INT(H5Rcreate_object(loc_id, c_name, oapl_id_default, ref))

  END SUBROUTINE h5rcreate_object_f
!>
!! \ingroup FH5R
!!
!! \brief Creates a region reference.
!!
!! \attention  \fortran_approved
!!
!! \param loc_id   Location identifier
!! \param name     Name of object
!! \param space_id Dataspace identifier
!! \param ref_ptr  Pointer to reference
!! \param hdferr   \fortran_error
!! \param oapl_id  Object access property list identifier
!!
!! See C API: @ref H5Rcreate_region()
!!
  SUBROUTINE h5rcreate_region_f(loc_id, name, space_id, ref_ptr, hdferr, oapl_id)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T)  , INTENT(IN) :: space_id
    TYPE(C_PTR)                  :: ref_ptr
    INTEGER        , INTENT(OUT) :: hdferr
    INTEGER(HID_T) , INTENT(IN), OPTIONAL :: oapl_id

    INTEGER(HID_T) :: oapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Rcreate_region(loc_id, c_name, space_id, oapl_id_default, ref_ptr) &
            BIND(C, NAME='H5Rcreate_region')
         IMPORT :: C_PTR, C_INT, C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: c_name
         INTEGER(HID_T), VALUE :: space_id
         INTEGER(HID_T), VALUE :: oapl_id_default
         TYPE(C_PTR), VALUE :: ref_ptr
       END FUNCTION H5Rcreate_region
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    oapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(oapl_id)) oapl_id_default = oapl_id

    hdferr = INT(H5Rcreate_region(loc_id, c_name, space_id, oapl_id_default, ref_ptr))

  END SUBROUTINE h5rcreate_region_f
!>
!! \ingroup FH5R
!!
!! \brief Creates an attribute reference.
!!
!! \attention  \fortran_approved
!!
!! \param loc_id    Location identifier
!! \param name      Name of object
!! \param attr_name Name of attribute
!! \param ref_ptr   Pointer to reference
!! \param hdferr    \fortran_error
!! \param oapl_id   Object access property list identifier
!!
!! See C API: @ref H5Rcreate_attr()
!!
  SUBROUTINE h5rcreate_attr_f(loc_id, name, attr_name, ref_ptr, hdferr, oapl_id)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    TYPE(C_PTR)                  :: ref_ptr
    INTEGER        , INTENT(OUT) :: hdferr
    INTEGER(HID_T) , INTENT(IN), OPTIONAL :: oapl_id

    INTEGER(HID_T) :: oapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    CHARACTER(LEN=LEN_TRIM(attr_name)+1,KIND=C_CHAR) :: c_attr_name

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Rcreate_attr(loc_id, c_name, c_attr_name, oapl_id_default, ref_ptr) &
            BIND(C, NAME='H5Rcreate_attr')
         IMPORT :: C_PTR, C_INT, C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: c_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: c_attr_name
         INTEGER(HID_T), VALUE :: oapl_id_default
         TYPE(C_PTR), VALUE :: ref_ptr
       END FUNCTION H5Rcreate_attr
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR
    c_attr_name  = TRIM(attr_name)//C_NULL_CHAR

    oapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(oapl_id)) oapl_id_default = oapl_id

    hdferr = INT(H5Rcreate_attr(loc_id, c_name, c_attr_name, oapl_id_default, ref_ptr))

  END SUBROUTINE h5rcreate_attr_f
!>
!! \ingroup FH5R
!!
!! \brief Retrieves the object name for a referenced object.
!!
!! \param ref_ptr  Pointer to reference to query
!! \param name     Buffer to place the file name of the reference
!! \param hdferr   \fortran_error
!! \param rapl_id  Reference access property list identifier
!! \param name_len Maximum length of the name to retrieve
!!
  SUBROUTINE h5rget_obj_name_f( ref_ptr, name, hdferr, rapl_id, name_len)

    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    TYPE(C_PTR)                :: ref_ptr
    CHARACTER(LEN=*)           :: name
    INTEGER      , INTENT(OUT) :: hdferr
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: name_len
    INTEGER(HID_T) , INTENT(IN) , OPTIONAL :: rapl_id

    CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(1:LEN(name)+1), TARGET :: c_name
    INTEGER(HID_T) :: rapl_id_default
    INTEGER(SIZE_T) :: l

    INTERFACE
       INTEGER(SIZE_T) FUNCTION H5Rget_obj_name(ref_ptr, rapl_id, name, size_default) &
            BIND(C, NAME='H5Rget_obj_name')
         IMPORT :: c_char, c_ptr
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: ref_ptr
         INTEGER(HID_T), VALUE :: rapl_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(SIZE_T), VALUE :: size_default
       END FUNCTION H5Rget_obj_name
    END INTERFACE

    rapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(rapl_id)) rapl_id_default = rapl_id

    hdferr = 0
    IF(PRESENT(name_len))THEN
       c_name(1:1)(1:1) = C_NULL_CHAR
       name_len = H5Rget_obj_name(ref_ptr, rapl_id_default, c_name, 1_SIZE_T)
       IF(name_len.LT.0_SIZE_T) hdferr = H5I_INVALID_HID_F
    ELSE
       l = INT(LEN(name)+1,SIZE_T)
       IF(H5Rget_obj_name(ref_ptr, rapl_id_default, c_name, l) .LT. 0_SIZE_T)THEN
          hdferr = H5I_INVALID_HID_F
       ELSE
          CALL HD5c2fstring(name, c_name, LEN(name,KIND=SIZE_T), LEN(name,KIND=SIZE_T)+1_SIZE_T )
       ENDIF
    ENDIF

  END SUBROUTINE h5rget_obj_name_f
!>
!! \ingroup FH5R
!!
!! \brief Retrieves the attribute name for a referenced object.
!!
!! \param ref_ptr  Pointer to reference to query
!! \param name     Buffer to place the attribute name of the reference
!! \param hdferr   \fortran_error
!! \param name_len Maximum length of the name to retrieve
!!
  SUBROUTINE h5rget_attr_name_f(ref_ptr, name, hdferr, name_len)

    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    TYPE(C_PTR)                :: ref_ptr
    CHARACTER(LEN=*)           :: name
    INTEGER      , INTENT(OUT) :: hdferr
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: name_len

    CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(1:LEN(name)+1), TARGET :: c_name
    INTEGER(SIZE_T) :: l

    INTERFACE
       INTEGER(SIZE_T) FUNCTION H5Rget_attr_name(ref_ptr, name, size_default) &
            BIND(C, NAME='H5Rget_attr_name')
         IMPORT :: c_char, c_ptr
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: ref_ptr
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(SIZE_T), VALUE :: size_default
       END FUNCTION H5Rget_attr_name
    END INTERFACE

    hdferr = 0
    IF(PRESENT(name_len))THEN
       c_name(1:1)(1:1) = C_NULL_CHAR
       name_len = H5Rget_attr_name(ref_ptr, c_name, 1_SIZE_T)
       IF(name_len.LT.0_SIZE_T) hdferr = H5I_INVALID_HID_F
    ELSE
       l = INT(LEN(name)+1,SIZE_T)
       IF(H5Rget_attr_name(ref_ptr, c_name, l) .LT. 0_SIZE_T)THEN
          hdferr = H5I_INVALID_HID_F
       ELSE
          CALL HD5c2fstring(name, c_name, LEN(name,KIND=SIZE_T), LEN(name,KIND=SIZE_T)+1_SIZE_T )
       ENDIF
    ENDIF

  END SUBROUTINE h5rget_attr_name_f
!>
!! \ingroup FH5R
!!
!! \brief Retrieves the file name for a referenced object.
!!
!! \param ref_ptr  Pointer to reference to query
!! \param name     Buffer to place the file name of the reference
!! \param hdferr   \fortran_error
!! \param name_len The size of the name buffer
!!
!! See C API: @ref H5Rget_file_name()
!!
  SUBROUTINE h5rget_file_name_f(ref_ptr, name, hdferr, name_len)

    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    TYPE(C_PTR)                :: ref_ptr
    CHARACTER(LEN=*)           :: name
    INTEGER      , INTENT(OUT) :: hdferr
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: name_len

    CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(1:LEN(name)+1), TARGET :: c_name
    INTEGER(SIZE_T) :: l

    INTERFACE
       INTEGER(SIZE_T) FUNCTION H5Rget_file_name(ref_ptr, name, size_default) &
            BIND(C, NAME='H5Rget_file_name')
         IMPORT :: c_char, c_ptr
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: ref_ptr
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(SIZE_T), VALUE :: size_default
       END FUNCTION H5Rget_file_name
    END INTERFACE

    hdferr = 0
    IF(PRESENT(name_len))THEN
       c_name(1:1)(1:1) = C_NULL_CHAR
       name_len = H5Rget_file_name(ref_ptr, c_name, 1_SIZE_T)
       IF(name_len.LT.0_SIZE_T) hdferr = H5I_INVALID_HID_F
    ELSE
       l = INT(LEN(name)+1,SIZE_T)
       IF(H5Rget_file_name(ref_ptr, c_name, l) .LT. 0_SIZE_T)THEN
          hdferr = H5I_INVALID_HID_F
       ELSE
          CALL HD5c2fstring(name, c_name, LEN(name,KIND=SIZE_T), LEN(name,KIND=SIZE_T)+1_SIZE_T )
       ENDIF
    ENDIF

  END SUBROUTINE h5rget_file_name_f

END MODULE H5R
