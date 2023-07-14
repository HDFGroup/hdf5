!> @defgroup FH5R Fortran References (H5R) Interface
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
  PRIVATE h5rcreate_object_f, h5rcreate_region_f, h5rcreate_ptr_f
  PRIVATE h5rdereference_object_f, h5rdereference_region_f, h5rdereference_ptr_f
  PRIVATE h5rget_name_object_f, h5rget_name_region_f, h5rget_name_ptr_f

!> @brief hdset_reg_ref_t_f03 C compatible reference
  TYPE :: hdset_reg_ref_t_f03
     INTEGER(C_SIGNED_CHAR), DIMENSION(1:H5R_DSET_REG_REF_BUF_SIZE_F) :: ref
  END TYPE hdset_reg_ref_t_f03

  INTERFACE h5rget_object_type_f
     MODULE PROCEDURE h5rget_object_type_obj_f
  END INTERFACE

#ifndef H5_DOXYGEN

  INTERFACE h5rget_region_f
     MODULE PROCEDURE h5rget_region_ptr_f    ! F2003
     MODULE PROCEDURE h5rget_region_region_f ! obsolete
  END INTERFACE

  INTERFACE h5rcreate_f
     MODULE PROCEDURE h5rcreate_ptr_f    ! F2003
     MODULE PROCEDURE h5rcreate_object_f ! obsolete
     MODULE PROCEDURE h5rcreate_region_f ! obsolete
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
  SUBROUTINE h5rcreate_object_f(loc_id, name, ref, hdferr)
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

  END SUBROUTINE h5rcreate_object_f

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
  SUBROUTINE h5rcreate_region_f(loc_id, name, space_id, ref, hdferr)
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

  END SUBROUTINE h5rcreate_region_f

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

END MODULE H5R
