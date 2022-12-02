!> @defgroup FH5DS Fortran High Level Dimension Scales (H5DS) Interface
!!
!! @see H5DS, C-HL API
!!
!! @see @ref H5DS_UG, User Guide
!!

!> @ingroup FH5DS
!!
!! @brief This module contains Fortran interfaces for H5DS
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

MODULE H5DS

  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_CHAR, C_FLOAT, C_DOUBLE, C_LOC, C_CHAR
  USE h5fortran_types
  USE hdf5

CONTAINS

!>
!! \ingroup FH5DS
!!
!! \brief Convert dataset \p dsid to a dimension scale, with optional name, \p dimname.
!!
!! \param dsid    The dataset to be made a Dimemsion Scale.
!! \param errcode \fortran_error
!! \param dimname The dimension name
!!
!! See C API: @ref H5DSset_scale()
!!
  SUBROUTINE H5DSset_scale_f( dsid, errcode, dimname)

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: dsid
    CHARACTER(LEN=*), INTENT(in), OPTIONAL :: dimname
    INTEGER :: errcode

    INTEGER(SIZE_T) :: dimname_len ! length of dimname (if present)

    INTERFACE
       INTEGER FUNCTION H5DSset_scale_c(dsid, dimname, dimname_len) &
            BIND(C,NAME='h5dsset_scale_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: dsid
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dimname
         INTEGER(SIZE_T),  INTENT(in) :: dimname_len
       END FUNCTION H5DSset_scale_c
    END INTERFACE

    IF(PRESENT(dimname))THEN
       dimname_len = LEN(dimname)
       errcode = H5DSset_scale_c(dsid, dimname, dimname_len )
    ELSE
       errcode = H5DSset_scale_c(dsid, " ", INT(0,SIZE_T) )
    ENDIF

  END SUBROUTINE H5DSset_scale_f

!>
!! \ingroup FH5DS
!!
!! \brief Attach dimension scale dsid to dimension \p idx of dataset \p did.
!!
!! \param did     The dataset.
!! \param dsid    The scale to be attached.
!! \param idx     The dimension of \p did that \p dsid is associated with.
!! \param errcode \fortran_error
!!
!! See C API: @ref H5DSattach_scale()
!!
  SUBROUTINE H5DSattach_scale_f( did, dsid, idx, errcode)

    IMPLICIT NONE

    INTEGER(hid_t), INTENT(in) :: did
    INTEGER(hid_t), INTENT(in) :: dsid
    INTEGER       , INTENT(in) :: idx
    INTEGER                    :: errcode
    INTEGER                    :: c_idx

    INTERFACE
       INTEGER FUNCTION  H5DSattach_scale_c(did, dsid, idx) &
            BIND(C,NAME='h5dsattach_scale_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(hid_t), INTENT(in) :: did
         INTEGER(hid_t), INTENT(in) :: dsid
         INTEGER       , INTENT(in) :: idx
       END FUNCTION H5DSattach_scale_c
    END INTERFACE

    c_idx = idx -1 ! account for C-dimensions starting at 0

    errcode = H5DSattach_scale_c( did, dsid, c_idx)

  END SUBROUTINE H5DSattach_scale_f

!>
!! \ingroup FH5DS
!!
!! \brief Detach dimension scale dsid from the dimension idx of dataset \p did.
!!
!! \param did     The dataset.
!! \param dsid    The scale to be detached.
!! \param idx     The dimension of \p did to detach.
!! \param errcode \fortran_error
!!

!! See C API: @ref H5DSdetach_scale()
!!
  SUBROUTINE H5DSdetach_scale_f( did, dsid, idx, errcode)

    IMPLICIT NONE

    INTEGER(hid_t), INTENT(in) :: did
    INTEGER(hid_t), INTENT(in) :: dsid
    INTEGER       , INTENT(in) :: idx
    INTEGER                    :: errcode
    INTEGER                    :: c_idx

    INTERFACE
       INTEGER FUNCTION  H5DSdetach_scale_c(did, dsid, idx) &
            BIND(C,NAME='h5dsdetach_scale_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(hid_t), INTENT(in) :: did
         INTEGER(hid_t), INTENT(in) :: dsid
         INTEGER       , INTENT(in) :: idx
       END FUNCTION H5DSdetach_scale_c
    END INTERFACE

    c_idx = idx - 1 ! account for C-dimensions starting at 0

    errcode = H5DSdetach_scale_c( did, dsid, c_idx)

  END SUBROUTINE H5DSdetach_scale_f

!>
!! \ingroup FH5DS
!!
!! \brief Report if dimension scale dsid is currently attached to dimension idx of dataset did.
!!
!! \param did         The dataset.
!! \param dsid        The scale to be attached.
!! \param idx         The dimension of \p did that \p dsid is associated with.
!! \param is_attached If dimension scale \p dsid is currently attached to dimension \p idx of dataset \p did.
!! \param errcode     \fortran_error
!!
!! See C API: @ref H5DSis_attached()
!!
  SUBROUTINE H5DSis_attached_f( did, dsid, idx, is_attached, errcode)

    IMPLICIT NONE

    INTEGER(hid_t), INTENT(in)  :: did
    INTEGER(hid_t), INTENT(in)  :: dsid
    INTEGER       , INTENT(in)  :: idx
    LOGICAL       , INTENT(out) :: is_attached
    INTEGER                     :: errcode
    INTEGER                     :: c_is_attached
    INTEGER                     :: c_idx

    INTERFACE
       INTEGER FUNCTION H5DSis_attached_c(did, dsid, idx, c_is_attached) &
            BIND(C,NAME='h5dsis_attached_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(hid_t), INTENT(in)  :: did
         INTEGER(hid_t), INTENT(in)  :: dsid
         INTEGER       , INTENT(in)  :: idx
         INTEGER       , INTENT(out) :: c_is_attached
       END FUNCTION H5DSis_attached_c
    END INTERFACE

    c_idx = idx - 1 ! account for C-dimensions starting at 0

    errcode = H5DSis_attached_c(did, dsid, c_idx, c_is_attached)

    is_attached = .FALSE. ! default
    IF(c_is_attached.GT.0)THEN
       is_attached = .TRUE.
    ELSE IF(errcode.LT.0)THEN
       errcode = -1
    ENDIF

  END SUBROUTINE H5DSis_attached_f

!
! H5DSiterate_scales: Implement in  F2003
!
!>
!! \ingroup FH5DS
!!
!! \brief Determines whether \p did is a Dimension Scale.
!!
!! \param did         The data set to query.
!! \param is_scale    If is a Dimension Scale.
!! \param errcode     \fortran_error
!!
!! See C API: @ref H5DSis_scale()
!!
  SUBROUTINE H5DSis_scale_f( did, is_scale, errcode)

    IMPLICIT NONE

    INTEGER(hid_t), INTENT(in)  :: did
    LOGICAL       , INTENT(out) :: is_scale
    INTEGER       , INTENT(out) :: errcode
    INTEGER                     :: c_is_scale

    INTERFACE
       INTEGER FUNCTION H5DSis_scale_c(did,c_is_scale) &
            BIND(C,NAME='h5dsis_scale_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(hid_t), INTENT(in) :: did
         INTEGER, INTENT(out) :: c_is_scale
       END FUNCTION H5DSis_scale_c
    END INTERFACE

    errcode = H5DSis_scale_c(did, c_is_scale)

    is_scale = .FALSE. ! default
    IF(c_is_scale.GT.0)THEN
       is_scale = .TRUE.
    ELSE IF(errcode.LT.0)THEN
       errcode = -1
    ENDIF

  END SUBROUTINE H5DSis_scale_f

!>
!! \ingroup FH5DS
!!
!! \brief Set label for the dimension \p idx of \p did to the value \p label.
!!
!! \param did     The data set.
!! \param idx     The dimension.
!! \param label   The label.
!! \param errcode \fortran_error
!!
!! See C API: @ref H5DSset_label()
!!
  SUBROUTINE H5DSset_label_f( did, idx, label, errcode)

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: did
    INTEGER       ,   INTENT(in) :: idx
    CHARACTER(LEN=*), INTENT(in) :: label
    INTEGER :: errcode                     ! Error code

    INTEGER(SIZE_T) :: label_len  ! Length of label
    INTEGER :: c_idx

    INTERFACE
       INTEGER FUNCTION H5DSset_label_c(did, idx, label, label_len) &
            BIND(C,NAME='h5dsset_label_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: did
         INTEGER       ,   INTENT(in) :: idx
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: label
         INTEGER(SIZE_T),  INTENT(in) :: label_len
       END FUNCTION H5DSset_label_c
    END INTERFACE

    c_idx = idx - 1

    label_len = LEN(label)
    errcode = H5DSset_label_c(did, c_idx, label, label_len)

  END SUBROUTINE H5DSset_label_f

!>
!! \ingroup FH5DS
!!
!! \brief Read the \p label for dimension \p idx of \p did into buffer \p label.
!!
!! \param did     The dataset.
!! \param idx     The dimension.
!! \param label   The label.
!! \param size    The length of the \p label buffer.
!! \param errcode \fortran_error
!!
!! See C API: @ref H5DSget_label()
!!
  SUBROUTINE H5DSget_label_f( did, idx, label, size, errcode)

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: did
    INTEGER       ,   INTENT(in) :: idx
    CHARACTER(LEN=*), INTENT(INOUT) :: label
    INTEGER(size_t) , INTENT(INOUT) :: size
    INTEGER :: errcode
    INTEGER :: c_idx

    INTERFACE
       INTEGER FUNCTION H5DSget_label_c(did, idx, label, size) &
            BIND(C,NAME='h5dsget_label_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in)    :: did
         INTEGER       ,   INTENT(in)    :: idx
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: label
         INTEGER(SIZE_T),  INTENT(inout) :: size
       END FUNCTION H5DSget_label_c
    END INTERFACE

    c_idx = idx - 1

    errcode = H5DSget_label_c(did, c_idx, label, size)

  END SUBROUTINE H5DSget_label_f

!>
!! \ingroup FH5DS
!!
!! \brief Read the name of scale \p did into buffer name.
!!
!! \param did     Dimension scale identifier.
!! \param name    Buffer to contain the returned name.
!! \param size    Size in bytes, of the name buffer.
!! \param errcode \fortran_error
!!
!! See C API: @ref H5DSget_scale_name()
!!
  SUBROUTINE H5DSget_scale_name_f(did, name, size, errcode)

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: did
    CHARACTER(LEN=*), INTENT(INOUT) :: name
    INTEGER(size_t) , INTENT(INOUT) :: size
    INTEGER :: errcode

    INTERFACE
       INTEGER FUNCTION H5DSget_scale_name_c(did, name, size) &
            bind(c,name='h5dsget_scale_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in)    :: did
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: name
         INTEGER(SIZE_T),  INTENT(inout) :: size
       END FUNCTION H5DSget_scale_name_c
    END INTERFACE

    errcode = H5DSget_scale_name_c(did, name, size)

  END SUBROUTINE H5DSget_scale_name_f

!>
!! \ingroup FH5DS
!!
!! \brief Determines how many Dimension Scales are attached to dimension idx of \p did.
!!
!! \param did        The dataset to query.
!! \param idx        The dimension of \p did to query.
!! \param num_scales Number of Dimension Scales associated with \p did.
!! \param errcode    \fortran_error
!!
!! See C API: @ref H5DSget_num_scales()
!!
  SUBROUTINE H5DSget_num_scales_f( did, idx, num_scales, errcode)

    IMPLICIT NONE
    INTEGER(hid_t), INTENT(in)  :: did
    INTEGER       , INTENT(in)  :: idx
    INTEGER       , INTENT(INOUT) :: num_scales
    INTEGER                     :: errcode
    INTEGER                     :: c_idx

    INTERFACE
       INTEGER FUNCTION H5DSget_num_scales_c(did, idx, num_scales) &
            BIND(C,NAME='h5dsget_num_scales_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(hid_t), INTENT(in)  :: did
         INTEGER       , INTENT(in)  :: idx
         INTEGER       , INTENT(INOUT) :: num_scales
       END FUNCTION H5DSget_num_scales_c
    END INTERFACE

    c_idx = idx - 1
    errcode = H5DSget_num_scales_c(did, c_idx, num_scales)

  END SUBROUTINE H5DSget_num_scales_f

END MODULE h5ds






