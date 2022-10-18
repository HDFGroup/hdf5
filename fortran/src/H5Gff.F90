!> @defgroup FH5G Fortran Group (H5G) Interface
!!
!! @see H5G, C-API
!!
!! @see @ref H5G_UG, User Guide
!!

!> @ingroup FH5G
!!
!! @brief This module contains Fortran interfaces for H5G functions.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
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
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5G function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5G
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR
  USE H5GLOBAL

CONTAINS

!>
!! \ingroup FH5G
!!
!! \brief Creates a new group.
!!
!! \param loc_id    Location identifier.
!! \param name      Group name at the specified location.
!! \param grp_id    Group identifier.
!! \param hdferr    \fortran_error
!! \param size_hint A parameter indicating the number of bytes to reserve for the names that will appear in the group.
!!                  Set to OBJECT_NAMELEN_DEFAULT_F if using any of the optional parameters lcpl_id, gcpl_id,
!!                  and/or gapl_id when not using keywords in specifying the optional parameters.
!! \param lcpl_id   Property list for link creation.
!! \param gcpl_id   Property list for group creation.
!! \param gapl_id   Property list for group access.
!!
!! See C API: @ref H5Gcreate2()
!!
  SUBROUTINE h5gcreate_f(loc_id, name, grp_id, hdferr, size_hint, lcpl_id, gcpl_id, gapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(OUT) :: grp_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(SIZE_T), OPTIONAL, INTENT(IN) :: size_hint
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lcpl_id
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: gcpl_id
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: gapl_id

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: gcpl_id_default
    INTEGER(HID_T) :: gapl_id_default
    INTEGER :: namelen ! Length of the name character string
    INTEGER(SIZE_T) :: size_hint_default

    INTERFACE
       INTEGER FUNCTION h5gcreate_c(loc_id, name, namelen, &
            size_hint_default, grp_id, lcpl_id_default, gcpl_id_default, gapl_id_default) &
            BIND(C,NAME='h5gcreate_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(SIZE_T) :: size_hint_default
         INTEGER(HID_T), INTENT(OUT) :: grp_id
         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: gcpl_id_default
         INTEGER(HID_T) :: gapl_id_default
       END FUNCTION h5gcreate_c
    END INTERFACE

    size_hint_default = OBJECT_NAMELEN_DEFAULT_F
    IF (PRESENT(size_hint)) size_hint_default = size_hint
    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    gcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(gcpl_id)) gcpl_id_default = gcpl_id
    gapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(gapl_id)) gapl_id_default = gapl_id

    namelen = LEN(name)

    hdferr = h5gcreate_c(loc_id, name, namelen, size_hint_default, grp_id, &
         lcpl_id_default, gcpl_id_default, gapl_id_default)

  END SUBROUTINE h5gcreate_f

!>
!! \ingroup FH5G
!!
!! \brief Opens an existing group.
!!
!! \param loc_id  Location identifier.
!! \param name    Name of the group to open.
!! \param grp_id  Group identifier.
!! \param hdferr  \fortran_error
!! \param gapl_id Group access property list identifier.
!!
!! See C API: @ref H5Gopen2()
!!
  SUBROUTINE h5gopen_f(loc_id, name, grp_id, hdferr, gapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(OUT) :: grp_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: gapl_id
    INTEGER(HID_T) :: gapl_id_default
    INTEGER :: namelen ! Length of the name character string

    INTERFACE
       INTEGER FUNCTION h5gopen_c(loc_id, name, namelen, gapl_id_default, grp_id) &
            BIND(C,NAME='h5gopen_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(HID_T), INTENT(IN) :: gapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: grp_id
       END FUNCTION h5gopen_c
    END INTERFACE

    gapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(gapl_id)) gapl_id_default = gapl_id

    namelen = LEN(name)
    hdferr = h5gopen_c(loc_id, name, namelen, gapl_id_default, grp_id)

  END SUBROUTINE h5gopen_f
!>
!! \ingroup FH5G
!!
!! \brief Closes the specified group.
!!
!! \param grp_id Group identifier.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Gclose()
!!
  SUBROUTINE h5gclose_f(grp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: grp_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5gclose_c(grp_id) BIND(C,NAME='h5gclose_c')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: grp_id
       END FUNCTION h5gclose_c
    END INTERFACE

    hdferr = h5gclose_c(grp_id)

  END SUBROUTINE h5gclose_f
!>
!! \ingroup FH5G
!!
!! \brief Returns name and type of the group member identified by its index.
!!
!! \param loc_id   Location identifier.
!! \param name     Name of the group at the specified location.
!! \param idx      Object index (zero-based).
!! \param obj_name Object name.
!! \param obj_type Object type.
!! \param hdferr   \fortran_error
!!
  SUBROUTINE h5gget_obj_info_idx_f(loc_id, name, idx, &
       obj_name, obj_type, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: idx
    CHARACTER(LEN=*), INTENT(OUT) :: obj_name
    INTEGER, INTENT(OUT) :: obj_type
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER :: namelen ! Length of the name character string
    INTEGER :: obj_namelen ! Length of the obj_name character string

    INTERFACE
       INTEGER FUNCTION h5gget_obj_info_idx_c(loc_id, name, &
            namelen, idx, &
            obj_name, obj_namelen, obj_type) BIND(C,NAME='h5gget_obj_info_idx_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER, INTENT(IN) :: idx
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: obj_name
         INTEGER :: obj_namelen
         INTEGER, INTENT(OUT) :: obj_type
       END FUNCTION h5gget_obj_info_idx_c
    END INTERFACE

    namelen = LEN(name)
    obj_namelen = LEN(obj_name)
    hdferr = h5gget_obj_info_idx_c(loc_id, name, namelen, idx, &
                                           obj_name, obj_namelen, obj_type)
  END SUBROUTINE h5gget_obj_info_idx_f

!>
!! \ingroup FH5G
!!
!! \brief Returns the number of group members.
!!
!! \param loc_id   Location identifier.
!! \param name     Name of the group at the specified location.
!! \param nmembers Number of group members.
!! \param hdferr   \fortran_error
!!
  SUBROUTINE h5gn_members_f(loc_id, name, nmembers, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: nmembers
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER :: namelen ! Length of the name character string

    INTERFACE
       INTEGER FUNCTION h5gn_members_c(loc_id, name, namelen, nmembers) &
            BIND(C,NAME='h5gn_members_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER, INTENT(OUT) :: nmembers
       END FUNCTION h5gn_members_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5gn_members_c(loc_id, name, namelen, nmembers)

  END SUBROUTINE h5gn_members_f
!>
!! \ingroup FH5G
!!
!! \brief Creates a link of the specified type from new_name to current_name.
!!
!! \param loc_id       Location identifier.
!! \param link_type    Link type; possible values are:
!!                     \li H5G_LINK_HARD_F
!!                     \li H5G_LINK_SOFT_F
!! \param current_name Name of the existing object if link is a hard link. Can be anything for the soft link.
!! \param new_name     New name for the object.
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Glink()
!!
  SUBROUTINE h5glink_f(loc_id, link_type, current_name, &
       new_name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    INTEGER, INTENT(IN) :: link_type

    CHARACTER(LEN=*), INTENT(IN) :: current_name
    CHARACTER(LEN=*), INTENT(IN) :: new_name
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER :: current_namelen ! Length of the current_name string
    INTEGER :: new_namelen     ! Length of the new_name string

    INTERFACE
       INTEGER FUNCTION h5glink_c(loc_id, link_type, current_name, &
            current_namelen, new_name, new_namelen) &
                   BIND(C,NAME='h5glink_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER, INTENT(IN) :: link_type
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: current_name
         INTEGER :: current_namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: new_name
         INTEGER :: new_namelen
       END FUNCTION h5glink_c
    END INTERFACE

    current_namelen = LEN(current_name)
    new_namelen = LEN(new_name)
    hdferr = h5glink_c(loc_id, link_type, current_name, &
         current_namelen, new_name, new_namelen)
  END SUBROUTINE h5glink_f

!>
!! \ingroup FH5G
!!
!! \brief Creates a link of the specified type from new_name
!!        to current_name. current_name and new_name are interpreted
!!        relative to current and new location identifiers.
!!
!! \param cur_loc_id Location identifier.
!! \param cur_name   Name of the existing object if link is a hard link. Can be anything for the soft link.
!! \param link_type  Link type; possible values are:
!!                   \li H5G_LINK_HARD_F
!!                   \li H5G_LINK_SOFT_F
!! \param new_loc_id New location identifier.
!! \param new_name   New name for the object.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Glink2()
!!
  SUBROUTINE h5glink2_f(cur_loc_id, cur_name, link_type, new_loc_id, &
       new_name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: cur_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: cur_name
    INTEGER, INTENT(IN) :: link_type

    INTEGER(HID_T), INTENT(IN) :: new_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: new_name
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER :: cur_namelen ! Length of the current_name string
    INTEGER :: new_namelen ! Length of the new_name string

    INTERFACE
       INTEGER FUNCTION h5glink2_c(cur_loc_id, cur_name, cur_namelen, &
            link_type, new_loc_id, &
            new_name, new_namelen) BIND(C,NAME='h5glink2_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: cur_loc_id
         INTEGER(HID_T), INTENT(IN) :: new_loc_id
         INTEGER, INTENT(IN) :: link_type
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: cur_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: new_name
         INTEGER :: cur_namelen
         INTEGER :: new_namelen
       END FUNCTION h5glink2_c
    END INTERFACE

    cur_namelen = LEN(cur_name)
    new_namelen = LEN(new_name)
    hdferr = h5glink2_c(cur_loc_id, cur_name, cur_namelen, link_type, &
         new_loc_id, new_name, new_namelen)
  END SUBROUTINE h5glink2_f

!>
!! \ingroup FH5G
!!
!! \brief Removes the specified name from the group graph and
!!        decrements the link count for the object to which name points
!!
!! \param loc_id Location identifier.
!! \param name   Name of the object to unlink.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Gunlink()
!!
  SUBROUTINE h5gunlink_f(loc_id, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen ! Length of the name character string

    INTERFACE
       INTEGER FUNCTION h5gunlink_c(loc_id, name, namelen) BIND(C,NAME='h5gunlink_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
       END FUNCTION h5gunlink_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5gunlink_c(loc_id, name, namelen)
  END SUBROUTINE h5gunlink_f

!>
!! \ingroup FH5G
!!
!! \brief Renames an object within an HDF5 file.
!!
!! \param loc_id   Location identifier.
!! \param name     Object&apos;s name at specified location.
!! \param new_name Object&apos;s new name.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Gmove()
!!
  SUBROUTINE h5gmove_f(loc_id, name, new_name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=*), INTENT(IN) :: new_name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen         ! Length of the current_name string
    INTEGER :: new_namelen     ! Length of the new_name string

    INTERFACE
       INTEGER FUNCTION h5gmove_c(loc_id, name, namelen, new_name, new_namelen) BIND(C,NAME='h5gmove_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: new_name
         INTEGER :: new_namelen
       END FUNCTION h5gmove_c
    END INTERFACE

    namelen = LEN(name)
    new_namelen = LEN(new_name)
    hdferr = h5gmove_c(loc_id, name, namelen, new_name, new_namelen)
  END SUBROUTINE h5gmove_f
!>
!! \ingroup FH5G
!!
!! \brief Renames an object within an HDF5 file.
!!
!! \param src_loc_id Original location identifier.
!! \param src_name   Object&apos;s name at specified original location.
!! \param dst_loc_id Original location identifier.
!! \param dst_name   Object&apos;s new name.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Gmove2()
!!
  SUBROUTINE h5gmove2_f(src_loc_id, src_name, dst_loc_id, dst_name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)   :: src_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: src_name
    INTEGER(HID_T), INTENT(IN)   :: dst_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dst_name
    INTEGER, INTENT(OUT)         :: hdferr
    INTEGER :: src_namelen         ! Length of the current_name string
    INTEGER :: dst_namelen         ! Length of the new_name string

    INTERFACE
       INTEGER FUNCTION h5gmove2_c(src_loc_id, src_name, src_namelen, &
            dst_loc_id, dst_name, dst_namelen) BIND(C,NAME='h5gmove2_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: src_loc_id
         INTEGER(HID_T), INTENT(IN) :: dst_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: src_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: dst_name
         INTEGER :: src_namelen
         INTEGER :: dst_namelen
       END FUNCTION h5gmove2_c
    END INTERFACE

    src_namelen = LEN(src_name)
    dst_namelen = LEN(dst_name)
    hdferr = h5gmove2_c(src_loc_id, src_name, src_namelen, dst_loc_id, dst_name, dst_namelen)
  END SUBROUTINE h5gmove2_f
!>
!! \ingroup FH5G
!!
!! \brief Returns the name of the object that the symbolic link points to.
!!
!! \param loc_id Location identifier.
!! \param name   Symbolic link to the object whose name is to be returned.
!! \param size   Maximum number of characters to be returned.
!! \param buffer A buffer to hold the name of the object being sought.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Gget_linkval()
!!
  SUBROUTINE h5gget_linkval_f(loc_id, name, size, buffer, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    CHARACTER(LEN=size), INTENT(OUT) :: buffer
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen ! Length of the current_name string

    INTERFACE
       INTEGER FUNCTION h5gget_linkval_c(loc_id, name, namelen, size, buffer) BIND(C,NAME='h5gget_linkval_c')
         IMPORT :: C_CHAR, SIZE_T
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(SIZE_T), INTENT(IN) :: size
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: buffer
       END FUNCTION h5gget_linkval_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5gget_linkval_c(loc_id, name, namelen, size, buffer)
  END SUBROUTINE h5gget_linkval_f

!>
!! \ingroup FH5G
!!
!! \brief Sets comment for specified object.
!!
!! \param loc_id  Location identifier.
!! \param name    Name of the object.
!! \param comment Comment to set for the object.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Gset_comment()
!!
  SUBROUTINE h5gset_comment_f(loc_id, name, comment, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=*), INTENT(IN) :: comment
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen ! Length of the current_name string
    INTEGER :: commentlen     ! Length of the comment string

    INTERFACE
       INTEGER FUNCTION h5gset_comment_c(loc_id, name, namelen, &
            comment, commentlen) BIND(C,NAME='h5gset_comment_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: comment
         INTEGER :: commentlen
       END FUNCTION h5gset_comment_c
    END INTERFACE

    namelen = LEN(name)
    commentlen = LEN(comment)
    hdferr = h5gset_comment_c(loc_id, name, namelen, comment, commentlen)
  END SUBROUTINE h5gset_comment_f
!>
!! \ingroup FH5G
!!
!! \brief Retrieves comment for specified object.
!!
!! \param loc_id Location identifier.
!! \param name   Name of the object at specified location.
!! \param size   Size of the buffer required to hold comment.
!! \param buffer Buffer to hold object&apos;s comment.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Gget_comment()
!!
  SUBROUTINE h5gget_comment_f(loc_id, name, size, buffer, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    CHARACTER(LEN=size), INTENT(OUT) :: buffer
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen ! Length of the current_name string

    INTERFACE
       INTEGER FUNCTION h5gget_comment_c(loc_id, name, namelen, size, buffer) BIND(C,NAME='h5gget_comment_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(SIZE_T), INTENT(IN) :: size
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: buffer
       END FUNCTION h5gget_comment_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5gget_comment_c(loc_id, name, namelen, size, buffer)

  END SUBROUTINE h5gget_comment_f
!>
!! \ingroup FH5G
!!
!! \brief Creates a new empty group without linking it into the file structure.
!!
!! \param loc_id  Location identifier.
!! \param grp_id  Group identifier.
!! \param hdferr  \fortran_error
!! \param gcpl_id Group creation property list identifier.
!! \param gapl_id Group access property list identifier.
!!
!! See C API: @ref H5Gcreate_anon()
!!
  SUBROUTINE h5Gcreate_anon_f(loc_id, grp_id, hdferr, gcpl_id, gapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    INTEGER(HID_T), INTENT(OUT) :: grp_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: gcpl_id
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: gapl_id
    INTEGER(HID_T) :: gcpl_id_default
    INTEGER(HID_T) :: gapl_id_default

    INTERFACE
       INTEGER FUNCTION h5gcreate_anon_c(loc_id, gcpl_id_default, gapl_id_default, grp_id) &
            BIND(C,NAME='h5gcreate_anon_c')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(HID_T), INTENT(IN) :: gcpl_id_default
         INTEGER(HID_T), INTENT(IN) :: gapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: grp_id
       END FUNCTION h5gcreate_anon_c
    END INTERFACE

    gcpl_id_default = H5P_DEFAULT_F
    gapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(gcpl_id)) gcpl_id_default = gcpl_id
    IF(PRESENT(gapl_id)) gapl_id_default = gapl_id

    hdferr = h5gcreate_anon_c(loc_id, gcpl_id_default, gapl_id_default, grp_id)

  END SUBROUTINE h5Gcreate_anon_f
!>
!! \ingroup FH5G
!!
!! \brief Gets a group creation property list identifier.
!!
!! \param grp_id  Group identifier.
!! \param gcpl_id Group creation property list identifier.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Gget_create_plist()
!!
  SUBROUTINE h5gget_create_plist_f(grp_id, gcpl_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: grp_id
    INTEGER(HID_T), INTENT(OUT) :: gcpl_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5gget_create_plist_c(grp_id, gcpl_id ) BIND(C,NAME='h5gget_create_plist_c')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN)  :: grp_id
         INTEGER(HID_T), INTENT(OUT) :: gcpl_id
       END FUNCTION h5gget_create_plist_c
    END INTERFACE

    hdferr = h5gget_create_plist_c(grp_id, gcpl_id )

  END SUBROUTINE h5gget_create_plist_f

!>
!! \ingroup FH5G
!!
!! \brief Retrieves information about a group
!!
!! \param group_id     Group identifier.
!! \param storage_type Type of storage for links in group:
!!                     \li H5G_STORAGE_TYPE_COMPACT_F: Compact storage
!!                     \li H5G_STORAGE_TYPE_DENS_FE: Indexed storage
!!                     \li H5G_STORAGE_TYPE_SYMBOL_TABLE_F: Symbol tables, the original HDF5 structure
!! \param nlinks       Number of links in group.
!! \param max_corder   Current maximum creation order value for group.
!! \param hdferr       \fortran_error
!! \param mounted      Whether group has a file mounted on it.
!!
!! See C API: @ref H5Gget_info()
!!
  SUBROUTINE h5gget_info_f(group_id, storage_type, nlinks, max_corder, hdferr, mounted)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: group_id

    INTEGER, INTENT(OUT) :: storage_type
    INTEGER, INTENT(OUT) :: nlinks
    INTEGER, INTENT(OUT) :: max_corder
    INTEGER, INTENT(OUT) :: hdferr
    LOGICAL, INTENT(OUT), OPTIONAL :: mounted
    INTEGER :: mounted_c

    INTERFACE
       INTEGER FUNCTION h5gget_info_c(group_id, storage_type, nlinks, max_corder, mounted_c) &
            BIND(C,NAME='h5gget_info_c')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: group_id
         INTEGER, INTENT(OUT) :: storage_type
         INTEGER, INTENT(OUT) :: nlinks
         INTEGER, INTENT(OUT) :: max_corder
         INTEGER :: mounted_c
       END FUNCTION h5gget_info_c
    END INTERFACE

    hdferr = h5gget_info_c(group_id, storage_type, nlinks, max_corder, mounted_c)

    IF(PRESENT(mounted))THEN
       IF(mounted_c.EQ.0) THEN
          mounted = .FALSE.
       ELSE
          mounted = .TRUE.
       ENDIF
    ENDIF

  END SUBROUTINE h5gget_info_f
!>
!! \ingroup FH5G
!!
!! \brief Retrieves information about a group, according to the group’s position within an index.
!!
!! \param loc_id       File or group identifier.
!! \param group_name   Name of group containing group for which information is to be retrieved.
!! \param index_type   Index type.
!! \param order        Order of the count in the index.
!! \param n            Position in the index of the group for which information is retrieved.
!! \param storage_type Type of storage for links in group:
!!                     \li H5G_STORAGE_TYPE_COMPACT_F: Compact storage
!!                     \li H5G_STORAGE_TYPE_DENSE_F: Indexed storage
!!                     \li H5G_STORAGE_TYPE_SYMBOL_TABLE_F: Symbol tables, the original HDF5 structure
!! \param nlinks       Number of links in group.
!! \param max_corder   Current maximum creation order value for group.
!! \param hdferr       \fortran_error
!! \param lapl_id      Link access property list.
!! \param mounted      Whether group has a file mounted on it.
!!
!! See C API: @ref H5Gget_info_by_idx()
!!
  SUBROUTINE h5gget_info_by_idx_f(loc_id, group_name, index_type, order, n, &
       storage_type, nlinks, max_corder, hdferr, lapl_id, mounted)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: group_name
    INTEGER, INTENT(IN) :: index_type
    INTEGER, INTENT(IN) :: order
    INTEGER(HSIZE_T), INTENT(IN) :: n

    INTEGER, INTENT(OUT) :: storage_type
    INTEGER, INTENT(OUT) :: nlinks
    INTEGER, INTENT(OUT) :: max_corder
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id
    LOGICAL, INTENT(OUT), OPTIONAL :: mounted
    INTEGER :: mounted_c
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T) :: group_name_len ! length of group name

    INTERFACE
       INTEGER FUNCTION h5gget_info_by_idx_c(loc_id, group_name, group_name_len, index_type, order, n, lapl_id_default, &
            storage_type, nlinks, max_corder, mounted_c) BIND(C,NAME='h5gget_info_by_idx_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: group_name
         INTEGER, INTENT(IN) :: index_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(HID_T) :: lapl_id_default
         INTEGER, INTENT(OUT) :: storage_type
         INTEGER, INTENT(OUT) :: nlinks
         INTEGER, INTENT(OUT) :: max_corder

         INTEGER(SIZE_T) :: group_name_len
         INTEGER :: mounted_c

       END FUNCTION h5gget_info_by_idx_c
    END INTERFACE

    group_name_len = LEN(group_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5gget_info_by_idx_c(loc_id, group_name, group_name_len, &
         index_type, order, n, lapl_id_default, &
         storage_type, nlinks, max_corder, mounted_c)

    IF(PRESENT(mounted))THEN
       IF(mounted_c.EQ.0) THEN
          mounted = .FALSE.
       ELSE
          mounted = .TRUE.
       ENDIF
    ENDIF

  END SUBROUTINE h5gget_info_by_idx_f
!>
!! \ingroup FH5G
!!
!! \brief Retrieves information about a group.
!!
!! \param loc_id       File or group identifier.
!! \param group_name   Name of group containing group for which information is to be retrieved.
!! \param storage_type Type of storage for links in group:
!!                     \li H5G_STORAGE_TYPE_COMPACT_F: Compact storage
!!                     \li H5G_STORAGE_TYPE_DENSE_F: Indexed storage
!!                     \li H5G_STORAGE_TYPE_SYMBOL_TABLE_F: Symbol tables, the original HDF5 structure
!! \param nlinks       Number of links in group.
!! \param max_corder   Current maximum creation order value for group.
!! \param hdferr       \fortran_error
!! \param lapl_id      Link access property list.
!! \param mounted      Whether group has a file mounted on it.
!!
!! See C API: @ref H5Gget_info_by_name()
!!
  SUBROUTINE h5gget_info_by_name_f(loc_id, group_name, &
       storage_type, nlinks, max_corder, hdferr, lapl_id, mounted)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: group_name

    INTEGER, INTENT(OUT) :: storage_type
    INTEGER, INTENT(OUT) :: nlinks
    INTEGER, INTENT(OUT) :: max_corder
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id
    LOGICAL, INTENT(OUT), OPTIONAL :: mounted
    INTEGER :: mounted_c
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T) :: group_name_len ! length of group name

    INTERFACE
       INTEGER FUNCTION h5gget_info_by_name_c(loc_id, group_name, group_name_len, lapl_id_default, &
            storage_type, nlinks, max_corder, mounted_c) BIND(C,NAME='h5gget_info_by_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: group_name
         INTEGER(HID_T), INTENT(IN) :: lapl_id_default
         INTEGER, INTENT(OUT) :: storage_type
         INTEGER, INTENT(OUT) :: nlinks
         INTEGER, INTENT(OUT) :: max_corder

         INTEGER(SIZE_T) :: group_name_len
         INTEGER :: mounted_c

       END FUNCTION h5gget_info_by_name_c
    END INTERFACE

    group_name_len = LEN(group_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5gget_info_by_name_c(loc_id, group_name, group_name_len, lapl_id_default, &
         storage_type, nlinks, max_corder, mounted_c)

    IF(PRESENT(mounted))THEN
       IF(mounted_c.EQ.0) THEN
          mounted = .FALSE.
       ELSE
          mounted = .TRUE.
       ENDIF
    ENDIF

  END SUBROUTINE h5gget_info_by_name_f

END MODULE H5G
