!> @defgroup FH5T Fortran Datatype (H5T) Interface
!!
!! @see H5T, C-API
!!
!! @see @ref H5T_UG, User Guide
!!

!> @ingroup FH5T
!!
!! @brief This module contains Fortran interfaces for H5T functions.
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
!  If you add a new function here then you MUST add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5T

  USE H5GLOBAL
  IMPLICIT NONE

  PRIVATE h5tenum_insert_f03, h5tenum_insert_f90

! Fortran2003 Derived Type:
  TYPE hvl_t
     INTEGER(size_t) :: len !< Length of VL data (in base type units)
     TYPE(C_PTR)     :: p   !< Pointer to VL data
  END TYPE hvl_t

#ifndef H5_DOXYGEN

  INTERFACE h5tenum_insert_f
     MODULE PROCEDURE h5tenum_insert_f03
     MODULE PROCEDURE h5tenum_insert_f90
  END INTERFACE

#endif

CONTAINS

!>
!! \ingroup FH5T
!!
!! \brief Opens named datatype.
!!
!! \param loc_id  Location identifier.
!! \param name    A datatype name.
!! \param type_id Datatype identifier.
!! \param hdferr  \fortran_error
!! \param tapl_id Datatype access property list identifier.
!!
!! See C API: @ref H5Topen2()
!!
  SUBROUTINE h5topen_f(loc_id, name, type_id, hdferr, tapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(OUT) :: type_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: tapl_id
    INTEGER :: namelen                  ! Name length
    INTEGER(HID_T) :: tapl_id_default

    INTERFACE
       INTEGER FUNCTION h5topen_c(loc_id, name, namelen, type_id, tapl_id_default) BIND(C,NAME='h5topen_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(HID_T), INTENT(OUT) :: type_id
         INTEGER(HID_T) :: tapl_id_default
       END FUNCTION h5topen_c
    END INTERFACE

    namelen = LEN(name)

    tapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(tapl_id)) tapl_id_default = tapl_id

    hdferr = h5topen_c(loc_id, name, namelen, type_id, tapl_id_default)
  END SUBROUTINE h5topen_f
!>
!! \ingroup FH5T
!!
!! \brief Commits a transient datatype to a file, creating a new named datatype.
!!
!! \param loc_id  Location identifier.
!! \param name    Name of the datatype to be stored at the specified location
!! \param type_id Identifier of a datatype to be stored.
!! \param hdferr  \fortran_error
!! \param lcpl_id Link creation property list.
!! \param tcpl_id Datatype creation property list.
!! \param tapl_id Datatype access property list.
!!
!! See C API: @ref H5Tcommit2()
!!
  SUBROUTINE h5tcommit_f(loc_id, name, type_id, hdferr, &
       lcpl_id, tcpl_id, tapl_id  )
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: tcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: tapl_id

    INTEGER :: namelen          ! Name length

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: tcpl_id_default
    INTEGER(HID_T) :: tapl_id_default

    INTERFACE
       INTEGER FUNCTION h5tcommit_c(loc_id, name, namelen, type_id, &
            lcpl_id_default, tcpl_id_default, tapl_id_default ) BIND(C,NAME='h5tcommit_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: tcpl_id_default
         INTEGER(HID_T) :: tapl_id_default
       END FUNCTION h5tcommit_c
    END INTERFACE

    lcpl_id_default = H5P_DEFAULT_F
    tcpl_id_default = H5P_DEFAULT_F
    tapl_id_default = H5P_DEFAULT_F

    IF (PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    IF (PRESENT(tcpl_id)) tcpl_id_default = tcpl_id
    IF (PRESENT(tapl_id)) tapl_id_default = tapl_id

    namelen = LEN(name)

    hdferr = h5tcommit_c(loc_id, name, namelen, type_id, &
         lcpl_id_default, tcpl_id_default, tapl_id_default )

  END SUBROUTINE h5tcommit_f
!>
!! \ingroup FH5T
!!
!! \brief Creates a copy of existing datatype.
!!
!! \param type_id     Datatype identifier.
!! \param new_type_id Identifier of datatype&apos;s copy.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Tcopy()
!!
  SUBROUTINE h5tcopy_f(type_id, new_type_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(HID_T), INTENT(OUT) :: new_type_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION  h5tcopy_c(type_id, new_type_id) BIND(C,NAME='h5tcopy_c')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(OUT) :: new_type_id
       END FUNCTION h5tcopy_c
    END INTERFACE

    hdferr = h5tcopy_c(type_id, new_type_id)
  END SUBROUTINE h5tcopy_f
!>
!! \ingroup FH5T
!!
!! \brief Determines whether two datatype identifiers refer to the same datatype.
!!
!! \param type1_id Datatype identifier.
!! \param type2_id Datatype identifier.
!! \param flag     TRUE/FALSE flag to indicate if two datatypes are equal.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Tequal()
!!
  SUBROUTINE h5tequal_f(type1_id, type2_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type1_id
    INTEGER(HID_T), INTENT(IN) :: type2_id
    LOGICAL, INTENT(OUT) :: flag
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: c_flag
    INTERFACE
       INTEGER FUNCTION h5tequal_c(type1_id, type2_id, c_flag) BIND(C,NAME='h5tequal_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type1_id
         INTEGER(HID_T), INTENT(IN) :: type2_id
         INTEGER :: c_flag
       END FUNCTION h5tequal_c
    END INTERFACE

    flag = .FALSE.
    hdferr = h5tequal_c(type1_id, type2_id, c_flag)
    IF(c_flag .GT. 0) flag = .TRUE.
  END SUBROUTINE h5tequal_f
!>
!! \ingroup FH5T
!!
!! \brief Releases a datatype.
!!
!! \param type_id Datatype identifier.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tclose()
!!
  SUBROUTINE h5tclose_f(type_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tclose_c(type_id) BIND(C,NAME='h5tclose_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
       END FUNCTION h5tclose_c
    END INTERFACE

    hdferr = h5tclose_c(type_id)
  END SUBROUTINE h5tclose_f
!>
!! \ingroup FH5T
!!
!! \brief Returns the datatype class identifier.
!!
!! \param type_id Datatype identifier.
!! \param class   Class, possible values are:
!!                \li H5T_NO_CLASS_F
!!                \li H5T_INTEGER_F
!!                \li H5T_FLOAT_F
!!                \li H5T_TIME_F
!!                \li H5T_STRING_F
!!                \li H5T_BITFIELD_F
!!                \li H5T_OPAQUE_F
!!                \li H5T_COMPOUND_F
!!                \li H5T_REFERENCE_F
!!                \li H5T_ENUM_F
!!                \li H5T_VLEN_F
!!                \li H5T_ARRAY_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_class()
!!
  SUBROUTINE h5tget_class_f(type_id, class, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: class
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_class_c(type_id, class) BIND(C,NAME='h5tget_class_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(OUT) :: class
       END FUNCTION h5tget_class_c
    END INTERFACE

    hdferr = h5tget_class_c(type_id, class)
  END SUBROUTINE h5tget_class_f
!>
!! \ingroup FH5T
!!
!! \brief Returns the size of a datatype.
!!
!! \param type_id Datatype identifier.
!! \param size    Datatype size.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_size()
!!
  SUBROUTINE h5tget_size_f(type_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(SIZE_T), INTENT(OUT) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_size_c(type_id, size) BIND(C,NAME='h5tget_size_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(SIZE_T), INTENT(OUT) :: size
       END FUNCTION h5tget_size_c
    END INTERFACE

    hdferr = h5tget_size_c(type_id, size)
  END SUBROUTINE h5tget_size_f

!>
!! \ingroup FH5T
!!
!! \brief Sets the total size for an atomic datatype.
!!
!! \param type_id Datatype identifier.
!! \param size    Size of the datatype.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tset_size()
!!
  SUBROUTINE h5tset_size_f(type_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(SIZE_T), INTENT(IN) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tset_size_c(type_id, size) BIND(C,NAME='h5tset_size_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(SIZE_T), INTENT(IN) :: size
       END FUNCTION h5tset_size_c
    END INTERFACE

    hdferr = h5tset_size_c(type_id, size)
  END SUBROUTINE h5tset_size_f

!>
!! \ingroup FH5T
!!
!! \brief Returns the byte order of an atomic datatype.
!!
!! \param type_id Datatype identifier.
!! \param order   Byte order for the datatype, possible values are:
!!                \li H5T_ORDER_LE_F
!!                \li H5T_ORDER_BE_F
!!                \li H5T_ORDER_VAX_F (not implemented yet)
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_order()
!!
  SUBROUTINE h5tget_order_f(type_id, order, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: order
                                    ! H5T_ORDER_VAX_F
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_order_c(type_id, order) BIND(C,NAME='h5tget_order_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(OUT) :: order
       END FUNCTION h5tget_order_c
    END INTERFACE

    hdferr = h5tget_order_c(type_id, order)
  END SUBROUTINE h5tget_order_f
!>
!! \ingroup FH5T
!!
!! \brief Sets the byte ordering of an atomic datatype.
!!
!! \param type_id Datatype identifier.
!! \param order   Datatype byte order Possible values are:
!!                \li H5T_ORDER_LE_F
!!                \li H5T_ORDER_BE_F
!!                \li H5T_ORDER_VAX_F (not implemented yet)
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tset_order()
!!
  SUBROUTINE h5tset_order_f(type_id, order, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN) :: order
                                   ! H5T_ORDER_VAX_F
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tset_order_c(type_id, order) BIND(C,NAME='h5tset_order_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: order
       END FUNCTION h5tset_order_c
    END INTERFACE

    hdferr = h5tset_order_c(type_id, order)
  END SUBROUTINE h5tset_order_f

!>
!! \ingroup FH5T
!!
!! \brief Returns the precision of an atomic datatype.
!!
!! \param type_id   Datatype identifier.
!! \param precision Precision of the datatype.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Tget_precision()
!!
  SUBROUTINE h5tget_precision_f(type_id, PRECISION, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(SIZE_T), INTENT(OUT) :: precision
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_precision_c(type_id, PRECISION) BIND(C,NAME='h5tget_precision_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(SIZE_T), INTENT(OUT) :: PRECISION
       END FUNCTION h5tget_precision_c
    END INTERFACE

    hdferr = h5tget_precision_c(type_id, PRECISION)
  END SUBROUTINE h5tget_precision_f

!>
!! \ingroup FH5T
!!
!! \brief Sets the precision of an atomic datatype.
!!
!! \param type_id   Datatype identifier.
!! \param precision Datatype precision.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Tset_precision()
!!
  SUBROUTINE h5tset_precision_f(type_id, PRECISION, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(SIZE_T), INTENT(IN) :: PRECISION
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tset_precision_c (type_id, PRECISION) BIND(C,NAME='h5tset_precision_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(SIZE_T), INTENT(IN) :: PRECISION
       END FUNCTION h5tset_precision_c
    END INTERFACE

    hdferr = h5tset_precision_c(type_id, PRECISION)
  END SUBROUTINE h5tset_precision_f

!>
!! \ingroup FH5T
!!
!! \brief Retrieves the bit offset of the first significant bit.
!!
!! \param type_id Datatype identifier.
!! \param offset  Offset value.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_offset()
!!
  SUBROUTINE h5tget_offset_f(type_id, offset, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(SIZE_T), INTENT(OUT) :: offset
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_offset_c(type_id, offset) BIND(C,NAME='h5tget_offset_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(SIZE_T), INTENT(OUT) :: offset
       END FUNCTION h5tget_offset_c
    END INTERFACE

    hdferr = h5tget_offset_c(type_id, offset)
  END SUBROUTINE h5tget_offset_f

!>
!! \ingroup FH5T
!!
!! \brief Sets the bit offset of the first significant bit.
!!
!! \param type_id Datatype identifier.
!! \param offset  Offset value.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tset_offset()
!!
  SUBROUTINE h5tset_offset_f(type_id, offset, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(SIZE_T), INTENT(IN) :: offset
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tset_offset_c(type_id, offset) BIND(C,NAME='h5tset_offset_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(SIZE_T), INTENT(IN) :: offset
       END FUNCTION h5tset_offset_c
    END INTERFACE

    hdferr = h5tset_offset_c(type_id, offset)
  END SUBROUTINE h5tset_offset_f

!>
!! \ingroup FH5T
!!
!! \brief Retrieves the padding type of the least and most-significant bit padding.
!!
!! \param type_id Datatype identifier.
!! \param lsbpad  Least-significant bit padding type.
!! \param msbpad  Most-significant bit padding type. Possible values are:
!!                \li H5T_PAD_ERROR_F
!!                \li H5T_PAD_ZERO_F
!!                \li H5T_PAD_ONE_F
!!                \li H5T_PAD_BACKGROUND_F
!!                \li H5T_PAD_NPAD_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_pad()
!!
  SUBROUTINE h5tget_pad_f(type_id, lsbpad, msbpad, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: lsbpad
    INTEGER, INTENT(OUT) :: msbpad
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_pad_c(type_id, lsbpad, msbpad) BIND(C,NAME='h5tget_pad_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(OUT) :: lsbpad
         INTEGER, INTENT(OUT) :: msbpad
       END FUNCTION h5tget_pad_c
    END INTERFACE

    hdferr = h5tget_pad_c(type_id, lsbpad, msbpad)
  END SUBROUTINE h5tget_pad_f

!>
!! \ingroup FH5T
!!
!! \brief Sets the least and most-significant bits padding types.
!!
!! \param type_id Datatype identifier.
!! \param lsbpad  Least-significant bit padding type.
!! \param msbpad  Most-significant bit padding type. Possible values are:
!!                \li H5T_PAD_ERROR_F
!!                \li H5T_PAD_ZERO_F
!!                \li H5T_PAD_ONE_F
!!                \li H5T_PAD_BACKGROUND_F
!!                \li H5T_PAD_NPAD_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tset_pad()
!!
  SUBROUTINE h5tset_pad_f(type_id, lsbpad, msbpad, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN) :: lsbpad
    INTEGER, INTENT(IN) :: msbpad
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tset_pad_c(type_id, lsbpad, msbpad) BIND(C,NAME='h5tset_pad_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: lsbpad
         INTEGER, INTENT(IN) :: msbpad
       END FUNCTION h5tset_pad_c
    END INTERFACE

    hdferr = h5tset_pad_c(type_id, lsbpad, msbpad)
  END SUBROUTINE h5tset_pad_f

!>
!! \ingroup FH5T
!!
!! \brief Retrieves the sign type for an integer type.
!!
!! \param type_id  Datatype identifier.
!! \param sign     Sign type. Possible values are:
!!                \li  Unsigned integer type
!!                     H5T_SGN_NONE_F = 0
!!                \li  Two&apos;s complement signed integer type
!!                     H5T_SGN_2_F = 1
!!                \li  Error value
!!                     H5T_SGN_ERROR_F = -1
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Tget_sign()
!!
  SUBROUTINE h5tget_sign_f(type_id, sign, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: sign
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5tget_sign_c(type_id, sign) BIND(C,NAME='h5tget_sign_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(OUT) :: sign
       END FUNCTION h5tget_sign_c
    END INTERFACE

    hdferr = h5tget_sign_c(type_id, sign)
  END SUBROUTINE h5tget_sign_f

!>
!! \ingroup FH5T
!!
!! \brief Sets the sign property for an integer type.
!!
!! \param type_id  Datatype identifier.
!! \param sign     Sign type. Possible values are:
!!                 \li Unsigned integer type
!!                     H5T_SGN_NONE_F = 0
!!                 \li Two&apos;s complement signed integer type
!!                     H5T_SGN_2_F = 1
!!                 \li Error value
!!                     H5T_SGN_ERROR_F = -1
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Tset_sign()
!!
  SUBROUTINE h5tset_sign_f(type_id, sign, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN) :: sign
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tset_sign_c(type_id, sign) BIND(C,NAME='h5tset_sign_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: sign
       END FUNCTION h5tset_sign_c
    END INTERFACE

    hdferr = h5tset_sign_c(type_id, sign)
  END SUBROUTINE h5tset_sign_f

!>
!! \ingroup FH5T
!!
!! \brief Retrieves floating point datatype bit field information.
!!
!! \param type_id Datatype identifier.
!! \param spos    Sign bit-position.
!! \param epos    Exponent bit-position.
!! \param esize   Size of exponent in bits.
!! \param mpos    Mantissa position.
!! \param msize   Size of mantissa in bits.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_fields()
!!
  SUBROUTINE h5tget_fields_f(type_id, spos, epos, esize, mpos, msize, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(SIZE_T), INTENT(OUT) :: spos
    INTEGER(SIZE_T), INTENT(OUT) :: epos
    INTEGER(SIZE_T), INTENT(OUT) :: esize
    INTEGER(SIZE_T), INTENT(OUT) :: mpos
    INTEGER(SIZE_T), INTENT(OUT) :: msize
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5tget_fields_c(type_id, spos, epos, esize, mpos, msize) &
            BIND(C,NAME='h5tget_fields_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(SIZE_T), INTENT(OUT) :: spos
         INTEGER(SIZE_T), INTENT(OUT) :: epos
         INTEGER(SIZE_T), INTENT(OUT) :: esize
         INTEGER(SIZE_T), INTENT(OUT) :: mpos
         INTEGER(SIZE_T), INTENT(OUT) :: msize
       END FUNCTION h5tget_fields_c
    END INTERFACE

    hdferr = h5tget_fields_c(type_id, spos, epos, esize, mpos, msize)
  END SUBROUTINE h5tget_fields_f

!>
!! \ingroup FH5T
!!
!! \brief Sets locations and sizes of floating point bit fields.
!!
!! \param type_id Datatype identifier.
!! \param spos    Sign bit-position.
!! \param epos    Exponent bit-position.
!! \param esize   Size of exponent in bits.
!! \param mpos    Mantissa position.
!! \param msize   Size of mantissa in bits.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tset_fields()
!!
  SUBROUTINE h5tset_fields_f(type_id, spos, epos, esize, mpos, msize, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(SIZE_T), INTENT(IN) :: spos
    INTEGER(SIZE_T), INTENT(IN) :: epos
    INTEGER(SIZE_T), INTENT(IN) :: esize
    INTEGER(SIZE_T), INTENT(IN) :: mpos
    INTEGER(SIZE_T), INTENT(IN) :: msize
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5tset_fields_c(type_id, spos, epos, esize, mpos, msize) &
            BIND(C,NAME='h5tset_fields_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(SIZE_T), INTENT(IN) :: spos
         INTEGER(SIZE_T), INTENT(IN) :: epos
         INTEGER(SIZE_T), INTENT(IN) :: esize
         INTEGER(SIZE_T), INTENT(IN) :: mpos
         INTEGER(SIZE_T), INTENT(IN) :: msize
       END FUNCTION h5tset_fields_c
    END INTERFACE

    hdferr = h5tset_fields_c(type_id, spos, epos, esize, mpos, msize)
  END SUBROUTINE h5tset_fields_f

!>
!! \ingroup FH5T
!!
!! \brief Retrieves the exponent bias of a floating-point type.
!!
!! \param type_id Datatype identifier.
!! \param ebias   Datatype exponent bias.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_ebias()
!!
  SUBROUTINE h5tget_ebias_f(type_id, ebias, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(SIZE_T), INTENT(OUT) :: ebias
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5tget_ebias_c(type_id, ebias) BIND(C,NAME='h5tget_ebias_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(SIZE_T), INTENT(OUT) :: ebias
       END FUNCTION h5tget_ebias_c
    END INTERFACE

    hdferr = h5tget_ebias_c(type_id, ebias)
  END SUBROUTINE h5tget_ebias_f

!>
!! \ingroup FH5T
!!
!! \brief Sets the exponent bias of a floating-point type.
!!
!! \param type_id Datatype identifier.
!! \param ebias   Datatype exponent bias.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tset_ebias()
!!
  SUBROUTINE h5tset_ebias_f(type_id, ebias, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(SIZE_T), INTENT(IN) :: ebias
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tset_ebias_c(type_id, ebias) BIND(C,NAME='h5tset_ebias_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(SIZE_T), INTENT(IN) :: ebias
       END FUNCTION h5tset_ebias_c
    END INTERFACE

    hdferr = h5tset_ebias_c(type_id, ebias)
  END SUBROUTINE h5tset_ebias_f

!>
!! \ingroup FH5T
!!
!! \brief Retrieves mantissa normalization of a floating-point datatype.
!!
!! \param type_id Datatype identifier.
!! \param norm    Normalization types, valid values are:
!!                \li H5T_NORM_IMPLIED_F
!!                \li H5T_NORM_MSBSET_F
!!                \li H5T_NORM_NONE_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_norm()
!!
  SUBROUTINE h5tget_norm_f(type_id, norm, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: norm
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5tget_norm_c(type_id, norm) BIND(C,NAME='h5tget_norm_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(OUT) :: norm
       END FUNCTION h5tget_norm_c
    END INTERFACE

    hdferr = h5tget_norm_c(type_id, norm)
  END SUBROUTINE h5tget_norm_f

!>
!! \ingroup FH5T
!!
!! \brief Sets the mantissa normalization of a floating-point datatype.
!!
!! \param type_id Datatype identifier.
!! \param norm    Normalization types, valid values are:
!!                \li H5T_NORM_IMPLIED_F
!!                \li H5T_NORM_MSBSET_F
!!                \li H5T_NORM_NONE_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tset_norm()
!!
  SUBROUTINE h5tset_norm_f(type_id, norm, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN) :: norm
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tset_norm_c(type_id, norm) BIND(C,NAME='h5tset_norm_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: norm
       END FUNCTION h5tset_norm_c
    END INTERFACE

    hdferr = h5tset_norm_c(type_id, norm)
  END SUBROUTINE h5tset_norm_f

!>
!! \ingroup FH5T
!!
!! \brief Retrieves the internal padding type for unused bits in floating-point datatypes.
!!
!! \param type_id Datatype identifier.
!! \param padtype Padding type for unused bits. Possible values are:
!!                \li H5T_PAD_ZERO_F
!!                \li H5T_PAD_ONE_F
!!                \li H5T_PAD_BACKGROUND_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_inpad()
!!
  SUBROUTINE h5tget_inpad_f(type_id, padtype, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: padtype
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_inpad_c(type_id, padtype) BIND(C,NAME='h5tget_inpad_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(OUT) :: padtype
       END FUNCTION h5tget_inpad_c
    END INTERFACE

    hdferr = h5tget_inpad_c(type_id, padtype)
  END SUBROUTINE h5tget_inpad_f

!>
!! \ingroup FH5T
!!
!! \brief Fills unused internal floating point bits.
!!
!! \param type_id Datatype identifier.
!! \param padtype Padding type for unused bits. Possible values are:
!!                \li H5T_PAD_ZERO_F
!!                \li H5T_PAD_ONE_F
!!                \li H5T_PAD_BACKGROUND_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tset_inpad()
!!
  SUBROUTINE h5tset_inpad_f(type_id, padtype, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN) :: padtype
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tset_inpad_c(type_id, padtype) BIND(C,NAME='h5tset_inpad_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: padtype
       END FUNCTION h5tset_inpad_c
    END INTERFACE

    hdferr = h5tset_inpad_c(type_id, padtype)
  END SUBROUTINE h5tset_inpad_f

!>
!! \ingroup FH5T
!!
!! \brief Retrieves the character set type of a string datatype.
!!
!! \param type_id Datatype identifier.
!! \param cset    Character set type of a string datatype. Possible values are:
!!                \li H5T_CSET_ASCII_F
!!                \li H5T_CSET_UTF8_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_cset()
!!
  SUBROUTINE h5tget_cset_f(type_id, cset, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: cset
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_cset_c(type_id, cset) BIND(C,NAME='h5tget_cset_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(OUT) :: cset
       END FUNCTION h5tget_cset_c
    END INTERFACE

    hdferr = h5tget_cset_c(type_id, cset)
  END SUBROUTINE h5tget_cset_f

!>
!! \ingroup FH5T
!!
!! \brief Sets character set to be used.
!!
!! \param type_id Datatype identifier.
!! \param cset    Character set type of a string datatype. Possible values are:
!!                \li H5T_CSET_ASCII_F
!!                \li H5T_CSET_UTF8_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tset_cset()
!!
  SUBROUTINE h5tset_cset_f(type_id, cset, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN) :: cset
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tset_cset_c(type_id, cset) BIND(C,NAME='h5tset_cset_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: cset
       END FUNCTION h5tset_cset_c
    END INTERFACE

    hdferr = h5tset_cset_c(type_id, cset)
  END SUBROUTINE h5tset_cset_f
!>
!! \ingroup FH5T
!!
!! \brief Retrieves the storage mechanism for a string datatype.
!!
!! \param type_id Datatype identifier.
!! \param strpad  Storage method for a string datatype. Possible values are:
!!                \li H5T_STR_NULLTERM_F
!!                \li H5T_STR_NULLPAD_F
!!                \li H5T_STR_SPACEPAD_F
!!                \li H5T_STR_ERROR_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_strpad()
!!
  SUBROUTINE h5tget_strpad_f(type_id, strpad, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: strpad
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_strpad_c(type_id, strpad) BIND(C,NAME='h5tget_strpad_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(OUT) :: strpad
       END FUNCTION h5tget_strpad_c
    END INTERFACE

    hdferr = h5tget_strpad_c(type_id, strpad)
  END SUBROUTINE h5tget_strpad_f

!>
!! \ingroup FH5T
!!
!! \brief Defines the storage mechanism for character strings.
!!
!! \param type_id Datatype identifier.
!! \param strpad  Storage method for a string datatype. Possible values are:
!!                \li H5T_STR_NULLTERM_F
!!                \li H5T_STR_NULLPAD_F
!!                \li H5T_STR_SPACEPAD_F
!!                \li H5T_STR_ERROR_F
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tset_strpad()
!!
  SUBROUTINE h5tset_strpad_f(type_id, strpad, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN) :: strpad
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tset_strpad_c(type_id, strpad) BIND(C,NAME='h5tset_strpad_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: strpad
       END FUNCTION h5tset_strpad_c
    END INTERFACE

    hdferr = h5tset_strpad_c(type_id, strpad)
  END SUBROUTINE h5tset_strpad_f

!>
!! \ingroup FH5T
!!
!! \brief Retrieves the number of fields in a compound datatype.
!!
!! \param type_id     Datatype identifier.
!! \param num_members Number of members.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Tget_nmembers()
!!
  SUBROUTINE h5tget_nmembers_f(type_id, num_members, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: num_members
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_nmembers_c(type_id, num_members) BIND(C,NAME='h5tget_nmembers_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(OUT) :: num_members
       END FUNCTION h5tget_nmembers_c
    END INTERFACE

    hdferr = h5tget_nmembers_c(type_id, num_members)
  END SUBROUTINE h5tget_nmembers_f

!>
!! \ingroup FH5T
!!
!! \brief Retrieves the name of a field of a compound datatype.
!!
!! \param type_id     Datatype identifier.
!! \param index       Filed index (0-based).
!! \param member_name Buffer to hold member&apos;s name.
!! \param namelen     Name length.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Tget_member_name()
!!
  SUBROUTINE h5tget_member_name_f(type_id, index, member_name,  namelen, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=*), INTENT(OUT) :: member_name
    INTEGER, INTENT(OUT) :: namelen
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_member_name_c(type_id, index, member_name, namelen) BIND(C,NAME='h5tget_member_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: index
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: member_name
         INTEGER, INTENT(OUT) :: namelen
       END FUNCTION h5tget_member_name_c
    END INTERFACE

    hdferr = h5tget_member_name_c(type_id, index, member_name, namelen)
  END SUBROUTINE h5tget_member_name_f

!>
!! \ingroup FH5T
!!
!! \brief Retrieves the offset of a field of a compound datatype.
!!
!! \param type_id   Datatype identifier.
!! \param member_no Number of the field.
!! \param offset    Byte offset of the requested field.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Tget_member_offset()
!!
  SUBROUTINE h5tget_member_offset_f(type_id, member_no, offset, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN) :: member_no
    INTEGER(SIZE_T), INTENT(OUT) :: offset
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_member_offset_c(type_id, member_no, offset ) BIND(C,NAME='h5tget_member_offset_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: member_no
         INTEGER(SIZE_T), INTENT(OUT) :: offset
       END FUNCTION h5tget_member_offset_c
    END INTERFACE

    hdferr = h5tget_member_offset_c(type_id, member_no, offset )
  END SUBROUTINE h5tget_member_offset_f
!>
!! \ingroup FH5T
!!
!! \brief Retrieves the index of a compound or enumeration datatype member.
!!
!! \param type_id Datatype identifier.
!! \param name    Name of the field or member whose index to be retrieved from the datatype.
!! \param index   Based index of the filed or member (0 to N-1).
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_member_index()
!!
  SUBROUTINE h5tget_member_index_f(type_id, name, index, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: index
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen          ! Name length

    INTERFACE
       INTEGER FUNCTION h5tget_member_index_c(type_id, name, namelen, index) BIND(C,NAME='h5tget_member_index_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER, INTENT(IN)  :: namelen
         INTEGER, INTENT(OUT) :: index
       END FUNCTION h5tget_member_index_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5tget_member_index_c(type_id, name, namelen, index)
  END SUBROUTINE h5tget_member_index_f

!>
!! \ingroup FH5T
!!
!! \brief Returns sizes of array dimensions.
!!
!! \param type_id Array datatype identifier.
!! \param dims    Buffer to store array datatype dimensions.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_array_dims2()
!!
  SUBROUTINE h5tget_array_dims_f(type_id, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(HSIZE_T),DIMENSION(*), INTENT(OUT) :: dims
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_array_dims_c(type_id, dims) BIND(C,NAME='h5tget_array_dims_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HSIZE_T),DIMENSION(*), INTENT(OUT) ::  dims
       END FUNCTION h5tget_array_dims_c
    END INTERFACE

    hdferr = h5tget_array_dims_c(type_id, dims)

  END SUBROUTINE h5tget_array_dims_f

!>
!! \ingroup FH5T
!!
!! \brief Returns the rank of an array datatype.
!!
!! \param type_id Array datatype identifier.
!! \param ndims   Number of array dimensions.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_array_ndims()
!!
  SUBROUTINE h5tget_array_ndims_f(type_id, ndims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: ndims
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_array_ndims_c(type_id, ndims) BIND(C,NAME='h5tget_array_ndims_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(OUT) ::  ndims
       END FUNCTION h5tget_array_ndims_c
    END INTERFACE

    hdferr = h5tget_array_ndims_c(type_id, ndims)

  END SUBROUTINE h5tget_array_ndims_f

!>
!! \ingroup FH5T
!!
!! \brief Returns the base datatype from which a datatype is derived.
!!
!! \param type_id      Datatype identifier.
!! \param base_type_id Identifier of the base type.
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Tget_super()
!!
  SUBROUTINE h5tget_super_f(type_id, base_type_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(HID_T), INTENT(OUT) :: base_type_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_super_c(type_id, base_type_id) BIND(C,NAME='h5tget_super_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(OUT) :: base_type_id
       END FUNCTION h5tget_super_c
    END INTERFACE

    hdferr = h5tget_super_c(type_id, base_type_id)

  END SUBROUTINE h5tget_super_f

!>
!! \ingroup FH5T
!!
!! \brief Returns the datatype of the specified member.
!!
!! \param type_id   Compound datatype identifier.
!! \param field_idx Field index (0-based).
!! \param datatype  Identifier of the member&apos;s datatype.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Tget_member_type()
!!
  SUBROUTINE h5tget_member_type_f(type_id,  field_idx, datatype, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN) :: field_idx
    INTEGER(HID_T), INTENT(OUT) :: datatype
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_member_type_c(type_id, field_idx , datatype) &
            BIND(C,NAME='h5tget_member_type_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: field_idx
         INTEGER(HID_T), INTENT(OUT) :: datatype
       END FUNCTION h5tget_member_type_c
    END INTERFACE

    hdferr = h5tget_member_type_c(type_id, field_idx , datatype)
  END SUBROUTINE h5tget_member_type_f

!>
!! \ingroup FH5T
!!
!! \brief Creates a new datatype.
!!
!! \param class   Datatype class can be one of:
!!                \li H5T_COMPOUND_F
!!                \li H5T_ENUM_F
!!                \li H5T_OPAQUE_F
!!                \li H5T_STRING_F
!! \param size    Size of the datatype.
!! \param type_id Datatype identifier.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tcreate()
!!
  SUBROUTINE h5tcreate_f(class, size, type_id, hdferr)
    IMPLICIT NONE
    INTEGER        , INTENT(IN)  :: class
    INTEGER(SIZE_T), INTENT(IN)  :: size
    INTEGER(HID_T) , INTENT(OUT) :: type_id
    INTEGER        , INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tcreate_c(class, size, type_id) BIND(C,NAME='h5tcreate_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: class
         INTEGER(SIZE_T), INTENT(IN) :: size
         INTEGER(HID_T), INTENT(OUT) :: type_id
       END FUNCTION h5tcreate_c
    END INTERFACE

    hdferr = h5tcreate_c(class, size, type_id)
  END SUBROUTINE h5tcreate_f

!>
!! \ingroup FH5T
!!
!! \brief Adds a new member to a compound datatype.
!!
!! \param type_id  Compound datatype identifier.
!! \param name     Name of the field to insert.
!! \param offset   Start of the member in an instance of the compound datatype.
!! \param field_id Datatype identifier of the field to insert.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Tinsert()
!!
  SUBROUTINE h5tinsert_f(type_id,  name, offset, field_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: offset
    INTEGER(HID_T), INTENT(IN) :: field_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen

    INTERFACE
       INTEGER FUNCTION h5tinsert_c(type_id, name, namelen, offset, field_id) BIND(C,NAME='h5tinsert_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(SIZE_T), INTENT(IN) :: offset
         INTEGER(HID_T), INTENT(IN) :: field_id
         INTEGER :: namelen
       END FUNCTION h5tinsert_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5tinsert_c(type_id, name, namelen, offset, field_id )
  END SUBROUTINE h5tinsert_f

!>
!! \ingroup FH5T
!!
!! \brief Recursively removes padding from within a compound datatype.
!!
!! \param type_id Compound datatype identifier.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tpack()
!!
  SUBROUTINE h5tpack_f(type_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tpack_c(type_id) BIND(C,NAME='h5tpack_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
       END FUNCTION h5tpack_c
    END INTERFACE

    hdferr = h5tpack_c(type_id)
  END SUBROUTINE h5tpack_f

!
! NAME
!  h5tinsert_array_f
!
! PURPOSE
!  This function is not available on hdf5-1.4.*
!
! INPUTS
! OUTPUTS
!            hdferr:            - error code
!                               Success:  0
!                               Failure: -1
!
! SOURCE
!  SUBROUTINE h5tinsert_array_f(parent_id,name,offset, ndims, dims, member_id, hdferr, perm)
!  IMPLICIT NONE
!
!  INTEGER, EXTERNAL :: h5tinsert_array_c,  h5tinsert_array_c2
!  namelen = LEN(name)
!  if (present(perm)) then
!  hdferr = h5tinsert_array_c(parent_id, name, namelen, offset, ndims,dims, member_id, perm)
!  else
!  hdferr = h5tinsert_array_c2(parent_id, name, namelen, offset, ndims,dims, member_id)
!  end if
!
!  END SUBROUTINE h5tinsert_array_f

!>
!! \ingroup FH5T
!!
!! \brief Creates an array datatype object.
!!
!! \param base_id Datatype identifier for the array base datatype
!! \param rank    Rank of the array.
!! \param dims    Array dimension sizes.
!! \param type_id Array datatype identifier.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tarray_create2()
!!
  SUBROUTINE h5tarray_create_f(base_id, rank, dims, type_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: base_id
    INTEGER, INTENT(IN) ::  rank
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(HID_T), INTENT(OUT) :: type_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tarray_create_c(base_id, rank, dims, type_id) BIND(C,NAME='h5tarray_create_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: base_id
         INTEGER, INTENT(IN) ::  rank
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims
         INTEGER(HID_T), INTENT(OUT) :: type_id
       END FUNCTION h5tarray_create_c
    END INTERFACE

    hdferr = h5tarray_create_c(base_id, rank, dims, type_id)

  END SUBROUTINE h5tarray_create_f

!>
!! \ingroup FH5T
!!
!! \brief Creates a new enumeration datatype.
!!
!! \param parent_id   Datatype identifier for base datatype.
!! \param new_type_id Datatype identifier for the enumeration datatype.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Tenum_create()
!!
  SUBROUTINE h5tenum_create_f(parent_id, new_type_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: parent_id
    INTEGER(HID_T), INTENT(OUT) :: new_type_id
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5tenum_create_c(parent_id, new_type_id) BIND(C,NAME='h5tenum_create_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: parent_id
         INTEGER(HID_T), INTENT(OUT) :: new_type_id
       END FUNCTION h5tenum_create_c
    END INTERFACE

    hdferr = h5tenum_create_c(parent_id, new_type_id)
  END SUBROUTINE h5tenum_create_f
!>
!! \ingroup FH5T
!!
!! \brief Returns the symbol name corresponding to a specified member of an enumeration datatype.
!!
!! \param type_id Datatype identifier.
!! \param value   Value of the enumeration datatype.
!! \param namelen Name buffer size.
!! \param name    Buffer to hold symbol name.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tenum_nameof()
!!
  SUBROUTINE h5tenum_nameof_f(type_id,  value, namelen, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER(SIZE_T), INTENT(IN) :: namelen
    INTEGER, INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tenum_nameof_c(type_id, value, name, namelen) BIND(C,NAME='h5tenum_nameof_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: name
         INTEGER(SIZE_T), INTENT(IN) :: namelen
         INTEGER, INTENT(IN) :: value
       END FUNCTION h5tenum_nameof_c
    END INTERFACE

    name(1:LEN(name)) = ' '

    hdferr = h5tenum_nameof_c(type_id, value, name, namelen)
  END SUBROUTINE h5tenum_nameof_f
!>
!! \ingroup FH5T
!!
!! \brief Returns the value corresponding to a specified member of an enumeration datatype.
!!
!! \param type_id Datatype identifier.
!! \param name    Symbol name.
!! \param value   Value of the enumeration datatype.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tenum_valueof()
!!
  SUBROUTINE h5tenum_valueof_f(type_id,  name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen

    INTERFACE
       INTEGER FUNCTION h5tenum_valueof_c(type_id, name, namelen,  value) &
            BIND(C,NAME='h5tenum_valueof_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER, INTENT(IN) :: namelen
         INTEGER, INTENT(OUT) :: value
       END FUNCTION h5tenum_valueof_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5tenum_valueof_c(type_id, name, namelen,  value)
  END SUBROUTINE h5tenum_valueof_f

!>
!! \ingroup FH5T
!!
!! \brief Returns the value of an enumeration datatype member.
!!
!! \param type_id   Datatype identifier.
!! \param member_no Number of the enumeration datatype member.
!! \param value     Value of the enumeration datatype.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Tget_member_value()
!!
  SUBROUTINE h5tget_member_value_f(type_id,  member_no, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN) :: member_no
    INTEGER, INTENT(OUT) :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_member_value_c(type_id, member_no, value) &
            BIND(C,NAME='h5tget_member_value_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN) :: member_no
         INTEGER, INTENT(OUT) :: value
       END FUNCTION h5tget_member_value_c
    END INTERFACE

    hdferr = h5tget_member_value_c(type_id, member_no, value)
  END SUBROUTINE h5tget_member_value_f

!>
!! \ingroup FH5T
!!
!! \brief Tags an opaque datatype.
!!
!! \param type_id Identifier for opaque datatype.
!! \param tag     Unique ASCII string with which the opaque datatype is to be tagged.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tset_tag()
!!
  SUBROUTINE h5tset_tag_f(type_id, tag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    CHARACTER(LEN=*), INTENT(IN) :: tag
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: taglen

    INTERFACE
       INTEGER FUNCTION h5tset_tag_c(type_id, tag, taglen) BIND(C,NAME='h5tset_tag_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: tag
         INTEGER :: taglen
       END FUNCTION h5tset_tag_c
    END INTERFACE

    taglen = LEN(tag)
    hdferr = h5tset_tag_c(type_id, tag, taglen)
  END SUBROUTINE h5tset_tag_f

!>
!! \ingroup FH5T
!!
!! \brief Gets the tag associated with an opaque datatype.
!!
!! \param type_id Identifier for opaque datatype.
!! \param tag     Unique ASCII string associated with opaque datatype.
!! \param taglen  Length of tag.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tget_tag()
!!
  SUBROUTINE h5tget_tag_f(type_id, tag,taglen, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    CHARACTER(LEN=*), INTENT(OUT) :: tag
    INTEGER, INTENT(OUT) :: taglen
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(SIZE_T):: tag_size      ! Declared character length of tab
    INTERFACE
       INTEGER FUNCTION h5tget_tag_c(type_id, tag, tag_size, taglen) &
            BIND(C,NAME='h5tget_tag_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: tag
         INTEGER(SIZE_T), INTENT(IN) :: tag_size
         INTEGER, INTENT(OUT) :: taglen
       END FUNCTION h5tget_tag_c
    END INTERFACE

    tag_size = LEN(tag)
    hdferr = h5tget_tag_c(type_id, tag, tag_size, taglen )
  END SUBROUTINE h5tget_tag_f

!>
!! \ingroup FH5T
!!
!! \brief Creates a new variable-length datatype.
!!
!! \param type_id   Identifier iof base datatype.
!! \param vltype_id Identifier for VL datatype.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Tvlen_create()
!!
  SUBROUTINE h5tvlen_create_f(type_id, vltype_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: type_id
    INTEGER(HID_T), INTENT(OUT) :: vltype_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tvlen_create_c(type_id, vltype_id) BIND(C,NAME='h5tvlen_create_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN)  :: type_id
         INTEGER(HID_T), INTENT(OUT) :: vltype_id
       END FUNCTION h5tvlen_create_c
    END INTERFACE

    hdferr = h5tvlen_create_c(type_id, vltype_id)
  END SUBROUTINE h5tvlen_create_f

!>
!! \ingroup FH5T
!!
!! \brief Determines whether a dattype is a variable string.
!!
!! \param type_id Datartpe identifier.
!! \param status  Flag to indicate if datatype is a variable string ( TRUE or FALSE).
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Tis_variable_str()
!!
  SUBROUTINE h5tis_variable_str_f(type_id, status, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    LOGICAL, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: flag                     ! "TRUE/FALSE/ERROR from C"

    INTERFACE
       INTEGER FUNCTION h5tis_variable_str_c(type_id, flag) &
            BIND(C,NAME='h5tis_variable_str_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER :: flag
       END FUNCTION h5tis_variable_str_c
    END INTERFACE

    hdferr = h5tis_variable_str_c(type_id, flag)
    status = .TRUE.
    IF (flag .EQ. 0) status = .FALSE.

  END SUBROUTINE h5tis_variable_str_f

!>
!! \ingroup FH5T
!!
!! \brief Returns datatype class of compound datatype member.
!!
!! \param type_id   Datartpe identifier.
!! \param member_no Index of compound datatype member.
!! \param class     Class type for compound dadtype member. Valid classes:
!!                  \li H5T_NO_CLASS_F (error)
!!                  \li H5T_INTEGER_F
!!                  \li H5T_FLOAT_F
!!                  \li H5T_TIME_F
!!                  \li H5T_STRING_F
!!                  \li H5T_BITFIELD_F
!!                  \li H5T_OPAQUE_F
!!                  \li H5T_COMPOUND_F
!!                  \li H5T_REFERENCE_F
!!                  \li H5T_ENUM_F
!!                  \li H5T_VLEN_F
!!                  \li H5T_ARRAY_F
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Tget_member_class()
!!
  SUBROUTINE h5tget_member_class_f(type_id, member_no, class, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER, INTENT(IN)       :: member_no
    INTEGER, INTENT(OUT)     :: class
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_member_class_c(type_id, member_no, class) &
            BIND(C,NAME='h5tget_member_class_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER, INTENT(IN)       :: member_no
         INTEGER, INTENT(OUT)     :: class
       END FUNCTION h5tget_member_class_c
    END INTERFACE

    hdferr = h5tget_member_class_c(type_id, member_no, class)

  END SUBROUTINE h5tget_member_class_f

!>
!! \ingroup FH5T
!!
!! \brief Commits a transient datatype to a file, creating a new named datatype, but does not link it into the file structure.
!!
!! \param loc_id   A file or group identifier specifying the file in which the new named datatype is to be created.
!! \param dtype_id A datatype identifier.
!! \param hdferr   \fortran_error
!! \param tcpl_id  A datatype creation property list identifier (H5P_DEFAULT_F for the default property list.)
!! \param tapl_id  A datatype access property list identifier should always be passed as the value H5P_DEFAULT_F.
!!
!! See C API: @ref H5Tcommit_anon()
!!
  SUBROUTINE h5tcommit_anon_f(loc_id, dtype_id, hdferr, tcpl_id, tapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    INTEGER(HID_T), INTENT(IN) :: dtype_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: tcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: tapl_id
    INTEGER(HID_T) :: tcpl_id_default
    INTEGER(HID_T) :: tapl_id_default

    INTERFACE
       INTEGER FUNCTION h5tcommit_anon_c(loc_id, dtype_id, &
            tcpl_id_default, tapl_id_default) BIND(C,NAME='h5tcommit_anon_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(HID_T), INTENT(IN) :: dtype_id
         INTEGER(HID_T) :: tcpl_id_default
         INTEGER(HID_T) :: tapl_id_default
       END FUNCTION h5tcommit_anon_c
    END INTERFACE

    tcpl_id_default = H5P_DEFAULT_F
    tapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(tcpl_id)) tcpl_id_default = tcpl_id
    IF(PRESENT(tapl_id)) tapl_id_default = tapl_id

    hdferr = h5tcommit_anon_c(loc_id, dtype_id, &
         tcpl_id_default, tapl_id_default )

  END SUBROUTINE h5tcommit_anon_f

!>
!! \ingroup FH5T
!!
!! \brief Determines whether a datatype is a named type or a transient type.
!!
!! \param dtype_id  A datatype identifier.
!! \param committed .TRUE. if the datatype has been committed, and .FALSE. if the datatype has not been committed.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Tcommitted()
!!
  SUBROUTINE h5tcommitted_f(dtype_id, committed, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dtype_id
    LOGICAL, INTENT(OUT) :: committed
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tcommitted_c(dtype_id) BIND(C,NAME='h5tcommitted_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dtype_id
       END FUNCTION h5tcommitted_c
    END INTERFACE

    hdferr = h5tcommitted_c(dtype_id)

    IF(hdferr.GT.0)THEN
       committed = .TRUE.
       hdferr = 0
    ELSE IF(hdferr.EQ.0)THEN
       committed = .FALSE.
       hdferr = 0
    ELSE
       hdferr = -1
    ENDIF

  END SUBROUTINE h5tcommitted_f

!>
!! \ingroup FH5T
!!
!! \brief Decode A binary object description of data type and return a new object handle.
!!
!! \param buf    Buffer for the data space object to be decoded.
!! \param obj_id Object ID.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Tdecode()
!!
  SUBROUTINE h5tdecode_f(buf, obj_id, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: buf
    INTEGER(HID_T), INTENT(OUT) :: obj_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tdecode_c(buf, obj_id) BIND(C,NAME='h5tdecode_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: buf
         INTEGER(HID_T), INTENT(OUT) :: obj_id
       END FUNCTION h5tdecode_c
    END INTERFACE

    hdferr = h5tdecode_c(buf, obj_id)

  END SUBROUTINE h5tdecode_f

!>
!! \ingroup FH5T
!!
!! \brief Encode a data type object description into a binary buffer.
!!
!! \param obj_id Identifier of the object to be encoded.
!! \param buf    Buffer for the object to be encoded into.
!! \param nalloc If set to zero, returns the size of the buffer needed. Otherwise, it sets the size of \p buf allocated.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Tencode()
!!
  SUBROUTINE h5tencode_f(obj_id, buf, nalloc, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    CHARACTER(LEN=*), INTENT(OUT) :: buf
    INTEGER(SIZE_T), INTENT(INOUT) :: nalloc
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5tencode_c(buf, obj_id, nalloc) BIND(C,NAME='h5tencode_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: buf
         INTEGER(SIZE_T), INTENT(INOUT) :: nalloc
       END FUNCTION h5tencode_c
    END INTERFACE

    hdferr = h5tencode_c(buf, obj_id, nalloc)

  END SUBROUTINE h5tencode_f

!>
!! \ingroup FH5T
!!
!! \brief Returns a copy of a datatype creation property list.
!!
!! \param dtype_id Datatype identifier.
!! \param dtpl_id  Datatype property list identifier.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Tget_create_plist()
!!
  SUBROUTINE h5tget_create_plist_f(dtype_id, dtpl_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dtype_id
    INTEGER(HID_T), INTENT(OUT) :: dtpl_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_create_plist_c(dtype_id, dtpl_id) BIND(C,NAME='h5tget_create_plist_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dtype_id
         INTEGER(HID_T), INTENT(OUT) :: dtpl_id
       END FUNCTION h5tget_create_plist_c
    END INTERFACE

    hdferr = h5tget_create_plist_c(dtype_id, dtpl_id)
  END SUBROUTINE h5tget_create_plist_f

!>
!! \ingroup FH5T
!!
!! \brief Check whether the library&apos;s default conversion is hard conversion.
!!
!! \param src_id Identifier for the source datatype.
!! \param dst_id Identifier for the destination datatype.
!! \param flag   .TRUE. for compiler conversion, .FALSE. for library conversion.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Tcompiler_conv()
!!
  SUBROUTINE h5tcompiler_conv_f(src_id, dst_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: src_id
    INTEGER(HID_T), INTENT(IN) :: dst_id
    LOGICAL, INTENT(OUT) :: flag
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: c_flag

    INTERFACE
       INTEGER FUNCTION h5tcompiler_conv_c(src_id, dst_id, c_flag) BIND(C,NAME='h5tcompiler_conv_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: src_id
         INTEGER(HID_T), INTENT(IN) :: dst_id
         INTEGER :: c_flag
       END FUNCTION h5tcompiler_conv_c
    END INTERFACE

    hdferr = h5tcompiler_conv_c(src_id, dst_id, c_flag)

    flag = .FALSE.
    IF(c_flag .GT. 0) flag = .TRUE.

  END SUBROUTINE h5tcompiler_conv_f

!>
!! \ingroup FH5T
!!
!! \brief Returns the native datatype of a specified datatype.
!!
!! \param dtype_id        Datatype identifier for the dataset datatype.
!! \param direction       Direction of search:
!!                         H5T_DIR_DEFAULT,   default direction is inscendent,
!!                         H5T_DIR_ASCEND ,   in inscendent order,
!!                         H5T_DIR_DESCEND,   in descendent order.
!!                         * NOTE: In C it is defined as a structure: H5T_direction_t
!! \param native_dtype_id The native datatype identifier for the specified dataset datatype.
!! \param hdferr          \fortran_error
!!
!! See C API: @ref H5Tget_native_type()
!!
  SUBROUTINE h5tget_native_type_f(dtype_id, direction, native_dtype_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dtype_id
    INTEGER, INTENT(IN) :: direction
    INTEGER(HID_T), INTENT(OUT) :: native_dtype_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5tget_native_type_c(dtype_id, direction, native_dtype_id) BIND(C,NAME='h5tget_native_type_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dtype_id
         INTEGER, INTENT(IN) :: direction
         INTEGER(HID_T), INTENT(OUT) :: native_dtype_id
       END FUNCTION h5tget_native_type_c
    END INTERFACE

    hdferr = h5tget_native_type_c(dtype_id, direction, native_dtype_id)
  END SUBROUTINE h5tget_native_type_f

!>
!! \ingroup FH5T
!!
!! \brief Converts data from between specified datatypes.
!!
!! \param src_id     Identifier for the source datatype.
!! \param dst_id     Identifier for the destination datatype.
!! \param nelmts     Size of array buf.
!! \param buf        Array containing pre-conversion values.
!! \param hdferr     \fortran_error
!! \param background Background buffer.
!! \param plist_id   Dataset transfer property list identifier.
!!
!! See C API: @ref H5Tconvert()
!!
  SUBROUTINE h5tconvert_f(src_id, dst_id, nelmts, buf, hdferr, background, plist_id)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)            :: src_id
    INTEGER(HID_T) , INTENT(IN)            :: dst_id
    INTEGER(SIZE_T), INTENT(IN)            :: nelmts
    TYPE(C_PTR)    , INTENT(IN)            :: buf
    INTEGER        , INTENT(OUT)           :: hdferr
    TYPE(C_PTR)    , INTENT(IN), OPTIONAL  :: background
    INTEGER(HID_T) , INTENT(IN), OPTIONAL  :: plist_id
    INTEGER(HID_T) :: plist_id_default
    TYPE(C_PTR) :: background_default

    INTERFACE
       INTEGER FUNCTION h5tconvert_c(src_id, dst_id, nelmts, buf, background, plist_id) &
            BIND(C, NAME='h5tconvert_c')
         IMPORT :: c_ptr
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T) , INTENT(IN)           :: src_id
         INTEGER(HID_T) , INTENT(IN)           :: dst_id
         INTEGER(SIZE_T), INTENT(IN)           :: nelmts
         TYPE(C_PTR)                , VALUE    :: buf
         TYPE(C_PTR)                , VALUE    :: background
         INTEGER(HID_T) , INTENT(IN)           :: plist_id
       END FUNCTION h5tconvert_c
    END INTERFACE

    plist_id_default = H5P_DEFAULT_F
    IF(PRESENT(plist_id)) plist_id_default = plist_id

    background_default = C_NULL_PTR
    IF(PRESENT(background)) background_default = background

    hdferr = H5Tconvert_c(src_id, dst_id, nelmts, buf, background_default, plist_id_default)

  END SUBROUTINE h5tconvert_f

!>
!! \ingroup FH5T
!!
!! \brief Inserts a new enumeration datatype member.
!!
!! \attention \fortran_approved
!!
!! \param type_id Datatype identifier for the enumeration datatype.
!! \param name    Datatype identifier.
!! \param value   Pointer to the value of the new member.
!! \param hdferr  \fortran_error
!!
#ifdef H5_DOXYGEN
!! See C API: @ref H5Tenum_insert()
!!
  SUBROUTINE h5tenum_insert_f(&
#else
  SUBROUTINE h5tenum_insert_f03(&
#endif
      type_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN) :: type_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(C_PTR)     , INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen

    INTERFACE
       INTEGER FUNCTION h5tenum_insert_ptr_c(type_id, name, namelen, value) &
            BIND(C, NAME='h5tenum_insert_ptr_c')
         IMPORT :: C_CHAR, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         TYPE(C_PTR), VALUE :: value
       END FUNCTION h5tenum_insert_ptr_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5tenum_insert_ptr_c(type_id, name, namelen, value)
#ifdef H5_DOXYGEN
  END SUBROUTINE h5tenum_insert_f
#else
  END SUBROUTINE h5tenum_insert_f03
#endif

!>
!! \ingroup FH5T
!!
!! \brief Inserts a new enumeration datatype member.
!!
!! \attention  \fortran_obsolete
!!
!! \param type_id Datatype identifier for the enumeration datatype.
!! \param name    Datatype identifier.
!! \param value   Value of the new member.
!! \param hdferr  \fortran_error
!!
#ifdef H5_DOXYGEN
!! See C API: @ref H5Tenum_insert()
!!
  SUBROUTINE h5tenum_insert_f(type_id,  name, value, hdferr)
#else
  SUBROUTINE h5tenum_insert_f90(type_id,  name, value, hdferr)
#endif
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen
    INTERFACE
       INTEGER FUNCTION h5tenum_insert_c(type_id, name, namelen, value) BIND(C,NAME='h5tenum_insert_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: type_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER, INTENT(IN) :: value
         INTEGER :: namelen
       END FUNCTION h5tenum_insert_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5tenum_insert_c(type_id, name, namelen, value)
#ifdef H5_DOXYGEN
  END SUBROUTINE h5tenum_insert_f
#else
  END SUBROUTINE h5tenum_insert_f90
#endif

END MODULE H5T
