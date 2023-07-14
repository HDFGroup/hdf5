!> @defgroup FH5VL Fortran VOL (H5VL) Interface
!!
!! @see H5VL, C-API
!!
!! @see @ref H5VL_UG, User Guide
!!

!> @ingroup FH5VL
!!
!! @brief This module contains Fortran interfaces for H5VL (VOL) functions.
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
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5VL function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5VL

  USE H5GLOBAL
  USE H5fortkit

  IMPLICIT NONE

CONTAINS

! H5VLregister_connector

!>
!! \ingroup FH5VL
!!
!! \brief Registers a new VOL connector as a member of the virtual object layer class by name.
!!
!! \param name    \fortran_vol_name
!! \param vol_id  VOL connector identifier if successful; otherwise returns H5I_INVALID_HID_F
!! \param hdferr  \fortran_error
!! \param vipl_id VOL initialization property list identifier
!!
!! See C API: @ref H5VLregister_connector_by_name()
!!
  SUBROUTINE H5VLregister_connector_by_name_f(name, vol_id, hdferr, vipl_id)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(OUT) :: vol_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: vipl_id

    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    INTEGER(HID_T) :: vipl_id_default

    INTERFACE
       INTEGER(HID_T) FUNCTION H5VLregister_connector_by_name(name, vipl_id) &
            BIND(C,NAME='H5VLregister_connector_by_name')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(HID_T), VALUE :: vipl_id
       END FUNCTION H5VLregister_connector_by_name
    END INTERFACE

    vipl_id_default = H5P_DEFAULT_F
    IF(PRESENT(vipl_id)) vipl_id_default = vipl_id

    c_name = TRIM(name)//C_NULL_CHAR
    vol_id = H5VLregister_connector_by_name(c_name, vipl_id_default)

    hdferr = 0
    IF(vol_id.LT.0) hdferr = H5I_INVALID_HID_F

  END SUBROUTINE H5VLregister_connector_by_name_f
!>
!! \ingroup FH5VL
!!
!! \brief Registers a new VOL connector by value.
!!
!! \param connector_value Connector value
!! \param vol_id          VOL connector identifier if successful; otherwise returns H5I_INVALID_HID_F
!! \param hdferr          \fortran_error
!! \param vipl_id         VOL initialization property list identifier
!!
!! See C API: @ref H5VLregister_connector_by_value()
!!
  SUBROUTINE H5VLregister_connector_by_value_f(connector_value, vol_id, hdferr, vipl_id)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: connector_value
    INTEGER(HID_T), INTENT(OUT) :: vol_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: vipl_id
    INTEGER(HID_T) :: vipl_id_default

    INTERFACE
       INTEGER(HID_T) FUNCTION H5VLregister_connector_by_value(connector_value, vipl_id) &
            BIND(C,NAME='H5VLregister_connector_by_value')
         IMPORT :: HID_T
         IMPORT :: C_INT
         INTEGER(C_INT), VALUE :: connector_value
         INTEGER(HID_T), VALUE :: vipl_id
       END FUNCTION H5VLregister_connector_by_value
    END INTERFACE

    vipl_id_default = H5P_DEFAULT_F
    IF(PRESENT(vipl_id)) vipl_id_default = vipl_id

    vol_id = H5VLregister_connector_by_value(INT(connector_value,C_INT), vipl_id_default)

    hdferr = 0
    IF(vol_id.LT.0) hdferr = H5I_INVALID_HID_F

  END SUBROUTINE H5VLregister_connector_by_value_f

!>
!! \ingroup FH5VL
!!
!! \brief Determines whether a VOL class has been registered or not ccording to a specified connector name.
!!
!! \param name       \fortran_vol_name
!! \param registered State of VOL class registration
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5VLis_connector_registered_by_name()
!!
  SUBROUTINE H5VLis_connector_registered_by_name_f(name, registered,  hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    LOGICAL, INTENT(OUT) :: registered
    INTEGER, INTENT(OUT) :: hdferr
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    INTEGER(C_INT) :: registered_c

    INTERFACE
       INTEGER(C_INT) FUNCTION H5VLis_connector_registered_by_name(name) BIND(C,NAME='H5VLis_connector_registered_by_name')
         IMPORT :: C_CHAR
         IMPORT :: C_INT
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       END FUNCTION H5VLis_connector_registered_by_name
    END INTERFACE

    c_name = TRIM(name)//C_NULL_CHAR
    registered_c = H5VLis_connector_registered_by_name(c_name)

    hdferr = 0
    registered = .FALSE.
    IF(registered_c .GT. 0) registered = .TRUE.
    IF(registered_c .LT. 0) hdferr = INT(registered_c)

  END SUBROUTINE H5VLis_connector_registered_by_name_f

!>
!! \ingroup FH5VL
!!
!! \brief Determines whether a VOL class has been registered or not according to a specified connector value (ID).
!!
!! \param value      Connector value
!! \param registered State of VOL class registration
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5VLis_connector_registered_by_value()
!!
  SUBROUTINE H5VLis_connector_registered_by_value_f(value, registered,  hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: value
    LOGICAL, INTENT(OUT) :: registered
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(C_INT) :: registered_c

    INTERFACE
       INTEGER(C_INT) FUNCTION H5VLis_connector_registered_by_value(value) BIND(C,NAME='H5VLis_connector_registered_by_value')
         IMPORT :: C_INT
         INTEGER(C_INT), VALUE :: value
       END FUNCTION H5VLis_connector_registered_by_value
    END INTERFACE

    registered_c = H5VLis_connector_registered_by_value(INT(value,C_INT))

    hdferr = 0
    registered = .FALSE.
    IF(registered_c .GT. 0) registered = .TRUE.
    IF(registered_c .LT. 0) hdferr = INT(registered_c)

  END SUBROUTINE H5VLis_connector_registered_by_value_f

!>
!! \ingroup FH5VL
!!
!! \brief Retrieves the ID for a registered VOL connector.
!!
!! \param obj_id Object id
!! \param vol_id Connector id.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5VLget_connector_id()
!!
  SUBROUTINE H5VLget_connector_id_f(obj_id, vol_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    INTEGER(HID_T), INTENT(OUT) :: vol_id
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(HID_T) FUNCTION H5VLget_connector_id(obj_id) BIND(C,NAME='H5VLget_connector_id')
         IMPORT :: HID_T
         INTEGER(HID_T), VALUE :: obj_id
       END FUNCTION H5VLget_connector_id
    END INTERFACE

    vol_id = H5VLget_connector_id(obj_id)

    IF(vol_id.LT.0)THEN
       hdferr = -1
       vol_id = H5I_INVALID_HID_F
    ENDIF

  END SUBROUTINE H5VLget_connector_id_f

!>
!! \ingroup FH5VL
!!
!! \brief Retrieves the ID for a registered VOL connector.
!!
!! \param name   \fortran_vol_name
!! \param vol_id Connector id.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5VLget_connector_id_by_name()
!!
  SUBROUTINE H5VLget_connector_id_by_name_f(name, vol_id, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(OUT) :: vol_id
    INTEGER, INTENT(OUT) :: hdferr
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5VLget_connector_id_by_name(name) BIND(C,NAME='H5VLget_connector_id_by_name')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       END FUNCTION H5VLget_connector_id_by_name
    END INTERFACE

    c_name = TRIM(name)//C_NULL_CHAR
    vol_id = H5VLget_connector_id_by_name(c_name)

    hdferr = 0
    IF(vol_id.LT.0)THEN
       hdferr = -1
       vol_id = H5I_INVALID_HID_F
    ENDIF

  END SUBROUTINE H5VLget_connector_id_by_name_f

!>
!! \ingroup FH5VL
!!
!! \brief Retrieves the ID for a registered VOL connector.
!!
!! \param value  Connector value
!! \param vol_id Connector id
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5VLget_connector_id_by_value()
!!
  SUBROUTINE H5VLget_connector_id_by_value_f(value, vol_id, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: value
    INTEGER(HID_T), INTENT(OUT) :: vol_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(HID_T) FUNCTION H5VLget_connector_id_by_value(value) BIND(C,NAME='H5VLget_connector_id_by_value')
         IMPORT :: C_INT
         IMPORT :: HID_T
         INTEGER(C_INT), VALUE :: value
       END FUNCTION H5VLget_connector_id_by_value
    END INTERFACE

    vol_id = H5VLget_connector_id_by_value(INT(value,C_INT))

    hdferr = 0
    IF(vol_id.LT.0)THEN
       hdferr = -1
       vol_id = H5I_INVALID_HID_F
    ENDIF

  END SUBROUTINE H5VLget_connector_id_by_value_f
!>
!! \ingroup FH5VL
!!
!! \brief Retrieves a connector name for a VOL.
!!
!! \param obj_id   Object identifier or file identifier
!! \param name     \fortran_vol_name
!! \param hdferr   \fortran_error
!! \param name_len Maximum length of the name to retrieve
!!
!! See C API: @ref H5VLget_connector_name()
!!
  SUBROUTINE H5VLget_connector_name_f(obj_id, name, hdferr, name_len)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(SIZE_T), OPTIONAL     :: name_len
    CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(1:LEN(name)+1), TARGET :: c_name
    INTEGER(SIZE_T) :: l

    INTERFACE
       INTEGER(SIZE_T) FUNCTION H5VLget_connector_name(obj_id, name, size) BIND(C,NAME='H5VLget_connector_name')
         IMPORT :: HID_T, SIZE_T, C_PTR, C_CHAR
         IMPLICIT NONE
         INTEGER(HID_T) , VALUE :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(SIZE_T), VALUE :: size
       END FUNCTION H5VLget_connector_name
    END INTERFACE

    hdferr = 0
    IF(PRESENT(name_len))THEN
       c_name(1:1)(1:1) = C_NULL_CHAR
       name_len = INT(H5VLget_connector_name(obj_id, c_name, 1_SIZE_T), SIZE_T)
       IF(name_len.LT.0) hdferr = H5I_INVALID_HID_F
    ELSE
       l = INT(LEN(name)+1,SIZE_T)
       IF(INT(H5VLget_connector_name(obj_id, c_name, l), SIZE_T).LT.0)THEN
          hdferr = H5I_INVALID_HID_F
       ELSE
          CALL HD5c2fstring(name,c_name,LEN(name))
       ENDIF
    ENDIF

  END SUBROUTINE H5VLget_connector_name_f

!>
!! \ingroup FH5VL
!!
!! \brief Closes a VOL connector ID.
!!
!! \param vol_id A valid identifier of the connectory to unregister
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5VLclose()
!!
  SUBROUTINE H5VLclose_f(vol_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: vol_id
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION H5VLclose(vol_id) BIND(C, NAME='H5VLclose')
         IMPORT :: HID_T
         INTEGER(HID_T), VALUE :: vol_id
       END FUNCTION H5VLclose
    END INTERFACE

    hdferr = INT(H5VLclose(vol_id))

  END SUBROUTINE H5VLclose_f

!>
!! \ingroup FH5VL
!!
!! \brief Removes a VOL connector ID from the library.
!!
!! \param plugin_id A valid identifier of the connector to unregister
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5VLunregister_connector()
!!
  SUBROUTINE H5VLunregister_connector_f(plugin_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plugin_id
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION H5VLunregister_connector(plugin_id) BIND(C, NAME='H5VLunregister_connector')
         IMPORT :: HID_T
         INTEGER(HID_T), VALUE :: plugin_id
       END FUNCTION H5VLunregister_connector
    END INTERFACE

    hdferr = INT(H5VLunregister_connector(plugin_id))

  END SUBROUTINE H5VLunregister_connector_f

END MODULE H5VL
