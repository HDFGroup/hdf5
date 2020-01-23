!****h* ROBODoc/H5VL
!
! NAME
!  MODULE H5VL
!
! PURPOSE
!  This file contains Fortran interfaces for H5VL (VOL) functions.
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
!   distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
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
!*****

MODULE H5VL

  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_FUNPTR, C_CHAR, C_INT64_T, C_INT
  USE H5GLOBAL
  USE H5fortkit

  IMPLICIT NONE

CONTAINS

! H5VLregister_connector

!
!****s* H5VL/H5VLregister_connector_by_name_f
!
! NAME
!  H5VLregister_connector_by_name_f
!
! PURPOSE
!  Registers a new VOL connector as a member of the virtual object
!  layer class by name.
!
! INPUTS
!  name - Connector name
! OUTPUTS
!  vol_id - VOL id
!  hdferr - Returns 0 if successful and -1 if fails
! SOURCE

  SUBROUTINE H5VLregister_connector_by_name_f(name, vol_id, hdferr, vipl_id)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(OUT) :: vol_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: vipl_id
!*****
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    INTEGER(HID_T) :: vipl_id_default

    INTERFACE
       INTEGER(HID_T) FUNCTION H5VLregister_connector_by_name(name, vipl_id) &
            BIND(C,NAME='H5VLregister_connector_by_name')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(HID_T), INTENT(IN), VALUE :: vipl_id
       END FUNCTION H5VLregister_connector_by_name
    END INTERFACE
    
    vipl_id_default = H5P_DEFAULT_F
    IF(PRESENT(vipl_id)) vipl_id_default = vipl_id

    c_name = TRIM(name)//C_NULL_CHAR
    vol_id = H5VLregister_connector_by_name(c_name, vipl_id_default)

    hdferr = 0
    IF(vol_id.LT.0) hdferr = H5I_INVALID_HID_F

  END SUBROUTINE H5VLregister_connector_by_name_f

  SUBROUTINE H5VLregister_connector_by_value_f(connector_value, vol_id, hdferr, vipl_id)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: connector_value
    INTEGER(HID_T), INTENT(OUT) :: vol_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: vipl_id
!*****
    INTEGER(HID_T) :: vipl_id_default

    INTERFACE
       INTEGER(HID_T) FUNCTION H5VLregister_connector_by_value(connector_value, vipl_id) &
            BIND(C,NAME='H5VLregister_connector_by_value')
         IMPORT :: HID_T
         IMPORT :: C_INT
         INTEGER(C_INT), VALUE :: connector_value
         INTEGER(HID_T), INTENT(IN), VALUE :: vipl_id
       END FUNCTION H5VLregister_connector_by_value
    END INTERFACE

    vipl_id_default = H5P_DEFAULT_F
    IF(PRESENT(vipl_id)) vipl_id_default = vipl_id

    vol_id = H5VLregister_connector_by_value(INT(connector_value,C_INT), vipl_id_default)

    hdferr = 0
    IF(vol_id.LT.0) hdferr = H5I_INVALID_HID_F

  END SUBROUTINE H5VLregister_connector_by_value_f

!
!****s* H5VL/H5VLis_connector_registered_f
!
! NAME
!  H5VLis_connector_registered_f
!
! PURPOSE
!  Tests whether a VOL class has been registered or not.
!
! INPUTS
!  name - Connector name
! OUTPUTS
!  registered - state of VOL class registration 
!  hdferr - Returns 0 if successful and -1 if fails
! SOURCE

  SUBROUTINE H5VLis_connector_registered_f(name, registered,  hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    LOGICAL, INTENT(OUT) :: registered
    INTEGER, INTENT(OUT) :: hdferr
!*****
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    INTEGER(C_INT) :: registered_c

    INTERFACE
       INTEGER(C_INT) FUNCTION H5VLis_connector_registered(name) BIND(C,NAME='H5VLis_connector_registered')
         IMPORT :: C_CHAR
         IMPORT :: C_INT
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
       END FUNCTION H5VLis_connector_registered
    END INTERFACE
    
    c_name = TRIM(name)//C_NULL_CHAR
    registered_c = H5VLis_connector_registered(c_name)

    hdferr = 0
    registered = .FALSE.
    IF(registered_c .GT. 0) registered = .TRUE.
    IF(registered_c .LT. 0) hdferr = INT(registered_c)

  END SUBROUTINE H5VLis_connector_registered_f

!
!****s* H5VL/H5VLget_connector_id_f
!
! NAME
!  H5VLget_connector_id_f
!
! PURPOSE
!  Retrieves the ID for a registered VOL connector.
!
! INPUTS
!  obj_id - Object id
! OUTPUTS
!  vol_id - Connector id
!  hdferr - Returns 0 if successful and -1 if fails
! SOURCE

  SUBROUTINE H5VLget_connector_id_f(obj_id, vol_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    INTEGER(HID_T), INTENT(OUT) :: vol_id
    INTEGER, INTENT(OUT) :: hdferr
!*****

    INTERFACE
       INTEGER(HID_T) FUNCTION H5VLget_connector_id(obj_id) BIND(C,NAME='H5VLget_connector_id')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: obj_id
       END FUNCTION H5VLget_connector_id
    END INTERFACE

    vol_id = H5VLget_connector_id(obj_id)

    IF(vol_id.LT.0)THEN
       hdferr = -1
       vol_id = H5I_INVALID_HID_F
    ENDIF

  END SUBROUTINE H5VLget_connector_id_f

!
!****s* H5VL/H5VLget_connector_id_by_name_f
!
! NAME
!  H5VLget_connector_id_by_name_f
!
! PURPOSE
!  Retrieves the ID for a registered VOL connector.
!
! INPUTS
!  name - Connector name
! OUTPUTS
!  vol_id - Connector id
!  hdferr - Returns 0 if successful and -1 if fails
! SOURCE

  SUBROUTINE H5VLget_connector_id_by_name_f(name, vol_id, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(OUT) :: vol_id
    INTEGER, INTENT(OUT) :: hdferr
!*****
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5VLget_connector_id_by_name(name) BIND(C,NAME='H5VLget_connector_id_by_name')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
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

  SUBROUTINE H5VLget_connector_name_f(obj_id, name, hdferr, name_len)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(SIZE_T), OPTIONAL     :: name_len
!*****
    CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(1:LEN(name)+1), TARGET :: c_name
    INTEGER(SIZE_T) :: l

    INTERFACE
       INTEGER(SIZE_T) FUNCTION H5VLget_connector_name(obj_id, name, size) BIND(C,NAME='H5VLget_connector_name')
         IMPORT :: HID_T, SIZE_T, C_PTR, C_CHAR
         IMPLICIT NONE
         INTEGER(HID_T) , INTENT(IN), VALUE :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: name
        ! TYPE(C_PTR), value :: name
         INTEGER(SIZE_T), INTENT(IN), VALUE :: size
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

! 
!
!****s* H5VL/H5VLclose_f
!
! NAME
!  H5VLclose_f
!
! PURPOSE
!  Closes a VOL connector ID.
!
! INPUTS
!  vol_id - A valid identifier of the connectory to unregister.
!
! OUTPUTS
!  hdferr - Returns 0 if successful and -1 if fails
! SOURCE

  SUBROUTINE H5VLclose_f(vol_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: vol_id
    INTEGER, INTENT(OUT) :: hdferr
!*****

    INTERFACE
       INTEGER FUNCTION H5VLclose(vol_id) BIND(C, NAME='H5VLclose')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN), VALUE :: vol_id
       END FUNCTION H5VLclose
    END INTERFACE

    hdferr = INT(H5VLclose(vol_id))

  END SUBROUTINE H5VLclose_f

!
!****s* H5VL/H5VLunregister_connector_f
!
! NAME
!  H5VLunregister_connector_f
!
! PURPOSE
!  Removes a VOL connector ID from the library.
!
! INPUTS
!  plugin_id - A valid identifier of the connector to unregister.
!
! OUTPUTS
!  hdferr - Returns 0 if successful and -1 if fails
! SOURCE

  SUBROUTINE H5VLunregister_connector_f(plugin_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plugin_id
    INTEGER, INTENT(OUT) :: hdferr
!*****

    INTERFACE
       INTEGER FUNCTION H5VLunregister_connector(plugin_id) BIND(C, NAME='H5VLunregister_connector')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN), VALUE :: plugin_id
       END FUNCTION H5VLunregister_connector
    END INTERFACE

    hdferr = INT(H5VLunregister_connector(plugin_id))

  END SUBROUTINE H5VLunregister_connector_f

END MODULE H5VL
