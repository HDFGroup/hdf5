!****h* ROBODoc/H5F
!
! NAME
!  MODULE H5F
!
! FILE
!  H5Fff.f90
!
! PURPOSE
!  This file contains Fortran interfaces for H5F functions.
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
!                         *** IMPORTANT ***
!  If you add a new H5F function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5F
  USE H5GLOBAL

CONTAINS
!****s* H5F/h5fcreate_f
!
! NAME
!  h5fcreate_f
!
! PURPOSE
!  Creates HDF5 files.
!
! INPUTS
!  name 	 - name of the file to create
!  access_flags  - File access flags. Allowable values are:
!                   H5F_ACC_TRUNC_F
!                   H5F_ACC_EXCL_F
! OUTPUTS
!  file_id 	 - file identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! OPTIONAL PARAMETERS
!  creation_prp  - file creation property list identifier
!  access_prp 	 - file access property list identifier
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
! SOURCE
  SUBROUTINE h5fcreate_f(name, access_flags, file_id, hdferr, &
       creation_prp, access_prp)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the file
    INTEGER, INTENT(IN) :: access_flags    ! File access flags
    INTEGER(HID_T), INTENT(OUT) :: file_id ! File identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: creation_prp
                                           ! File creation propertly
                                           ! list identifier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp
                                           ! File access property list
                                           ! identifier
!*****
    INTEGER(HID_T) :: creation_prp_default
    INTEGER(HID_T) :: access_prp_default
    INTEGER :: namelen ! Length of the name character string

    INTERFACE
       INTEGER FUNCTION h5fcreate_c(name, namelen, access_flags, &
            creation_prp_default, access_prp_default, file_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FCREATE_C':: h5fcreate_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN) :: access_flags
         INTEGER(HID_T), INTENT(OUT) :: file_id
         INTEGER(HID_T), INTENT(IN) :: creation_prp_default
         INTEGER(HID_T), INTENT(IN) :: access_prp_default
         INTEGER :: namelen
       END FUNCTION h5fcreate_c
    END INTERFACE

    creation_prp_default = H5P_DEFAULT_F
    access_prp_default = H5P_DEFAULT_F

    IF (PRESENT(creation_prp)) creation_prp_default = creation_prp
    IF (PRESENT(access_prp))   access_prp_default   = access_prp
    namelen = LEN_TRIM(name)
    hdferr = h5fcreate_c(name, namelen, access_flags, &
         creation_prp_default, access_prp_default, file_id)

  END SUBROUTINE h5fcreate_f
!****s* H5F/h5fflush_f
!
! NAME
!  h5fflush_f
!
! PURPOSE
!  Flushes all buffers associated WITH a file to disk
!
! INPUTS
!  object_id 	 - identifier of object used to identify the file.
!  scope 	 - specifies the scope of the flushing action.
!                  Possible values are:
!                    H5F_SCOPE_GLOBAL_F
!                    H5F_SCOPE_LOCAL_F
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! OPTIONAL PARAMETERS
!  creation_prp  - file creation property list identifier
!  access_prp 	 - file access property list identifier
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
! SOURCE
  SUBROUTINE h5fflush_f(object_id, scope, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: object_id !identifier for any object
                                            !associate with a file,
                                            !including the file itself,
                                            !a dataset, a group, an
                                            !attribute, or a named
                                            !data type

    INTEGER, INTENT(IN) :: scope            !scope of the flushing
                                            !action, possible values
                                            !are: H5F_SCOPE_GLOBAL_F
                                            ! which flushes the entire
                                            !virtual file,
                                            !and H5F_SCOPE_LOCAL_F
                                            !which flushes only the
                                            !specified file.

    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5fflush_c(object_id, scope)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FFLUSH_C':: h5fflush_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: object_id
         INTEGER, INTENT(IN) :: scope
       END FUNCTION h5fflush_c
    END INTERFACE

    hdferr = h5fflush_c(object_id, scope)

  END SUBROUTINE h5fflush_f
!****s* H5F/h5fmount_f
!
! NAME
!  h5fmount_f
!
! PURPOSE
!  Mounts a file.
!
! INPUTS
!  loc_id 	 - the identifier for of file or group in
!                  which name is defined
!  name 	 - the name of the group onto which the file
!                  specified by child_id is to be mounted.
!  child_id 	 - the identifier of the file to be mounted.
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  access_prp 	 - the identifier of the property list to be used
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
! SOURCE
  SUBROUTINE h5fmount_f(loc_id, name, child_id, hdferr, access_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! Identifier for file or group
                                           ! in which dsetname is defined
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the group
    INTEGER(HID_T), INTENT(IN) :: child_id ! File identifier for the
                                           ! file to be mounted
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp
                                           ! File access property list
                                           ! identifier
!*****
    INTEGER(HID_T) :: access_prp_default
    INTEGER :: namelen ! Length of the name character string

    INTERFACE
       INTEGER FUNCTION h5fmount_c(loc_id, name, namelen, &
            child_id, access_prp_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FMOUNT_C':: h5fmount_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER(HID_T), INTENT(IN) :: child_id
         INTEGER(HID_T), INTENT(IN) :: access_prp_default
         INTEGER :: namelen
       END FUNCTION h5fmount_c
    END INTERFACE

    access_prp_default = H5P_DEFAULT_F
    IF (PRESENT(access_prp))   access_prp_default   = access_prp
    namelen = LEN_TRIM(name)
    hdferr = h5fmount_c(loc_id, name, namelen, child_id, access_prp_default)

  END SUBROUTINE h5fmount_f

!****s* H5F/h5funmount_f
!
! NAME
!  h5funmount_f
!
! PURPOSE
!  Unmounts a file.
!
! INPUTS
!  loc_id 	 - the identifier for of file or group in
!                  which name is defined
!  name 	 - the name of the mount point
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! SOURCE
  SUBROUTINE h5funmount_f(loc_id, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! Identifier for file or group
                                           ! at which the specified file
                                           ! is to be unmounted
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the mount point
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER :: namelen ! Length of the name character string

    INTERFACE
       INTEGER FUNCTION h5funmount_c(loc_id, name, namelen)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FUNMOUNT_C':: h5funmount_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER :: namelen
       END FUNCTION h5funmount_c
    END INTERFACE

    namelen = LEN_TRIM(name)
    hdferr = h5funmount_c(loc_id, name, namelen)

  END SUBROUTINE h5funmount_f
!****s* H5F/h5fopen_f
!
! NAME
!  h5fopen_f
!
! PURPOSE
!  Opens HDF5 file.
!
! INPUTS
!  name 	 - name of the file to acecss
!  access_flags  - File access flags. Allowable values are:
!                   H5F_ACC_RDWR_F
!                   H5F_ACC_RDONLY_F
! OUTPUTS
!  file_id 	 - file identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  access_prp 	 - file access property list identifier
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
! SOURCE
  SUBROUTINE h5fopen_f(name, access_flags, file_id, hdferr, access_prp)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the file
    INTEGER, INTENT(IN) :: access_flags    ! File access flags
    INTEGER(HID_T), INTENT(OUT) :: file_id ! File identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp
                                           ! File access property list
                                           ! identifier
!*****
    INTEGER(HID_T) :: access_prp_default
    INTEGER :: namelen ! Length of the name character string

    INTERFACE
       INTEGER FUNCTION h5fopen_c(name, namelen, access_flags, &
            access_prp_default, file_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FOPEN_C':: h5fopen_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER, INTENT(IN) :: access_flags
         INTEGER(HID_T), INTENT(IN) :: access_prp_default
         INTEGER(HID_T), INTENT(OUT) :: file_id
       END FUNCTION h5fopen_c
    END INTERFACE

    access_prp_default = H5P_DEFAULT_F
    IF (PRESENT(access_prp))   access_prp_default   = access_prp
    namelen = LEN_TRIM(name)
    hdferr = h5fopen_c(name, namelen, access_flags, &
                               access_prp_default, file_id)
  END SUBROUTINE h5fopen_f
!****s* H5F/h5freopen_f
!
! NAME
!  h5freopen_f
!
! PURPOSE
!  Reopens HDF5 file.
!
! INPUTS
!  file_id 	 - identifier of a file for which an
!                  additional identifier is required
! OUTPUTS
!  ret_file_id 	 - new file identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! SOURCE
  SUBROUTINE h5freopen_f(file_id, ret_file_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id      ! File identifier
    INTEGER(HID_T), INTENT(OUT) :: ret_file_id ! New File identifier
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5freopen_c(file_id, ret_file_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FREOPEN_C':: h5freopen_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HID_T), INTENT(OUT) :: ret_file_id
       END FUNCTION h5freopen_c
    END INTERFACE

    hdferr = h5freopen_c(file_id, ret_file_id)

  END SUBROUTINE h5freopen_f
!****s* H5F/h5fget_create_plist_f
!
! NAME
!  h5fget_create_plist_f
!
! PURPOSE
!  Returns a file creation property list identifier.
!
! INPUTS
!  file_id 	 - identifier of a file to creation property list of
! OUTPUTS
!  prop_id 	 - creation property list identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
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
! SOURCE
  SUBROUTINE h5fget_create_plist_f(file_id, prop_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id    ! File identifier
    INTEGER(HID_T), INTENT(OUT) :: prop_id   ! File creation property
                                             ! list identifier
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5fget_create_plist_c(file_id, prop_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_CREATE_PLIST_C':: h5fget_create_plist_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HID_T), INTENT(OUT) :: prop_id
       END FUNCTION h5fget_create_plist_c
    END INTERFACE

    hdferr = h5fget_create_plist_c(file_id, prop_id)

  END SUBROUTINE h5fget_create_plist_f
!****s* H5F/h5fget_access_plist_f
!
! NAME
!  h5fget_access_plist_f
!
! PURPOSE
!  Returns a file access property list identifier.
!
! INPUTS
!  file_id 	 - identifier of a file to creation property list of
! OUTPUTS
!  access_id 	 - access property list identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! SOURCE
  SUBROUTINE h5fget_access_plist_f(file_id, access_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id      ! File identifier
    INTEGER(HID_T), INTENT(OUT) :: access_id   ! File access property
                                               ! list identifier
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5fget_access_plist_c(file_id, access_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_CREATE_PLIST_C':: h5fget_access_plist_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HID_T), INTENT(OUT) :: access_id
       END FUNCTION h5fget_access_plist_c
    END INTERFACE

    hdferr = h5fget_access_plist_c(file_id, access_id)

  END SUBROUTINE h5fget_access_plist_f

!****s* H5F/h5fis_hdf5_f
!
! NAME
!  h5fis_hdf5_f
!
! PURPOSE
!  Determines whether a file is in the HDF5 format.
!
! INPUTS
!  name 	 - name of the file to check
! OUTPUTS
!  status 	 - indicates if file is and HDF5 file
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! SOURCE
  SUBROUTINE h5fis_hdf5_f(name, status, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the file
    LOGICAL, INTENT(OUT) :: status         ! Indicates if file
                                           ! is an HDF5 file
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER :: namelen ! Length of the name character string
    INTEGER :: flag    ! "TRUE/FALSE" flag from C routine
                       ! to define status value.

    INTERFACE
       INTEGER FUNCTION h5fis_hdf5_c(name, namelen, flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FIS_HDF5_C':: h5fis_hdf5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER :: flag
       END FUNCTION h5fis_hdf5_c
    END INTERFACE

    namelen = LEN_TRIM(name)
    hdferr = h5fis_hdf5_c(name, namelen, flag)
    status = .TRUE.
    IF (flag .EQ. 0) status = .FALSE.

  END SUBROUTINE h5fis_hdf5_f
!****s* H5F/h5fclose_f
!
! NAME
!  h5fclose_f
!
! PURPOSE
!  Closes HDF5 file.
!
! INPUTS
!  file_id 	 - file identifier
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! SOURCE
  SUBROUTINE h5fclose_f(file_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id ! File identifier
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5fclose_c(file_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FCLOSE_C':: h5fclose_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: file_id
       END FUNCTION h5fclose_c
    END INTERFACE

    hdferr = h5fclose_c(file_id)

  END SUBROUTINE h5fclose_f

!****s* H5F/h5fget_obj_count_f
!
! NAME
!  h5fget_obj_count_f
!
! PURPOSE
!  Gets number of the objects open within a file
!
! INPUTS
!  file_id 	 - file identifier
!  obj_type 	 - type of the object; possible values are:
!                    H5F_OBJ_FILE_F
!                    H5F_OBJ_DATASET_F
!                    H5F_OBJ_GROUP_F
!                    H5F_OBJ_DATATYPE_F
!                    H5F_OBJ_ALL_F
! OUTPUTS
!  obj_count 	 - number of open objects
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  September 30, 2002
!
! HISTORY
!  Changed the type of obj_count to INTEGER(SIZE_T)
!  September 25, 2008 EIP
!
! SOURCE
  SUBROUTINE h5fget_obj_count_f(file_id, obj_type, obj_count, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id ! File identifier
    INTEGER, INTENT(IN)  :: obj_type      ! Object type
    INTEGER(SIZE_T), INTENT(OUT) :: obj_count
                                          ! Number of open objects
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5fget_obj_count_c(file_id, obj_type, obj_count)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_OBJ_COUNT_C':: h5fget_obj_count_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER, INTENT(IN)  :: obj_type      ! Object type
         INTEGER(SIZE_T), INTENT(OUT) :: obj_count
         ! Number of open objects
       END FUNCTION h5fget_obj_count_c
    END INTERFACE

    hdferr = h5fget_obj_count_c(file_id, obj_type, obj_count)

  END SUBROUTINE h5fget_obj_count_f

!****s* H5F/h5fget_obj_ids_f
!
! NAME
!  h5fget_obj_ids_f
!
! PURPOSE
!  Get list of open objects identifiers within a file
!
! INPUTS
!  file_id 	 - file identifier
!  obj_type 	 - type of the object; possible values are:
!                    H5F_OBJ_FILE_F
!                    H5F_OBJ_DATASET_F
!                    H5F_OBJ_GROUP_F
!                    H5F_OBJ_DATATYPE_F
!                    H5F_OBJ_ALL_F
! OUTPUTS
!  obj_ids 	 - array of open object identifiers
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! OPTIONAL PARAMETERS
!  num_objs 	 - number of open objects
!
! AUTHOR
!  Elena Pourmal
!  September 30, 2002
!
! HISTORY
!  Added optional parameter num_objs for number of open objects
!  of the specified type and changed type of max_obj to
!  INTEGER(SIZE_T)
!  September 25, 2008 EIP
!
! SOURCE
  SUBROUTINE h5fget_obj_ids_f(file_id, obj_type, max_objs, obj_ids, hdferr, num_objs)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id      ! File identifier
    INTEGER, INTENT(IN)  :: obj_type           ! Object type
    INTEGER(SIZE_T), INTENT(IN)  :: max_objs   ! Maximum # of objects to retrieve
    INTEGER(HID_T), DIMENSION(*), INTENT(INOUT) :: obj_ids
                                               ! Array of open objects iidentifiers
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: num_objs ! number of open objects
!*****
    INTEGER(SIZE_T) :: c_num_objs ! Number of open objects of the specified type

    INTERFACE
       INTEGER FUNCTION h5fget_obj_ids_c(file_id, obj_type, max_objs, obj_ids, c_num_objs)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_OBJ_IDS_C':: h5fget_obj_ids_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER, INTENT(IN)  :: obj_type
         INTEGER(SIZE_T), INTENT(IN)  :: max_objs
         INTEGER(HID_T), DIMENSION(*), INTENT(INOUT) :: obj_ids
         INTEGER(SIZE_T), INTENT(OUT) :: c_num_objs
       END FUNCTION h5fget_obj_ids_c
    END INTERFACE

    hdferr = h5fget_obj_ids_c(file_id, obj_type, max_objs, obj_ids, c_num_objs)
    IF (PRESENT(num_objs)) num_objs= c_num_objs

  END SUBROUTINE h5fget_obj_ids_f
!****s* H5F/h5fget_freespace_f
!
! NAME
!  h5fget_freespace_f
!
! PURPOSE
!  Get amount of free space within a file
!
! INPUTS
!  file_id 	 - file identifier
! OUTPUTS
!  free_space 	 - amount of free space in file
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Quincey Koziol
!  October 7, 2003
!
! SOURCE
  SUBROUTINE h5fget_freespace_f(file_id, free_space, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id ! File identifier
    INTEGER(HSSIZE_T), INTENT(OUT) :: free_space
                                          ! amount of free space in file
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5fget_freespace_c(file_id, free_space)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_FREESPACE_C':: h5fget_freespace_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HSSIZE_T), INTENT(OUT) :: free_space
       END FUNCTION h5fget_freespace_c
    END INTERFACE

    hdferr = h5fget_freespace_c(file_id, free_space)

  END SUBROUTINE h5fget_freespace_f
!****s* H5F/h5fget_name_f
!
! NAME
!  h5fget_name_f
!
! PURPOSE
!  Gets the name of the file from the object identifier
!
! INPUTS
!  obj_id 	 - object identifier
! OUTPUTS
!  buf 	         - buffer to store the read name
!  size 	 - actual size of the name
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  July 6, 2004
!
! SOURCE
  SUBROUTINE h5fget_name_f(obj_id, buf, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id   ! Object identifier
    CHARACTER(LEN=*), INTENT(INOUT) :: buf
                                           ! Buffer to hold file name
    INTEGER(SIZE_T), INTENT(OUT) :: size   ! Size of the file name
    INTEGER, INTENT(OUT) :: hdferr         ! Error code: 0 on success,
                                           !   	 -1 if fail
!*****
    INTEGER(SIZE_T) :: buflen

    INTERFACE
       INTEGER FUNCTION h5fget_name_c(obj_id, size, buf, buflen)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_NAME_C'::h5fget_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER(SIZE_T), INTENT(OUT) :: size
         INTEGER(SIZE_T) :: buflen
         CHARACTER(LEN=*), INTENT(OUT) :: buf
       END FUNCTION h5fget_name_c
    END INTERFACE
    buflen = LEN_TRIM(buf)
    hdferr = h5fget_name_c(obj_id, size, buf, buflen)
  END SUBROUTINE h5fget_name_f
!****s* H5F/h5fget_filesize_f
!
! NAME
!  h5fget_filesize_f
!
! PURPOSE
!  Retrieves the file size of the HDF5 file.
!
! INPUTS
!  file_id 	 - file identifier
! OUTPUTS
!  size 	 - file size
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  July 7, 2004
!
! SOURCE
  SUBROUTINE h5fget_filesize_f(file_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id  ! file identifier
    INTEGER(HSIZE_T), INTENT(OUT) :: size  ! Size of the file
    INTEGER, INTENT(OUT) :: hdferr         ! Error code: 0 on success,
                                           !   	 -1 if fail
!*****
    INTERFACE
       INTEGER FUNCTION h5fget_filesize_c(file_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_FILESIZE_C'::h5fget_filesize_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HSIZE_T), INTENT(OUT) :: size
       END FUNCTION h5fget_filesize_c
    END INTERFACE
    hdferr = h5fget_filesize_c(file_id, size)
  END SUBROUTINE h5fget_filesize_f


END MODULE H5F
