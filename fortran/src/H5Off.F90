!****h* ROBODoc/H5O
!
! NAME
!  MODULE H5O
!
! FILE
!  fortran/src/H5Off.F90
!
! PURPOSE
!  This file contains Fortran interfaces for H5O functions.
!
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
!  If you add a new H5O function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5O

  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_FUNPTR, C_CHAR, C_INT64_T, C_LONG, C_INT, C_LOC
  USE H5GLOBAL
  IMPLICIT NONE

!****t* H5T (F03)/h5o_info_t
!
! Fortran2003 Derived Type:
!
  TYPE, BIND(C) :: space_t
     INTEGER(hsize_t) :: total ! Total space for storing object header in file
     INTEGER(hsize_t) :: meta  ! Space within header for object header metadata information
     INTEGER(hsize_t) :: mesg  ! Space within header for actual message information
     INTEGER(hsize_t) :: free  ! Free space within object header
  END TYPE space_t

  TYPE, BIND(C) :: mesg_t
     INTEGER(c_int64_t) :: present ! Flags to indicate presence of message type in header 
     INTEGER(c_int64_t) :: shared  ! Flags to indicate message type is shared in header
  END TYPE mesg_t
  
  TYPE, BIND(C) :: hdr_t
     INTEGER :: version ! Version number of header format in file
     INTEGER :: nmesgs  ! Number of object header messages
     INTEGER :: nchunks ! Number of object header chunks
     INTEGER :: flags   ! Object header status flags
     TYPE(space_t)  :: space   
     TYPE(mesg_t)   :: mesg
  END TYPE hdr_t

  TYPE, BIND(C) :: c_hdr_t
     INTEGER(C_INT) :: version ! Version number of header format in file
     INTEGER(C_INT) :: nmesgs  ! Number of object header messages
     INTEGER(C_INT) :: nchunks ! Number of object header chunks
     INTEGER(C_INT) :: flags   ! Object header status flags
     TYPE(space_t)  :: space   
     TYPE(mesg_t)   :: mesg
  END TYPE c_hdr_t

  ! Extra metadata storage for obj & attributes
  TYPE, BIND(C) :: H5_ih_info_t
     INTEGER(hsize_t) :: index_size ! btree and/or list
     INTEGER(hsize_t) :: heap_size
  END TYPE H5_ih_info_t

  TYPE, BIND(C) :: meta_size_t
     TYPE(H5_ih_info_t) :: obj  ! v1/v2 B-tree & local/fractal heap for groups, B-tree for chunked datasets
     TYPE(H5_ih_info_t) :: attr ! v2 B-tree & heap for attributes
  ENDTYPE meta_size_t
  
  TYPE, BIND(C) :: h5o_info_t
     INTEGER(C_LONG)  :: fileno     ! File number that object is located in
     INTEGER(haddr_t) :: addr       ! Object address in file  
     INTEGER(C_INT)   :: type       ! Basic object type (group, dataset, etc.)
     INTEGER          :: rc         ! Reference count of object

     INTEGER, DIMENSION(8) :: atime ! Access time         !    -- NOTE --
     INTEGER, DIMENSION(8) :: mtime ! Modification time   ! Returns an integer array    
     INTEGER, DIMENSION(8) :: ctime ! Change time         ! as specified in the Fortran 
     INTEGER, DIMENSION(8) :: btime ! Birth time          ! intrinsic DATE_AND_TIME(VALUES)

     INTEGER(hsize_t) :: num_attrs  ! # of attributes attached to object

     TYPE(hdr_t) :: hdr

     TYPE(meta_size_t) :: meta_size
  END TYPE h5o_info_t

! C interoperable structure for h5o_info_t. The Fortran derived type returns the time
! values as an integer array as specified in the Fortran intrinsic DATE_AND_TIME(VALUES).
! Whereas, this derived type does not.

  TYPE, BIND(C) :: c_h5o_info_t
     INTEGER(C_LONG)  :: fileno     ! File number that object is located in
     INTEGER(haddr_t) :: addr       ! Object address in file  
     INTEGER(C_INT)   :: type       ! Basic object type (group, dataset, etc.)
     INTEGER(C_INT)   :: rc         ! Reference count of object

     INTEGER(KIND=TIME_T) :: atime ! Access time
     INTEGER(KIND=TIME_T) :: mtime ! modify time
     INTEGER(KIND=TIME_T) :: ctime ! create time
     INTEGER(KIND=TIME_T) :: btime ! Access time

     INTEGER(hsize_t) :: num_attrs  ! # of attributes attached to object

     TYPE(c_hdr_t) :: hdr

     TYPE(meta_size_t) :: meta_size
  END TYPE c_h5o_info_t

!*****

CONTAINS

!****s* H5O/h5olink_f
!
! NAME
!  h5olink_f
!
! PURPOSE
!  Creates a hard link to an object in an HDF5 file.
!
! Inputs:
!  object_id 	 - Object to be linked.
!  new_loc_id 	 - File or group identifier specifying location at which object is to be linked.
!  new_link_name - Name of link to be created, relative to new_loc_id.
!
! Outputs:
!  hdferr        - Returns 0 if successful and -1 if fails.
!
! Optional parameters:
!  lcpl_id 	 - Link creation property list identifier.
!  lapl_id 	 - Link access property list identifier.
!
! AUTHOR
!  M. Scot Breitenfeld
!  April 21, 2008
!
! Fortran90 Interface:
  SUBROUTINE h5olink_f(object_id, new_loc_id, new_link_name, hdferr, lcpl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: object_id
    INTEGER(HID_T)  , INTENT(IN)  :: new_loc_id
    CHARACTER(LEN=*), INTENT(IN)  :: new_link_name
    INTEGER         , INTENT(OUT) :: hdferr
    INTEGER(HID_T)  , INTENT(IN), OPTIONAL :: lcpl_id
    INTEGER(HID_T)  , INTENT(IN), OPTIONAL :: lapl_id
!*****
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(HID_T) :: lcpl_id_default

    INTEGER(SIZE_T) :: new_link_namelen

    INTERFACE
       INTEGER FUNCTION h5olink_c(object_id, new_loc_id, new_link_name, new_link_namelen, &
            lcpl_id_default, lapl_id_default) BIND(C,NAME='h5olink_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: object_id
         INTEGER(HID_T), INTENT(IN) :: new_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: new_link_name
         INTEGER(SIZE_T) :: new_link_namelen
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(HID_T) :: lcpl_id_default
       END FUNCTION h5olink_c
    END INTERFACE

    new_link_namelen = LEN(new_link_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id

    hdferr = h5olink_c(object_id, new_loc_id, new_link_name, new_link_namelen, &
         lcpl_id_default, lapl_id_default)

  END SUBROUTINE h5olink_f

!****s* H5O/h5oopen_f
!
! NAME
!  h5oopen_f
!
! PURPOSE
!  Opens an object in an HDF5 file by location identifier and path name.
!
! Inputs:
!  loc_id  - File or group identifier.
!  name    - Path to the object, relative to loc_id.
!
! Outputs:
!  obj_id  - Object identifier for the opened object.
!  hdferr  - Returns 0 if successful and -1 if fails.
!
! Optional parameters:
!  lapl_id - Access property list identifier for the link pointing to the object.
!
! AUTHOR
!  M. Scot Breitenfeld
!  April 18, 2008
!
! Fortran90 Interface:
  SUBROUTINE h5oopen_f(loc_id, name, obj_id, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: loc_id
    CHARACTER(LEN=*), INTENT(IN)  :: name
    INTEGER(HID_T)  , INTENT(OUT) :: obj_id
    INTEGER         , INTENT(OUT) :: hdferr
    INTEGER(HID_T)  , INTENT(IN), OPTIONAL :: lapl_id
!*****
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T) :: namelen

    INTERFACE
       INTEGER FUNCTION h5oopen_c(loc_id, name, namelen, lapl_id_default, obj_id) BIND(C,NAME='h5oopen_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: namelen
         INTEGER(HID_T), INTENT(OUT) :: obj_id
       END FUNCTION h5oopen_c
    END INTERFACE

    namelen = LEN(name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5oopen_c(loc_id, name, namelen, lapl_id_default, obj_id)

  END SUBROUTINE h5oopen_f
!
!****s* H5O/h5oclose_f
!
! NAME
!  h5oclose_f
!
! PURPOSE
!  Closes an object in an HDF5 file.
!
! Inputs:
!  object_id - Object identifier.
!
! Outputs:
!  hdferr    - Returns 0 if successful and -1 if fails.
!
! AUTHOR
!  M. Scot Breitenfeld
!  December 17, 2008
!
! Fortran90 Interface:
  SUBROUTINE h5oclose_f(object_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)   :: object_id
    INTEGER       , INTENT(OUT) :: hdferr
!*****
    INTERFACE
       INTEGER FUNCTION h5oclose_c(object_id) BIND(C,NAME='h5oclose_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: object_id
       END FUNCTION h5oclose_c
    END INTERFACE

    hdferr = h5oclose_c(object_id)
  END SUBROUTINE h5oclose_f

!
!****s* H5O/h5open_by_addr_f
! NAME		
!  h5oopen_by_addr_f 
!
! PURPOSE
!  Opens an object using its address within an HDF5 file. 
!
! Inputs:  
!  loc_id - File or group identifier.
!  addr   - Object’s address in the file.
!
! Outputs:
!  obj_id - Object identifier for the opened object.
!  hdferr - Returns 0 if successful and -1 if fails.
!
! AUTHOR	
!  M. Scot Breitenfeld
!  September 14, 2009
! 
! Fortran90 Interface:
  SUBROUTINE h5oopen_by_addr_f(loc_id, addr, obj_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: loc_id
    INTEGER(HADDR_T), INTENT(IN)  :: addr
    INTEGER(HID_T)  , INTENT(OUT) :: obj_id
    INTEGER         , INTENT(OUT) :: hdferr
!*****
    INTERFACE
       INTEGER FUNCTION h5oopen_by_addr_c(loc_id, addr, obj_id) BIND(C,NAME='h5oopen_by_addr_c')
         IMPORT :: HID_T, HADDR_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(HADDR_T), INTENT(IN) :: addr
         INTEGER(HID_T), INTENT(OUT) :: obj_id
       END FUNCTION h5oopen_by_addr_c
    END INTERFACE

    hdferr = h5oopen_by_addr_c(loc_id, addr, obj_id)

  END SUBROUTINE h5oopen_by_addr_f
!
!****s* H5O/h5ocopy_f
! NAME		
!  h5ocopy_f 
!
! PURPOSE
!  Copies an object in an HDF5 file.
!
! Inputs:  
!  src_loc_id - Object identifier indicating the location of the source object to be copied.
!  src_name   - Name of the source object to be copied.
!  dst_loc_id - Location identifier specifying the destination.
!  dst_name   - Name to be assigned to the new copy.
!
! Optional parameters:
!  ocpypl_id  - Object copy property list.
!  lcpl_id    - Link creation property list for the new hard link.
!
! Outputs: 
!  hdferr     - Returns 0 if successful and -1 if fails.
!
! AUTHOR	
!  M. Scot Breitenfeld
!  March 14, 2012
! 
! Fortran90 Interface:
  SUBROUTINE h5ocopy_f(src_loc_id, src_name, dst_loc_id, dst_name, hdferr, ocpypl_id, lcpl_id)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: src_loc_id
    CHARACTER(LEN=*), INTENT(IN)  :: src_name
    INTEGER(HID_T)  , INTENT(IN)  :: dst_loc_id
    CHARACTER(LEN=*), INTENT(IN)  :: dst_name
    INTEGER         , INTENT(OUT) :: hdferr
    INTEGER(HID_T)  , INTENT(IN), OPTIONAL :: ocpypl_id
    INTEGER(HID_T)  , INTENT(IN), OPTIONAL :: lcpl_id
!*****

    INTEGER(SIZE_T) :: src_name_len, dst_name_len
    INTEGER(HID_T)  :: ocpypl_id_default, lcpl_id_default

    INTERFACE
       INTEGER FUNCTION h5ocopy_c(src_loc_id, src_name, src_name_len, &
            dst_loc_id, dst_name, dst_name_len, ocpypl_id_default, lcpl_id_default) &
            BIND(C,NAME='h5ocopy_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN) :: src_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: src_name
         INTEGER(HID_T)  , INTENT(IN) :: dst_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: dst_name
         INTEGER(HID_T)  , INTENT(IN) :: ocpypl_id_default
         INTEGER(HID_T)  , INTENT(IN) :: lcpl_id_default
         INTEGER(SIZE_T)              :: src_name_len, dst_name_len

       END FUNCTION h5ocopy_c
    END INTERFACE

    src_name_len = LEN(src_name)
    dst_name_len = LEN(dst_name)

    ocpypl_id_default = H5P_DEFAULT_F
    IF(PRESENT(ocpypl_id)) ocpypl_id_default = ocpypl_id
    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id

    hdferr = h5ocopy_c(src_loc_id, src_name, src_name_len, &
         dst_loc_id, dst_name, dst_name_len, ocpypl_id_default, lcpl_id_default)

  END SUBROUTINE h5ocopy_f

!****s* H5O/h5odecr_refcount_f
! NAME		
!  h5odecr_refcount_f
!
! PURPOSE
!  Decrements an object reference count. 
!
! Inputs:  
!  object_id - Object identifier.
!
! Outputs: 
!  hdferr    - Returns 0 if successful and -1 if fails.
!
! AUTHOR	
!  M. Scot Breitenfeld
!  May 11, 2012
! 
! Fortran90 Interface:
  SUBROUTINE h5odecr_refcount_f(object_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: object_id
    INTEGER       , INTENT(OUT) :: hdferr
!*****

    INTERFACE
       INTEGER FUNCTION h5odecr_refcount_c(object_id) BIND(C,NAME='h5odecr_refcount_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN) :: object_id
       END FUNCTION h5odecr_refcount_c
    END INTERFACE

    hdferr = h5odecr_refcount_c(object_id)  

  END SUBROUTINE h5odecr_refcount_f

!****s* H5O/h5oexists_by_name_f
! NAME		
!  h5oexists_by_name_f
!
! PURPOSE
!  Determines whether a link resolves to an actual object.
!
! Inputs:
!  loc_id   - Identifier of the file or group to query. 
!  name     - The name of the link to check. 
!    
!
! Optional parameters:
!  lapl_id  - Link access property list identifier.
!
! Outputs: 
!  link_exists - Existing link resolves to an object.
!  hdferr      - Returns 0 if successful and -1 if fails.
!
! AUTHOR	
!  M. Scot Breitenfeld
!  May 11, 2012
! 
! Fortran90 Interface:
  SUBROUTINE h5oexists_by_name_f(loc_id, name, link_exists, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: loc_id
    CHARACTER(LEN=*), INTENT(IN)  :: name
    LOGICAL         , INTENT(OUT) :: link_exists
    INTEGER         , INTENT(OUT) :: hdferr
    INTEGER(HID_T)  , INTENT(IN), OPTIONAL :: lapl_id
!*****

    INTEGER(size_t) :: namelen
    INTEGER :: status
    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION h5oexists_by_name_c(loc_id, name, namelen, lapl_id) &
            BIND(C,NAME='h5oexists_by_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(SIZE_T) , INTENT(IN) :: namelen
         INTEGER(HID_T)  , INTENT(IN) :: lapl_id

       END FUNCTION h5oexists_by_name_c
    END INTERFACE

    namelen = LEN(name)
    
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    status = h5oexists_by_name_c(loc_id, name, namelen, lapl_id_default)

    link_exists = .FALSE.
    IF(status.EQ.1)THEN
       link_exists = .TRUE.
    ENDIF

    hdferr = 0
    IF(status.LT.0)THEN
       hdferr = -1
    ENDIF

  END SUBROUTINE h5oexists_by_name_f

!****s* H5O/h5oget_comment_f
! NAME		
!  h5oget_comment_f
!
! PURPOSE
!  Retrieves comment for specified object. 
!
! Inputs:
!  obj_id - Identifier for the target object.
!
! Optional parameters:
!  bufsize - Size of the comment buffer.
!
! Outputs: 
!  comment - The comment.
!  hdferr  - Returns 0 if successful and -1 if fails.
!
! AUTHOR	
!  M. Scot Breitenfeld
!  May 11, 2012
! 
! Fortran90 Interface:
  SUBROUTINE h5oget_comment_f(obj_id, comment, hdferr, bufsize)
    IMPLICIT NONE
    INTEGER(HID_T)   , INTENT(IN)            :: obj_id
    CHARACTER(LEN=*) , INTENT(OUT)           :: comment
    INTEGER          , INTENT(OUT)           :: hdferr
    INTEGER(HSSIZE_T), INTENT(OUT), OPTIONAL :: bufsize 
!*****

    INTEGER(SIZE_T)   :: commentsize_default
    INTEGER(HSSIZE_T) :: bufsize_default

    INTERFACE
       INTEGER FUNCTION h5oget_comment_c(obj_id, comment, commentsize_default, bufsize) &
            BIND(C,NAME='h5oget_comment_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN)  :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: comment
         INTEGER(SIZE_T) , INTENT(IN)  :: commentsize_default
         INTEGER(HSSIZE_T) , INTENT(OUT) :: bufsize
       END FUNCTION h5oget_comment_c
    END INTERFACE

    commentsize_default = LEN(comment)

    hdferr = h5oget_comment_c(obj_id, comment, commentsize_default, bufsize_default)
    
    IF(PRESENT(bufsize)) bufsize = bufsize_default

  END SUBROUTINE h5oget_comment_f

!****s* H5O/h5oget_comment_by_name_f
! NAME		
!  h5oget_comment_by_name_f
!
! PURPOSE
!  Retrieves comment for specified object.
!
! Inputs:
!  loc_id   - Identifier of a file, group, dataset, or named datatype.
!  name     - Name of the object whose comment is to be retrieved, 
!             specified as a path relative to loc_id. 
!
! Optional parameters:
!  bufsize  - Size of the comment buffer.
!
! Outputs: 
!  comment  - The comment.
!  hdferr   - Returns 0 if successful and -1 if fails.
!
! AUTHOR	
!  M. Scot Breitenfeld
!  July 6, 2012
! 
! Fortran90 Interface:
  SUBROUTINE h5oget_comment_by_name_f(loc_id, name, comment, hdferr, bufsize, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)            :: loc_id
    CHARACTER(LEN=*), INTENT(IN)            :: name
    CHARACTER(LEN=*), INTENT(OUT)           :: comment
    INTEGER         , INTENT(OUT)           :: hdferr
    INTEGER(SIZE_T) , INTENT(OUT), OPTIONAL :: bufsize 
    INTEGER(HID_T)  , INTENT(IN) , OPTIONAL :: lapl_id 
!*****

    INTEGER(SIZE_T) :: commentsize_default
    INTEGER(SIZE_T) :: name_size
    INTEGER(SIZE_T) :: bufsize_default
    INTEGER(HID_T)  :: lapl_id_default
    INTERFACE
       INTEGER FUNCTION h5oget_comment_by_name_c(loc_id, name, name_size, &
            comment, commentsize_default, bufsize_default, lapl_id) BIND(C,NAME='h5oget_comment_by_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN)  :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)  :: name
         INTEGER(SIZE_T) , INTENT(IN)  :: name_size
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: comment
         INTEGER(SIZE_T) , INTENT(IN)  :: commentsize_default
         INTEGER(SIZE_T) , INTENT(OUT) :: bufsize_default
         INTEGER(HID_T)  , INTENT(IN)  :: lapl_id
       END FUNCTION h5oget_comment_by_name_c
    END INTERFACE

    commentsize_default = LEN(comment)
    name_size = LEN(name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5oget_comment_by_name_c(loc_id, name, name_size, &
         comment, commentsize_default, bufsize_default, lapl_id_default)
    
    IF(PRESENT(bufsize)) bufsize = bufsize_default

  END SUBROUTINE h5oget_comment_by_name_f

!****s* H5O/h5oincr_refcount_f
! NAME		
!  h5oincr_refcount_f
!
! PURPOSE
!  Increments an object reference count.
!
! Inputs:  
!  obj_id  - Object identifier.
!
! Outputs: 
!  hdferr  - Returns 0 if successful and -1 if fails.
!
! AUTHOR	
!  M. Scot Breitenfeld
!  May 15, 2012
! 
! Fortran90 Interface:
  SUBROUTINE h5oincr_refcount_f(obj_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: obj_id
    INTEGER       , INTENT(OUT) :: hdferr
!*****

    INTERFACE
       INTEGER FUNCTION h5oincr_refcount_c(obj_id) BIND(C,NAME='h5oincr_refcount_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN) :: obj_id
       END FUNCTION h5oincr_refcount_c
    END INTERFACE

    hdferr = h5oincr_refcount_c(obj_id) 

  END SUBROUTINE h5oincr_refcount_f

!****s* H5O/h5oopen_by_idx_f
!
! NAME
!  h5oopen_by_idx_f
!
! PURPOSE
!  Open the nth object in a group. 
!
! Inputs:
!  loc_id      - A file or group identifier.
!  group_name  - Name of group, relative to loc_id, in which object is located.
!  index_type  - Type of index by which objects are ordered.
!  order       - Order of iteration within index, NOTE: zero-based.
!  n           - Object to open.
!
! Outputs:
!  obj_id      - An object identifier for the opened object.
!  hdferr      - Returns 0 if successful and -1 if fails.
!
! Optional parameters:
!  lapl_id     - Link access property list.
!
! AUTHOR
!  M. Scot Breitenfeld
!  May 17, 2012
!
! Fortran90 Interface:
  SUBROUTINE h5oopen_by_idx_f(loc_id, group_name, index_type, order, n, obj_id, &
       hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)            :: loc_id
    CHARACTER(LEN=*), INTENT(IN)            :: group_name
    INTEGER         , INTENT(IN)            :: index_type
    INTEGER         , INTENT(IN)            :: order
    INTEGER(HSIZE_T), INTENT(IN)            :: n
    INTEGER(HID_T)  , INTENT(OUT)           :: obj_id
    INTEGER         , INTENT(OUT)           :: hdferr
    INTEGER(HID_T)  , INTENT(IN) , OPTIONAL :: lapl_id
!*****
    INTEGER(SIZE_T) :: group_namelen
    INTEGER(HID_T)  :: lapl_id_default
    
    INTERFACE
       INTEGER FUNCTION h5oopen_by_idx_c(loc_id, group_name, group_namelen, index_type, order, n, obj_id, lapl_id_default) &
            BIND(C,NAME='h5oopen_by_idx_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN)  :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)  :: group_name
         INTEGER(SIZE_T) , INTENT(IN)  :: group_namelen
         INTEGER         , INTENT(IN)  :: index_type
         INTEGER         , INTENT(IN)  :: order
         INTEGER(HSIZE_T), INTENT(IN)  :: n
         INTEGER(HID_T)  , INTENT(OUT) :: obj_id
         INTEGER(HID_T)  , INTENT(IN)  :: lapl_id_default

       END FUNCTION h5oopen_by_idx_c
    END INTERFACE

    group_namelen = LEN(group_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5oopen_by_idx_c(loc_id, group_name, group_namelen, index_type, order, n, obj_id, lapl_id_default)

  END SUBROUTINE H5Oopen_by_idx_f

!****s* H5O/h5oset_comment_f
! NAME		
!  h5oset_comment_f
!
! PURPOSE
!  Sets comment for specified object.
!
! Inputs:  
!  obj_id    - Identifier of the target object.
!  comment   - The new comment.
!
! Outputs: 
!  hdferr    - Returns 0 if successful and -1 if fails.
!
! AUTHOR	
!  M. Scot Breitenfeld
!  May 15, 2012
! 
! Fortran90 Interface:
  SUBROUTINE h5oset_comment_f(obj_id, comment, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: obj_id
    CHARACTER(LEN=*), INTENT(IN)  :: comment
    INTEGER         , INTENT(OUT) :: hdferr
!*****
    INTEGER(SIZE_T) :: commentlen

    INTERFACE
       INTEGER FUNCTION h5oset_comment_c(obj_id, comment, commentlen) BIND(C,NAME='h5oset_comment_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN) :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: comment
         INTEGER(SIZE_T) , INTENT(IN) :: commentlen

       END FUNCTION h5oset_comment_c
    END INTERFACE

    commentlen = LEN(comment)
    
    hdferr = h5oset_comment_c(obj_id, comment, commentlen)

  END SUBROUTINE h5oset_comment_f

!****s* H5O/h5oset_comment_by_name_f
! NAME		
!  h5oset_comment_by_name_f
!
! PURPOSE
!  Sets comment for specified object. 
!
! Inputs:  
!  loc_id   - Identifier of a file, group, dataset, or named datatype.
!  name     - Name of the object whose comment is to be set or reset, 
!              specified as a path relative to loc_id. 
!  comment  - The new comment.
!
! Outputs: 
!  hdferr   - Returns 0 if successful and -1 if fails.
!
! Optional parameters:
!  lapl_id  - Link access property list identifier.
!
! AUTHOR	
!  M. Scot Breitenfeld
!  May 15, 2012
! 
! Fortran90 Interface:
  SUBROUTINE h5oset_comment_by_name_f(loc_id, name, comment, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: loc_id
    CHARACTER(LEN=*), INTENT(IN)  :: name
    CHARACTER(LEN=*), INTENT(IN)  :: comment
    INTEGER         , INTENT(OUT) :: hdferr
    INTEGER(HID_T)  , INTENT(IN), OPTIONAL :: lapl_id
!*****
    INTEGER(SIZE_T) :: commentlen
    INTEGER(SIZE_T) :: namelen
    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION h5oset_comment_by_name_c(loc_id, name, namelen, comment, commentlen, lapl_id) &
            BIND(C,NAME='h5oset_comment_by_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: comment
         INTEGER(SIZE_T) , INTENT(IN) :: commentlen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(SIZE_T) , INTENT(IN) :: namelen
         INTEGER(HID_T)  , INTENT(IN) :: lapl_id
       END FUNCTION h5oset_comment_by_name_c
    END INTERFACE

    commentlen = LEN(comment)
    namelen = LEN(name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
    
    hdferr = h5oset_comment_by_name_c(loc_id, name, namelen, comment, commentlen, lapl_id_default)

  END SUBROUTINE h5oset_comment_by_name_f

!****s* H5O (F03)/h5ovisit_f_F03
!
! NAME
!  h5ovisit_f
!
! PURPOSE
!  Recursively visits all objects starting from a specified object.
!
! Inputs:
!  object_id  - Identifier of the object at which the recursive iteration begins.
!  index_type - Type of index; valid values include:
!                H5_INDEX_NAME_F
!                H5_INDEX_CRT_ORDER_F
!  order      - Order in which index is traversed; valid values include:
!                H5_ITER_DEC_F
!                H5_ITER_INC_F
!                H5_ITER_NATIVE_F
!  op 	      - Callback function passing data regarding the group to the calling application
!  op_data    - User-defined pointer to data required by the application for its processing of the group
!
! Outputs:
!  return_value - returns the return value of the first operator that returns a positive value, or 
!                 zero if all members were processed with no operator returning non-zero.
!  hdferr       - Returns 0 if successful and -1 if fails
!
! Optional parameters:
!  fields      - Flags specifying the fields to include in object_info.
!
! AUTHOR
!  M. Scot Breitenfeld
!  November 19, 2008
!
! Fortran2003 Interface:
  SUBROUTINE h5ovisit_f(object_id, index_type, order, op, op_data, return_value, hdferr, fields)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: object_id
    INTEGER, INTENT(IN) :: index_type 
    INTEGER, INTENT(IN) :: order

    TYPE(C_FUNPTR):: op
    TYPE(C_PTR)   :: op_data
    INTEGER, INTENT(OUT) :: return_value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER, INTENT(IN), OPTIONAL  :: fields 
!*****
    INTEGER :: fields_c

    INTERFACE
       INTEGER FUNCTION h5ovisit_c(object_id, index_type, order, op, op_data, fields) &
            BIND(C, NAME='h5ovisit_c')
         IMPORT :: C_FUNPTR, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: object_id
         INTEGER, INTENT(IN) :: index_type
         INTEGER, INTENT(IN) :: order
         TYPE(C_FUNPTR), VALUE :: op
         TYPE(C_PTR), VALUE :: op_data
         INTEGER, INTENT(IN) :: fields
       END FUNCTION h5ovisit_c
    END INTERFACE

    fields_c = H5O_INFO_ALL_F
    IF(PRESENT(fields)) fields_c = fields

    return_value = h5ovisit_c(object_id, index_type, order, op, op_data, fields_c)

    IF(return_value.GE.0)THEN
       hdferr = 0
    ELSE
       hdferr = -1
    END IF

  END SUBROUTINE h5ovisit_f

!****s* H5O (F03)/h5oget_info_by_name_f_F03
!
! NAME
!  h5oget_info_by_name_f
!
! PURPOSE
!  Retrieves the metadata for an object, identifying the object by location and relative name.
!
! Inputs:
!  loc_id      - File or group identifier specifying location of group 
!                in which object is located.
!  name        - Name of group, relative to loc_id.
!
! Outputs:  
!  object_info - Buffer in which to return object information.
!  hdferr      - Returns 0 if successful and -1 if fails.
!
! Optional parameters:
!  lapl_id     - Link access property list.
!  fields      - Flags specifying the fields to include in object_info.
!
! AUTHOR
!  M. Scot Breitenfeld
!  December 1, 2008
!
! Fortran2003 Interface:
  SUBROUTINE h5oget_info_by_name_f(loc_id, name, object_info, hdferr, lapl_id, fields)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)            :: loc_id
    CHARACTER(LEN=*), INTENT(IN)            :: name
    TYPE(h5o_info_t), INTENT(OUT), TARGET   :: object_info
    INTEGER         , INTENT(OUT)           :: hdferr
    INTEGER(HID_T)  , INTENT(IN) , OPTIONAL :: lapl_id
    INTEGER         , INTENT(IN) , OPTIONAL :: fields 
!*****
    INTEGER(SIZE_T) :: namelen
    INTEGER(HID_T)  :: lapl_id_default
    TYPE(C_PTR)     :: ptr
    INTEGER :: fields_c
    
    INTERFACE
       INTEGER FUNCTION h5oget_info_by_name_c(loc_id, name, namelen, lapl_id_default, object_info, fields) &
            BIND(C, NAME='h5oget_info_by_name_c')
         IMPORT :: c_char, c_ptr
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN)  :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(SIZE_T) , INTENT(IN)  :: namelen
         INTEGER(HID_T)  , INTENT(IN)  :: lapl_id_default
         TYPE(C_PTR),VALUE             :: object_info
         INTEGER         , INTENT(IN)  :: fields
       END FUNCTION h5oget_info_by_name_c
    END INTERFACE

    fields_c = H5O_INFO_ALL_F
    IF(PRESENT(fields)) fields_c = fields

    namelen = LEN(name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    ptr = C_LOC(object_info)

    hdferr = H5Oget_info_by_name_c(loc_id, name, namelen, lapl_id_default, ptr, fields_c)

  END SUBROUTINE H5Oget_info_by_name_f

!****s* H5O (F03)/h5oget_info_f_F03
!
! NAME
!  h5oget_info_f
!
! PURPOSE
!  Retrieves the metadata for an object specified by an identifier.
!
! Inputs:
!  object_id   - Identifier for target object.
!
! Outputs:
!  object_info - Buffer in which to return object information.
!  hdferr      - Returns 0 if successful and -1 if fails.
!
! Optional parameters:
!  fields      - Flags specifying the fields to include in object_info.
!
! AUTHOR
!  M. Scot Breitenfeld
!  May 11, 2012
!
! Fortran2003 Interface:
  SUBROUTINE h5oget_info_f(object_id, object_info, hdferr, fields)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)            :: object_id
    TYPE(h5o_info_t), INTENT(OUT), TARGET   :: object_info
    INTEGER         , INTENT(OUT)           :: hdferr
    INTEGER         , INTENT(IN), OPTIONAL  :: fields 
!*****
    TYPE(C_PTR) :: ptr
    INTEGER :: fields_c

    INTERFACE
       INTEGER FUNCTION h5oget_info_c(object_id, object_info, fields) &
            BIND(C, NAME='h5oget_info_c')
         IMPORT :: C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN)  :: object_id
         TYPE(C_PTR), VALUE          :: object_info
         INTEGER, INTENT(IN)         :: fields
       END FUNCTION h5oget_info_c
    END INTERFACE

    fields_c = H5O_INFO_ALL_F
    IF(PRESENT(fields)) fields_c = fields

    ptr = C_LOC(object_info)
    hdferr = H5Oget_info_c(object_id, ptr, fields_c)

  END SUBROUTINE H5Oget_info_f

!****s* H5O (F03)/h5oget_info_by_idx_f_F03
!
! NAME
!  h5oget_info_by_idx_f
!
! PURPOSE
!  Retrieves the metadata for an object, identifying the object by an index position.
!
! Inputs:
!  loc_id      - File or group identifier specifying location of group 
!                in which object is located.
!  group_name  - Name of group in which object is located.
!  index_field - Index or field that determines the order.
!  order       - Order within field or index.
!  n           - Object for which information is to be returned
!
! Outputs:  
!  object_info - Buffer in which to return object information.
!  hdferr      - Returns 0 if successful and -1 if fails.
!
! Optional parameters:
!  lapl_id     - Link access property list. (Not currently used.)
!  fields      - Flags specifying the fields to include in object_info.
!
! AUTHOR
!  M. Scot Breitenfeld
!  May 11, 2012
!
! Fortran2003 Interface:
  SUBROUTINE h5oget_info_by_idx_f(loc_id, group_name, index_field, order, n, &
       object_info, hdferr, lapl_id, fields)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)            :: loc_id
    CHARACTER(LEN=*), INTENT(IN)            :: group_name
    INTEGER         , INTENT(IN)            :: index_field
    INTEGER         , INTENT(IN)            :: order
    INTEGER(HSIZE_T), INTENT(IN)            :: n
    TYPE(h5o_info_t), INTENT(OUT), TARGET   :: object_info
    INTEGER         , INTENT(OUT)           :: hdferr
    INTEGER(HID_T)  , INTENT(IN) , OPTIONAL :: lapl_id
    INTEGER         , INTENT(IN) , OPTIONAL :: fields 
!*****
    INTEGER(SIZE_T) :: namelen
    INTEGER(HID_T)  :: lapl_id_default
    TYPE(C_PTR)     :: ptr
    INTEGER         :: fields_c
    
    INTERFACE
       INTEGER FUNCTION h5oget_info_by_idx_c(loc_id, group_name, namelen, &
            index_field, order, n, lapl_id_default, object_info, fields) BIND(C, NAME='h5oget_info_by_idx_c')
         IMPORT :: c_char, c_ptr, c_funptr
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         INTEGER(HID_T)  , INTENT(IN)  :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: group_name
         INTEGER(SIZE_T) , INTENT(IN)  :: namelen
         INTEGER         , INTENT(IN)  :: index_field
         INTEGER         , INTENT(IN)  :: order
         INTEGER(HSIZE_T), INTENT(IN)  :: n
         INTEGER(HID_T)  , INTENT(IN)  :: lapl_id_default
         TYPE(C_PTR), VALUE            :: object_info
         INTEGER, INTENT(IN)           :: fields
       END FUNCTION h5oget_info_by_idx_c
    END INTERFACE

    fields_c = H5O_INFO_ALL_F
    IF(PRESENT(fields)) fields_c = fields

    namelen = LEN(group_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    ptr = C_LOC(object_info)
    hdferr = H5Oget_info_by_idx_c(loc_id, group_name, namelen, index_field, order, n, lapl_id_default, ptr, fields_c)

  END SUBROUTINE H5Oget_info_by_idx_f


!****s* H5O (F03)/h5ovisit_by_name_f_F03
!
! NAME
!  h5ovisit_by_name_f
!
! PURPOSE
!  Recursively visits all objects starting from a specified object.
!
! Inputs:
!  loc_id      - Identifier of a file or group.
!  object_name - Name of the object, generally relative to loc_id, that will serve as root of the iteration 
!  index_type  - Type of index; valid values include:
!                 H5_INDEX_NAME_F
!                 H5_INDEX_CRT_ORDER_F
!  order       - Order in which index is traversed; valid values include:
!                 H5_ITER_DEC_F
!                 H5_ITER_INC_F
!                 H5_ITER_NATIVE_F
!  op 	       - Callback function passing data regarding the group to the calling application
!  op_data     - User-defined pointer to data required by the application for its processing of the group
!
! Outputs:
!  return_value - Returns the return value of the first operator that returns a positive value, or 
!                 zero if all members were processed with no operator returning non-zero.
!  hdferr       - Returns 0 if successful and -1 if fails
!
! Optional parameters:
!  lapl_id      - Link access property list identifier.
!  fields       - Flags specifying the fields to include in object_info.
!
! AUTHOR
!  M. Scot Breitenfeld
!  November 19, 2008
!
! Fortran2003 Interface:
  SUBROUTINE h5ovisit_by_name_f(loc_id, object_name, index_type, order, op, op_data, &
       return_value, hdferr, lapl_id, fields)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)             :: loc_id
    CHARACTER(LEN=*), INTENT(IN)             :: object_name
    INTEGER         , INTENT(IN)             :: index_type 
    INTEGER         , INTENT(IN)             :: order

    TYPE(C_FUNPTR)                           :: op
    TYPE(C_PTR)                              :: op_data
    INTEGER         , INTENT(OUT)            :: return_value
    INTEGER         , INTENT(OUT)            :: hdferr
    INTEGER(HID_T)  , INTENT(IN) , OPTIONAL  :: lapl_id
    INTEGER         , INTENT(IN) , OPTIONAL  :: fields 
!*****

    INTEGER(SIZE_T) :: namelen
    INTEGER(HID_T)  :: lapl_id_default
    INTEGER :: fields_c

    INTERFACE
       INTEGER FUNCTION h5ovisit_by_name_c(loc_id, object_name, namelen, index_type, order, &
            op, op_data, lapl_id, fields) BIND(C, NAME='h5ovisit_by_name_c')
         IMPORT :: C_CHAR, C_PTR, C_FUNPTR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: object_name
         INTEGER(SIZE_T)              :: namelen
         INTEGER         , INTENT(IN) :: index_type
         INTEGER         , INTENT(IN) :: order
         TYPE(C_FUNPTR)  , VALUE      :: op
         TYPE(C_PTR)     , VALUE      :: op_data
         INTEGER(HID_T)  , INTENT(IN) :: lapl_id
         INTEGER         , INTENT(IN) :: fields
       END FUNCTION h5ovisit_by_name_c
    END INTERFACE

    fields_c = H5O_INFO_ALL_F
    IF(PRESENT(fields)) fields_c = fields

    namelen = LEN(object_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    return_value = h5ovisit_by_name_c(loc_id, object_name, namelen, index_type, order, &
         op, op_data, lapl_id_default, fields_c)

    IF(return_value.GE.0)THEN
       hdferr = 0
    ELSE
       hdferr = -1
    END IF

  END SUBROUTINE h5ovisit_by_name_f

END MODULE H5O

