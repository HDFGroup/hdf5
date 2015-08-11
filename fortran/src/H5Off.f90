!****h* ROBODoc/H5O
!
! NAME
!  MODULE H5O
!
! FILE
!  fortran/src/H5Off.f90
!
! PURPOSE
!  This file contains Fortran interfaces for H5O functions. It includes
!  all the functions that are independent on whether the Fortran 2003 functions
!  are enabled or disabled.
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
!  If you add a new H5O function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5O

  USE H5GLOBAL

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
            lcpl_id_default, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OLINK_C'::h5olink_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: new_link_name
         INTEGER(HID_T), INTENT(IN) :: object_id
         INTEGER(HID_T), INTENT(IN) :: new_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: new_link_name
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
       INTEGER FUNCTION h5oopen_c(loc_id, name, namelen, lapl_id_default, obj_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OOPEN_C'::h5oopen_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
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
       INTEGER FUNCTION h5oclose_c(object_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OCLOSE_C'::h5oclose_c
         !DEC$ENDIF
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
       INTEGER FUNCTION h5oopen_by_addr_c(loc_id, addr, obj_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OOPEN_BY_ADDR_C'::h5oopen_by_addr_c
         !DEC$ENDIF
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
            dst_loc_id, dst_name, dst_name_len, ocpypl_id_default, lcpl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OCOPY_C'::h5ocopy_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: src_name, dst_name
         INTEGER(HID_T)  , INTENT(IN) :: src_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: src_name
         INTEGER(HID_T)  , INTENT(IN) :: dst_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: dst_name
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
       INTEGER FUNCTION h5odecr_refcount_c(object_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ODECR_REFCOUNT_C'::h5odecr_refcount_c
         !DEC$ENDIF
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
       INTEGER FUNCTION h5oexists_by_name_c(loc_id, name, namelen, lapl_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OEXISTS_BY_NAME_C'::h5oexists_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T)  , INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
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
       INTEGER FUNCTION h5oget_comment_c(obj_id, comment, commentsize_default, bufsize)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OGET_COMMENT_C'::h5oget_comment_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: comment
         INTEGER(HID_T)  , INTENT(IN)  :: obj_id
         CHARACTER(LEN=*), INTENT(OUT) :: comment
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
            comment, commentsize_default, bufsize_default, lapl_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OGET_COMMENT_BY_NAME_C'::h5oget_comment_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: comment, name
         INTEGER(HID_T)  , INTENT(IN)  :: loc_id
         CHARACTER(LEN=*), INTENT(IN)  :: name
         INTEGER(SIZE_T) , INTENT(IN)  :: name_size
         CHARACTER(LEN=*), INTENT(OUT) :: comment
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
       INTEGER FUNCTION h5oincr_refcount_c(obj_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OINCR_REFCOUNT_C'::h5oincr_refcount_c
         !DEC$ENDIF
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
       INTEGER FUNCTION h5oopen_by_idx_c(loc_id, group_name, group_namelen, index_type, order, n, obj_id, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OOPEN_BY_IDX_C'::h5oopen_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: group_name
         INTEGER(HID_T)  , INTENT(IN)  :: loc_id
         CHARACTER(LEN=*), INTENT(IN)  :: group_name
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
       INTEGER FUNCTION h5oset_comment_c(obj_id, comment, commentlen)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OSET_COMMENT_C'::h5oset_comment_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: comment
         INTEGER(HID_T)  , INTENT(IN) :: obj_id
         CHARACTER(LEN=*), INTENT(IN) :: comment
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
       INTEGER FUNCTION h5oset_comment_by_name_c(loc_id, name, namelen, comment, commentlen, lapl_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OSET_COMMENT_BY_NAME_C'::h5oset_comment_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name, comment
         INTEGER(HID_T)  , INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: comment
         INTEGER(SIZE_T) , INTENT(IN) :: commentlen
         CHARACTER(LEN=*), INTENT(IN) :: name
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

END MODULE H5O

