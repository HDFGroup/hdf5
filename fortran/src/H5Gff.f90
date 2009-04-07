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
!
! This file contains Fortran90 interfaces for H5F functions.
! 
      MODULE H5G
      USE H5GLOBAL
 
        CONTAINS
          
!----------------------------------------------------------------------
! Name:		h5gcreate_f 
!
! Purpose:	Creates a new group. 
!
! Inputs:  
!		loc_id		- location identifier
!		name		- group name at the specified location
! Outputs:  
!		grp_id		- group identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!		size_hint	- a parameter indicating the number of bytes 
!				  to reserve for the names that will appear 
!				  in the group
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 5, 2001 
!
! Comment:		
!----------------------------------------------------------------------
          SUBROUTINE h5gcreate_f(loc_id, name, grp_id, hdferr, size_hint)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5gcreate_f
!DEC$endif
!
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the group 
            INTEGER(HID_T), INTENT(OUT) :: grp_id  ! Group identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 
            INTEGER(SIZE_T), OPTIONAL, INTENT(IN) :: size_hint 
                                                   ! Parameter indicating
                                                   ! the number of bytes
                                                   ! to reserve for the
                                                   ! names that will appear
                                                   ! in the group  
            INTEGER :: namelen ! Length of the name character string
            INTEGER(SIZE_T) :: size_hint_default 

!            INTEGER, EXTERNAL :: h5gcreate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5gcreate_c(loc_id, name, namelen, &
                               size_hint_default, grp_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GCREATE_C'::h5gcreate_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER(SIZE_T) :: size_hint_default
              INTEGER(HID_T), INTENT(OUT) :: grp_id
              END FUNCTION h5gcreate_c
            END INTERFACE

            size_hint_default = OBJECT_NAMELEN_DEFAULT_F 
            if (present(size_hint)) size_hint_default = size_hint 
            namelen = LEN(name)
            hdferr = h5gcreate_c(loc_id, name, namelen, size_hint_default, &
                                 grp_id)

          END SUBROUTINE h5gcreate_f

!----------------------------------------------------------------------
! Name:		h5gopen_f 
!
! Purpose: 	Opens an existing group. 	
!
! Inputs:  
!		loc_id		- location identifier
!		name 		- name of the group to open
! Outputs:  
!		grp_id		- group identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 5, 2001 
!
! Comment:		
!----------------------------------------------------------------------
          SUBROUTINE h5gopen_f(loc_id, name, grp_id, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5gopen_f
!DEC$endif
!
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the group 
            INTEGER(HID_T), INTENT(OUT) :: grp_id  ! File identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 

            INTEGER :: namelen ! Length of the name character string

!            INTEGER, EXTERNAL :: h5gopen_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5gopen_c(loc_id, name, namelen, grp_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GOPEN_C'::h5gopen_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER(HID_T), INTENT(OUT) :: grp_id
              END FUNCTION h5gopen_c
            END INTERFACE
  
            namelen = LEN(name)
            hdferr = h5gopen_c(loc_id, name, namelen, grp_id) 

          END SUBROUTINE h5gopen_f

!----------------------------------------------------------------------
! Name:		h5gclose_f 
!
! Purpose:	Closes the specified group. 	
!
! Inputs:  
!		grp_id		- group identifier
! Outputs:  
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 5, 2001 
!
! Comment:		
!----------------------------------------------------------------------
          SUBROUTINE h5gclose_f(grp_id, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5gclose_f
!DEC$endif
!

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: grp_id  ! Group identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5gclose_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5gclose_c(grp_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GCLOSE_C'::h5gclose_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: grp_id
              END FUNCTION h5gclose_c
            END INTERFACE

            hdferr = h5gclose_c(grp_id)

          END SUBROUTINE h5gclose_f


!----------------------------------------------------------------------
! Name:		h5gget_obj_info_idx_f 
!
! Purpose:	Returns name and type of the group member identified by 
!		its index. 
!
! Inputs:  
!		loc_id		- location identifier
!		name		- name of the group at the specified location
!		idx		- object index (zero-based)
! Outputs:  
!		obj_name	- object name
!		obj_type	- object type
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 5, 2001 
!
! Comment:		
!----------------------------------------------------------------------
          SUBROUTINE h5gget_obj_info_idx_f(loc_id, name, idx, &
                                           obj_name, obj_type, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5gget_obj_info_idx_f
!DEC$endif
!
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the group 
            INTEGER, INTENT(IN) :: idx             ! Index of member object 
            CHARACTER(LEN=*), INTENT(OUT) :: obj_name   ! Name of the object 
            INTEGER, INTENT(OUT) :: obj_type       ! Object type 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 

            INTEGER :: namelen ! Length of the name character string
            INTEGER :: obj_namelen ! Length of the obj_name character string

!            INTEGER, EXTERNAL :: h5gget_obj_info_idx_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5gget_obj_info_idx_c(loc_id, name, &
                               namelen, idx, &
                               obj_name, obj_namelen, obj_type)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GGET_OBJ_INFO_IDX_C'::h5gget_obj_info_idx_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              !DEC$ATTRIBUTES reference :: obj_name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER, INTENT(IN) :: idx
              CHARACTER(LEN=*), INTENT(OUT) :: obj_name
              INTEGER :: obj_namelen
              INTEGER, INTENT(OUT) :: obj_type
              END FUNCTION h5gget_obj_info_idx_c
            END INTERFACE

            namelen = LEN(name)
            obj_namelen = LEN(obj_name)
            hdferr = h5gget_obj_info_idx_c(loc_id, name, namelen, idx, &
                                           obj_name, obj_namelen, obj_type)     

          END SUBROUTINE h5gget_obj_info_idx_f

!----------------------------------------------------------------------
! Name:		h5gn_members_f 
!
! Purpose: 	Returns the number of group members. 	
!
! Inputs:  
!		loc_id		- location identifier
!		name		- name of the group at the specified location
! Outputs:  
!		nmembers	- number of group members
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 5, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5gn_members_f(loc_id, name, nmembers, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5gn_members_f
!DEC$endif
!
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the group 
            INTEGER, INTENT(OUT) :: nmembers       ! Number of members in the
                                                   ! group 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 

            INTEGER :: namelen ! Length of the name character string
  
!            INTEGER, EXTERNAL :: h5gn_members_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5gn_members_c(loc_id, name, namelen, nmembers)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GN_MEMBERS_C'::h5gn_members_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER, INTENT(OUT) :: nmembers
              END FUNCTION h5gn_members_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5gn_members_c(loc_id, name, namelen, nmembers) 

          END SUBROUTINE h5gn_members_f

!----------------------------------------------------------------------
! Name:		h5glink_f 
!
! Purpose: 	Creates a link of the specified type from new_name 
!		to current_name. 	
!
! Inputs:  
!		loc_id		- location identifier
!		link_type	- link type
!				  Possible values are:
!				  H5G_LINK_HARD_F (0) or
!				  H5G_LINK_SOFT_F (1) 
!		current_name	- name of the existing object if link is a 
!				  hard link. Can be anything for the soft link. 
!		new_name	- new name for the object
! Outputs:  
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 5, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5glink_f(loc_id, link_type, current_name, &
                                                   new_name, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5glink_f
!DEC$endif
!

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            INTEGER, INTENT(IN) :: link_type       ! link type
                                                   ! Possible values are:
                                                   ! H5G_LINK_HARD_F (0) or
                                                   ! H5G_LINK_SOFT_F (1) 
            
            CHARACTER(LEN=*), INTENT(IN) :: current_name   
                                                   ! Current name of an object 
            CHARACTER(LEN=*), INTENT(IN) :: new_name ! New name of an object
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            
            INTEGER :: current_namelen ! Lenghth of the current_name string
            INTEGER :: new_namelen     ! Lenghth of the new_name string

!            INTEGER, EXTERNAL :: h5glink_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5glink_c(loc_id, link_type, current_name, &
                               current_namelen, new_name, new_namelen)
                              
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GLINK_C'::h5glink_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: current_name
              !DEC$ATTRIBUTES reference :: new_name
              INTEGER(HID_T), INTENT(IN) :: loc_id 
              INTEGER, INTENT(IN) :: link_type
              CHARACTER(LEN=*), INTENT(IN) :: current_name
              INTEGER :: current_namelen
              CHARACTER(LEN=*), INTENT(IN) :: new_name
              INTEGER :: new_namelen
              END FUNCTION h5glink_c
            END INTERFACE
            
            current_namelen = LEN(current_name)
            new_namelen = LEN(new_name)
            hdferr = h5glink_c(loc_id, link_type, current_name, &
                               current_namelen, new_name, new_namelen)
          END SUBROUTINE h5glink_f

!----------------------------------------------------------------------
! Name:		h5glink2_f 
!
! Purpose: 	Creates a link of the specified type from new_name 
!		to current_name. current_name and new_name are interpreted
!               releative to current and new location identifiers.	
!
! Inputs:  
!		cur_loc_id	- location identifier
!		cur_name	- name of the existing object if link is a 
!				  hard link. Can be anything for the soft link. 
!		link_type	- link type
!				  Possible values are:
!				  H5G_LINK_HARD_F (0) or
!				  H5G_LINK_SOFT_F (1) 
!		new_loc_id	- new location identifier
!		new_name	- new name for the object
! Outputs:  
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		September 25, 2002
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5glink2_f(cur_loc_id, cur_name, link_type, new_loc_id, &
                                                   new_name, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5glink2_f
!DEC$endif
!

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: cur_loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: cur_name   
                                                   ! Current name of an object 
            INTEGER, INTENT(IN) :: link_type       ! link type
                                                   ! Possible values are:
                                                   ! H5G_LINK_HARD_F (0) or
                                                   ! H5G_LINK_SOFT_F (1) 
            
            INTEGER(HID_T), INTENT(IN) :: new_loc_id ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: new_name ! New name of an object
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            
            INTEGER :: cur_namelen ! Lenghth of the current_name string
            INTEGER :: new_namelen ! Lenghth of the new_name string

            INTERFACE
              INTEGER FUNCTION h5glink2_c(cur_loc_id, cur_name, cur_namelen, &
                               link_type, new_loc_id, &
                               new_name, new_namelen)
                              
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GLINK2_C'::h5glink2_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: cur_name
              !DEC$ATTRIBUTES reference :: new_name
              INTEGER(HID_T), INTENT(IN) :: cur_loc_id 
              INTEGER(HID_T), INTENT(IN) :: new_loc_id 
              INTEGER, INTENT(IN) :: link_type
              CHARACTER(LEN=*), INTENT(IN) :: cur_name
              CHARACTER(LEN=*), INTENT(IN) :: new_name
              INTEGER :: cur_namelen
              INTEGER :: new_namelen
              END FUNCTION h5glink2_c
            END INTERFACE
            
            cur_namelen = LEN(cur_name)
            new_namelen = LEN(new_name)
            hdferr = h5glink2_c(cur_loc_id, cur_name, cur_namelen, link_type, &
                               new_loc_id, new_name, new_namelen)
          END SUBROUTINE h5glink2_f

!----------------------------------------------------------------------
! Name:		h5gunlink_f 
!
! Purpose: 	Removes the specified name from the group graph and 
!		decrements the link count for the object to which name 
!		points	
!
! Inputs:  
!		loc_id		- location identifier
!		name		- name of the object to unlink
! Outputs:  
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 5, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5gunlink_f(loc_id, name, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5gunlink_f
!DEC$endif
!

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of an object 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            
            INTEGER :: namelen ! Lenghth of the name character string
            
!            INTEGER, EXTERNAL :: h5gunlink_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5gunlink_c(loc_id, name, namelen)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GUNLINK_C'::h5gunlink_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              END FUNCTION h5gunlink_c
            END INTERFACE
            
            namelen = LEN(name)
            hdferr = h5gunlink_c(loc_id, name, namelen)
          END SUBROUTINE h5gunlink_f

!----------------------------------------------------------------------
! Name:		h5gmove_f 
!
! Purpose:	Renames an object within an HDF5 file.  	
!
! Inputs:  
!		loc_id		- location identifier
!		name		- object's name at specified location
!		new_name	- object's new name
! Outputs:  
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 5, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          
          SUBROUTINE h5gmove_f(loc_id, name, new_name, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5gmove_f
!DEC$endif
!

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Current name of an object 
            CHARACTER(LEN=*), INTENT(IN) :: new_name ! New name of an object
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            
            INTEGER :: namelen         ! Lenghth of the current_name string
            INTEGER :: new_namelen     ! Lenghth of the new_name string

!            INTEGER, EXTERNAL :: h5gmove_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5gmove_c(loc_id, name, namelen, new_name, new_namelen)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GMOVE_C'::h5gmove_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              !DEC$ATTRIBUTES reference :: new_name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              CHARACTER(LEN=*), INTENT(IN) :: new_name
              INTEGER :: new_namelen
              END FUNCTION h5gmove_c
            END INTERFACE
            
            namelen = LEN(name)
            new_namelen = LEN(new_name)
            hdferr = h5gmove_c(loc_id, name, namelen, new_name, new_namelen)
          END SUBROUTINE h5gmove_f

!----------------------------------------------------------------------
! Name:		h5gmove2_f 
!
! Purpose:	Renames an object within an HDF5 file.  	
!
! Inputs:  
!		src_loc_id	- original location identifier
!		src_name	- object's name at specified original location
!		dst_loc_id	- original location identifier
!		dst_name	- object's new name
! Outputs:  
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		September 25, 2002
!
! Comment:		
!----------------------------------------------------------------------

          
          SUBROUTINE h5gmove2_f(src_loc_id, src_name, dst_loc_id, dst_name, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5gmove2_f
!DEC$endif
!

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN)   :: src_loc_id  ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: src_name    ! Original name of an object 
            INTEGER(HID_T), INTENT(IN)   :: dst_loc_id  ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: dst_name    ! New name of an object
            INTEGER, INTENT(OUT)         :: hdferr      ! Error code
            
            INTEGER :: src_namelen         ! Length of the current_name string
            INTEGER :: dst_namelen         ! Lenghth of the new_name string

!            INTEGER, EXTERNAL :: h5gmove2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5gmove2_c(src_loc_id, src_name, src_namelen, &
                               dst_loc_id, dst_name, dst_namelen)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GMOVE2_C'::h5gmove2_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: src_name
              !DEC$ATTRIBUTES reference :: dst_name
              INTEGER(HID_T), INTENT(IN) :: src_loc_id
              INTEGER(HID_T), INTENT(IN) :: dst_loc_id
              CHARACTER(LEN=*), INTENT(IN) :: src_name
              CHARACTER(LEN=*), INTENT(IN) :: dst_name
              INTEGER :: src_namelen
              INTEGER :: dst_namelen
              END FUNCTION h5gmove2_c
            END INTERFACE
            
            src_namelen = LEN(src_name)
            dst_namelen = LEN(dst_name)
            hdferr = h5gmove2_c(src_loc_id, src_name, src_namelen,&
                    dst_loc_id,  dst_name, dst_namelen)
          END SUBROUTINE h5gmove2_f

!----------------------------------------------------------------------
! Name:		h5gget_linkval_f 
!
! Purpose: 	Returns the name of the object that the symbolic link 
! 		points to. 	
!
! Inputs:  
!		loc_id		- location identifier
!		name		- symbolic link to the object whose name 
!				  is to be returned.  
!		size		- maximum number of characters to be returned
! Outputs:  
!		buffer		- a buffer to hold the name of the object 
!				  being sought 
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 5, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5gget_linkval_f(loc_id, name, size, buffer, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5gget_linkval_f
!DEC$endif
!

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Current name of an object 
            INTEGER(SIZE_T), INTENT(IN) :: size    ! Maximum number of buffer
            CHARACTER(LEN=size), INTENT(OUT) :: buffer 
                                                   ! Buffer to hold a name of
                                                   ! the object symbolic link
                                                   ! points to
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            
            INTEGER :: namelen ! Lenghth of the current_name string

!            INTEGER, EXTERNAL :: h5gget_linkval_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5gget_linkval_c(loc_id, name, namelen, size, buffer)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GGET_LINKVAL_C'::h5gget_linkval_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              !DEC$ATTRIBUTES reference :: buffer 
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name 
              INTEGER :: namelen
              INTEGER(SIZE_T), INTENT(IN) :: size
              CHARACTER(LEN=*), INTENT(OUT) :: buffer
              END FUNCTION h5gget_linkval_c
            END INTERFACE
            
            namelen = LEN(name)
            hdferr = h5gget_linkval_c(loc_id, name, namelen, size, buffer)
          END SUBROUTINE h5gget_linkval_f

!----------------------------------------------------------------------
! Name:		h5gset_comment_f 
!
! Purpose: 	Sets comment for specified object. 	
!
! Inputs:  
!		loc_id		- location identifier
!		name		- name of the object
!		comment		- comment to set for the object
! Outputs:  
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 5, 2001 
!
! Comment:		
!----------------------------------------------------------------------

           SUBROUTINE h5gset_comment_f(loc_id, name, comment, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5gset_comment_f
!DEC$endif
!

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Current name of an object 
            CHARACTER(LEN=*), INTENT(IN) :: comment ! New name of an object
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            
            INTEGER :: namelen ! Lenghth of the current_name string
            INTEGER :: commentlen     ! Lenghth of the comment string

!            INTEGER, EXTERNAL :: h5gset_comment_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5gset_comment_c(loc_id, name, namelen, &
                                                comment, commentlen)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GSET_COMMENT_C'::h5gset_comment_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name 
              !DEC$ATTRIBUTES reference :: comment 
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              CHARACTER(LEN=*), INTENT(IN) :: comment
              INTEGER :: commentlen
              END FUNCTION h5gset_comment_c
            END INTERFACE
            
            namelen = LEN(name)
            commentlen = LEN(comment)
            hdferr = h5gset_comment_c(loc_id, name, namelen, comment, commentlen)
          END SUBROUTINE h5gset_comment_f

!----------------------------------------------------------------------
! Name:		h5gget_comment_f 
!
! Purpose:	Retrieves comment for specified object.  	
!
! Inputs:  
!		loc_id		- location identifier
!		name		- name of the object at specified location
!		size		- size of the buffer required to hold comment
! Outputs:  
!		buffer		- buffer to hold object's comment
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 5, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5gget_comment_f(loc_id, name, size, buffer, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5gget_comment_f
!DEC$endif
!

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Current name of an object 
            INTEGER(SIZE_T), INTENT(IN) :: size    ! Maximum number of buffer
            CHARACTER(LEN=size), INTENT(OUT) :: buffer 
                                                   ! Buffer to hold a comment
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            
            INTEGER :: namelen ! Lenghth of the current_name string

!            INTEGER, EXTERNAL :: h5gget_comment_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5gget_comment_c(loc_id, name, namelen, &
                                                size, buffer)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5GGET_COMMENT_C'::h5gget_comment_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name 
              !DEC$ATTRIBUTES reference :: buffer 
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER(SIZE_T), INTENT(IN) :: size
              CHARACTER(LEN=*), INTENT(OUT) :: buffer
              END FUNCTION h5gget_comment_c
            END INTERFACE
            
            namelen = LEN(name)
            hdferr = h5gget_comment_c(loc_id, name, namelen, size, buffer)
          END SUBROUTINE h5gget_comment_f


      END MODULE H5G
