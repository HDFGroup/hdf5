!
! This file contains Fortran90 interfaces for H5F functions.
! 
      MODULE H5G
      USE H5FORTRAN_TYPES
      USE H5FORTRAN_FLAGS
 
        CONTAINS
          
          !!!============================================================

          SUBROUTINE h5gcreate_f(loc_id, name, grp_id, hdferr, size_hint)
           
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
            INTEGER, EXTERNAL :: h5gcreate_c
            size_hint_default = OBJECT_NAMELEN_DEFAULT_F 
            if (present(size_hint)) size_hint_default = size_hint 
            namelen = LEN(name)
            hdferr = h5gcreate_c(loc_id, name, namelen, size_hint_default, &
                                 grp_id)

          END SUBROUTINE h5gcreate_f
          
          !!!============================================================

          SUBROUTINE h5gopen_f(loc_id, name, grp_id, hdferr)
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the group 
            INTEGER(HID_T), INTENT(OUT) :: grp_id  ! File identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 

            INTEGER :: namelen ! Length of the name character string
            INTEGER, EXTERNAL :: h5gopen_c
  
            namelen = LEN(name)
            hdferr = h5gopen_c(loc_id, name, namelen, grp_id) 

          END SUBROUTINE h5gopen_f
          
          !!!============================================================

          SUBROUTINE h5gclose_f(grp_id, hdferr)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: grp_id  ! Group identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5gclose_c

            hdferr = h5gclose_c(grp_id)

          END SUBROUTINE h5gclose_f

          !!!============================================================

          SUBROUTINE h5gget_obj_info_idx_f(loc_id, name, idx, &
                                           obj_name, obj_type, hdferr)
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the group 
            INTEGER, INTENT(IN) :: idx             ! Index of member object 
            CHARACTER(LEN=*), INTENT(OUT) :: obj_name   ! Name of the object 
            INTEGER, INTENT(OUT) :: obj_type       ! Object type 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 

            INTEGER :: namelen ! Length of the name character string
            INTEGER :: obj_namelen ! Length of the obj_name character string
            INTEGER, EXTERNAL :: h5gget_obj_info_idx_c

            namelen = LEN(name)
            obj_namelen = LEN(obj_name)
            hdferr = h5gget_obj_info_idx_c(loc_id, name, namelen, idx, &
                                           obj_name, obj_namelen, obj_type)     

          END SUBROUTINE h5gget_obj_info_idx_f
          
          !!!============================================================

          SUBROUTINE h5gn_members_f(loc_id, name, nmembers, hdferr)
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the group 
            INTEGER, INTENT(OUT) :: nmembers       ! Number of members in the
                                                   ! group 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 

            INTEGER :: namelen ! Length of the name character string
            INTEGER, EXTERNAL :: h5gn_members_c
  
            namelen = LEN(name)
            hdferr = h5gn_members_c(loc_id, name, namelen, nmembers) 

          END SUBROUTINE h5gn_members_f

          !!!============================================================
          
          SUBROUTINE h5glink_f(loc_id, link_type, current_name, &
                                                   new_name, hdferr)

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
            INTEGER, EXTERNAL :: h5glink_c
            
            current_namelen = LEN(current_name)
            new_namelen = LEN(new_name)
            hdferr = h5glink_c(loc_id, link_type, current_name, &
                               current_namelen, new_name, new_namelen)
          END SUBROUTINE h5glink_f

          !!!============================================================

          SUBROUTINE h5gunlink_f(loc_id, name, hdferr)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of an object 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            
            INTEGER :: namelen ! Lenghth of the name character string
            
            INTEGER, EXTERNAL :: h5gunlink_c
            
            namelen = LEN(name)
            hdferr = h5gunlink_c(loc_id, name, namelen)
          END SUBROUTINE h5gunlink_f

          !!!============================================================
          
          SUBROUTINE h5gmove_f(loc_id, name, new_name, hdferr)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Current name of an object 
            CHARACTER(LEN=*), INTENT(IN) :: new_name ! New name of an object
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            
            INTEGER :: namelen         ! Lenghth of the current_name string
            INTEGER :: new_namelen     ! Lenghth of the new_name string
            INTEGER, EXTERNAL :: h5gmove_c
            
            namelen = LEN(name)
            new_namelen = LEN(new_name)
            hdferr = h5gmove_c(loc_id, name, namelen, new_name, new_namelen)
          END SUBROUTINE h5gmove_f

          !!!============================================================

          SUBROUTINE h5gget_linkval_f(loc_id, name, size, buffer, hdferr)

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
            INTEGER, EXTERNAL :: h5gget_linkval_c
            
            namelen = LEN(name)
            hdferr = h5gget_linkval_c(loc_id, name, namelen, size, buffer)
          END SUBROUTINE h5gget_linkval_f

          !!!============================================================

           SUBROUTINE h5gset_comment_f(loc_id, name, comment, hdferr)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Current name of an object 
            CHARACTER(LEN=*), INTENT(IN) :: comment ! New name of an object
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            
            INTEGER :: namelen ! Lenghth of the current_name string
            INTEGER :: commentlen     ! Lenghth of the comment string
            INTEGER, EXTERNAL :: h5gset_comment_c
            
            namelen = LEN(name)
            commentlen = LEN(comment)
            hdferr = h5gset_comment_c(loc_id, name, namelen, comment, commentlen)
          END SUBROUTINE h5gset_comment_f

          !!!============================================================

          SUBROUTINE h5gget_comment_f(loc_id, name, size, buffer, hdferr)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Current name of an object 
            INTEGER(SIZE_T), INTENT(IN) :: size    ! Maximum number of buffer
            CHARACTER(LEN=size), INTENT(OUT) :: buffer 
                                                   ! Buffer to hold a comment
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            
            INTEGER :: namelen ! Lenghth of the current_name string
            INTEGER, EXTERNAL :: h5gget_comment_c
            
            namelen = LEN(name)
            hdferr = h5gget_comment_c(loc_id, name, namelen, size, buffer)
          END SUBROUTINE h5gget_comment_f

          !!!============================================================

      END MODULE H5G
