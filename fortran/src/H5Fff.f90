!
! This file contains Fortran90 interfaces for H5F functions.
! 
      MODULE H5F
      USE H5GLOBAL
 
        CONTAINS
          
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
            INTEGER :: creation_prp_default 
            INTEGER :: access_prp_default
            INTEGER :: namelen ! Length of the name character string
            INTEGER, EXTERNAL :: h5fcreate_c
  
            creation_prp_default = H5P_DEFAULT_F
            access_prp_default = H5P_DEFAULT_F

            if (present(creation_prp)) creation_prp_default = creation_prp 
            if (present(access_prp))   access_prp_default   = access_prp 
            namelen = LEN(name)
            hdferr = h5fcreate_c(name, namelen, access_flags, &
                     creation_prp_default, access_prp_default, file_id) 

          END SUBROUTINE h5fcreate_f

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

            INTEGER, EXTERNAL :: h5fflush_c

           hdferr = h5fflush_c(object_id, scope) 

          END SUBROUTINE h5fflush_f

 
          SUBROUTINE h5fmount_f(loc_id, dsetname, file_id, hdferr, access_prp)
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! Identifier for file or group 
                                                   ! in which dsetname is defined 
            CHARACTER(LEN=*), INTENT(IN) :: dsetname  ! Name of the dataset
            INTEGER(HID_T), INTENT(IN) :: file_id ! File identifier for the
                                                   ! file to be mounted 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp
                                                   ! File access property list
                                                   ! identifier
            INTEGER :: access_prp_default 
            INTEGER :: namelen ! Length of the dsetname character string
            INTEGER, EXTERNAL :: h5fmount_c
  
            access_prp_default = H5P_DEFAULT_F
            if (present(access_prp))   access_prp_default   = access_prp 
            namelen = LEN(dsetname)
            hdferr = h5fmount_c(loc_id, dsetname, namelen, file_id, access_prp_default) 
                      
          END SUBROUTINE h5fmount_f


          SUBROUTINE h5funmount_f(loc_id, dsetname, hdferr)
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! Identifier for file or group 
                                                   ! in which dsetname is defined 
            CHARACTER(LEN=*), INTENT(IN) :: dsetname  ! Name of the dataset
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 
            INTEGER :: namelen ! Length of the dsetname character string
            INTEGER, EXTERNAL :: h5funmount_c
  
            namelen = LEN(dsetname)
            hdferr = h5funmount_c(loc_id, dsetname, namelen) 
    
          END SUBROUTINE h5funmount_f
          
          SUBROUTINE h5fopen_f(name, access_flags, file_id, hdferr, &
                               access_prp)
           
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the file
            INTEGER, INTENT(IN) :: access_flags    ! File access flags
            INTEGER(HID_T), INTENT(OUT) :: file_id ! File identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp
                                                   ! File access property list
                                                   ! identifier
            INTEGER :: access_prp_default 
            INTEGER :: namelen ! Length of the name character string
            INTEGER, EXTERNAL :: h5fopen_c
  
            access_prp_default = H5P_DEFAULT_F
            if (present(access_prp))   access_prp_default   = access_prp 
            namelen = LEN(name)
            hdferr = h5fopen_c(name, namelen, access_flags, &
                               access_prp_default, file_id) 

          END SUBROUTINE h5fopen_f

          SUBROUTINE h5freopen_f(file_id, ret_file_id, hdferr)
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id      ! File identifier 
            INTEGER(HID_T), INTENT(OUT) :: ret_file_id ! New File identifier 
            INTEGER, INTENT(OUT) :: hdferr             ! Error code 
            INTEGER, EXTERNAL :: h5freopen_c
  
            hdferr = h5freopen_c(file_id, ret_file_id) 

          END SUBROUTINE h5freopen_f
          
          SUBROUTINE h5fget_create_plist_f(file_id, prop_id, hdferr)
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id    ! File identifier 
            INTEGER(HID_T), INTENT(OUT) :: prop_id   ! File creation property
                                                     ! list identifier 
            INTEGER, INTENT(OUT) :: hdferr           ! Error code 
            INTEGER, EXTERNAL :: h5fget_create_plist_c
  
            hdferr = h5fget_create_plist_c(file_id, prop_id) 

          END SUBROUTINE h5fget_create_plist_f

          SUBROUTINE h5fget_access_plist_f(file_id, access_id, hdferr)
           
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id      ! File identifier 
            INTEGER(HID_T), INTENT(OUT) :: access_id   ! File access property
                                                       ! list identifier 
            INTEGER, INTENT(OUT) :: hdferr             ! Error code 
            INTEGER, EXTERNAL :: h5fget_access_plist_c
  
            hdferr = h5fget_access_plist_c(file_id, access_id) 

          END SUBROUTINE h5fget_access_plist_f
 

          SUBROUTINE h5fis_hdf5_f(name, status, hdferr)
           
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the file
            LOGICAL, INTENT(OUT) :: status         ! Indicates if file
                                                   ! is an HDF5 file
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 
            INTEGER :: namelen ! Length of the name character string
            INTEGER :: flag    ! "TRUE/FALSE" flag from C routine
                               ! to define status value. 
            INTEGER, EXTERNAL :: h5fis_hdf5_c
  
            namelen = LEN(name)
            hdferr = h5fis_hdf5_c(name, namelen, flag) 
            status = .TRUE.
            if (flag .EQ. 0) status = .FALSE.

          END SUBROUTINE h5fis_hdf5_f
          
          SUBROUTINE h5fclose_f(file_id, hdferr)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id ! File identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER, EXTERNAL :: h5fclose_c

            hdferr = h5fclose_c(file_id)

          END SUBROUTINE h5fclose_f

      END MODULE H5F
