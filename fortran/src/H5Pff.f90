!****h* ROBODoc/H5P (F90)
!
! NAME
!  H5P_PROVISIONAL
!
! PURPOSE
!  This file contains Fortran interfaces for H5P functions. It includes
!  all the functions that are independent on whether the Fortran 2003 functions
!  are enabled or disabled.
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
!  If you add a new H5P function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!*****

MODULE H5P

  USE H5GLOBAL
  
  INTERFACE h5pset_fapl_multi_f
     MODULE PROCEDURE h5pset_fapl_multi_l
     MODULE PROCEDURE h5pset_fapl_multi_s
  END INTERFACE

CONTAINS

!****s* H5P/h5pcreate_f 
! NAME
!  h5pcreate_f 
!
! PURPOSE
!  Creates a new property as an instance of a property 
!  list class.
!
! INPUTS
!  class  - type of the property class to be created.
!	    Possible values are:
!             H5P_OBJECT_CREATE_F
!             H5P_FILE_CREATE_F
!             H5P_FILE_ACCESS_F
!             H5P_DATASET_CREATE_F
!             H5P_DATASET_ACCESS_F
!             H5P_DATASET_XFER_F
!             H5P_FILE_MOUNT_F
!             H5P_GROUP_CREATE_F
!             H5P_GROUP_ACCESS_F
!             H5P_DATATYPE_CREATE_F
!             H5P_DATATYPE_ACCESS_F
!             H5P_STRING_CREATE_F
!             H5P_ATTRIBUTE_CREATE _F
!             H5P_OBJECT_COPY_F
!             H5P_LINK_CREATE_F
!             H5P_LINK_ACCESS_F
!
! OUTPUTS
!  prp_id - property list identifier
!  hdferr - error code		
!	     Success:  0
!	     Failure: -1 
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!
! Fortran90 Interface:
  SUBROUTINE h5pcreate_f(class, prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: class
    INTEGER(HID_T), INTENT(OUT) :: prp_id
    INTEGER       , INTENT(OUT) :: hdferr
!*****
!            INTEGER, EXTERNAL :: h5pcreate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pcreate_c(class, prp_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PCREATE_C'::h5pcreate_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: class
         INTEGER(HID_T), INTENT(OUT) :: prp_id
       END FUNCTION h5pcreate_c
    END INTERFACE

    hdferr = h5pcreate_c(class, prp_id) 
  END SUBROUTINE h5pcreate_f

!****s* H5P/h5pset_preserve_f 
! NAME
!   h5pset_preserve_f 
!
! PURPOSE
!  Sets the dataset transfer property list status to 
!  TRUE or FALSE for initializing compound datatype
!  members during write/read operations.
!
! INPUTS
!  prp_id	- property list identifier
!  flag		- status flag
!
! OUTPUTS
!  hdferr	- Returns 0 if successful and -1 if fails
!
! OPTIONAL PARAMETERS
!  NONE
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!                       
!  Datatype of the flag parameter is changed from 
!  INTEGER to LOGICAL June 4, 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pset_preserve_f(prp_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    LOGICAL, INTENT(IN) ::  flag         ! TRUE/FALSE flag to set the dataset
                                         ! transfer property for partila writing/reading
                                         ! compound datatype
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****
    INTEGER :: flag_c

!            INTEGER, EXTERNAL :: h5pset_preserve_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_preserve_c(prp_id, flag_c)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_PRESERVE_C'::h5pset_preserve_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER ::  flag_c
       END FUNCTION h5pset_preserve_c
    END INTERFACE
    flag_c = 0
    IF(flag) flag_c = 1
    hdferr = h5pset_preserve_c(prp_id, flag_c) 
  END SUBROUTINE h5pset_preserve_f

!****s* H5P/h5pget_preserve_f 
! NAME
!  h5pget_preserve_f 
!
! PURPOSE
!  Checks status of the dataset transfer property list.
!
! INPUTS
!  prp_id  - property list identifier
!
! OUTPUTS
!  flag	   - status flag
!  hdferr  - error code		
!	Success:  0
!	Failure: -1
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001
! 
!  Datatype of the flag parameter is changed from 
!  INTEGER to LOGICAL 
!  June 4, 2003 
! 
! Fortran90 Interface:
  SUBROUTINE h5pget_preserve_f(prp_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    LOGICAL, INTENT(OUT) ::  flag        ! TRUE/FALSE flag. Shows status of the dataset's
                                         ! transfer property for partial writing/reading
                                         ! compound datatype
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****
    INTEGER :: flag_c

!            INTEGER, EXTERNAL :: h5pget_preserve_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_preserve_c(prp_id, flag_c)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_PRESERVE_C'::h5pget_preserve_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER ::  flag_c
       END FUNCTION h5pget_preserve_c
    END INTERFACE
    
    hdferr = h5pget_preserve_c(prp_id, flag_c) 
    flag = .FALSE.
    IF(flag_c .EQ. 1) flag = .TRUE.
  END SUBROUTINE h5pget_preserve_f

!****s* H5P/h5pget_class_f 
! NAME
!  h5pget_class_f 
!
! PURPOSE
!  Returns the property list class for a property list.
!
! INPUTS
!  
!  prp_id	- property list identifier
! OUTPUTS
!  
!  classtype	- property list class
!		  Possible values are:
!		   H5P_ROOT_F
!		   H5P_FILE_CREATE_F
!		   H5P_FILE_ACCESS_F
!		   H5PE_DATASET_CREATE_F
!		   H5P_DATASET_XFER_F
!		   H5P_FILE_MOUNT_F
!  hdferr:	- error code		
!		   Success:  0
!		   Failure: -1 
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!
! Fortran90 Interface:
  SUBROUTINE h5pget_class_f(prp_id, classtype, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: classtype    ! The type of the property list 
                                         ! to be created. Possible values are: 
                                         !  H5P_ROOT_F
                                         !  H5P_FILE_CREATE_F
                                         !  H5P_FILE_ACCESS_F
                                         !  H5PE_DATASET_CREATE_F 
                                         !  H5P_DATASET_XFER_F
                                         !  H5P_FILE_MOUNT_F
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!   INTEGER, EXTERNAL :: h5pget_class_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_class_c(prp_id, classtype)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_CLASS_C'::h5pget_class_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: classtype 
       END FUNCTION h5pget_class_c
    END INTERFACE

    hdferr = h5pget_class_c(prp_id, classtype) 
  END SUBROUTINE h5pget_class_f

!****s* H5P/h5pcopy_f 
! NAME
!  h5pcopy_f 
!
! PURPOSE
!  Copies an existing property list to create a new 
!  property list
!
! INPUTS
!  prp_id       - property list identifier
! OUTPUTS
!  new_prp_id	- new property list identifier
!  hdferr:	- error code		
!		   Success:  0
!		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001
!
! Fortran90 Interface:
  SUBROUTINE h5pcopy_f(prp_id, new_prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id      ! Property list identifier 
    INTEGER(HID_T), INTENT(OUT) :: new_prp_id ! Identifier of property list
    INTEGER, INTENT(OUT) :: hdferr            ! Error code
                                              ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pcopy_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pcopy_c(prp_id, new_prp_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PCOPY_C'::h5pcopy_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(OUT) :: new_prp_id
       END FUNCTION h5pcopy_c
    END INTERFACE
    
    hdferr = h5pcopy_c(prp_id, new_prp_id)
  END SUBROUTINE h5pcopy_f

!****s* H5P/h5pclose_f 
! NAME
!  h5pclose_f 
!
! PURPOSE
!  Terminates access to a property list. 
!
! INPUTS
!  prp_id - identifier of the property list to 
!	    terminate access to. 
! OUTPUTS
!  hdferr - error code		
!	    Success:  0
!	    Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pclose_f(prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pclose_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pclose_c(prp_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PCLOSE_C'::h5pclose_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id 
       END FUNCTION h5pclose_c
    END INTERFACE
    
    hdferr = h5pclose_c(prp_id)
  END SUBROUTINE h5pclose_f

!****s* H5P/h5pset_chunk_f 
! NAME
!   h5pset_chunk_f 
!
! PURPOSE
!  Sets the size of the chunks used to store 
!  a chunked layout dataset. 
!
! INPUTS
!  prp_id  - datatset creation property list identifier
!  ndims   - number of dimensions for each chunk
!  dims	   - array with dimension sizes for each chunk
! OUTPUTS
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!   Explicit Fortran interfaces were added for 
!   called C functions (it is needed for Windows
!   port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_chunk_f(prp_id, ndims, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: ndims         ! Number of chunk dimensions
    INTEGER(HSIZE_T), DIMENSION(ndims), INTENT(IN) :: dims    
                                         ! Array containing sizes of
                                         ! chunk dimensions
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_chunk_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_chunk_c(prp_id, ndims, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_CHUNK_C'::h5pset_chunk_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: ndims
         INTEGER(HSIZE_T), DIMENSION(ndims), INTENT(IN) :: dims
       END FUNCTION h5pset_chunk_c
    END INTERFACE
    
    hdferr =  h5pset_chunk_c(prp_id, ndims, dims)
  END SUBROUTINE h5pset_chunk_f

!****s* H5P/h5pget_chunk_f 
! NAME
!   h5pget_chunk_f 
!
! PURPOSE
!  Retrieves the size of chunks for the raw data of a 
!  chunked layout dataset
!
! INPUTS
!  prp_id	- property list identifier
!  ndims	- size of dims array
! OUTPUTS
!  dims		- array with dimension sizes for each chunk
!  hdferr	- error code		
!		   Success:  number of chunk dimensions
!		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pget_chunk_f(prp_id, ndims, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: ndims         ! Number of chunk dimensions to
                                         ! to return
    INTEGER(HSIZE_T), DIMENSION(ndims), INTENT(OUT) :: dims    
                                         ! Array containing sizes of
                                         ! chunk dimensions
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         !  number of chunk dimensions on success,
                                         !  -1 on failure
!*****
!            INTEGER, EXTERNAL :: h5pget_chunk_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_chunk_c(prp_id, ndims, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_CHUNK_C'::h5pget_chunk_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER :: ndims
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: dims
       END FUNCTION h5pget_chunk_c
    END INTERFACE

    hdferr =  h5pget_chunk_c(prp_id, ndims, dims)
  END SUBROUTINE h5pget_chunk_f

!****s* H5P/h5pset_deflate_f 
! NAME
!   h5pset_deflate_f 
!
! PURPOSE
!   Sets compression method and compression level. 
!
! INPUTS
!   prp_id  - property list identifier
!   level   - compression level
! OUTPUTS
!  
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1
!
! AUTHOR
!	Elena Pourmal
!		August 12, 1999
!
! HISTORY
! 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 14, 2001
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_deflate_f(prp_id, level, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: level         ! Compression level 
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!  INTEGER, EXTERNAL :: h5pset_deflate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_deflate_c(prp_id, level)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_DEFLATE_C'::h5pset_deflate_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: level
       END FUNCTION h5pset_deflate_c
    END INTERFACE
    hdferr = h5pset_deflate_c(prp_id, level)
    
  END SUBROUTINE h5pset_deflate_f

!****s* H5P/h5pget_version_f 
! NAME
!  h5pget_version_f 
!
! PURPOSE
!  Retrieves the version information of various objects 
!  for a file creation property list
!
! INPUTS
!  prp_id	- file createion property list identifier
! OUTPUTS
!  boot		- super block version number
!  freelist	- global freelist version number
!  stab		- symbol table version number
!  shhdr	- shared object header version number
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:          
  SUBROUTINE h5pget_version_f(prp_id, boot, freelist, &
       stab, shhdr, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id        ! Property list identifier 
    INTEGER, DIMENSION(:), INTENT(OUT) :: boot  ! Array to put boot
                                                ! block version number
    INTEGER, DIMENSION(:), INTENT(OUT) :: freelist  ! Array to put global
                                                    ! Freelist version number

    INTEGER, DIMENSION(:), INTENT(OUT) :: stab  ! Array to put symbol
                                                ! table version number
    INTEGER, DIMENSION(:), INTENT(OUT) :: shhdr ! Array to put shared
                                                ! object header version number
    INTEGER, INTENT(OUT) :: hdferr              ! Error code
                                                ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pget_version_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_version_c(prp_id, boot, freelist, stab, shhdr)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_VERSION_C'::h5pget_version_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, DIMENSION(:), INTENT(OUT) :: boot 
         INTEGER, DIMENSION(:), INTENT(OUT) :: freelist 
         INTEGER, DIMENSION(:), INTENT(OUT) :: stab
         INTEGER, DIMENSION(:), INTENT(OUT) :: shhdr
       END FUNCTION h5pget_version_c
    END INTERFACE
    
    hdferr = h5pget_version_c(prp_id, boot, freelist, stab, shhdr)
  END SUBROUTINE h5pget_version_f

!****s* H5P/h5pset_userblock_f 
! NAME
!  h5pset_userblock_f 
!
! PURPOSE
!   Sets user block size
!
! INPUTS
!   prp_id - file creation property list to modify
!   size   - size of the user-block in bytes
!
! OUTPUTS
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!
! Fortran90 Interface:
  SUBROUTINE h5pset_userblock_f (prp_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER(HSIZE_T), INTENT(IN) :: size ! Size of the user-block in bytes 
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_userblock_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_userblock_c(prp_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_USERBLOCK_C'::h5pset_userblock_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(IN) :: size
       END FUNCTION h5pset_userblock_c
    END INTERFACE

    hdferr = h5pset_userblock_c(prp_id, size)
  END SUBROUTINE h5pset_userblock_f

!****s* H5P/h5pget_userblock_f 
! NAME
!  h5pget_userblock_f 
!
! PURPOSE
!  Gets user block size.
!
! INPUTS
!  
!  prp_id	- file creation property list identifier
! OUTPUTS
!  
!  block_size	- size of the user block in bytes
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pget_userblock_f(prp_id, block_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id         ! Property list identifier 
    INTEGER(HSIZE_T), INTENT(OUT) ::  block_size ! Size of the 
                                                 ! user-block in bytes 
    INTEGER, INTENT(OUT) :: hdferr               ! Error code
                                                 ! 0 on success and -1 on failure
!*****
!            INTEGER, EXTERNAL :: h5pget_userblock_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_userblock_c(prp_id, block_size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_USERBLOCK_C'::h5pget_userblock_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(OUT) :: block_size
       END FUNCTION h5pget_userblock_c
    END INTERFACE
    hdferr = h5pget_userblock_c(prp_id,  block_size)
  END SUBROUTINE h5pget_userblock_f

!****s* H5P/h5pset_sizes_f 
! NAME
!  h5pset_sizes_f 
!
! PURPOSE
!  Sets the byte size of the offsets and lengths used 
!  to address objects in an HDF5 file.
!
! INPUTS
!  prp_id	- file creation property list identifier
!  sizeof_addr	- size of an object offset in bytes 
!  sizeof_size	- size of an object length in bytes
! OUTPUTS
!  
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_sizes_f (prp_id, sizeof_addr, sizeof_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id       ! Property list identifier 
    INTEGER(SIZE_T), INTENT(IN) :: sizeof_addr ! Size of an object 
                                               !  offset in bytes 
    INTEGER(SIZE_T), INTENT(IN) :: sizeof_size ! Size of an object 
                                               !  length in bytes 
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
                                               ! 0 on success and -1 on failure
!*****
!            INTEGER, EXTERNAL :: h5pset_sizes_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_sizes_c(prp_id, sizeof_addr, sizeof_size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_SIZES_C'::h5pset_sizes_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id 
         INTEGER(SIZE_T), INTENT(IN) :: sizeof_addr
         INTEGER(SIZE_T), INTENT(IN) :: sizeof_size
       END FUNCTION h5pset_sizes_c
    END INTERFACE
    
    hdferr = h5pset_sizes_c(prp_id, sizeof_addr, sizeof_size)
  END SUBROUTINE h5pset_sizes_f

!****s* H5P/h5pget_sizes_f 
! NAME
!  h5pget_sizes_f 
!
! PURPOSE
!  Retrieves the size of the offsets and lengths used 
!  in an HDF5 file
!
! INPUTS
!  prp_id	- file creation property list identifier
! OUTPUTS
!  
!  sizeof_addr	- size of an object offset in bytes 
!  sizeof_size	- size of an object length in bytes
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pget_sizes_f(prp_id, sizeof_addr, sizeof_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id        ! Property list identifier 
    INTEGER(SIZE_T), INTENT(OUT) :: sizeof_addr ! Size of an object
                                                ! offset in bytes 
    INTEGER(SIZE_T), INTENT(OUT) :: sizeof_size ! Size of an object
                                                ! length in bytes 
    INTEGER, INTENT(OUT) :: hdferr              ! Error code
                                                ! 0 on success and -1 on failure
!*****
!            INTEGER, EXTERNAL :: h5pget_sizes_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_sizes_c(prp_id, sizeof_addr, sizeof_size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_SIZES_C'::h5pget_sizes_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id 
         INTEGER(SIZE_T), INTENT(OUT) :: sizeof_addr
         INTEGER(SIZE_T), INTENT(OUT) :: sizeof_size
       END FUNCTION h5pget_sizes_c
    END INTERFACE
    
    hdferr = h5pget_sizes_c(prp_id, sizeof_addr, sizeof_size)
  END SUBROUTINE h5pget_sizes_f

!****s* H5P/h5pset_sym_k_f 
! NAME
!  h5pset_sym_k_f 
!
! PURPOSE
!  Sets the size of parameters used to control the 
!symbol table nodes
!
! INPUTS
!  
!  prp_id  - file creation property list identifier
!  ik	   - symbol table tree rank
!  lk	   - symbol table node size
! OUTPUTS
!  
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_sym_k_f (prp_id, ik, lk, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: ik            ! Symbol table tree rank 
    INTEGER, INTENT(IN) :: lk            ! Symbol table node size 
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_sym_k_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_sym_k_c(prp_id, ik, lk)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_SYM_K_C'::h5pset_sym_k_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id 
         INTEGER, INTENT(IN) :: ik
         INTEGER, INTENT(IN) :: lk
       END FUNCTION h5pset_sym_k_c
    END INTERFACE
  
    hdferr = h5pset_sym_k_c(prp_id, ik, lk)
  END SUBROUTINE h5pset_sym_k_f
!****s* H5P/h5pget_sym_k_f 
! NAME
!  h5pget_sym_k_f 
!
! PURPOSE
!  Retrieves the size of the symbol table B-tree 1/2 rank
!  and the symbol table leaf node 1/2 size. 
!
! INPUTS
!  
!  prp_id	- file creation property list identifier
! OUTPUTS
!  
!  ik		- symbol table tree 1/2 rank
!  lk		- symbol table node 1/2 size
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pget_sym_k_f(prp_id, ik, lk, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: ik           ! Symbol table tree rank
    INTEGER, INTENT(OUT) :: lk           ! Symbol table node size
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pget_sym_k_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_sym_k_c(prp_id, ik, lk)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_SYM_K_C'::h5pget_sym_k_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id 
         INTEGER, INTENT(OUT) :: ik
         INTEGER, INTENT(OUT) :: lk
       END FUNCTION h5pget_sym_k_c
    END INTERFACE
    
    hdferr = h5pget_sym_k_c(prp_id, ik, lk)
  END SUBROUTINE h5pget_sym_k_f
!****s* H5P/h5pset_istore_k_f 
! NAME
!  h5pset_istore_k_f 
!
! PURPOSE
!  Sets the size of the parameter used to control the 
!  B-trees for indexing chunked datasets
!
! INPUTS
!  
!  prp_id	- file creation property list identifier
!  ik		- 1/2 rank of chunked storage B-tree
! OUTPUTS
!  
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_istore_k_f (prp_id, ik, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: ik            ! 1/2 rank of chunked storage B-tree
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_istore_k_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_istore_k_c(prp_id, ik)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_ISTORE_K_C'::h5pset_istore_k_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: ik
       END FUNCTION h5pset_istore_k_c
    END INTERFACE
    
    hdferr = h5pset_istore_k_c(prp_id, ik)
  END SUBROUTINE h5pset_istore_k_f

!****s* H5P/h5pget_istore_k_f 
! NAME
!  h5pget_istore_k_f 
!
! PURPOSE
!  Queries the 1/2 rank of an indexed storage B-tree. 
!
! INPUTS
!  
!  prp_id	- file creation property list identifier
! OUTPUTS
!  
!  ik		- 1/2 rank of chunked storage B-tree
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pget_istore_k_f(prp_id, ik, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: ik           ! 1/2 rank of chunked storage B-tree
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pget_istore_k_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_istore_k_c(prp_id, ik)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_ISTORE_K_C'::h5pget_istore_k_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: ik
       END FUNCTION h5pget_istore_k_c
    END INTERFACE
    
    hdferr = h5pget_istore_k_c(prp_id, ik)
  END SUBROUTINE h5pget_istore_k_f

!****s* H5P/h5pget_driver_f 
! NAME
!  h5pget_driver_f 
!
! PURPOSE
!  Returns low-lever driver identifier. 
!
! INPUTS
!  
!  prp_id  - file access or data transfer property 
!	     list identifier. 
! OUTPUTS
!  
!  driver  - low-level driver identifier
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pget_driver_f(prp_id, driver, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    INTEGER(HID_T), INTENT(OUT) :: driver ! Low-level file driver identifier
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!*****
!            INTEGER, EXTERNAL :: h5pget_driver_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_driver_c(prp_id, driver)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_DRIVER_C'::h5pget_driver_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(OUT) :: driver
       END FUNCTION h5pget_driver_c
    END INTERFACE
    
    hdferr = h5pget_driver_c(prp_id, driver)
  END SUBROUTINE h5pget_driver_f

!****s* H5P/h5pset_fapl_stdio_f 
! NAME
!  h5pset_fapl_stdio_f 
!
! PURPOSE
!  Sets the standard I/O driver. 
!
! INPUTS
!  
!  prp_id  - file access property list identifier
! OUTPUTS
!  
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_fapl_stdio_f (prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****
    
!            INTEGER, EXTERNAL :: h5pset_fapl_stdio_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_stdio_c(prp_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FAPL_STDIO_C'::h5pset_fapl_stdio_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
       END FUNCTION h5pset_fapl_stdio_c
    END INTERFACE
    
    hdferr = h5pset_fapl_stdio_c(prp_id)
  END SUBROUTINE h5pset_fapl_stdio_f

!****s* H5P/h5pget_stdio_f 
! NAME
!  h5pget_stdio_f 
!
! PURPOSE
!  NOT AVAILABLE
!
! INPUTS
!  
! OUTPUTS
!  
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! SOURCE
!          SUBROUTINE h5pget_stdio_f (prp_id, io, hdferr)
!
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
!            INTEGER, INTENT(OUT) :: io   ! value indicates that the file
                                         !access property list is set to
                                         !the stdio driver
!            INTEGER, INTENT(OUT) :: hdferr  ! Error code
                                     ! 0 on success and -1 on failure
!*****
!            INTEGER, EXTERNAL :: h5pget_stdio_c
!            hdferr = h5pget_stdio_c(prp_id, io)
!          END SUBROUTINE h5pget_stdio_f

!****s* H5P/h5pset_fapl_sec2_f 
! NAME
!  h5pset_fapl_sec2_f 
!
! PURPOSE
!  Sets the sec2 driver. 
!
! INPUTS
!  
!  prp_id  - file access property list identifier
! OUTPUTS
!  
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_fapl_sec2_f (prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_fapl_sec2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_sec2_c(prp_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FAPL_SEC2_C'::h5pset_fapl_sec2_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
       END FUNCTION h5pset_fapl_sec2_c
    END INTERFACE
  
    hdferr = h5pset_fapl_sec2_c(prp_id)
  END SUBROUTINE h5pset_fapl_sec2_f

!****s* H5P/h5pget_sec2_f 
! NAME
!   h5pget_sec2_f 
!
! PURPOSE
!  NOT AVAILABLE
!
! INPUTS
!  
! OUTPUTS
!  
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! SOURCE!          SUBROUTINE h5pget_sec2_f (prp_id, sec2, hdferr) 
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
!            INTEGER, INTENT(OUT) :: sec2   ! value indicates whether the file
                                           !driver uses the functions declared
                                           !in the unistd.h file
!            INTEGER, INTENT(OUT) :: hdferr  ! Error code
                                     ! 0 on success and -1 on failure
!*****
!            INTEGER, EXTERNAL :: h5pget_sec2_c
!            hdferr = h5pget_sec2_c(prp_id, sec2)
!          END SUBROUTINE h5pget_sec2_f

!****s* H5P/h5pset_alignment_f 
! NAME
!  h5pset_alignment_f 
!
! PURPOSE
!  Sets alignment properties of a file access property list. 
!
! INPUTS
!  
!  prp_id	- file access property list identifier
!  threshold	- threshold value	
!  alignment	- alignment value
! OUTPUTS
!  
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_alignment_f(prp_id, threshold,  alignment, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id      ! Property list identifier 
    INTEGER(HSIZE_T), INTENT(IN) :: threshold ! Threshold value
    INTEGER(HSIZE_T), INTENT(IN) :: alignment ! alignment value
    INTEGER, INTENT(OUT) :: hdferr            ! Error code
                                              ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_alignment_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_alignment_c(prp_id, threshold, alignment)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_ALIGNMENT_C'::h5pset_alignment_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(IN) :: threshold
         INTEGER(HSIZE_T), INTENT(IN) :: alignment
       END FUNCTION h5pset_alignment_c
    END INTERFACE
  
    hdferr = h5pset_alignment_c(prp_id, threshold, alignment)
  END SUBROUTINE h5pset_alignment_f

!****s* H5P/h5pget_alignment_f 
! NAME
!   h5pget_alignment_f 
!
! PURPOSE
!  Retrieves the current settings for alignment 
!  properties from a file access property list. 
!
! INPUTS
!  prp_id       - file access property list identifier
!
! OUTPUTS
!  threshold	- threshold value	
!  alignment	- alignment value
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pget_alignment_f(prp_id, threshold,  alignment, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id       ! Property list identifier 
    INTEGER(HSIZE_T), INTENT(OUT) :: threshold ! Threshold value
    INTEGER(HSIZE_T), INTENT(OUT) :: alignment ! alignment value
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
                                             ! 0 on success and -1 on failure
!*****
!            INTEGER, EXTERNAL :: h5pget_alignment_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_alignment_c(prp_id, threshold, alignment)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_ALIGNMENT_C'::h5pget_alignment_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(OUT) :: threshold
         INTEGER(HSIZE_T), INTENT(OUT) :: alignment
       END FUNCTION h5pget_alignment_c
    END INTERFACE
    
    hdferr = h5pget_alignment_c(prp_id, threshold, alignment)
  END SUBROUTINE h5pget_alignment_f

!****s* H5P/h5pset_fapl_core_f 
! NAME
!   h5pset_fapl_core_f 
!
! PURPOSE
!  Modifies the file access property list to use the 
!  H5FD_CORE driver. 
!
! INPUTS
!  prp_id	    - file access property list identifier
!  increment	    - size, in bytes, of memory increments 
!  backing_store    - boolean flag indicating whether to write 
!		      the file contents to disk when the file is closed. 
! OUTPUTS
!  hdferr           - error code		
!	                Success:  0
!	                Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_fapl_core_f(prp_id, increment, backing_store, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id     ! Property list identifier 
    INTEGER(SIZE_T), INTENT(IN) :: increment ! File block size in bytes.
    LOGICAL, INTENT(IN) :: backing_store ! Flag to indicate that
                                         ! entire file contents are flushed to a file 
                                         ! with the same name as this core file.
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****
    INTEGER :: backing_store_flag 

!            INTEGER, EXTERNAL :: h5pset_fapl_core_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_core_c(prp_id, increment, backing_store_flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FAPL_CORE_C'::h5pset_fapl_core_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id 
         INTEGER(SIZE_T), INTENT(IN) :: increment 
         INTEGER :: backing_store_flag 
       END FUNCTION h5pset_fapl_core_c
    END INTERFACE
    backing_store_flag = 0
    IF(backing_store) backing_store_flag = 1
    hdferr = h5pset_fapl_core_c(prp_id, increment, backing_store_flag)
  END SUBROUTINE h5pset_fapl_core_f

!****s* H5P/h5pget_fapl_core_f 
! NAME
!   h5pget_fapl_core_f 
!
! PURPOSE
!  Queries core file driver properties. 
!
! INPUTS
!  prp_id	 - file access property list identifier
! OUTPUTS
!  
!  increment	 - size, in bytes, of memory increments 
!  backing_store - boolean flag indicating whether to write 
!		   the file contents to disk when the file is closed. 
!  hdferr        - error code		
!	            Success:  0
!	            Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pget_fapl_core_f(prp_id, increment, backing_store, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id      ! Property list identifier 
    INTEGER(SIZE_T), INTENT(OUT) :: increment ! File block size in bytes.
    LOGICAL, INTENT(OUT) :: backing_store   ! Flag to indicate that
                                            ! entire file contents are flushed to a file 
                                            ! with the same name as this core file.
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
                                            ! 0 on success and -1 on failure
!*****
    INTEGER :: backing_store_flag 

!            INTEGER, EXTERNAL :: h5pget_fapl_core_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_fapl_core_c(prp_id, increment, backing_store_flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FAPL_CORE_C'::h5pget_fapl_core_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id 
         INTEGER(SIZE_T), INTENT(OUT) :: increment 
         INTEGER :: backing_store_flag 
       END FUNCTION h5pget_fapl_core_c
    END INTERFACE
    
    hdferr = h5pget_fapl_core_c(prp_id, increment, backing_store_flag)
    backing_store =.FALSE.
    IF (backing_store_flag .EQ. 1) backing_store =.TRUE.
  END SUBROUTINE h5pget_fapl_core_f

!****s* H5P/ h5pset_fapl_family_f 
! NAME
!   h5pset_fapl_family_f 
!
! PURPOSE
!  Sets the file access property list to use the family driver. 
!
! INPUTS
!  prp_id	- file access property list identifier
!  memb_size	- size in bytes of each file member 
!  memb_plist	- identifier of the file access property 
!		  list to be used for each family member
! OUTPUTS
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_fapl_family_f(prp_id, memb_size, memb_plist , hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id      ! Property list identifier 
    INTEGER(HSIZE_T), INTENT(IN) :: memb_size ! Logical size, in bytes,
                                              ! of each family member
    INTEGER(HID_T), INTENT(IN) :: memb_plist  ! Identifier of the file 
                                              ! access property list for 
                                              ! each member of the family
    INTEGER, INTENT(OUT) :: hdferr            ! Error code
                                              ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_fapl_family_f
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_family_c(prp_id, memb_size, memb_plist)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FAPL_FAMILY_C'::h5pset_fapl_family_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(IN) :: memb_size
         INTEGER(HID_T), INTENT(IN) :: memb_plist
       END FUNCTION h5pset_fapl_family_c
    END INTERFACE

    hdferr = h5pset_fapl_family_c(prp_id, memb_size, memb_plist)
  END SUBROUTINE h5pset_fapl_family_f

!****s* H5P/h5pget_fapl_family_f 
! NAME
!  h5pget_fapl_family_f 
!
! PURPOSE
!  Returns file access property list information.  	
!
! INPUTS
!  prp_id	- file access property list identifier
! OUTPUTS
!  memb_size	- size in bytes of each file member 
!  memb_plist	- identifier of the file access property 
!		  list to be used for each family member
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pget_fapl_family_f(prp_id, memb_size, memb_plist , hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id       ! Property list identifier 
    INTEGER(HSIZE_T), INTENT(OUT) :: memb_size ! Logical size, in bytes,
                                               ! of each family member
    INTEGER(HID_T), INTENT(OUT) :: memb_plist  ! Identifier of the file 
                                               ! access property list for 
                                               ! each member of the family
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
                                               ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pget_fapl_family_f
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_fapl_family_c(prp_id, memb_size, memb_plist)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FAPL_FAMILY_C'::h5pget_fapl_family_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(OUT) :: memb_size
         INTEGER(HID_T), INTENT(OUT) :: memb_plist
       END FUNCTION h5pget_fapl_family_c
    END INTERFACE
    
    hdferr = h5pget_fapl_family_c(prp_id, memb_size, memb_plist)
  END SUBROUTINE h5pget_fapl_family_f

!****s* H5P/h5pset_cache_f 
! NAME
!   h5pset_cache_f 
!
! PURPOSE
!  Sets the meta data cache and raw data chunk 
!  cache parameters
!
! INPUTS
!  
!  prp_id	- file access property list identifier
!  mdc_nelmts	- number of elements (objects) in the meta 
!		  data cache 
!  rdcc_nelmts	- number of elements (objects) in the raw 
!		  data chunk cache 
!  rdcc_nbytes	- total size of the raw data chunk cache, in bytes 
!  rdcc_w0	- preemption policy (0 or 1)
! OUTPUTS
!  
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_cache_f(prp_id, mdc_nelmts,rdcc_nelmts, rdcc_nbytes, rdcc_w0, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: mdc_nelmts    ! Number of elements (objects)
                                         !  in the meta data cache
    INTEGER(SIZE_T), INTENT(IN) :: rdcc_nelmts ! Number of elements (objects)
                                               !  in the meta data cache
    INTEGER(SIZE_T), INTENT(IN) :: rdcc_nbytes ! Total size of the raw data 
                                               !  chunk cache, in bytes 
    REAL, INTENT(IN) :: rdcc_w0                ! Preemption policy
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
                                               !  0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_cache_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_cache_c(prp_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_CACHE_C'::h5pset_cache_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: mdc_nelmts 
         INTEGER(SIZE_T), INTENT(IN) :: rdcc_nelmts 
         INTEGER(SIZE_T), INTENT(IN) :: rdcc_nbytes
         REAL, INTENT(IN) :: rdcc_w0
       END FUNCTION h5pset_cache_c
    END INTERFACE

    hdferr = h5pset_cache_c(prp_id, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0 )
  END SUBROUTINE h5pset_cache_f

!****s* H5P/h5pget_cache_f 
! NAME
!   h5pget_cache_f 
!
! PURPOSE
!  Queries the meta data cache and raw data chunk cache 
!  parameters.  
!
! INPUTS
!  prp_id	- file access property list identifier
!
! OUTPUTS
!  mdc_nelmts	- number of elements (objects) in the meta 
!		  data cache 
!  rdcc_nelmts	- number of elements (objects) in the raw 
!		  data chunk cache 
!  rdcc_nbytes	- total size of the raw data chunk cache, in bytes 
!  rdcc_w0	- preemption policy (0 or 1)
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!
!  Bug fix: type of the rdcc_nelmts parameter should be INTEGER
!  instead of INTEGER(SIZE_T) October 10, 2003 
!		
! Fortran90 Interface:
  SUBROUTINE h5pget_cache_f(prp_id, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: mdc_nelmts   ! Number of elements (objects)
                                         !  in the meta data cache
    INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nelmts  ! Number of elements (objects)
                                                 !  in the meta data cache
    INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nbytes  ! Total size of the raw data 
                                                 !  chunk cache, in bytes 
    REAL, INTENT(OUT) :: rdcc_w0                 ! Preemption policy
    INTEGER, INTENT(OUT) :: hdferr               ! Error code
                                                 ! 0 on success and -1 on failure
!*****
!            INTEGER, EXTERNAL :: h5pget_cache_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_cache_c(prp_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_CACHE_C'::h5pget_cache_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: mdc_nelmts 
         INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nelmts 
         INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nbytes
         REAL, INTENT(OUT) :: rdcc_w0
       END FUNCTION h5pget_cache_c
    END INTERFACE
    
    hdferr = h5pget_cache_c(prp_id, mdc_nelmts,rdcc_nelmts, rdcc_nbytes, rdcc_w0 )
  END SUBROUTINE h5pget_cache_f

!****s* H5P/h5pset_fapl_split_f 
! NAME
!   h5pset_fapl_split_f 
!
! PURPOSE
!  Emulates the old split file driver. 
!
! INPUTS
!  
!  prp_id	- file access property list identifier
!  meta_ext	- name of the extension for the metafile 
!		  filename
!  meta_plist	- identifier of the meta file access property 
!		  list
!  raw_ext 	- name extension for the raw file filename
!  raw_plist	- identifier of the raw file access property list
!
! OUTPUTS
!  
!  hdferr       - error code		
!	            Success:  0
!	            Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pset_fapl_split_f(prp_id, meta_ext, meta_plist, raw_ext, raw_plist, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id     ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: meta_ext ! Name of the extension for
                                             !  the metafile filename
    INTEGER(HID_T), INTENT(IN) :: meta_plist ! Identifier of the meta file
                                             !  access property list
    CHARACTER(LEN=*), INTENT(IN) :: raw_ext  ! Name extension for the raw file filename
    INTEGER(HID_T), INTENT(IN) :: raw_plist  ! Identifier of the raw file 
                                             !  access property list
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
                                             ! 0 on success and -1 on failure
!*****
    INTEGER :: meta_len, raw_len

!            INTEGER, EXTERNAL :: h5pset_fapl_split_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_split_c(prp_id,meta_len,meta_ext,meta_plist,raw_len,raw_ext,raw_plist)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FAPL_SPLIT_C'::h5pset_fapl_split_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: meta_ext
         !DEC$ATTRIBUTES reference :: raw_ext
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: meta_ext 
         INTEGER(HID_T), INTENT(IN) :: meta_plist
         CHARACTER(LEN=*), INTENT(IN) :: raw_ext
         INTEGER(HID_T), INTENT(IN) :: raw_plist 
         INTEGER :: meta_len, raw_len
       END FUNCTION h5pset_fapl_split_c
    END INTERFACE

    meta_len = LEN(meta_ext)
    raw_len = LEN(raw_ext)
    hdferr = h5pset_fapl_split_c(prp_id,meta_len,meta_ext,meta_plist,raw_len,raw_ext,raw_plist)
  END SUBROUTINE h5pset_fapl_split_f

!****s* H5P/h5pget_split_f 
! NAME
!   h5pget_split_f 
!
! PURPOSE
!  NOT AVAILABLE
!
! INPUTS
!  
! OUTPUTS
!  
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! SOURCE
!          SUBROUTINE h5pget_split_f(prp_id, meta_ext_size, meta_ext, meta_plist,raw_ext_size,&
!                                     raw_ext, raw_plist, hdferr)
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
!            INTEGER(SIZE_T), INTENT(IN) :: meta_ext_size ! Number of characters of the meta
                                                         ! file extension to be copied to the
                                                         ! meta_ext buffer

!            CHARACTER(LEN=*), INTENT(OUT) :: meta_ext  !Name of the extension for
                                                      !the metafile filename
!            INTEGER(HID_T), INTENT(OUT) :: meta_plist  ! Identifier of the meta file
                                                      ! access property list
!            INTEGER(SIZE_T), INTENT(IN) :: raw_ext_size ! Number of characters of the raw
                                                         ! file extension to be copied to the
                                                         ! raw_ext buffer
!            CHARACTER(LEN=*), INTENT(OUT) :: raw_ext  !Name extension for the raw file filename
!            INTEGER(HID_T), INTENT(OUT) :: raw_plist  !Identifier of the raw file
                                                     !access property list
!            INTEGER, INTENT(OUT) :: hdferr  ! Error code
                                     ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pget_split_c
!            hdferr = h5pget_split_c(prp_id, meta_ext_size, meta_ext, meta_plist, &
!                                    raw_ext_size, raw_ext, raw_plist )
!          END SUBROUTINE h5pget_split_f

!****s* H5P/h5pset_gc_references_f 
! NAME
!   h5pset_gc_references_f 
!
! PURPOSE
!  Sets garbage collecting references flag. 
!
! INPUTS
!  
!  prp_id	- file access property list identifier
!  gc_reference	- flag for stting garbage collection on 
!		  and off (1 or 0)
! OUTPUTS
!  
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface: 
  SUBROUTINE h5pset_gc_references_f (prp_id, gc_reference, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: gc_reference  ! The flag for garbage collecting
                                         !  references for the file
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_gc_references_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_gc_references_c(prp_id, gc_reference)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_GC_REFERENCES_C'::h5pset_gc_references_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: gc_reference
       END FUNCTION h5pset_gc_references_c
    END INTERFACE

    hdferr = h5pset_gc_references_c(prp_id, gc_reference)
  END SUBROUTINE h5pset_gc_references_f

!****s* H5P/h5pget_gc_references_f 
! NAME
!   h5pget_gc_references_f 
!
! PURPOSE
!  Returns garbage collecting references setting. 	
!
! INPUTS
!  
!  prp_id	- file access property list identifier
! OUTPUTS
!  
!  gc_reference	- flag for stting garbage collection on 
!		  and off (1 or 0)
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!		
! Fortran90 Interface:
  SUBROUTINE h5pget_gc_references_f(prp_id, gc_reference, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: gc_reference ! The flag for garbage collecting
                                         !  references for the file
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pget_gc_references_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_gc_references_c(prp_id, gc_reference)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_GC_REFERENCES_C'::h5pget_gc_references_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: gc_reference
       END FUNCTION h5pget_gc_references_c
    END INTERFACE
    
    hdferr = h5pget_gc_references_c(prp_id, gc_reference)
  END SUBROUTINE h5pget_gc_references_f

!****s* H5P/h5pset_layout_f 
! NAME
!   h5pset_layout_f 
!
! PURPOSE
!  Sets the type of storage used store the raw data 
!  for a dataset. 
!
! INPUTS
!  
!  prp_id	- data creation property list identifier
!  layout	- type of storage layout for raw data
!  		  possible values are:
!  		    H5D_COMPACT_F
!  		    H5D_CONTIGUOUS_F
!  		    H5D_CHUNKED_F
! OUTPUTS
!  
!  hdferr      - error code		
!	          Success:  0
!	          Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!  
! Fortran90 Interface:
  SUBROUTINE h5pset_layout_f (prp_id, layout, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: layout        ! Type of storage layout for raw data
                                         ! possible values are:
                                         !   H5D_COMPACT_F
                                         !   H5D_CONTIGUOUS_F
                                         !   H5D_CHUNKED_F
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_layout_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_layout_c(prp_id, layout)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_LAYOUT_C'::h5pset_layout_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: layout
       END FUNCTION h5pset_layout_c
    END INTERFACE
    
    hdferr = h5pset_layout_c(prp_id, layout)
  END SUBROUTINE h5pset_layout_f

!****s* H5P/h5pget_layout_f 
! NAME
!   h5pget_layout_f 
!
! PURPOSE
!  Returns the layout of the raw data for a dataset. 
!
! INPUTS
!  
!  prp_id	- data creation property list identifier
! OUTPUTS
!  
!  layout	- type of storage layout for raw data
!  		  possible values are:
!  		   H5D_COMPACT_F
!  		   H5D_CONTIGUOUS_F
!  		   H5D_CHUNKED_F
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!  
! Fortran90 Interface:
  SUBROUTINE h5pget_layout_f (prp_id, layout, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: layout       ! Type of storage layout for raw data
                                         ! possible values are:
                                         !  H5D_COMPACT_F(0)
                                         !  H5D_CONTIGUOUS_F(1)
                                         !  H5D_CHUNKED_F(2)
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pget_layout_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_layout_c(prp_id, layout)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_LAYOUT_C'::h5pget_layout_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: layout
       END FUNCTION h5pget_layout_c
    END INTERFACE
    
    hdferr = h5pget_layout_c(prp_id, layout)
  END SUBROUTINE h5pget_layout_f

!****s* H5P/h5pset_filter_f 
! NAME
!  h5pset_filter_f 
!
! PURPOSE
!  Adds a filter to the filter pipeline. 
!
! INPUTS
!  
!  prp_id	- data creation or transfer property list 
!  		  identifier
!  filter	- filter to be added to the pipeline 
!  flags	- bit vector specifying certain general
!  		  properties of the filter
!  cd_nelmts	- number of elements in cd_values
!  cd_values	- auxiliary data for the filter
! OUTPUTS
!  
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  February, 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pset_filter_f(prp_id, filter, flags, cd_nelmts, cd_values,  hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: filter        ! Filter to be added to the pipeline.
    INTEGER, INTENT(IN) :: flags         ! Bit vector specifying certain general
                                       !  properties of the filter.
    INTEGER(SIZE_T), INTENT(IN) :: cd_nelmts       ! Number of elements in cd_values.
    INTEGER, DIMENSION(*), INTENT(IN) :: cd_values ! Auxiliary data for the filter.
    INTEGER, INTENT(OUT) :: hdferr                 ! Error code
                                                 ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_filter_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_filter_c(prp_id, filter, flags, cd_nelmts, cd_values)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FILTER_C'::h5pset_filter_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id 
         INTEGER, INTENT(IN) :: filter 
         INTEGER, INTENT(IN) :: flags 
         INTEGER(SIZE_T), INTENT(IN) :: cd_nelmts 
         INTEGER, DIMENSION(*), INTENT(IN) :: cd_values 
       END FUNCTION h5pset_filter_c
    END INTERFACE
  
    hdferr = h5pset_filter_c(prp_id, filter, flags, cd_nelmts, cd_values )
  END SUBROUTINE h5pset_filter_f

!****s* H5P/h5pget_nfilters_f 
! NAME
!   h5pget_nfilters_f 
!
! PURPOSE
!  Returns the number of filters in the pipeline. 
!
! INPUTS
!  
!  prp_id	- data creation or transfer property list 
!  		  identifier
! OUTPUTS
!  
!  nfilters	- number of filters in the pipeline
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!  
! Fortran90 Interface:
  SUBROUTINE h5pget_nfilters_f (prp_id, nfilters, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: nfilters     ! The number of filters in the pipeline
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pget_nfilters_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_nfilters_c(prp_id, nfilters)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_NFILTERS_C'::h5pget_nfilters_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: nfilters
       END FUNCTION h5pget_nfilters_c
    END INTERFACE
    
    hdferr = h5pget_nfilters_c(prp_id, nfilters)
  END SUBROUTINE h5pget_nfilters_f

!****s* H5P/h5pget_filter_f 
! NAME
!   h5pget_filter_f 
!
! PURPOSE
!  Returns information about a filter in a pipeline
!
! INPUTS
!  
!  prp_id	 - data creation or transfer property list 
!  		   identifier
!  filter_number - sequence number within the filter
!                  pipeline of the filter for which 
!                  information is sought
! OUTPUTS
!  
!  filter_id	- filter identification number
!  flags	- bit vector specifying certain general
!  		  properties of the filter
!  cd_nelmts	- number of elements in cd_values
!  cd_values	- auxiliary data for the filter
!  namelen	- number of characters in the name buffer
!  name		- buffer to retrieve filter name
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!  
! Fortran90 Interface:
  SUBROUTINE h5pget_filter_f(prp_id, filter_number, flags, cd_nelmts, cd_values, namelen, name, filter_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: filter_number ! Sequence number within the filter
                                         !  pipeline of the filter for which 
                                         !  information is sought
    INTEGER, DIMENSION(*), INTENT(OUT) :: cd_values  ! Auxiliary data for the filter.
    INTEGER, INTENT(OUT) :: flags        ! Bit vector specifying certain general
                                         !  properties of the filter.
    INTEGER(SIZE_T), INTENT(INOUT) :: cd_nelmts  ! Number of elements in cd_values.
    INTEGER(SIZE_T), INTENT(IN) :: namelen       ! Anticipated number of characters in name.
    CHARACTER(LEN=*), INTENT(OUT) :: name        ! Name of the filter
    INTEGER, INTENT(OUT) :: filter_id            ! Filter identification number  
    INTEGER, INTENT(OUT) :: hdferr               ! Error code
                                                 ! 0 on success and -1 on failure
!*****


!            INTEGER, EXTERNAL :: h5pget_filter_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_filter_c(prp_id, filter_number, flags, cd_nelmts,  &
            cd_values, namelen, name, filter_id )
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FILTER_C'::h5pget_filter_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: filter_number 
         INTEGER, DIMENSION(*), INTENT(OUT) :: cd_values
         INTEGER, INTENT(OUT) :: flags 
         INTEGER(SIZE_T), INTENT(INOUT) :: cd_nelmts
         INTEGER(SIZE_T), INTENT(IN) :: namelen
         CHARACTER(LEN=*), INTENT(OUT) :: name
         INTEGER, INTENT(OUT) :: filter_id
       END FUNCTION h5pget_filter_c
    END INTERFACE
    
    hdferr = h5pget_filter_c(prp_id, filter_number, flags, cd_nelmts,  & 
         cd_values, namelen, name, filter_id )
  END SUBROUTINE h5pget_filter_f

!****s* H5P/h5pset_external_f 
! NAME
!   h5pset_external_f 
!
! PURPOSE
!  Adds an external file to the list of external files. 
!
! INPUTS
!   
!  prp_id	- dataset creation property list identifier
!  name		- name of external file
!  offset	- offset in bytes from the beginning of the 
!  		  file to the location in the file
!  		  where the data starts
!  bytes	- size of the external file data. 
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001
!
!  Changed type of 'offset' from integer to off_t -- MSB January 9, 2012
!  
! Fortran90 Interface:
  SUBROUTINE h5pset_external_f(prp_id, name, offset, bytes, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of an external file
    INTEGER(OFF_T), INTENT(IN) :: offset  ! Offset, in bytes, from the beginning 
                                          !  of the file to the location in the file 
                                          !  where the data starts.
    INTEGER(HSIZE_T), INTENT(IN) :: bytes ! Number of bytes reserved in the 
                                          !  file for the data
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!*****

    INTEGER :: namelen

    INTERFACE
       INTEGER FUNCTION h5pset_external_c(prp_id, name,namelen, offset, bytes)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_EXTERNAL_C'::h5pset_external_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(OFF_T), INTENT(IN) :: offset
         INTEGER(HSIZE_T), INTENT(IN) :: bytes
       END FUNCTION h5pset_external_c
    END INTERFACE
  
    namelen = LEN(name)
    hdferr = h5pset_external_c(prp_id, name, namelen, offset, bytes)
  END SUBROUTINE h5pset_external_f

!****s* H5P/h5pget_external_count_f 
! NAME
!   h5pget_external_count_f 
!
! PURPOSE
!  Returns the number of external files for a dataset. 
!
! INPUTS
!  
!  prp_id	- dataset creation property list identifier
! OUTPUTS
!  
!  count	- number of external files for the 
!  		  specified dataset
!  hdferr       - error code		
!	            Success:  0
!	            Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!  
! Fortran90 Interface:
  SUBROUTINE h5pget_external_count_f (prp_id, count, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: count        ! Number of external files for the 
                                         ! Specified dataset
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****
!            INTEGER, EXTERNAL :: h5pget_external_count_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_external_count_c(prp_id, count)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_EXTERNAL_COUNT_C'::h5pget_external_count_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id 
         INTEGER, INTENT(OUT) :: count
       END FUNCTION h5pget_external_count_c
    END INTERFACE
    
    hdferr = h5pget_external_count_c(prp_id, count)
  END SUBROUTINE h5pget_external_count_f

!****s* H5P/h5pget_external_f 
! NAME
!  h5pget_external_f 
!
! PURPOSE
!  Returns information about an external file. 
!
! INPUTS
!  
!  prp_id	- dataset creation property list identifier
! OUTPUTS
!  
!  idx		- external file index 
!  name_size	- maximum size of name array
!  name		- name of the external file	
!  name		- name of external file
!  offset	- offset in bytes from the beginning of the 
!  		  file to the location in the file
!  		  where the data starts
!  bytes	- size of the external file data
!  hdferr       - error code		
!	           Success:  0
!	            Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001
!
! Changed type of 'offset' from integer to off_t -- MSB January 9, 2012
!  
! Fortran90 Interface:
  SUBROUTINE h5pget_external_f(prp_id, idx, name_size, name, offset,bytes, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    INTEGER, INTENT(IN) :: idx           ! External file index.
    INTEGER(SIZE_T), INTENT(IN) :: name_size ! Maximum length of name array 
    CHARACTER(LEN=*), INTENT(OUT) :: name    ! Name of an external file
    INTEGER(OFF_T), INTENT(OUT) :: offset    ! Offset, in bytes, from the beginning 
                                             !  of the file to the location in the file 
                                             !  where the data starts.
    INTEGER(HSIZE_T), INTENT(OUT) :: bytes   ! Number of bytes reserved in the 
                                             !  file for the data
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
                                             ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pget_external_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_external_c(prp_id, idx, name_size, name, offset, bytes)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_EXTERNAL_C'::h5pget_external_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: idx 
         INTEGER(SIZE_T), INTENT(IN) :: name_size
         CHARACTER(LEN=*), INTENT(OUT) :: name
         INTEGER(OFF_T), INTENT(OUT) :: offset
         INTEGER(HSIZE_T), INTENT(OUT) :: bytes
       END FUNCTION h5pget_external_c
    END INTERFACE
    
    hdferr = h5pget_external_c(prp_id, idx, name_size, name, offset, bytes)
  END SUBROUTINE h5pget_external_f

!****s* H5P/h5pset_btree_ratios_f 
! NAME
!   h5pset_btree_ratios_f 
!
! PURPOSE
!  Sets B-tree split ratios for a dataset transfer 
!  property list. 
!
! INPUTS
!  	
!  prp_id	- the dataset transfer property list 
!  		  identifier 
!  left		- the B-tree split ratio for left-most nodes 
!  middle	- the B-tree split ratio for all other nodes
!  right	- the B-tree split ratio for right-most nodes
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!  
! Fortran90 Interface:
  SUBROUTINE h5pset_btree_ratios_f(prp_id, left, middle, right, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    REAL, INTENT(IN) :: left   ! The B-tree split ratio for left-most nodes.
    REAL, INTENT(IN) :: middle ! The B-tree split ratio for all other nodes 
    REAL, INTENT(IN) :: right  ! The B-tree split ratio for right-most 
                               !  nodes and lone nodes. 
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
                                    ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_btree_ratios_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION  h5pset_btree_ratios_c(prp_id, left, middle, right)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_BTREE_RATIOS_C'::h5pset_btree_ratios_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         REAL, INTENT(IN) :: left
         REAL, INTENT(IN) :: middle
         REAL, INTENT(IN) :: right
       END FUNCTION h5pset_btree_ratios_c
    END INTERFACE
    
    hdferr = h5pset_btree_ratios_c(prp_id, left, middle, right)
  END SUBROUTINE h5pset_btree_ratios_f

!****s* H5P/h5pget_btree_ratios_f
! NAME
!  h5pget_btree_ratios_f
!
! PURPOSE
!  Gets B-tree split ratios for a dataset transfer property list
!
! INPUTS
!  
!  prp_id	- the dataset transfer property list 
!  		  identifier 
! OUTPUTS
!  
!  left		- the B-tree split ratio for left-most nodes 
!  middle	- the B-tree split ratio for all other nodes
!  right	- the B-tree split ratio for right-most nodes
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999	
!
! HISTORY
!  Explicit Fortran interfaces were added for 
!  called C functions (it is needed for Windows
!  port).  March 14, 2001 
!  
! Fortran90 Interface:
  SUBROUTINE h5pget_btree_ratios_f(prp_id, left, middle, right, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    REAL, INTENT(OUT) :: left   ! The B-tree split ratio for left-most nodes.
    REAL, INTENT(OUT) :: middle ! The B-tree split ratio for all other nodes 
    REAL, INTENT(OUT) :: right  ! The B-tree split ratio for right-most 
                                !  nodes and lone nodes.
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
                                    ! 0 on success and -1 on failure
!*****


!            INTEGER, EXTERNAL :: h5pget_btree_ratios_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION  h5pget_btree_ratios_c(prp_id, left, middle, right)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_BTREE_RATIOS_C'::h5pget_btree_ratios_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         REAL, INTENT(OUT) :: left
         REAL, INTENT(OUT) :: middle
         REAL, INTENT(OUT) :: right
       END FUNCTION h5pget_btree_ratios_c
    END INTERFACE
  
    hdferr = h5pget_btree_ratios_c(prp_id, left, middle, right)
  END SUBROUTINE h5pget_btree_ratios_f

!****s* H5P/h5pget_fclose_degree_f 
! NAME
!  h5pget_fclose_degree_f 
!
! PURPOSE
!  Returns the degree for the file close behavior.
!
! INPUTS
!  
!  fapl_id	- File access property list identifier
! OUTPUTS
!  
!  degree  	- Possible values are:
!  		   H5F_CLOSE_DEFAULT_F
!  		   H5F_CLOSE_WEAK_F
!  		   H5F_CLOSE_SEMI_F
!  		   H5F_CLOSE_STRONG_F
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  September 26, 2002	
!
! HISTORY
! 
!  
! Fortran90 Interface:
  SUBROUTINE h5pget_fclose_degree_f(fapl_id, degree, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: fapl_id ! File Access Property list identifier 
    INTEGER, INTENT(OUT) :: degree        ! Possible values are: 
                                          !  H5F_CLOSE_DEFAULT_F
					  !  H5F_CLOSE_WEAK_F
					  !  H5F_CLOSE_SEMI_F
					  !  H5F_CLOSE_STRONG_F
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pget_fclose_degree_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_fclose_degree_c(fapl_id, degree)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FCLOSE_DEGREE_C'::h5pget_fclose_degree_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: fapl_id
         INTEGER, INTENT(OUT) :: degree
       END FUNCTION h5pget_fclose_degree_c
    END INTERFACE
    
    hdferr = h5pget_fclose_degree_c(fapl_id, degree) 
  END SUBROUTINE h5pget_fclose_degree_f

!****s* H5P/h5pset_fclose_degree_f 
! NAME
!  h5pset_fclose_degree_f 
!
! PURPOSE
!  Sets the degree for the file close behavior.
!
! INPUTS
!  
!  fapl_id	- file access property list identifier
!  degree  	- Possible values are:
!  		    H5F_CLOSE_DEFAULT_F
!  		    H5F_CLOSE_WEAK_F
!  		    H5F_CLOSE_SEMI_F
!  		    H5F_CLOSE_STRONG_F
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  September 26, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pset_fclose_degree_f(fapl_id, degree, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: fapl_id ! File Access Property list identifier 
    INTEGER, INTENT(IN) :: degree         ! Possible values are: 
                                          !  H5F_CLOSE_DEFAULT_F
					  !  H5F_CLOSE_WEAK_F
					  !  H5F_CLOSE_SEMI_F
					  !  H5F_CLOSE_STRONG_F
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pset_fclose_degree_c(fapl_id, degree)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FCLOSE_DEGREE_C'::h5pset_fclose_degree_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: fapl_id
         INTEGER, INTENT(IN) :: degree
       END FUNCTION h5pset_fclose_degree_c
    END INTERFACE

    hdferr = h5pset_fclose_degree_c(fapl_id, degree) 
  END SUBROUTINE h5pset_fclose_degree_f

!****s* H5P/h5pequal_f 
! NAME
!  h5pequal_f 
!
! PURPOSE
!  Checks if two property lists are eqaul
!
! INPUTS
!  
!  plist1_id	- property list identifier
!  plist2_id	- property list identifier
! OUTPUTS
!  
!  flag		- flag, possible values
!  		    .TRUE. or .FALSE.
!  hdferr:	- error code		
!  		   Success:  0
!  		   Failure: -1, flag is set to .FALSE.   
!
! AUTHOR
!  Elena Pourmal
!  September 30, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pequal_f(plist1_id, plist2_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist1_id ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: plist2_id ! Property list identifier 
    LOGICAL, INTENT(OUT)       :: flag      ! Flag
    INTEGER, INTENT(OUT)       :: hdferr    ! Error code
                                            !  0 on success and -1 on failure
!*****
    INTEGER                    :: c_flag
    
    INTERFACE
       INTEGER FUNCTION h5pequal_c(plist1_id, plist2_id, c_flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PEQUAL_C'::h5pequal_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist1_id
         INTEGER(HID_T), INTENT(IN) :: plist2_id
         INTEGER, INTENT(OUT) :: c_flag
       END FUNCTION h5pequal_c
    END INTERFACE

    flag = .FALSE.
    hdferr = h5pequal_c(plist1_id, plist2_id, c_flag) 
    IF (c_flag .GT. 0) flag = .TRUE.
  END SUBROUTINE h5pequal_f

!****s* H5P/h5pset_buffer_f
! NAME
!  h5pset_buffer_f 
!
! PURPOSE
!  Sets sixe for conversion buffer
!
! INPUTS
!  plist_id	- data transfer property list identifier
!  size		- buffer size 
! OUTPUTS
!  
!  hdferr:	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 2, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pset_buffer_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! Data transfer property list identifier 
    INTEGER(HSIZE_T), INTENT(IN) :: size   ! Buffer size in bytes; 
                                           ! buffer is allocated and freed by 
                                           ! the library.
    INTEGER, INTENT(OUT)       :: hdferr   ! Error code
                                           ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pset_buffer_c(plist_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_BUFFER_C'::h5pset_buffer_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(IN) :: size
       END FUNCTION h5pset_buffer_c
    END INTERFACE

    hdferr = h5pset_buffer_c(plist_id, size) 
  END SUBROUTINE h5pset_buffer_f

!****s* H5P/h5pget_buffer_f
! NAME
!  h5pget_buffer_f 
!
! PURPOSE
!  Gets size for conversion buffer
!
! INPUTS
!  
!  plist_id	- data transfer property list identifier
! OUTPUTS
!  
!  size		- buffer size 
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 2, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pget_buffer_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! Data transfer property list identifier 
    INTEGER(HSIZE_T), INTENT(OUT) :: size  ! Buffer size in bytes; 
                                           !  buffer is allocated and freed by 
                                           !  the library.
    INTEGER, INTENT(OUT)       :: hdferr   ! Error code
                                           ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pget_buffer_c(plist_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_BUFFER_C'::h5pget_buffer_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_buffer_c
    END INTERFACE

    hdferr = h5pget_buffer_c(plist_id, size) 
  END SUBROUTINE h5pget_buffer_f

!****s* H5P/h5pfill_value_defined_f
! NAME
!  h5pfill_value_defined_f
!
! PURPOSE
!  Check if fill value is defined.
!
! INPUTS
!  
!  plist_id	- dataset creation property list identifier
! OUTPUTS
!  
!  flag         - fill value status flag
!                 Possible values are:
!  		    H5D_FILL_VALUE_ERROR_F
!  		    H5D_FILL_VALUE_UNDEFINED_F
!  		    H5D_FILL_VALUE_DEFAULT_F
!  		    H5D_FILL_VALUE_USER_DEFINED_F
!  hdferr	- error code		
!  		    Success:  0
!  		    Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 4, 2002
!
! Fortran90 Interface:
  SUBROUTINE h5pfill_value_defined_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id  ! Dataset creation property list identifier
    INTEGER, INTENT(OUT) :: flag            ! Fill value status flag
                                            !  H5D_FILL_VALUE_ERROR_F
                                            !  H5D_FILL_VALUE_UNDEFINED_F
                                            !  H5D_FILL_VALUE_DEFAULT_F
                                            !  H5D_FILL_VALUE_USER_DEFINED_F
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
                                            ! 0 on success and -1 on failure
!*****
    INTERFACE
       INTEGER FUNCTION h5pfill_value_defined_c(plist_id, flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PFILL_VALUE_DEFINED_C'::h5pfill_value_defined_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(OUT) :: flag
       END FUNCTION h5pfill_value_defined_c
    END INTERFACE

    hdferr = h5pfill_value_defined_c(plist_id, flag) 
  END SUBROUTINE h5pfill_value_defined_f

!****s* H5P/h5pset_alloc_time_f
! NAME
!  h5pset_alloc_time_f
!
! PURPOSE
!  Set space allocation time for dataset during creation.
!
! INPUTS
!  
!  plist_id	- dataset creation property list identifier
!  flag         - allocation time flag:
!  		    H5D_ALLOC_TIME_ERROR_F
!  		    H5D_ALLOC_TIME_DEFAULT_F
!  		    H5D_ALLOC_TIME_EARLY_F
!  		    H5D_ALLOC_TIME_LATE_F
!  		    H5D_ALLOC_TIME_INCR_F
! OUTPUTS
!  
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 4, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pset_alloc_time_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id  ! Dataset creation property list identifier
    INTEGER, INTENT(IN) :: flag             ! Allocation time flag:
                                            !  H5D_ALLOC_TIME_ERROR_F
                                            !  H5D_ALLOC_TIME_DEFAULT_F
                                            !  H5D_ALLOC_TIME_EARLY_F
                                            !  H5D_ALLOC_TIME_LATE_F
                                            !  H5D_ALLOC_TIME_INCR_F
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
                                            ! 0 on success and -1 on failure
!*****  
    
    INTERFACE
       INTEGER FUNCTION h5pset_alloc_time_c(plist_id, flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_ALLOC_TIME_C'::h5pset_alloc_time_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(IN) :: flag
       END FUNCTION h5pset_alloc_time_c
    END INTERFACE
    
    hdferr = h5pset_alloc_time_c(plist_id, flag) 
  END SUBROUTINE h5pset_alloc_time_f

!****s* H5P/h5pget_alloc_time_f
! NAME
!  h5pget_alloc_time_f
!
! PURPOSE
!  Get space allocation time for dataset during creation.
!
! INPUTS
!  
!  plist_id	- dataset creation property list identifier
! OUTPUTS
!  
!  flag         - allocation time flag:
!  		    H5D_ALLOC_TIME_ERROR_F
!  		    H5D_ALLOC_TIME_DEFAULT_F
!  		    H5D_ALLOC_TIME_EARLY_F
!  		    H5D_ALLOC_TIME_LATE_F
!  		    H5D_ALLOC_TIME_INCR_F
!  hdferr:	- error code		
!  		    Success:  0
!  		    Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 4, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pget_alloc_time_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id  ! Dataset creation property list identifier
    INTEGER, INTENT(OUT) :: flag   ! Allocation time flag:
                                   !  H5D_ALLOC_TIME_ERROR_F
                                   !  H5D_ALLOC_TIME_DEFAULT_F
                                   !  H5D_ALLOC_TIME_EARLY_F
                                   !  H5D_ALLOC_TIME_LATE_F
                                   !  H5D_ALLOC_TIME_INCR_F
    INTEGER, INTENT(OUT) :: hdferr ! Error code
                                   ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pget_alloc_time_c(plist_id, flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_ALLOC_TIME_C'::h5pget_alloc_time_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(OUT) :: flag
       END FUNCTION h5pget_alloc_time_c
    END INTERFACE
    
    hdferr = h5pget_alloc_time_c(plist_id, flag) 
  END SUBROUTINE h5pget_alloc_time_f

!****s* H5P/h5pset_fill_time_f
! NAME
!  h5pset_fill_time_f
!
! PURPOSE
!  Set fill value writing time for dataset
!
! INPUTS
!  
!  plist_id	- dataset creation property list identifier
!  flag         - fill time flag:
!  		    H5D_FILL_TIME_ERROR_F
!  		    H5D_FILL_TIME_ALLOC_F
!  		    H5D_FILL_TIME_NEVER_F
! OUTPUTS
!  
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 4, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pset_fill_time_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id  ! Dataset creation property list identifier
    INTEGER, INTENT(IN) :: flag             ! Fill time flag:
                                            !  H5D_FILL_TIME_ERROR_F
                                            !  H5D_FILL_TIME_ALLOC_F
                                            !  H5D_FILL_TIME_NEVER_F
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
                                            ! 0 on success and -1 on failure
!*****  

    INTERFACE
       INTEGER FUNCTION h5pset_fill_time_c(plist_id, flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FILL_TIME_C'::h5pset_fill_time_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(IN) :: flag
       END FUNCTION h5pset_fill_time_c
    END INTERFACE
    
    hdferr = h5pset_fill_time_c(plist_id, flag) 
  END SUBROUTINE h5pset_fill_time_f

!****s* H5P/h5pget_fill_time_f
! NAME
!   h5pget_fill_time_f
!
! PURPOSE
!  Get fill value writing time for dataset
!
! INPUTS
!  
!  plist_id	- dataset creation property list identifier
! OUTPUTS
!  
!  hdferr:	- error code		
!  		   Success:  0
!  		   Failure: -1
! OPTIONAL PARAMETERS
!
!  flag         - fill time flag:
!  		   H5D_FILL_TIME_ERROR_F
!  		   H5D_FILL_TIME_ALLOC_F
!  		   H5D_FILL_TIME_NEVER_F
! AUTHOR
!  Elena Pourmal
!  October 4, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pget_fill_time_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id  ! Dataset creation property list identifier
    INTEGER, INTENT(OUT) :: flag   ! Fill time flag:
                                   !  H5D_FILL_TIME_ERROR_F
                                   !  H5D_FILL_TIME_ALLOC_F
                                   !  H5D_FILL_TIME_NEVER_F
    INTEGER, INTENT(OUT) :: hdferr ! Error code
                                   ! 0 on success and -1 on failure
!*****   
  
    INTERFACE
       INTEGER FUNCTION h5pget_fill_time_c(plist_id, flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FILL_TIME_C'::h5pget_fill_time_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(OUT) :: flag
       END FUNCTION h5pget_fill_time_c
    END INTERFACE
    
    hdferr = h5pget_fill_time_c(plist_id, flag) 
  END SUBROUTINE h5pget_fill_time_f

!****s* H5P/ h5pset_meta_block_size_f 
! NAME
!  h5pset_meta_block_size_f 
!
! PURPOSE
!  Sets the minimum size of metadata block allocations 
!
! INPUTS
!  
!  plist_id	- file access property list identifier
!  size		- metatdata block size
! OUTPUTS
!  
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 7, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pset_meta_block_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id  ! File access property list identifier 
    INTEGER(HSIZE_T), INTENT(IN) :: size    ! Block size in bytes; 
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
                                            ! 0 on success and -1 on failure
!*****
    INTERFACE
       INTEGER FUNCTION h5pset_meta_block_size_c(plist_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_META_BLOCK_SIZE_C'::h5pset_meta_block_size_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(IN) :: size
       END FUNCTION h5pset_meta_block_size_c
    END INTERFACE
    
    hdferr = h5pset_meta_block_size_c(plist_id, size) 
  END SUBROUTINE h5pset_meta_block_size_f

!****s* H5P/h5pget_meta_block_size_f 
! NAME
!  h5pget_meta_block_size_f 
!
! PURPOSE
!  Gets the minimum size of metadata block allocations 
!
! INPUTS
!  
!  plist_id	- file access property list identifier
! OUTPUTS
!  
!  size		- metatdata block size
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 7, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pget_meta_block_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id  ! File access property list identifier 
    INTEGER(HSIZE_T), INTENT(OUT) :: size   ! Block size in bytes; 
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
                                            ! 0 on success and -1 on failure
!*****
    INTERFACE
       INTEGER FUNCTION h5pget_meta_block_size_c(plist_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_META_BLOCK_SIZE_C'::h5pget_meta_block_size_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_meta_block_size_c
    END INTERFACE
    
    hdferr = h5pget_meta_block_size_c(plist_id, size) 
  END SUBROUTINE h5pget_meta_block_size_f

!****s* H5P/h5pset_sieve_buf_size_f 
! NAME
!  h5pset_sieve_buf_size_f 
!
! PURPOSE
!  Sets the maximum size of the data sieve buffer
!
! INPUTS
!  
!  plist_id	- file access property list identifier
!  size		- sieve buffer size
! OUTPUTS
!  
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 7, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pset_sieve_buf_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! File access property list identifier 
    INTEGER(SIZE_T), INTENT(IN) :: size    ! Buffer size in bytes; 
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
                                           ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pset_sieve_buf_size_c(plist_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_SIEVE_BUF_SIZE_C'::h5pset_sieve_buf_size_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(SIZE_T), INTENT(IN) :: size
       END FUNCTION h5pset_sieve_buf_size_c
    END INTERFACE
    
    hdferr = h5pset_sieve_buf_size_c(plist_id, size) 
  END SUBROUTINE h5pset_sieve_buf_size_f

!****s* H5P/h5pget_sieve_buf_size_f
! NAME
!  h5pget_sieve_buf_size_f 
!
! PURPOSE
!  Gets the maximum size of the data sieve buffer
!
! INPUTS
!  
!  plist_id	- file access property list identifier
! OUTPUTS
!  
!  size		- sieve buffer size
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 7, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pget_sieve_buf_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! File access property list identifier 
    INTEGER(SIZE_T), INTENT(OUT) :: size   ! Buffer size in bytes 
    INTEGER, INTENT(OUT)       :: hdferr   ! Error code
                                           ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pget_sieve_buf_size_c(plist_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_SIEVE_BUF_SIZE_C'::h5pget_sieve_buf_size_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(SIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_sieve_buf_size_c
    END INTERFACE
    
    hdferr = h5pget_sieve_buf_size_c(plist_id, size) 
  END SUBROUTINE h5pget_sieve_buf_size_f

!****s* H5P/h5pset_small_data_block_size_f 
! NAME
!  h5pset_small_data_block_size_f 
!
! PURPOSE
!  Sets the minimum size of "small" raw data block
!
! INPUTS
!  
!  plist_id	- file access property list identifier
!  size		- small raw data block size
! OUTPUTS
!  
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 7, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pset_small_data_block_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! File access property list identifier 
    INTEGER(HSIZE_T), INTENT(IN) :: size   ! Small raw data block size
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
                                           ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pset_small_data_block_size_c(plist_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_SMALL_DATA_BLOCK_SIZE_C'::h5pset_small_data_block_size_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(IN) :: size
       END FUNCTION h5pset_small_data_block_size_c
    END INTERFACE

    hdferr = h5pset_small_data_block_size_c(plist_id, size) 
  END SUBROUTINE h5pset_small_data_block_size_f

!****s* H5P/h5pget_small_data_block_size_f 
! NAME
!  h5pget_small_data_block_size_f 
!
! PURPOSE
!  Gets the minimum size of "small" raw data block
!
! INPUTS
!  
!  plist_id	- file access property list identifier
! OUTPUTS
!  
!  size		- small raw data block size
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 7, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pget_small_data_block_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id  ! File access property list identifier 
    INTEGER(HSIZE_T), INTENT(OUT) :: size   ! Small raw data block size
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
                                            ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pget_small_data_block_size_c(plist_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_SMALL_DATA_BLOCK_SIZE_C'::h5pget_small_data_block_size_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(HSIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_small_data_block_size_c
    END INTERFACE
    
    hdferr = h5pget_small_data_block_size_c(plist_id, size) 
  END SUBROUTINE h5pget_small_data_block_size_f

!****s* H5P/h5pset_hyper_vector_size_f 
! NAME
!  h5pset_hyper_vector_size_f 
!
! PURPOSE
!  Set the number of "I/O" vectors (vector size)
!
! INPUTS
!  
!  plist_id	- dataset transfer property list identifier
!  size		- vector size
! OUTPUTS
!  
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 7, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pset_hyper_vector_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! Dataset transfer property list identifier 
    INTEGER(SIZE_T), INTENT(IN) :: size    ! Vector size
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
                                           ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pset_hyper_vector_size_c(plist_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_HYPER_VECTOR_SIZE_C'::h5pset_hyper_vector_size_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(SIZE_T), INTENT(IN) :: size
       END FUNCTION h5pset_hyper_vector_size_c
    END INTERFACE
    
    hdferr = h5pset_hyper_vector_size_c(plist_id, size) 
  END SUBROUTINE h5pset_hyper_vector_size_f

!****s* H5P/ h5pget_hyper_vector_size_f 
! NAME
!  h5pget_hyper_vector_size_f 
!
! PURPOSE
!  Get the number of "I/O" vectors (vector size)
!
! INPUTS
!  
!  plist_id	- dataset transfer property list identifier
! OUTPUTS
!  
!  size		- vector size
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  October 7, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pget_hyper_vector_size_f(plist_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! Dataset transfer property list identifier 
    INTEGER(SIZE_T), INTENT(OUT) :: size   ! Vector size
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
                                           ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pget_hyper_vector_size_c(plist_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_HYPER_VECTOR_SIZE_C'::h5pget_hyper_vector_size_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER(SIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_hyper_vector_size_c
    END INTERFACE

    hdferr = h5pget_hyper_vector_size_c(plist_id, size) 
  END SUBROUTINE h5pget_hyper_vector_size_f

!****s* H5P/h5pexist_f 
! NAME
!   h5pexist_f 
!
! PURPOSE
!  Queries whether a property name exists in a property list or class. 
!
! INPUTS
!  
!  prp_id	- property list identifier to query
!  name 	- name of property to check for
! OUTPUTS
!  
!  flag         - logical flag
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pexist_f(prp_id, name, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    LOGICAL, INTENT(OUT) :: flag          ! .TRUE. if exists, .FALSE. otherwise
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
                                    ! 0 on success and -1 on failure
!*****
    INTEGER :: name_len
    
    INTERFACE
       INTEGER FUNCTION h5pexist_c(prp_id, name, name_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PEXIST_C'::h5pexist_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
       END FUNCTION h5pexist_c
    END INTERFACE
    flag = .FALSE.
    name_len = LEN(name)
    hdferr = h5pexist_c(prp_id, name , name_len)
    IF (hdferr > 0) THEN
       flag = .TRUE.
       hdferr = 0
    ENDIF
  END SUBROUTINE h5pexist_f

!****s* H5P/h5pget_size_f 
!
! NAME
!  h5pget_size_f 
!
! PURPOSE
!  Queries the size of a property value in bytes.
!
! INPUTS
!  
!  prp_id	- property list identifier to query
!  name 	- name of property to query
! OUTPUTS
!  
!  size         - size of property in bytes
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002	
!
! HISTORY
! 	
!  
! Fortran90 Interface:
  SUBROUTINE h5pget_size_f(prp_id, name, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to query
    INTEGER(SIZE_T), INTENT(OUT) :: size  ! Size in bytes
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!*****
    INTEGER :: name_len
    
    INTERFACE
       INTEGER FUNCTION h5pget_size_c(prp_id, name, name_len, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_SIZE_C'::h5pget_size_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(SIZE_T), INTENT(OUT) :: size
       END FUNCTION h5pget_size_c
    END INTERFACE
    name_len = LEN(name)
    hdferr = h5pget_size_c(prp_id, name , name_len, size)
  END SUBROUTINE h5pget_size_f

!****s* H5P/h5pget_npros_f 
! NAME
!   h5pget_npros_f 
!
! PURPOSE
!  Queries number of properties in property list or class
!
! INPUTS
!  
!  prp_id	- iproperty list identifier to query
! OUTPUTS
!  
!  nprops       - number of properties in property object
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pget_nprops_f(prp_id, nprops, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id    ! Property list identifier 
    INTEGER(SIZE_T), INTENT(OUT) :: nprops  ! Number of properties
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
                                            ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pget_nprops_c(prp_id, nprops)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_NPROPS_C'::h5pget_nprops_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(SIZE_T), INTENT(OUT) :: nprops
       END FUNCTION h5pget_nprops_c
    END INTERFACE
    hdferr = h5pget_nprops_c(prp_id, nprops)
  END SUBROUTINE h5pget_nprops_f

!****s* H5P/h5pget_class_name_f 
! NAME
!  h5pget_class_name_f 
!
! PURPOSE
!  Queries the name of a class.
!
! INPUTS
!  
!  prp_id       - property list identifier to query
! OUTPUTS
!  
!  name 	- name of a class
!  size         - Actual length of the class name
!                   NOTE: If provided buffer "name" is smaller,
!                   than name will be truncated to fit into
!                   provided user buffer
!  hdferr:	- error code
!  		   Success: 0
!  		   Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002	
!
! HISTORY
! Returned the size of name as an argument	
!  
! Fortran90 Interface:
  SUBROUTINE h5pget_class_name_f(prp_id, name, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
    CHARACTER(LEN=*), INTENT(OUT) :: name ! Buffer to retireve class name
    INTEGER, INTENT(OUT) :: size          ! Actual length of the class name
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!*****
    INTEGER :: name_len
    
    INTERFACE
       INTEGER FUNCTION h5pget_class_name_c(prp_id, name, name_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_CLASS_NAME_C'::h5pget_class_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(INOUT) :: name
         INTEGER, INTENT(IN)         :: name_len
       END FUNCTION h5pget_class_name_c
    END INTERFACE
    
    name_len = LEN(name)
    size = h5pget_class_name_c(prp_id, name, name_len)
    
    hdferr = 0
    IF(size.LT.0) hdferr = -1
    
  END SUBROUTINE h5pget_class_name_f

!****s* H5P/h5pget_class_parent_f 
! NAME
!  h5pget_class_parent_f 
!
! PURPOSE
!  Retrieves the parent class of a genric property class. 
!
! INPUTS
!  
!  prp_id	- property list identifier to query
! OUTPUTS
!  
!  parent_id 	- identifier of the parent class
!  hdferr:	- error code          
!  		   Success:  0
!  		   Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pget_class_parent_f(prp_id, parent_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id     ! Property list identifier 
    INTEGER(HID_T), INTENT(OUT) :: parent_id ! Parent class property list 
                                             ! identifier 
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
                                    ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5pget_class_parent_c(prp_id, parent_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_CLASS_PARENT_C'::h5pget_class_parent_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(OUT) :: parent_id
       END FUNCTION h5pget_class_parent_c
    END INTERFACE
    hdferr = h5pget_class_parent_c(prp_id, parent_id)
  END SUBROUTINE h5pget_class_parent_f

!****s* H5P/h5pisa_class_f 
! NAME
!   h5pisa_class_f 
!
! PURPOSE
!  Determines whether a property list is a member of a class. 
!
! INPUTS
!  
!  plist	- property list identifier 
!  pclass	- identifier of the property class
! OUTPUTS
!  
!  flag         - .TRUE. if a member, .FALSE. otherwise
!  hdferr:	- error code           
!  		   Success:  0
!  		   Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pisa_class_f(plist, pclass, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist     ! Property list identifier 
    INTEGER(HID_T), INTENT(IN) :: pclass    ! Class identifier
    LOGICAL, INTENT(OUT) :: flag            ! logical flag
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
                                    ! 0 on success and -1 on failure
!*****
    INTERFACE
       INTEGER FUNCTION h5pisa_class_c(plist, pclass)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PISA_CLASS_C'::h5pisa_class_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist
         INTEGER(HID_T), INTENT(IN) :: pclass
       END FUNCTION h5pisa_class_c
    END INTERFACE
    flag = .FALSE.
    hdferr = h5pisa_class_c(plist, pclass)
    IF (hdferr .GT. 0) THEN
       flag = .TRUE.
       hdferr = 0
    ENDIF
  END SUBROUTINE h5pisa_class_f

!****s* H5P/h5pcopy_prop_f 
! NAME
!   h5pcopy_prop_f 
!
! PURPOSE
!  Copies a property from one list or class to another.
!
! INPUTS
!  
!  dst_id		- Identifier of the destination property list
!  src_id		- Identifier of the source property list 
!  name 		- name of the property to copy
! OUTPUTS
!  
!  hdferr:		- error code
!  		 	   Success: 0 
!  		  	   Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pcopy_prop_f(dst_id, src_id, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dst_id  ! Destination property list 
                                          ! identifier 
    INTEGER(HID_T), INTENT(IN) :: src_id  ! Source property list identifier 
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Property name
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!*****
    INTEGER :: name_len
    
    INTERFACE
       INTEGER FUNCTION h5pcopy_prop_c(dst_id, src_id, name, name_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PCOPY_PROP_C'::h5pcopy_prop_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: dst_id
         INTEGER(HID_T), INTENT(IN) :: src_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
       END FUNCTION h5pcopy_prop_c
    END INTERFACE
    name_len = LEN(name)
    hdferr = h5pcopy_prop_c(dst_id, src_id, name , name_len)
  END SUBROUTINE h5pcopy_prop_f

!****s* H5P/h5premove_f 
! NAME
!   h5premove_f 
!
! PURPOSE
!  Removes a property from a property list. 

!
! INPUTS
!  
!  plid		- Property list identofoer
!  name 	- name of the property to remove
! OUTPUTS
!  
!  hdferr:	- error code
!  		   Success: 0 
!  		   Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5premove_f(plid, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plid   ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name ! Name of property to remove
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         !  0 on success and -1 on failure
!*****
    INTEGER :: name_len
    
    INTERFACE
       INTEGER FUNCTION h5premove_c(plid, name, name_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PREMOVE_C'::h5premove_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: plid 
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
       END FUNCTION h5premove_c
    END INTERFACE
    name_len = LEN(name)
    hdferr = h5premove_c(plid, name , name_len)
  END SUBROUTINE h5premove_f

!****s* H5P/h5punregister_f 
! NAME
!  h5punregister_f 
!
! PURPOSE
!  Removes a property from a property list class. 
!
! INPUTS
!  
!  class	- Property list class identifier
!  name 	- name of the property to remove
! OUTPUTS
!  
!  hdferr:	- error code
!                  Success: 0 
!  		   Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5punregister_f(class, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class  ! property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name ! name of property to remove
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5punregister_c(class, name, name_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PUNREGISTER_C'::h5punregister_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: class
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
       END FUNCTION h5punregister_c
    END INTERFACE
    name_len = LEN(name)
    hdferr = h5punregister_c(class, name , name_len)
  END SUBROUTINE h5punregister_f

!****s* H5P/h5pclose_class_f 
! NAME
!  h5pclose_class_f 
!
! PURPOSE
!  Closes an existing property list class.
!
! INPUTS
!  
!  class	- Property list class identifier
! OUTPUTS
!  
!  hdferr	- error code         
!  		   Success: 0 
!  		   Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  October 9, 2002	
!
! Fortran90 Interface:
  SUBROUTINE h5pclose_class_f(class, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class ! Property list class identifier
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
                                        ! 0 on success and -1 on failure
!*****
    INTERFACE
       INTEGER FUNCTION h5pclose_class_c(class)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PCLOSE_CLASS_C'::h5pclose_class_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: class
       END FUNCTION h5pclose_class_c
    END INTERFACE
    hdferr = h5pclose_class_c(class)
  END SUBROUTINE h5pclose_class_f

!****s* H5P/h5pset_shuffle_f 
! NAME
!  h5pset_shuffle_f 
!
! PURPOSE
!  Sets shuffling filter
!
! INPUTS
!  prp_id	- dataset creation property list identifier
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pset_shuffle_f(prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_shuffle_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_shuffle_c(prp_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_SHUFFLE_C'::h5pset_shuffle_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
       END FUNCTION h5pset_shuffle_c
    END INTERFACE
    hdferr = h5pset_shuffle_c(prp_id)
    
  END SUBROUTINE h5pset_shuffle_f

!****s* H5P/h5pset_edc_check_f 
! NAME
!  h5pset_edc_check_f 
!
! PURPOSE
!  Enables/disables error detecting  
!
! INPUTS
!  
!  prp_id	- dataset creation property list identifier
!  flag         - EDC flag; possible values:
!                   H5Z_DISABLE_EDC_F
!                   H5Z_ENABLE_EDC_F
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  March 13, 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pset_edc_check_f(prp_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: flag          ! Checksum filter flag
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pset_edc_check_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_edc_check_c(prp_id, flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_EDC_CHECK_C'::h5pset_edc_check_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: flag 
       END FUNCTION h5pset_edc_check_c
    END INTERFACE
    hdferr = h5pset_edc_check_c(prp_id, flag)
    
  END SUBROUTINE h5pset_edc_check_f

!****s* H5P/h5pget_edc_check_f
! NAME
!  h5pget_edc_check_f 
!
! PURPOSE
!  Queries error detecting  
!
! INPUTS
!  
!  prp_id	- dataset creation property list identifier
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  March 13, 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pget_edc_check_f(prp_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Dataset transfer property list identifier 
    INTEGER, INTENT(OUT) :: flag         ! Checksum filter flag
                                         ! May have one of the following values:
                                         !  H5Z_ERROR_EDC_F
                                         !  H5Z_DISABLE_EDC_F
                                         !  H5Z_ENABLE_EDC_F
                                         !  H5Z_NO_EDC_F
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

!            INTEGER, EXTERNAL :: h5pget_edc_check_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_edc_check_c(prp_id, flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_EDC_CHECK_C'::h5pget_edc_check_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: flag
       END FUNCTION h5pget_edc_check_c
    END INTERFACE
    hdferr = h5pget_edc_check_c(prp_id, flag)
    
  END SUBROUTINE h5pget_edc_check_f

!****s* H5P/h5pset_fletcher32_f
! NAME
!  h5pset_fletcher32_f 
!
! PURPOSE
!  Sets Fletcher32 checksum of EDC for a dataset creation 
!  property list.
!
! INPUTS
!  
!  prp_id	- dataset creation property list identifier
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  March 13, 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pset_fletcher32_f(prp_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!***** 

!            INTEGER, EXTERNAL :: h5pset_fletcher32_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_fletcher32_c(prp_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FLETCHER32_C'::h5pset_fletcher32_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
       END FUNCTION h5pset_fletcher32_c
    END INTERFACE
    hdferr = h5pset_fletcher32_c(prp_id)

  END SUBROUTINE h5pset_fletcher32_f

!****s* H5P/ h5pset_family_offset_f
! NAME
!  h5pset_family_offset_f 
!
! PURPOSE
!  Sets offset for family file driver.
!
! INPUTS
!  
!  prp_id	- file creation property list identifier
!  offset	- file offset
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  19 March 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pset_family_offset_f(prp_id, offset, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id   ! Property list identifier 
    INTEGER(HSIZE_T), INTENT(IN) :: offset ! Offset in bytes
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
                                           ! 0 on success and -1 on failure
!***** 

!            INTEGER, EXTERNAL :: h5pset_family_offset_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_family_offset_c(prp_id, offset)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FAMILY_OFFSET_C'::h5pset_family_offset_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HSIZE_T), INTENT(IN) :: offset 
       END FUNCTION h5pset_family_offset_c
    END INTERFACE
    hdferr = h5pset_family_offset_c(prp_id, offset)
    
  END SUBROUTINE h5pset_family_offset_f

!****s* H5P/h5pset_fapl_multi_l
! NAME
!  h5pset_fapl_multi_l 
!
! PURPOSE
!  Sets up use of the multi-file driver. 
!
! INPUTS
!  
!  prp_id	- file creation property list identifier
!  mem_map      - mapping array
!  memb_fapl    - property list for each memory usage type
!  memb_name    - names of member file
!  relax        - flag 
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  20 March 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pset_fapl_multi_l(prp_id, memb_map, memb_fapl, memb_name, memb_addr, relax, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! File creation property list identifier 
    INTEGER, DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(IN) :: memb_map ! Mapping array
    INTEGER(HID_T), DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(IN) :: memb_fapl ! Property list for each memory usage type
    CHARACTER(LEN=*), DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(IN) :: memb_name ! Names of member file
    REAL, DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(IN) :: memb_addr 
    LOGICAL, INTENT(IN) :: relax     ! Flag
    INTEGER, INTENT(OUT) :: hdferr   ! Error code
                                     ! 0 on success and -1 on failure
!***** 
    INTEGER, DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: lenm
    INTEGER :: maxlen
    INTEGER :: flag
    INTEGER :: i

!            INTEGER, EXTERNAL :: h5pset_fapl_multi_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_multi_c(prp_id, memb_map, memb_fapl, memb_name, lenm, &
            maxlen, memb_addr, flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FAPL_MULTI_C'::h5pset_fapl_multi_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: memb_name
         INTEGER(HID_T), INTENT(IN) :: prp_id ! File creation property list identifier 
         INTEGER, DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(IN) :: memb_map
         INTEGER(HID_T), DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(IN) :: memb_fapl
         CHARACTER(LEN=*), DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(IN) :: memb_name
         REAL, DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(IN) :: memb_addr
         !INTEGER(HADDR_T), DIMENSION(H5FD_MEM_NTYPES_F), INTENT(IN) :: memb_addr
         INTEGER, DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: lenm
         INTEGER :: maxlen
         INTEGER, INTENT(IN) :: flag
       END FUNCTION h5pset_fapl_multi_c
    END INTERFACE
    maxlen = LEN(memb_name(1))
    DO i=0, H5FD_MEM_NTYPES_F-1
       lenm(i) = LEN_TRIM(memb_name(i))
    ENDDO
    flag = 0
    IF (relax) flag = 1
    hdferr = h5pset_fapl_multi_c(prp_id, memb_map, memb_fapl, memb_name, lenm, maxlen, memb_addr, flag) 
    
  END SUBROUTINE h5pset_fapl_multi_l
!****s* H5P/h5pset_fapl_multi_s 
! NAME
!  h5pset_fapl_multi_s 
!
! PURPOSE
!  Sets up use of the multi-file driver. 
!
! INPUTS
!  
!  prp_id	- file creation property list identifier
!  relax        - flag 
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  31 March 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pset_fapl_multi_s(prp_id, relax, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! File creation property list identifier 
    LOGICAL, INTENT(IN) :: relax
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!***** 
    INTEGER :: flag

!            INTEGER, EXTERNAL :: h5pset_fapl_multi_sc
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_multi_sc(prp_id,flag) 
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FAPL_MULTI_SC'::h5pset_fapl_multi_sc
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id ! File creation property list identifier 
         INTEGER, INTENT(IN) :: flag
       END FUNCTION h5pset_fapl_multi_sc
    END INTERFACE
    flag = 0
    IF (relax) flag = 1
    hdferr = h5pset_fapl_multi_sc(prp_id, flag) 
    
  END SUBROUTINE h5pset_fapl_multi_s
!****s* H5P/h5pget_fapl_multi_f 
! NAME
!  h5pget_fapl_multi_f 
!
! PURPOSE
!  Sets up use of the multi-file driver. 
!
! INPUTS
!  
!  prp_id	- file creation property list identifier
! OUTPUTS
!  
!  mem_map      - mapping array
!  memb_fapl    - property list for each memory usage type
!  memb_name    - names of member file
!  relax        - flag 
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1
!
! OPTIONAL PARAMETERS
!  maxlen_out   - maximum length for memb_name array element 
!
! AUTHOR
!  Elena Pourmal
!  24 March 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pget_fapl_multi_f(prp_id, memb_map, memb_fapl, memb_name, memb_addr, relax, hdferr, maxlen_out)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! File creation property list identifier 
    INTEGER, DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(OUT) :: memb_map
    INTEGER(HID_T), DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(OUT) :: memb_fapl
    CHARACTER(LEN=*), DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(OUT) :: memb_name
    !INTEGER(HADDR_T), DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(OUT) :: memb_addr
    REAL, DIMENSION(0:H5FD_MEM_NTYPES_F-1), INTENT(OUT) :: memb_addr
    INTEGER, OPTIONAL, INTENT(OUT) :: maxlen_out 
    LOGICAL, INTENT(OUT) :: relax
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!***** 
    INTEGER, DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: lenm
    INTEGER :: maxlen
    INTEGER :: c_maxlen_out 
    INTEGER :: flag
    INTEGER :: i

!            INTEGER, EXTERNAL :: h5pget_fapl_multi_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_fapl_multi_c(prp_id, memb_map, memb_fapl, memb_name, lenm, &
            maxlen, memb_addr, flag, c_maxlen_out)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FAPL_MULTI_C'::h5pget_fapl_multi_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: memb_name
         INTEGER(HID_T), INTENT(IN) :: prp_id ! File creation property list identifier 
         INTEGER, DIMENSION(H5FD_MEM_NTYPES_F), INTENT(OUT) :: memb_map
         INTEGER(HID_T), DIMENSION(H5FD_MEM_NTYPES_F), INTENT(OUT) :: memb_fapl
         CHARACTER(LEN=*), DIMENSION(H5FD_MEM_NTYPES_F), INTENT(OUT) :: memb_name
         REAL, DIMENSION(H5FD_MEM_NTYPES_F), INTENT(OUT) :: memb_addr
         INTEGER, DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: lenm
         INTEGER :: maxlen
         INTEGER :: c_maxlen_out 
         INTEGER, INTENT(OUT) :: flag
       END FUNCTION h5pget_fapl_multi_c
    END INTERFACE
    maxlen = LEN(memb_name(0))
    DO i=0, H5FD_MEM_NTYPES_F-1
       lenm(i) = LEN_TRIM(memb_name(i))
    ENDDO
    hdferr = h5pget_fapl_multi_c(prp_id, memb_map, memb_fapl, memb_name, lenm, maxlen, memb_addr, flag, c_maxlen_out) 
    relax = .TRUE.
    IF(flag .EQ. 0) relax = .FALSE. 
    IF(PRESENT(maxlen_out)) maxlen_out = c_maxlen_out
  END SUBROUTINE h5pget_fapl_multi_f
!****s* H5P/h5pset_szip_f 
! NAME
!  h5pset_szip_f 
!
! PURPOSE
!  Sets up use of szip compression
!
! INPUTS
!  
!  prp_id	    - dataset creation property list identifier
!  options_mask     - A bit-mask conveying the desired SZIP options.
!                     Current valid values in Fortran are:
!                        H5_SZIP_EC_OM_F
!                        H5_SZIP_NN_OM_F
!  pixels_per_block - szip parameters
! OUTPUTS
!  hdferr           - error code		
!	                Success:  0
!	                Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  April 10 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pset_szip_f(prp_id, options_mask, pixels_per_block, hdferr) 
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Dataset creation property 
                                         ! list identifier 
    INTEGER, INTENT(IN) :: options_mask  ! A bit-mask conveying the desired
                                         ! SZIP options
                                         ! Current valid values in Fortran are:
                                         !    H5_SZIP_EC_OM_F
                                         !    H5_SZIP_NN_OM_F
    INTEGER, INTENT(IN) :: pixels_per_block ! The number of pixels or data elements 
                                            ! in each data block
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!***** 

!  INTEGER, EXTERNAL :: h5pset_szip_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_szip_c(prp_id, options_mask, pixels_per_block) 
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_SZIP_C'::h5pset_szip_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id ! File creation property list identifier 
         INTEGER, INTENT(IN) :: options_mask
         INTEGER, INTENT(IN) :: pixels_per_block
       END FUNCTION h5pset_szip_c
    END INTERFACE
    hdferr = h5pset_szip_c(prp_id, options_mask, pixels_per_block) 
    
  END SUBROUTINE h5pset_szip_f

!****s* H5P/h5pall_filters_avail_f 
! NAME
!  h5pall_filters_avail_f 
!
! PURPOSE
!  Checks if all filters set in the dataset creation
!  property list are available
!
! INPUTS
!  
!  prp_id	- data creation property list identifier
! OUTPUTS
!  
!  flag         - .TRUE. if all filters are available
!                 .FALSE. otherwise
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  April 10 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pall_filters_avail_f(prp_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Dataset creation property 
                                         !  list identifier 
    LOGICAL, INTENT(OUT) :: flag         ! .TRUE. if all filters are available
                                         ! .FALSE. otherwise
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!***** 
    INTEGER :: status

!            INTEGER, EXTERNAL :: h5pall_filters_avail_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pall_filters_avail_c(prp_id, status) 
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PALL_FILTERS_AVAIL_C'::h5pall_filters_avail_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id ! File creation property list identifier 
         INTEGER, INTENT(OUT) :: status
       END FUNCTION h5pall_filters_avail_c
    END INTERFACE
    flag = .TRUE.
    hdferr = h5pall_filters_avail_c(prp_id, status) 
    IF (status .EQ. 0 ) flag = .FALSE.
    
  END SUBROUTINE h5pall_filters_avail_f

!****s* H5P/h5pget_filter_by_id_f
! NAME
!  h5pget_filter_by_id_f 
!
! PURPOSE
!  Returns information about a filter in a pipeline
!
! INPUTS
!  
!  prp_id	- data creation or transfer property list 
!  		  identifier
! OUTPUTS
!  
!  filter_id	- filter identifier
!  flags	- bit vector specifying certain general
!  		  properties of the filter
!  cd_nelmts	- number of elements in cd_values
!  cd_values	- auxiliary data for the filter
!  namelen	- number of characters in the name buffer
!  name		- buffer to retrieve filter name
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  April 10 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pget_filter_by_id_f(prp_id, filter_id, flags, cd_nelmts, cd_values, namelen, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id   ! Property list identifier 
    INTEGER, INTENT(IN) :: filter_id       ! Filter identifier
    INTEGER(SIZE_T), INTENT(INOUT) :: cd_nelmts     ! Number of elements in cd_values.
    INTEGER, DIMENSION(*), INTENT(OUT) :: cd_values ! Auxiliary data for the filter.
    INTEGER, INTENT(OUT) :: flags          ! Bit vector specifying certain general
                                           ! properties of the filter.
    INTEGER(SIZE_T), INTENT(IN) :: namelen ! Anticipated number of characters in name.
    CHARACTER(LEN=*), INTENT(OUT) :: name  ! Name of the filter
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
                                           ! 0 on success and -1 on failure
!***** 


!            INTEGER, EXTERNAL :: h5pget_filter_by_id_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_filter_by_id_c(prp_id, filter_id, flags, cd_nelmts,  &
            cd_values, namelen, name)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FILTER_BY_ID_C'::h5pget_filter_by_id_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: filter_id 
         INTEGER, DIMENSION(*), INTENT(OUT) :: cd_values
         INTEGER, INTENT(OUT) :: flags 
         INTEGER(SIZE_T), INTENT(INOUT) :: cd_nelmts
         INTEGER(SIZE_T), INTENT(IN) :: namelen
         CHARACTER(LEN=*), INTENT(OUT) :: name
       END FUNCTION h5pget_filter_by_id_c
    END INTERFACE
    
    hdferr = h5pget_filter_by_id_c(prp_id, filter_id, flags, cd_nelmts,  & 
         cd_values, namelen, name)
  END SUBROUTINE h5pget_filter_by_id_f

!****s* H5P/h5pmodify_filter_f
! NAME
!  h5pmodify_filter_f 
!
! PURPOSE
!  Adds a filter to the filter pipeline. 
!
! INPUTS
!  
!  prp_id	- data creation or transfer property list 
!  		  identifier
!  filter	- filter to be modified
!  flags	- bit vector specifying certain general
!  		  properties of the filter
!  cd_nelmts	- number of elements in cd_values
!  cd_values	- auxiliary data for the filter
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Elena Pourmal
!  April 10 2003
!
! Fortran90 Interface:
  SUBROUTINE h5pmodify_filter_f(prp_id, filter, flags, cd_nelmts, cd_values,  hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
    INTEGER, INTENT(IN) :: filter        ! Filter to be modified
    INTEGER, INTENT(IN) :: flags         ! Bit vector specifying certain general
                                         !  properties of the filter
    INTEGER(SIZE_T), INTENT(IN) :: cd_nelmts       ! Number of elements in cd_values
    INTEGER, DIMENSION(*), INTENT(IN) :: cd_values ! Auxiliary data for the filter
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!***** 

!            INTEGER, EXTERNAL :: h5pmodify_filter_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pmodify_filter_c(prp_id, filter, flags, cd_nelmts, cd_values)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PMODIFY_FILTER_C'::h5pmodify_filter_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id 
         INTEGER, INTENT(IN) :: filter 
         INTEGER, INTENT(IN) :: flags 
         INTEGER(SIZE_T), INTENT(IN) :: cd_nelmts 
         INTEGER, DIMENSION(*), INTENT(IN) :: cd_values 
       END FUNCTION h5pmodify_filter_c
    END INTERFACE
    
    hdferr = h5pmodify_filter_c(prp_id, filter, flags, cd_nelmts, cd_values )
  END SUBROUTINE h5pmodify_filter_f

!****s* H5P/h5premove_filter_f 
! NAME
!  h5premove_filter_f 
!
! PURPOSE
!  Delete one or more filters from the filter pipeline. 
!
! INPUTS
!  
!  prp_id	- data creation or transfer property list 
!  		  identifier
!  filter	- filter to be removed
! OUTPUTS
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  Quincey Koziol
!  January 27 2004
!
! Fortran90 Interface:
  SUBROUTINE h5premove_filter_f(prp_id, filter, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Dataset creation property list
                                         ! identifier
    INTEGER, INTENT(IN) :: filter        ! Filter to be removed
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!***** 

!            INTEGER, EXTERNAL :: h5premove_filter_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5premove_filter_c(prp_id, filter)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PREMOVE_FILTER_C'::h5premove_filter_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id 
         INTEGER, INTENT(IN) :: filter 
       END FUNCTION h5premove_filter_c
    END INTERFACE
    
    hdferr = h5premove_filter_c(prp_id, filter)
  END SUBROUTINE h5premove_filter_f

!****s* H5P/H5Pget_attr_phase_change_f
! NAME
!  H5Pget_attr_phase_change_f 
!
! PURPOSE
!  Retrieves attribute storage phase change thresholds 
!
! INPUTS
!  
!  ocpl_id	   - Object (dataset or group) creation property list identifier
! OUTPUTS
!  
!  max_compact     - Maximum number of attributes to be stored in compact storage
!                    (Default: 8)
!  min_dense       - Minimum number of attributes to be stored in dense storage
!                    (Default: 6)
!  hdferr          - Error code		
!	              Success:  0
!	              Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! Fortran90 Interface:  
  SUBROUTINE h5pget_attr_phase_change_f(ocpl_id, max_compact, min_dense, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocpl_id ! Object (dataset or group) creation property list identifier
    INTEGER, INTENT(OUT) :: max_compact   ! Maximum number of attributes to be stored in compact storage
                                          ! (Default: 8)
    INTEGER, INTENT(OUT) :: min_dense     ! Minimum number of attributes to be stored in dense storage
                                          ! (Default: 6)
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_attr_phase_change_c(ocpl_id, max_compact, min_dense)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_ATTR_PHASE_CHANGE_C'::h5pget_attr_phase_change_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: ocpl_id
         INTEGER, INTENT(OUT) :: max_compact
         INTEGER, INTENT(OUT) :: min_dense

       END FUNCTION h5pget_attr_phase_change_c
    END INTERFACE

    hdferr = h5pget_attr_phase_change_c(ocpl_id, max_compact, min_dense)
  END SUBROUTINE h5pget_attr_phase_change_f

!****s* H5P/H5Pset_attr_creation_order_f 
! NAME
!  H5Pset_attr_creation_order_f 
!
! PURPOSE
!  Sets tracking and indexing of attribute creation order
!
! INPUTS
!  
!  ocpl_id	   - Object creation property list identifier
!  crt_order_flags - Flags specifying whether to track and index attribute creation order
! OUTPUTS
!
!  hdferr          - Error code		
!	              Success:  0
!	              Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! Fortran90 Interface: 
  SUBROUTINE h5pset_attr_creation_order_f(ocpl_id, crt_order_flags , hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocpl_id   ! Object (dataset or group) creation property list identifier
    INTEGER, INTENT(IN) :: crt_order_flags  ! Flags specifying whether to track and index attribute creation order
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
                                            ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION H5Pset_attr_creation_order_c(ocpl_id, crt_order_flags)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_ATTR_CREATION_ORDER_C'::h5pset_attr_creation_order_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: ocpl_id
         INTEGER, INTENT(IN) :: crt_order_flags

       END FUNCTION H5Pset_attr_creation_order_c
    END INTERFACE

    hdferr = H5Pset_attr_creation_order_c(ocpl_id, crt_order_flags)
  END SUBROUTINE h5pset_attr_creation_order_f

!****s* H5P/H5Pset_shared_mesg_nindexes_f 
! NAME
!  H5Pset_shared_mesg_nindexes_f 
!
! PURPOSE
!  Sets number of shared object header message indexes 
!
! INPUTS
!  
!  plist_id - file creation property list
!  nindexes - Number of shared object header message indexes to be available in files created with this property list
! OUTPUTS
!
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! Fortran90 Interface:  
  SUBROUTINE h5pset_shared_mesg_nindexes_f( plist_id, nindexes, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! File creation property list
    INTEGER, INTENT(IN) :: nindexes  ! Number of shared object header message indexes 
                                     !  available in files created WITH this property list
    INTEGER, INTENT(OUT) :: hdferr   ! Error code
                                     ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_shared_mesg_nindexes_c(plist_id, nindexes)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_SHARED_MESG_NINDEXES_C'::h5pset_shared_mesg_nindexes_c
         !DEC$ENDIF
         
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(IN) :: nindexes

       END FUNCTION H5pset_shared_mesg_nindexes_c
    END INTERFACE

    hdferr = h5pset_shared_mesg_nindexes_c(plist_id, nindexes)

  END SUBROUTINE h5pset_shared_mesg_nindexes_f

!****s* H5P/H5Pset_shared_mesg_index_f
! NAME
!  H5Pset_shared_mesg_index_f
!
! PURPOSE
!  Configures the specified shared object header message index
!
! INPUTS
!  
!  fcpl_id         - File creation property list identifier.
!  index_num       - Index being configured.
!  mesg_type_flags - Types of messages that should be stored in this index.
!  min_mesg_size   - Minimum message size.
!
! OUTPUTS
!
!  hdferr  - error code		
!	      Success:  0
!	      Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! Fortran90 Interface:  
  SUBROUTINE h5pset_shared_mesg_index_f(fcpl_id, index_num, mesg_type_flags, min_mesg_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: fcpl_id  ! file creation property list
    INTEGER, INTENT(IN) :: index_num       ! Index being configured.
    INTEGER, INTENT(IN) :: mesg_type_flags ! Types of messages that should be stored in this index.
    INTEGER, INTENT(IN) :: min_mesg_size   ! Minimum message size.
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
                                           ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_shared_mesg_index_c(fcpl_id, index_num, mesg_type_flags, min_mesg_size)

         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_SHARED_MESG_INDEX_C'::h5pset_shared_mesg_index_c
         !DEC$ENDIF
         
         INTEGER(HID_T), INTENT(IN) :: fcpl_id 
         INTEGER, INTENT(IN) :: index_num
         INTEGER, INTENT(IN) :: mesg_type_flags
         INTEGER, INTENT(IN) :: min_mesg_size

       END FUNCTION H5pset_shared_mesg_index_c
    END INTERFACE

    hdferr = h5pset_shared_mesg_index_c(fcpl_id, index_num, mesg_type_flags, min_mesg_size)

  END SUBROUTINE h5pset_shared_mesg_index_f

!****s* H5P/H5Pget_attr_creation_order_f
! NAME
!  H5Pget_attr_creation_order_f
!
! PURPOSE
!  Retrieves tracking and indexing settings for attribute creation order
!
! INPUTS
!
!  ocpl_id         - Object (group or dataset) creation property list identifier
!
! OUTPUTS
!
!  crt_order_flags - Flags specifying whether to track and index attribute creation order
!  hdferr	   - Error code		
!  		 	Success:  0
!  		 	Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  February, 2008
!
! Fortran90 Interface:  
  SUBROUTINE h5pget_attr_creation_order_f(ocpl_id, crt_order_flags, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocpl_id   ! Object (group or dataset) creation property list identifier 
    INTEGER, INTENT(OUT) :: crt_order_flags ! Flags specifying whether to track and index attribute creation order 
    INTEGER, INTENT(OUT) :: hdferr   ! Error code
                                     ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_attr_creation_order_c(ocpl_id, crt_order_flags)

         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_ATTR_CREATION_ORDER_C'::h5pget_attr_creation_order_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: ocpl_id
         INTEGER, INTENT(OUT) :: crt_order_flags

       END FUNCTION H5pget_attr_creation_order_c
    END INTERFACE

    hdferr = h5pget_attr_creation_order_c(ocpl_id, crt_order_flags)

  END SUBROUTINE h5pget_attr_creation_order_f

!****s* H5P/H5Pset_libver_bounds_f
! NAME
! 	      H5Pset_libver_bounds_f
!
! PURPOSE
!    Sets bounds on library versions, and indirectly format versions, to be used when creating objects.
!
! INPUTS
!
!  fapl_id - File access property list identifier
!  low     - The earliest version of the library that will be used for writing objects.
!  high    - The latest version of the library that will be used for writing objects.
!
! OUTPUTS
!
!  hdferr  - error code		
!  	      Success:  0
!  	      Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  February 18, 2008
!
! Fortran90 Interface:  
  SUBROUTINE h5pset_libver_bounds_f(fapl_id, low, high, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: fapl_id ! File access property list identifier
    INTEGER, INTENT(IN) :: low   ! The earliest version of the library that will be used for writing objects.
                                 ! Currently, low must be one of two pre-defined values:
                                 !            HDF_LIBVER_EARLIEST_F
                                 !            HDF_LIBVER_LATEST_F
    INTEGER, INTENT(IN) :: high  ! The latest version of the library that will be used for writing objects.
                                 ! Currently, low must set to the pre-defined value:
                                 !            HDF_LIBVER_LATEST_F
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
                                    ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_libver_bounds_c(fapl_id, low, high)

         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_LIBVER_BOUNDS_C'::h5pset_libver_bounds_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: fapl_id
         INTEGER, INTENT(IN) :: low
         INTEGER, INTENT(IN) :: high

       END FUNCTION H5pset_libver_bounds_c
    END INTERFACE

    hdferr = h5pset_libver_bounds_c(fapl_id, low, high)

  END SUBROUTINE h5pset_libver_bounds_f

!****s* H5P/H5Pset_link_creation_order_f 
! NAME
!  H5Pset_link_creation_order_f 
!
! PURPOSE
!    Sets creation order tracking and indexing for links in a group.
!
! INPUTS
!
!  gcpl_id  	   - Group creation property list identifier
!  crt_order_flags - Creation order flag(s)
!
! OUTPUTS
!
!  hdferr	    - Error code		
!  		 	Success:  0
!  		 	Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  February 18, 2008
!
! Fortran90 Interface:  
  SUBROUTINE h5pset_link_creation_order_f(gcpl_id, crt_order_flags, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id  ! File access property list identifier
    INTEGER, INTENT(IN) :: crt_order_flags ! Creation order flag(s)
    INTEGER, INTENT(OUT) :: hdferr   ! Error code
                                     ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_link_creation_order_c(gcpl_id, crt_order_flags)

         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_LINK_CREATION_ORDER_C'::h5pset_link_creation_order_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER, INTENT(IN) :: crt_order_flags

       END FUNCTION H5pset_link_creation_order_c
    END INTERFACE

    hdferr = h5pset_link_creation_order_c(gcpl_id, crt_order_flags)

  END SUBROUTINE h5pset_link_creation_order_f

!****s* H5P/H5Pget_link_phase_change_f
! NAME
!  H5Pget_link_phase_change_f
!
! PURPOSE
!  Queries the settings for conversion between compact and dense groups.
!
! INPUTS
!  
!  gcpl_id  	- Group creation property list identifier
! OUTPUTS
!  
!  max_compact  - Maximum number of attributes to be stored in compact storage
!  min_dense    - Minimum number of attributes to be stored in dense storage
!  hdferr       - Error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  February 20, 2008
!
! Fortran90 Interface:  
  SUBROUTINE h5pget_link_phase_change_f(gcpl_id, max_compact, min_dense, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id ! Group creation property list identifier
    INTEGER, INTENT(OUT) :: max_compact   ! Maximum number of attributes to be stored in compact storage
    INTEGER, INTENT(OUT) :: min_dense     ! Minimum number of attributes to be stored in dense storage
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_link_phase_change_c(gcpl_id, max_compact, min_dense)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_LINK_PHASE_CHANGE_C'::h5pget_link_phase_change_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER, INTENT(OUT) :: max_compact
         INTEGER, INTENT(OUT) :: min_dense

       END FUNCTION h5pget_link_phase_change_c
    END INTERFACE

    hdferr = h5pget_link_phase_change_c(gcpl_id, max_compact, min_dense)
  END SUBROUTINE h5pget_link_phase_change_f

!****s* H5P/H5Pget_obj_track_times_f 
! NAME
!  H5Pget_obj_track_times_f 
!
! PURPOSE
!  Returns whether times are tracked for an object.
!
! INPUTS
!  
!  plist_id	- property list id
!  flag         - object timestamp setting
!                 .TRUE.,.FALSE.
! OUTPUTS
!
!  hdferr       - error code		
!	          Success:  0
!	          Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  February 22, 2008
!
! Fortran90 Interface:  
  SUBROUTINE h5pget_obj_track_times_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! Dataset creation property 
                                           ! list identifier 
    LOGICAL, INTENT(OUT) :: flag   ! Object timestamp setting
    INTEGER, INTENT(OUT) :: hdferr ! Error code
                                   ! 0 on success and -1 on failure
!***** 
    INTEGER :: status
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_obj_track_times_c(plist_id, status)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_OBJ_TRACK_TIMES_C'::h5pget_obj_track_times_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id ! File creation property list identifier 
         INTEGER, INTENT(OUT) :: status
       END FUNCTION h5pget_obj_track_times_c
    END INTERFACE
    flag = .TRUE.
    hdferr = h5pget_obj_track_times_c(plist_id, status)
    IF(status.EQ.0) flag = .FALSE.

  END SUBROUTINE h5pget_obj_track_times_f

!****s* H5P/H5Pset_obj_track_times_f 
! NAME
!  H5Pset_obj_track_times_f 
!
! PURPOSE
!  Set whether the birth, access, modification & change times for
!  an object are stored.
!
!  Birth time is the time the object was created.  Access time is
!  the last time that metadata or raw data was read from this
!  object.  Modification time is the last time the data for
!  this object was changed (either writing raw data to a dataset
!  or inserting/modifying/deleting a link in a group).  Change
!  time is the last time the metadata for this object was written
!  (adding/modifying/deleting an attribute on an object, extending
!  the size of a dataset, etc).
!
!   If these times are not tracked, they will be reported as
!   12:00 AM UDT, Jan. 1, 1970 (i.e. 0 seconds past the UNIX
!   epoch) when queried.
!
! INPUTS
!  
!  plist_id	- property list id
!  flag         - object timestamp setting
!                 .TRUE.,.FALSE.
! OUTPUTS
!
!  hdferr       - error code		
!	           Success:  0
!	           Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  February 22, 2008
!	
!  
! Fortran90 Interface:  
  SUBROUTINE h5pset_obj_track_times_f(plist_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! Dataset creation property
                                           ! list identifier
    LOGICAL, INTENT(IN) :: flag    ! Object timestamp setting
    INTEGER, INTENT(OUT) :: hdferr ! Error code
                                   ! 0 on success and -1 on failure
!***** 
    INTEGER :: status
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_obj_track_times_c(plist_id, status)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_OBJ_TRACK_TIMES_C'::h5pset_obj_track_times_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id ! File creation property list identifier 
         INTEGER, INTENT(IN) :: status
       END FUNCTION h5pset_obj_track_times_c
    END INTERFACE

    status = 0
    IF(flag) status = 1

    hdferr = h5pset_obj_track_times_c(plist_id, status)

  END SUBROUTINE h5pset_obj_track_times_f

!****s* H5P/H5Pset_create_inter_group_f
! NAME
!  H5Pset_create_inter_group_f
!
! PURPOSE
!  Specifies in property list whether to create missing intermediate groups.
!
! INPUTS
!  
!  lcpl_id            - Link creation property list identifier
!  crt_intermed_group - crt_intermed_group specifying whether 
!                       to create intermediate groups upon the creation 
!                       of an object
! OUTPUTS
!
!  hdferr	      - Error code		
!  		 	  Success:  0
!  		 	  Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  February 22, 2008
!
! HISTORY	
! The long subroutine name (>31) on older f90 compilers causes problems
!          so had to shorten the name
! Fortran90 Interface:
  SUBROUTINE h5pset_create_inter_group_f(lcpl_id, crt_intermed_group, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: lcpl_id      ! Link creation property list identifier
    INTEGER, INTENT(IN) :: crt_intermed_group  ! specifying whether to create intermediate groups
                                               ! upon the creation of an object
    INTEGER, INTENT(OUT) :: hdferr ! Error code
                                   ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_create_inter_group_c(lcpl_id, crt_intermed_group)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_CREATE_INTER_GROUP_C'::h5pset_create_inter_group_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: lcpl_id
         INTEGER, INTENT(IN) :: crt_intermed_group
       END FUNCTION h5pset_create_inter_group_c
    END INTERFACE

    hdferr = h5pset_create_inter_group_c(lcpl_id, crt_intermed_group)

  END SUBROUTINE h5pset_create_inter_group_f

!****s* H5P/H5Pget_link_creation_order_f
! NAME
!  H5Pget_link_creation_order_f
!
! PURPOSE
!  Queries whether link creation order is tracked and/or indexed in a group.
!
! INPUTS
!
!  gcpl_id - Group creation property list identifier
!
! OUTPUTS
!
!  crt_order_flags - Creation order flag(s)
!  hdferr	   - Error code		
!  		      Success:  0
!  		      Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 3, 2008
!
! Fortran90 Interface:  
  SUBROUTINE h5pget_link_creation_order_f(gcpl_id, crt_order_flags, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id   ! Group creation property list identifier
    INTEGER, INTENT(OUT) :: crt_order_flags ! Creation order flag(s)
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
                                            ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_link_creation_order_c(gcpl_id, crt_order_flags)

         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_LINK_CREATION_ORDER_C'::h5pget_link_creation_order_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER, INTENT(OUT) :: crt_order_flags

       END FUNCTION H5pget_link_creation_order_c
    END INTERFACE

    hdferr = h5pget_link_creation_order_c(gcpl_id, crt_order_flags)

  END SUBROUTINE h5pget_link_creation_order_f

!****s* H5P/H5Pset_char_encoding_f
! NAME
!  H5Pset_char_encoding_f
!
! PURPOSE
!  Sets the character encoding used to encode a string.
!
! INPUTS
!
!  plist_id - Property list identifier
!  encoding - Valid values for encoding are:
!     	        H5T_CSET_ASCII_F -> US ASCII
!     	        H5T_CSET_UTF8_F -> UTF-8 Unicode encoding
!
! OUTPUTS
!  hdferr   - Error code		
!  	        Success:  0
!  		Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 3, 2008
!
! Fortran90 Interface:  
  SUBROUTINE h5pset_char_encoding_f(plist_id, encoding, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! Property list identifier
    INTEGER, INTENT(IN) :: encoding        ! String encoding character set:
     	                                   !   H5T_CSET_ASCII_F -> US ASCII
     	                                   !   H5T_CSET_UTF8_F  -> UTF-8 Unicode encoding
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
                                           ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_char_encoding_c(plist_id, encoding)

         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_CHAR_ENCODING_C'::h5pset_char_encoding_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(IN) :: encoding

       END FUNCTION H5pset_char_encoding_c
    END INTERFACE

    hdferr = h5pset_char_encoding_c(plist_id, encoding)

  END SUBROUTINE h5pset_char_encoding_f

!****s* H5P/H5Pget_char_encoding_f
! NAME
!  H5Pget_char_encoding_f
!
! PURPOSE
!  Retrieves the character encoding used to create a string
!
! INPUTS
!
!  plist_id - Property list identifier
!
! OUTPUTS
!
!  encoding - Valid values for encoding are:
!     	        H5T_CSET_ASCII_F -> US ASCII
!     	        H5T_CSET_UTF8_F -> UTF-8 Unicode encoding
!  hdferr   - Error code		
!               Success:  0
!               Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 3, 2008
!
! Fortran90 Interface:  
  SUBROUTINE  h5pget_char_encoding_f(plist_id, encoding, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id ! Property list identifier

    INTEGER, INTENT(OUT) :: encoding       ! Valid values for encoding are:
     	                                   !  H5T_CSET_ASCII_F -> US ASCII
     	                                   !  H5T_CSET_UTF8_F  -> UTF-8 Unicode encoding
    INTEGER, INTENT(OUT) :: hdferr   ! Error code
                                     ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_char_encoding_c(plist_id, encoding)

         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_CHAR_ENCODING_C'::h5pget_char_encoding_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(OUT) :: encoding

       END FUNCTION H5pget_char_encoding_c
    END INTERFACE

    hdferr = h5pget_char_encoding_c(plist_id, encoding)

  END SUBROUTINE h5pget_char_encoding_f

!****s* H5P/h5pset_copy_object_f
! NAME
!  h5pset_copy_object_f 
!
! PURPOSE
!  Sets properties to be used when an object is copied.
!
! INPUTS
! 
!  ocp_plist_id - Object copy property list identifier
!  copy_options - Copy option(s) to be set
! OUTPUTS
!  
!  hdferr	- error code		
!  		   Success:  0
!  		   Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 3, 2008
!
! HISTORY
!
!  
! Fortran90 Interface:  
  SUBROUTINE h5pset_copy_object_f(ocp_plist_id, copy_options, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocp_plist_id ! Object copy property list identifier
    INTEGER, INTENT(IN) :: copy_options ! Copy option(s) to be set, valid options are:
                                        !   H5O_COPY_SHALLOW_HIERARCHY_F
                                        !   H5O_COPY_EXPAND_SOFT_LINK_F
                                        !   H5O_COPY_EXPAND_EXT_LINK_F
                                        !   H5O_COPY_EXPAND_REFERENCE_F
                                        !   H5O_COPY_WITHOUT_ATTR_FLAG_F
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
                                        ! 0 on success and -1 on failure
!***** 

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_copy_object_c(ocp_plist_id, copy_options)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_COPY_OBJECT_C'::h5pset_copy_object_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: ocp_plist_id
         INTEGER, INTENT(IN) :: copy_options
       END FUNCTION h5pset_copy_object_c
    END INTERFACE
    hdferr = h5pset_copy_object_c(ocp_plist_id, copy_options)
  END SUBROUTINE h5pset_copy_object_f

!****s* H5P/h5pget_copy_object_f
! NAME
!  h5pget_copy_object_f 
!
! PURPOSE
!  Retrieves the properties to be used when an object is copied.
!
! INPUTS
! 
!  ocp_plist_id - Object copy property list identifier
! OUTPUTS
!  
!  copy_options - Copy option(s) to be get
!  hdferr	- Error code		
!  		   Success:  0
!  		   Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 3, 2008
!
! HISTORY
!
!  
! Fortran90 Interface:
  SUBROUTINE h5pget_copy_object_f(ocp_plist_id, copy_options, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocp_plist_id ! Object copy property list identifier
    INTEGER, INTENT(OUT) :: copy_options       ! Valid copy options returned are:
                                               !   H5O_COPY_SHALLOW_HIERARCHY_F
                                               !   H5O_COPY_EXPAND_SOFT_LINK_F 
                                               !   H5O_COPY_EXPAND_EXT_LINK_F
                                               !   H5O_COPY_EXPAND_REFERENCE_F
                                               !   H5O_COPY_WITHOUT_ATTR_FLAG_F
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
                                               ! 0 on success and -1 on failure
!***** 

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_copy_object_c(ocp_plist_id, copy_options)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_COPY_OBJECT_C'::h5pget_copy_object_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: ocp_plist_id
         INTEGER, INTENT(OUT) :: copy_options
       END FUNCTION h5pget_copy_object_c
    END INTERFACE
    hdferr = h5pget_copy_object_c(ocp_plist_id, copy_options)
  END SUBROUTINE h5pget_copy_object_f

!****s* H5P/h5pget_data_transform_f 
! NAME
!  h5pget_data_transform_f 
!
! PURPOSE
!  Retrieves a data transform expression.
!
! INPUTS
! 
!  plist_id   - Identifier of the property list or class
! OUTPUTS
!  
!  expression - buffer to hold transform expression
!  hdferr     - Error code
!                 Success:  Actual lenght of the expression
!                           If provided buffer "expression" is 
!                           smaller, than expression will be 
!                           truncated to fit into
!                           provided user buffer
!  		  Failure: -1
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 19, 2008
!
! HISTORY
!
! Should hdferr return just 0 or 1 and add another arguement for the size?
! Fortran90 Interface:
SUBROUTINE h5pget_data_transform_f(plist_id, expression, hdferr, size)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id         ! Identifier of the property list or class
    CHARACTER(LEN=*), INTENT(OUT) :: expression    ! Buffer to hold transform expression
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: size ! Registered size of the transform expression
    INTEGER, INTENT(OUT) :: hdferr                 ! Error code
                                                   !  0 on success and -1 on failure
!***** 
    INTEGER :: expression_len
    INTEGER(SIZE_T) :: size_default


!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_data_transform_c(plist_id, expression, expression_len, size_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_DATA_TRANSFORM_C'::h5pget_data_transform_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: expression
         INTEGER(HID_T), INTENT(IN) :: plist_id 
         CHARACTER(LEN=*), INTENT(OUT) :: expression 
         INTEGER(SIZE_T) :: size_default
         INTEGER :: expression_len
       END FUNCTION h5pget_data_transform_c
    END INTERFACE

    size_default = 0
    expression_len = LEN(expression)

    hdferr = h5pget_data_transform_c(plist_id, expression, expression_len, size_default)

    IF(present(size)) size = size_default

  END SUBROUTINE h5pget_data_transform_f

!****s* H5P/h5pset_data_transform_f 
! NAME
!  h5pset_data_transform_f 
!
! PURPOSE
!  Sets a data transform expression.
!
! INPUTS
! 
!  plist_id   - Identifier of the property list or class 
!  expression - Buffer to hold transform expression
! OUTPUTS
! 
!  hdferr     - error code
!                 Success:  0
!  		  Failure: -1
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 19, 2008
!
! Fortran90 Interface:
  SUBROUTINE h5pset_data_transform_f(plist_id, expression, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist_id     ! Identifier of the property list or class
    CHARACTER(LEN=*), INTENT(IN) :: expression ! Buffer to hold transform expression
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
                                               ! 0 on success and -1 on failure
!***** 
    INTEGER :: expression_len

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_data_transform_c(plist_id, expression, expression_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_DATA_TRANSFORM_C'::h5pset_data_transform_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: expression
         INTEGER(HID_T), INTENT(IN) :: plist_id 
         CHARACTER(LEN=*), INTENT(IN) :: expression
         INTEGER :: expression_len
       END FUNCTION h5pset_data_transform_c
    END INTERFACE

    expression_len = LEN(expression)
    hdferr = h5pset_data_transform_c(plist_id, expression, expression_len)

  END SUBROUTINE h5pset_data_transform_f

!****s* H5P/H5Pget_local_heap_size_hint_f 
! NAME
!  H5Pget_local_heap_size_hint_f 
!
! PURPOSE
!  Queries the local heap size hint for original-style groups.
!
! INPUTS
! 
!  gcpl_id   - Group creation property list identifier
! OUTPUTS
!
!  size_hint - Hint for size of local heap
!  hdferr    - Error code
!               Success:  0
!  		Failure: -1
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 21, 2008
!
! Fortran90 Interface:
  SUBROUTINE h5pget_local_heap_size_hint_f(gcpl_id, size_hint, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id     ! Group creation property list identifier
    INTEGER(SIZE_T), INTENT(OUT) :: size_hint ! Hint for size of local heap
    INTEGER, INTENT(OUT) :: hdferr            ! Error code
                                              ! 0 on success and -1 on failure
!***** 

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_local_heap_size_hint_c(gcpl_id, size_hint)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_LOCAL_HEAP_SIZE_HINT_C'::h5pget_local_heap_size_hint_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER(SIZE_T), INTENT(OUT) :: size_hint
       END FUNCTION H5Pget_local_heap_size_hint_c
    END INTERFACE

    hdferr = H5Pget_local_heap_size_hint_c(gcpl_id, size_hint)

  END SUBROUTINE h5pget_local_heap_size_hint_f

!****s* H5P/H5Pget_est_link_info_f 
! NAME
!  H5Pget_est_link_info_f 
!
! PURPOSE
!  Queries data required to estimate required local heap or object header size.
!
! INPUTS
! 
!  gcpl_id         - Group creation property list identifier
! OUTPUTS
!  
!  est_num_entries - Estimated number of links to be inserted into group
!  est_name_len    - Estimated average length of link names
!  hdferr	   - Error code
!                     Success:  0
!  		      Failure: -1
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 21, 2008
!
! HISTORY
!
! 
! Fortran90 Interface:
  SUBROUTINE h5pget_est_link_info_f(gcpl_id, est_num_entries, est_name_len, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id   ! Group creation property list identifier  
    INTEGER, INTENT(OUT) :: est_num_entries ! Estimated number of links to be inserted into group
    INTEGER, INTENT(OUT) :: est_name_len    ! Estimated average length of link names
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
                                            ! 0 on success and -1 on failure
!***** 

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_est_link_info_c(gcpl_id, est_num_entries, est_name_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_EST_LINK_INFO_C'::h5pget_est_link_info_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: gcpl_id  
         INTEGER, INTENT(OUT) :: est_num_entries
         INTEGER, INTENT(OUT) :: est_name_len
       END FUNCTION h5pget_est_link_info_c
    END INTERFACE

    hdferr = h5pget_est_link_info_c(gcpl_id, est_num_entries, est_name_len)

  END SUBROUTINE h5pget_est_link_info_f

!****s* H5P/H5Pset_local_heap_size_hint_f 
! NAME
!  H5Pset_local_heap_size_hint_f 
!
! PURPOSE
!  Sets the local heap size hint for original-style groups.
!
! INPUTS
! 
!  gcpl_id   - Group creation property list identifier
!  size_hint - Hint for size of local heap
! OUTPUTS
!
!  hdferr    - Error code
!               Success:  0
!  		Failure: -1
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 21, 2008
!
! Fortran90 Interface:
  SUBROUTINE h5pset_local_heap_size_hint_f(gcpl_id, size_hint, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id    ! Group creation property list identifier
    INTEGER(SIZE_T), INTENT(IN) :: size_hint ! Hint for size of local heap
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
                                             ! 0 on success and -1 on failure
!***** 

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_local_heap_size_hint_c(gcpl_id, size_hint)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_LOCAL_HEAP_SIZE_HINT_C'::h5pset_local_heap_size_hint_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER(SIZE_T), INTENT(IN) :: size_hint
       END FUNCTION h5pset_local_heap_size_hint_c
    END INTERFACE

    hdferr = H5Pset_local_heap_size_hint_c(gcpl_id, size_hint)

  END SUBROUTINE h5pset_local_heap_size_hint_f

!****s* H5P/h5pset_est_link_info_f 
! NAME
!  h5pset_est_link_info_f 
!
! PURPOSE
!  Sets estimated number of links and length of link names in a group.
!
! INPUTS
! 
! gcpl_id         - Group creation property list identifier  
! est_num_entries - Estimated number of links to be inserted into group
! est_name_len    - Estimated average length of link names
! OUTPUTS
!
!  hdferr	  - Error code
!                    Success:  0
!  		     Failure: -1
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 21, 2008
!
! Fortran90 Interface:
  SUBROUTINE h5pset_est_link_info_f(gcpl_id, est_num_entries, est_name_len, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id  ! Group creation property list identifier  
    INTEGER, INTENT(IN) :: est_num_entries ! Estimated number of links to be inserted into group
    INTEGER, INTENT(IN) :: est_name_len    ! Estimated average length of link names
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
                                           ! 0 on success and -1 on failure
!***** 

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_est_link_info_c(gcpl_id, est_num_entries, est_name_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_EST_LINK_INFO_C'::h5pset_est_link_info_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: gcpl_id  
         INTEGER, INTENT(IN) :: est_num_entries
         INTEGER, INTENT(IN) :: est_name_len
       END FUNCTION h5pset_est_link_info_c
    END INTERFACE

    hdferr = H5Pset_est_link_info_c(gcpl_id, est_num_entries, est_name_len)

  END SUBROUTINE h5pset_est_link_info_f

!****s* H5P/h5pset_link_phase_change_f
! NAME
!  h5pset_link_phase_change_f
!
! PURPOSE
!  Sets the parameters for conversion between compact and dense groups.
!
! INPUTS
!  
!  gcpl_id         - Group creation property list identifier  
!  max_compact     - Maximum number of attributes to be stored in compact storage
!  min_dense       - Minimum number of attributes to be stored in dense storage
! OUTPUTS
!
!  hdferr          - error code		
!	              Success:  0
!	              Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 21, 2008
!
! Fortran90 Interface:
SUBROUTINE h5pset_link_phase_change_f(gcpl_id, max_compact, min_dense, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: gcpl_id ! Group creation property list identifier
    INTEGER, INTENT(IN) :: max_compact    ! Maximum number of attributes to be stored in compact storage
    INTEGER, INTENT(IN) :: min_dense      ! Minimum number of attributes to be stored in dense storage
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_link_phase_change_c(gcpl_id, max_compact, min_dense)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_LINK_PHASE_CHANGE_C'::h5pset_link_phase_change_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: gcpl_id
         INTEGER, INTENT(IN) :: max_compact
         INTEGER, INTENT(IN) :: min_dense

       END FUNCTION h5pset_link_phase_change_c
    END INTERFACE

    hdferr = h5pset_link_phase_change_c(gcpl_id, max_compact, min_dense)
  END SUBROUTINE h5pset_link_phase_change_f

!****s* H5P/h5pset_fapl_direct_f
! NAME
!  h5pset_fapl_direct_f
!
! PURPOSE
!  Sets up use of the direct I/O driver.
!
! INPUTS
!  
!  fapl_id 	- File access property list identifier
!  alignment 	- Required memory alignment boundary
!  block_size   - File system block size
!  cbuf_size 	- Copy buffer size
! OUTPUTS
!
!  hdferr       - error code		
!                  Success:  0
!                  Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 21, 2008
!
! Fortran90 Interface:
SUBROUTINE h5pset_fapl_direct_f(fapl_id, alignment, block_size, cbuf_size, hdferr)
    IMPLICIT NONE  
    INTEGER(HID_T), INTENT(IN) :: fapl_id     ! File access property list identifier
    INTEGER(SIZE_T), INTENT(IN) :: alignment  ! Required memory alignment boundary!
    INTEGER(SIZE_T), INTENT(IN) :: block_size ! File system block size
    INTEGER(SIZE_T), INTENT(IN) :: cbuf_size  ! Copy buffer size
    INTEGER, INTENT(OUT) :: hdferr            ! Error code
                                              ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_fapl_direct_c(fapl_id, alignment, block_size, cbuf_size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FAPL_DIRECT_C'::h5pset_fapl_direct_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: fapl_id 
         INTEGER(SIZE_T), INTENT(IN) :: alignment
         INTEGER(SIZE_T), INTENT(IN) :: block_size
         INTEGER(SIZE_T), INTENT(IN) :: cbuf_size
       END FUNCTION h5pset_fapl_direct_c
    END INTERFACE

    hdferr = H5Pset_fapl_direct_c(fapl_id, alignment, block_size, cbuf_size)
  END SUBROUTINE h5pset_fapl_direct_f

!****s* H5P/h5pget_fapl_direct_f
! NAME
!  h5pget_fapl_direct_f
!
! PURPOSE
!  Gets up use of the direct I/O driver.
!
! INPUTS
!  
!  fapl_id 	- File access property list identifier
! OUTPUTS
!
!  alignment 	- Required memory alignment boundary
!  block_size   - File system block size
!  cbuf_size 	- Copy buffer size
!  hdferr       - error code		
!                  Success:  0
!                  Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 21, 2008
!
! Fortran90 Interface:
  SUBROUTINE h5pget_fapl_direct_f(fapl_id, alignment, block_size, cbuf_size, hdferr)
    IMPLICIT NONE  
    INTEGER(HID_T), INTENT(IN) :: fapl_id       ! File access property list identifier
    INTEGER(SIZE_T), INTENT(OUT) :: alignment   ! Required memory alignment boundary!
    INTEGER(SIZE_T), INTENT(OUT) :: block_size  ! File system block size
    INTEGER(SIZE_T), INTENT(OUT) :: cbuf_size 	! Copy buffer size
    INTEGER, INTENT(OUT) :: hdferr              ! Error code
                                                ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_fapl_direct_c(fapl_id, alignment, block_size, cbuf_size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FAPL_DIRECT_C'::h5pget_fapl_direct_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: fapl_id 
         INTEGER(SIZE_T), INTENT(OUT) :: alignment
         INTEGER(SIZE_T), INTENT(OUT) :: block_size
         INTEGER(SIZE_T), INTENT(OUT) :: cbuf_size
       END FUNCTION h5pget_fapl_direct_c
    END INTERFACE

    hdferr = H5Pget_fapl_direct_c(fapl_id, alignment, block_size, cbuf_size)
  END SUBROUTINE h5pget_fapl_direct_f

!****s* H5P/H5Pset_attr_phase_change_f
! NAME
!  H5Pset_attr_phase_change_f 
!
! PURPOSE
!  Sets attribute storage phase change thresholds.
!
! INPUTS
!  
!  ocpl_id - Object (dataset or group) creation property list identifier
! OUTPUTS
!  
!  max_compact     - Maximum number of attributes to be stored in compact storage
!                    (Default: 8)
!  min_dense       - Minimum number of attributes to be stored in dense storage
!                    (Default: 6)
!  hdferr          - Error code		
!	              Success:  0
!	              Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! Fortran90 Interface:
SUBROUTINE h5pset_attr_phase_change_f(ocpl_id, max_compact, min_dense, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: ocpl_id ! Object (dataset or group) creation property list identifier
    INTEGER, INTENT(IN) :: max_compact    ! Maximum number of attributes to be stored in compact storage
                                          !(Default: 8)
    INTEGER, INTENT(IN) :: min_dense      ! Minimum number of attributes to be stored in dense storage
                                          ! (Default: 6)
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          ! 0 on success and -1 on failure
!***** 
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_attr_phase_change_c(ocpl_id, max_compact, min_dense)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_ATTR_PHASE_CHANGE_C'::h5pset_attr_phase_change_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: ocpl_id
         INTEGER, INTENT(IN) :: max_compact
         INTEGER, INTENT(IN) :: min_dense

       END FUNCTION h5pset_attr_phase_change_c
    END INTERFACE

    hdferr = h5pset_attr_phase_change_c(ocpl_id, max_compact, min_dense)


  END SUBROUTINE h5pset_attr_phase_change_f

!****s* H5P/H5Pset_nbit_f
! NAME
!  H5Pset_nbit_f 
!
! PURPOSE
!  Sets up the use of the N-Bit filter.
!
! Inputs:
!  plist_id - Dataset creation property list identifier.
!
! Outputs:
!  hdferr   - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 21, 2008
!
! Fortran90 Interface:
  SUBROUTINE h5pset_nbit_f(plist_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: plist_id
    INTEGER       , INTENT(OUT) :: hdferr
!*****
    INTERFACE
       INTEGER FUNCTION H5Pset_nbit_c(plist_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_NBIT_C'::h5pset_nbit_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
       END FUNCTION H5Pset_nbit_c
    END INTERFACE

    hdferr = H5Pset_nbit_c(plist_id)

  END SUBROUTINE h5pset_nbit_f

!****s* H5P/h5pset_scaleoffset_f
! NAME
!  h5pset_scaleoffset_f 
!
! PURPOSE
!  Sets up the use of the scale-offset filter.
!
! Inputs:
!  plist_id     - Dataset creation property list identifier.
!  scale_type   - Flag indicating compression method. Valid values:
!                    H5Z_SO_FLOAT_DSCALE_F
!                    H5Z_SO_FLOAT_ESCALE_F
!                    H5Z_SO_INT_F
!
!  scale_factor - Parameter related to scale.
!
! Outputs:
!  hdferr       - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 21, 2008
!
! Fortran90 Interface:
  SUBROUTINE h5pset_scaleoffset_f(plist_id, scale_type, scale_factor, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: plist_id
    INTEGER       , INTENT(IN)  :: scale_type
    INTEGER       , INTENT(IN)  :: scale_factor
    INTEGER       , INTENT(OUT) :: hdferr
!***** 

    INTERFACE
       INTEGER FUNCTION h5pset_scaleoffset_c(plist_id, scale_type, scale_factor)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_SCALEOFFSET_C'::h5pset_scaleoffset_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: plist_id
         INTEGER, INTENT(IN) :: scale_type
         INTEGER, INTENT(IN) :: scale_factor
       END FUNCTION h5pset_scaleoffset_c
    END INTERFACE

    hdferr = H5Pset_scaleoffset_c(plist_id, scale_type, scale_factor)

  END SUBROUTINE h5pset_scaleoffset_f

!****s* H5P/h5pset_nlinks_f 
! NAME
!  h5pset_nlinks_f 
!
! PURPOSE
!  Sets maximum number of soft or user-defined link traversals.
!
! INPUTS
! 
!  lapl_id - File access property list identifier
!   nlinks - Maximum number of links to traverse
!
! OUTPUTS
!
!  hdferr  - Error code
!             Success:  0
!  	      Failure: -1
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 24, 2008
!
! HISTORY
!
! 
! Fortran90 Interface:
  SUBROUTINE h5pset_nlinks_f(lapl_id, nlinks, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: lapl_id ! File access property list identifier
    INTEGER(SIZE_T), INTENT(IN) :: nlinks ! Maximum number of links to traverse
    INTEGER, INTENT(OUT) :: hdferr        ! Error code   
                                          ! 0 on success and -1 on failure
!*****      

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pset_nlinks_c(lapl_id, nlinks)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_NLINKS_C'::h5pset_nlinks_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: lapl_id
         INTEGER(SIZE_T), INTENT(IN) :: nlinks
       END FUNCTION h5pset_nlinks_c
    END INTERFACE

    hdferr = h5pset_nlinks_c(lapl_id, nlinks)

  END SUBROUTINE h5pset_nlinks_f

!****s* H5P/h5pget_nlinks_f 
! NAME
!  h5pget_nlinks_f 
!
! PURPOSE
!  Gets maximum number of soft or user-defined link traversals.
!
! INPUTS
! 
!  lapl_id - File access property list identifier
!  nlinks  - Maximum number of links to traverse
!
! OUTPUTS
!
!  hdferr  - error code
!             Success:  0
!  	      Failure: -1
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 24, 2008
!
! Fortran90 Interface:
  SUBROUTINE h5pget_nlinks_f(lapl_id, nlinks, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: lapl_id  ! File access property list identifier
    INTEGER(SIZE_T), INTENT(OUT) :: nlinks ! Maximum number of links to traverse
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
                                           ! 0 on success and -1 on failure
!***** 
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5pget_nlinks_c(lapl_id, nlinks)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_NLINKS_C'::h5pget_nlinks_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: lapl_id
         INTEGER(SIZE_T), INTENT(OUT) :: nlinks
       END FUNCTION h5pget_nlinks_c
    END INTERFACE

    hdferr = h5pget_nlinks_c(lapl_id, nlinks)

  END SUBROUTINE h5pget_nlinks_f

!****s* H5P/H5Pget_create_inter_group_f
! NAME
!  H5Pget_create_inter_group_f
!
! PURPOSE
!  Determines whether property is set to enable creating missing intermediate groups.
!
! INPUTS
!  
!  lcpl_id            - Link creation property list identifier
!  crt_intermed_group - Specifying whether to create intermediate groups upon 
!                       the creation of an object
! OUTPUTS
!
!  hdferr	      - Error code		
!  		 	 Success:  0
!  		 	 Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  April 4, 2008
!
! HISTORY
! 	
! The long subroutine name (>31) on older f90 compilers causes problems
!          so the name was shortened
! Fortran90 Interface:
  SUBROUTINE h5pget_create_inter_group_f(lcpl_id, crt_intermed_group, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: lcpl_id      ! Link creation property list identifier
    INTEGER, INTENT(IN) :: crt_intermed_group  ! Flag specifying whether to create intermediate groups
                                               ! upon creation of an object
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
                                               ! 0 on success and -1 on failure
!***** 
    INTERFACE
       INTEGER FUNCTION h5pget_create_inter_group_c(lcpl_id, crt_intermed_group)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_CREATE_INTER_GROUP_C'::h5pget_create_inter_group_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: lcpl_id
         INTEGER, INTENT(IN) :: crt_intermed_group
       END FUNCTION h5pget_create_inter_group_c
    END INTERFACE

    hdferr = h5pget_create_inter_group_c(lcpl_id, crt_intermed_group)

  END SUBROUTINE h5pget_create_inter_group_f

!****s* H5P/H5Pset_chunk_cache_f
! NAME
!  H5Pset_chunk_cache_f
!
! PURPOSE
!  Set the number of objects in the meta data cache and the
!  maximum number of chunks and bytes in the raw data chunk cache.
!  Once set, these values will override the values in the file access
!  property list.  Each of these values can be individually unset
!  (or not set at all) by passing the macros:
!    H5D_CHUNK_CACHE_NSLOTS_DFLT_F,
!    H5D_CHUNK_CACHE_NBYTES_DFLT_F, and/or
!    H5D_CHUNK_CACHE_W0_DFLT_F
!    as appropriate.
!
!  The RDCC_W0 value should be between 0 and 1 inclusive and
!  indicates how much chunks that have been fully read or fully
!  written are favored for preemption.  A value of zero means
!  fully read or written chunks are treated no differently than
!  other chunks (the preemption is strictly LRU) while a value
!  of one means fully read chunks are always preempted before
!  other chunks.
!
! INPUTS
!  
!  dapl_id          - Dataset access property list identifier.
!  rdcc_nslots      - The number of chunk slots in the raw data chunk cache for this dataset.
!  rdcc_nbytes      - The total size of the raw data chunk cache for this dataset.
!  rdcc_w0          - The chunk preemption policy for this dataset.
! OUTPUTS
!
!  hdferr	    - Error code		
!  		 	Success:  0
!  		 	Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  April 13, 2009
!
! HISTORY
!
! Fortran90 Interface:
  SUBROUTINE h5pset_chunk_cache_f(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dapl_id      ! Dataset access property list identifier.
    INTEGER(SIZE_T), INTENT(IN) :: rdcc_nslots ! The number of chunk slots in the raw data 
                                               ! chunk cache for this dataset.
    INTEGER(SIZE_T), INTENT(IN) :: rdcc_nbytes ! The total size of the raw data chunk cache 
                                               ! for this dataset.
    REAL, INTENT(IN) :: rdcc_w0                ! The chunk preemption policy for this dataset.
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
                                               ! 0 on success and -1 on failure
!*****       

    INTERFACE
       INTEGER FUNCTION h5pset_chunk_cache_c(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_CHUNK_CACHE_C'::h5pset_chunk_cache_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dapl_id
         INTEGER(SIZE_T), INTENT(IN) :: rdcc_nslots
         INTEGER(SIZE_T), INTENT(IN) :: rdcc_nbytes
         REAL, INTENT(IN) :: rdcc_w0
       END FUNCTION h5pset_chunk_cache_c
    END INTERFACE

    hdferr = h5pset_chunk_cache_c(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0)

  END SUBROUTINE h5pset_chunk_cache_f

!****s* H5P/H5Pget_chunk_cache_f
! NAME
!  H5Pget_chunk_cache_f
!
! PURPOSE
!  Retrieves the maximum possible number of elements in the meta
!  data cache and the maximum possible number of elements and
!  bytes and the RDCC_W0 value in the raw data chunk cache.  Any
!  (or all) arguments may be null pointers in which case the
!  corresponding datum is not returned.  If these properties have
!  not been set on this property list, the default values for a
!  file access property list are returned.
!
! INPUTS
!  
!  dapl_id            - Dataset access property list identifier.
! OUTPUTS
! 
!  rdcc_nslots        - Number of chunk slots in the raw data chunk cache hash table. 
!  rdcc_nbytes        - Total size of the raw data chunk cache, in bytes. 
!  rdcc_w0            - Preemption policy. 
!  hdferr	      - Error code		
!  		 	 Success:  0
!  		 	 Failure: -1   
!
! AUTHOR
!  M. Scot Breitenfeld
!  April 13, 2009
!
! HISTORY
!
! Fortran90 Interface:
  SUBROUTINE h5pget_chunk_cache_f(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dapl_id       ! Dataset access property list identifier.
    INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nslots ! Number of chunk slots in the raw data chunk cache hash table.
    INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nbytes ! Total size of the raw data chunk cache, in bytes. 
    REAL, INTENT(OUT) :: rdcc_w0                ! Preemption policy.
    INTEGER, INTENT(OUT) :: hdferr              ! Error code:
                                                ! 0 on success and -1 on failure
!*****    
    INTERFACE
       INTEGER FUNCTION h5pget_chunk_cache_c(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_CHUNK_CACHE_C'::h5pget_chunk_cache_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dapl_id
         INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nslots
         INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nbytes
         REAL, INTENT(OUT) :: rdcc_w0
       END FUNCTION h5pget_chunk_cache_c
    END INTERFACE

    hdferr = h5pget_chunk_cache_c(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0)

  END SUBROUTINE h5pget_chunk_cache_f

END MODULE H5P



