! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
!   access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!
! This file contains Fortran90 interfaces for H5P functions.
!
     MODULE H5P

       USE H5GLOBAL

       INTERFACE h5pset_fill_value_f
         MODULE PROCEDURE h5pset_fill_value_integer
         MODULE PROCEDURE h5pset_fill_value_real
! Comment if on T3E
         MODULE PROCEDURE h5pset_fill_value_double
! End comment if on T3E
         MODULE PROCEDURE h5pset_fill_value_char
       END INTERFACE
     
       INTERFACE h5pget_fill_value_f
         MODULE PROCEDURE h5pget_fill_value_integer
         MODULE PROCEDURE h5pget_fill_value_real
! Comment if on T3E
         MODULE PROCEDURE h5pget_fill_value_double
! End comment if on T3E
         MODULE PROCEDURE h5pget_fill_value_char
       END INTERFACE

     CONTAINS

!----------------------------------------------------------------------
! Name:		h5pcreate_f 
!
! Purpose: 	Creates a new property as an instance of a property 
!		list class.
!
! Inputs:  
!		classtype	- type of the property list to be created.
!				  Possible values are:
!				  H5P_FILE_CREATE_F
!				  H5P_FILE_ACCESS_F
!				  H5P_DATASET_CREATE_F
!				  H5P_DATASET_XFER_F
!				  H5P_MOUNT_F
! Outputs:  
!		prp_id		- property list identifier
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------
          SUBROUTINE h5pcreate_f(classtype, prp_id, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pcreate_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: classtype  ! The type of the property list 
                                              ! to be created. Possible values
                                              ! are: 
                                              !  H5P_FILE_CREATE_F
                                              !  H5P_FILE_ACCESS_F
                                              !  H5P_DATASET_CREATE_F
                                              !  H5P_DATASET_XFER_F
                                              !  H5P_MOUNT_F
            INTEGER(HID_T), INTENT(OUT) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5pcreate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pcreate_c(classtype, prp_id)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PCREATE_C'::h5pcreate_c
              !DEC$ ENDIF
              INTEGER, INTENT(IN) :: classtype
              INTEGER(HID_T), INTENT(OUT) :: prp_id
              END FUNCTION h5pcreate_c
            END INTERFACE

            hdferr = h5pcreate_c(classtype, prp_id) 
          END SUBROUTINE h5pcreate_f

!----------------------------------------------------------------------
! Name:		h5pset_preserve_f 
!
! Purpose: 	Sets the dataset transfer property list status to 
!		TRUE or FALSE for initializing compound datatype
!		members during write/read operations.
!
! Inputs:  
!		prp_id		- property list identifier
!		flag		- status flag
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pset_preserve_f(prp_id, flag, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_preserve_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) ::  flag ! TRUE/FALSE flag to set the dataset
                                         ! transfer property for partila writing/reading
                                         ! compound datatype 
            INTEGER, INTENT(OUT) :: hdferr    ! Error code

!            INTEGER, EXTERNAL :: h5pset_preserve_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_preserve_c(prp_id, flag)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_PRESERVE_C'::h5pset_preserve_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(IN) ::  flag
              END FUNCTION h5pset_preserve_c
            END INTERFACE

            hdferr = h5pset_preserve_c(prp_id, flag) 
          END SUBROUTINE h5pset_preserve_f

!----------------------------------------------------------------------
! Name:		h5pget_preserve_f 
!
! Purpose: 	Checks status of the dataset transfer property list.
!
! Inputs:  
!		prp_id		- property list identifier
! Outputs:  
!		flag		- status flag
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_preserve_f(prp_id, flag, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_preserve_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) ::  flag ! TRUE/FALSE flag. Shows status of the dataset's
                                         ! transfer property for partial writing/reading
                                         ! compound datatype 
            INTEGER, INTENT(OUT) :: hdferr    ! Error code

!            INTEGER, EXTERNAL :: h5pget_preserve_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_preserve_c(prp_id, flag)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_PRESERVE_C'::h5pget_preserve_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(OUT) ::  flag
              END FUNCTION h5pget_preserve_c
            END INTERFACE

            hdferr = h5pget_preserve_c(prp_id, flag) 
          END SUBROUTINE h5pget_preserve_f

!----------------------------------------------------------------------
! Name:		h5pget_class_f 
!
! Purpose: 	Returns the property list class for a property list.
!
! Inputs:  
!		prp_id		- property list identifier
! Outputs:  
!		classtype	- property list class
!				  Possible values are:
!				  H5P_NO_CLASS
!				  H5P_FILE_CREATE_F
!				  H5P_FILE_ACCESS_F
!				  H5PE_DATASET_CREATE_F
!				  H5P_DATASET_XFER_F
!				  H5P_MOUNT_F
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_class_f(prp_id, classtype, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_class_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: classtype  ! The type of the property list 
                                              ! to be created. Possible values
                                              ! are: 
                                              !  H5P_NO_CLASS
                                              !  H5P_FILE_CREATE_F
                                              !  H5P_FILE_ACCESS_F
                                              !  H5PE_DATASET_CREATE_F 
                                              !  H5P_DATASET_XFER_F
                                              !  H5P_MOUNT_F
            INTEGER, INTENT(OUT) :: hdferr    ! Error code

!            INTEGER, EXTERNAL :: h5pget_class_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_class_c(prp_id, classtype)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_CLASS_C'::h5pget_class_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(OUT) :: classtype 
              END FUNCTION h5pget_class_c
            END INTERFACE

            hdferr = h5pget_class_c(prp_id, classtype) 
          END SUBROUTINE h5pget_class_f

!----------------------------------------------------------------------
! Name:		h5pcopy_f 
!
! Purpose: 	Copies an existing property list to create a new 
!		property list
!
! Inputs:  
!		prp_id		- property list identifier
! Outputs:  
!		new_prp_id	- new property list identifier
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pcopy_f(prp_id, new_prp_id, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pcopy_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(OUT) :: new_prp_id 
                                                ! Identifier  of property list
                                                ! copy  
            INTEGER, INTENT(OUT) :: hdferr      ! Error code

!            INTEGER, EXTERNAL :: h5pcopy_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pcopy_c(prp_id, new_prp_id)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PCOPY_C'::h5pcopy_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HID_T), INTENT(OUT) :: new_prp_id
              END FUNCTION h5pcopy_c
            END INTERFACE

            hdferr = h5pcopy_c(prp_id, new_prp_id)
          END SUBROUTINE h5pcopy_f

!----------------------------------------------------------------------
! Name:		h5pclose_f 
!
! Purpose: 	Terminates access to a property list. 
!
! Inputs:  
!		prp_id		- identifier of the property list to 
!				  terminate access to. 
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pclose_f(prp_id, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pclose_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier 
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5pclose_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pclose_c(prp_id)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PCLOSE_C'::h5pclose_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id 
              END FUNCTION h5pclose_c
            END INTERFACE

            hdferr = h5pclose_c(prp_id)
          END SUBROUTINE h5pclose_f

!----------------------------------------------------------------------
! Name:		h5pset_chunk_f 
!
! Purpose: 	Sets the size of the chunks used to store 
!		a chunked layout dataset. 
!
! Inputs:  
!		prp_id		- datatset creation property list identifier
!		ndims		- number of dimensions for each chunk
!		dims		- array with dimension sizes for each chunk
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pset_chunk_f(prp_id, ndims, dims, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_chunk_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: ndims    ! Number of chunk dimensions
            INTEGER(HSIZE_T), DIMENSION(ndims), INTENT(IN) :: dims    
                                            ! Array containing sizes of
                                            ! chunk dimensions
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_chunk_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_chunk_c(prp_id, ndims, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_CHUNK_C'::h5pset_chunk_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(IN) :: ndims
              INTEGER(HSIZE_T), DIMENSION(ndims), INTENT(IN) :: dims
              END FUNCTION h5pset_chunk_c
            END INTERFACE

            hdferr =  h5pset_chunk_c(prp_id, ndims, dims)
          END SUBROUTINE h5pset_chunk_f

!----------------------------------------------------------------------
! Name:		h5pget_chunk_f 
!
! Purpose: 	Retrieves the size of chunks for the raw data of a 
!		chunked layout dataset
!
! Inputs:  
!		prp_id		- property list identifier
!		ndims		- size of dims array
! Outputs:  
!		dims		- array with dimension sizes for each chunk
!		hdferr:		- error code		
!				 	Success:  number of chunk dimensions
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pget_chunk_f(prp_id, ndims, dims, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_chunk_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: ndims    ! Number of chunk dimensions to
                                            ! to return
            INTEGER(HSIZE_T), DIMENSION(ndims), INTENT(OUT) :: dims    
                                            ! Array containing sizes of
                                            ! chunk dimensions
            INTEGER, INTENT(OUT) :: hdferr  ! Error code; number of
                                            ! chunk dimensions on success,
                                            ! -1 on failure

!            INTEGER, EXTERNAL :: h5pget_chunk_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_chunk_c(prp_id, ndims, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_CHUNK_C'::h5pget_chunk_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER :: ndims
              INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: dims
              END FUNCTION h5pget_chunk_c
            END INTERFACE

            hdferr =  h5pget_chunk_c(prp_id, ndims, dims)
          END SUBROUTINE h5pget_chunk_f

!----------------------------------------------------------------------
! Name:		h5pset_deflate_f 
!
! Purpose: 	Sets compression method and compression level. 
!
! Inputs:  
!		prp_id		- property list identifier
!		level		- compression level
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pset_deflate_f(prp_id, level, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_deflate_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: level        ! Compression level 
            INTEGER, INTENT(OUT) :: hdferr       ! Error code

!            INTEGER, EXTERNAL :: h5pset_deflate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_deflate_c(prp_id, level)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_DEFLATE_C'::h5pset_deflate_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(IN) :: level
              END FUNCTION h5pset_deflate_c
            END INTERFACE
            hdferr = h5pset_deflate_c(prp_id, level)

          END SUBROUTINE h5pset_deflate_f

!----------------------------------------------------------------------
! Name:		h5pset(get)fill_value_f 
!
! Purpose: 	Sets(gets) fill value for a dataset creation property list
!
! Inputs:  
!		prp_id		- dataset creation property list identifier
!		type_id		- datatype identifier for fill value
!		fillvalue	- fill value
! Outputs:  
!	(	type_id		- datatype identifier for fill value )
!	(		fillvalue	- fill value )
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
!			port).  March 14, 2001 
!
! Comment:	h5pset(get)fill_value_f function is overloaded to support
!		INTEGER, REAL, DOUBLE PRECISION and CHARACTER dtatypes.		
!----------------------------------------------------------------------


          SUBROUTINE h5pset_fill_value_integer(prp_id, type_id, fillvalue, &
                                               hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fill_value_integer
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype 
                                                  ! (in memory)
            INTEGER, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_fill_value_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_fill_value_c(prp_id, type_id, fillvalue)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_FILL_VALUE_C'::h5pset_fill_value_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN) :: fillvalue
              END FUNCTION h5pset_fill_value_c
            END INTERFACE

            hdferr = h5pset_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pset_fill_value_integer


          SUBROUTINE h5pget_fill_value_integer(prp_id, type_id, fillvalue, &
                                               hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_fill_value_integer
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype
                                                  ! (in memory) 
            INTEGER, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_fill_value_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_fill_value_c(prp_id, type_id, fillvalue)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_FILL_VALUE_C'::h5pget_fill_value_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER :: fillvalue
              END FUNCTION h5pget_fill_value_c
            END INTERFACE

            hdferr = h5pget_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pget_fill_value_integer


          SUBROUTINE h5pset_fill_value_real(prp_id, type_id, fillvalue, &
                                               hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fill_value_real
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype 
                                                  ! (in memory)
            REAL, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_fill_value_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_fill_value_c(prp_id, type_id, fillvalue)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_FILL_VALUE_C'::h5pset_fill_value_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HID_T), INTENT(IN) :: type_id
              REAL, INTENT(IN) :: fillvalue
              END FUNCTION h5pset_fill_value_c
            END INTERFACE

            hdferr = h5pset_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pset_fill_value_real


          SUBROUTINE h5pget_fill_value_real(prp_id, type_id, fillvalue, &
                                               hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_fill_value_real
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype
                                                  ! (in memory) 
            REAL, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_fill_value_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_fill_value_c(prp_id, type_id, fillvalue)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_FILL_VALUE_C'::h5pget_fill_value_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HID_T), INTENT(IN) :: type_id
              REAL :: fillvalue
              END FUNCTION h5pget_fill_value_c
            END INTERFACE

            hdferr = h5pget_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pget_fill_value_real


          SUBROUTINE h5pset_fill_value_double(prp_id, type_id, fillvalue, &
                                               hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fill_value_double
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype 
                                                  ! (in memory)
            DOUBLE PRECISION, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_fill_value_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_fill_value_c(prp_id, type_id, fillvalue)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_FILL_VALUE_C'::h5pset_fill_value_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HID_T), INTENT(IN) :: type_id
              DOUBLE PRECISION, INTENT(IN) :: fillvalue
              END FUNCTION h5pset_fill_value_c
            END INTERFACE

            hdferr = h5pset_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pset_fill_value_double


          SUBROUTINE h5pget_fill_value_double(prp_id, type_id, fillvalue, &
                                               hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_fill_value_double
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype
                                                  ! (in memory) 
            DOUBLE PRECISION, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_fill_value_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_fill_value_c(prp_id, type_id, fillvalue)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_FILL_VALUE_C'::h5pget_fill_value_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HID_T), INTENT(IN) :: type_id
              DOUBLE PRECISION :: fillvalue
              END FUNCTION h5pget_fill_value_c
            END INTERFACE

            hdferr = h5pget_fill_value_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pget_fill_value_double

          SUBROUTINE h5pset_fill_value_char(prp_id, type_id, fillvalue, &
                                               hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fill_value_char
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype 
                                                  ! (in memory)
            CHARACTER, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_fill_valuec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_fill_valuec_c(prp_id, type_id, fillvalue)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_FILL_VALUEC_C'::h5pset_fill_valuec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: fillvalue 
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HID_T), INTENT(IN) :: type_id
              CHARACTER, INTENT(IN) :: fillvalue
              END FUNCTION h5pset_fill_valuec_c
            END INTERFACE

            hdferr = h5pset_fill_valuec_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pset_fill_value_char

          SUBROUTINE h5pget_fill_value_char(prp_id, type_id, fillvalue, &
                                               hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_fill_value_char
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of 
                                                  ! of fillvalue datatype
                                                  ! (in memory) 
            CHARACTER, INTENT(IN) :: fillvalue   ! Fillvalue
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_fill_valuec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_fill_valuec_c(prp_id, type_id, fillvalue)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_FILL_VALUEC_C'::h5pget_fill_valuec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: fillvalue 
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HID_T), INTENT(IN) :: type_id
              CHARACTER :: fillvalue
              END FUNCTION h5pget_fill_valuec_c
            END INTERFACE

            hdferr = h5pget_fill_valuec_c(prp_id, type_id, fillvalue)
          END SUBROUTINE h5pget_fill_value_char

!----------------------------------------------------------------------
! Name:		h5pget_version_f 
!
! Purpose: 	Retrieves the version information of various objects 
!		for a file creation property list
!
! Inputs:  
!		prp_id		- file createion property list identifier
! Outputs:  
!		boot		- super block version number
!		freelist	- global freelist version number
!		stab		- symbol table version number
!		shhdr		- shared object header version number
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_version_f(prp_id, boot, freelist, &
                                    stab, shhdr, hdferr)
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_version_f
!DEC$endif
!

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, DIMENSION(:), INTENT(OUT) :: boot  !array to put boot
                                                        !block version number
            INTEGER, DIMENSION(:), INTENT(OUT) :: freelist  !array to put global
                                                        !freelist version number
     
            INTEGER, DIMENSION(:), INTENT(OUT) :: stab  !array to put symbol
                                                        !table version number
            INTEGER, DIMENSION(:), INTENT(OUT) :: shhdr !array to put shared
                                                        !object header version number
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_version_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_version_c(prp_id, boot, freelist, stab, shhdr)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_VERSION_C'::h5pget_version_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, DIMENSION(:), INTENT(OUT) :: boot 
              INTEGER, DIMENSION(:), INTENT(OUT) :: freelist 
              INTEGER, DIMENSION(:), INTENT(OUT) :: stab
              INTEGER, DIMENSION(:), INTENT(OUT) :: shhdr
              END FUNCTION h5pget_version_c
            END INTERFACE

            hdferr = h5pget_version_c(prp_id, boot, freelist, stab, shhdr)
          END SUBROUTINE h5pget_version_f

!----------------------------------------------------------------------
! Name:		h5pset_userblock_f 
!
! Purpose: 	Sets user block size
!
! Inputs:  
!		prp_id		- file creation property list to modify
!		size		- size of the user-block in bytes
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------
 
          SUBROUTINE h5pset_userblock_f (prp_id, size, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_userblock_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), INTENT(IN) :: size !Size of the user-block in bytes 
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_userblock_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_userblock_c(prp_id, size)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_USERBLOCK_C'::h5pset_userblock_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HSIZE_T), INTENT(IN) :: size
              END FUNCTION h5pset_userblock_c
            END INTERFACE

            hdferr = h5pset_userblock_c(prp_id, size)
          END SUBROUTINE h5pset_userblock_f

!----------------------------------------------------------------------
! Name:		h5pget_userblock_f 
!
! Purpose: 	Gets user block size.
!
! Inputs:  
!		prp_id		- file creation property list identifier
! Outputs:  
!		block_size	- size of the user block in bytes
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pget_userblock_f(prp_id, block_size, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_userblock_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), INTENT(OUT) ::  block_size !Size of the 
                                                               !user-block in bytes 
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_userblock_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_userblock_c(prp_id, block_size)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_USERBLOCK_C'::h5pget_userblock_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HSIZE_T), INTENT(OUT) :: block_size
              END FUNCTION h5pget_userblock_c
            END INTERFACE
            hdferr = h5pget_userblock_c(prp_id,  block_size)
          END SUBROUTINE h5pget_userblock_f

!----------------------------------------------------------------------
! Name:		h5pset_sizes_f 
!
! Purpose: 	Sets the byte size of the offsets and lengths used 
!		to address objects in an HDF5 file.
!
! Inputs:  
!		prp_id		- file creation property list identifier
!		sizeof_addr	- size of an object offset in bytes 
!		sizeof_size	- size of an object length in bytes
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_sizes_f (prp_id, sizeof_addr, sizeof_size, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_sizes_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(SIZE_T), INTENT(IN) :: sizeof_addr !Size of an object 
                                                       !offset in bytes 
            INTEGER(SIZE_T), INTENT(IN) :: sizeof_size !Size of an object 
                                                       !length in bytes 
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_sizes_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_sizes_c(prp_id, sizeof_addr, sizeof_size)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_SIZES_C'::h5pset_sizes_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id 
              INTEGER(SIZE_T), INTENT(IN) :: sizeof_addr
              INTEGER(SIZE_T), INTENT(IN) :: sizeof_size
              END FUNCTION h5pset_sizes_c
            END INTERFACE

            hdferr = h5pset_sizes_c(prp_id, sizeof_addr, sizeof_size)
          END SUBROUTINE h5pset_sizes_f

!----------------------------------------------------------------------
! Name:		h5pget_sizes_f 
!
! Purpose: 	Retrieves the size of the offsets and lengths used 
!		in an HDF5 file
!
! Inputs:  
!		prp_id		- file creation property list identifier
! Outputs:  
!		sizeof_addr	- size of an object offset in bytes 
!		sizeof_size	- size of an object length in bytes
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pget_sizes_f(prp_id, sizeof_addr, sizeof_size, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_sizes_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(SIZE_T), INTENT(OUT) :: sizeof_addr !Size of an object
                                                                      !offset in bytes 
            INTEGER(SIZE_T), INTENT(OUT) :: sizeof_size !Size of an object
                                                                      !length in bytes 
  
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_sizes_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_sizes_c(prp_id, sizeof_addr, sizeof_size)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_SIZES_C'::h5pget_sizes_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id 
              INTEGER(SIZE_T), INTENT(OUT) :: sizeof_addr
              INTEGER(SIZE_T), INTENT(OUT) :: sizeof_size
              END FUNCTION h5pget_sizes_c
            END INTERFACE

            hdferr = h5pget_sizes_c(prp_id, sizeof_addr, sizeof_size)
          END SUBROUTINE h5pget_sizes_f

!----------------------------------------------------------------------
! Name:		h5pset_sym_k_f 
!
! Purpose: 	Sets the size of parameters used to control the 
!		symbol table nodes
!
! Inputs:  
!		prp_id		- file creation property list identifier
!		ik		- symbol table tree rank
!		lk		- symbol table node size
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_sym_k_f (prp_id, ik, lk, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_sym_k_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: ik ! Symbol table tree rank 
            INTEGER, INTENT(IN) :: lk ! Symbol table node size 
                                                       
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_sym_k_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_sym_k_c(prp_id, ik, lk)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_SYM_K_C'::h5pset_sym_k_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id 
              INTEGER, INTENT(IN) :: ik
              INTEGER, INTENT(IN) :: lk
              END FUNCTION h5pset_sym_k_c
            END INTERFACE

            hdferr = h5pset_sym_k_c(prp_id, ik, lk)
          END SUBROUTINE h5pset_sym_k_f

!----------------------------------------------------------------------
! Name:		h5pget_sym_k_f 
!
! Purpose: 	Retrieves the size of the symbol table B-tree 1/2 rank
!		 and the symbol table leaf node 1/2 size. 
!
! Inputs:  
!		prp_id		- file creation property list identifier
! Outputs:  
!		ik		- symbol table tree 1/2 rank
!		lk		- symbol table node 1/2 size
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pget_sym_k_f(prp_id, ik, lk, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_sym_k_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: ik !Symbol table tree rank
            INTEGER, INTENT(OUT) :: lk !Symbol table node size
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_sym_k_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_sym_k_c(prp_id, ik, lk)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_SYM_K_C'::h5pget_sym_k_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id 
              INTEGER, INTENT(OUT) :: ik
              INTEGER, INTENT(OUT) :: lk
              END FUNCTION h5pget_sym_k_c
            END INTERFACE

            hdferr = h5pget_sym_k_c(prp_id, ik, lk)
          END SUBROUTINE h5pget_sym_k_f

!----------------------------------------------------------------------
! Name:		h5pset_istore_k_f 
!
! Purpose: 	Sets the size of the parameter used to control the 
!		B-trees for indexing chunked datasets
!
! Inputs:  
!		prp_id		- file creation property list identifier
!		ik		- 1/2 rank of chunked storage B-tree
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_istore_k_f (prp_id, ik, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_istore_k_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: ik ! 1/2 rank of chunked storage B-tree
                                                       
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_istore_k_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_istore_k_c(prp_id, ik)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_ISTORE_K_C'::h5pset_istore_k_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(IN) :: ik
              END FUNCTION h5pset_istore_k_c
            END INTERFACE

            hdferr = h5pset_istore_k_c(prp_id, ik)
          END SUBROUTINE h5pset_istore_k_f

!----------------------------------------------------------------------
! Name:		h5pget_istore_k_f 
!
! Purpose: 	Queries the 1/2 rank of an indexed storage B-tree. 
!
! Inputs:  
!		prp_id		- file creation property list identifier
! Outputs:  
!		ik		- 1/2 rank of chunked storage B-tree
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pget_istore_k_f(prp_id, ik, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_istore_k_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: ik !1/2 rank of chunked storage B-tree
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_istore_k_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_istore_k_c(prp_id, ik)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_ISTORE_K_C'::h5pget_istore_k_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(OUT) :: ik
              END FUNCTION h5pget_istore_k_c
            END INTERFACE

            hdferr = h5pget_istore_k_c(prp_id, ik)
          END SUBROUTINE h5pget_istore_k_f

!----------------------------------------------------------------------
! Name:		h5pget_driver_f 
!
! Purpose: 	Returns low-lever driver identifier. 
!
! Inputs:  
!		prp_id		- file access or data transfer property 
!				  list identifier. 
! Outputs:  
!		driver		- low-level driver identifier
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_driver_f(prp_id, driver, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_driver_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HID_T), INTENT(OUT) :: driver !low-level file driver identifier
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_driver_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_driver_c(prp_id, driver)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_DRIVER_C'::h5pget_driver_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HID_T), INTENT(OUT) :: driver
              END FUNCTION h5pget_driver_c
            END INTERFACE

            hdferr = h5pget_driver_c(prp_id, driver)
          END SUBROUTINE h5pget_driver_f

!----------------------------------------------------------------------
! Name:		h5pset_fapl_stdio_f 
!
! Purpose: 	Sets the standard I/O driver. 
!
! Inputs:  
!		prp_id		- file access property list identifier
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_fapl_stdio_f (prp_id, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fapl_stdio_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_fapl_stdio_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_fapl_stdio_c(prp_id)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_FAPL_STDIO_C'::h5pset_fapl_stdio_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              END FUNCTION h5pset_fapl_stdio_c
            END INTERFACE

            hdferr = h5pset_fapl_stdio_c(prp_id)
          END SUBROUTINE h5pset_fapl_stdio_f

!----------------------------------------------------------------------
! Name:		h5pget_stdio_f 
!
! Purpose:  	NOT AVAILABLE
!
! Inputs:  
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

!          SUBROUTINE h5pget_stdio_f (prp_id, io, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_stdio_f
!DEC$endif
!
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
!            INTEGER, INTENT(OUT) :: io   ! value indicates that the file 
                                         !access property list is set to 
                                         !the stdio driver
!            INTEGER, INTENT(OUT) :: hdferr  ! Error code
!            INTEGER, EXTERNAL :: h5pget_stdio_c
!            hdferr = h5pget_stdio_c(prp_id, io)
!          END SUBROUTINE h5pget_stdio_f

!----------------------------------------------------------------------
! Name:		h5pset_fapl_sec2_f 
!
! Purpose: 	Sets the sec2 driver. 
!
! Inputs:  
!		prp_id		- file access property list identifier
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_fapl_sec2_f (prp_id, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fapl_sec2_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_fapl_sec2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_fapl_sec2_c(prp_id)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_FAPL_SEC2_C'::h5pset_fapl_sec2_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
              END FUNCTION h5pset_fapl_sec2_c
            END INTERFACE

            hdferr = h5pset_fapl_sec2_c(prp_id)
          END SUBROUTINE h5pset_fapl_sec2_f

!----------------------------------------------------------------------
! Name:		h5pget_sec2_f 
!
! Purpose: 	NOT AVAILABLE
!
! Inputs:  
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

!          SUBROUTINE h5pget_sec2_f (prp_id, sec2, hdferr) 
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
!            INTEGER, INTENT(OUT) :: sec2   ! value indicates whether the file 
                                           !driver uses the functions declared
                                           !in the unistd.h file
!            INTEGER, INTENT(OUT) :: hdferr  ! Error code
!            INTEGER, EXTERNAL :: h5pget_sec2_c
!            hdferr = h5pget_sec2_c(prp_id, sec2)
!          END SUBROUTINE h5pget_sec2_f

!----------------------------------------------------------------------
! Name:		h5pset_alignment_f 
!
! Purpose: 	Sets alignment properties of a file access property list. 
!
! Inputs:  
!		prp_id		- file access property list identifier
!		threshold	- threshold value	
!		alignment	- alignment value
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_alignment_f(prp_id, threshold,  alignment, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_alignment_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), INTENT(IN) :: threshold ! Threshold value
            INTEGER(HSIZE_T), INTENT(IN) :: alignment ! alignment value
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_alignment_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_alignment_c(prp_id, threshold, alignment)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_ALIGNMENT_C'::h5pset_alignment_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HSIZE_T), INTENT(IN) :: threshold
              INTEGER(HSIZE_T), INTENT(IN) :: alignment
              END FUNCTION h5pset_alignment_c
            END INTERFACE

            hdferr = h5pset_alignment_c(prp_id, threshold, alignment)
          END SUBROUTINE h5pset_alignment_f

!----------------------------------------------------------------------
! Name:		h5pget_alignment_f 
!
! Purpose: 	Retrieves the current settings for alignment 
!		properties from a file access property list. 
!
! Inputs:  
!		prp_id		- file access property list identifier
! Outputs:  
!		threshold	- threshold value	
!		alignment	- alignment value
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_alignment_f(prp_id, threshold,  alignment, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_alignment_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), INTENT(OUT) :: threshold ! Threshold value
            INTEGER(HSIZE_T), INTENT(OUT) :: alignment ! alignment value
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_alignment_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_alignment_c(prp_id, threshold, alignment)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_ALIGNMENT_C'::h5pget_alignment_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HSIZE_T), INTENT(OUT) :: threshold
              INTEGER(HSIZE_T), INTENT(OUT) :: alignment
              END FUNCTION h5pget_alignment_c
            END INTERFACE

            hdferr = h5pget_alignment_c(prp_id, threshold, alignment)
          END SUBROUTINE h5pget_alignment_f

!----------------------------------------------------------------------
! Name:		h5pset_fapl_core_f 
!
! Purpose: 	Modifies the file access property list to use the 
!		H5FD_CORE driver. 
!
! Inputs:  	prp_id		- file access property list identifier
!		increment	- size, in bytes, of memory increments 
!		backing_store	- boolean flag indicating whether to write 
!				  the file contents to disk when the file is closed. 
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_fapl_core_f(prp_id, increment, backing_store, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fapl_core_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(SIZE_T), INTENT(IN) :: increment ! File block size in bytes.
            INTEGER, INTENT(IN) :: backing_store ! flag to indicate that
                                    ! entire file contents are flushed to a file 
                                    ! with the same name as this core file.
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_fapl_core_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_fapl_core_c(prp_id, increment, backing_store)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_FAPL_CORE_C'::h5pset_fapl_core_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id 
              INTEGER(SIZE_T), INTENT(IN) :: increment 
              INTEGER, INTENT(IN) :: backing_store 
              END FUNCTION h5pset_fapl_core_c
            END INTERFACE

            hdferr = h5pset_fapl_core_c(prp_id, increment, backing_store)
          END SUBROUTINE h5pset_fapl_core_f

!----------------------------------------------------------------------
! Name:		h5pget_fapl_core_f 
!
! Purpose: 	Queries core file driver properties. 
!
! Inputs:  
!		prp_id		- file access property list identifier
! Outputs:  
!		increment	- size, in bytes, of memory increments 
!		backing_store	- boolean flag indicating whether to write 
!				  the file contents to disk when the file is closed. 
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_fapl_core_f(prp_id, increment, backing_store, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_fapl_core_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(SIZE_T), INTENT(OUT) :: increment ! File block size in bytes.
            INTEGER, INTENT(OUT) :: backing_store ! flag to indicate that
                                    ! entire file contents are flushed to a file 
                                    ! with the same name as this core file.
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_fapl_core_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_fapl_core_c(prp_id, increment, backing_store)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_FAPL_CORE_C'::h5pget_fapl_core_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id 
              INTEGER(SIZE_T), INTENT(OUT) :: increment 
              INTEGER, INTENT(OUT) :: backing_store 
              END FUNCTION h5pget_fapl_core_c
            END INTERFACE

            hdferr = h5pget_fapl_core_c(prp_id, increment, backing_store)
          END SUBROUTINE h5pget_fapl_core_f

!----------------------------------------------------------------------
! Name:		h5pset_fapl_family_f 
!
! Purpose: 	Sets the file access property list to use the family driver. 
!
! Inputs:  
!		prp_id		- file access property list identifier
!		memb_size	- size in bytes of each file member 
!		memb_plist	- identifier of the file access property 
!				  list to be used for each family member
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_fapl_family_f(prp_id, memb_size, memb_plist , hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fapl_family_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), INTENT(IN) :: memb_size ! Logical size, in bytes,
                                                      !of each family member
            INTEGER(HID_T), INTENT(IN) :: memb_plist !Identifier of the file 
                                                     !access property list for 
                                                     !each member of the family
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_fapl_family_f
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_fapl_family_c(prp_id, memb_size, memb_plist)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_FAPL_FAMILY_C'::h5pset_fapl_family_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HSIZE_T), INTENT(IN) :: memb_size
              INTEGER(HID_T), INTENT(IN) :: memb_plist
              END FUNCTION h5pset_fapl_family_c
            END INTERFACE

            hdferr = h5pset_fapl_family_c(prp_id, memb_size, memb_plist)
          END SUBROUTINE h5pset_fapl_family_f

!----------------------------------------------------------------------
! Name:		h5pget_fapl_family_f 
!
! Purpose:	Returns file access property list information.  	
!
! Inputs:  
!		prp_id		- file access property list identifier
! Outputs:  
!		memb_size	- size in bytes of each file member 
!		memb_plist	- identifier of the file access property 
!				  list to be used for each family member
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pget_fapl_family_f(prp_id, memb_size, memb_plist , hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_fapl_family_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER(HSIZE_T), INTENT(OUT) :: memb_size ! Logical size, in bytes,
                                                      !of each family member
            INTEGER(HID_T), INTENT(OUT) :: memb_plist !Identifier of the file 
                                                     !access property list for 
                                                     !each member of the family
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_fapl_family_f
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_fapl_family_c(prp_id, memb_size, memb_plist)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_FAPL_FAMILY_C'::h5pget_fapl_family_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER(HSIZE_T), INTENT(OUT) :: memb_size
              INTEGER(HID_T), INTENT(OUT) :: memb_plist
              END FUNCTION h5pget_fapl_family_c
            END INTERFACE

            hdferr = h5pget_fapl_family_c(prp_id, memb_size, memb_plist)
          END SUBROUTINE h5pget_fapl_family_f

!----------------------------------------------------------------------
! Name:		h5pset_cache_f 
!
! Purpose: 	Sets the meta data cache and raw data chunk 
!		cache parameters
!
! Inputs:  
!		prp_id		- file access property list identifier
!		mdc_nelmts	- number of elements (objects) in the meta 
!				  data cache 
!		rdcc_nelmts	- number of elements (objects) in the raw 
!			          data chunk cache 
!		rdcc_nbytes	- total size of the raw data chunk cache, in bytes 
!		rdcc_w0		- preemption policy (0 or 1)
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_cache_f(prp_id, mdc_nelmts,rdcc_nelmts, rdcc_nbytes, rdcc_w0, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_cache_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: mdc_nelmts  !Number of elements (objects)
                                                        ! in the meta data cache
            INTEGER, INTENT(IN) :: rdcc_nelmts  !Number of elements (objects)
                                                        ! in the meta data cache
            INTEGER(SIZE_T), INTENT(IN) :: rdcc_nbytes !Total size of the raw data 
                                                      !chunk cache, in bytes 
            REAL, INTENT(IN) :: rdcc_w0 !Preemption policy
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_cache_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_cache_c(prp_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_CACHE_C'::h5pset_cache_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(IN) :: mdc_nelmts 
              INTEGER, INTENT(IN) :: rdcc_nelmts 
              INTEGER(SIZE_T), INTENT(IN) :: rdcc_nbytes
              REAL, INTENT(IN) :: rdcc_w0
              END FUNCTION h5pset_cache_c
            END INTERFACE

            hdferr = h5pset_cache_c(prp_id, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0 )
          END SUBROUTINE h5pset_cache_f

!----------------------------------------------------------------------
! Name:		h5pget_cache_f 
!
! Purpose: 	Queries the meta data cache and raw data chunk cache 
!		parameters.  
!
! Inputs:  
!		prp_id		- file access property list identifier
! Outputs:  
!		mdc_nelmts	- number of elements (objects) in the meta 
!				  data cache 
!		rdcc_nelmts	- number of elements (objects) in the raw 
!			          data chunk cache 
!		rdcc_nbytes	- total size of the raw data chunk cache, in bytes 
!		rdcc_w0		- preemption policy (0 or 1)
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_cache_f(prp_id, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_cache_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: mdc_nelmts  !Number of elements (objects)
                                                        ! in the meta data cache
            INTEGER, INTENT(OUT) :: rdcc_nelmts  !Number of elements (objects)
                                                        ! in the meta data cache
            INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nbytes !Total size of the raw data 
                                                      !chunk cache, in bytes 
            REAL, INTENT(OUT) :: rdcc_w0 !Preemption policy
            INTEGER, INTENT(OUT) :: hdferr  ! Error code


!            INTEGER, EXTERNAL :: h5pget_cache_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_cache_c(prp_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_CACHE_C'::h5pget_cache_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(OUT) :: mdc_nelmts 
              INTEGER, INTENT(OUT) :: rdcc_nelmts 
              INTEGER(SIZE_T), INTENT(OUT) :: rdcc_nbytes
              REAL, INTENT(OUT) :: rdcc_w0
              END FUNCTION h5pget_cache_c
            END INTERFACE

            hdferr = h5pget_cache_c(prp_id, mdc_nelmts,rdcc_nelmts, rdcc_nbytes, rdcc_w0 )
          END SUBROUTINE h5pget_cache_f

!----------------------------------------------------------------------
! Name:		h5pset_fapl_split_f 
!
! Purpose: 	Emulates the old split file driver. 
!
! Inputs:  
!		prp_id		- file access property list identifier
!		meta_ext	- name of the extension for the metafile 
!				  filename
!		meta_plist	- identifier of the meta file access property 
!				  list
!		raw_ext 	- name extension for the raw file filename
!		raw_plist	- identifier of the raw file access property list
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_fapl_split_f(prp_id, meta_ext, meta_plist, raw_ext, raw_plist, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fapl_split_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            CHARACTER(LEN=*), INTENT(IN) :: meta_ext  !Name of the extension for
                                                      !the metafile filename
            INTEGER(HID_T), INTENT(IN) :: meta_plist  ! Identifier of the meta file
                                                      ! access property list
            CHARACTER(LEN=*), INTENT(IN) :: raw_ext  !Name extension for the raw file filename
            INTEGER(HID_T), INTENT(IN) :: raw_plist  !Identifier of the raw file 
                                                     !access property list
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER :: meta_len, raw_len

!            INTEGER, EXTERNAL :: h5pset_fapl_split_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_fapl_split_c(prp_id,meta_len,meta_ext,meta_plist,raw_len,raw_ext,raw_plist)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_FAPL_SPLIT_C'::h5pset_fapl_split_c
              !DEC$ ENDIF
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

!----------------------------------------------------------------------
! Name:		h5pget_split_f 
!
! Purpose: 	NOT AVAILABLE
!
! Inputs:  
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

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

!            INTEGER, EXTERNAL :: h5pget_split_c
!            hdferr = h5pget_split_c(prp_id, meta_ext_size, meta_ext, meta_plist, &
!                                    raw_ext_size, raw_ext, raw_plist )
!          END SUBROUTINE h5pget_split_f

!----------------------------------------------------------------------
! Name:		h5pset_gc_references_f 
!
! Purpose: 	Sets garbage collecting references flag. 
!
! Inputs:  
!		prp_id		- file access property list identifier
!		gc_reference	- flag for stting garbage collection on 
!				  and off (1 or 0)
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

 
          SUBROUTINE h5pset_gc_references_f (prp_id, gc_reference, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_gc_references_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: gc_reference !the flag for garbage collecting
                                                ! references for the file
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_gc_references_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_gc_references_c(prp_id, gc_reference)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_GC_REFERENCES_C'::h5pset_gc_references_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(IN) :: gc_reference
              END FUNCTION h5pset_gc_references_c
            END INTERFACE

            hdferr = h5pset_gc_references_c(prp_id, gc_reference)
          END SUBROUTINE h5pset_gc_references_f

!----------------------------------------------------------------------
! Name:		h5pget_gc_references_f 
!
! Purpose: 	Returns garbage collecting references setting. 	
!
! Inputs:  
!		prp_id		- file access property list identifier
! Outputs:  
!		gc_reference	- flag for stting garbage collection on 
!				  and off (1 or 0)
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_gc_references_f (prp_id, gc_reference, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_gc_references_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: gc_reference !the flag for garbage collecting
                                                ! references for the file
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_gc_references_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_gc_references_c(prp_id, gc_reference)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_GC_REFERENCES_C'::h5pget_gc_references_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(OUT) :: gc_reference
              END FUNCTION h5pget_gc_references_c
            END INTERFACE

            hdferr = h5pget_gc_references_c(prp_id, gc_reference)
          END SUBROUTINE h5pget_gc_references_f

!----------------------------------------------------------------------
! Name:		h5pset_layout_f 
!
! Purpose: 	Sets the type of storage used store the raw data 
!		for a dataset. 
!
! Inputs:  
!		prp_id		- data creation property list identifier
!		layout		- type of storage layout for raw data
!				  possible values are:
!				  H5D_COMPACT_F
!				  H5D_CONTIGUOUS_F
!				  H5D_CHUNKED_F
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_layout_f (prp_id, layout, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_layout_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: layout !Type of storage layout for raw data
                                          !possible values are:
                                          !H5D_COMPACT_F
                                          !H5D_CONTIGUOUS_F
                                          !H5D_CHUNKED_F
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_layout_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_layout_c(prp_id, layout)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_LAYOUT_C'::h5pset_layout_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(IN) :: layout
              END FUNCTION h5pset_layout_c
            END INTERFACE

            hdferr = h5pset_layout_c(prp_id, layout)
          END SUBROUTINE h5pset_layout_f

!----------------------------------------------------------------------
! Name:		h5pget_layout_f 
!
! Purpose: 	Returns the layout of the raw data for a dataset. 
!
! Inputs:  
!		prp_id		- data creation property list identifier
! Outputs:  
!		layout		- type of storage layout for raw data
!				  possible values are:
!				  H5D_COMPACT_F
!				  H5D_CONTIGUOUS_F
!				  H5D_CHUNKED_F
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_layout_f (prp_id, layout, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_layout_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: layout !Type of storage layout for raw data
                                          !possible values are:
                                          !H5D_COMPACT_F(0)
                                          !H5D_CONTIGUOUS_F(1)
                                          !H5D_CHUNKED_F(2)
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_layout_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_layout_c(prp_id, layout)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_LAYOUT_C'::h5pget_layout_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(OUT) :: layout
              END FUNCTION h5pget_layout_c
            END INTERFACE

            hdferr = h5pget_layout_c(prp_id, layout)
          END SUBROUTINE h5pget_layout_f

!----------------------------------------------------------------------
! Name:		h5pset_filter_f 
!
! Purpose: 	Adds a filter to the filter pipeline. 
!
! Inputs:  
!		prp_id		- data creation or transfer property list 
!				  identifier
!		filter		- filter to be added to the pipeline 
!		flags		- bit vector specifying certain general
!				  properties of the filter
!		cd_nelmts	- number of elements in cd_values
!		cd_values	- auxiliary data for the filter
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_filter_f(prp_id, filter, flags, cd_nelmts, cd_values,  hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_filter_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: filter  !Filter to be added to the pipeline.
            INTEGER, INTENT(IN) :: flags  !Bit vector specifying certain general
                                          !properties of the filter.
            INTEGER(SIZE_T), INTENT(IN) :: cd_nelmts  !Number of elements in cd_values.
            INTEGER, DIMENSION(*), INTENT(IN) :: cd_values  !Auxiliary data for the filter.

            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_filter_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_filter_c(prp_id, filter, flags, cd_nelmts, cd_values)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_FILTER_C'::h5pset_filter_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id 
              INTEGER, INTENT(IN) :: filter 
              INTEGER, INTENT(IN) :: flags 
              INTEGER(SIZE_T), INTENT(IN) :: cd_nelmts 
              INTEGER, DIMENSION(*), INTENT(IN) :: cd_values 
              END FUNCTION h5pset_filter_c
            END INTERFACE

            hdferr = h5pset_filter_c(prp_id, filter, flags, cd_nelmts, cd_values )
          END SUBROUTINE h5pset_filter_f

!----------------------------------------------------------------------
! Name:		h5pget_nfilters_f 
!
! Purpose: 	Returns the number of filters in the pipeline. 
!
! Inputs:  
!		prp_id		- data creation or transfer property list 
!				  identifier
! Outputs:  
!		nfilters	- number of filters in the pipeline
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_nfilters_f (prp_id, nfilters, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_nfilters_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: nfilters !the number of filters in the pipeline
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_nfilters_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_nfilters_c(prp_id, nfilters)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_NFILTERS_C'::h5pget_nfilters_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(OUT) :: nfilters
              END FUNCTION h5pget_nfilters_c
            END INTERFACE

            hdferr = h5pget_nfilters_c(prp_id, nfilters)
          END SUBROUTINE h5pget_nfilters_f

!----------------------------------------------------------------------
! Name:		h5pget_filter_f 
!
! Purpose: 	Returns information about a filter in a pipeline
!
! Inputs:  
!		prp_id		- data creation or transfer property list 
!				  identifier
! Outputs:  
!				  identifier
!		filter		- filter to be added to the pipeline 
!		flags		- bit vector specifying certain general
!				  properties of the filter
!		cd_nelmts	- number of elements in cd_values
!		cd_values	- auxiliary data for the filter
!		namelen		- number of characters in the name buffer
!		name		- buffer to retrieve filter name
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_filter_f(prp_id, filter_number, flags, cd_nelmts, cd_values, namelen, name, filter_id, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_filter_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(IN) :: filter_number  !Sequence number within the filter
                                                  !pipeline of the filter for which 
                                                  !information is sought
            INTEGER, DIMENSION(*), INTENT(OUT) :: cd_values  !Auxiliary data for the filter.
            INTEGER, INTENT(OUT) :: flags  !Bit vector specifying certain general
                                          !properties of the filter.
            INTEGER(SIZE_T), INTENT(INOUT) :: cd_nelmts  !Number of elements in cd_values.
            INTEGER(SIZE_T), INTENT(IN) :: namelen !Anticipated number of characters in name.
            CHARACTER(LEN=*), INTENT(OUT) :: name !Name of the filter
            INTEGER, INTENT(OUT) :: filter_id ! filter identification number  

            INTEGER, INTENT(OUT) :: hdferr  ! Error code


!            INTEGER, EXTERNAL :: h5pget_filter_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_filter_c(prp_id, filter_number, flags, cd_nelmts,  &
                                              cd_values, namelen, name, filter_id )
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_FILTER_C'::h5pget_filter_c
              !DEC$ ENDIF
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

!----------------------------------------------------------------------
! Name:		h5pset_external_f 
!
! Purpose: 	Adds an external file to the list of external files. 
!
! Inputs:   
!		prp_id		- dataset creation property list identifier
!		name		- name of external file
!		offset		- offset in bytes from the beginning of the 
!				  file to the location in the file
!				  where the data starts
!		bytes		- size of the external file data. 
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_external_f(prp_id, name, offset,bytes, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_external_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            CHARACTER(LEN=*), INTENT(IN) :: name !Name of an external file
            INTEGER, INTENT(IN) :: offset !Offset, in bytes, from the beginning 
                                          !of the file to the location in the file 
                                          !where the data starts.
            INTEGER(HSIZE_T), INTENT(IN) :: bytes ! Number of bytes reserved in the 
                                                 !file for the data
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

            INTEGER :: namelen

!            INTEGER, EXTERNAL :: h5pset_external_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_external_c(prp_id, name,namelen, offset, bytes)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_EXTERNAL_C'::h5pset_external_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: prp_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER, INTENT(IN) :: offset
              INTEGER(HSIZE_T), INTENT(IN) :: bytes
              END FUNCTION h5pset_external_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5pset_external_c(prp_id, name, namelen, offset, bytes)
          END SUBROUTINE h5pset_external_f

!----------------------------------------------------------------------
! Name:		h5pget_external_count_f 
!
! Purpose: 	Returns the number of external files for a dataset. 
!
! Inputs:  
!		prp_id		- dataset creation property list identifier
! Outputs:  
!		count		- number of external files for the 
!				  specified dataset
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_external_count_f (prp_id, count, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_external_count_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier 
            INTEGER, INTENT(OUT) :: count !number of external files for the 
                                          !specified dataset
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
!            INTEGER, EXTERNAL :: h5pget_external_count_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_external_count_c(prp_id, count)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_EXTERNAL_COUNT_C'::h5pget_external_count_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id 
              INTEGER, INTENT(OUT) :: count
              END FUNCTION h5pget_external_count_c
            END INTERFACE

            hdferr = h5pget_external_count_c(prp_id, count)
          END SUBROUTINE h5pget_external_count_f

!----------------------------------------------------------------------
! Name:		h5pget_external_f 
!
! Purpose: 	Returns information about an external file. 
!
! Inputs:  
!		prp_id		- dataset creation property list identifier
! Outputs:  
!		idx		- external file index 
!		name_size	- maximum size of name array
!		name		- name of the external file	
!		name		- name of external file
!	 	offset		- offset in bytes from the beginning of the 
!				  file to the location in the file
!				  where the data starts
!		bytes		- size of the external file data
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5pget_external_f(prp_id, idx, name_size, name, offset,bytes, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_external_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER, INTENT(IN) :: idx !External file index.
            INTEGER(SIZE_T), INTENT(IN) :: name_size !Maximum length of name array 
            CHARACTER(LEN=*), INTENT(OUT) :: name !Name of an external file
            INTEGER, INTENT(OUT) :: offset !Offset, in bytes, from the beginning 
                                          !of the file to the location in the file 
                                          !where the data starts.
            INTEGER(HSIZE_T), INTENT(OUT) :: bytes ! Number of bytes reserved in the 
                                                 !file for the data
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pget_external_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_external_c(prp_id, idx, name_size, name, offset, bytes)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_EXTERNAL_C'::h5pget_external_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(IN) :: idx 
              INTEGER(SIZE_T), INTENT(IN) :: name_size
              CHARACTER(LEN=*), INTENT(OUT) :: name
              INTEGER, INTENT(OUT) :: offset
              INTEGER(HSIZE_T), INTENT(OUT) :: bytes
              END FUNCTION h5pget_external_c
            END INTERFACE

            hdferr = h5pget_external_c(prp_id, idx, name_size, name, offset, bytes)
          END SUBROUTINE h5pget_external_f

!----------------------------------------------------------------------
! Name:		h5pset_hyper_cache_f 
!
! Purpose: 	Indicates whether to cache hyperslab blocks during I/O
!
! Inputs:  
!		prp_id		- dataset transfer property list identifier
!		cache		- A flag indicating whether caching is to 
!				  be set to on (1) or off (0). 
!		limit		- maximum size of the hyperslab block to
!			          cache; 0 (zero) indicates no limit
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_hyper_cache_f(prp_id, cache, limit, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_hyper_cache_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER, INTENT(IN) :: cache !
            INTEGER, INTENT(IN) :: limit ! Maximum size of the hyperslab block to 
                                         !cache. 0 (zero) indicates no limit.
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_hyper_cache_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pset_hyper_cache_c(prp_id, cache, limit)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_HYPER_CACHE_C'::h5pset_hyper_cache_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(IN) :: cache 
              INTEGER, INTENT(IN) :: limit 
              END FUNCTION h5pset_hyper_cache_c
            END INTERFACE

            hdferr = h5pset_hyper_cache_c(prp_id, cache, limit)
          END SUBROUTINE h5pset_hyper_cache_f

!----------------------------------------------------------------------
! Name:		h5pget_hyper_cache_f 
!
! Purpose: 	Returns information regarding the caching of hyperslab 
!		blocks during I/O. 
!
! Inputs:  
!		prp_id		- dataset transfer property list identifier
! Outputs:  
!		cache		- a flag indicating whether caching is  
!				  set to on (1) or off (0). 
!		limit		- maximum size of the hyperslab block to
!			          cache; 0 (zero) indicates no limit
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_hyper_cache_f(prp_id, cache, limit, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_hyper_cache_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER, INTENT(OUT) :: cache !
            INTEGER, INTENT(OUT) :: limit ! Maximum size of the hyperslab block to 
                                         !cache. 0 (zero) indicates no limit.
            INTEGER, INTENT(OUT) :: hdferr  ! Error code


!            INTEGER, EXTERNAL :: h5pget_hyper_cache_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5pget_hyper_cache_c(prp_id, cache, limit)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_HYPER_CACHE_C'::h5pget_hyper_cache_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              INTEGER, INTENT(OUT) :: cache 
              INTEGER, INTENT(OUT) :: limit 
              END FUNCTION h5pget_hyper_cache_c
            END INTERFACE

            hdferr = h5pget_hyper_cache_c(prp_id, cache, limit)
          END SUBROUTINE h5pget_hyper_cache_f

!----------------------------------------------------------------------
! Name:		h5pset_btree_ratios_f 
!
! Purpose: 	Sets B-tree split ratios for a dataset transfer 
!		property list. 
!
! Inputs:  	
!		prp_id		- the dataset transfer property list 
!				  identifier 
!		left		- the B-tree split ratio for left-most nodes 
!		middle		- the B-tree split ratio for all other nodes
!		right		- the B-tree split ratio for right-most nodes
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pset_btree_ratios_f(prp_id, left, middle, right, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_btree_ratios_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            REAL, INTENT(IN) :: left !The B-tree split ratio for left-most nodes.
            REAL, INTENT(IN) :: middle !The B-tree split ratio for all other nodes 
            REAL, INTENT(IN) :: right !The B-tree split ratio for right-most 
                                      !nodes and lone nodes. 

            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5pset_btree_ratios_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION  h5pset_btree_ratios_c(prp_id, left, middle, right)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PSET_BTREE_RATIOS_C'::h5pset_btree_ratios_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              REAL, INTENT(IN) :: left
              REAL, INTENT(IN) :: middle
              REAL, INTENT(IN) :: right
              END FUNCTION h5pset_btree_ratios_c
            END INTERFACE

            hdferr = h5pset_btree_ratios_c(prp_id, left, middle, right)
          END SUBROUTINE h5pset_btree_ratios_f

!----------------------------------------------------------------------
! Name:		h5pget_btree_ratios_f 
!
! Purpose: 	Gets B-tree split ratios for a dataset transfer property list
!
! Inputs:  
!		prp_id		- the dataset transfer property list 
!				  identifier 
! Outputs:  
!		left		- the B-tree split ratio for left-most nodes 
!		middle		- the B-tree split ratio for all other nodes
!		right		- the B-tree split ratio for right-most nodes
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
!			port).  March 14, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5pget_btree_ratios_f(prp_id, left, middle, right, hdferr) 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_btree_ratios_f
!DEC$endif
!
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            REAL, INTENT(OUT) :: left !The B-tree split ratio for left-most nodes.
            REAL, INTENT(OUT) :: middle !The B-tree split ratio for all other nodes 
            REAL, INTENT(OUT) :: right !The B-tree split ratio for right-most 
                                      !nodes and lone nodes. 

            INTEGER, INTENT(OUT) :: hdferr  ! Error code


!            INTEGER, EXTERNAL :: h5pget_btree_ratios_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION  h5pget_btree_ratios_c(prp_id, left, middle, right)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5PGET_BTREE_RATIOS_C'::h5pget_btree_ratios_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: prp_id
              REAL, INTENT(OUT) :: left
              REAL, INTENT(OUT) :: middle
              REAL, INTENT(OUT) :: right
              END FUNCTION h5pget_btree_ratios_c
            END INTERFACE

            hdferr = h5pget_btree_ratios_c(prp_id, left, middle, right)
          END SUBROUTINE h5pget_btree_ratios_f

     END MODULE H5P
