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
!
! This file contains FORTRAN90 interfaces for H5I functions
!
      MODULE H5Z

        USE H5GLOBAL
      
      CONTAINS

!----------------------------------------------------------------------
! Name:		h5zunregister_f
!
! Purpose:	Unregisters specified filetr
!
! Inputs: 	filter	        - filter; may have one of the following values:
!                                 H5Z_FILTER_DEFLATE_F 
!                                 H5Z_FILTER_SHUFFLE_F 
!                                 H5Z_FILTER_FLETCHER32_F 
! Outputs:  
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		March 12, 2003
!
! Modifications: 	
!
! Comment:		
!----------------------------------------------------------------------
          SUBROUTINE h5zunregister_f(filter, hdferr) 
!
!This definition is needed for Windows DLLs
!FooDECSif defined(BUILD_HDF5_DLL)
!FooDECSattributes dllexport :: h5zunregister_f
!FooDECSendif
!
            IMPLICIT NONE
            INTEGER, INTENT(IN)  :: filter
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5zunregister_c
!  Interface is needed for MS FORTRAN
!
            INTERFACE
              INTEGER FUNCTION h5zunregister_c (filter)
              USE H5GLOBAL
              !FooDECS IF DEFINED(HDF5F90_WINDOWS)
              !FooMSSATTRIBUTES C,reference,alias:'_H5ZUNREGISTER_C':: h5zunregister_c
              !FooDECS ENDIF
              INTEGER, INTENT(IN) :: filter
              END FUNCTION h5zunregister_c
            END INTERFACE
            hdferr = h5zunregister_c (filter)
          END SUBROUTINE h5zunregister_f
!----------------------------------------------------------------------
! Name:		h5zfilter_avail_f
!
! Purpose:      Queries if filter is available	
!
! Inputs:  
!		filter		- filter
! Outputs:  
!		status		- status; .TRUE. if filter is available, 
!                                 .FALSE. otherwise
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		March 12, 2003
!
! Modifications: 	
!
!----------------------------------------------------------------------
          SUBROUTINE h5zfilter_avail_f(filter, status, hdferr) 
!
!This definition is needed for Windows DLLs
!FooDECSif defined(BUILD_HDF5_DLL)
!FooDECSattributes dllexport :: h5zfilter_avail_f
!FooDECSendif
!

            IMPLICIT NONE
            INTEGER, INTENT(IN)  :: filter      ! Filter; may be one of the following:
                                                ! H5Z_FILTER_DEFLATE_F                  
                                                ! H5Z_FILTER_SHUFFLE_F                  
                                                ! H5Z_FILTER_FLETCHER32_F                  
            LOGICAL, INTENT(OUT) :: status      ! Flag, idicates if filter
                                                ! is availble  not ( TRUE or
                                                ! FALSE)  
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER :: flag                     ! "TRUE/FALSE/ERROR from C" 

!            INTEGER, EXTERNAL :: h5zfilter_avail_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5zfilter_avail_c(filter, flag) 
              USE H5GLOBAL
              !FooDECS IF DEFINED(HDF5F90_WINDOWS)
              !FooMSSATTRIBUTES C,reference,alias:'_H5ZFILTER_AVAIL_C'::h5zfilter_avail_c
              !FooDECS ENDIF
              INTEGER, INTENT(IN) :: filter
              INTEGER :: flag
              END FUNCTION h5zfilter_avail_c
            END INTERFACE

            hdferr = h5zfilter_avail_c(filter, flag)
            status = .TRUE.
            if (flag .EQ. 0) status = .FALSE.
 
          END SUBROUTINE h5zfilter_avail_f

      END MODULE H5Z
            

                                     


