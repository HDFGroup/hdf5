!
! This file contains FORTRAN90 interfaces for H5I functions
!
      MODULE H5I

        USE H5GLOBAL
      
      CONTAINS

!----------------------------------------------------------------------
! Name:		h5iget_type_f 
!
! Purpose:	Retrieves the type of an object.  	
!
! Inputs: 	obj_id		- object identifier 
! Outputs:  
!		type		- type of the object, possible values:   
!				  H5I_FILE_F
!				  H5I_GROUP_F
!				  H5I_DATATYPE_F
!				  H5I_DATASPACE_F
!				  H5I_DATASET_F
!				  H5I_ATTR_F
!				  H5I_BADID_F
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
          SUBROUTINE h5iget_type_f(obj_id, type, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id  !Object identifier 
            INTEGER, INTENT(OUT) :: type !type of an object. 
                                         !possible values are:
                                         !H5I_FILE_F
                                         !H5I_GROUP_F
                                         !H5I_DATATYPE_F
                                         !H5I_DATASPACE_F
                                         !H5I_DATASET_F
                                         !H5I_ATTR_F
                                         !H5I_BADID_F
            INTEGER, INTENT(OUT) :: hdferr  ! Error code

!            INTEGER, EXTERNAL :: h5iget_type_c
!  Interface is needed for MS FORTRAN
!
            INTERFACE
              INTEGER FUNCTION h5iget_type_c(obj_id, type)
              USE H5GLOBAL
              !MS$ATTRIBUTES C,reference,alias:'_H5IGET_TYPE_C':: h5iget_type_c
              INTEGER(HID_T), INTENT(IN) :: obj_id 
              INTEGER, INTENT(OUT) :: type
              END FUNCTION h5iget_type_c
            END INTERFACE
            hdferr = h5iget_type_c(obj_id, type)
          END SUBROUTINE h5iget_type_f

      END MODULE H5I
            

                                     


