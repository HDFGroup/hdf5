
!----------------------------------------------------------------------
! Name:		h5open_f 
!
! Purpose:	Initializes the HDF5 library and Fortran90 interface.   	
!
! Inputs:  
! Outputs:  
!		error:		- error code		
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
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------
      SUBROUTINE h5open_f(error)
        USE H5GLOBAL

        IMPLICIT NONE
        INTEGER, INTENT(OUT) :: error
        INTEGER :: error_0, error_1, error_2
!        INTEGER, EXTERNAL :: h5init_types_c
!        INTEGER, EXTERNAL :: h5init_flags_c
!        INTEGER, EXTERNAL :: h5open_c
        
!
! MS FORTRAN needs explicit interfaces for C functions called here.
!
        INTERFACE
          INTEGER FUNCTION h5open_c()
          !MS$ATTRIBUTES C,reference,alias:'_H5OPEN_C'::h5dopen_c
          END FUNCTION h5open_c
        END INTERFACE
        INTERFACE
          INTEGER FUNCTION h5init_types_c(predef_types, floating_types, integer_types)
          USE H5GLOBAL
          INTEGER(HID_T), DIMENSION(PREDEF_TYPES_LEN) :: predef_types
          INTEGER(HID_T), DIMENSION(FLOATING_TYPES_LEN) :: floating_types
          INTEGER(HID_T), DIMENSION(INTEGER_TYPES_LEN) :: integer_types   
          !MS$ATTRIBUTES C,reference,alias:'_H5INIT_TYPES_C'::h5init_types_c
          END FUNCTION h5init_types_c
        END INTERFACE
        INTERFACE
          INTEGER FUNCTION h5init_flags_c(H5D_flags, &
                                H5E_flags, &
                                H5F_flags, &
                                H5FD_flags, &
                                H5G_flags, &
                                H5I_flags, &
                                H5P_flags, &
                                H5R_flags, &
                                H5S_flags, &
                                H5T_flags  )
          USE H5GLOBAL
          INTEGER H5F_flags(H5F_FLAGS_LEN)
          INTEGER H5G_flags(H5G_FLAGS_LEN)
          INTEGER H5D_flags(H5D_FLAGS_LEN)
          INTEGER H5FD_flags(H5FD_FLAGS_LEN)
          INTEGER H5E_flags(H5E_FLAGS_LEN)
          INTEGER H5I_flags(H5I_FLAGS_LEN)
          INTEGER H5P_flags(H5P_FLAGS_LEN)
          INTEGER H5R_flags(H5R_FLAGS_LEN)
          INTEGER H5S_flags(H5S_FLAGS_LEN)
          INTEGER H5T_flags(H5T_FLAGS_LEN)

          !MS$ATTRIBUTES C,reference,alias:'_H5INIT_FLAGS_C'::h5init_flags_c
          END FUNCTION h5init_flags_c
        END INTERFACE
        error_0 = h5open_c()
        error_1 = h5init_types_c(predef_types, floating_types, integer_types)
        error_2 = h5init_flags_c(H5D_flags, &
                                H5E_flags, &
                                H5F_flags, &
                                H5FD_flags, &
                                H5G_flags, &
                                H5I_flags, &
                                H5P_flags, &
                                H5R_flags, &
                                H5S_flags, &
                                H5T_flags  )
        error = error_0 + error_1 + error_2

      END SUBROUTINE h5open_f

!----------------------------------------------------------------------
! Name:		h5close_f 
!
! Purpose:	Closes the HDF5 library and Fortran90 interface.   	
!
! Inputs:  
! Outputs:  
!		error:		- error code		
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
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------

      SUBROUTINE h5close_f(error)
        USE H5GLOBAL

        IMPLICIT NONE
        INTEGER :: error_1, error_2
        INTEGER, INTENT(OUT) :: error
!        INTEGER, EXTERNAL :: h5close_types_c, h5close_c
        INTERFACE
          INTEGER FUNCTION h5close_c()
          !MS$ATTRIBUTES C,reference,alias:'_H5CLOSE_C'::h5close_c
          END FUNCTION h5close_c
        END INTERFACE
        INTERFACE
          INTEGER FUNCTION h5close_types_c(predef_types, PREDEF_TYPES_LEN, &
                                         floating_types, FLOATING_TYPES_LEN, &
                                         integer_types, INTEGER_TYPES_LEN )
          USE H5GLOBAL
          INTEGER PREDEF_TYPES_LEN
          INTEGER FLOATING_TYPES_LEN
          INTEGER INTEGER_TYPES_LEN
          INTEGER(HID_T), DIMENSION(PREDEF_TYPES_LEN) :: predef_types
          INTEGER(HID_T), DIMENSION(FLOATING_TYPES_LEN) :: floating_types
          INTEGER(HID_T), DIMENSION(INTEGER_TYPES_LEN) :: integer_types   
          !MS$ATTRIBUTES C,reference,alias:'_H5CLOSE_TYPES_C'::h5close_types_c
          END FUNCTION h5close_types_c
        END INTERFACE
        error_1 = h5close_types_c(predef_types, PREDEF_TYPES_LEN, &
                                floating_types, FLOATING_TYPES_LEN, &
                                integer_types, INTEGER_TYPES_LEN )
        error_2 = h5close_c()
        error = error_1 + error_2

      END SUBROUTINE h5close_f
        
