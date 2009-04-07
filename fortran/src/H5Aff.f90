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
! This file contains Fortran90 interfaces for H5A functions.
!
      MODULE H5A

        USE H5GLOBAL
!
!On Windows there are no big (integer*8) integers, so overloading 
!for bug #670 does not work. I have to use DEC compilation directives to make
!Windows DEC Visual Fortran and OSF compilers happy and do right things.
!						05/01/02 EP
          
!
          INTERFACE h5awrite_f

            MODULE PROCEDURE h5awrite_integer_scalar 
            MODULE PROCEDURE h5awrite_integer_1 
            MODULE PROCEDURE h5awrite_integer_2 
            MODULE PROCEDURE h5awrite_integer_3 
            MODULE PROCEDURE h5awrite_integer_4 
            MODULE PROCEDURE h5awrite_integer_5 
            MODULE PROCEDURE h5awrite_integer_6 
            MODULE PROCEDURE h5awrite_integer_7 
            MODULE PROCEDURE h5awrite_char_scalar 
            MODULE PROCEDURE h5awrite_char_1 
            MODULE PROCEDURE h5awrite_char_2 
            MODULE PROCEDURE h5awrite_char_3 
            MODULE PROCEDURE h5awrite_char_4 
            MODULE PROCEDURE h5awrite_char_5 
            MODULE PROCEDURE h5awrite_char_6 
            MODULE PROCEDURE h5awrite_char_7 
            MODULE PROCEDURE h5awrite_real_scalar
            MODULE PROCEDURE h5awrite_real_1
            MODULE PROCEDURE h5awrite_real_2
            MODULE PROCEDURE h5awrite_real_3
            MODULE PROCEDURE h5awrite_real_4
            MODULE PROCEDURE h5awrite_real_5
            MODULE PROCEDURE h5awrite_real_6
            MODULE PROCEDURE h5awrite_real_7
! Comment if on Crays
            MODULE PROCEDURE h5awrite_double_scalar
            MODULE PROCEDURE h5awrite_double_1
            MODULE PROCEDURE h5awrite_double_2
            MODULE PROCEDURE h5awrite_double_3
            MODULE PROCEDURE h5awrite_double_4
            MODULE PROCEDURE h5awrite_double_5
            MODULE PROCEDURE h5awrite_double_6
            MODULE PROCEDURE h5awrite_double_7
! End commnet if on Crays


          END INTERFACE

          INTERFACE h5aread_f

            MODULE PROCEDURE h5aread_integer_scalar
            MODULE PROCEDURE h5aread_integer_1 
            MODULE PROCEDURE h5aread_integer_2 
            MODULE PROCEDURE h5aread_integer_3 
            MODULE PROCEDURE h5aread_integer_4 
            MODULE PROCEDURE h5aread_integer_5 
            MODULE PROCEDURE h5aread_integer_6 
            MODULE PROCEDURE h5aread_integer_7 
            MODULE PROCEDURE h5aread_char_scalar 
            MODULE PROCEDURE h5aread_char_1 
            MODULE PROCEDURE h5aread_char_2 
            MODULE PROCEDURE h5aread_char_3 
            MODULE PROCEDURE h5aread_char_4 
            MODULE PROCEDURE h5aread_char_5 
            MODULE PROCEDURE h5aread_char_6 
            MODULE PROCEDURE h5aread_char_7 
            MODULE PROCEDURE h5aread_real_scalar
            MODULE PROCEDURE h5aread_real_1
            MODULE PROCEDURE h5aread_real_2
            MODULE PROCEDURE h5aread_real_3
            MODULE PROCEDURE h5aread_real_4
            MODULE PROCEDURE h5aread_real_5
            MODULE PROCEDURE h5aread_real_6
            MODULE PROCEDURE h5aread_real_7
! Comment if on Crays
            MODULE PROCEDURE h5aread_double_scalar
            MODULE PROCEDURE h5aread_double_1
            MODULE PROCEDURE h5aread_double_2
            MODULE PROCEDURE h5aread_double_3
            MODULE PROCEDURE h5aread_double_4
            MODULE PROCEDURE h5aread_double_5
            MODULE PROCEDURE h5aread_double_6
            MODULE PROCEDURE h5aread_double_7
! End commnet if on Crays
!
          END INTERFACE

      CONTAINS

!----------------------------------------------------------------------
! Name:		h5acreate_f 
!
! Purpose: 	Creates a dataset as an attribute of a group, dataset, 
!		or named datatype 
!
! Inputs:  
!		obj_id		- identifier of an object (group, dataset,
!				  or named datatype) attribute is attached to
!		name		- attribute name
!		type_id		- attribute datatype identifier
!		space_id	- attribute dataspace identifier
!
! Outputs:  
!		attr_id		- attribute identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!		creation_prp	- creation property list identifier 			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces are added for 
!			called C functions (it is needed for Windows
!			port).  February 27, 2001 
!
!----------------------------------------------------------------------
          SUBROUTINE h5acreate_f(obj_id, name, type_id, space_id, attr_id, &
                                 hdferr, creation_prp) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5acreate_f
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name    ! Attribute name
            INTEGER(HID_T), INTENT(IN) :: type_id   
                                           ! Attribute datatype identifier 
            INTEGER(HID_T), INTENT(IN) :: space_id  
                                           ! Attribute dataspace identifier
            INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: creation_prp
                                                ! Attribute creation property 
                                                ! list identifier 
            INTEGER(HID_T) :: creation_prp_default 
            INTEGER(SIZE_T) :: namelen
!            INTEGER, EXTERNAL :: h5acreate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5acreate_c(obj_id, name, namelen, type_id, &
                               space_id, creation_prp_default, attr_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5ACREATE_C'::h5acreate_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: obj_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER(SIZE_T) :: namelen
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER(HID_T) :: creation_prp_default
              INTEGER(HID_T), INTENT(OUT) :: attr_id
              END FUNCTION h5acreate_c
            END INTERFACE

            creation_prp_default = H5P_DEFAULT_F
            namelen = LEN(NAME)
            if (present(creation_prp)) creation_prp_default = creation_prp
            hdferr = h5acreate_c(obj_id, name, namelen, type_id, space_id, &
                                 creation_prp_default, attr_id)
          END SUBROUTINE h5acreate_f


!----------------------------------------------------------------------
! Name:		h5aopen_name_f 
!
! Purpose:  	Opens an attribute specified by name. 
!
! Inputs:  	
!		obj_id 		- identifier of a group, dataset, or named 
!				  datatype atttribute to be attached to
!		name		- attribute name
! Outputs:  
!		attr_id		- attribute identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces are added for 
!			called C functions (it is needed for Windows
!			port).  February 27, 2001 
!
!----------------------------------------------------------------------

          SUBROUTINE h5aopen_name_f(obj_id, name, attr_id, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aopen_name_f
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name    ! Attribute name
            INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER(SIZE_T) :: namelen

!            INTEGER, EXTERNAL :: h5aopen_name_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aopen_name_c(obj_id, name, namelen, attr_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AOPEN_NAME_C'::h5aopen_name_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: obj_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER(SIZE_T) :: namelen
              INTEGER(HID_T), INTENT(OUT) :: attr_id
              END FUNCTION h5aopen_name_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5aopen_name_c(obj_id, name, namelen, attr_id)
          END SUBROUTINE h5aopen_name_f


!----------------------------------------------------------------------
! Name:		h5aopen_idx_f 
!
! Purpose:  	Opens the attribute specified by its index.
!
! Inputs:  
!		obj_id		- identifier of a group, dataset, or named
!				  datatype an attribute to be attached to
!		index		- index of the attribute to open (zero-based)
! Outputs:  
!		attr_id		- attribute identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces are added for 
!			called C functions (it is needed for Windows
!			port).  February 27, 2001 
!
!----------------------------------------------------------------------

          SUBROUTINE h5aopen_idx_f(obj_id, index, attr_id, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aopen_idx_f
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier 
            INTEGER, INTENT(IN) :: index            ! Attribute index 
            INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aopen_idx_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aopen_idx_c(obj_id, index, attr_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AOPEN_IDX_C'::h5aopen_idx_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: obj_id
              INTEGER, INTENT(IN) :: index
              INTEGER(HID_T), INTENT(OUT) :: attr_id
              END FUNCTION h5aopen_idx_c
            END INTERFACE

            hdferr = h5aopen_idx_c(obj_id, index, attr_id)
          END SUBROUTINE h5aopen_idx_f


          SUBROUTINE h5awrite_integer_scalar(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_integer_scalar
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(IN) :: buf              ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_integer_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_INTEGER_S_C'::h5awrite_integer_s_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(IN)::buf
              END FUNCTION h5awrite_integer_s_c
            END INTERFACE

            hdferr = h5awrite_integer_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_integer_scalar

          SUBROUTINE h5awrite_integer_1(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_integer_1
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(IN) , &
            DIMENSION(dims(1)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_integer_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_INTEGER_1_C'::h5awrite_integer_1_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5awrite_integer_1_c
            END INTERFACE

            hdferr = h5awrite_integer_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_integer_1


          SUBROUTINE h5awrite_integer_2(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_integer_2
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(IN) , &
            DIMENSION(dims(1),dims(2)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_integer_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_INTEGER_2_C'::h5awrite_integer_2_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5awrite_integer_2_c
            END INTERFACE

            hdferr = h5awrite_integer_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_integer_2


          SUBROUTINE h5awrite_integer_3(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_integer_3
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(IN) , &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_integer_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_INTEGER_3_C'::h5awrite_integer_3_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5awrite_integer_3_c
            END INTERFACE

            hdferr = h5awrite_integer_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_integer_3


          SUBROUTINE h5awrite_integer_4(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_integer_4
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_integer_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_INTEGER_4_C'::h5awrite_integer_4_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5awrite_integer_4_c
            END INTERFACE

            hdferr = h5awrite_integer_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_integer_4


          SUBROUTINE h5awrite_integer_5(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_integer_5
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_integer_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_INTEGER_5_C'::h5awrite_integer_5_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5awrite_integer_5_c
            END INTERFACE

            hdferr = h5awrite_integer_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_integer_5


          SUBROUTINE h5awrite_integer_6(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_integer_6
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_integer_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_INTEGER_6_C'::h5awrite_integer_6_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5awrite_integer_6_c
            END INTERFACE

            hdferr = h5awrite_integer_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_integer_6


          SUBROUTINE h5awrite_integer_7(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_integer_7
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_integer_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_INTEGER_7_C'::h5awrite_integer_7_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5awrite_integer_7_c
            END INTERFACE

            hdferr = h5awrite_integer_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_integer_7


          SUBROUTINE h5awrite_real_scalar(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_real_scalar
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(IN) :: buf                 ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_REAL_S_C'::h5awrite_real_s_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN)::buf
              END FUNCTION h5awrite_real_s_c
            END INTERFACE

            hdferr = h5awrite_real_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_scalar

          SUBROUTINE h5awrite_real_1(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_real_1
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(IN), &
            DIMENSION(dims(1)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_REAL_1_C'::h5awrite_real_1_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5awrite_real_1_c
            END INTERFACE

            hdferr = h5awrite_real_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_1


          SUBROUTINE h5awrite_real_2(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_real_2
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_REAL_2_C'::h5awrite_real_2_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5awrite_real_2_c
            END INTERFACE

            hdferr = h5awrite_real_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_2


          SUBROUTINE h5awrite_real_3(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_real_3
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_REAL_3_C'::h5awrite_real_3_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5awrite_real_3_c
            END INTERFACE

            hdferr = h5awrite_real_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_3


          SUBROUTINE h5awrite_real_4(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_real_4
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_REAL_4_C'::h5awrite_real_4_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5awrite_real_4_c
            END INTERFACE

            hdferr = h5awrite_real_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_4


          SUBROUTINE h5awrite_real_5(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_real_5
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_REAL_5_C'::h5awrite_real_5_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5awrite_real_5_c
            END INTERFACE

            hdferr = h5awrite_real_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_5


          SUBROUTINE h5awrite_real_6(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_real_6
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_REAL_6_C'::h5awrite_real_6_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5awrite_real_6_c
            END INTERFACE

            hdferr = h5awrite_real_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_6


          SUBROUTINE h5awrite_real_7(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_real_7
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_REAL_7_C'::h5awrite_real_7_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5awrite_real_7_c
            END INTERFACE

            hdferr = h5awrite_real_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_7


          SUBROUTINE h5awrite_double_scalar(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_double_scalar
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(IN) :: buf     ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_double_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_double_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_DOUBLE_S_C'::h5awrite_double_s_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(IN)::buf
              END FUNCTION h5awrite_double_s_c
            END INTERFACE

            hdferr = h5awrite_double_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_double_scalar

          SUBROUTINE h5awrite_double_1(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_double_1
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1)) :: buf ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_double_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_double_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_DOUBLE_1_C'::h5awrite_double_1_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5awrite_double_1_c
            END INTERFACE

            hdferr = h5awrite_double_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_double_1


          SUBROUTINE h5awrite_double_2(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_double_2
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(IN), & 
            DIMENSION(dims(1),dims(2)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_double_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_double_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_DOUBLE_2_C'::h5awrite_double_2_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5awrite_double_2_c
            END INTERFACE

            hdferr = h5awrite_double_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_double_2


          SUBROUTINE h5awrite_double_3(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_double_3
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_double_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_double_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_DOUBLE_3_C'::h5awrite_double_3_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5awrite_double_3_c
            END INTERFACE

            hdferr = h5awrite_double_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_double_3


          SUBROUTINE h5awrite_double_4(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_double_4
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_double_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_double_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_DOUBLE_4_C'::h5awrite_double_4_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5awrite_double_4_c
            END INTERFACE

            hdferr = h5awrite_double_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_double_4


          SUBROUTINE h5awrite_double_5(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_double_5
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_double_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_double_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_DOUBLE_5_C'::h5awrite_double_5_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5awrite_double_5_c
            END INTERFACE

            hdferr = h5awrite_double_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_double_5


          SUBROUTINE h5awrite_double_6(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_double_6
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_double_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_double_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_DOUBLE_6_C'::h5awrite_double_6_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5awrite_double_6_c
            END INTERFACE

            hdferr = h5awrite_double_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_double_6


          SUBROUTINE h5awrite_double_7(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_double_7
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_double_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_double_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITE_DOUBLE_7_C'::h5awrite_double_7_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5awrite_double_7_c
            END INTERFACE

            hdferr = h5awrite_double_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_double_7

          SUBROUTINE h5awrite_char_scalar(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_char_scalar
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*),INTENT(IN) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITEC_S_C'::h5awritec_s_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN)::buf
              END FUNCTION h5awritec_s_c
            END INTERFACE

            hdferr = h5awritec_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_scalar

          SUBROUTINE h5awrite_char_1(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_char_1
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITEC_1_C'::h5awritec_1_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1))::buf
              END FUNCTION h5awritec_1_c
            END INTERFACE

            hdferr = h5awritec_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_1


          SUBROUTINE h5awrite_char_2(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_char_2
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awritec_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITEC_2_C'::h5awritec_2_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5awritec_2_c
            END INTERFACE

            hdferr = h5awritec_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_2


          SUBROUTINE h5awrite_char_3(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_char_3
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITEC_3_C'::h5awritec_3_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5awritec_3_c
            END INTERFACE

            hdferr = h5awritec_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_3


          SUBROUTINE h5awrite_char_4(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_char_4
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITEC_4_C'::h5awritec_4_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5awritec_4_c
            END INTERFACE

            hdferr = h5awritec_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_4


          SUBROUTINE h5awrite_char_5(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_char_5
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITEC_5_C'::h5awritec_5_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5awritec_5_c
            END INTERFACE

            hdferr = h5awritec_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_5


          SUBROUTINE h5awrite_char_6(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_char_6
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITEC_6_C'::h5awritec_6_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5awritec_6_c
            END INTERFACE

            hdferr = h5awritec_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_6


          SUBROUTINE h5awrite_char_7(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_char_7
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AWRITEC_7_C'::h5awritec_7_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5awritec_7_c
            END INTERFACE

            hdferr = h5awritec_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_7

!----------------------------------------------------------------------
! Name:		h5aread_f 
!
! Purpose:  	Reads an attribute.
!
! Inputs:  
!		attr_id		- attribute identifier
!		memtype_id	- attribute memory type identifier
!		dims		- 1D array of size 7, stores sizes of the 
!				- buf array dimensions.
! Outputs:  
!		buf		- buffer to read attribute data in
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces are added for 
!			called C functions (it is needed for Windows
!			port).  February 27, 2001 
!
!			dims parameter was added to make code portable;
!			Aprile 4, 2001
!
! Comment:		This function is overloaded to write INTEGER,
!			REAL, DOUBLE PRECISION and CHARACTER buffers
!			up to 7 dimensions.	
!----------------------------------------------------------------------

          SUBROUTINE h5aread_integer_scalar(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_integer_scalar
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(OUT) :: buf             ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_INTEGER_S_C'::h5aread_integer_s_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(OUT)::buf
              END FUNCTION h5aread_integer_s_c
            END INTERFACE

            hdferr = h5aread_integer_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_scalar

          SUBROUTINE h5aread_integer_1(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_integer_1
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(OUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_INTEGER_1_C'::h5aread_integer_1_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5aread_integer_1_c
            END INTERFACE

            hdferr = h5aread_integer_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_1


          SUBROUTINE h5aread_integer_2(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_integer_2
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(OUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_INTEGER_2_C'::h5aread_integer_2_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5aread_integer_2_c
            END INTERFACE

            hdferr = h5aread_integer_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_2


          SUBROUTINE h5aread_integer_3(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_integer_3
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_INTEGER_3_C'::h5aread_integer_3_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5aread_integer_3_c
            END INTERFACE

            hdferr = h5aread_integer_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_3


          SUBROUTINE h5aread_integer_4(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_integer_4
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_INTEGER_4_C'::h5aread_integer_4_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5aread_integer_4_c
            END INTERFACE

            hdferr = h5aread_integer_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_4


          SUBROUTINE h5aread_integer_5(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_integer_5
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_INTEGER_5_C'::h5aread_integer_5_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5aread_integer_5_c
            END INTERFACE

            hdferr = h5aread_integer_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_5


          SUBROUTINE h5aread_integer_6(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_integer_6
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_INTEGER_6_C'::h5aread_integer_6_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5aread_integer_6_c
            END INTERFACE

            hdferr = h5aread_integer_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_6


          SUBROUTINE h5aread_integer_7(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_integer_7
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            INTEGER, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_INTEGER_7_C'::h5aread_integer_7_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5aread_integer_7_c
            END INTERFACE

            hdferr = h5aread_integer_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_7


          SUBROUTINE h5aread_real_scalar(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_real_scalar
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(OUT) :: buf                ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_REAL_S_C'::h5aread_real_s_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(OUT)::buf
              END FUNCTION h5aread_real_s_c
            END INTERFACE

            hdferr = h5aread_real_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_scalar

          SUBROUTINE h5aread_real_1(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_real_1
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(OUT), &
            DIMENSION(dims(1)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_REAL_1_C'::h5aread_real_1_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(OUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5aread_real_1_c
            END INTERFACE

            hdferr = h5aread_real_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_1


          SUBROUTINE h5aread_real_2(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_real_2
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(OUT), &
            DIMENSION(dims(1),dims(2)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_REAL_2_C'::h5aread_real_2_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(OUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5aread_real_2_c
            END INTERFACE

            hdferr = h5aread_real_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_2


          SUBROUTINE h5aread_real_3(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_real_3
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_REAL_3_C'::h5aread_real_3_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5aread_real_3_c
            END INTERFACE

            hdferr = h5aread_real_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_3


          SUBROUTINE h5aread_real_4(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_real_4
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_REAL_4_C'::h5aread_real_4_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5aread_real_4_c
            END INTERFACE

            hdferr = h5aread_real_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_4


          SUBROUTINE h5aread_real_5(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_real_5
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_REAL_5_C'::h5aread_real_5_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5aread_real_5_c
            END INTERFACE

            hdferr = h5aread_real_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_5


          SUBROUTINE h5aread_real_6(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_real_6
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_REAL_6_C'::h5aread_real_6_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5aread_real_6_c
            END INTERFACE

            hdferr = h5aread_real_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_6


          SUBROUTINE h5aread_real_7(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_real_7
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            REAL, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_REAL_7_C'::h5aread_real_7_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5aread_real_7_c
            END INTERFACE

            hdferr = h5aread_real_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_7


          SUBROUTINE h5aread_double_scalar(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_double_scalar
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(OUT) :: buf    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_double_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_double_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_DOUBLE_S_C'::h5aread_double_s_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(OUT)::buf
              END FUNCTION h5aread_double_s_c
            END INTERFACE

            hdferr = h5aread_double_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_double_scalar

          SUBROUTINE h5aread_double_1(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_double_1
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(OUT), &
            DIMENSION(dims(1)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_double_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_double_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_DOUBLE_1_C'::h5aread_double_1_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(OUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5aread_double_1_c
            END INTERFACE

            hdferr = h5aread_double_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_double_1


          SUBROUTINE h5aread_double_2(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_double_2
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(OUT), &
            DIMENSION(dims(1),dims(2)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_double_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_double_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_DOUBLE_2_C'::h5aread_double_2_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(OUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5aread_double_2_c
            END INTERFACE

            hdferr = h5aread_double_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_double_2


          SUBROUTINE h5aread_double_3(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_double_3
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_double_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_double_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_DOUBLE_3_C'::h5aread_double_3_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5aread_double_3_c
            END INTERFACE

            hdferr = h5aread_double_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_double_3


          SUBROUTINE h5aread_double_4(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_double_4
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_double_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_double_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_DOUBLE_4_C'::h5aread_double_4_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5aread_double_4_c
            END INTERFACE

            hdferr = h5aread_double_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_double_4


          SUBROUTINE h5aread_double_5(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_double_5
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_double_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_double_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_DOUBLE_5_C'::h5aread_double_5_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5aread_double_5_c
            END INTERFACE

            hdferr = h5aread_double_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_double_5


          SUBROUTINE h5aread_double_6(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_double_6
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_double_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_double_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_DOUBLE_6_C'::h5aread_double_6_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5aread_double_6_c
            END INTERFACE

            hdferr = h5aread_double_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_double_6


          SUBROUTINE h5aread_double_7(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_double_7
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            DOUBLE PRECISION, INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_double_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_double_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREAD_DOUBLE_7_C'::h5aread_double_7_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              DOUBLE PRECISION, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5aread_double_7_c
            END INTERFACE

            hdferr = h5aread_double_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_double_7


          SUBROUTINE h5aread_char_scalar(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_char_scalar
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(OUT) :: buf 
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREADC_S_C'::h5areadc_s_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*) :: buf
              END FUNCTION h5areadc_s_c
            END INTERFACE

            hdferr = h5areadc_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_scalar

          SUBROUTINE h5aread_char_1(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_char_1
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(OUT), &
            DIMENSION(dims(1)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREADC_1_C'::h5areadc_1_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(OUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5areadc_1_c
            END INTERFACE

            hdferr = h5areadc_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_1


          SUBROUTINE h5aread_char_2(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_char_2
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(OUT), &
            DIMENSION(dims(1),dims(2)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREADC_2_C'::h5areadc_2_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(OUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5areadc_2_c
            END INTERFACE

            hdferr = h5areadc_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_2


          SUBROUTINE h5aread_char_3(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_char_3
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREADC_3_C'::h5areadc_3_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5areadc_3_c
            END INTERFACE

            hdferr = h5areadc_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_3


          SUBROUTINE h5aread_char_4(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_char_4
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREADC_4_C'::h5areadc_4_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5areadc_4_c
            END INTERFACE

            hdferr = h5areadc_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_4


          SUBROUTINE h5aread_char_5(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_char_5
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREADC_5_C'::h5areadc_5_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5areadc_5_c
            END INTERFACE

            hdferr = h5areadc_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_5


          SUBROUTINE h5aread_char_6(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_char_6
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREADC_6_C'::h5areadc_6_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5areadc_6_c
            END INTERFACE

            hdferr = h5areadc_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_6


          SUBROUTINE h5aread_char_7(attr_id, memtype_id,  buf, dims, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_char_7
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier 
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype 
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
            CHARACTER(LEN=*), INTENT(OUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AREADC_7_C'::h5areadc_7_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes 
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5areadc_7_c
            END INTERFACE

            hdferr = h5areadc_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_7


!----------------------------------------------------------------------
! Name:		h5aget_space_f 
!
! Purpose:  	Gets a copy of the dataspace for an attribute.
!
! Inputs:  
!		attr_id		- attribute identifier
! Outputs:  
!		space_id	- attribite dataspace identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces are added for 
!			called C functions (it is needed for Windows
!			port).  February 27, 2001 
!
!----------------------------------------------------------------------

          SUBROUTINE h5aget_space_f(attr_id, space_id, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aget_space_f
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier 
            INTEGER(HID_T), INTENT(OUT) :: space_id 
                                            ! Attribute dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL:: h5aget_space_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aget_space_c(attr_id, space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AGET_SPACE_C'::h5aget_space_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(OUT) :: space_id
              END FUNCTION h5aget_space_c
            END INTERFACE

            hdferr = h5aget_space_c(attr_id, space_id)
          END SUBROUTINE h5aget_space_f

!----------------------------------------------------------------------
! Name:		h5aget_type_f 
!
! Purpose:  	Gets an attribute datatype.
!
! Inputs:  
!		attr_id 	- attribute identifier
! Outputs:  
!		type_id		- attribute datatype identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces are added for 
!			called C functions (it is needed for Windows
!			port).  February 27, 2001 
!
!----------------------------------------------------------------------

          SUBROUTINE h5aget_type_f(attr_id, type_id, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aget_type_f
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier 
            INTEGER(HID_T), INTENT(OUT) :: type_id 
                                              ! Attribute datatype identifier
            INTEGER, INTENT(OUT) :: hdferr    ! Error code

!            INTEGER, EXTERNAL :: h5aget_type_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aget_type_c(attr_id, type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AGET_TYPE_C'::h5aget_type_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(OUT) :: type_id
              END FUNCTION h5aget_type_c
            END INTERFACE

            hdferr = h5aget_type_c(attr_id, type_id)
          END SUBROUTINE h5aget_type_f

!----------------------------------------------------------------------
! Name:		h5aget_name_f 
!
! Purpose: 	Gets an attribute name.  
!
! Inputs:  
!		attr_id		- attribute identifier
!		size		- size of a buffer to read name in
! Outputs:  
!		buf		- buffer to read name in
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces are added for 
!			called C functions (it is needed for Windows
!			port).  February 27, 2001 
!
!----------------------------------------------------------------------


          SUBROUTINE h5aget_name_f(attr_id, size, buf, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aget_name_f
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier 
            INTEGER(SIZE_T), INTENT(IN) :: size            ! Buffer size 
            CHARACTER(LEN=*), INTENT(INOUT) :: buf   
                                               ! Buffer to hold attribute name
            INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                                   ! name length is successful,
                                                   ! -1 if fail
!            INTEGER, EXTERNAL :: h5aget_name_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aget_name_c(attr_id, size, buf)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AGET_NAME_C'::h5aget_name_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(SIZE_T), INTENT(IN) :: size
              CHARACTER(LEN=*), INTENT(OUT) :: buf
              END FUNCTION h5aget_name_c
            END INTERFACE

            hdferr = h5aget_name_c(attr_id, size, buf)
          END SUBROUTINE h5aget_name_f


!----------------------------------------------------------------------
! Name:		h5aget_num_attrs_f 
!
! Purpose:  	Determines the number of attributes attached to an object.
!
! Inputs:  
!		obj_id		- object (group, dataset, or named datatype)
!				  identifier
! Outputs:  
!		attr_num	- number of attributes attached to the object
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces are added for 
!			called C functions (it is needed for Windows
!			port).  February 27, 2001 
!
!----------------------------------------------------------------------

          SUBROUTINE h5aget_num_attrs_f(obj_id, attr_num, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aget_num_attrs_f
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id  ! Object identifier 
            INTEGER, INTENT(OUT) :: attr_num      ! Number of attributes of the
                                                  ! object
            INTEGER, INTENT(OUT) :: hdferr         ! Error code

!            INTEGER, EXTERNAL :: h5aget_num_attrs_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aget_num_attrs_c(obj_id, attr_num)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5AGET_NUM_ATTRS_C'::h5aget_num_attrs_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: obj_id
              INTEGER, INTENT(OUT) :: attr_num
              END FUNCTION h5aget_num_attrs_c
            END INTERFACE

            hdferr = h5aget_num_attrs_c(obj_id, attr_num)
          END SUBROUTINE h5aget_num_attrs_f

!----------------------------------------------------------------------
! Name:		h5adelete_f 
!
! Purpose:  	Deletes an attribute of an object (group, dataset or
!		named datatype)
!
! Inputs:  
!		obj_id		- object identifier
!		name		- attribute name
! Outputs:  
!
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces are added for 
!			called C functions (it is needed for Windows
!			port).  February 27, 2001 
!
!----------------------------------------------------------------------

          SUBROUTINE h5adelete_f(obj_id, name, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5adelete_f
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name    ! Attribute name
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER(SIZE_T) :: namelen

!            INTEGER, EXTERNAL ::  h5adelete_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5adelete_c(obj_id, name, namelen) 
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5ADELETE_C'::h5adelete_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: obj_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER(SIZE_T) :: namelen
              END FUNCTION h5adelete_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5adelete_c(obj_id, name, namelen)
          END SUBROUTINE h5adelete_f

!----------------------------------------------------------------------
! Name:		h5aclose_f 
!
! Purpose:  	Closes the specified attribute.
!		
! Inputs:  
!		attr_id		- attribute identifier
! Outputs:  
!
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!				NONE			
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces are added for 
!			called C functions (it is needed for Windows
!			port).  February 27, 2001 
!
!----------------------------------------------------------------------

          SUBROUTINE h5aclose_f(attr_id, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aclose_f
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code:

!            INTEGER, EXTERNAL :: h5aclose_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aclose_c(attr_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5ACLOSE_C'::h5aclose_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: attr_id
              END FUNCTION h5aclose_c
            END INTERFACE

            hdferr = h5aclose_c(attr_id)
          END SUBROUTINE h5aclose_f

   END MODULE H5A
