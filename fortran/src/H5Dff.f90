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
! This file contains Fortran90 interfaces for H5D functions.
! 
      MODULE H5D
        USE H5GLOBAL
!        USE H5R
!
!On Window: there are no big (integer*8) integers, so overloading 
!for bug #670 does not work. I have to use DEC compilation directives to make
!Windows DEC Visual Fortran and OSF compilers happy and do right things.
!						05/01/02 EP
          
!
!DEC$ IF DEFINED(HDF5F90_WINDOWS)
!DEC$ DEFINE OVERLOAD_FLAG = 0 
!DEC$ ELSE
!DEC$ DEFINE OVERLOAD_FLAG = 1 
!DEC$ ENDIF

          INTERFACE h5dwrite_f

            MODULE PROCEDURE h5dwrite_reference_obj
            MODULE PROCEDURE h5dwrite_reference_dsetreg
            MODULE PROCEDURE h5dwrite_integer_scalar
            MODULE PROCEDURE h5dwrite_integer_1 
            MODULE PROCEDURE h5dwrite_integer_2 
            MODULE PROCEDURE h5dwrite_integer_3 
            MODULE PROCEDURE h5dwrite_integer_4 
            MODULE PROCEDURE h5dwrite_integer_5 
            MODULE PROCEDURE h5dwrite_integer_6 
            MODULE PROCEDURE h5dwrite_integer_7 
            MODULE PROCEDURE h5dwrite_char_scalar
            MODULE PROCEDURE h5dwrite_char_1 
            MODULE PROCEDURE h5dwrite_char_2 
            MODULE PROCEDURE h5dwrite_char_3 
            MODULE PROCEDURE h5dwrite_char_4 
            MODULE PROCEDURE h5dwrite_char_5 
            MODULE PROCEDURE h5dwrite_char_6 
            MODULE PROCEDURE h5dwrite_char_7 
            MODULE PROCEDURE h5dwrite_real_scalar
            MODULE PROCEDURE h5dwrite_real_1
            MODULE PROCEDURE h5dwrite_real_2
            MODULE PROCEDURE h5dwrite_real_3
            MODULE PROCEDURE h5dwrite_real_4
            MODULE PROCEDURE h5dwrite_real_5
            MODULE PROCEDURE h5dwrite_real_6
            MODULE PROCEDURE h5dwrite_real_7
! Comment if on T3E
            MODULE PROCEDURE h5dwrite_double_scalar
            MODULE PROCEDURE h5dwrite_double_1
            MODULE PROCEDURE h5dwrite_double_2
            MODULE PROCEDURE h5dwrite_double_3
            MODULE PROCEDURE h5dwrite_double_4
            MODULE PROCEDURE h5dwrite_double_5
            MODULE PROCEDURE h5dwrite_double_6
            MODULE PROCEDURE h5dwrite_double_7
! End comment if on T3E
!
!OVERLOADING for bug#670
!
!DEC$ IF (OVERLOAD_FLAG .EQ. 1)
!
            MODULE PROCEDURE h5dwrite_reference_obj_b
            MODULE PROCEDURE h5dwrite_reference_dsetreg_b
            MODULE PROCEDURE h5dwrite_integer_scalar_b
            MODULE PROCEDURE h5dwrite_integer_1_b
            MODULE PROCEDURE h5dwrite_integer_2_b
            MODULE PROCEDURE h5dwrite_integer_3_b
            MODULE PROCEDURE h5dwrite_integer_4_b
            MODULE PROCEDURE h5dwrite_integer_5_b 
            MODULE PROCEDURE h5dwrite_integer_6_b 
            MODULE PROCEDURE h5dwrite_integer_7_b 
            MODULE PROCEDURE h5dwrite_char_scalar_b
            MODULE PROCEDURE h5dwrite_char_1_b
            MODULE PROCEDURE h5dwrite_char_2_b
            MODULE PROCEDURE h5dwrite_char_3_b
            MODULE PROCEDURE h5dwrite_char_4_b
            MODULE PROCEDURE h5dwrite_char_5_b 
            MODULE PROCEDURE h5dwrite_char_6_b 
            MODULE PROCEDURE h5dwrite_char_7_b 
            MODULE PROCEDURE h5dwrite_real_scalar_b
            MODULE PROCEDURE h5dwrite_real_1_b
            MODULE PROCEDURE h5dwrite_real_2_b
            MODULE PROCEDURE h5dwrite_real_3_b
            MODULE PROCEDURE h5dwrite_real_4_b
            MODULE PROCEDURE h5dwrite_real_5_b
            MODULE PROCEDURE h5dwrite_real_6_b
            MODULE PROCEDURE h5dwrite_real_7_b
! Comment if on T3E
            MODULE PROCEDURE h5dwrite_double_scalar_b
            MODULE PROCEDURE h5dwrite_double_1_b
            MODULE PROCEDURE h5dwrite_double_2_b
            MODULE PROCEDURE h5dwrite_double_3_b
            MODULE PROCEDURE h5dwrite_double_4_b
            MODULE PROCEDURE h5dwrite_double_5_b
            MODULE PROCEDURE h5dwrite_double_6_b
            MODULE PROCEDURE h5dwrite_double_7_b
!
!END OVERLOADING
!
!DEC$ ENDIF
!
          END INTERFACE 
          
          INTERFACE h5dread_f

            MODULE PROCEDURE h5dread_reference_obj
            MODULE PROCEDURE h5dread_reference_dsetreg
            MODULE PROCEDURE h5dread_integer_scalar
            MODULE PROCEDURE h5dread_integer_1 
            MODULE PROCEDURE h5dread_integer_2 
            MODULE PROCEDURE h5dread_integer_3 
            MODULE PROCEDURE h5dread_integer_4 
            MODULE PROCEDURE h5dread_integer_5 
            MODULE PROCEDURE h5dread_integer_6 
            MODULE PROCEDURE h5dread_integer_7 
            MODULE PROCEDURE h5dread_char_scalar
            MODULE PROCEDURE h5dread_char_1 
            MODULE PROCEDURE h5dread_char_2 
            MODULE PROCEDURE h5dread_char_3 
            MODULE PROCEDURE h5dread_char_4 
            MODULE PROCEDURE h5dread_char_5 
            MODULE PROCEDURE h5dread_char_6 
            MODULE PROCEDURE h5dread_char_7 
            MODULE PROCEDURE h5dread_real_scalar
            MODULE PROCEDURE h5dread_real_1
            MODULE PROCEDURE h5dread_real_2
            MODULE PROCEDURE h5dread_real_3
            MODULE PROCEDURE h5dread_real_4
            MODULE PROCEDURE h5dread_real_5
            MODULE PROCEDURE h5dread_real_6
            MODULE PROCEDURE h5dread_real_7
! Comment if on T3E
            MODULE PROCEDURE h5dread_double_scalar
            MODULE PROCEDURE h5dread_double_1
            MODULE PROCEDURE h5dread_double_2
            MODULE PROCEDURE h5dread_double_3
            MODULE PROCEDURE h5dread_double_4
            MODULE PROCEDURE h5dread_double_5
            MODULE PROCEDURE h5dread_double_6
            MODULE PROCEDURE h5dread_double_7
! End comment if on T3E
!
!OVERLAODING for bug#670
!
!
!DEC$ IF (OVERLOAD_FLAG .EQ. 1)
!
            MODULE PROCEDURE h5dread_reference_obj_b
            MODULE PROCEDURE h5dread_reference_dsetreg_b
            MODULE PROCEDURE h5dread_integer_scalar_b
            MODULE PROCEDURE h5dread_integer_1_b
            MODULE PROCEDURE h5dread_integer_2_b 
            MODULE PROCEDURE h5dread_integer_3_b 
            MODULE PROCEDURE h5dread_integer_4_b 
            MODULE PROCEDURE h5dread_integer_5_b 
            MODULE PROCEDURE h5dread_integer_6_b 
            MODULE PROCEDURE h5dread_integer_7_b 
            MODULE PROCEDURE h5dread_char_scalar_b
            MODULE PROCEDURE h5dread_char_1_b 
            MODULE PROCEDURE h5dread_char_2_b 
            MODULE PROCEDURE h5dread_char_3_b 
            MODULE PROCEDURE h5dread_char_4_b 
            MODULE PROCEDURE h5dread_char_5_b 
            MODULE PROCEDURE h5dread_char_6_b 
            MODULE PROCEDURE h5dread_char_7_b 
            MODULE PROCEDURE h5dread_real_scalar_b
            MODULE PROCEDURE h5dread_real_1_b
            MODULE PROCEDURE h5dread_real_2_b
            MODULE PROCEDURE h5dread_real_3_b
            MODULE PROCEDURE h5dread_real_4_b
            MODULE PROCEDURE h5dread_real_5_b
            MODULE PROCEDURE h5dread_real_6_b
            MODULE PROCEDURE h5dread_real_7_b
! Comment if on T3E
            MODULE PROCEDURE h5dread_double_scalar_b
            MODULE PROCEDURE h5dread_double_1_b
            MODULE PROCEDURE h5dread_double_2_b
            MODULE PROCEDURE h5dread_double_3_b
            MODULE PROCEDURE h5dread_double_4_b
            MODULE PROCEDURE h5dread_double_5_b
            MODULE PROCEDURE h5dread_double_6_b
            MODULE PROCEDURE h5dread_double_7_b
!
!END OVERLAODING
!
!DEC$ ENDIF

          END INTERFACE 

        CONTAINS
          
!----------------------------------------------------------------------
! Name:		h5dcreate_f 
!
! Purpose: 	Creates a dataset at the specified location 	
!
! Inputs:  
!		loc_id		- file or group identifier
!		name		- dataset name
!		type_id		- dataset datatype identifier
!		space_id	- dataset dataspace identifier
! Outputs:  
!		dset_id		- dataset identifier
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!		createion_prp	- dataset creation property list identifier
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
  
          SUBROUTINE h5dcreate_f(loc_id, name, type_id, space_id, dset_id, & 
                                 hdferr, creation_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dcreate_f
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the dataset 
            INTEGER(HID_T), INTENT(IN) :: type_id  ! Datatype identifier 
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER(HID_T), INTENT(OUT) :: dset_id ! Dataset identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: creation_prp 
                                                   ! Dataset creation propertly
                                                   ! list identifier
            INTEGER :: creation_prp_default
            INTEGER :: namelen                     ! Name length

!            INTEGER, EXTERNAL :: h5dcreate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dcreate_c(loc_id, name, namelen, type_id, &
                                           space_id, creation_prp_default, dset_id)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DCREATE_C'::h5dcreate_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER :: creation_prp_default
              INTEGER(HID_T), INTENT(OUT) :: dset_id
              END FUNCTION h5dcreate_c
            END INTERFACE

            creation_prp_default = H5P_DEFAULT_F
            if (present(creation_prp)) creation_prp_default = creation_prp 
            namelen = LEN(name)
            hdferr = h5dcreate_c(loc_id, name, namelen, type_id, space_id, & 
                                 creation_prp_default, dset_id) 
          END SUBROUTINE h5dcreate_f
          
!----------------------------------------------------------------------
! Name:		h5dopen_f 
!
! Purpose: 	Opens an existing dataset.  	
!
! Inputs:  
!		loc_id		- file or group identifier
!		name		- dataset name
! Outputs:  
!		dset_id		- dataset identifier
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
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5dopen_f(loc_id, name, dset_id, hdferr)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dopen_f
!DEC$endif
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier 
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the dataset 
            INTEGER(HID_T), INTENT(OUT) :: dset_id ! Dataset identifier 
            INTEGER, INTENT(OUT) :: hdferr         ! Error code 
            INTEGER :: namelen                     ! Name length

!            INTEGER, EXTERNAL :: h5dopen_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dopen_c(loc_id, name, namelen, dset_id)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DOPEN_C'::h5dopen_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER(HID_T), INTENT(OUT) :: dset_id
              END FUNCTION h5dopen_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5dopen_c(loc_id, name, namelen, dset_id) 

          END SUBROUTINE h5dopen_f
          
!----------------------------------------------------------------------
! Name:		h5dclose_f 
!
! Purpose: 	Closes a dataset.  	
!
! Inputs:  
!		dset_id		- dataset identifier
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
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5dclose_f(dset_id, hdferr)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dclose_f
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id ! Dataset identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5dclose_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dclose_c(dset_id)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DCLOSE_C'::h5dclose_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              END FUNCTION h5dclose_c
            END INTERFACE

            hdferr = h5dclose_c(dset_id)

          END SUBROUTINE h5dclose_f

!----------------------------------------------------------------------
! Name:		h5dwrite_f 
!
! Purpose: 	Reads raw data from the specified dataset into buf, 
!		converting from file datatype and dataspace to memory 
!		datatype and dataspace.
!
! Inputs:  
!		dset_id		- dataset identifier
!		mem_type_id	- memory type identifier
!		buf		- data buffer to write
!		dims		- 1-dim array of size 7; dims(k) has the size 
!				- of k-th dimension of the buf array
! Outputs:  
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!		mem_space_id	- memory dataspace identifier
!		file_space_id 	- file dataspace identifier
!		xfer_prp	- trasfer property list identifier	
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  February 28, 2001 
!                       
!                       dims parameter was added to make code portable;
!                       n parameter was replaced with dims parameter in
!			the h5dwrite_reference_obj and h5dwrite_reference_dsetreg
!			functions.  April 2, 2001
!
! Comment:		This function is overloaded to write INTEGER,
!			REAL, DOUBLE PRECISION and CHARACTER buffers
!			up to 7 dimensions, and one dimensional buffers
!			of the TYPE(hobj_ref_t_f) and TYPE(hdset_reg_ref_t_f)
!			types.	
!----------------------------------------------------------------------

          SUBROUTINE h5dwrite_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_reference_obj
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, DIMENSION(7), INTENT(IN) :: dims ! size of the bufffer buf
            TYPE(hobj_ref_t_f), DIMENSION(dims(1)), INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j

!            INTEGER, EXTERNAL :: h5dwrite_ref_obj_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_ref_obj_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_REF_OBJ_C'::h5dwrite_ref_obj_c  
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER, DIMENSION(*) :: ref_buf
              INTEGER, DIMENSION(7) :: dims
              END FUNCTION h5dwrite_ref_obj_c
            END INTERFACE 

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 
            
            allocate(ref_buf(REF_OBJ_BUF_LEN*dims(1)), stat=hdferr)
            if (hdferr .NE. 0 ) then
                hdferr = -1
                return
            else
                do j = 1, dims(1)
                  do i = 1, REF_OBJ_BUF_LEN  
                   ref_buf(REF_OBJ_BUF_LEN*(j-1) + i ) = buf(j)%ref(i)
                 enddo  
                enddo  
            endif
            hdferr = h5dwrite_ref_obj_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims(1))
            deallocate(ref_buf)

          END SUBROUTINE h5dwrite_reference_obj

          SUBROUTINE h5dwrite_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_reference_dsetreg
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, DIMENSION(7), INTENT(IN) :: dims ! size of the bufffer buf  
            TYPE(hdset_reg_ref_t_f), DIMENSION(dims(1)), INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j

!            INTEGER, EXTERNAL :: h5dwrite_ref_reg_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_ref_reg_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_REF_REG_C'::h5dwrite_ref_reg_c  
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER, DIMENSION(*) :: ref_buf
              INTEGER, DIMENSION(7) ::  dims
              END FUNCTION h5dwrite_ref_reg_c
            END INTERFACE 


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            allocate(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
            if (hdferr .NE. 0 ) then
                hdferr = -1
                return
            else
                do j = 1, dims(1)
                  do i = 1, REF_REG_BUF_LEN  
                   ref_buf(REF_REG_BUF_LEN*(j-1) + i) = buf(j)%ref(i)
                 enddo
                enddo
            endif
            hdferr = h5dwrite_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims)
            deallocate(ref_buf)

          END SUBROUTINE h5dwrite_reference_dsetreg
           
           
          SUBROUTINE h5dwrite_integer_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_scalar
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(IN) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_integer_scalar

          SUBROUTINE h5dwrite_integer_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_1
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_integer_1

          SUBROUTINE h5dwrite_integer_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_2
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf   ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_2

          SUBROUTINE h5dwrite_integer_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_3
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_3

          SUBROUTINE h5dwrite_integer_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_4
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F
            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_4

          SUBROUTINE h5dwrite_integer_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_5
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F


            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_5

          SUBROUTINE h5dwrite_integer_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_6
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_6

          SUBROUTINE h5dwrite_integer_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_7
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_7


          SUBROUTINE h5dwrite_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_scalar
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(IN) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_scalar

          SUBROUTINE h5dwrite_char_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_1
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_1

          SUBROUTINE h5dwrite_char_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_2
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_2

          SUBROUTINE h5dwrite_char_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_3
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_3

          SUBROUTINE h5dwrite_char_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_4
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_4

          SUBROUTINE h5dwrite_char_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_5
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_5

          SUBROUTINE h5dwrite_char_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_6
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_6

          SUBROUTINE h5dwrite_char_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_7
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_7

          SUBROUTINE h5dwrite_real_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_scalar
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(IN) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default  = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F
            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_scalar

          SUBROUTINE h5dwrite_real_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_1
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_1

          SUBROUTINE h5dwrite_real_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_2
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_2

          SUBROUTINE h5dwrite_real_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_3
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_3

          SUBROUTINE h5dwrite_real_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_4
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_4

          SUBROUTINE h5dwrite_real_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_5
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_5

          SUBROUTINE h5dwrite_real_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_6
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_6

          SUBROUTINE h5dwrite_real_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_7
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_7


          SUBROUTINE h5dwrite_double_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_scalar
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(IN) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_scalar

          SUBROUTINE h5dwrite_double_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_1
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_1

          SUBROUTINE h5dwrite_double_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_2
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_2

          SUBROUTINE h5dwrite_double_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_3
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_3

          SUBROUTINE h5dwrite_double_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_4
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_4

          SUBROUTINE h5dwrite_double_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_5
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_5

          SUBROUTINE h5dwrite_double_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_6
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_6

          SUBROUTINE h5dwrite_double_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_7
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_7

!----------------------------------------------------------------------
! Name:		h5dread_f 
!
! Purpose: 	Reads raw data from the specified dataset into buf, 
!		converting from file datatype and dataspace to memory 
!		datatype and dataspace.
!
! Inputs:  
!		dset_id		- dataset identifier
!		mem_type_id	- memory type identifier
!		dims		- 1-dim array of size 7; dims(k) has the size 
!				- of k-th dimension of the buf array
! Outputs:  
!		buf		- buffer to read data in
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!		mem_space_id	- memory dataspace identifier
!		file_space_id 	- file dataspace identifier
!		xfer_prp	- trasfer property list identifier	
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  February 28, 2001 
!
!                       dims parameter was added to make code portable;
!                       n parameter was replaced with dims parameter in
!			the h5dwrite_reference_obj and h5dwrite_reference_dsetreg
!			functions.  April 2, 2001
!
! Comment:		This function is overloaded to read INTEGER,
!			REAL, DOUBLE PRECISION and CHARACTER buffers
!			up to 7 dimensions, and one dimensional buffers
!			of the TYPE(hobj_ref_t_f) and TYPE(hdset_reg_ref_t_f)
!			types.	
!----------------------------------------------------------------------
          SUBROUTINE h5dread_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_reference_obj
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            TYPE(hobj_ref_t_f), INTENT(INOUT) , &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j  

!            INTEGER, EXTERNAL :: h5dread_ref_obj_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_ref_obj_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_REF_OBJ_C'::h5dread_ref_obj_c  
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, DIMENSION(*) :: ref_buf
              END FUNCTION h5dread_ref_obj_c
            END INTERFACE 

            allocate(ref_buf(REF_OBJ_BUF_LEN*dims(1)), stat=hdferr)
            if (hdferr .NE. 0) then
                hdferr = -1
                return
            endif 

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_ref_obj_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims)
             do j = 1, dims(1)
              do i = 1, REF_OBJ_BUF_LEN  
                    buf(j)%ref(i) = ref_buf(REF_OBJ_BUF_LEN*(j-1) + i)
              enddo
             enddo  
             deallocate(ref_buf) 
          END SUBROUTINE h5dread_reference_obj

          SUBROUTINE h5dread_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_reference_dsetreg
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            TYPE(hdset_reg_ref_t_f), INTENT(INOUT), & 
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j 

!            INTEGER, EXTERNAL :: h5dread_ref_reg_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_ref_reg_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_REF_REG_C'::h5dread_ref_reg_c  
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, DIMENSION(*) :: ref_buf
              END FUNCTION h5dread_ref_reg_c
            END INTERFACE 

            allocate(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
            if (hdferr .NE. 0) then
                hdferr = -1
                return
            endif

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims)
           
            do j = 1, dims(1)
             do i = 1, REF_REG_BUF_LEN  
                   buf(j)%ref(i) = ref_buf(REF_REG_BUF_LEN*(j-1) + i)
             enddo
            enddo   
            deallocate(ref_buf)
          END SUBROUTINE h5dread_reference_dsetreg


          SUBROUTINE h5dread_integer_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_scalar
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(OUT) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_integer_scalar

          SUBROUTINE h5dread_integer_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_1
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_integer_1

          SUBROUTINE h5dread_integer_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_2
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_2

          SUBROUTINE h5dread_integer_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_3
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_3

          SUBROUTINE h5dread_integer_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_4
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_4

          SUBROUTINE h5dread_integer_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_5
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_5

          SUBROUTINE h5dread_integer_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_6
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_6

          SUBROUTINE h5dread_integer_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_7
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_7

          SUBROUTINE h5dread_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_scalar
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(OUT) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_scalar

          SUBROUTINE h5dread_char_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_1
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_1

          SUBROUTINE h5dread_char_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_2
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_2

          SUBROUTINE h5dread_char_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_3
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_3

          SUBROUTINE h5dread_char_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_4
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_4

          SUBROUTINE h5dread_char_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_5
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_5

          SUBROUTINE h5dread_char_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_6
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_6

          SUBROUTINE h5dread_char_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_7
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_7

          SUBROUTINE h5dread_real_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_scalar
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(OUT) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_scalar

          SUBROUTINE h5dread_real_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_1
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_1

          SUBROUTINE h5dread_real_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_2
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_2

          SUBROUTINE h5dread_real_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_3
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(INOUT), & 
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_3

          SUBROUTINE h5dread_real_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_4
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3), dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3), dims(4)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_4

          SUBROUTINE h5dread_real_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_5
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(INOUT), & 
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_5

          SUBROUTINE h5dread_real_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_6
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_6

          SUBROUTINE h5dread_real_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_7
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_7

          SUBROUTINE h5dread_double_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_scalar
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(OUT) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_scalar

          SUBROUTINE h5dread_double_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_1
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_1

          SUBROUTINE h5dread_double_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_2
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(INOUT), & 
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_2

          SUBROUTINE h5dread_double_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_3
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_3

          SUBROUTINE h5dread_double_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_4
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_4

          SUBROUTINE h5dread_double_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_5
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_5

          SUBROUTINE h5dread_double_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_6
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_6

          SUBROUTINE h5dread_double_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_7
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN), DIMENSION(7) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER, INTENT(IN), DIMENSION(7) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_7
!
!OVERLOADING for bug#670
!
!
!DEC$ IF (OVERLOAD_FLAG .EQ. 1)
!
          SUBROUTINE h5dwrite_reference_obj_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_reference_obj_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf
            TYPE(hobj_ref_t_f), DIMENSION(dims(1)), INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j

!            INTEGER, EXTERNAL :: h5dwrite_ref_obj_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_ref_obj_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_REF_OBJ_C'::h5dwrite_ref_obj_c  
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER, DIMENSION(*) :: ref_buf
              INTEGER(HSIZE_T), DIMENSION(*) :: dims
              END FUNCTION h5dwrite_ref_obj_c
            END INTERFACE 

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 
            
            allocate(ref_buf(REF_OBJ_BUF_LEN*dims(1)), stat=hdferr)
            if (hdferr .NE. 0 ) then
                hdferr = -1
                return
            else
                do j = 1, dims(1)
                  do i = 1, REF_OBJ_BUF_LEN  
                   ref_buf(REF_OBJ_BUF_LEN*(j-1) + i ) = buf(j)%ref(i)
                 enddo  
                enddo  
            endif
            hdferr = h5dwrite_ref_obj_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims(1))
            deallocate(ref_buf)

          END SUBROUTINE h5dwrite_reference_obj_b

          SUBROUTINE h5dwrite_reference_dsetreg_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_reference_dsetreg_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf  
            TYPE(hdset_reg_ref_t_f), DIMENSION(dims(1)), INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j

!            INTEGER, EXTERNAL :: h5dwrite_ref_reg_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_ref_reg_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_REF_REG_C'::h5dwrite_ref_reg_c  
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER, DIMENSION(*) :: ref_buf
              INTEGER(HSIZE_T), DIMENSION(*) ::  dims
              END FUNCTION h5dwrite_ref_reg_c
            END INTERFACE 


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            allocate(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
            if (hdferr .NE. 0 ) then
                hdferr = -1
                return
            else
                do j = 1, dims(1)
                  do i = 1, REF_REG_BUF_LEN  
                   ref_buf(REF_REG_BUF_LEN*(j-1) + i) = buf(j)%ref(i)
                 enddo
                enddo
            endif
            hdferr = h5dwrite_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims)
            deallocate(ref_buf)

          END SUBROUTINE h5dwrite_reference_dsetreg_b
           
           
          SUBROUTINE h5dwrite_integer_scalar_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_scalar_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN) :: buf ! Data buffer
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_integer_scalar_b

          SUBROUTINE h5dwrite_integer_1_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_1_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_integer_1_b

          SUBROUTINE h5dwrite_integer_2_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_2_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf   ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_2_b

          SUBROUTINE h5dwrite_integer_3_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_3_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_3_b

          SUBROUTINE h5dwrite_integer_4_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_4_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F
            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_4_b

          SUBROUTINE h5dwrite_integer_5_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_5_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F


            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_5_b

          SUBROUTINE h5dwrite_integer_6_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_6_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_6_b

          SUBROUTINE h5dwrite_integer_7_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_integer_7_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dwrite_integer_7_b


          SUBROUTINE h5dwrite_char_scalar_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_scalar_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_scalar_b

          SUBROUTINE h5dwrite_char_1_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_1_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_1_b

          SUBROUTINE h5dwrite_char_2_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_2_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_2_b

          SUBROUTINE h5dwrite_char_3_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_3_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_3_b

          SUBROUTINE h5dwrite_char_4_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_4_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_4_b

          SUBROUTINE h5dwrite_char_5_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_5_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_5_b

          SUBROUTINE h5dwrite_char_6_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_6_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_6_b

          SUBROUTINE h5dwrite_char_7_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_char_7_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwritec_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITEC_C'::h5dwritec_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dwritec_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwritec_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_char_7_b

          SUBROUTINE h5dwrite_real_scalar_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_scalar_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default  = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F
            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_scalar_b

          SUBROUTINE h5dwrite_real_1_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_1_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_1_b

          SUBROUTINE h5dwrite_real_2_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_2_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_2_b

          SUBROUTINE h5dwrite_real_3_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_3_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_3_b

          SUBROUTINE h5dwrite_real_4_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_4_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_4_b

          SUBROUTINE h5dwrite_real_5_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_5_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_5_b

          SUBROUTINE h5dwrite_real_6_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_6_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_6_b

          SUBROUTINE h5dwrite_real_7_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_real_7_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_real_7_b


          SUBROUTINE h5dwrite_double_scalar_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_scalar_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(IN) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_scalar_b

          SUBROUTINE h5dwrite_double_1_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_1_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_1_b

          SUBROUTINE h5dwrite_double_2_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_2_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_2_b

          SUBROUTINE h5dwrite_double_3_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_3_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_3_b

          SUBROUTINE h5dwrite_double_4_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_4_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_4_b

          SUBROUTINE h5dwrite_double_5_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_5_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_5_b

          SUBROUTINE h5dwrite_double_6_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_6_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_6_b

          SUBROUTINE h5dwrite_double_7_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_double_7_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dwrite_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DWRITE_C'::h5dwrite_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dwrite_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dwrite_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dwrite_double_7_b

!----------------------------------------------------------------------
! Name:		h5dread_f 
!
! Purpose: 	Reads raw data from the specified dataset into buf, 
!		converting from file datatype and dataspace to memory 
!		datatype and dataspace.
!
! Inputs:  
!		dset_id		- dataset identifier
!		mem_type_id	- memory type identifier
!		dims		- 1-dim array of size 7; dims(k) has the size 
!				- of k-th dimension of the buf array
! Outputs:  
!		buf		- buffer to read data in
!		hdferr:		- error code		
!				 	Success:  0
!				 	Failure: -1   
! Optional parameters:
!		mem_space_id	- memory dataspace identifier
!		file_space_id 	- file dataspace identifier
!		xfer_prp	- trasfer property list identifier	
!
! Programmer:	Elena Pourmal
!		August 12, 1999	
!
! Modifications: 	Explicit Fortran interfaces were added for 
!			called C functions (it is needed for Windows
!			port).  February 28, 2001 
!
!                       dims parameter was added to make code portable;
!                       n parameter was replaced with dims parameter in
!			the h5dwrite_reference_obj and h5dwrite_reference_dsetreg
!			functions.  April 2, 2001
!
! Comment:		This function is overloaded to read INTEGER,
!			REAL, DOUBLE PRECISION and CHARACTER buffers
!			up to 7 dimensions, and one dimensional buffers
!			of the TYPE(hobj_ref_t_f) and TYPE(hdset_reg_ref_t_f)
!			types.	
!----------------------------------------------------------------------
          SUBROUTINE h5dread_reference_obj_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_reference_obj_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            TYPE(hobj_ref_t_f), INTENT(INOUT) , &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j  

!            INTEGER, EXTERNAL :: h5dread_ref_obj_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_ref_obj_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_REF_OBJ_C'::h5dread_ref_obj_c  
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, DIMENSION(*) :: ref_buf
              END FUNCTION h5dread_ref_obj_c
            END INTERFACE 

            allocate(ref_buf(REF_OBJ_BUF_LEN*dims(1)), stat=hdferr)
            if (hdferr .NE. 0) then
                hdferr = -1
                return
            endif 

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_ref_obj_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims)
             do j = 1, dims(1)
              do i = 1, REF_OBJ_BUF_LEN  
                    buf(j)%ref(i) = ref_buf(REF_OBJ_BUF_LEN*(j-1) + i)
              enddo
             enddo  
             deallocate(ref_buf) 
          END SUBROUTINE h5dread_reference_obj_b

          SUBROUTINE h5dread_reference_dsetreg_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_reference_dsetreg_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            TYPE(hdset_reg_ref_t_f), INTENT(INOUT), & 
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j 

!            INTEGER, EXTERNAL :: h5dread_ref_reg_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_ref_reg_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_REF_REG_C'::h5dread_ref_reg_c  
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id   
              INTEGER(HID_T), INTENT(IN) :: mem_type_id 
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, DIMENSION(*) :: ref_buf
              END FUNCTION h5dread_ref_reg_c
            END INTERFACE 

            allocate(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
            if (hdferr .NE. 0) then
                hdferr = -1
                return
            endif

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims)
           
            do j = 1, dims(1)
             do i = 1, REF_REG_BUF_LEN  
                   buf(j)%ref(i) = ref_buf(REF_REG_BUF_LEN*(j-1) + i)
             enddo
            enddo   
            deallocate(ref_buf)
          END SUBROUTINE h5dread_reference_dsetreg_b


          SUBROUTINE h5dread_integer_scalar_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_scalar_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(OUT) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_integer_scalar_b

          SUBROUTINE h5dread_integer_1_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_1_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_integer_1_b

          SUBROUTINE h5dread_integer_2_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_2_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_2_b

          SUBROUTINE h5dread_integer_3_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_3_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_3_b

          SUBROUTINE h5dread_integer_4_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_4_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_4_b

          SUBROUTINE h5dread_integer_5_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_5_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_5_b

          SUBROUTINE h5dread_integer_6_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_6_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_6_b

          SUBROUTINE h5dread_integer_7_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_integer_7_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T)  :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)
           
          END SUBROUTINE h5dread_integer_7_b

          SUBROUTINE h5dread_char_scalar_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_scalar_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(OUT) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_scalar_b

          SUBROUTINE h5dread_char_1_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_1_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_1_b

          SUBROUTINE h5dread_char_2_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_2_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_2_b

          SUBROUTINE h5dread_char_3_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_3_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_3_b

          SUBROUTINE h5dread_char_4_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_4_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_4_b

          SUBROUTINE h5dread_char_5_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_5_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_5_b

          SUBROUTINE h5dread_char_6_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_6_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_6_b

          SUBROUTINE h5dread_char_7_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_char_7_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREADC_C'::h5dreadc_c
              !DEC$ ENDIF
              !DEC$ATTRIBUTES reference :: buf 
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dreadc_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dreadc_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_char_7_b

          SUBROUTINE h5dread_real_scalar_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_scalar_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(OUT) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_scalar_b

          SUBROUTINE h5dread_real_1_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_1_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_1_b

          SUBROUTINE h5dread_real_2_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_2_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_2_b

          SUBROUTINE h5dread_real_3_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_3_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), & 
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_3_b

          SUBROUTINE h5dread_real_4_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_4_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3), dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3), dims(4)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_4_b

          SUBROUTINE h5dread_real_5_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_5_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default  
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), & 
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_5_b

          SUBROUTINE h5dread_real_6_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_6_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_6_b

          SUBROUTINE h5dread_real_7_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_real_7_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_real_7_b

          SUBROUTINE h5dread_double_scalar_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_scalar_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(OUT) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_scalar_b

          SUBROUTINE h5dread_double_1_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_1_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_1_b

          SUBROUTINE h5dread_double_2_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_2_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(INOUT), & 
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_2_b

          SUBROUTINE h5dread_double_3_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_3_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_3_b

          SUBROUTINE h5dread_double_4_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_4_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_4_b

          SUBROUTINE h5dread_double_5_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_5_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_5_b

          SUBROUTINE h5dread_double_6_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_6_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default 
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_6_b

          SUBROUTINE h5dread_double_7_b(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_double_7_b
!DEC$endif

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            DOUBLE PRECISION, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id 
                                                ! Memory dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id 
                                                ! File dataspace identfier 
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp 
                                                ! Transfer property list identifier 
            
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default 
            INTEGER(HID_T) :: file_space_id_default 

!            INTEGER, EXTERNAL :: h5dread_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_c(dset_id, mem_type_id, &
                                          mem_space_id_default, & 
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DREAD_C'::h5dread_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dread_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp 
            if (present(mem_space_id))  mem_space_id_default = mem_space_id 
            if (present(file_space_id)) file_space_id_default = file_space_id 

            hdferr = h5dread_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)
           
          END SUBROUTINE h5dread_double_7_b
!
!END OVERLOADING
!
!DEC$ ENDIF
!
!
!----------------------------------------------------------------------
! Name:		h5dget_space_f 
!
! Purpose:	Returns an identifier for a copy of the dataspace for a 
!		dataset.   	
!
! Inputs:  
!		dataset_id	- dataset identifier
! Outputs:  
!		dataspace_id	- dataspace identifier
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
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------
          SUBROUTINE h5dget_space_f(dataset_id, dataspace_id, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dget_space_f
!DEC$endif
            IMPLICIT NONE 
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HID_T), INTENT(OUT) :: dataspace_id   ! Dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr                ! Error code 

!            INTEGER, EXTERNAL :: h5dget_space_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dget_space_c(dataset_id, dataspace_id)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DGET_SPACE_C'::h5dget_space_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HID_T), INTENT(OUT) :: dataspace_id
              END FUNCTION h5dget_space_c
            END INTERFACE

            hdferr = h5dget_space_c(dataset_id, dataspace_id)
          END SUBROUTINE h5dget_space_f  

!----------------------------------------------------------------------
! Name:		h5dget_type_f 
!
! Purpose:	Returns an identifier for a copy of the datatype for a 
!		dataset.   	
!
! Inputs:  
!		dataset_id	- dataset identifier
! Outputs:  
!		datatype_id	- dataspace identifier
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
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------

          SUBROUTINE h5dget_type_f(dataset_id, datatype_id, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dget_type_f
!DEC$endif
            IMPLICIT NONE 
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HID_T), INTENT(OUT) :: datatype_id    ! Datatype identifier
            INTEGER, INTENT(OUT) :: hdferr                ! Error code 
!            INTEGER, EXTERNAL :: h5dget_type_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dget_type_c (dataset_id, datatype_id)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DGET_TYPE_C'::h5dget_type_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HID_T), INTENT(OUT) :: datatype_id
              END FUNCTION h5dget_type_c
            END INTERFACE

            hdferr = h5dget_type_c (dataset_id, datatype_id)
          END SUBROUTINE h5dget_type_f  

!----------------------------------------------------------------------
! Name:		h5dextend_f 
!
! Purpose:	Extends a dataset with unlimited dimension.	
!
! Inputs:  
!		dataset_id	- dataset identifier
!		size		- array containing the new magnitude of 
!				  each dimension
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
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5dextend_f(dataset_id, size, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dextend_f
!DEC$endif
            IMPLICIT NONE 
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN)  :: size
                                                          ! Array containing 
                                                          ! dimensions' sizes 
            INTEGER, INTENT(OUT) :: hdferr                ! Error code 

!            INTEGER, EXTERNAL ::  h5dextend_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dextend_c(dataset_id, size)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DEXTEND_C'::h5dextend_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN)  :: size
              END FUNCTION h5dextend_c
            END INTERFACE

            hdferr = h5dextend_c(dataset_id, size)
          END SUBROUTINE h5dextend_f  


!----------------------------------------------------------------------
! Name:		h5dget_create_plist_f 
!
! Purpose:	Returns an identifier for a copy of the dataset creation 
!		property list for a dataset. 	
!
! Inputs:  
!		dataset_id	- dataset identifier
! Outputs:  
!		plist_id	- creation property list identifier
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
!			port).  February 28, 2001 
!
! Comment:		
!----------------------------------------------------------------------


          SUBROUTINE h5dget_create_plist_f(dataset_id, plist_id, hdferr) 
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dget_create_plist_f
!DEC$endif
            IMPLICIT NONE 
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HID_T), INTENT(OUT) :: plist_id    ! Dataset creation
                                                  ! property list identifier
            INTEGER, INTENT(OUT) :: hdferr                ! Error code 

!            INTEGER, EXTERNAL :: h5dget_create_plist_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dget_create_plist_c(dataset_id, plist_id)
              USE H5GLOBAL
              !DEC$ IF DEFINED(HDF5F90_WINDOWS)
              !MS$ATTRIBUTES C,reference,alias:'_H5DGET_CREATE_PLIST_C'::h5dget_create_plist_c
              !DEC$ ENDIF
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HID_T), INTENT(OUT) :: plist_id
              END FUNCTION h5dget_create_plist_c
            END INTERFACE

            hdferr = h5dget_create_plist_c(dataset_id, plist_id)
          END SUBROUTINE h5dget_create_plist_f  

      END MODULE H5D
