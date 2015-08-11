!****h* ROBODoc/H5R (F90)
!
! NAME
!  MODULE H5R_PROVISIONAL
!
! FILE
!  fortran/src/H5Rff_F90.f90
!
! PURPOSE
!  This file contains Fortran 90 interfaces for H5R functions. It contains
!  the same functions as H5Rff_F03.f90 but excludes the Fortran 2003 functions
!  and the interface listings. This file will be compiled instead of H5Rff_F03.f90
!  if Fortran 2003 functions are not enabled.
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  Copyright by The HDF Group.                                                 *
!  Copyright by the Board of Trustees of the University of Illinois.           *
!  All rights reserved.                                                        *
!                                                                              *
!  This file is part of HDF5.  The full HDF5 copyright notice, including       *
!  terms governing use, modification, and redistribution, is contained in      *
!  the files COPYING and Copyright.html.  COPYING can be found at the root     *
!  of the source code distribution tree; Copyright.html can be found at the    *
!  root level of an installed copy of the electronic HDF5 document set and     *
!  is linked from the top-level documents page.  It can also be found at       *
!  http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have            *
!  access to either file, you may request a copy from help@hdfgroup.org.       *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES
!                         *** IMPORTANT ***
!  If you add a new H5R function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!*****

MODULE H5R_PROVISIONAL
  USE H5GLOBAL

!  If you change the value of these parameters, do not forget to change corresponding
!  values in the H5f90.h file.
!  INTEGER, PARAMETER :: REF_OBJ_BUF_LEN = 2
!  INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3
!
!  TYPE hobj_ref_t_f
!  INTEGER ref(REF_OBJ_BUF_LEN)
!  END TYPE
!
!  TYPE hdset_reg_ref_t_f
!  INTEGER ref(REF_REG_BUF_LEN)
!  END TYPE
!
  INTERFACE h5rcreate_f

     MODULE PROCEDURE h5rcreate_object_f
     MODULE PROCEDURE h5rcreate_region_f

  END INTERFACE

  INTERFACE h5rdereference_f

     MODULE PROCEDURE h5rdereference_object_f
     MODULE PROCEDURE h5rdereference_region_f

  END INTERFACE

  INTERFACE h5rget_name_f

     MODULE PROCEDURE h5rget_name_object_f
     MODULE PROCEDURE h5rget_name_region_f

  END INTERFACE

  INTERFACE h5rget_region_f

     MODULE PROCEDURE h5rget_region_region_f

  END INTERFACE


CONTAINS


!****s* H5R/h5rget_region_region_f
!
! NAME
!  h5rget_region_region_f
!
! PURPOSE
!  Retrieves a dataspace with the specified region selected
!
! INPUTS
!  dset_id 	 - identifier of the dataset containing
!                  reference to the regions
!  ref 	         - reference to open
! OUTPUTS
!  space_id 	 - dataspace identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY 	
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! NOTES
!  This is a module procedure for the h5rget_region_f subroutine.
!
! SOURCE
  SUBROUTINE h5rget_region_region_f(dset_id, ref, space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id      ! Dataset identifier
    TYPE(hdset_reg_ref_t_f), INTENT(IN) :: ref ! Dataset region reference
    INTEGER(HID_T), INTENT(OUT) :: space_id    ! Space identifier
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
!*****
    INTEGER :: ref_f(REF_REG_BUF_LEN)          ! Local buffer to pass reference

    INTERFACE
       INTEGER FUNCTION h5rget_region_region_c(dset_id, ref_f, space_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RGET_REGION_REGION_C':: h5rget_region_region_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         !              INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3
         INTEGER :: ref_f(REF_REG_BUF_LEN)
         INTEGER(HID_T), INTENT(OUT) :: space_id
       END FUNCTION h5rget_region_region_c
    END INTERFACE

    ref_f = ref%ref
    hdferr = h5rget_region_region_c(dset_id, ref_f, space_id )

  END SUBROUTINE h5rget_region_region_f



!****s* H5R (F90)/h5rcreate_object_f
!
! NAME
!  h5rcreate_object_f
!
! PURPOSE
!  Creates reference to the object
!
! INPUTS
!  loc_id 	 - location identifier
!  name 	 - name of the object at the specified location
! OUTPUTS
!  ref 	         - reference to the specified object
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! NOTES
!  This is a module procedure for the h5rcreate_f subroutine.
!
! SOURCE
  SUBROUTINE h5rcreate_object_f(loc_id, name, ref, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! Location identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the object at location specified
                                           ! by loc_id identifier
    TYPE(hobj_ref_t_f), INTENT(OUT) :: ref ! Object reference
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER :: namelen                     ! Name length
    INTEGER(HADDR_T) :: ref_f              ! Local buffer to pass reference

    INTERFACE
       INTEGER FUNCTION h5rcreate_object_c(ref_f, loc_id, name, namelen)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RCREATE_OBJECT_C':: h5rcreate_object_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HADDR_T) :: ref_f
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER :: namelen
       END FUNCTION h5rcreate_object_c
    END INTERFACE

    namelen = LEN(name)
    ref_f = 0
    hdferr = h5rcreate_object_c(ref_f, loc_id, name, namelen )
    ref%ref = ref_f

  END SUBROUTINE h5rcreate_object_f

!****s* H5R (F90)/h5rcreate_region_f
!
! NAME
!  h5rcreate_region_f
!
! PURPOSE
!  Creates reference to the dataset region
!
! INPUTS
!  loc_id 	 - location identifier
!  name 	 - name of the dataset at the specified location
!  space_id 	 - dataspace identifier that describes selected region
! OUTPUTS
!  ref 	         - reference to the dataset region
!  hdferr:	 - error code
!                   Success:  0
!                   Failure: -1
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! NOTES
!  This is a module procedure for the h5rcreate_f subroutine.
!
! SOURCE
  SUBROUTINE h5rcreate_region_f(loc_id, name, space_id, ref, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! Location identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the dataset at location specified
                                           ! by loc_id identifier
    INTEGER(HID_T), INTENT(IN) :: space_id ! Dataset's dataspace identifier
    TYPE(hdset_reg_ref_t_f), INTENT(OUT) :: ref ! Dataset region reference
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER :: namelen                     ! Name length
    INTEGER :: ref_f(REF_REG_BUF_LEN)      ! Local buffer to pass reference

    INTERFACE
       INTEGER FUNCTION h5rcreate_region_c(ref_f, loc_id, name, namelen, space_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RCREATE_REGION_C':: h5rcreate_region_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         !              INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3
         INTEGER :: ref_f(REF_REG_BUF_LEN)
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(HID_T), INTENT(IN) :: space_id
       END FUNCTION h5rcreate_region_c
    END INTERFACE

    namelen = LEN(name)
    ref_f = 0
    hdferr = h5rcreate_region_c(ref_f, loc_id, name, namelen, space_id )
    ref%ref = ref_f

  END SUBROUTINE h5rcreate_region_f
!****s* H5R (F90)/h5rdereference_object_f
!
! NAME
!  h5rdereference_object_f
!
! PURPOSE
!  Opens the HDF5 object referenced
!
! INPUTS
!  dset_id  - identifier of the dataset containing reference
!  ref 	    - reference to open
! OUTPUTS
!  obj_id   - object_identifier
!  hdferr   - error code
!              Success:  0
!              Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! NOTES
!  This is a module procedure for the h5rdereference_f subroutine.
!
! SOURCE
  SUBROUTINE h5rdereference_object_f(dset_id, ref, obj_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    TYPE(hobj_ref_t_f), INTENT(IN) :: ref   ! Object reference
    INTEGER(HID_T), INTENT(OUT) :: obj_id   ! Object identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    INTEGER(HADDR_T) :: ref_f          ! Local buffer to pass reference

    INTERFACE
       INTEGER FUNCTION h5rdereference_object_c(dset_id, ref_f, obj_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RDEREFERENCE_OBJECT_C':: h5rdereference_object_c
         !DEC$ENDIF
         !              INTEGER, PARAMETER :: REF_OBJ_BUF_LEN = 2
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HADDR_T) :: ref_f
         INTEGER(HID_T), INTENT(OUT) :: obj_id
       END FUNCTION h5rdereference_object_c
    END INTERFACE

    ref_f = ref%ref
    hdferr = h5rdereference_object_c(dset_id, ref_f, obj_id )

  END SUBROUTINE h5rdereference_object_f
!****s* H5R (F90)/h5rdereference_region_f
!
! NAME
!  h5rdereference_region_f
!
! PURPOSE
!  Opens the dataset region
!
! INPUTS
!  dset_id 	 - identifier of the dataset containing
!                  reference to teh regions
!  ref 	         - reference to open
! OUTPUTS
!  obj_id 	 - dataspace identifier
!  hdferr 	 - error code
!                   Success:  0
!                   Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
!
! NOTES
!  This is a module procedure for the h5rdereference_f subroutine.
!
! SOURCE
  SUBROUTINE h5rdereference_region_f(dset_id, ref, obj_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id      ! Dataset identifier
    TYPE(hdset_reg_ref_t_f), INTENT(IN) :: ref ! Object reference
    INTEGER(HID_T), INTENT(OUT) :: obj_id   ! Dataspace identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    INTEGER :: ref_f(REF_REG_BUF_LEN)       ! Local buffer to pass reference

    INTERFACE
       INTEGER FUNCTION h5rdereference_region_c(dset_id, ref_f, obj_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RDEREFERENCE_REGION_C':: h5rdereference_region_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
!  INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3
         INTEGER :: ref_f(REF_REG_BUF_LEN)
         INTEGER(HID_T), INTENT(OUT) :: obj_id
       END FUNCTION h5rdereference_region_c
    END INTERFACE

    ref_f = ref%ref
    hdferr = h5rdereference_region_c(dset_id, ref_f, obj_id )

  END SUBROUTINE h5rdereference_region_f
!****s* H5R (F90)/h5rget_name_object_f
!
! NAME
!  h5rget_name_object_f
!
! PURPOSE
!  Retrieves a name of a referenced object.
!
! INPUTS
!  loc_id  - Identifier for the file containing the reference or for any object in that file.
!  ref 	   - An object or dataset region reference.
!
! OUTPUTS
!  name    - A name associated with the referenced object or dataset region.
!
!  hdferr  - error code
!             Success:  0
!             Failure: -1
!
! OPTIONAL PARAMETERS
!  size    - The size of the name buffer, returning 0 (zero) if 
!            no name is associated with the identifier
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 28, 2008
!
! SOURCES
  SUBROUTINE h5rget_name_object_f(loc_id,  ref, name, hdferr, size)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! Identifier for the file containing the reference or 
                                           ! for any object in that file.
                                           ! or for the group that dataset is in.
    TYPE(hobj_ref_t_f), INTENT(IN) :: ref  ! Object reference
    INTEGER(SIZE_T), OPTIONAL, INTENT(OUT) :: size   ! The size of the name buffer,
                                           ! returning 0 (zero) if no name is associated with the identifier
    CHARACTER(LEN=*), INTENT(OUT) :: name  ! A name associated with the referenced object or dataset region.
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER(HADDR_T) :: ref_f              ! Local buffer to pass reference

    INTEGER(SIZE_T) :: size_default
    INTEGER(SIZE_T) :: name_len

    INTERFACE
       INTEGER FUNCTION h5rget_name_object_c(loc_id, ref_f, name, name_len, size_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RGET_NAME_OBJECT_C':: h5rget_name_object_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(SIZE_T)  :: size_default
         CHARACTER(LEN=*), INTENT(OUT) :: name
         INTEGER(HADDR_T) :: ref_f

         INTEGER(SIZE_T) :: name_len
       END FUNCTION h5rget_name_object_c
    END INTERFACE

    name_len=LEN(name)

    ref_f = ref%ref
    hdferr = h5rget_name_object_c(loc_id, ref_f, name, name_len, size_default)

    IF(PRESENT(size)) size = size_default

  END SUBROUTINE h5rget_name_object_f

!****s* H5R (F90)/h5rget_name_region_f
!
! NAME
!  h5rget_name_region_f
!
! PURPOSE
!  Retrieves a name of a dataset region.
!
! INPUTS
!  loc_id 	 - Identifier for the file containing the reference or 
!                  for any object in that file.
!  ref 	         - An object or dataset region reference.
!
! OUTPUTS
!  name 	 - A name associated with the referenced object or dataset region.
!  hdferr 	 - error code
!                   Success:  0
!                   Failure: -1
!
! OPTIONAL PARAMETERS
!  size 	 - The size of the name buffer,  returning 0 (zero) if no 
!                  name is associated  with the identifier
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 28, 2008
!
! SOURCE
  SUBROUTINE h5rget_name_region_f(loc_id, ref, name, hdferr, size)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    TYPE(hdset_reg_ref_t_f), INTENT(IN) :: ref
    INTEGER(SIZE_T), OPTIONAL, INTENT(OUT) :: size
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
!*****
    INTEGER :: ref_f(REF_REG_BUF_LEN)      ! Local buffer to pass reference
    INTEGER(SIZE_T) :: size_default
    INTEGER(SIZE_T) :: name_len

    INTERFACE
       INTEGER FUNCTION h5rget_name_region_c(loc_id, ref_f, name, name_len, size_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RGET_NAME_REGION_C':: h5rget_name_region_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(SIZE_T)  :: size_default
         CHARACTER(LEN=*), INTENT(OUT) :: name
         INTEGER :: ref_f(REF_REG_BUF_LEN)

         INTEGER(SIZE_T) :: name_len
       END FUNCTION h5rget_name_region_c
    END INTERFACE

    name_len=LEN(name)

    ref_f = ref%ref
    hdferr = h5rget_name_region_c(loc_id, ref_f, name, name_len, size_default)

    IF(PRESENT(size)) size = size_default

  END SUBROUTINE h5rget_name_region_f

END MODULE H5R_PROVISIONAL
