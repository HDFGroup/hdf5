!****h* ROBODoc/H5R
!
! NAME
!  MODULE H5R
!
! FILE
!  fortran/src/H5Rff.F90
!
! PURPOSE
!  This file contains Fortran interfaces for H5R functions.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES
!
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5R function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5R
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_CHAR, C_SIGNED_CHAR
  USE H5GLOBAL

  ! If you change the value of these parameters, do not forget to change corresponding
  ! values in the H5f90.h file.
  !        INTEGER, PARAMETER :: REF_OBJ_BUF_LEN = 2
  !        INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3
  !
  !        TYPE hobj_ref_t_f
  !             INTEGER ref(REF_OBJ_BUF_LEN)
  !        END TYPE
  !
  !        TYPE hdset_reg_ref_t_f
  !             INTEGER ref(REF_REG_BUF_LEN)
  !        END TYPE
  !

  PRIVATE h5rget_object_type_obj_f
  PRIVATE h5rget_region_region_f, h5rget_region_ptr_f
  PRIVATE h5rcreate_object_f, h5rcreate_region_f, h5rcreate_ptr_f
  PRIVATE h5rdereference_object_f, h5rdereference_region_f, h5rdereference_ptr_f
  PRIVATE h5rget_name_object_f, h5rget_name_region_f, h5rget_name_ptr_f
  
  INTERFACE h5rget_object_type_f

     MODULE PROCEDURE h5rget_object_type_obj_f

  END INTERFACE

  TYPE :: hdset_reg_ref_t_f03
     INTEGER(C_SIGNED_CHAR), DIMENSION(1:H5R_DSET_REG_REF_BUF_SIZE_F) :: ref
  END TYPE hdset_reg_ref_t_f03

  INTERFACE h5rget_region_f

     MODULE PROCEDURE h5rget_region_region_f ! obsolete
     MODULE PROCEDURE h5rget_region_ptr_f  ! F2003

  END INTERFACE


  INTERFACE h5rcreate_f

     MODULE PROCEDURE h5rcreate_object_f ! obsolete
     MODULE PROCEDURE h5rcreate_region_f ! obsolete
     MODULE PROCEDURE h5rcreate_ptr_f  ! F2003

  END INTERFACE

  INTERFACE h5rdereference_f

     MODULE PROCEDURE h5rdereference_object_f ! obsolete
     MODULE PROCEDURE h5rdereference_region_f ! obsolete
     MODULE PROCEDURE h5rdereference_ptr_f ! F2003

  END INTERFACE

  INTERFACE h5rget_name_f

     MODULE PROCEDURE h5rget_name_object_f ! obsolete
     MODULE PROCEDURE h5rget_name_region_f ! obsolete
     MODULE PROCEDURE h5rget_name_ptr_f ! F2003

  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5rget_name_ptr_c(loc_id, ref_type, ref, name, name_len, size_default) &
          BIND(C, NAME='h5rget_name_ptr_c')
       IMPORT :: c_char, c_ptr
       IMPORT :: HID_T, SIZE_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: loc_id
       INTEGER, INTENT(IN) :: ref_type
       TYPE(C_PTR), INTENT(IN), VALUE :: ref
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
       INTEGER(SIZE_T) :: name_len
       INTEGER(SIZE_T) :: size_default
     END FUNCTION h5rget_name_ptr_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5rdereference_ptr_c(obj_id, ref_type, ref, ref_obj_id) &
          BIND(C, NAME='h5rdereference_ptr_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: obj_id
       INTEGER, INTENT(IN) :: ref_type
       TYPE(C_PTR), INTENT(IN), VALUE :: ref
       INTEGER(HID_T), INTENT(OUT) :: ref_obj_id
     END FUNCTION h5rdereference_ptr_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5rcreate_ptr_c(ref, loc_id, name, namelen, ref_type, space_id) &
          BIND(C, NAME='h5rcreate_ptr_c')
       IMPORT :: c_ptr, c_char
       IMPORT :: HID_T
       IMPLICIT NONE
       TYPE(C_PTR), VALUE :: ref
       INTEGER(HID_T), INTENT(IN) :: loc_id
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
       INTEGER :: namelen
       INTEGER, INTENT(IN) :: ref_type
       INTEGER(HID_T), INTENT(IN) :: space_id
     END FUNCTION h5rcreate_ptr_c
  END INTERFACE

  INTERFACE
     INTEGER FUNCTION h5rget_region_ptr_c(dset_id, ref, space_id) &
          BIND(C, NAME='h5rget_region_ptr_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: dset_id
       TYPE(C_PTR), VALUE :: ref
       INTEGER(HID_T), INTENT(OUT) :: space_id
     END FUNCTION h5rget_region_ptr_c
  END INTERFACE


CONTAINS

!****s* H5R/h5rget_object_type_obj_f
!
! NAME
!  h5rget_object_type_obj_f
!
! PURPOSE
!  Retrieves the type of object that an object reference points to.
!
! INPUTS
!  dset_id      - identifier of the dataset containing
!                  reference to the objects
!  ref              - reference to open
! OUTPUTS
!  obj_type      - object_type, possible values:
!                    H5G_UNKNOWN_F
!                    H5G_GROUP_F
!                    H5G_DATASET_F
!                    H5G_TYPE_F
!  hdferr      - Returns 0 if successful and -1 if fails
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
!  This is a module procedure for the h5rget_object_type_f
!  subroutine.
! SOURCE
  SUBROUTINE h5rget_object_type_obj_f(dset_id, ref, obj_type, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    TYPE(hobj_ref_t_f), INTENT(IN) :: ref   ! Object reference
    INTEGER, INTENT(OUT) :: obj_type     ! Object type
                                         !  H5G_UNKNOWN_F
                                         !  H5G_GROUP_F
                                         !  H5G_DATASET_F
                                         !  H5G_TYPE_F
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
!*****
    INTEGER(HADDR_T) :: ref_f          ! Local buffer to pass reference

    INTERFACE
       INTEGER FUNCTION h5rget_object_type_obj_c(dset_id, ref_f, obj_type) BIND(C, NAME='h5rget_object_type_obj_c')
         IMPORT :: HID_T, HADDR_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HADDR_T) :: ref_f
         INTEGER, INTENT(OUT) :: obj_type
       END FUNCTION h5rget_object_type_obj_c
    END INTERFACE

    ref_f = ref%ref
    hdferr = h5rget_object_type_obj_c(dset_id, ref_f, obj_type )

  END SUBROUTINE h5rget_object_type_obj_f


!****s* H5R/h5rget_region_region_f
!
! NAME
!  h5rget_region_region_f
!
! PURPOSE
!  Retrieves a dataspace with the specified region selected
!
! INPUTS
!  dset_id      - identifier of the dataset containing
!                  reference to the regions
!  ref              - reference to open
! OUTPUTS
!  space_id      - dataspace identifier
!  hdferr      - Returns 0 if successful and -1 if fails
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
       INTEGER FUNCTION h5rget_region_region_c(dset_id, ref_f, space_id) BIND(C, NAME='h5rget_region_region_c')
         IMPORT :: HID_T, REF_REG_BUF_LEN
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER :: ref_f(REF_REG_BUF_LEN)
         INTEGER(HID_T), INTENT(OUT) :: space_id
       END FUNCTION h5rget_region_region_c
    END INTERFACE

    ref_f = ref%ref
    hdferr = h5rget_region_region_c(dset_id, ref_f, space_id )

  END SUBROUTINE h5rget_region_region_f

!****s* H5R/h5rget_region_ptr_f
!
! NAME
!  h5rget_region_ptr_f
!
! PURPOSE
!  Retrieves a dataspace with the specified region
!  selected using pointer
!
! INPUTS
!  dset_id      - identifier of the dataset containing
!                  reference to the regions
!  ref              - reference to open
! OUTPUTS
!  space_id      - dataspace identifier
!  hdferr      - Returns 0 if successful and -1 if fails
! AUTHOR
!  M. Scot Breitenfeld
!  August 4, 2012
!
! NOTES
!  This is a module procedure for the h5rget_region_f subroutine.
!
! SOURCE
  SUBROUTINE h5rget_region_ptr_f(dset_id, ref, space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id      ! Dataset identifier
    TYPE(C_PTR), INTENT(IN) :: ref ! Dataset region reference
    INTEGER(HID_T), INTENT(OUT) :: space_id    ! Space identifier
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
!*****

    hdferr = h5rget_region_ptr_c(dset_id, ref, space_id )

  END SUBROUTINE h5rget_region_ptr_f


!****s* H5R (F03)/h5rcreate_object_f
!
! NAME
!  h5rcreate_object_f
!
! PURPOSE
!  Creates reference to the object
!
! Inputs:
!  loc_id    - location identifier
!  name      - name of the object at the specified location
! Outputs:
!  ref          - reference to the specified object
!  hdferr    - returns 0 if successful and -1 if fails
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
! Signature:
  SUBROUTINE h5rcreate_object_f(loc_id, name, ref, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! Location identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the object at location specified
                                           ! by loc_id identifier
    TYPE(hobj_ref_t_f), INTENT(INOUT), TARGET :: ref   ! Object reference
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER :: namelen                     ! Name length
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(ref)

    namelen = LEN(name)

    hdferr = h5rcreate_ptr_c(f_ptr, loc_id, name, namelen, INT(0), INT(-1,HID_T))

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
!  loc_id      - location identifier
!  name      - name of the dataset at the specified location
!  space_id      - dataspace identifier that describes selected region
! OUTPUTS
!  ref              - reference to the dataset region
!  hdferr        - returns 0 if successful and -1 if fails
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
       INTEGER FUNCTION h5rcreate_region_c(ref_f, loc_id, name, namelen, space_id) BIND(C,NAME='h5rcreate_region_c')
         IMPORT :: HID_T, REF_REG_BUF_LEN
         IMPORT :: C_CHAR
         IMPLICIT NONE
         INTEGER :: ref_f(REF_REG_BUF_LEN)
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(HID_T), INTENT(IN) :: space_id
       END FUNCTION h5rcreate_region_c
    END INTERFACE

    namelen = LEN(name)
    ref_f = 0
    hdferr = h5rcreate_region_c(ref_f, loc_id, name, namelen, space_id )
    ref%ref = ref_f

  END SUBROUTINE h5rcreate_region_f

!****s* H5R (F03)/h5rcreate_ptr_f
!
! NAME
!  h5rcreate_ptr_f
!
! PURPOSE
!  Creates a reference.
!
! Inputs:
!  loc_id     - location identifier
!  name          - name of the dataset at the specified location
!  ref_type   - type of reference:
!                H5R_OBJECT
!                H5T_STD_REF_DSETREG
! Outputs:
!  ref          - reference created by the function call.
!  hdferr     - returns 0 if successful and -1 if fails.
! OPTIONAL
!  space_id   - dataspace identifier that describes selected region
!
! AUTHOR
!  M. Scot Breitenfeld
!  June 20, 2008
!
! NOTES
!  This is a module procedure for the h5rcreate_f
!  subroutine where the output is a pointer.
!
! Signature:
  SUBROUTINE h5rcreate_ptr_f(loc_id, name, ref_type, ref, hdferr, space_id)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id     ! Location identifier
    CHARACTER(LEN=*), INTENT(IN) :: name     ! Name of the dataset at location specified
                                             ! by loc_id identifier
    INTEGER, INTENT(IN) :: ref_type          ! type of reference
    TYPE(C_PTR), INTENT(INOUT) :: ref        ! Reference created by the function call
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: space_id ! Dataset's dataspace identifier
!*****
    INTEGER :: namelen                       ! Name length
    INTEGER(HID_T) :: space_id_c

    namelen = LEN(name)
    space_id_c = -1
    IF(PRESENT(space_id)) space_id_c =  space_id
    hdferr = h5rcreate_ptr_c(ref, loc_id, name, namelen, ref_type, space_id_c)

  END SUBROUTINE h5rcreate_ptr_f
!****s* H5R (F03)/h5rdereference_object_f
!
! NAME
!  h5rdereference_object_f
!
! PURPOSE
!  Opens the HDF5 object referenced
!
! Inputs:
!  dset_id  - identifier of the dataset containing
!             reference
!  ref         - reference to open
! Outputs:
!  obj_id   - object_identifier
!  hdferr   - returns 0 if successful and -1 if fails
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
! Signature:
  SUBROUTINE h5rdereference_object_f(obj_id, ref, ref_obj_id, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id          ! Dataset identifier
    TYPE(hobj_ref_t_f), INTENT(IN), TARGET :: ref ! Object reference
    INTEGER(HID_T), INTENT(OUT) :: ref_obj_id     ! Object identifier
    INTEGER, INTENT(OUT) :: hdferr                ! Error code
!*****
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(ref)
    hdferr = h5rdereference_ptr_c(obj_id, 0, f_ptr, ref_obj_id)

  END SUBROUTINE h5rdereference_object_f
!****s* H5R (F03)/h5rdereference_region_f
!
! NAME
!  h5rdereference_region_f
!
! PURPOSE
!  Opens the dataset region
!
! Inputs:
!  dset_id  - identifier of the dataset containing
!             reference to the regions
!  ref         - reference to open
! Outputs:
!  obj_id   - dataspace identifier
!  hdferr   - returns 0 if successful and -1 if fails
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
! Signature:
  SUBROUTINE h5rdereference_region_f(obj_id, ref, ref_obj_id, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id   ! Dataset identifier
    TYPE(hdset_reg_ref_t_f), INTENT(IN), TARGET :: ref   ! Object reference
    INTEGER(HID_T), INTENT(OUT) :: ref_obj_id  ! Dataspace identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(ref)
    hdferr = h5rdereference_ptr_c(obj_id, 1, f_ptr, ref_obj_id)

  END SUBROUTINE h5rdereference_region_f

!****s* H5R (F03)/h5rdereference_ptr_f
!
! NAME
!  h5rdereference_ptr_f
!
! PURPOSE
!  Opens the HDF5 object referenced.
!
! Inputs:
!  obj_id     - valid identifier for the file containing the
!               referenced object or any object in that file.
!  ref_type   - the reference type of ref.
!  ref        - Reference to open.
! Outputs:
!  ref_obj_id - identifier of referenced object
!  hdferr     - returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  June 20, 2008
!
! NOTES
!  This is a module procedure for the h5rdereference_f
!  subroutine using pointers.
!
! Signature:
  SUBROUTINE h5rdereference_ptr_f(obj_id, ref_type, ref, ref_obj_id, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id  ! Valid identifier for the file containing the
                                          !  referenced object or any object in that file.
    INTEGER, INTENT(IN) :: ref_type       ! The reference type of ref.
    TYPE(C_PTR), INTENT(IN) :: ref        ! Object reference
    INTEGER(HID_T), INTENT(OUT) :: ref_obj_id
                                          ! Identifier of referenced object
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    hdferr = h5rdereference_ptr_c(obj_id, ref_type, ref, ref_obj_id)

  END SUBROUTINE h5rdereference_ptr_f
!
!****s* H5R (F03)/h5rget_name_object_f
!
! NAME
!  h5rget_name_object_f
!
! PURPOSE
!  Retrieves a name of a referenced object.
!
! Inputs:
!  loc_id    - Identifier for the file containing the reference or for any object in that file.
!  ref          - An object or dataset region reference.
!
! Outputs:
!  name      - A name associated with the referenced object or dataset region.
!  hdferr    - Returns 0 if successful and -1 if fails.
!
! Optional parameters:
!  size     - The size of the name buffer, returning 0 (zero) if no name is associated
!             with the identifier.
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 28, 2008
!
! Signature:
  SUBROUTINE h5rget_name_object_f(loc_id,  ref, name, hdferr, size)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    TYPE(hobj_ref_t_f), INTENT(IN), TARGET :: ref
    INTEGER(SIZE_T), OPTIONAL, INTENT(OUT) :: size
    CHARACTER(LEN=*), INTENT(INOUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
!*****

    INTEGER(SIZE_T) :: size_default
    INTEGER(SIZE_T) :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(ref)

    name_len=LEN(name)

    hdferr = h5rget_name_ptr_c(loc_id, 0, f_ptr, name, name_len, size_default)

    IF(PRESENT(size)) size = size_default

  END SUBROUTINE h5rget_name_object_f
!****s* H5R (F03)/h5rget_name_region_f
!
! NAME
!  h5rget_name_region_f
!
! PURPOSE
!  Retrieves a name of a dataset region.
!
! Inputs:
!  loc_id  - Identifier for the file containing the reference or for any object in that file.
!  ref        - An object or dataset region reference.
!
! Outputs:
!  name    - A name associated with the referenced object or dataset region.
!  hdferr  - Returns 0 if successful and -1 if fails.
!
! Optional parameters:
!  size    - The size of the name buffer, returning 0 (zero) if no name is associated with the identifier
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 28, 2008
!
! Signature:
  SUBROUTINE h5rget_name_region_f(loc_id, ref, name, hdferr, size)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    TYPE(hdset_reg_ref_t_f), INTENT(IN), TARGET :: ref
    INTEGER(SIZE_T), OPTIONAL, INTENT(OUT) :: size
    CHARACTER(LEN=*), INTENT(INOUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
!*****
    INTEGER(SIZE_T) :: size_default
    INTEGER(SIZE_T) :: name_len
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(ref)

    name_len=LEN(name)

    hdferr = h5rget_name_ptr_c(loc_id, 1, f_ptr, name, name_len, size_default)

    IF(PRESENT(size)) size = size_default

  END SUBROUTINE h5rget_name_region_f

  !****s* H5R (F03)/h5rget_name_ptr_f
  !
  ! NAME
  !  h5rget_name_ptr_f
  !
  ! PURPOSE
  !  Retrieves a name of a referenced object.
  !
  ! Inputs:
  !  loc_id   - Identifier for the file containing the reference or for any object in that file.
  !  ref_type - Type of reference.
  !  ref      - An object or dataset region reference.
  !
  ! Outputs:
  !  name     - A name associated with the referenced object or dataset ptr.
  !  hdferr   - Returns 0 if successful and -1 if fails.
  !
  ! Optional parameters:
  !   size    - The size of the name buffer, returning 0 (zero) if no name is associated
  !             with the identifier
  !
  ! AUTHOR
  !  M. Scot Breitenfeld
  !  March 28, 2008
  !
  ! Signature:
  SUBROUTINE h5rget_name_ptr_f(loc_id, ref_type, ref, name, hdferr, size)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    INTEGER, INTENT(IN) :: ref_type
    TYPE(C_PTR), INTENT(IN) :: ref
    CHARACTER(LEN=*), INTENT(INOUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(SIZE_T), OPTIONAL, INTENT(OUT) :: size
!*****
    INTEGER(SIZE_T) :: size_default
    INTEGER(SIZE_T) :: name_len

    name_len=LEN(name)

    hdferr = h5rget_name_ptr_c(loc_id, ref_type, ref, name, name_len, size_default)

    IF(PRESENT(size)) size = size_default

  END SUBROUTINE h5rget_name_ptr_f

  !****s* H5R (F03)/h5rget_obj_type_f
  !
  ! NAME
  !  h5rget_obj_type_f
  !
  ! PURPOSE
  !  Retrieves the type of object that an object reference points to.
  !
  ! Inputs:
  !  loc_id   - Identifier for the dataset containing the reference or
  !             for the group that dataset is in.
  !  ref_type - Type of reference to query.
  !  ref      - Reference to query.
  !
  ! Outputs:
  !  obj_type - Type of referenced object.
  !               H5G_UNKNOWN_F
  !               H5G_GROUP_F
  !               H5G_DATASET_F
  !               H5G_TYPE_F
  !
  !  hdferr   - Returns 0 if successful and -1 if fails.
  !
  ! AUTHOR
  !  M. Scot Breitenfeld
  !  Decemeber 17, 2008
  !
  ! Signature:
  SUBROUTINE h5rget_obj_type_f(loc_id, ref_type, ref, obj_type, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    INTEGER, INTENT(IN) :: ref_type
    TYPE(C_PTR), INTENT(IN) :: ref
    INTEGER, INTENT(OUT) :: obj_type
    INTEGER, INTENT(OUT) :: hdferr
    !*****

    INTERFACE
       INTEGER FUNCTION h5rget_obj_type_c(loc_id, ref_type, ref, obj_type) &
            BIND(C, NAME='h5rget_obj_type_c')
         IMPORT :: C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER, INTENT(IN) :: ref_type
         TYPE(C_PTR), VALUE :: ref
         INTEGER :: obj_type
       END FUNCTION h5rget_obj_type_c
    END INTERFACE

    hdferr = h5rget_obj_type_c(loc_id, ref_type, ref, obj_type)

  END SUBROUTINE h5rget_obj_type_f

END MODULE H5R
