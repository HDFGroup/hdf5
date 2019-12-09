!****h* ROBODoc/H5D
!
! NAME
!  MODULE H5D
!
! FILE
!  fortran/src/H5Dff.F90
!
! PURPOSE
!  This file contains Fortran interfaces for H5D functions.
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
!  (1) The maximum rank of an array allowed in Fortran is 7, therefore
!  we only provide an interface for arrays up to and including rank 7.
!
!  (2) Unfortunately we are using a generic interface and one of the factors
!  used in determining the proper routine to select is that of the array
!  rank being passed. Therefore, we can not create just one subroutine for
!  each array type (integer, real, etc...) and use a
!  rank 1 array of assumed size to handle multiple ranks, i.e.
!  (i.e. integer, dimension(*) :: ... )
!  (i.e. real   , dimension(*) :: ... ) etc...
!
!  (3) Could not place the USE, INTRINSIC :: ISO_C_BINDING in the module header because it may
!  conflict with the USE, INTRINSIC :: ISO_C_BINDING included in the user's program. Moved
!  the statement instead to each subroutine.
!
!
!  (4) C_LOC and character strings according to the Fortran 2003 standard:
!
!  15.1.2.5 C_LOC(X)
!
!  Argument. X shall either
!
!  (A) have interoperable type and type parameters and be
!  (a) a variable that has the TARGET attribute and is interoperable,
!  (b) an allocated allocatable variable that has the TARGET attribute
!  and is not an array of zero size, or
!  (c) an associated scalar pointer, or
!  (B) be a nonpolymorphic scalar, have no length type parameters, and be
!  (a) a nonallocatable, nonpointer variable that has the TARGET attribute,
!  (b) an allocated allocatable variable that has the TARGET attribute, or
!  (c) an associated pointer.
!
!   	 - When X is a character, for interoperability the standard is:
!
!  15.2.1 Interoperability of intrinsic types
!
!  ...if the type is character, interoperability also requires that the length type parameter
!  be omitted or be specified by an initialization expression whose value is one.
!
!  THEREFORE compilers that have not extended the standard require the
!  argument in C_LOC to be of the variant:
!
!  CHARACTER(LEN=1), TARGET :: chr
!  or
!  CHARACTER, TARGET :: chr
!
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5D function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

#include <H5config_f.inc>

MODULE H5D
  
  USE, INTRINSIC :: ISO_C_BINDING
  USE H5GLOBAL

  PRIVATE h5dread_vl_integer, h5dread_vl_real, h5dread_vl_string
  PRIVATE h5dwrite_vl_integer, h5dwrite_vl_real, h5dwrite_vl_string
  PRIVATE h5dwrite_reference_obj, h5dwrite_reference_dsetreg, h5dwrite_char_scalar, h5dwrite_ptr
  PRIVATE h5dread_reference_obj, h5dread_reference_dsetreg, h5dread_char_scalar, h5dread_ptr
  PRIVATE h5dfill_integer, h5dfill_c_float, h5dfill_c_double, h5dfill_char
#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
  PRIVATE h5dfill_c_long_double
#endif

  INTERFACE h5dextend_f
     MODULE PROCEDURE h5dset_extent_f
  END INTERFACE

  INTERFACE h5dread_vl_f
     MODULE PROCEDURE h5dread_vl_integer
     MODULE PROCEDURE h5dread_vl_real
     MODULE PROCEDURE h5dread_vl_string
  END INTERFACE

  INTERFACE h5dwrite_vl_f
     MODULE PROCEDURE h5dwrite_vl_integer
     MODULE PROCEDURE h5dwrite_vl_real
     MODULE PROCEDURE h5dwrite_vl_string
  END INTERFACE

  INTERFACE h5dwrite_f
     MODULE PROCEDURE h5dwrite_reference_obj
     MODULE PROCEDURE h5dwrite_reference_dsetreg
     MODULE PROCEDURE h5dwrite_char_scalar
     ! This is the preferred way to call h5dwrite
     ! by passing an address
     MODULE PROCEDURE h5dwrite_ptr
  END INTERFACE

  INTERFACE h5dread_f
     MODULE PROCEDURE h5dread_reference_obj
     MODULE PROCEDURE h5dread_reference_dsetreg
     MODULE PROCEDURE h5dread_char_scalar
     ! This is the preferred way to call h5dread
     ! by passing an address
     MODULE PROCEDURE h5dread_ptr

  END INTERFACE


!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Dwrite routine

  INTERFACE
     INTEGER FUNCTION h5dwrite_f_c(dset_id, mem_type_id, &
          mem_space_id_default ,                         &
          file_space_id_default,                         &
          xfer_prp_default, buf ) BIND(C, NAME='h5dwrite_f_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: dset_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       INTEGER(HID_T) :: mem_space_id_default
       INTEGER(HID_T) :: file_space_id_default
       INTEGER(HID_T) :: xfer_prp_default
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5dwrite_f_c
  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Dread routine

  INTERFACE
     INTEGER FUNCTION h5dread_f_c(dset_id, mem_type_id, &
          mem_space_id_default,                         &
          file_space_id_default,                        &
          xfer_prp_default, buf) BIND(C, NAME='h5dread_f_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: dset_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       INTEGER(HID_T) :: mem_space_id_default
       INTEGER(HID_T) :: file_space_id_default
       INTEGER(HID_T) :: xfer_prp_default
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5dread_f_c
  END INTERFACE

  INTERFACE h5dfill_f
     MODULE PROCEDURE h5dfill_integer
     MODULE PROCEDURE h5dfill_c_float
     MODULE PROCEDURE h5dfill_c_double
#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
     MODULE PROCEDURE h5dfill_c_long_double
#endif
     MODULE PROCEDURE h5dfill_char
  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Dfill routine

  INTERFACE
     INTEGER FUNCTION h5dfill_c(f_ptr_fill_value, fill_type_id, space_id, &
          f_ptr_buf, mem_type_id) BIND(C, NAME='h5dfill_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       TYPE(C_PTR), VALUE :: f_ptr_fill_value
       INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
       INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
       TYPE(C_PTR), VALUE :: f_ptr_buf
       INTEGER(HID_T) :: mem_type_id
     END FUNCTION h5dfill_c
  END INTERFACE

CONTAINS

!
!****s* H5D/h5dcreate_f
!
! NAME
!  h5dcreate_f
!
! PURPOSE
!  Creates a dataset at the specified location
!
! INPUTS
!  loc_id 	 - file or group identifier
!  name 	 - dataset name
!  type_id 	 - dataset datatype identifier
!  space_id 	 - dataset dataspace identifier
! OUTPUTS
!  dset_id 	 - dataset identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  creation_prp  - Dataset creation property list
!  lcpl_id 	 - Link creation property list
!  dapl_id 	 - Dataset access property list
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!
!  - Explicit Fortran interfaces were added for
!    called C functions (it is needed for Windows
!    port).  February 28, 2001
!
!  - Added version's 1.8 new optional parameters
!    February, 2008
!
! SOURCE
  SUBROUTINE h5dcreate_f(loc_id, name, type_id, space_id, dset_id, &
       hdferr, dcpl_id, lcpl_id, dapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the dataset
    INTEGER(HID_T), INTENT(IN) :: type_id  ! Datatype identifier
    INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
    INTEGER(HID_T), INTENT(OUT) :: dset_id ! Dataset identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: dcpl_id ! Dataset creation property list
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lcpl_id ! Link creation property list
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: dapl_id ! Dataset access property list

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: dcpl_id_default
    INTEGER(HID_T) :: dapl_id_default

    INTEGER :: namelen ! Name length

    INTERFACE
       INTEGER FUNCTION h5dcreate_c(loc_id, name, namelen, type_id, &
            space_id, lcpl_id_default, dcpl_id_default, dapl_id_default, dset_id) &
            BIND(C,NAME='h5dcreate_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id

         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: dcpl_id_default
         INTEGER(HID_T) :: dapl_id_default

         INTEGER(HID_T), INTENT(OUT) :: dset_id
       END FUNCTION h5dcreate_c
    END INTERFACE

    lcpl_id_default = H5P_DEFAULT_F
    dcpl_id_default = H5P_DEFAULT_F
    dapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    IF(PRESENT(dcpl_id)) dcpl_id_default = dcpl_id
    IF(PRESENT(dapl_id)) dapl_id_default = dapl_id

    namelen = LEN(name)
    hdferr = h5dcreate_c(loc_id, name, namelen, type_id, space_id, &
         lcpl_id_default, dcpl_id_default, dapl_id_default, dset_id)

  END SUBROUTINE h5dcreate_f

!
!****s* H5D/h5dopen_f
!
! NAME
!  h5dopen_f
!
! PURPOSE
!  Opens an existing dataset.
!
! INPUTS
!  loc_id 	 - file or group identifier
!  name 	 - dataset name
! OUTPUTS
!  dset_id 	 - dataset identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  dapl_id 	 - Dataset access property list
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  -Explicit Fortran interfaces were added for
!   called C functions (it is needed for Windows
!   port).  February 28, 2001
!
!  -Added 1.8 (optional) parameter dapl_id
!   February, 2008, M. Scot Breitenfeld
!
! SOURCE
  SUBROUTINE h5dopen_f(loc_id, name, dset_id, hdferr, dapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the dataset
    INTEGER(HID_T), INTENT(OUT) :: dset_id ! Dataset identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: dapl_id ! Dataset access property list
!*****
    INTEGER :: namelen                     ! Name length

    INTEGER(HID_T) :: dapl_id_default

    INTERFACE
       INTEGER FUNCTION h5dopen_c(loc_id, name, namelen, dapl_id_default, dset_id) &
            BIND(C,NAME='h5dopen_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(HID_T), INTENT(IN) :: dapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: dset_id
       END FUNCTION h5dopen_c
    END INTERFACE

    dapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(dapl_id)) dapl_id_default = dapl_id

    namelen = LEN(name)
    hdferr = h5dopen_c(loc_id, name, namelen, dapl_id_default, dset_id)

  END SUBROUTINE h5dopen_f

!
!****s* H5D/h5dclose_f
!
! NAME
!  h5dclose_f
!
! PURPOSE
!  Closes a dataset.
!
! INPUTS
!  dset_id 	 - dataset identifier
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
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
! SOURCE
  SUBROUTINE h5dclose_f(dset_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id ! Dataset identifier
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5dclose_c(dset_id) &
            BIND(C,NAME='h5dclose_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
       END FUNCTION h5dclose_c
    END INTERFACE

    hdferr = h5dclose_c(dset_id)

  END SUBROUTINE h5dclose_f

!
!****s* H5D/h5dget_type_f
!
! NAME
!  h5dget_type_f
!
! PURPOSE
!  Returns an identifier for a copy of the datatype for a
!  dataset.
!
! INPUTS
!  dataset_id 	 - dataset identifier
! OUTPUTS
!  datatype_id 	 - dataspace identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
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
!
! SOURCE
  SUBROUTINE h5dget_type_f(dataset_id, datatype_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
    INTEGER(HID_T), INTENT(OUT) :: datatype_id    ! Datatype identifier
    INTEGER, INTENT(OUT) :: hdferr                ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5dget_type_c(dataset_id, datatype_id) &
            BIND(C,NAME='h5dget_type_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dataset_id
         INTEGER(HID_T), INTENT(OUT) :: datatype_id
       END FUNCTION h5dget_type_c
    END INTERFACE

    hdferr = h5dget_type_c (dataset_id, datatype_id)
  END SUBROUTINE h5dget_type_f

!
!****s* H5D/h5dset_extent
!
! NAME
!  h5dset_extent (instead of obsolete name: h5dextend_f)
!
! PURPOSE
!  Extends a dataset with unlimited dimension.
!
! INPUTS
!  dataset_id 	 - dataset identifier
!  size 	 - array containing the new magnitude of
!                  each dimension
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
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
!  Changed name from the now obsolete h5dextend_f
!  to h5dset_extent_f. Provided interface to old name
!  for backward compatability. -MSB- March 14, 2008
!
! SOURCE
  SUBROUTINE h5dset_extent_f(dataset_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN)  :: size
    ! Array containing
    ! dimensions' sizes
    INTEGER, INTENT(OUT) :: hdferr                ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5dset_extent_c(dataset_id, size) &
            BIND(C,NAME='h5dset_extent_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dataset_id
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN)  :: size
       END FUNCTION h5dset_extent_c
    END INTERFACE

    hdferr = H5Dset_extent_c(dataset_id, size)
  END SUBROUTINE h5dset_extent_f

!****s* H5D/h5dget_create_plist_f
!
! NAME
!  h5dget_create_plist_f
!
! PURPOSE
!  Returns an identifier for a copy of the dataset creation
!  property list for a dataset.
!
! INPUTS
!  dataset_id 	 - dataset identifier
! OUTPUTS
!  plist_id 	 - creation property list identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  February 28, 2001
! SOURCE
  SUBROUTINE h5dget_create_plist_f(dataset_id, plist_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(OUT) :: plist_id    ! Dataset creation
                                               ! property list identifier
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5dget_create_plist_c(dataset_id, plist_id) &
            BIND(C,NAME='h5dget_create_plist_c')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: dataset_id
         INTEGER(HID_T), INTENT(OUT) :: plist_id
       END FUNCTION h5dget_create_plist_c
    END INTERFACE

    hdferr = h5dget_create_plist_c(dataset_id, plist_id)
  END SUBROUTINE h5dget_create_plist_f

!
!****s* H5D/h5dget_storage_size_f
!
! NAME
!  h5dget_storage_size_f
!
! PURPOSE
!  Returns the amount of storage requires by a dataset
!
! INPUTS
!  dataset_id 	 - dataset identifier
! OUTPUTS
!  size 	 - datastorage size
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  October 15, 2002
! SOURCE
  SUBROUTINE h5dget_storage_size_f(dataset_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id ! Dataset identifier
    INTEGER(HSIZE_T),  INTENT(OUT)  :: size  ! Amount of storage
                                             ! allocated for dataset
    INTEGER, INTENT(OUT) :: hdferr           ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5dget_storage_size_c(dataset_id, size) &
            BIND(C,NAME='h5dget_storage_size_c')
         IMPORT :: HID_T, HSIZE_T
         INTEGER(HID_T), INTENT(IN) :: dataset_id
         INTEGER(HSIZE_T), INTENT(OUT)  :: size
       END FUNCTION h5dget_storage_size_c
    END INTERFACE

    hdferr = h5dget_storage_size_c(dataset_id, size)
  END SUBROUTINE h5dget_storage_size_f

!
!****s* H5D/h5dvlen_get_max_len_f
!
! NAME
!  h5dvlen_get_max_len_f
!
! PURPOSE
!  Returns maximum length of the VL array elements
!
! INPUTS
!  dataset_id 	 - dataset identifier
!  type_id 	 - datatype identifier
!  space_id 	 - dataspace identifier
! OUTPUTS
!  size 	 - buffer size
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  NONE
!
! AUTHOR
!  Elena Pourmal
!  October 15, 2002
!
! SOURCE
  SUBROUTINE h5dvlen_get_max_len_f(dataset_id, type_id, space_id, len,  hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: type_id         ! Datatype identifier
    INTEGER(HID_T), INTENT(IN) :: space_id        ! Dataspace identifier
    INTEGER(SIZE_T),  INTENT(OUT)  :: len         ! Maximum length of the element
    INTEGER, INTENT(OUT) :: hdferr                ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5dvlen_get_max_len_c(dataset_id, type_id, space_id, len) &
            BIND(C,NAME='h5dvlen_get_max_len_c')
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), INTENT(IN) :: dataset_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(SIZE_T), INTENT(OUT)  :: len
       END FUNCTION h5dvlen_get_max_len_c
    END INTERFACE

    hdferr = h5dvlen_get_max_len_c(dataset_id, type_id,  space_id, len)
  END SUBROUTINE h5dvlen_get_max_len_f

!
!****s* H5D/h5dget_space_status_f
!
! NAME
!  h5dget_space_status_f
!
! PURPOSE
!  Returns the status of data space allocation.
!
! INPUTS
!  dset_id	 - dataset identifier
! OUTPUTS
!  flag          - status; may have one of the following values:
!		    H5D_SPACE_STS_ERROR_F
!		    H5D_SPACE_STS_NOT_ALLOCATED_F
!		    H5D_SPACE_STS_PART_ALLOCATED_F
!		    H5D_SPACE_STS_ALLOCATED_F
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
! SOURCE
  SUBROUTINE h5dget_space_status_f(dset_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id  ! Dataspace identifier
    INTEGER, INTENT(OUT)       :: flag     ! Memory buffer to fill in
    INTEGER, INTENT(OUT)       :: hdferr   ! Error code
  !*****
    INTERFACE
       INTEGER FUNCTION h5dget_space_status_c(dset_id, flag) &
            BIND(C,NAME='h5dget_space_status_c')
         IMPORT :: HID_T
         INTEGER(HID_T) :: dset_id
         INTEGER        :: flag
       END FUNCTION h5dget_space_status_c
    END INTERFACE

    hdferr = h5dget_space_status_c(dset_id, flag)
  END SUBROUTINE h5dget_space_status_f

!
!****s* H5D/h5dcreate_anon_f
!
! NAME
!  h5dcreate_anon_f
!
! PURPOSE
!  Creates a dataset in a file without linking it into the file structure
!
! INPUTS
!  loc_id	 - Identifier of the file or group within which to create the dataset.
!  type_id	 - Identifier of the datatype to use when creating the dataset.
!  space_id	 - Identifier of the dataspace to use when creating the dataset.
! OUTPUTS
!  dset_id	 - dataset identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  dcpl_id       - Dataset creation property list identifier.
!  dapl_id  	 - Dataset access property list identifier.
!
! AUTHOR
!  M. Scot Breitenfeld
!  February 11, 2008
!
! SOURCE
  SUBROUTINE h5dcreate_anon_f(loc_id, type_id, space_id, dset_id, hdferr, dcpl_id, dapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier.
    INTEGER(HID_T), INTENT(IN) :: type_id  ! Datatype identifier.
    INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier.
    INTEGER(HID_T), INTENT(OUT) :: dset_id ! Dataset identifier.
    INTEGER, INTENT(OUT) :: hdferr         ! Error code.
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: dcpl_id  ! Dataset creation property list identifier.
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: dapl_id  ! Dataset access property list identifier.
!*****
    INTEGER(HID_T) :: dcpl_id_default
    INTEGER(HID_T) :: dapl_id_default

    !
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dcreate_anon_c(loc_id, type_id, space_id, dcpl_id_default, dapl_id_default, dset_id) &
            BIND(C,NAME='h5dcreate_anon_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HID_T) :: dcpl_id_default
         INTEGER(HID_T) :: dapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: dset_id
       END FUNCTION h5dcreate_anon_c
    END INTERFACE

    dcpl_id_default = H5P_DEFAULT_F
    dapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(dcpl_id)) dcpl_id_default = dcpl_id
    IF(PRESENT(dapl_id)) dapl_id_default = dapl_id

    hdferr = h5dcreate_anon_c(loc_id, type_id, space_id, dcpl_id_default, dapl_id_default, dset_id)

  END SUBROUTINE h5dcreate_anon_f

  SUBROUTINE h5dwrite_vl_integer(dset_id, mem_type_id, buf, dims, len, &
       hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims ! MAX len x num_elem
    INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len ! Array to store
                                                     ! the length of each
                                                     ! element
    INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf   ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_vl_integer_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims, len) &
            BIND(C,NAME='h5dwrite_vl_integer_c')
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len
         INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dwrite_vl_integer_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_vl_integer_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, len)

  END SUBROUTINE h5dwrite_vl_integer

  SUBROUTINE h5dread_vl_integer(dset_id, mem_type_id, buf, dims, len, &
       hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims  ! MAX len x num_elem
    INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len ! Array to store
                                                        ! the length of each
                                                        ! element
    INTEGER, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf   ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr ! Error code
                                   ! -1 if failed, 0 otherwise
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    INTEGER(HID_T) :: tmp
    INTEGER :: error

    INTERFACE
       INTEGER FUNCTION h5dread_vl_integer_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims, len) &
            BIND(C,NAME='h5dread_vl_integer_c')
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len
         INTEGER, INTENT(INOUT), DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dread_vl_integer_c
    END INTERFACE

    CALL h5dget_space_f(dset_id, tmp, error)
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = tmp
    file_space_id_default = tmp

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_vl_integer_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, len)

  END SUBROUTINE h5dread_vl_integer

  SUBROUTINE h5dwrite_vl_real(dset_id, mem_type_id, buf, dims, len, &
       hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims ! MAX len x num_elem
    INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len   ! Array to store
                                                       ! the length of each
                                                       ! element
    REAL, INTENT(IN), &
         DIMENSION(dims(1),dims(2)) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_vl_real_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims, len) &
            BIND(C,NAME='h5dwrite_vl_real_c')
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len
         REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dwrite_vl_real_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_vl_real_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, len)

  END SUBROUTINE h5dwrite_vl_real

  SUBROUTINE h5dread_vl_real(dset_id, mem_type_id, buf, dims, len, &
       hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims  ! MAX len x num_elem
    INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len ! Array to store the length of each element
    REAL, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
                                           ! -1 if failed, 0 otherwise
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    INTEGER(HID_T) :: tmp
    INTEGER :: error

    INTERFACE
       INTEGER FUNCTION h5dread_vl_real_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims, len) &
            BIND(C,NAME='h5dread_vl_real_c')
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len
         REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dread_vl_real_c
    END INTERFACE

    CALL h5dget_space_f(dset_id, tmp, error)
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = tmp
    file_space_id_default = tmp

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_vl_real_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, len)

  END SUBROUTINE h5dread_vl_real

  SUBROUTINE h5dwrite_vl_string(dset_id, mem_type_id, buf, dims, str_len, &
       hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_char
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims   ! Number of strings
    INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: str_len ! Array to store the length of each element
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(2)) :: buf  ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dwrite_vl_string_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            ! xfer_prp_default, tmp_buf, dims, str_len)
            xfer_prp_default, buf, dims, str_len) &
            BIND(C,NAME='h5dwrite_vl_string_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
         INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: str_len
         CHARACTER(KIND=C_CHAR), DIMENSION(dims(2)) :: buf
       END FUNCTION
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_vl_string_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, str_len)

  END SUBROUTINE h5dwrite_vl_string

  SUBROUTINE h5dread_vl_string(dset_id, mem_type_id, buf, dims, str_len, &
       hdferr, mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims ! number of strings
    INTEGER(SIZE_T), INTENT(OUT), DIMENSION(*) :: str_len ! Array to store
    ! the length of each
    ! element
    CHARACTER(LEN=*), INTENT(OUT), &
         DIMENSION(dims(2)) :: buf      ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T)  :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTERFACE
       INTEGER FUNCTION h5dread_vl_string_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims, str_len) &
            BIND(C,NAME='h5dread_vl_string_c')
         IMPORT :: c_char
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
         INTEGER(SIZE_T), INTENT(OUT), DIMENSION(*) :: str_len
         CHARACTER(KIND=C_CHAR), DIMENSION(dims(2)) :: buf
       END FUNCTION h5dread_vl_string_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_vl_string_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, &
         buf, dims, str_len)
    RETURN
  END SUBROUTINE h5dread_vl_string

!
!****s* H5D/h5dget_offset_f
!
! NAME
!  h5dget_offset_f
!
! PURPOSE
!  Returns dataset address in file.
!
! INPUTS
!  dataset_id  - Dataset identifier.
! OUTPUTS
!  offset      - The offset in bytes.
!  hdferr      - Returns 0 if successful and -1 if fails.
!
! AUTHOR
!  M. Scot Breitenfeld
!  April 16, 2015
!
! SOURCE
  SUBROUTINE h5dget_offset_f(dset_id, offset, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)    :: dset_id
    INTEGER(HADDR_T), INTENT(OUT) :: offset 
    INTEGER, INTENT(OUT)          :: hdferr
!*****
    INTERFACE
       INTEGER(HADDR_T) FUNCTION h5dget_offset(dset_id) BIND(C,NAME='H5Dget_offset')
         IMPORT :: HID_T, HADDR_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: dset_id
       END FUNCTION h5dget_offset
    END INTERFACE

    offset = h5dget_offset(dset_id)
    
    hdferr = 0 ! never returns a function error because C API never returns a function error.

  END SUBROUTINE h5dget_offset_f

!
!****s* H5D/h5dget_space_f
!
! NAME
!  h5dget_space_f
!
! PURPOSE
!  Returns an identifier for a copy of the dataspace for a
!  dataset.
!
! INPUTS
!  dataset_id 	 - dataset identifier
! OUTPUTS
!  dataspace_id  - dataspace identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
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
! SOURCE
  SUBROUTINE h5dget_space_f(dataset_id, dataspace_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
    INTEGER(HID_T), INTENT(OUT) :: dataspace_id   ! Dataspace identifier
    INTEGER, INTENT(OUT) :: hdferr                ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5dget_space_c(dataset_id, dataspace_id) BIND(C,NAME='h5dget_space_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dataset_id
         INTEGER(HID_T), INTENT(OUT) :: dataspace_id
       END FUNCTION h5dget_space_c
    END INTERFACE

    hdferr = h5dget_space_c(dataset_id, dataspace_id)
  END SUBROUTINE h5dget_space_f

!****s* H5D/h5dget_access_plist_f
!
! NAME
!  h5dget_access_plist_f
!
! PURPOSE
!  Returns a copy of the dataset creation property list.
!
! INPUTS
!  dset_id       - Dataset identifier
!
! OUTPUTS
!  plist_id	 - Dataset access property list identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR   
!  M. Scot Breitenfeld
!  April 13, 2009
!
! SOURCE
  SUBROUTINE h5dget_access_plist_f(dset_id, plist_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: dset_id
    INTEGER(HID_T), INTENT(OUT) :: plist_id 
    INTEGER       , INTENT(OUT) :: hdferr  
    !*****
    INTERFACE
       INTEGER FUNCTION h5dget_access_plist_c(dset_id, plist_id) BIND(C,NAME='h5dget_access_plist_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(OUT) :: plist_id
       END FUNCTION h5dget_access_plist_c
    END INTERFACE
    
    hdferr = h5dget_access_plist_c(dset_id, plist_id)
  
  END SUBROUTINE h5dget_access_plist_f


  SUBROUTINE h5dwrite_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf
    TYPE(hobj_ref_t_f), DIMENSION(dims(1)), INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_reference_obj

  SUBROUTINE h5dwrite_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf
    TYPE(hdset_reg_ref_t_f), DIMENSION(dims(1)), INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
    INTEGER :: i
    INTEGER(HSIZE_T) :: j
    TYPE(C_PTR) :: f_ptr
    INTERFACE
       INTEGER FUNCTION h5dwrite_ref_reg_c(dset_id, mem_type_id,&
            mem_space_id_default, &
            file_space_id_default, xfer_prp_default, ref_buf, dims) &
            BIND(C,NAME='h5dwrite_ref_reg_c')
         IMPORT :: HID_T, HSIZE_T, SIZE_T
         IMPLICIT NONE
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

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

    ALLOCATE(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
    IF (hdferr .NE. 0 ) THEN
       hdferr = -1
       RETURN
    ELSE
       DO j = 1, dims(1)
          DO i = 1, REF_REG_BUF_LEN
             ref_buf(REF_REG_BUF_LEN*(j-1) + i) = buf(j)%ref(i)
          ENDDO
       ENDDO
    ENDIF
    hdferr = h5dwrite_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, ref_buf, dims)
    DEALLOCATE(ref_buf)

  END SUBROUTINE h5dwrite_reference_dsetreg

  SUBROUTINE h5dwrite_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(*), INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
                                 
    CALL h5dwrite_char_scalar_fix(dset_id, mem_type_id, buf, LEN(buf), dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)

  END SUBROUTINE h5dwrite_char_scalar

  SUBROUTINE h5dwrite_char_scalar_fix(dset_id, mem_type_id, buf, buf_len, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN) :: buf_len
    CHARACTER(LEN=buf_len), INTENT(IN), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    f_ptr = C_LOC(buf(1:1))

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dwrite_char_scalar_fix

  SUBROUTINE h5dread_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    TYPE(hobj_ref_t_f), INTENT(INOUT) , &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)

  END SUBROUTINE h5dread_reference_obj

  SUBROUTINE h5dread_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    TYPE(hdset_reg_ref_t_f), INTENT(INOUT), &
         DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
    INTEGER :: i
    INTEGER(HSIZE_T) :: j
    INTERFACE
       INTEGER FUNCTION h5dread_ref_reg_c(dset_id, mem_type_id,&
            mem_space_id_default, &
            file_space_id_default, xfer_prp_default, ref_buf, dims) &
            BIND(C,NAME='h5dread_ref_reg_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER, DIMENSION(*) :: ref_buf
       END FUNCTION h5dread_ref_reg_c
    END INTERFACE

    ALLOCATE(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, ref_buf, dims)

    DO j = 1, dims(1)
       DO i = 1, REF_REG_BUF_LEN
          buf(j)%ref(i) = ref_buf(REF_REG_BUF_LEN*(j-1) + i)
       ENDDO
    ENDDO
    DEALLOCATE(ref_buf)

  END SUBROUTINE h5dread_reference_dsetreg

  SUBROUTINE h5dread_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id         ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id   ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT) :: buf     ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr             ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id

    CALL h5dread_char_scalar_fix(dset_id, mem_type_id, buf, LEN(buf), hdferr, &
         mem_space_id_default, file_space_id_default, xfer_prp_default)

  END SUBROUTINE h5dread_char_scalar

  SUBROUTINE h5dread_char_scalar_fix(dset_id, mem_type_id, buf, buf_len, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER, INTENT(IN)  :: buf_len
    CHARACTER(LEN=buf_len), INTENT(INOUT), TARGET :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1:1))

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id, &
         file_space_id, xfer_prp, f_ptr)

  END SUBROUTINE h5dread_char_scalar_fix

!****s* H5D (F03)/h5dwrite_f_F03
!
! NAME		
!  h5dwrite_f_F03
!
! PURPOSE
!  Writes raw data from a dataset into a buffer. 
!
! Inputs:
!  dset_id	 - Identifier of the dataset to write to.
!  mem_type_id	 - Identifier of the memory datatype.
!  buf		 - Buffer with data to be written to the file.
!  
! Outputs:
!  hdferr        - Returns 0 if successful and -1 if fails
!
! Optional parameters:
!  mem_space_id	 - Identifier of the memory dataspace.
!  file_space_id - Identifier of the dataset's dataspace in the file.
!  xfer_prp	 - Identifier of a transfer property list for this I/O operation.
!
! AUTHOR
!  M. Scot Breitenfeld
!  September 17, 2011
!
! Fortran2003 Interface:
!!  SUBROUTINE h5dwrite_f(dset_id, mem_type_id, buf, hdferr, &
!!                        mem_space_id, file_space_id, xfer_prp)
!!    INTEGER(HID_T), INTENT(IN)              :: dset_id
!!    INTEGER(HID_T), INTENT(IN)              :: mem_type_id
!!    TYPE(C_PTR)   , INTENT(IN)              :: buf
!!    INTEGER       , INTENT(OUT)             :: hdferr
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: mem_space_id
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: file_space_id
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: xfer_prp
!*****
  SUBROUTINE h5dwrite_ptr(dset_id, mem_type_id, buf, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    TYPE(C_PTR), INTENT(IN) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf)

  END SUBROUTINE h5dwrite_ptr

!****s* H5D (F03)/h5dread_f_F03
!
! NAME		
!  h5dread_f_F03
!
! PURPOSE
!  Reads raw data from a dataset into a buffer. 
!
! Inputs:
!  dset_id	 - Identifier of the dataset read from.
!  mem_type_id	 - Identifier of the memory datatype.
!  
! Outputs:
!  buf		 - Buffer to receive data read from file.
!  hdferr        - Returns 0 if successful and -1 if fails
!
! Optional parameters:
!  mem_space_id	 - Identifier of the memory dataspace.
!  file_space_id - Identifier of the dataset's dataspace in the file.
!  xfer_prp	 - Identifier of a transfer property list for this I/O operation.
!
! AUTHOR
!  M. Scot Breitenfeld
!  September 17, 2011
!
! Fortran2003 Interface:
!!  SUBROUTINE h5dread_f(dset_id, mem_type_id, buf, hdferr, &
!!                       mem_space_id, file_space_id, xfer_prp)
!!    INTEGER(HID_T), INTENT(IN)              :: dset_id
!!    INTEGER(HID_T), INTENT(IN)              :: mem_type_id
!!    TYPE(C_PTR)   , INTENT(INOUT)           :: buf
!!    INTEGER       , INTENT(OUT)             :: hdferr
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: mem_space_id
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: file_space_id
!!    INTEGER(HID_T), INTENT(IN)   , OPTIONAL :: xfer_prp
!*****
  SUBROUTINE h5dread_ptr(dset_id, mem_type_id, buf, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    TYPE(C_PTR), INTENT(INOUT) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id  ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp      ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    IF (PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF (PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF (PRESENT(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf)

  END SUBROUTINE h5dread_ptr

!
! NAME		
!  h5dfill_integer
!
! PURPOSE 
!  Fills dataspace elements with a fill value in a memory buffer.
!  Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes
!  of the fillvalues and buffers are supported. Buffer and fillvalue
!  are assumed to have the same datatype.
!  Only one-dimesional buffers are supported.
!
! Inputs:
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
!

  SUBROUTINE h5dfill_integer(fill_value, space_id, buf,  hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER, INTENT(IN), TARGET :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    INTEGER, INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id  ! Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf        ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf(1))

    fill_type_id = H5T_NATIVE_INTEGER
    mem_type_id  = H5T_NATIVE_INTEGER

    hdferr = h5dfill_c(f_ptr_fill_value, fill_type_id, space_id, &
         f_ptr_buf, mem_type_id)

  END SUBROUTINE h5dfill_integer

!
! NAME
!  h5dfill_c_float
!
! PURPOSE
!  Fills dataspace elements with a fill value in a memory buffer.
!  Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes
!  of the fillvalues and buffers are supported. Buffer and fillvalue
!  are assumed to have the same datatype.
!  Only one-dimesional buffers are supported.
!
! Inputs:
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
  SUBROUTINE h5dfill_c_float(fill_valuer, space_id, buf,  hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: fill_valuer  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    REAL(KIND=C_FLOAT), INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_valuer ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_valuer = C_LOC(fill_valuer)
    f_ptr_buf = C_LOC(buf(1))

    fill_type_id = H5T_NATIVE_REAL
    mem_type_id  = H5T_NATIVE_REAL

    hdferr = h5dfill_c(f_ptr_fill_valuer, fill_type_id, space_id, &
         f_ptr_buf, mem_type_id)

  END SUBROUTINE h5dfill_c_float

  !----------------------------------------------------------------------
  ! Name:	  h5dfill_c_double
  !
  ! Purpose:      Fills dataspace elements with a fill value in a memory buffer.
  !               Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes
  !               of the fillvalues and buffers are supported. Buffer and fillvalue
  !               are assumed to have the same datatype.
  !               Only one-dimesional buffers are supported.
  !
  ! Inputs:
  !		fill_value	- fill value
  !		space_id	- memory space selection identifier
  !		buf		- data buffer iin memory ro apply selection to
  !				- of k-th dimension of the buf array
  ! Outputs:
  !		hdferr:		- error code
  !				 	Success:  0
  !				 	Failure: -1
  !
  ! Programmer:	Elena Pourmal
  !		March 12, 2003
  !
  !----------------------------------------------------------------------

  SUBROUTINE h5dfill_c_double(fill_value, space_id, buf,  hdferr)
    IMPLICIT NONE
    REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    REAL(KIND=C_DOUBLE), INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_valuer ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_valuer = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf(1))

    fill_type_id = H5T_NATIVE_DOUBLE
    mem_type_id  = H5T_NATIVE_DOUBLE

    hdferr = h5dfill_c(f_ptr_fill_valuer, fill_type_id, space_id, &
         f_ptr_buf, mem_type_id)

  END SUBROUTINE h5dfill_c_double

#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
  SUBROUTINE h5dfill_c_long_double(fill_value, space_id, buf,  hdferr)
    IMPLICIT NONE
    REAL(KIND=C_LONG_DOUBLE), INTENT(IN), TARGET :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    REAL(KIND=C_LONG_DOUBLE), INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_valuer ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_valuer = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf(1))

    fill_type_id = H5T_NATIVE_DOUBLE
    mem_type_id  = H5T_NATIVE_DOUBLE

    hdferr = h5dfill_c(f_ptr_fill_valuer, fill_type_id, space_id, &
         f_ptr_buf, mem_type_id)

  END SUBROUTINE h5dfill_c_long_double
#endif
!
! NAME		
!  h5dfill_char
!
! PURPOSE 
!  Fills dataspace elements with a fill value in a memory buffer.
!  Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes
!  of the fillvalues and buffers are supported. Buffer and fillvalue
!  are assumed to have the same datatype.
!  Only one-dimesional buffers are supported.
!
! Inputs:
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! AUTHOR
!  Elena Pourmal
!  March 12, 2003
!
  SUBROUTINE h5dfill_char(fill_value, space_id, buf,  hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    CHARACTER, INTENT(IN), TARGET :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    CHARACTER, INTENT(IN), DIMENSION(*), TARGET :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    TYPE(C_PTR) :: f_ptr_fill_value ! C pointer to fill_value
    TYPE(C_PTR) :: f_ptr_buf ! C pointer to buf

    f_ptr_fill_value = C_LOC(fill_value)
    f_ptr_buf = C_LOC(buf(1))

    hdferr = h5dfill_c(f_ptr_fill_value, fill_type_id, space_id, &
         f_ptr_buf, mem_type_id)

  END SUBROUTINE h5dfill_char
!
!****s* H5D (F03)/h5dvlen_reclaim_f
! NAME
!  h5dvlen_reclaim_f
!
! PURPOSE 
!  Reclaims VL datatype memory buffers. 
!
! Inputs:
!
!  type_id  - Identifier of the datatype. 
!  space_id - Identifier of the dataspace. 
!  plist_id - Identifier of the property list used to create the buffer. 
!  buf      - Pointer to the buffer to be reclaimed. 
!
! Outputs:
!  hdferr   - Returns 0 if successful and -1 if fails
!
! AUTHOR
! M. Scot Breitenfeld
! January 11, 2011
!
! Fortran2003 Interface:
  SUBROUTINE h5dvlen_reclaim_f(type_id, space_id, plist_id, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)    :: type_id
    INTEGER(HID_T), INTENT(IN)    :: space_id
    INTEGER(HID_T), INTENT(IN)    :: plist_id
    TYPE(C_PTR)   , INTENT(INOUT) :: buf
    INTEGER       , INTENT(OUT)   :: hdferr
!*****

    INTERFACE
       INTEGER FUNCTION h5dvlen_reclaim_c(type_id, space_id, plist_id, buf) BIND(C, NAME='h5dvlen_reclaim_c')
         IMPORT :: C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T) :: type_id
         INTEGER(HID_T) :: space_id
         INTEGER(HID_T) :: plist_id
         TYPE(C_PTR), VALUE :: buf
       END FUNCTION h5dvlen_reclaim_c
    END INTERFACE

    hdferr = H5Dvlen_reclaim_c(type_id, space_id, plist_id, buf)

  END SUBROUTINE H5Dvlen_reclaim_f


END MODULE H5D


