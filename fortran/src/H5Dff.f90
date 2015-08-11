!****h* ROBODoc/H5D
!
! NAME
!  MODULE H5D
!
! FILE
!  fortran/src/H5Dff.f90
!
! PURPOSE
!  This file contains Fortran interfaces for H5D functions. It includes
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
!  If you add a new H5D function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5D
  USE H5GLOBAL

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

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5dcreate_c(loc_id, name, namelen, type_id, &
            space_id, lcpl_id_default, dcpl_id_default, dapl_id_default, dset_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DCREATE_C'::h5dcreate_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
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
       INTEGER FUNCTION h5dopen_c(loc_id, name, namelen, dapl_id_default, dset_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DOPEN_C'::h5dopen_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
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
       INTEGER FUNCTION h5dclose_c(dset_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DCLOSE_C'::h5dclose_c
         !DEC$ENDIF
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
       INTEGER FUNCTION h5dget_type_c (dataset_id, datatype_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_TYPE_C'::h5dget_type_c
         !DEC$ENDIF
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
       INTEGER FUNCTION h5dset_extent_c(dataset_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DSET_EXTENT_C'::h5dset_extent_c
         !DEC$ENDIF
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
       INTEGER FUNCTION h5dget_create_plist_c(dataset_id, plist_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_CREATE_PLIST_C'::h5dget_create_plist_c
         !DEC$ENDIF
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
       INTEGER FUNCTION h5dget_storage_size_c(dataset_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_STORAGE_SIZE_C'::h5dget_storage_size_c
         !DEC$ENDIF
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
       INTEGER FUNCTION h5dvlen_get_max_len_c(dataset_id, type_id, space_id, len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DVLEN_GET_MAX_LEN_C'::h5dvlen_get_max_len_c
         !DEC$ENDIF
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
       INTEGER FUNCTION h5dget_space_status_c(dset_id, flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_SPACE_STATUS_C'::h5dget_space_status_c
         !DEC$ENDIF
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
       INTEGER FUNCTION h5dcreate_anon_c(loc_id, type_id, space_id, dcpl_id_default, dapl_id_default, dset_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DCREATE_ANON_C'::h5dcreate_anon_c
         !DEC$ENDIF
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
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2)), TARGET :: buf   ! Data buffer
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
            xfer_prp_default, buf, dims, len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_VL_INTEGER_C'::h5dwrite_vl_integer_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
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
            xfer_prp_default, buf, dims, len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_VL_INTEGER_C'::h5dread_vl_integer_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len
         INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
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
            xfer_prp_default, buf, dims, len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_VL_REAL_C'::h5dwrite_vl_real_c
         !DEC$ENDIF
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
            xfer_prp_default, buf, dims, len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_VL_REAL_C'::h5dread_vl_real_c
         !DEC$ENDIF
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
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id     ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims   ! Number of strings
    INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: str_len ! Array to store the length of each element
    CHARACTER(LEN=*), INTENT(IN), &
         DIMENSION(dims(2)) :: buf  ! Data buffer
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
            xfer_prp_default, buf, dims, str_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_VL_STRING_C'::h5dwrite_vl_string_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
         INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: str_len
         CHARACTER(LEN=*), DIMENSION(dims(2)) :: buf
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
                                         hdferr, &
                                         mem_space_id, file_space_id, xfer_prp)
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
            xfer_prp_default, buf, dims, str_len)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_VL_STRING_C'::h5dread_vl_string_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
         INTEGER(SIZE_T), INTENT(OUT), DIMENSION(*) :: str_len
         CHARACTER(LEN=*), DIMENSION(dims(2)) :: buf
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
       INTEGER FUNCTION h5dget_space_c(dataset_id, dataspace_id)
       USE H5GLOBAL
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_SPACE_C'::h5dget_space_c
       !DEC$ENDIF
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
     INTEGER FUNCTION h5dget_access_plist_c(dset_id, plist_id)
       USE H5GLOBAL
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_ACCESS_PLIST_C'::h5dget_access_plist_c
       !DEC$ENDIF
       INTEGER(HID_T), INTENT(IN) :: dset_id
       INTEGER(HID_T), INTENT(OUT) :: plist_id
     END FUNCTION h5dget_access_plist_c
  END INTERFACE
  
  hdferr = h5dget_access_plist_c(dset_id, plist_id)
  
END SUBROUTINE h5dget_access_plist_f

END MODULE H5D


