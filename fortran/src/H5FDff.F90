!> @defgroup FH5VFD Fortran VFD (H5VFD) Interface
!!
!! @see H5VFD, C-API
!!

!> @ingroup FH5VFD
!!
!! @brief This module contains Fortran interfaces for H5VFD (Virtual File Driver) functions.
!!
!! The H5VFD module provides Fortran bindings for HDF5 Virtual File Driver operations,
!! including subfiling functionality for parallel I/O optimization.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the LICENSE file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5VFD function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

#include <H5config_f.inc>

MODULE H5FD

  USE H5GLOBAL
  USE H5fortkit

  IMPLICIT NONE

#ifndef H5_DOXYGEN
  INTERFACE
     ! Helper function to free the C memory allocated by H5FDsubfiling_get_file_mapping
     INTEGER(C_INT) FUNCTION h5free_string_array_memory_c(filenames_ptr, num_files) &
          BIND(C, NAME='h5free_string_array_memory_c')
       IMPORT :: C_INT, C_PTR, C_SIZE_T
       IMPLICIT NONE
       TYPE(C_PTR) :: filenames_ptr
       INTEGER(C_SIZE_T) :: num_files
     END FUNCTION h5free_string_array_memory_c
  END INTERFACE
#endif

CONTAINS

#ifdef H5_HAVE_SUBFILING_VFD
!>
!! \ingroup FH5VFD
!!
!! \brief Retrieve the list of subfile names for a HDF5 file for the subfiling VFD
!!
!! \details This function retrieves the names of all subfiles associated with an HDF5 file
!!          that uses the subfiling Virtual File Driver (VFD). The subfiling VFD distributes
!!          file data across multiple subfiles to improve parallel I/O performance on shared
!!          file systems.
!!
!!          The returned filenames correspond to the physical subfiles stored on the file system
!!          that collectively make up the logical HDF5 file. This information is useful for:
!!          - File management and backup operations
!!          - Understanding the physical storage layout
!!          - Tools like h5fuse for recombining subfiles
!!          - Debugging subfiling configurations
!!
!! \param file_id      [in] HDF5 file identifier for a file using the subfiling VFD
!! \param filenames    [out] Allocatable array of subfile names. Memory is automatically
!!                     allocated by the function and must be deallocated by the caller.
!!                     See **Compiler Compatibility** note below.
!! \param num_files    [out] Number of subfiles in the filenames array
!! \param hdferr       [out] Error code:
!!                     \li 0 on success
!!                     \li -1 on failure
!!
!! \since 2.0.0
!!
!! \note **Memory Management**: The filenames array is automatically allocated. The caller
!!       is responsible for deallocating when it is no longer needed:
!!       \code{.f90}
!!       DEALLOCATE(filenames)
!!       \endcode
!!
!! \note
!! \parblock
!! **Compiler Compatibility**:
!! - With H5_FORTRAN_HAVE_CHAR_ALLOC: Variable-length character strings (Fortran 2003+)
!! - Without H5_FORTRAN_HAVE_CHAR_ALLOC: Fixed-length 8192 character strings (older compilers),
!!   filenames longer than 8192 characters will cause the function to fail with hdferr = -1.
!! \endparblock
!!
!! \note
!! \parblock
!!  This function will not be accessible if support for the subfiling VFD is unavailable or disabled.
!! \endparblock
!!
!! \note
!! \parblock
!! **Optimized Allocation**: The function uses knowledge of the subfiling filename template
!! to estimate optimal string lengths, typically reducing memory usage compared to the
!! maximum 8192 character limit. Subfile names follow the pattern:
!! `basename.subfile_<inode>_<index>_of_<total>`
!! \endparblock
!!
!! \par Example Usage:
!! \code{.f90}
!! USE HDF5
!! IMPLICIT NONE
!!
!! INTEGER(HID_T) :: file_id
!! CHARACTER(LEN=:), ALLOCATABLE, DIMENSION(:) :: subfile_names
!! INTEGER(SIZE_T) :: num_subfiles
!! INTEGER :: hdferr
!! INTEGER :: i
!!
!! ! Open file with subfiling VFD (file_id setup not shown)
!!
!! ! Get subfile mapping
!! CALL h5fdsubfiling_get_file_mapping_f(file_id, subfile_names, num_subfiles, hdferr)
!!
!! IF (hdferr == 0) THEN
!!   PRINT *, 'Found', num_subfiles, 'subfiles:'
!!   DO i = 1, num_subfiles
!!     PRINT *, '  ', TRIM(subfile_names(i))
!!   END DO
!!
!!   ! Clean up
!!   DEALLOCATE(subfile_names)
!! ELSE
!!   PRINT *, 'Error getting file mapping:', hdferr
!! END IF
!! \endcode
!!
!! \see H5FDsubfiling_get_file_mapping() (C API)
!! \see H5Pset_fapl_subfiling_f() for setting up subfiling VFD
!!
SUBROUTINE h5fdsubfiling_get_file_mapping_f(file_id, filenames, num_files, hdferr)
  IMPLICIT NONE
  INTEGER(HID_T), INTENT(IN) :: file_id
#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
  CHARACTER(LEN=:), ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: filenames
#else
  INTEGER, PARAMETER :: DEFAULT_MAX_LEN = 8192
  CHARACTER(LEN=DEFAULT_MAX_LEN), ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: filenames
#endif
  INTEGER(SIZE_T), INTENT(OUT) :: num_files
  INTEGER, INTENT(OUT) :: hdferr

  TYPE(C_PTR) :: filenames_ptr
  INTEGER(C_SIZE_T) :: c_num_files
  INTEGER(C_INT) :: ret_val
  INTEGER(SIZE_T) :: i, str_len
  TYPE(C_PTR), POINTER, DIMENSION(:) :: c_filename_ptrs
  INTEGER :: max_len
#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
  CHARACTER(LEN=:), ALLOCATABLE :: temp_filenames(:)
  INTEGER(SIZE_T) :: k
#endif
  INTERFACE
     INTEGER(C_INT) FUNCTION h5fdsubfiling_get_file_mapping(file_id, filenames_ptr, len) &
          BIND(C, NAME='H5FDsubfiling_get_file_mapping')
       IMPORT :: C_INT, HID_T, C_PTR, C_SIZE_T
       IMPLICIT NONE
       INTEGER(HID_T), VALUE :: file_id
       TYPE(C_PTR) :: filenames_ptr
       INTEGER(C_SIZE_T) :: len
     END FUNCTION h5fdsubfiling_get_file_mapping
  END INTERFACE

  hdferr = INT(h5fdsubfiling_get_file_mapping(file_id, filenames_ptr, c_num_files))
  num_files = INT(c_num_files, SIZE_T)

  IF (hdferr .NE. 0 .OR. num_files .EQ. 0) THEN
    ! Allocate empty array on error or no files
#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
    ALLOCATE(CHARACTER(LEN=0) :: filenames(0))
#else
    ALLOCATE(filenames(0))
#endif
    num_files = 0_SIZE_T
    RETURN
  END IF

  ! Convert the C char** array to Fortran character array
  CALL C_F_POINTER(filenames_ptr, c_filename_ptrs, [num_files])

  ! Allocate with a reasonable default - will expand if needed
#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
  ALLOCATE(CHARACTER(LEN=1024) :: filenames(num_files))
#else
  ALLOCATE(filenames(num_files))
#endif

  max_len = 0

  ! determine lengths and copy strings
  DO i = 1, num_files
    IF (C_ASSOCIATED(c_filename_ptrs(i))) THEN
      BLOCK
        CHARACTER(KIND=C_CHAR), POINTER :: c_string(:)
        INTEGER :: current_size, j
        INTEGER, PARAMETER :: INITIAL_SIZE = 1024

        current_size = INITIAL_SIZE
        DO
          CALL C_F_POINTER(c_filename_ptrs(i), c_string, [current_size])

          ! Find string length by searching for null terminator
          str_len = 0
          DO WHILE (str_len < current_size .AND. c_string(str_len + 1) /= C_NULL_CHAR)
            str_len = str_len + 1
          END DO

          ! If we found the null terminator, we're done
          IF (str_len < current_size) EXIT

          ! Otherwise, double the size and try again
          current_size = current_size * 2
          IF (current_size > 65536) THEN
            ! Sanity check - if path is longer than 64K, something is wrong
            hdferr = -1
            RETURN
          END IF
        END DO

        max_len = MAX(max_len, INT(str_len))

#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
        ! Reallocate if this string is longer than current allocation
        IF (INT(str_len) > LEN(filenames(1))) THEN
          ALLOCATE(CHARACTER(LEN=INT(str_len)) :: temp_filenames(num_files))
          DO k = 1, i-1
            temp_filenames(k) = filenames(k)
          END DO
          DEALLOCATE(filenames)
          ALLOCATE(CHARACTER(LEN=INT(str_len)) :: filenames(num_files))
          DO k = 1, i-1
            filenames(k) = temp_filenames(k)
          END DO
          DEALLOCATE(temp_filenames)
        END IF
#else
        ! Check if string exceeds our fixed buffer
        IF (INT(str_len) > DEFAULT_MAX_LEN) THEN
          hdferr = -1
          RETURN
        END IF
#endif

        ! Copy the string
        filenames(i) = ""
        DO j = 1, INT(str_len)
          filenames(i)(j:j) = c_string(j)
        END DO
      END BLOCK
    ELSE
      filenames(i) = ""
    END IF
  END DO

  ! Free the C memory allocated by H5FDsubfiling_get_file_mapping
  ret_val = h5free_string_array_memory_c(filenames_ptr, c_num_files)
  ! Note: We ignore the return value of the free function since the main operation succeeded

END SUBROUTINE h5fdsubfiling_get_file_mapping_f

#endif

END MODULE H5FD
