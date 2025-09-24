!> @defgroup FH5VFD Fortran VFD (H5VFD) Interface
!!
!! @see H5FD, C-API
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

MODULE H5VFD

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
  ! Use a larger buffer to accommodate full paths plus subfile suffix
  ! PATH_MAX (4096) + subfile template overhead (~64) + safety margin (448) = 4608
  ! Round up to 8192 for robust handling of long paths and future-proofing
  INTEGER, PARAMETER :: MAX_FILENAME_LEN = 8192
  INTEGER(HID_T), INTENT(IN) :: file_id
#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
  CHARACTER(LEN=:), ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: filenames
#else
  CHARACTER(LEN=MAX_FILENAME_LEN), ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: filenames
#endif
  INTEGER(SIZE_T), INTENT(OUT) :: num_files
  INTEGER, INTENT(OUT) :: hdferr

  TYPE(C_PTR) :: filenames_ptr
  INTEGER(C_SIZE_T) :: c_num_files
  INTEGER(C_INT) :: ret_val
  INTEGER(SIZE_T) :: i
  TYPE(C_PTR), POINTER, DIMENSION(:) :: c_filename_ptrs
  CHARACTER(KIND=C_CHAR), POINTER, DIMENSION(:) :: c_string
  INTEGER :: max_len = 0, str_len, estimated_max_len, search_limit
  INTEGER, PARAMETER :: SUBFILE_TEMPLATE_OVERHEAD = 64  ! ".subfile_" + inode + "_XX_of_XX" + safety margin
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

  ! Estimate maximum subfile name length based on subfiling template:
  ! Template: "%s.subfile_%" PRIu64 "_%0*d_of_%d"
  ! Components: base_filename + ".subfile_" + inode(20 digits) + "_" + subfile_index + "_of_" + total_subfiles
  ! Conservative estimate: assume original filename could be up to PATH_MAX/2, then add overhead
  estimated_max_len = MIN(MAX_FILENAME_LEN, 2048 + SUBFILE_TEMPLATE_OVERHEAD + INT(LOG10(REAL(MAX(1, INT(num_files))))) * 2)

  ! First pass: determine actual maximum string length (but limit search to reasonable bounds)
  DO i = 1, num_files
    IF (C_ASSOCIATED(c_filename_ptrs(i))) THEN
      ! Find length by searching for null terminator with optimized bound
      ! Use estimated length for first check, fall back to full buffer if needed
      search_limit = MIN(MAX_FILENAME_LEN, estimated_max_len + 256)
      CALL C_F_POINTER(c_filename_ptrs(i), c_string, [MAX_FILENAME_LEN])
      str_len = 0
      DO WHILE (str_len < search_limit .AND. c_string(str_len + 1) .NE. C_NULL_CHAR)
        str_len = str_len + 1
      END DO

      ! If we hit the search limit but not the absolute limit, extend search
      IF (str_len == search_limit .AND. search_limit < MAX_FILENAME_LEN .AND. c_string(str_len + 1) .NE. C_NULL_CHAR) THEN
        DO WHILE (str_len < MAX_FILENAME_LEN .AND. c_string(str_len + 1) .NE. C_NULL_CHAR)
          str_len = str_len + 1
        END DO
      END IF

      ! Check if we hit the absolute maximum without finding null terminator
      IF (str_len == MAX_FILENAME_LEN .AND. c_string(MAX_FILENAME_LEN) .NE. C_NULL_CHAR) THEN
        hdferr = -1
#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
        ALLOCATE(CHARACTER(LEN=0) :: filenames(0))
#else
        ALLOCATE(filenames(0))
#endif
        num_files = 0_SIZE_T
        RETURN
      END IF

      max_len = MAX(max_len, str_len)
    END IF
  END DO

  ! Allocate the output array with optimized length
  ! Use the actual max_len if determined, otherwise fall back to estimated length
#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
  IF (max_len > 0) THEN
    ALLOCATE(CHARACTER(LEN=max_len) :: filenames(num_files))
  ELSE
    ! Fallback to estimated length if no strings were found
    ALLOCATE(CHARACTER(LEN=estimated_max_len) :: filenames(num_files))
  END IF
#else
  ALLOCATE(filenames(num_files))
#endif

  ! Second pass: copy the strings
  DO i = 1, num_files
    IF (C_ASSOCIATED(c_filename_ptrs(i))) THEN
      CALL C_F_POINTER(c_filename_ptrs(i), c_string, [MAX_FILENAME_LEN])
      str_len = 0
      DO WHILE (str_len < MAX_FILENAME_LEN .AND. c_string(str_len + 1) .NE. C_NULL_CHAR)
        str_len = str_len + 1
      END DO

      ! Double-check for buffer overflow (should not happen if first pass was correct)
      IF (str_len == MAX_FILENAME_LEN .AND. c_string(MAX_FILENAME_LEN) .NE. C_NULL_CHAR) THEN
        hdferr = -1
        DEALLOCATE(filenames)
#ifdef H5_FORTRAN_HAVE_CHAR_ALLOC
        ALLOCATE(CHARACTER(LEN=0) :: filenames(0))
#else
        ALLOCATE(filenames(0))
#endif
        num_files = 0_SIZE_T
        RETURN
      END IF

      filenames(i) = TRANSFER(c_string(1:str_len), filenames(i)(1:str_len))
    ELSE
      filenames(i) = ""
    END IF
  END DO

  ! Free the C memory allocated by H5FDsubfiling_get_file_mapping
  ret_val = h5free_string_array_memory_c(filenames_ptr, c_num_files)
  ! Note: We ignore the return value of the free function since the main operation succeeded

END SUBROUTINE h5fdsubfiling_get_file_mapping_f

#endif

END MODULE H5VFD
