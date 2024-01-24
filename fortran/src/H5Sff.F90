!> @defgroup FH5S Fortran Dataspace (H5S) Interface
!!
!! @see H5S, C-API
!!
!! @see @ref H5S_UG, User Guide
!!

!> @ingroup FH5S
!!
!! @brief This module contains Fortran interfaces for H5S functions.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
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
!  If you add a new H5S function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5S

  USE H5GLOBAL
  IMPLICIT NONE

CONTAINS
!>
!! \ingroup FH5S
!!
!! \brief Creates a new simple data space and opens it for access.
!!
!! \param rank     Number of dimensions.
!! \param dims     An array of the size of each dimension.
!! \param space_id Dataspace identifier.
!! \param hdferr   \fortran_error
!! \param maxdims  An array of the maximum size of each dimension.
!!
!! See C API: @ref H5Screate_simple()
!!
  SUBROUTINE h5screate_simple_f(rank, dims, space_id, hdferr, maxdims)

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: rank
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(1:rank) :: dims
    INTEGER(HID_T), INTENT(OUT) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HSIZE_T), INTENT(IN), OPTIONAL, DIMENSION(1:rank) :: maxdims
    INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: f_maxdims

    INTERFACE
       INTEGER FUNCTION h5screate_simple_c(rank, dims, maxdims, space_id) BIND(C,NAME='h5screate_simple_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: rank
         INTEGER(HSIZE_T), INTENT(IN) :: dims(rank)
         INTEGER(HSIZE_T), INTENT(IN) :: maxdims(rank)
         INTEGER(HID_T), INTENT(OUT) :: space_id
       END FUNCTION h5screate_simple_c
    END INTERFACE

    ALLOCATE (f_maxdims(rank), stat=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF
    IF (PRESENT(maxdims)) THEN
       f_maxdims(1:rank) = maxdims(1:rank)
    ELSE
       f_maxdims(1:rank) = dims(1:rank)
    ENDIF
    hdferr = h5screate_simple_c(rank, dims, f_maxdims, space_id)
    DEALLOCATE(f_maxdims)

  END SUBROUTINE h5screate_simple_f

!>
!! \ingroup FH5S
!!
!! \brief Releases and terminates access to a dataspace.
!!
!! \param space_id Identifier of dataspace to release.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sclose()
!!
  SUBROUTINE h5sclose_f(space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sclose_c(space_id) BIND(C,NAME='h5sclose_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
       END FUNCTION h5sclose_c
    END INTERFACE

    hdferr = h5sclose_c(space_id)

  END SUBROUTINE h5sclose_f

!>
!! \ingroup FH5S
!!
!! \brief Creates a new dataspace of a specified type.
!!
!! \param classtype The type of the dataspace to be created. Possible values are:
!!                  \li H5S_SCALAR_F
!!                  \li H5S_SIMPLE_F
!!                  \li H5S_NULL_F
!! \param space_id  Dataspace identifier.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Screate()
!!
  SUBROUTINE h5screate_f(classtype, space_id, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: classtype
    INTEGER(HID_T), INTENT(OUT) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5screate_c(classtype, space_id) BIND(C,NAME='h5screate_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: classtype
         INTEGER(HID_T), INTENT(OUT) :: space_id
       END FUNCTION h5screate_c
    END INTERFACE

    hdferr = h5screate_c(classtype, space_id)

  END SUBROUTINE h5screate_f

!>
!! \ingroup FH5S
!!
!! \brief Creates an exact copy of a dataspace.
!!
!! \param space_id     Dataspace identifier.
!! \param new_space_id Identifier of dataspace&apos;s copy.
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Scopy()
!!
  SUBROUTINE h5scopy_f(space_id, new_space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HID_T), INTENT(OUT) :: new_space_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5scopy_c(space_id, new_space_id) BIND(C,NAME='h5scopy_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HID_T), INTENT(OUT):: new_space_id
       END FUNCTION h5scopy_c
    END INTERFACE

    hdferr = h5scopy_c(space_id, new_space_id)

  END SUBROUTINE h5scopy_f

!>
!! \ingroup FH5S
!!
!! \brief Get number of hyperslab blocks.
!!
!! \param space_id   Dataspace identifier.
!! \param num_blocks Number of hyperslab blocks in the current hyperslab selection.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Sget_select_hyper_nblocks()
!!
  SUBROUTINE h5sget_select_hyper_nblocks_f(space_id, num_blocks, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSSIZE_T), INTENT(OUT) :: num_blocks
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sget_select_hyper_nblocks_c (space_id, num_blocks) &
            BIND(C,NAME='h5sget_select_hyper_nblocks_c')
         IMPORT :: HID_T, HSSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HSSIZE_T), INTENT(OUT) :: num_blocks
       END FUNCTION h5sget_select_hyper_nblocks_c
    END INTERFACE

    hdferr =  h5sget_select_hyper_nblocks_c (space_id, num_blocks)

  END SUBROUTINE h5sget_select_hyper_nblocks_f

!>
!! \ingroup FH5S
!!
!! \brief Gets the list of hyperslab blocks currently selected.
!!
!! \param space_id   Dataspace identifier.
!! \param startblock Hyperslab block to start with.
!! \param num_blocks Number of blocks to get.
!! \param buf        Buffer to hold block list.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Sget_select_hyper_blocklist()
!!
  SUBROUTINE h5sget_select_hyper_blocklist_f(space_id, startblock, &
                                                    num_blocks, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSIZE_T), INTENT(IN) :: startblock
    INTEGER(HSIZE_T), INTENT(IN) :: num_blocks
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sget_select_hyper_blocklist_c(space_id, startblock, &
            num_blocks, buf ) BIND(C,NAME='h5sget_select_hyper_blocklist_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HSIZE_T), INTENT(IN) :: startblock
         INTEGER(HSIZE_T), INTENT(IN) :: num_blocks
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf
       END FUNCTION h5sget_select_hyper_blocklist_c
    END INTERFACE

    hdferr =  h5sget_select_hyper_blocklist_c(space_id, startblock, num_blocks, buf )

  END SUBROUTINE h5sget_select_hyper_blocklist_f

!>
!! \ingroup FH5S
!!
!! \brief Gets the bounding box containing the current selection.
!!
!! \param space_id Dataspace identifier.
!! \param start    Starting coordinates of bounding box.
!! \param end      Ending coordinates of bounding box, i.e., the coordinates of the diagonally opposite corner.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sget_select_bounds()
!!
  SUBROUTINE  h5sget_select_bounds_f(space_id, start, END, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: start
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: END
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sget_select_bounds_c(space_id, start, end) &
            BIND(C,NAME='h5sget_select_bounds_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: start
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: END
       END FUNCTION h5sget_select_bounds_c
    END INTERFACE

    hdferr =   h5sget_select_bounds_c(space_id, start, END)

  END SUBROUTINE h5sget_select_bounds_f

!>
!! \ingroup FH5S
!!
!! \brief Gets the number of element points in the current selection
!!
!! \param space_id   Dataspace identifier.
!! \param num_points Number of element points in the current dataspace selection
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Sget_select_elem_npoints()
!!
  SUBROUTINE h5sget_select_elem_npoints_f(space_id, num_points, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSSIZE_T), INTENT(OUT) :: num_points
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sget_select_elem_npoints_c (space_id, num_points) BIND(C,NAME='h5sget_select_elem_npoints_c')
         IMPORT :: HID_T, HSSIZE_T
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HSSIZE_T), INTENT(OUT) :: num_points
       END FUNCTION h5sget_select_elem_npoints_c
    END INTERFACE

    hdferr =  h5sget_select_elem_npoints_c (space_id, num_points)

  END SUBROUTINE h5sget_select_elem_npoints_f

!>
!! \ingroup FH5S
!!
!! \brief Gets the list of element points currently selected.
!!
!! \param space_id   Dataspace identifier.
!! \param startpoint Element point to start with.
!! \param num_points Number of element points to get.
!! \param buf        Buffer with element points selected.
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Sget_select_elem_pointlist()
!!
  SUBROUTINE h5sget_select_elem_pointlist_f(space_id, startpoint, &
       num_points, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSIZE_T), INTENT(IN) :: startpoint
    INTEGER(HSIZE_T), INTENT(IN) :: num_points
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sget_select_elem_pointlist_c(space_id, startpoint, &
            num_points, buf ) BIND(C,NAME='h5sget_select_elem_pointlist_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HSIZE_T), INTENT(IN) :: startpoint
         INTEGER(HSIZE_T), INTENT(IN) :: num_points
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf
       END FUNCTION h5sget_select_elem_pointlist_c
    END INTERFACE

    hdferr =  h5sget_select_elem_pointlist_c(space_id, startpoint, &
         num_points, buf )

  END SUBROUTINE h5sget_select_elem_pointlist_f

!>
!! \ingroup FH5S
!!
!! \brief Selects elements to be included in the selection for a dataspace
!!
!! \param space_id     Dataspace identifier.
!! \param operator     Flag, valid values are:
!!                     \li H5S_SELECT_SET_F
!!                     \li H5S_SELECT_APPEND_F
!!                     \li H5S_SELECT_PREPEND_F
!! \param rank         Number of dataspace dimensions.
!! \param num_elements Number of elements to be selected.
!! \param coord        2D (rank x num_elements) array with the elements coordinates ( 1-based); in C the
!!                     array is stored in 2D as (num_element x rank).
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Sselect_elements()
!!
  SUBROUTINE h5sselect_elements_f(space_id, OPERATOR, rank, &
       num_elements, coord, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T),   INTENT(IN)                                :: space_id
    INTEGER,          INTENT(IN)                                :: OPERATOR
    INTEGER,          INTENT(IN)                                :: rank
    INTEGER(SIZE_T),  INTENT(IN)                                :: num_elements
    INTEGER(HSIZE_T), INTENT(IN) , DIMENSION(rank,num_elements) :: coord
    INTEGER,          INTENT(OUT)                               :: hdferr
    INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:,:) :: c_coord
    INTEGER :: error, i

    INTERFACE
       INTEGER FUNCTION h5sselect_elements_c(space_id, OPERATOR,&
            num_elements,c_c_coord) BIND(C,NAME='h5sselect_elements_c')
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER, INTENT(IN) :: OPERATOR
         INTEGER(SIZE_T), INTENT(IN) :: num_elements
         INTEGER(HSIZE_T),DIMENSION(*) :: c_c_coord
       END FUNCTION h5sselect_elements_c
    END INTERFACE

    ALLOCATE(c_coord(rank,num_elements), STAT = error)
    IF (error.NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF
    DO i = 1, rank
       c_coord(i,:) = coord(rank-i+1, :) - 1
    ENDDO
    hdferr = h5sselect_elements_c(space_id, OPERATOR, num_elements, c_coord)

!  ALLOCATE(c_coord(num_elements,rank), stat = error)
!  IF (error.NE. 0) THEN
!  hdferr = -1
!  RETURN
!  ENDIF
!
!  c_coord = TRANSPOSE(coord)
!  hdferr = h5sselect_elements_c(space_id, OPERATOR, INT(rank,size_t), c_coord)


    DEALLOCATE(c_coord)

  END SUBROUTINE h5sselect_elements_f

!>
!! \ingroup FH5S
!!
!! \brief Selects the entire dataspace.
!!
!! \param space_id Identifier for the dataspace in which selection being made.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sselect_all()
!!
  SUBROUTINE h5sselect_all_f(space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sselect_all_c(space_id) BIND(C,NAME='h5sselect_all_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
       END FUNCTION h5sselect_all_c
    END INTERFACE

    hdferr = h5sselect_all_c(space_id)

  END SUBROUTINE h5sselect_all_f

!>
!! \ingroup FH5S
!!
!! \brief Checks if two selections are the same shape.
!!
!! \param space1_id Dataspace identifier
!! \param space2_id Dataspace identifier
!! \param same      Value of check
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Sselect_shape_same()
!!
  SUBROUTINE H5Sselect_shape_same_f(space1_id, space2_id, same, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: space1_id
    INTEGER(HID_T), INTENT(IN)  :: space2_id
    LOGICAL       , INTENT(OUT) :: same
    INTEGER       , INTENT(OUT) :: hdferr

    INTEGER(C_INT) :: c_same

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Sselect_shape_same(space1_id, space2_id) BIND(C,NAME='H5Sselect_shape_same')
         IMPORT :: C_INT, HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: space1_id
         INTEGER(HID_T), VALUE :: space2_id
       END FUNCTION H5Sselect_shape_same
    END INTERFACE

    c_same = H5Sselect_shape_same(space1_id, space2_id)

    same = .FALSE.
    IF(c_same .GT. 0_C_INT) same = .TRUE.

    hdferr = 0
    IF(c_same .LT. 0_C_INT) hdferr = -1

  END SUBROUTINE H5Sselect_shape_same_f

!>
!! \ingroup FH5S
!!
!! \brief Checks if current selection intersects with a block.
!!
!! \param space_id   Dataspace identifier
!! \param istart     Starting coordinate of the block
!! \param iend	     Opposite ("ending") coordinate of the block
!! \param intersects Dataspace intersects with the block specified
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Sselect_intersect_block()
!!

  SUBROUTINE H5Sselect_intersect_block_f(space_id, istart, iend, intersects, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)  ,               INTENT(IN)  :: space_id
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: istart
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: iend
    LOGICAL, INTENT(OUT) :: intersects
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(C_INT) :: c_intersects

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Sselect_intersect_block(space_id, istart, iend) &
            BIND(C,NAME='H5Sselect_intersect_block')
         IMPORT :: C_INT, HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE  :: space_id
         INTEGER(HSIZE_T), DIMENSION(*) :: istart
         INTEGER(HSIZE_T), DIMENSION(*) :: iend
       END FUNCTION H5Sselect_intersect_block
    END INTERFACE

    c_intersects = H5Sselect_intersect_block(space_id, istart, iend)

    intersects = .FALSE.
    IF(c_intersects .GT. 0_C_INT) intersects = .TRUE.

    hdferr = 0
    IF(c_intersects .LT. 0_C_INT) hdferr = -1

  END SUBROUTINE H5Sselect_intersect_block_f

!>
!! \ingroup FH5S
!!
!! \brief Resets the selection region to include no elements.
!!
!! \param space_id The identifier for the dataspace in which the selection is being reset.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sselect_none()
!!
  SUBROUTINE h5sselect_none_f(space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sselect_none_c(space_id) BIND(C,NAME='h5sselect_none_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
       END FUNCTION h5sselect_none_c
    END INTERFACE

    hdferr = h5sselect_none_c(space_id)

  END SUBROUTINE h5sselect_none_f

!>
!! \ingroup FH5S
!!
!! \brief Verifies that the selection is within the extent of the dataspace.
!!
!! \param space_id Identifier for the dataspace for which selection is verified
!! \param status   TRUE if the selection is contained within the extent, FALSE otherwise.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sselect_valid()
!!
  SUBROUTINE h5sselect_valid_f(space_id, status, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    LOGICAL, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: flag ! "TRUE/FALSE/ERROR" flag from C routine

    INTERFACE
       INTEGER FUNCTION h5sselect_valid_c(space_id, flag) BIND(C,NAME='h5sselect_valid_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER :: flag
       END FUNCTION h5sselect_valid_c
    END INTERFACE

    hdferr = h5sselect_valid_c(space_id, flag)
    status = .TRUE.
    IF (flag .EQ. 0) status = .FALSE.

  END SUBROUTINE h5sselect_valid_f

!>
!! \ingroup FH5S
!!
!! \brief Determines the number of elements in a dataspace.
!!
!! \param space_id Dataspace identifier.
!! \param npoints  Number of elements in the dataspace.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sget_simple_extent_npoints()
!!
  SUBROUTINE h5sget_simple_extent_npoints_f(space_id, npoints, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSIZE_T), INTENT(OUT) :: npoints
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sget_simple_extent_npoints_c( space_id, npoints) BIND(C,NAME='h5sget_simple_extent_npoints_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HSIZE_T), INTENT(OUT) :: npoints
       END FUNCTION h5sget_simple_extent_npoints_c
    END INTERFACE

    hdferr = h5sget_simple_extent_npoints_c( space_id, npoints)

  END SUBROUTINE h5sget_simple_extent_npoints_f

!>
!! \ingroup FH5S
!!
!! \brief Determines the number of elements in a dataspace selection.
!!
!! \param space_id Dataspace identifier.
!! \param npoints  Number of points in the dataspace selection.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sget_select_npoints()
!!
  SUBROUTINE h5sget_select_npoints_f(space_id, npoints, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSSIZE_T), INTENT(OUT) :: npoints
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sget_select_npoints_c(space_id, npoints) BIND(C,NAME='h5sget_select_npoints_c')
         IMPORT :: HID_T, HSSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HSSIZE_T), INTENT(OUT) :: npoints
       END FUNCTION h5sget_select_npoints_c
    END INTERFACE

    hdferr = h5sget_select_npoints_c(space_id, npoints)

  END SUBROUTINE h5sget_select_npoints_f

!>
!! \ingroup FH5S
!!
!! \brief Determines the dimensionality of a dataspace
!!
!! \param space_id Dataspace identifier.
!! \param rank     Number of dataspace dimensions.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sget_simple_extent_ndims()
!!
  SUBROUTINE h5sget_simple_extent_ndims_f(space_id, rank, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: rank
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sget_simple_extent_ndims_c(space_id, rank) BIND(C,NAME='h5sget_simple_extent_ndims_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER, INTENT(OUT) :: rank
       END FUNCTION h5sget_simple_extent_ndims_c
    END INTERFACE

    hdferr = h5sget_simple_extent_ndims_c(space_id, rank)

  END SUBROUTINE h5sget_simple_extent_ndims_f
!>
!! \ingroup FH5S
!!
!! \brief Retrieves dataspace dimension size and maximum size.
!!
!! \param space_id Dataspace identifier.
!! \param dims     Array to store size of each dimension.
!! \param maxdims  Array to store maximum size of each dimension.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sget_simple_extent_dims()
!!
  SUBROUTINE h5sget_simple_extent_dims_f(space_id, dims, maxdims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: dims
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: maxdims
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sget_simple_extent_dims_c(space_id, dims, maxdims) BIND(C,NAME='h5sget_simple_extent_dims_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: dims
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: maxdims
       END FUNCTION h5sget_simple_extent_dims_c
    END INTERFACE

    hdferr = h5sget_simple_extent_dims_c(space_id, dims, maxdims)

  END SUBROUTINE h5sget_simple_extent_dims_f

!>
!! \ingroup FH5S
!!
!! \brief Determine the current class of a dataspace
!!
!! \param space_id  Dataspace identifier.
!! \param classtype Class type, possible values are:
!!                  \li H5S_NO_CLASS_F
!!                  \li H5S_SCALAR_F
!!                  \li H5S_SIMPLE_F
!!                  \li H5S_NULL_F
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Sget_simple_extent_type()
!!
  SUBROUTINE h5sget_simple_extent_type_f(space_id, classtype, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: classtype
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sget_simple_extent_type_c(space_id, classtype) BIND(C,NAME='h5sget_simple_extent_type_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER, INTENT(OUT) :: classtype
       END FUNCTION h5sget_simple_extent_type_c
    END INTERFACE

    hdferr = h5sget_simple_extent_type_c(space_id, classtype)

  END SUBROUTINE h5sget_simple_extent_type_f
  !
!>
!! \ingroup FH5S
!!
!! \brief Sets or resets the size of an existing dataspace.
!!
!! \param space_id     Dataspace identifier.
!! \param rank         Dataspace number of dimensions.
!! \param current_size Array with the new sizes of dimensions.
!! \param maximum_size Array with the new maximum sizes of dimensions.
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Sset_extent_simple()
!!
  SUBROUTINE h5sset_extent_simple_f(space_id, rank, current_size, &
       maximum_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(IN) :: rank
    INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: current_size
    INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: maximum_size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sset_extent_simple_c(space_id, rank, &
            current_size, maximum_size) BIND(C,NAME='h5sset_extent_simple_c')
         IMPORT :: HID_T, HSIZE_T
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER, INTENT(IN) :: rank
         INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: current_size
         INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: maximum_size
       END FUNCTION h5sset_extent_simple_c
    END INTERFACE

    hdferr = h5sset_extent_simple_c(space_id, rank, current_size, &
         maximum_size)

  END SUBROUTINE h5sset_extent_simple_f
!>
!! \ingroup FH5S
!!
!! \brief Determines whether a dataspace is a simple dataspace.
!!
!! \param space_id Dataspace identifier.
!! \param status   Flag to indicate if dataspace is simple or not (TRUE or FALSE).
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sis_simple()
!!
  SUBROUTINE h5sis_simple_f(space_id, status, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    LOGICAL, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: flag                     ! "TRUE/FALSE/ERROR from C"

    INTERFACE
       INTEGER FUNCTION h5sis_simple_c(space_id, flag) BIND(C,NAME='h5sis_simple_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER :: flag
       END FUNCTION h5sis_simple_c
    END INTERFACE

    hdferr = h5sis_simple_c(space_id, flag)
    status = .TRUE.
    IF (flag .EQ. 0) status = .FALSE.

  END SUBROUTINE h5sis_simple_f

!>
!! \ingroup FH5S
!!
!! \brief Sets the offset of a simple dataspace.
!!
!! \param space_id Dataspace identifier.
!! \param offset   The offset at which to position the selection.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Soffset_simple()
!!
  SUBROUTINE h5soffset_simple_f(space_id, offset, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSSIZE_T), DIMENSION(*), INTENT(IN) ::  offset
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5soffset_simple_c(space_id, offset) BIND(C,NAME='h5soffset_simple_c')
         IMPORT :: HID_T, HSSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HSSIZE_T), DIMENSION(*), INTENT(IN) ::  offset
       END FUNCTION h5soffset_simple_c
    END INTERFACE

    hdferr = h5soffset_simple_c(space_id, offset)

  END SUBROUTINE h5soffset_simple_f

!>
!! \ingroup FH5S
!!
!! \brief Copies the extent of a dataspace.
!!
!! \param dest_space_id   The identifier for the dataspace to which the extent is copied.
!! \param source_space_id The identifier for the dataspace from which the extent is copied.
!! \param hdferr          \fortran_error
!!
!! See C API: @ref H5Sextent_copy()
!!
  SUBROUTINE h5sextent_copy_f(dest_space_id, source_space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dest_space_id
    INTEGER(HID_T), INTENT(IN) :: source_space_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sextent_copy_c(dest_space_id, source_space_id) BIND(C,NAME='h5sextent_copy_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: dest_space_id
         INTEGER(HID_T), INTENT(IN) :: source_space_id
       END FUNCTION h5sextent_copy_c
    END INTERFACE

    hdferr = h5sextent_copy_c(dest_space_id, source_space_id)

  END SUBROUTINE h5sextent_copy_f

!>
!! \ingroup FH5S
!!
!! \brief Removes the extent from a dataspace.
!!
!! \param space_id Dataspace identifier.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sset_extent_none()
!!
  SUBROUTINE h5sset_extent_none_f(space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sset_extent_none_c(space_id) BIND(C,NAME='h5sset_extent_none_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
       END FUNCTION h5sset_extent_none_c
    END INTERFACE

    hdferr = h5sset_extent_none_c(space_id)

  END SUBROUTINE h5sset_extent_none_f
!>
!! \ingroup FH5S
!!
!! \brief Selects a hyperslab region to add to the current selected
!!       region
!!
!! \param space_id Dataspace identifier.
!! \param operator Flag, valid values are:
!!                 \li H5S_SELECT_SET_F
!!                 \li H5S_SELECT_OR_F
!! \param start    Array with hyperslab offsets, \Bold{0-based indices}.
!! \param count    Number of blocks included in the hyperslab.
!! \param hdferr   \fortran_error
!! \param stride   Array with hyperslab strides.
!! \param block    Array with hyperslab block sizes.
!!
!! See C API: @ref H5Sselect_hyperslab()
!!
  SUBROUTINE h5sselect_hyperslab_f(space_id, OPERATOR, start, count, &
       hdferr, stride, BLOCK)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(IN) :: OPERATOR
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: start
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: count
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HSIZE_T), DIMENSION(:), INTENT(IN), OPTIONAL :: stride
    INTEGER(HSIZE_T), DIMENSION(:), INTENT(IN), OPTIONAL :: BLOCK
    INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: def_block
    INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: def_stride
    INTEGER :: rank
    INTEGER :: error1, error2

    INTERFACE
       INTEGER FUNCTION h5sselect_hyperslab_c(space_id, OPERATOR, &
            start, count, stride, BLOCK) BIND(C,NAME='h5sselect_hyperslab_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER, INTENT(IN) :: OPERATOR
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: start
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: count
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: stride
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: BLOCK
       END FUNCTION h5sselect_hyperslab_c
    END INTERFACE

    IF (PRESENT(stride).AND. PRESENT(BLOCK)) THEN
       hdferr = h5sselect_hyperslab_c(space_id, OPERATOR, start, count, &
            stride, BLOCK)
       RETURN
    ENDIF
    ! Case of optional parameters.
    !
    ! Find the rank of the dataspace to allocate memory for
    ! default stride and block arrays.
    !
    CALL h5sget_simple_extent_ndims_f(space_id, rank, hdferr)
    IF( hdferr .EQ. -1) RETURN
    !
    IF (PRESENT(stride).AND. .NOT.PRESENT(BLOCK)) THEN
       ALLOCATE(def_block(rank), stat=error1)
       IF (error1.NE.0) THEN
          hdferr = -1
          RETURN
       ENDIF
       def_block = 1
       hdferr = h5sselect_hyperslab_c(space_id, OPERATOR, start, count, &
            stride, def_block)
       DEALLOCATE(def_block)
       RETURN
    ENDIF

    IF (.NOT.PRESENT(stride).AND. PRESENT(BLOCK)) THEN
       ALLOCATE(def_stride(rank), stat=error2)
       IF (error2.NE.0) THEN
          hdferr = -1
          RETURN
       ENDIF
       def_stride = 1
       hdferr = h5sselect_hyperslab_c(space_id, OPERATOR, start, count, &
            def_stride, BLOCK)
       DEALLOCATE(def_stride)
       RETURN
    ENDIF
    ALLOCATE(def_block(rank), stat=error1)
    ALLOCATE(def_stride(rank), stat=error2)
    IF ((error1.NE.0) .OR. (error2.NE.0)) THEN
       hdferr = -1
       RETURN
    ENDIF
    def_block = 1
    def_stride = 1
    hdferr = h5sselect_hyperslab_c(space_id, OPERATOR, start, count, &
         def_stride, def_block)
    DEALLOCATE(def_block)
    DEALLOCATE(def_stride)

  END SUBROUTINE h5sselect_hyperslab_f
!
! NAME
!      h5scombine_hyperslab_f
!
! PURPOSE
!      Combine a hyperslab selection with the current
!               selection for a dataspace
!
! INPUTS
!            space_id      - dataspace of selection to use
!            operator      - flag, valid values are:
!                          H5S_SELECT_NOOP_F
!                          H5S_SELECT_SET_F
!                          H5S_SELECT_OR_F
!                          H5S_SELECT_AND_F
!                          H5S_SELECT_XOR_F
!                          H5S_SELECT_NOTB_F
!                          H5S_SELECT_NOTA_F
!                          H5S_SELECT_APPEND_F
!                          H5S_SELECT_PREPEND_F
!            start            - array with hyperslab offsets
!            count            - number of blocks included in the
!                          hyperslab
! OUTPUTS
!               hyper_id        - identifier for the new hyperslab
!            hdferr:            - error code
!                               Success:  0
!                               Failure: -1
! OPTIONAL PARAMETERS
!            stride            - array with hyperslab strides
!            block            - array with hyperslab block sizes
!
! NOTES
! Commented out until 1.6 ? 10/08/2002
!
! SOURCE
!  SUBROUTINE h5scombine_hyperslab_f(space_id, operator, start, count, &
!  hyper_id,  hdferr, stride, block)
!  IMPLICIT NONE
   !  H5S_SELECT_AND_F
   !  H5S_SELECT_XOR_F
   !  H5S_SELECT_NOTB_F
   !  H5S_SELECT_NOTA_F
   !  H5S_SELECT_APPEND_F
   !  H5S_SELECT_PREPEND_F
                                                !

!  INTEGER :: rank
!  INTEGER :: error1, error2

!  INTERFACE
!  INTEGER FUNCTION h5scombine_hyperslab_c(space_id, operator, &
!  start, count, stride, block, hyper_id)
!  USE H5GLOBAL
!  !DEC$IF DEFINED(HDF5F90_WINDOWS)
!  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SCOMBINE_HYPERSLAB_C'::h5scombine_hyperslab_c
!  !DEC$ENDIF


!  if (present(stride).and. present(block)) then
!  hdferr = h5scombine_hyperslab_c(space_id, operator, start, count, &
!  stride, block, hyper_id)
!  return
!  endif
            ! Case of optional parameters.
            !
            ! Find the rank of the dataspace to allocate memory for
            ! default stride and block arrays.
            !
!  CALL h5sget_simple_extent_ndims_f(space_id, rank, hdferr)
!  if( hdferr .EQ. -1) return
            !
!  if (present(stride).and. .not.present(block)) then
!  allocate(def_block(rank), stat=error1)
!  if (error1.NE.0) then
!  hdferr = -1
!  return
!  endif
!  def_block = 1
!  hdferr = h5scombine_hyperslab_c(space_id, operator, start, count, &
!  stride, def_block, hyper_id)
!  deallocate(def_block)
!  return
!  endif

!  if (.not.present(stride).and. present(block)) then
!  allocate(def_stride(rank), stat=error2)
!  if (error2.NE.0) then
!  hdferr = -1
!  return
!  endif
!  def_stride = 1
!  hdferr = h5scombine_hyperslab_c(space_id, operator, start, count, &
!  def_stride, block, hyper_id)
!  deallocate(def_stride)
!  return
!  endif
!  allocate(def_block(rank), stat=error1)
!  allocate(def_stride(rank), stat=error2)
!  if ((error1.NE.0) .OR. (error2.NE.0)) then
!  hdferr = -1
!  return
!  endif
!  def_block = 1
!  def_stride = 1
!  hdferr = h5scombine_hyperslab_c(space_id, operator, start, count, &
!  def_stride, def_block, hyper_id)
!  deallocate(def_block)
!  deallocate(def_stride)

!  END SUBROUTINE h5scombine_hyperslab_f

!
! NAME
!   h5scombine_select_f
!
! PURPOSE
!      Combine two hyperslab selections with an operation
!               and return a dataspace with resulting selection.
!
! INPUTS
!            space1_id      - dataspace of selection to use
!            operator      - flag, valid values are:
!                          H5S_SELECT_NOOP_F
!                          H5S_SELECT_SET_F
!                          H5S_SELECT_OR_F
!                          H5S_SELECT_AND_F
!                          H5S_SELECT_XOR_F
!                          H5S_SELECT_NOTB_F
!                          H5S_SELECT_NOTA_F
!                          H5S_SELECT_APPEND_F
!                          H5S_SELECT_PREPEND_F
!            space2_id      - dataspace of selection to use
! OUTPUTS
!               ds_id           - idataspace identifier with the new selection
!            hdferr:            - error code
!                               Success:  0
!                               Failure: -1
! OPTIONAL PARAMETERS            - NONE
!
! NOTES commented out until 1.6 release(?) 10/08/2002
!
!
! SOURCE
!  SUBROUTINE h5scombine_select_f(space1_id, operator, space2_id, s_id,  hdferr)
!  IMPLICIT NONE

                                    !  H5S_SELECT_AND_F
                                    !  H5S_SELECT_XOR_F
                                    !  H5S_SELECT_NOTB_F
                                    !  H5S_SELECT_NOTA_F
                                    !  H5S_SELECT_APPEND_F
                                    !  H5S_SELECT_PREPEND_F
                                                !

!  space2_id, ds_id)
!  USE H5GLOBAL
!  !DEC$IF DEFINED(HDF5F90_WINDOWS)
!  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SCOMBINE_SELECT_C'::h5scombine_select_c
!  !DEC$ENDIF


!  hdferr = h5scombine_select_c(space1_id, operator, space2_id, &
!  ds_id)
!  return

!  END SUBROUTINE h5scombine_select_f

!
! NAME
!            h5smodify_select_f
!
! PURPOSE
!      Refine a hyperslab selection with an operation
!               using second hyperslab
!
! INPUTS
!            space1_id      - dataspace of selection  to modify
!            operator      - flag, valid values are:
!                          H5S_SELECT_NOOP_F
!                          H5S_SELECT_SET_F
!                          H5S_SELECT_OR_F
!                          H5S_SELECT_AND_F
!                          H5S_SELECT_XOR_F
!                          H5S_SELECT_NOTB_F
!                          H5S_SELECT_NOTA_F
!                          H5S_SELECT_APPEND_F
!                          H5S_SELECT_PREPEND_F
!            space2_id      - dataspace of selection to use
!
! OUTPUTS
!            hdferr:            - error code
!                               Success:  0
!                               Failure: -1
! OPTIONAL PARAMETERS            - NONE
!
! NOTESCommented out until 1.6 release(?) 10/08/2002 EIP
!
!
! SOURCE
!  SUBROUTINE h5smodify_select_f(space1_id, operator, space2_id, &
!  hdferr)
!  IMPLICIT NONE

                                    !  H5S_SELECT_AND_F
                                    !  H5S_SELECT_XOR_F
                                    !  H5S_SELECT_NOTB_F
                                    !  H5S_SELECT_NOTA_F
                                    !  H5S_SELECT_APPEND_F
                                    !  H5S_SELECT_PREPEND_F
 !


!  space2_id)
!  USE H5GLOBAL
!  !DEC$IF DEFINED(HDF5F90_WINDOWS)
!  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SMODIFY_SELECT_C'::h5smodify_select_c
!  !DEC$ENDIF


!  hdferr = h5smodify_select_c(space1_id, operator, space2_id)
!  return

!  END SUBROUTINE h5smodify_select_f

!>
!! \ingroup FH5S
!!
!! \brief Retrieve the type of selection
!!
!! \param space_id Dataspace identifier with selection.
!! \param type     Selection type flag, valid values are:
!!                 \li H5S_SEL_ERROR_F
!!                 \li H5S_SEL_NONE_F
!!                 \li H5S_SEL_POINTS_F
!!                 \li H5S_SEL_HYPERSLABS_F
!!                 \li H5S_SEL_ALL_F
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sget_select_type()
!!
  SUBROUTINE h5sget_select_type_f(space_id, TYPE, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(INOUT) :: space_id
    INTEGER, INTENT(OUT) :: TYPE
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sget_select_type_c(space_id, TYPE) BIND(C,NAME='h5sget_select_type_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER, INTENT(OUT) :: TYPE
       END FUNCTION h5sget_select_type_c
    END INTERFACE

    hdferr = h5sget_select_type_c(space_id, TYPE)
    RETURN

  END SUBROUTINE h5sget_select_type_f

!>
!! \ingroup FH5S
!!
!! \brief Decode a binary object description of data space and return a new object handle.
!!
!! \param buf    Buffer for the data space object to be decoded.
!! \param obj_id Object ID.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Sdecode()
!!
  SUBROUTINE h5sdecode_f(buf, obj_id, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: buf
    INTEGER(HID_T), INTENT(OUT) :: obj_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5sdecode_c(buf, obj_id) BIND(C,NAME='h5sdecode_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: buf
         INTEGER(HID_T), INTENT(OUT) :: obj_id
       END FUNCTION h5sdecode_c
    END INTERFACE

    hdferr = h5sdecode_c(buf, obj_id)

  END SUBROUTINE h5sdecode_f

!>
!! \ingroup FH5S
!!
!! \brief Encode a data space object description into a binary buffer.
!!
!! \param obj_id  Identifier of the object to be encoded.
!! \param buf     Buffer for the object to be encoded into.
!! \param nalloc  The size of the buffer needed.
!! \param hdferr  \fortran_error
!! \param fapl_id File access property list identifier.
!!
!! See C API: @ref H5Sencode2()
!!
  SUBROUTINE h5sencode_f(obj_id, buf, nalloc, hdferr, fapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    CHARACTER(LEN=*), INTENT(OUT) :: buf
    INTEGER(SIZE_T), INTENT(INOUT) :: nalloc
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: fapl_id
    INTEGER(HID_T) :: fapl_id_default

    INTERFACE
       INTEGER FUNCTION h5sencode_c(buf, obj_id, nalloc, fapl_id_default) BIND(C,NAME='h5sencode_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: buf
         INTEGER(SIZE_T), INTENT(INOUT) :: nalloc
         INTEGER(HID_T) :: fapl_id_default
       END FUNCTION h5sencode_c
    END INTERFACE

    fapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(fapl_id)) fapl_id_default = fapl_id

    hdferr = h5sencode_c(buf, obj_id, nalloc, fapl_id_default)

  END SUBROUTINE h5sencode_f

!>
!! \ingroup FH5S
!!
!! \brief Determines whether two dataspace extents are equal.
!!
!! \param space1_id First dataspace identifier.
!! \param space2_id Second dataspace identifier.
!! \param Equal     .TRUE. if equal, .FALSE. if unequal.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Sextent_equal()
!!
  SUBROUTINE h5sextent_equal_f(space1_id, space2_id, equal, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space1_id
    INTEGER(HID_T), INTENT(IN) :: space2_id
    LOGICAL, INTENT(OUT) :: Equal
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T) :: c_equal

    INTERFACE
       INTEGER FUNCTION h5sextent_equal_c(space1_id, space2_id, c_equal) BIND(C,NAME='h5sextent_equal_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space1_id
         INTEGER(HID_T), INTENT(IN) :: space2_id
         INTEGER(HID_T) :: c_equal
       END FUNCTION h5sextent_equal_c
    END INTERFACE

    hdferr = h5sextent_equal_c(space1_id, space2_id, c_equal)
    equal = .FALSE.
    IF(c_equal.GT.0) equal = .TRUE.

  END SUBROUTINE h5sextent_equal_f

!>
!! \ingroup FH5S
!!
!! \brief Retrieves a regular hyperslab selection.
!!
!! \param space_id The identifier of the dataspace.
!! \param start    Offset of the start of the regular hyperslab.
!! \param stride   Stride of the regular hyperslab.
!! \param count    Number of blocks in the regular hyperslab.
!! \param block    Size of a block in the regular hyperslab.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Sget_regular_hyperslab()
!!
  SUBROUTINE h5sget_regular_hyperslab_f(space_id, start, stride, count, block, hdferr)

    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) ::  space_id
    INTEGER(HSIZE_T), INTENT(OUT), DIMENSION(*), TARGET ::  start
    INTEGER(HSIZE_T), INTENT(OUT), DIMENSION(*), TARGET ::  stride
    INTEGER(HSIZE_T), INTENT(OUT), DIMENSION(*), TARGET ::  count
    INTEGER(HSIZE_T), INTENT(OUT), DIMENSION(*), TARGET ::  block
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: start_c, stride_c, count_c, block_c
    INTEGER :: n

    INTERFACE
       INTEGER FUNCTION h5sget_regular_hyperslab(space_id, start, stride, count, block) BIND(C,NAME='H5Sget_regular_hyperslab')
         IMPORT :: HID_T, C_PTR
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: space_id
         TYPE(C_PTR), VALUE :: start, stride, count, block
       END FUNCTION h5sget_regular_hyperslab
    END INTERFACE

    hdferr = 0

    start_c = C_LOC(start(1))
    stride_c = C_LOC(stride(1))
    count_c = C_LOC(count(1))
    block_c = C_LOC(block(1))

    IF(INT(h5sget_regular_hyperslab(space_id, start_c, stride_c, count_c, block_c)).LT.0) hdferr = -1

    ! Reverse the C arrays description values of the hyperslab because
    ! the hyperslab was for a C stored hyperslab

    CALL H5Sget_simple_extent_ndims_f(space_id,n,hdferr)
    IF(hdferr.LT.0.OR.n.EQ.0)THEN
       hdferr=-1
    ELSE
       start(1:n)  = start(n:1:-1)
       stride(1:n) = stride(n:1:-1)
       count(1:n)  = count(n:1:-1)
       block(1:n)  = block(n:1:-1)
    ENDIF

  END SUBROUTINE h5sget_regular_hyperslab_f

!>
!! \ingroup FH5S
!!
!! \brief Retrieves a regular hyperslab selection.
!!
!! \param space_id  The identifier of the dataspace.
!! \param IsRegular TRUE or FALSE for hyperslab selection if successful.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Sis_regular_hyperslab()
!!
  SUBROUTINE h5sis_regular_hyperslab_f(space_id, IsRegular, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) ::  space_id
    LOGICAL :: IsRegular
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(C_INT) :: status

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Sis_regular_hyperslab(space_id) BIND(C,NAME='H5Sis_regular_hyperslab')
         IMPORT :: HID_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: space_id
       END FUNCTION H5Sis_regular_hyperslab
    END INTERFACE

    status = H5Sis_regular_hyperslab(space_id)

    hdferr = 0
    IsRegular = .FALSE.
    IF(status.GT.0)THEN
       IsRegular = .TRUE.
    ELSE IF(status.LT.0)THEN
       hdferr = -1
    ENDIF

  END SUBROUTINE H5Sis_regular_hyperslab_f

!>
!! \ingroup FH5S
!!
!! \brief Closes a dataspace selection iterator.
!!
!! \param sel_iter_id Dataspace selection iterator identifier
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Ssel_iter_close()
!!
  SUBROUTINE h5ssel_iter_close_f(sel_iter_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: sel_iter_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(C_INT) FUNCTION H5Ssel_iter_close(sel_iter_id) &
            BIND(C,NAME='H5Ssel_iter_close')
         IMPORT :: HID_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: sel_iter_id
       END FUNCTION H5Ssel_iter_close
    END INTERFACE

    hdferr = INT(h5ssel_iter_close(sel_iter_id), C_INT)

  END SUBROUTINE h5ssel_iter_close_f

!>
!! \ingroup FH5S
!!
!! \brief Creates a dataspace selection iterator for a dataspace's selection.
!!
!! \param space_id    Dataspace identifier
!! \param elmt_size   Size of element in the selection
!! \param flags       Selection iterator flag, valid values are:
!!                    \li H5S_SEL_ITER_GET_SEQ_LIST_SORTED_F, ref. @ref H5S_SEL_ITER_GET_SEQ_LIST_SORTED
!!                    \li H5S_SEL_ITER_SHARE_WITH_DATASPACE_F, ref. @ref H5S_SEL_ITER_SHARE_WITH_DATASPACE
!! \param ds_iter_id  Dataspace selection iterator identifier
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Ssel_iter_create()
!!
  SUBROUTINE h5ssel_iter_create_f(space_id, elmt_size, flags, ds_iter_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)  :: space_id
    INTEGER(SIZE_T), INTENT(IN)  :: elmt_size
    INTEGER        , INTENT(IN)  :: flags
    INTEGER(HID_T) , INTENT(OUT) :: ds_iter_id
    INTEGER        , INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Ssel_iter_create(space_id, elmt_size, flags) &
            BIND(C,NAME='H5Ssel_iter_create')
         IMPORT :: HID_T, C_INT, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T) , VALUE :: space_id
         INTEGER(SIZE_T), VALUE :: elmt_size
         INTEGER(C_INT) , VALUE :: flags
       END FUNCTION H5Ssel_iter_create
    END INTERFACE

    ds_iter_id = H5Ssel_iter_create(space_id, elmt_size, INT(flags, C_INT))

    hdferr = 0
    IF(ds_iter_id.LT.0) hdferr = -1

  END SUBROUTINE h5ssel_iter_create_f

!>
!! \ingroup FH5S
!!
!! \brief Retrieves a list of offset / length sequences for the elements in an iterator.
!!
!! \param sel_iter_id Dataspace selection iterator identifier
!! \param maxseq Maximum number of sequences to retrieve
!! \param maxbytes Maximum number of bytes to retrieve in sequences
!! \param nseq Number of sequences retrieved
!! \param nbytes Number of bytes retrieved, in all sequences
!! \param off Array of sequence offsets
!! \param len Array of sequence lengths
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Ssel_iter_get_seq_list()
!!
  SUBROUTINE h5ssel_iter_get_seq_list_f(sel_iter_id, maxseq, maxbytes, nseq, nbytes, off, len, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN) :: sel_iter_id
    INTEGER(SIZE_T), INTENT(IN) :: maxseq
    INTEGER(SIZE_T), INTENT(IN) :: maxbytes
    INTEGER(SIZE_T), INTENT(OUT), TARGET :: nseq
    INTEGER(SIZE_T), INTENT(OUT), TARGET :: nbytes
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: off
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: len
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Ssel_iter_get_seq_list(sel_iter_id, maxseq, maxbytes, nseq, nbytes, off, len) &
                      BIND(C,NAME='H5Ssel_iter_get_seq_list')
         IMPORT :: HID_T, C_INT, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T) , VALUE :: sel_iter_id
         INTEGER(SIZE_T), VALUE :: maxseq
         INTEGER(SIZE_T), VALUE :: maxbytes
         INTEGER(SIZE_T) :: nseq
         INTEGER(SIZE_T) :: nbytes
         INTEGER(HSIZE_T), DIMENSION(*) :: off
         INTEGER(HSIZE_T), DIMENSION(*) :: len
       END FUNCTION H5Ssel_iter_get_seq_list
    END INTERFACE

    hdferr = INT(H5Ssel_iter_get_seq_list(sel_iter_id, maxseq, maxbytes, nseq, nbytes, off, len), C_INT)

  END SUBROUTINE h5ssel_iter_get_seq_list_f

!>
!! \ingroup FH5S
!!
!! \brief Resets a dataspace selection iterator back to an initial state.
!!
!! \param sel_iter_id Identifier of the dataspace selection iterator to reset
!! \param space_id    Identifier of the dataspace with selection to iterate over
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Ssel_iter_reset()
!!
  SUBROUTINE h5ssel_iter_reset_f(sel_iter_id, space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: sel_iter_id
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(C_INT) FUNCTION H5Ssel_iter_reset(sel_iter_id, space_id) &
            BIND(C,NAME='H5Ssel_iter_reset')
         IMPORT :: HID_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: sel_iter_id
         INTEGER(HID_T), VALUE :: space_id
       END FUNCTION H5Ssel_iter_reset
    END INTERFACE

    hdferr = INT(h5ssel_iter_reset(sel_iter_id, space_id), C_INT)

  END SUBROUTINE h5ssel_iter_reset_f


END MODULE H5S
