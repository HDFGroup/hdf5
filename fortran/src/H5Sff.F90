!****h* ROBODoc/H5S
!
! NAME
!  MODULE H5S
!
! FILE
!  fortran/src/H5Sff.F90
!
! PURPOSE
!  This file contains Fortran interfaces for H5S functions.
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
!*****

MODULE H5S
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_CHAR, C_INT
  USE H5GLOBAL

CONTAINS
!
!****s* H5S/h5screate_simple_f
!
! NAME
!  h5screate_simple_f
!
! PURPOSE 	
!  Creates a new simple data space and opens it for access	.
!
! INPUTS
!  rank        - number of dimensions
!  dims        - an array of the size of each dimension
! OUTPUTS
!  space_id    - dataspace identifier
!  hdferr      - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  maxdims     - an array of the maximum size of each dimension
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
! SOURCE
  SUBROUTINE h5screate_simple_f(rank, dims, space_id, hdferr, maxdims)

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: rank
    INTEGER(HSIZE_T), INTENT(IN) :: dims(rank)
    INTEGER(HID_T), INTENT(OUT) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HSIZE_T), OPTIONAL, INTENT(IN) :: maxdims(rank)
!*****
    INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: f_maxdims

    INTERFACE
       INTEGER FUNCTION h5screate_simple_c(rank, dims, maxdims, space_id) BIND(C,NAME='h5screate_simple_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: rank
         INTEGER(HSIZE_T), INTENT(IN) :: dims(rank)
         INTEGER(HSIZE_T), DIMENSION(:),INTENT(IN) :: maxdims(rank)
         INTEGER(HID_T), INTENT(OUT) :: space_id
       END FUNCTION h5screate_simple_c
    END INTERFACE

    ALLOCATE (f_maxdims(rank), stat=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF
    IF (PRESENT(maxdims)) THEN
       f_maxdims = maxdims
    ELSE
       f_maxdims = dims
    ENDIF
    hdferr = h5screate_simple_c(rank, dims, f_maxdims, space_id)
    DEALLOCATE(f_maxdims)
    
  END SUBROUTINE h5screate_simple_f

!
!****s* H5S/h5sclose_f
!
! NAME
!  h5sclose_f
!
! PURPOSE
!  Releases and terminates access to a dataspace.
!
! INPUTS
!  space_id    - identifier of dataspace to release
! OUTPUTS
!  hdferr      - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sclose_f(space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5sclose_c(space_id) BIND(C,NAME='h5sclose_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
       END FUNCTION h5sclose_c
    END INTERFACE
    
    hdferr = h5sclose_c(space_id)
    
  END SUBROUTINE h5sclose_f

!
!****s* H5S/h5screate_f
!
! NAME
!  h5screate_f
!
! PURPOSE
!  Creates a new dataspace of a specified type.
!
! INPUTS
!  classtype   - The type of the dataspace to be created
!                Possible values are:
!                     H5S_SCALAR_F
!                     H5S_SIMPLE_F
!                     H5S_NULL_F
! OUTPUTS
!  space_id    - Dataspace identifier
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! NOTES
!

! SOURCE
  SUBROUTINE h5screate_f(classtype, space_id, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: classtype
    INTEGER(HID_T), INTENT(OUT) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5scopy_f
!
! NAME
!  h5scopy_f
!
! PURPOSE
!  Creates an exact copy of a dataspace.
!
! INPUTS
!  space_id 	 - dataspace identifier
! OUTPUTS
!  new_space_id  - identifier of dataspace's copy
!  hdferr        - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! NOTES
!

! SOURCE
  SUBROUTINE h5scopy_f(space_id, new_space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HID_T), INTENT(OUT) :: new_space_id
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5sget_select_hyper_nblocks_f
!
! NAME
!  h5sget_select_hyper_nblocks_f
!
! PURPOSE
!  Get number of hyperslab blocks.
!
! INPUTS
!  space_id    - dataspace identifier
! OUTPUTS
!  num_blocks  - number of hyperslab blocks in the current
!                hyperslab selection
!  hdferr      - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sget_select_hyper_nblocks_f(space_id, num_blocks, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSSIZE_T), INTENT(OUT) :: num_blocks
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5sget_select_hyper_blocklist_f
!
! NAME
!  h5sget_select_hyper_blocklist_f
!
! PURPOSE
!  Gets the list of hyperslab blocks currently selected.
!
! INPUTS
!  space_id    - dataspace identifier
!  startblock  - hyperslab block to start with
!  num_blocks  - number of blocks to get
! OUTPUTS
!  buf 	       - buffer to hold block list
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
! SOURCE
  SUBROUTINE h5sget_select_hyper_blocklist_f(space_id, startblock, &
                                                    num_blocks, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id 
    INTEGER(HSIZE_T), INTENT(IN) :: startblock
    INTEGER(HSIZE_T), INTENT(IN) :: num_blocks
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5sget_select_bounds_f
!
! NAME
!  h5sget_select_bounds_f
!
! PURPOSE
!  Gets the bounding box containing the current selection.
!
! INPUTS
!  space_id    - dataspace identifier
!
! OUTPUTS
!  start       - starting coordinates of bounding box
!  end 	       - ending coordinates of bounding box
!                i.e., the coordinates of the diagonally opposite corner
!  hdferr      - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  NONE
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
! SOURCE
  SUBROUTINE  h5sget_select_bounds_f(space_id, start, END, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: start
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: END
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5sget_select_elem_npoints_f
!
! NAME
!  h5sget_select_elem_npoints_f
!
! PURPOSE
!  Gets the number of element points in the current selection
!
! INPUTS
!  space_id 	 - dataspace identifier
! OUTPUTS
!  num_points 	 - number of element points in the current
!                  dataspace selection
!  hdferr        - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sget_select_elem_npoints_f(space_id, num_points, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSSIZE_T), INTENT(OUT) :: num_points
    INTEGER, INTENT(OUT) :: hdferr
!*****
    INTERFACE
       INTEGER FUNCTION h5sget_select_elem_npoints_c (space_id, num_points) BIND(C,NAME='h5sget_select_elem_npoints_c')
         IMPORT :: HID_T, HSSIZE_T
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HSSIZE_T), INTENT(OUT) :: num_points
       END FUNCTION h5sget_select_elem_npoints_c
    END INTERFACE

    hdferr =  h5sget_select_elem_npoints_c (space_id, num_points)
    
  END SUBROUTINE h5sget_select_elem_npoints_f

!
!****s* H5S/h5sget_select_elem_pointlist_f
!
! NAME
!  h5sget_select_elem_pointlist_f
!
! PURPOSE
!  Gets the list of element points currently selected.
!
! INPUTS
!  space_id    - dataspace identifier
!  startpoint  - element point to start with
!  num_points  - number of elemnt points to get
! OUTPUTS
!  buf 	       - buffer with element points selected
!  hdferr      - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sget_select_elem_pointlist_f(space_id, startpoint, &
       num_points, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSIZE_T), INTENT(IN) :: startpoint
    INTEGER(HSIZE_T), INTENT(IN) :: num_points
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5sselect_elements_f
!
! NAME
!  h5sselect_elements_f
!
! PURPOSE
!  Selects elements to be included in the selection for
!  a dataspace
!
! INPUTS
!  space_id 	 - dataspace identifier
!  operator 	 - flag, valid values are:
!                   H5S_SELECT_SET_F 
!                   H5S_SELECT_APPEND_F 
!                   H5S_SELECT_PREPEND_F
!  rank 	 - number of dataspace dimensions
!  num_elements  - number of elements to be selected
!  coord 	 - 2D (rank x num_elements) array with the
!                  elements coordinates ( 1-based); in C the
!                  array is stored in 2D as (num_element x rank)
! OUTPUTS
!  hdferr        - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
! SOURCE
  SUBROUTINE h5sselect_elements_f(space_id, OPERATOR, rank, &
       num_elements, coord, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T),   INTENT(IN)                                :: space_id
    INTEGER,          INTENT(IN)                                :: OPERATOR
    INTEGER,          INTENT(IN)                                :: rank
    INTEGER(SIZE_T),  INTENT(IN)                                :: num_elements
    INTEGER(HSIZE_T), INTENT(IN) , DIMENSION(rank,num_elements) :: coord
    INTEGER,          INTENT(OUT)                               :: hdferr 
!*****
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

!
!****s* H5S/h5sselect_all_f
!
! NAME
!  h5sselect_all_f
!
! PURPOSE
!  Selects the entire dataspace.
!
! INPUTS
!  space_id    - Identifier for the dataspace in which
!                selection being made
! OUTPUTS
!  hdferr      - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sselect_all_f(space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: hdferr 
!*****
    INTERFACE
       INTEGER FUNCTION h5sselect_all_c(space_id) BIND(C,NAME='h5sselect_all_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
       END FUNCTION h5sselect_all_c
    END INTERFACE
    
    hdferr = h5sselect_all_c(space_id)
    
  END SUBROUTINE h5sselect_all_f

!
!****s* H5S/h5sselect_none_f
!
! NAME
!  h5sselect_none_f
!
! PURPOSE
!  Resets the selection region to include no elements.
!
! INPUTS
!  space_id    - the identifier for the dataspace in which
!                the selection is being reset.
! OUTPUTS
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sselect_none_f(space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
!*****
    INTERFACE
       INTEGER FUNCTION h5sselect_none_c(space_id) BIND(C,NAME='h5sselect_none_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
       END FUNCTION h5sselect_none_c
    END INTERFACE
    
    hdferr = h5sselect_none_c(space_id)
    
  END SUBROUTINE h5sselect_none_f

!
!****s* H5S/h5sselect_valid_f
!
! NAME
!  h5sselect_valid_f
!
! PURPOSE
!  Verifies that the selection is within the extent of
!  the dataspace.
!
! INPUTS
!  space_id - identifier for the dataspace for which
!                  selection is verified
! OUTPUTS
!  status   - TRUE if the selection is contained within 
!             the extent, FALSE otherwise. 
!  hdferr   - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sselect_valid_f(space_id, status, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    LOGICAL, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5sget_simple_extent_npoints_f
!
! NAME
!  h5sget_simple_extent_npoints_f
!
! PURPOSE
!  Determines the number of elements in a dataspace.
!
! INPUTS
!  space_id 	 - dataspace identifier
! OUTPUTS
!  npoints 	 - number of elements in the dataspace
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sget_simple_extent_npoints_f(space_id, npoints, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id 
    INTEGER(HSIZE_T), INTENT(OUT) :: npoints
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5sget_select_npoints_f
!
! NAME
!  h5sget_select_npoints_f
!
! PURPOSE
!  Determines the number of elements in a dataspace selection.
!
! INPUTS
!  space_id - dataspace identifier
! OUTPUTS
!  npoints  - number of points in the dataspace selection
!  hdferr   - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
! SOURCE
  SUBROUTINE h5sget_select_npoints_f(space_id, npoints, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSSIZE_T), INTENT(OUT) :: npoints
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5sget_simple_extent_ndims_f
!
! NAME
!  h5sget_simple_extent_ndims_f
!
! PURPOSE
!  Determines the dimensionality of a dataspace
!
! INPUTS
!  space_id 	 - dataspace identifier
! OUTPUTS
!  rank 	 - number of dataspace dimensions
!  hdferr      - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sget_simple_extent_ndims_f(space_id, rank, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: rank
    INTEGER, INTENT(OUT) :: hdferr
!*****
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
!
!****s* H5S/h5sget_simple_extent_dims_f
!
! NAME
!  h5sget_simple_extent_dims_f
!
! PURPOSE
!  Retrieves dataspace dimension size and maximum size.
!
! INPUTS
!  space_id - dataspace identifier
!
! OUTPUTS
!  dims     - array to store size of each dimension
!  maxdims  - array to store maximum size of each dimension
!  hdferr   - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sget_simple_extent_dims_f(space_id, dims, maxdims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: dims
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: maxdims
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5sget_simple_extent_type_f
!
! NAME
!  h5sget_simple_extent_type_f
!
! PURPOSE
!  Determine the current class of a dataspace
!
! INPUTS
!  space_id 	 - dataspace identifier
! OUTPUTS
!  classtype 	 - class type, possible values are:
!                   H5S_NO_CLASS_F
!                   H5S_SCALAR_F
!                   H5S_SIMPLE_F
!                   H5S_NULL_F
!  hdferr      - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sget_simple_extent_type_f(space_id, classtype, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: classtype
    INTEGER, INTENT(OUT) :: hdferr
!*****
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
!****s* H5S/h5sset_extent_simple_f
!
! NAME
!  h5sset_extent_simple_f
!
! PURPOSE
!  Sets or resets the size of an existing dataspace.
!
! INPUTS
!  space_id 	 - dataspace identifier
!  rank 	 - dataspace number of dimensions
!  current_size 	 - array with the new sizes of dimensions
!  maximum_size 	 - array with the new maximum sizes of
!  dimensions
! OUTPUTS
!  hdferr      - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sset_extent_simple_f(space_id, rank, current_size, &
       maximum_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(IN) :: rank
    INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: current_size
    INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: maximum_size
    INTEGER, INTENT(OUT) :: hdferr
!*****
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
!
!****s* H5S/h5sis_simple_f
!
! NAME
!  h5sis_simple_f
!
! PURPOSE
!  Determines whether a dataspace is a simple dataspace.
!
! INPUTS
!  space_id 	 - dataspace identifier
! OUTPUTS
!  status 	 - flag to indicate if dataspace
!                  is simple or not (TRUE or FALSE)
!  hdferr        - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sis_simple_f(space_id, status, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    LOGICAL, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5soffset_simple_f
!
! NAME
!  h5soffset_simple_f
!
! PURPOSE
!  Sets the offset of a simple dataspace.
!
! INPUTS
!  space_id 	 - dataspace identifier
!  offset 	 - the offset at which to position the
!                  selection
! OUTPUTS
!  hdferr      - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  NONE
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5soffset_simple_f(space_id, offset, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HSSIZE_T), DIMENSION(*), INTENT(IN) ::  offset
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5sextent_copy_f
!
! NAME
!  h5sextent_copy_f
!
! PURPOSE
!  Copies the extent of a dataspace.
!
! INPUTS
!  dest_space_id     - the identifier for the dataspace to which
!                      the extent is copied
!  source_space_id   - the identifier for the dataspace from
!                      which the extent is copied
! OUTPUTS
!  hdferr            - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  NONE
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! NOTES
!

! SOURCE
  SUBROUTINE h5sextent_copy_f(dest_space_id, source_space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dest_space_id
    INTEGER(HID_T), INTENT(IN) :: source_space_id
    INTEGER, INTENT(OUT) :: hdferr                ! Error code
!*****
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

!
!****s* H5S/h5sset_extent_none_f
!
! NAME
!  h5sset_extent_none_f
!
! PURPOSE
!  Removes the extent from a dataspace.
!
! INPUTS
!  space_id 	 - dataspace identifier
! OUTPUTS
!  hdferr      - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sset_extent_none_f(space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
!*****
    INTERFACE
       INTEGER FUNCTION h5sset_extent_none_c(space_id) BIND(C,NAME='h5sset_extent_none_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: space_id
       END FUNCTION h5sset_extent_none_c
    END INTERFACE
    
    hdferr = h5sset_extent_none_c(space_id)
    
  END SUBROUTINE h5sset_extent_none_f
!
!****s* H5S/h5sselect_hyperslab_f
!
! NAME
!  h5sselect_hyperslab_f
!
! PURPOSE
!  Selects a hyperslab region to add to the current selected
!  region
!
! INPUTS
!  space_id 	 - dataspace identifier
!  operator 	 - flag, valid values are:
!                    H5S_SELECT_SET_F
!                    H5S_SELECT_OR_F
!  start 	 - array with hyperslab offsets
!  count 	 - number of blocks included in the hyperslab
! OUTPUTS
!  hdferr        - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  stride 	 - array with hyperslab strides
!  block 	 - array with hyperslab block sizes
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 6, 2001
!
! SOURCE
  SUBROUTINE h5sselect_hyperslab_f(space_id, OPERATOR, start, count, &
       hdferr, stride, BLOCK)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id 
    INTEGER, INTENT(IN) :: OPERATOR
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: start
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: count
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HSIZE_T), DIMENSION(:), OPTIONAL, INTENT(IN) :: stride
    INTEGER(HSIZE_T), DIMENSION(:), OPTIONAL, INTENT(IN) :: BLOCK
!*****
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
!  !$!
!  !$!****s* H5S/h5scombine_hyperslab_f
!  !$!
!  !$! NAME
!  !$!		h5scombine_hyperslab_f
!  !$!
!  !$! PURPOSE
!  !$!	Combine a hyperslab selection with the current
!  !$!               selection for a dataspace
!  !$!
!  !$! INPUTS
!  !$!		space_id	- dataspace of selection to use
!  !$!		operator	- flag, valid values are:
!  !$!				  H5S_SELECT_NOOP_F
!  !$!				  H5S_SELECT_SET_F
!  !$!				  H5S_SELECT_OR_F
!  !$!				  H5S_SELECT_AND_F
!  !$!				  H5S_SELECT_XOR_F
!  !$!				  H5S_SELECT_NOTB_F
!  !$!				  H5S_SELECT_NOTA_F
!  !$!				  H5S_SELECT_APPEND_F
!  !$!				  H5S_SELECT_PREPEND_F
!  !$!		start		- array with hyperslab offsets
!  !$!		count		- number of blocks included in the
!  !$!				  hyperslab
!  !$! OUTPUTS
!  !$!               hyper_id        - identifier for the new hyperslab
!  !$!		hdferr:		- error code
!  !$!				 	Success:  0
!  !$!				 	Failure: -1
!  !$! OPTIONAL PARAMETERS
!  !$!		stride		- array with hyperslab strides
!  !$!		block		- array with hyperslab block sizes
!  !$!
!  !$! AUTHOR
!  !$!	Elena Pourmal
!  !$!		October 7, 2002
!  !$!
!  !$! HISTORY
!  !$!
!  !$!
!  !$! NOTES
!  !$! Commented out until 1.6 ? 10/08/2002
!  !$!
!  !$! SOURCE
!  SUBROUTINE h5scombine_hyperslab_f(space_id, operator, start, count, &
!  hyper_id,  hdferr, stride, block)
!  IMPLICIT NONE
!  INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
!  INTEGER, INTENT(IN) :: operator     ! Flag, valid values are:
						!  H5S_SELECT_NOOP_F
						!  H5S_SELECT_SET_F
						!  H5S_SELECT_OR_F
						!  H5S_SELECT_AND_F
						!  H5S_SELECT_XOR_F
						!  H5S_SELECT_NOTB_F
						!  H5S_SELECT_NOTA_F
						!  H5S_SELECT_APPEND_F
						!  H5S_SELECT_PREPEND_F
                                                !
!  INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: start
                                          ! Starting coordinates of the hyperslab
!  INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: count
                                          ! Number of blocks to select
                                          ! from dataspace
!  INTEGER(HID_T), INTENT(OUT) :: hyper_id ! New hyperslab identifier
!  INTEGER, INTENT(OUT) :: hdferr     ! Error code
!  INTEGER(HSIZE_T), DIMENSION(:), OPTIONAL, INTENT(IN) :: stride
                                          ! Array of how many elements to move
                                          ! in each direction
!  INTEGER(HSIZE_T), DIMENSION(:), OPTIONAL, INTENT(IN) :: block
                                          ! Sizes of element block
!  INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: def_block
!  INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: def_stride
!  INTEGER :: rank
!  INTEGER :: error1, error2

!  INTERFACE
!  INTEGER FUNCTION h5scombine_hyperslab_c(space_id, operator, &
!  start, count, stride, block, hyper_id)
!  USE H5GLOBAL
!  !DEC$IF DEFINED(HDF5F90_WINDOWS)
!  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SCOMBINE_HYPERSLAB_C'::h5scombine_hyperslab_c
!  !DEC$ENDIF
!  INTEGER(HID_T), INTENT(IN) :: space_id
!  INTEGER, INTENT(IN) :: operator
!  INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: start
!  INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: count
!  INTEGER(HSIZE_T), DIMENSION(*), OPTIONAL, INTENT(IN) :: stride
!  INTEGER(HSIZE_T), DIMENSION(*), OPTIONAL, INTENT(IN) :: block
!  INTEGER(HID_T), INTENT(OUT) :: hyper_id
!  END FUNCTION h5scombine_hyperslab_c
!  END INTERFACE

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

!  !$!
!  !$!****s* H5S/
!  !$!
!  !$! NAME
!  !$!		h5scombine_select_f
!  !$!
!  !$! PURPOSE
!  !$!	Combine two hyperslab selections with an operation
!  !$!               and return a dataspace with resulting selection.
!  !$!
!  !$! INPUTS
!  !$!		space1_id	- dataspace of selection to use
!  !$!		operator	- flag, valid values are:
!  !$!				  H5S_SELECT_NOOP_F
!  !$!				  H5S_SELECT_SET_F
!  !$!				  H5S_SELECT_OR_F
!  !$!				  H5S_SELECT_AND_F
!  !$!				  H5S_SELECT_XOR_F
!  !$!				  H5S_SELECT_NOTB_F
!  !$!				  H5S_SELECT_NOTA_F
!  !$!				  H5S_SELECT_APPEND_F
!  !$!				  H5S_SELECT_PREPEND_F
!  !$!		space2_id	- dataspace of selection to use
!  !$! OUTPUTS
!  !$!               ds_id           - idataspace identifier with the new selection
!  !$!		hdferr:		- error code
!  !$!				 	Success:  0
!  !$!				 	Failure: -1
!  !$! OPTIONAL PARAMETERS		- NONE
!  !$!
!  !$! AUTHOR
!  !$!	Elena Pourmal
!  !$!		October 7, 2002
!  !$!
!  !$! HISTORY
!  !$!
!  !$!
!  !$! NOTES commented out until 1.6 release(?) 10/08/2002
!  !$!

!  ! SOURCE
!  !$          SUBROUTINE h5scombine_select_f(space1_id, operator, space2_id, &
!  ds_id,  hdferr)
!  IMPLICIT NONE
!  INTEGER(HID_T), INTENT(IN) :: space1_id ! First dataspace identifier
!  INTEGER(HID_T), INTENT(IN) :: space2_id ! Second dataspace identifier
!  INTEGER, INTENT(IN) :: operator     ! Flag, valid values are:
						!  H5S_SELECT_NOOP_F
						!  H5S_SELECT_SET_F
						!  H5S_SELECT_OR_F
						!  H5S_SELECT_AND_F
						!  H5S_SELECT_XOR_F
						!  H5S_SELECT_NOTB_F
						!  H5S_SELECT_NOTA_F
						!  H5S_SELECT_APPEND_F
						!  H5S_SELECT_PREPEND_F
                                                !
!  INTEGER(HID_T), INTENT(OUT) :: ds_id ! New dataspace identifier
!  INTEGER, INTENT(OUT) :: hdferr     ! Error code
!
!  INTERFACE
!  INTEGER FUNCTION h5scombine_select_c(space1_id, operator, &
!  space2_id, ds_id)
!  USE H5GLOBAL
!  !DEC$IF DEFINED(HDF5F90_WINDOWS)
!  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SCOMBINE_SELECT_C'::h5scombine_select_c
!  !DEC$ENDIF
!  INTEGER(HID_T), INTENT(IN) :: space1_id
!  INTEGER(HID_T), INTENT(IN) :: space2_id
!  INTEGER, INTENT(IN) :: operator
!  INTEGER(HID_T), INTENT(OUT) :: ds_id
!  END FUNCTION h5scombine_select_c
!  END INTERFACE

!  hdferr = h5scombine_select_c(space1_id, operator, space2_id, &
!  ds_id)
!  return

!  END SUBROUTINE h5scombine_select_f

!  !$!
!  !$!****s* H5S/
!  !$!
!  !$! NAME
!  !$!		h5sselect_select_f
!  !$!
!  !$! PURPOSE
!  !$!	Refine a hyperslab selection with an operation
!  !$!               using second hyperslab
!  !$!
!  !$! INPUTS
!  !$!		space1_id	- dataspace of selection  to modify
!  !$!		operator	- flag, valid values are:
!  !$!				  H5S_SELECT_NOOP_F
!  !$!				  H5S_SELECT_SET_F
!  !$!				  H5S_SELECT_OR_F
!  !$!				  H5S_SELECT_AND_F
!  !$!				  H5S_SELECT_XOR_F
!  !$!				  H5S_SELECT_NOTB_F
!  !$!				  H5S_SELECT_NOTA_F
!  !$!				  H5S_SELECT_APPEND_F
!  !$!				  H5S_SELECT_PREPEND_F
!  !$!		space2_id	- dataspace of selection to use
!  !$!
!  !$! OUTPUTS
!  !$!		hdferr:		- error code
!  !$!				 	Success:  0
!  !$!				 	Failure: -1
!  !$! OPTIONAL PARAMETERS		- NONE
!  !$!
!  !$! AUTHOR
!  !$!	Elena Pourmal
!  !$!		October 7, 2002
!  !$!
!  !$! HISTORY
!  !$!
!  !$!
!  !$! NOTESCommented out until 1.6 release(?) 10/08/2002 EIP
!  !$!

!  ! SOURCE
!  SUBROUTINE h5sselect_select_f(space1_id, operator, space2_id, &
!  hdferr)
!  IMPLICIT NONE
!  INTEGER(HID_T), INTENT(INOUT) :: space1_id ! Dataspace identifier to
                                                       ! modify
!  INTEGER(HID_T), INTENT(IN) :: space2_id ! Second dataspace identifier
!  INTEGER, INTENT(IN) :: operator     ! Flag, valid values are:
						!  H5S_SELECT_NOOP_F
						!  H5S_SELECT_SET_F
						!  H5S_SELECT_OR_F
						!  H5S_SELECT_AND_F
						!  H5S_SELECT_XOR_F
						!  H5S_SELECT_NOTB_F
						!  H5S_SELECT_NOTA_F
						!  H5S_SELECT_APPEND_F
						!  H5S_SELECT_PREPEND_F
                                                !
!  INTEGER, INTENT(OUT) :: hdferr     ! Error code

!  INTERFACE
!  INTEGER FUNCTION h5sselect_select_c(space1_id, operator, &
!  space2_id)
!  USE H5GLOBAL
!  !DEC$IF DEFINED(HDF5F90_WINDOWS)
!  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SSELECT_SELECT_C'::h5sselect_select_c
!  !DEC$ENDIF
!  INTEGER(HID_T), INTENT(INOUT) :: space1_id
!  INTEGER(HID_T), INTENT(IN) :: space2_id
!  INTEGER, INTENT(IN) :: operator
!  END FUNCTION h5sselect_select_c
!  END INTERFACE

!  hdferr = h5sselect_select_c(space1_id, operator, space2_id)
!  return

!  END SUBROUTINE h5sselect_select_f

!
!****s* H5S/h5sget_select_type_f
!
! NAME
!  h5sget_select_type_f
!
! PURPOSE
!  Retrieve the type of selection
!
! INPUTS
!  space_id  - dataspace identifier with selection
! OUTPUTS
!  type      - selection type flag, valid values are:
!                    H5S_SEL_ERROR_F
!                    H5S_SEL_NONE_F
!                    H5S_SEL_POINTS_F
!                    H5S_SEL_HYPERSLABS_F
!                    H5S_SEL_ALL_F
!  hdferr    - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  October 7, 2002
!
! SOURCE
  SUBROUTINE h5sget_select_type_f(space_id, TYPE, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(INOUT) :: space_id
    INTEGER, INTENT(OUT) :: TYPE
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/H5Sdecode_f
!
! NAME
!  H5Sdecode_f
!
! PURPOSE
!  Decode a binary object description of data space and return a new object handle.
!
! INPUTS
!  buf 	  -  Buffer for the data space object to be decoded.
!  obj_id - Object ID
! OUTPUTS
!  hdferr - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 26, 2008
! SOURCE
  SUBROUTINE h5sdecode_f(buf, obj_id, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: buf
    INTEGER(HID_T), INTENT(OUT) :: obj_id
    INTEGER, INTENT(OUT) :: hdferr
!*****
    INTERFACE
       INTEGER FUNCTION h5sdecode_c(buf, obj_id) BIND(C,NAME='h5sdecode_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: buf
         INTEGER(HID_T), INTENT(OUT) :: obj_id  ! Object ID
       END FUNCTION h5sdecode_c
    END INTERFACE

    hdferr = h5sdecode_c(buf, obj_id)

  END SUBROUTINE h5sdecode_f

!
!****s* H5S/H5Sencode_f
!
! NAME
!  H5Sencode_f
!
! PURPOSE
!  Encode a data space object description into a binary buffer.
!
! INPUTS
!  obj_id - Identifier of the object to be encoded.
!  buf 	  - Buffer for the object to be encoded into.
!  nalloc - The size of the allocated buffer.
! OUTPUTS
!  nalloc - The size of the buffer needed.
!  hdferr - Returns 0 if successful and -1 if fails.
!
! AUTHOR
!  M. Scot Breitenfeld
!  March 26, 2008
! SOURCE
  SUBROUTINE h5sencode_f(obj_id, buf, nalloc, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    CHARACTER(LEN=*), INTENT(OUT) :: buf
    INTEGER(SIZE_T), INTENT(INOUT) :: nalloc
    INTEGER, INTENT(OUT) :: hdferr
!*****

    INTERFACE
       INTEGER FUNCTION h5sencode_c(buf, obj_id, nalloc) BIND(C,NAME='h5sencode_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: buf
         INTEGER(SIZE_T), INTENT(INOUT) :: nalloc
       END FUNCTION h5sencode_c
    END INTERFACE

    hdferr = h5sencode_c(buf, obj_id, nalloc)

  END SUBROUTINE h5sencode_f

!****s* H5S/h5sextent_equal_f
!
! NAME
!  h5sextent_equal_f
!
! PURPOSE
!  Determines whether two dataspace extents are equal.
!
! INPUTS
!  space1_id - First dataspace identifier.
!  space2_id - Second dataspace identifier.
! OUTPUTS
!  Equal     - .TRUE. if equal, .FALSE. if unequal.
!  hdferr    - Returns 0 if successful and -1 if fails
! AUTHOR
!  M. Scot Breitenfeld
!  April 2, 2008
!
! SOURCE
  SUBROUTINE h5sextent_equal_f(space1_id, space2_id, equal, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space1_id
    INTEGER(HID_T), INTENT(IN) :: space2_id
    LOGICAL, INTENT(OUT) :: Equal
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!
!****s* H5S/h5sget_regular_hyperslab_f
!
! NAME
!  h5sget_regular_hyperslab_f
!
! PURPOSE
!  Retrieves a regular hyperslab selection.
!
! INPUTS
!  space_id - The identifier of the dataspace.
! OUTPUTS
!  start    - Offset of the start of the regular hyperslab.
!  stride   - Stride of the regular hyperslab.
!  count    - Number of blocks in the regular hyperslab.
!  block    - Size of a block in the regular hyperslab.
!  hdferr   - Returns 0 if successful and -1 if fails.
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 28 2016
! SOURCE
  SUBROUTINE h5sget_regular_hyperslab_f(space_id, start, stride, count, block, hdferr)
    
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) ::  space_id
    INTEGER(HSIZE_T), INTENT(OUT), DIMENSION(*), TARGET ::  start
    INTEGER(HSIZE_T), INTENT(OUT), DIMENSION(*), TARGET ::  stride
    INTEGER(HSIZE_T), INTENT(OUT), DIMENSION(*), TARGET ::  count
    INTEGER(HSIZE_T), INTENT(OUT), DIMENSION(*), TARGET ::  block
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

!****s* H5S/h5sis_regular_hyperslab_f
!
! NAME
!  h5sis_regular_hyperslab_f
!
! PURPOSE
!  Retrieves a regular hyperslab selection.
!
! INPUTS
!  space_id  - The identifier of the dataspace.
! OUTPUTS
!  IsRegular - TRUE or FALSE for hyperslab selection if successful.
!  hdferr    - Returns 0 if successful and -1 if fails.
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 28 2016
! SOURCE
  SUBROUTINE h5sis_regular_hyperslab_f(space_id, IsRegular, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) ::  space_id
    LOGICAL :: IsRegular
    INTEGER, INTENT(OUT) :: hdferr
!*****
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

END MODULE H5S
