!
! This file contains Fortran90 interfaces for H5S functions.
! 
      MODULE H5S
        USE H5GLOBAL
 
        CONTAINS
          
           
          SUBROUTINE h5screate_simple_f(rank, dims, space_id, hdferr, maxdims) 

            IMPLICIT NONE
            INTEGER, INTENT(IN) :: rank     ! Number of dataspace dimensions 
            INTEGER(HSIZE_T), INTENT(IN) :: dims(rank) 
                                                    ! Array with the dimension 
                                                    ! sizes 
            INTEGER(HID_T), INTENT(OUT) :: space_id ! Dataspace identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER(HSIZE_T), OPTIONAL, INTENT(IN) :: maxdims(rank) 
                                                    ! Array with the maximum 
                                                    ! dimension sizes 
            INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: f_maxdims
            INTEGER, EXTERNAL :: h5screate_simple_c
            
            allocate (f_maxdims(rank), stat=hdferr)
            if (hdferr .NE. 0) then 
                hdferr = -1
                return
            endif 
            if (present(maxdims)) then 
                f_maxdims = maxdims 
            else
                f_maxdims = dims
            endif 
            hdferr = h5screate_simple_c(rank, dims, f_maxdims, space_id)
            deallocate(f_maxdims)

          END SUBROUTINE h5screate_simple_f
          
          SUBROUTINE h5sclose_f(space_id, hdferr)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            INTEGER, EXTERNAL :: h5sclose_c

            hdferr = h5sclose_c(space_id)

          END SUBROUTINE h5sclose_f

          SUBROUTINE h5screate_f(classtype, space_id, hdferr) 

            IMPLICIT NONE
            INTEGER, INTENT(IN) :: classtype     ! The type of the dataspace
                                                 ! to be created. 
                                                 ! Possible values are:
                                                 !  H5S_SCALAR_F (0)
                                                 !  H5S_SIMPLE_F(1)
            INTEGER(HID_T), INTENT(OUT) :: space_id ! Dataspace identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5screate_c
            hdferr = h5screate_c(classtype, space_id)

          END SUBROUTINE h5screate_f


          SUBROUTINE h5scopy_f(space_id, new_space_id, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER(HID_T), INTENT(OUT) :: new_space_id 
                                             ! Identifier of dataspace's copy 
            INTEGER, INTENT(OUT) :: hdferr   ! Error code
            INTEGER, EXTERNAL :: h5scopy_c
            hdferr = h5scopy_c(space_id, new_space_id)
 
          END SUBROUTINE h5scopy_f

          SUBROUTINE h5sget_select_hyper_nblocks_f(space_id, num_blocks, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER(HSSIZE_T), INTENT(OUT) :: num_blocks 
                                             !number of hyperslab blocks 
                                             !in the current dataspace 
                                             !selection 
            INTEGER, INTENT(OUT) :: hdferr   ! Error code
            INTEGER, EXTERNAL :: h5sget_select_hyper_nblocks_c
            hdferr =  h5sget_select_hyper_nblocks_c (space_id, num_blocks)
 
          END SUBROUTINE h5sget_select_hyper_nblocks_f

          SUBROUTINE h5sget_select_hyper_blocklist_f(space_id, startblock, &
                                                    num_blocks, buf, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: startblock 
                                             !Hyperslab block to start with. 
            INTEGER(HSIZE_T), INTENT(IN) :: num_blocks 
                                             !number of hyperslab blocks 
                                             !to get in the current dataspace 
                                             !selection 
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf 
                                             !List of hyperslab blocks selected
            INTEGER, INTENT(OUT) :: hdferr   ! Error code

            INTEGER, EXTERNAL :: h5sget_select_hyper_blocklist_c
 
            hdferr =  h5sget_select_hyper_blocklist_c(space_id, startblock, &
                                                       num_blocks, buf )
 
          END SUBROUTINE h5sget_select_hyper_blocklist_f

          SUBROUTINE  h5sget_select_bounds_f(space_id, start, end, hdferr)

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: start
                                             !Starting coordinates of the bounding box. 
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: end
                                             !Ending coordinates of the bounding box,
                                             !i.e., the coordinates of the diagonally 
                                             !opposite corner 
            INTEGER, INTENT(OUT) :: hdferr   ! Error code
            INTEGER, EXTERNAL :: h5sget_select_bounds_c
            hdferr =   h5sget_select_bounds_c(space_id, start, end)
 
          END SUBROUTINE h5sget_select_bounds_f

          SUBROUTINE h5sget_select_elem_npoints_f(space_id, num_points, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER(HSSIZE_T), INTENT(OUT) :: num_points 
                                             !number of element points 
                                             !in the current dataspace 
                                             !selection 
            INTEGER, INTENT(OUT) :: hdferr   ! Error code
            INTEGER, EXTERNAL :: h5sget_select_elem_npoints_c
            hdferr =  h5sget_select_elem_npoints_c (space_id, num_points)
 
          END SUBROUTINE h5sget_select_elem_npoints_f

          SUBROUTINE h5sget_select_elem_pointlist_f(space_id, startpoint, &
                                                    num_points, buf, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER(HSIZE_T),DIMENSION(*), INTENT(IN) :: startpoint 
                                             !Element point to start with. 
            INTEGER(HSIZE_T), INTENT(IN) :: num_points 
                                             !Number of element points to get 
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf 
                                             !List of element points selected
            INTEGER, INTENT(OUT) :: hdferr   ! Error code
            INTEGER, EXTERNAL :: h5sget_select_elem_pointlist_c
            hdferr =  h5sget_select_elem_pointlist_c(space_id, startpoint, &
                                                       num_points, buf )
          END SUBROUTINE h5sget_select_elem_pointlist_f

          SUBROUTINE h5sselect_elements_f(space_id, operator, rank, & 
                                          num_elements, coord, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER, INTENT(IN) :: operator    ! Flag, valid values are:
                                               ! H5S_SELECT_SET_F (0)
                                               ! H5S_SELECT_OR_F (1)
            INTEGER, INTENT(IN) :: rank     ! Number of dataspace dimensions 
            INTEGER(SIZE_T), INTENT(IN) :: num_elements  ! Number of elements to be
                                                 ! selected
            INTEGER(HSSIZE_T), & 
            DIMENSION(rank,num_elements), INTENT(IN) :: coord 
                                          ! Array with the coordinates
                                          ! of the selected elements
                                          ! coord(rank, num_elements)
            INTEGER, INTENT(OUT) :: hdferr     ! Error code
            INTEGER, EXTERNAL :: h5sselect_elements_c
            INTEGER(HSSIZE_T), ALLOCATABLE, DIMENSION(:,:) :: c_coord
            INTEGER :: error, i,j
            allocate(c_coord(rank, num_elements), stat = error)
            if (error.NE. 0) then
                hdferr = -1
                return
            endif
            do i = 1, rank
               c_coord(i,:) = coord(rank-i+1, :) - 1
            enddo 
            hdferr = h5sselect_elements_c(space_id, operator, num_elements, &
                                          c_coord)
            deallocate(c_coord)
 
          END SUBROUTINE h5sselect_elements_f


          SUBROUTINE h5sselect_all_f(space_id, hdferr) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id  ! Dataspace identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5sselect_all_c
            hdferr = h5sselect_all_c(space_id)

          END SUBROUTINE h5sselect_all_f


          SUBROUTINE h5sselect_none_f(space_id, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id  ! Dataspace identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5sselect_none_c
            hdferr = h5sselect_none_c(space_id)

          END SUBROUTINE h5sselect_none_f



          SUBROUTINE h5sselect_valid_f(space_id, status, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id  ! Dataspace identifier 
            LOGICAL, INTENT(OUT) :: status          ! TRUE if the selection is
                                                    ! contained within the extent,
                                                    ! FALSE otherwise. 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER :: flag ! "TRUE/FALSE/ERROR" flag from C routine
            INTEGER, EXTERNAL :: h5sselect_valid_c
            hdferr = h5sselect_valid_c(space_id, flag)
            status = .TRUE.
            if (flag .EQ. 0) status = .FALSE.

          END SUBROUTINE h5sselect_valid_f


          SUBROUTINE h5sget_simple_extent_npoints_f(space_id, npoints, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id     ! Dataspace identifier 
            INTEGER(HSIZE_T), INTENT(OUT) :: npoints  ! Number of elements in 
                                                       ! dataspace
            INTEGER, INTENT(OUT) :: hdferr             ! Error code
            INTEGER, EXTERNAL :: h5sget_simple_extent_npoints_c
            hdferr = h5sget_simple_extent_npoints_c( space_id, npoints)
 
          END SUBROUTINE h5sget_simple_extent_npoints_f


          SUBROUTINE h5sget_select_npoints_f(space_id, npoints, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id     ! Dataspace identifier 
            INTEGER(HSSIZE_T), INTENT(OUT) :: npoints  ! Number of elements in the
                                                       ! selection 
            INTEGER, INTENT(OUT) :: hdferr             ! Error code
            INTEGER, EXTERNAL :: h5sget_select_npoints_c
            hdferr = h5sget_select_npoints_c(space_id, npoints)
 
          END SUBROUTINE h5sget_select_npoints_f


          SUBROUTINE h5sget_simple_extent_ndims_f(space_id, rank, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id     ! Dataspace identifier 
            INTEGER, INTENT(OUT) :: rank               ! Number of dimensions 
            INTEGER, INTENT(OUT) :: hdferr             ! Error code
            INTEGER, EXTERNAL :: h5sget_simple_extent_ndims_c
            hdferr = h5sget_simple_extent_ndims_c(space_id, rank)
 
          END SUBROUTINE h5sget_simple_extent_ndims_f


          SUBROUTINE h5sget_simple_extent_dims_f(space_id, dims, maxdims, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: dims 
                                                   ! Array to store dimension sizes 
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: maxdims 
                                                   ! Array to store max dimension 
                                                   ! sizes  
            INTEGER, INTENT(OUT) :: hdferr         ! Error code: -1 on failure,
                                                   ! number of dimensions on
                                                   ! on success
            INTEGER, EXTERNAL :: h5sget_simple_extent_dims_c
            hdferr = h5sget_simple_extent_dims_c(space_id, dims, maxdims)
 
          END SUBROUTINE h5sget_simple_extent_dims_f


          SUBROUTINE h5sget_simple_extent_type_f(space_id, classtype, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER, INTENT(OUT) :: classtype      ! Class type , possible values
                                                   ! are: 
                                                   !  H5S_NO_CLASS_F (-1)
                                                   !  H5S_SCALAR_F (0)
                                                   !  H5S_SIMPLE_F (1)
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            INTEGER, EXTERNAL :: h5sget_simple_extent_type_c
            hdferr = h5sget_simple_extent_type_c(space_id, classtype)
 
          END SUBROUTINE h5sget_simple_extent_type_f


          SUBROUTINE h5sset_extent_simple_f(space_id, rank, current_size, &
                                            maximum_size, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER, INTENT(IN) :: rank            ! Dataspace rank 
            INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: current_size 
                                                   ! Array with the new sizes
                                                   ! of dimensions 
            INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: maximum_size  
                                                   ! Array with the new maximum
                                                   ! sizes of dimensions 
                                                   ! sizes  
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            INTEGER, EXTERNAL :: h5sset_extent_simple_c
            hdferr = h5sset_extent_simple_c(space_id, rank, current_size, &
                                            maximum_size)
 
          END SUBROUTINE h5sset_extent_simple_f


          SUBROUTINE h5sis_simple_f(space_id, status, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id  ! Dataspace identifier 
            LOGICAL, INTENT(OUT) :: status      ! Flag, idicates if dataspace
                                                ! is simple or not ( TRUE or
                                                ! FALSE)  
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER :: flag                     ! "TRUE/FALSE/ERROR from C" 
            INTEGER, EXTERNAL :: h5sis_simple_c
            hdferr = h5sis_simple_c(space_id, flag)
            status = .TRUE.
            if (flag .EQ. 0) status = .FALSE.
 
          END SUBROUTINE h5sis_simple_f

          SUBROUTINE h5soffset_simple_f(space_id, offset, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER(HSSIZE_T), DIMENSION(*), INTENT(IN) ::  offset
                                                   ! The offset at which to position
                                                   ! the selection  
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            INTEGER, EXTERNAL :: h5soffset_simple_c
            hdferr = h5soffset_simple_c(space_id, offset)
 
          END SUBROUTINE h5soffset_simple_f


          SUBROUTINE h5sextent_copy_f(dest_space_id, source_space_id, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dest_space_id  ! Identifier of destination
                                                         ! dataspace
            INTEGER(HID_T), INTENT(IN) :: source_space_id ! Identifier of source 
                                                          ! dataspace
            INTEGER, INTENT(OUT) :: hdferr                ! Error code
            INTEGER, EXTERNAL :: h5sextent_copy_c
            hdferr = h5sextent_copy_c(dest_space_id, source_space_id)
 
          END SUBROUTINE h5sextent_copy_f


          SUBROUTINE h5sset_extent_none_f(space_id, hdferr) 

            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id  ! Dataspace identifier 
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5sset_extent_none_c
            hdferr = h5sset_extent_none_c(space_id)
 
          END SUBROUTINE h5sset_extent_none_f


          SUBROUTINE h5sselect_hyperslab_f(space_id, operator, start, count, &
                                           hdferr, stride, block) 
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier 
            INTEGER, INTENT(IN) :: operator     ! Flag, valid values are:
                                                ! H5S_SELECT_SET_F (0)
                                                ! H5S_SELECT_OR_F (1)
                                                !  
            INTEGER(HSSIZE_T), DIMENSION(*), INTENT(IN) :: start
                                          ! Starting coordinates of the hyperslab 
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: count 
                                          ! Number of blocks to select 
                                          ! from dataspace 
            INTEGER, INTENT(OUT) :: hdferr     ! Error code
            INTEGER(HSIZE_T), DIMENSION(:), OPTIONAL, INTENT(IN) :: stride
                                          ! Array of how many elements to move
                                          ! in each direction
            INTEGER(HSIZE_T), DIMENSION(:), OPTIONAL, INTENT(IN) :: block 
                                          ! Sizes of element block
            INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: def_block 
            INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: def_stride
            INTEGER, EXTERNAL :: h5sselect_hyperslab_c
            INTEGER :: rank
            INTEGER :: error1, error2 
            if (present(stride).and. present(block)) then
            hdferr = h5sselect_hyperslab_c(space_id, operator, start, count, &
                                           stride, block)
            return
            endif
            ! Case of optional parameters.
            !
            ! Find the rank of the dataspace to allocate memery for
            ! default stride and block arrays.
            !
            CALL h5sget_simple_extent_ndims_f(space_id, rank, hdferr)
            if( hdferr .EQ. -1) return
            !
            if (present(stride).and. .not.present(block)) then
            allocate(def_block(rank), stat=error1)
                if (error1.NE.0) then
                    hdferr = -1
                    return
                endif
            def_block = 1
            hdferr = h5sselect_hyperslab_c(space_id, operator, start, count, &
                                           stride, def_block)
            deallocate(def_block)
            return
            endif

            if (.not.present(stride).and. present(block)) then
            allocate(def_stride(rank), stat=error2)
                if (error2.NE.0) then
                    hdferr = -1
                    return
                endif
            def_stride = 1
            hdferr = h5sselect_hyperslab_c(space_id, operator, start, count, &
                                           def_stride, block)
            deallocate(def_stride)
            return
            endif
            allocate(def_block(rank), stat=error1)
            allocate(def_stride(rank), stat=error2)
                if ((error1.NE.0) .OR. (error2.NE.0)) then
                    hdferr = -1
                    return
                endif
            def_block = 1
            def_stride = 1
            hdferr = h5sselect_hyperslab_c(space_id, operator, start, count, &
                                           def_stride, def_block)
            deallocate(def_block)
            deallocate(def_stride)
 
          END SUBROUTINE h5sselect_hyperslab_f

      END MODULE H5S
